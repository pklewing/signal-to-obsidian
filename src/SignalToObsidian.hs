{-# LANGUAGE OverloadedStrings #-}

-- | Convert a Signal Desktop "local backup" JSONL export into Obsidian
-- Markdown, one file per conversation. Media are embedded inline as Obsidian
-- wikilinks, reactions are rendered inline in chronological order.
--
-- The converter reads the whole file once, building lookup tables from the
-- @recipient@ and @chat@ records, then renders every @chatItem@.
module SignalToObsidian
  ( -- * JSON record types
    Record (..)
  , Account (..)
  , Recipient (..)
  , Contact (..)
  , Group (..)
  , Chat (..)
  , ChatItem (..)
  , StandardMessage (..)
  , Attachment (..)
  , Pointer (..)
  , LocatorInfo (..)
  , Reaction (..)
  , Id (..)
  , Millis (..)

    -- * Attachment path derivation
  , localBackupFilename
  , extFor
  , attachmentPath
  , attachmentHash
  , buildFileIndex
  , resolveOnDisk

    -- * Time formatting
  , fmtTs

    -- * Name resolution
  , contactName
  , groupTitle
  , recipientDisplayName
  , resolveAuthorName

    -- * Filenames
  , sanitizeFilename

    -- * Index building
  , Index (..)
  , buildIndexes

    -- * Rendering
  , renderReactions
  , renderAttachments
  , renderChatItem

    -- * CLI / conversion
  , Args (..)
  , parseArgs
  , Options (..)
  , Summary (..)
  , convert
  ) where

import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as A
import Data.Aeson (FromJSON (..), withObject, (.!=), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getTimeZone, utcToLocalTime)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , listDirectory
  , makeAbsolute
  )
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- JSON record types
--
-- Only the fields the tool reads are modelled; everything is optional so a
-- record with unknown or missing keys degrades to fallbacks instead of
-- failing, mirroring the original map lookups.
-- ---------------------------------------------------------------------------

-- | A recipient/author id. The export uses strings; numbers are tolerated.
newtype Id = Id {unId :: Text}
  deriving (Show, Eq, Ord)

instance FromJSON Id where
  parseJSON (A.String t) = pure (Id t)
  parseJSON (A.Number n) =
    pure . Id . T.pack $ either (show :: Double -> String) (show :: Integer -> String) (floatingOrInteger n)
  parseJSON v = fail ("Id: expected string or number, got " <> show v)

-- | Epoch milliseconds; the export encodes them as strings, numbers tolerated.
newtype Millis = Millis {unMillis :: Integer}
  deriving (Show, Eq, Ord)

instance FromJSON Millis where
  parseJSON (A.String t) = case reads (T.unpack t) of
    [(n, "")] -> pure (Millis n)
    _ -> fail ("Millis: unparseable " <> show t)
  parseJSON (A.Number n) = case floatingOrInteger n of
    Right i -> pure (Millis i)
    Left (d :: Double) -> pure (Millis (truncate d))
  parseJSON v = fail ("Millis: expected string or number, got " <> show v)

-- | One line of the JSONL export.
data Record = Record
  { recRecipient :: Maybe Recipient
  , recChat :: Maybe Chat
  , recChatItem :: Maybe ChatItem
  , recAccount :: Maybe Account
  }
  deriving (Show)

instance FromJSON Record where
  parseJSON = withObject "Record" $ \o ->
    Record
      <$> o .:? "recipient"
      <*> o .:? "chat"
      <*> o .:? "chatItem"
      <*> o .:? "account"

data Account = Account
  { accGivenName :: Maybe Text
  , accFamilyName :: Maybe Text
  }
  deriving (Show)

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o ->
    Account <$> o .:? "givenName" <*> o .:? "familyName"

data Recipient = Recipient
  { rcpId :: Maybe Id
  , rcpSelf :: Maybe A.Value -- presence marker; contents unused
  , rcpContact :: Maybe Contact
  , rcpGroup :: Maybe Group
  }
  deriving (Show)

instance FromJSON Recipient where
  parseJSON = withObject "Recipient" $ \o ->
    Recipient
      <$> o .:? "id"
      <*> o .:? "self"
      <*> o .:? "contact"
      <*> o .:? "group"

data Contact = Contact
  { cSystemGivenName :: Maybe Text
  , cSystemFamilyName :: Maybe Text
  , cProfileGivenName :: Maybe Text
  , cProfileFamilyName :: Maybe Text
  , cE164 :: Maybe Id
  }
  deriving (Show)

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \o ->
    Contact
      <$> o .:? "systemGivenName"
      <*> o .:? "systemFamilyName"
      <*> o .:? "profileGivenName"
      <*> o .:? "profileFamilyName"
      <*> o .:? "e164"

-- | Only the group title (@snapshot.title.title@) is needed.
newtype Group = Group {grpTitle :: Maybe Text}
  deriving (Show)

instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    msnap <- o .:? "snapshot"
    title <- case msnap of
      Nothing -> pure Nothing
      Just snap -> do
        mtitle <- snap .:? "title"
        case mtitle of
          Nothing -> pure Nothing
          Just t -> t .:? "title"
    pure (Group title)

data Chat = Chat
  { chId :: Maybe Id
  , chRecipientId :: Maybe Id
  }
  deriving (Show)

instance FromJSON Chat where
  parseJSON = withObject "Chat" $ \o ->
    Chat <$> o .:? "id" <*> o .:? "recipientId"

data ChatItem = ChatItem
  { ciChatId :: Maybe Id
  , ciAuthorId :: Maybe Id
  , ciDateSent :: Maybe Millis
  , ciStandardMessage :: Maybe StandardMessage
  }
  deriving (Show)

instance FromJSON ChatItem where
  parseJSON = withObject "ChatItem" $ \o ->
    ChatItem
      <$> o .:? "chatId"
      <*> o .:? "authorId"
      <*> o .:? "dateSent"
      <*> o .:? "standardMessage"

data StandardMessage = StandardMessage
  { smBody :: Maybe Text -- text.body
  , smAttachments :: [Attachment]
  , smReactions :: [Reaction]
  }
  deriving (Show)

instance FromJSON StandardMessage where
  parseJSON = withObject "StandardMessage" $ \o -> do
    mtext <- o .:? "text"
    body <- maybe (pure Nothing) (.:? "body") mtext
    atts <- o .:? "attachments" .!= []
    reacts <- o .:? "reactions" .!= []
    pure (StandardMessage body atts reacts)

newtype Attachment = Attachment {attPointer :: Maybe Pointer}
  deriving (Show)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment <$> o .:? "pointer"

data Pointer = Pointer
  { pContentType :: Maybe Text
  , pFileName :: Maybe Text
  , pLocatorInfo :: Maybe LocatorInfo
  }
  deriving (Show)

instance FromJSON Pointer where
  parseJSON = withObject "Pointer" $ \o ->
    Pointer
      <$> o .:? "contentType"
      <*> o .:? "fileName"
      <*> o .:? "locatorInfo"

data LocatorInfo = LocatorInfo
  { liPlaintextHash :: Maybe Text
  , liLocalKey :: Maybe Text
  }
  deriving (Show)

instance FromJSON LocatorInfo where
  parseJSON = withObject "LocatorInfo" $ \o ->
    LocatorInfo <$> o .:? "plaintextHash" <*> o .:? "localKey"

data Reaction = Reaction
  { reEmoji :: Maybe Text
  , reAuthorId :: Maybe Id
  , reSentTimestamp :: Maybe Millis
  }
  deriving (Show)

instance FromJSON Reaction where
  parseJSON = withObject "Reaction" $ \o ->
    Reaction
      <$> o .:? "emoji"
      <*> o .:? "authorId"
      <*> o .:? "sentTimestamp"

-- ---------------------------------------------------------------------------
-- Attachment path derivation  (mirrors Signal-Desktop mediaId.preload.ts)
-- ---------------------------------------------------------------------------

b64Decode :: Text -> Maybe BS.ByteString
b64Decode = either (const Nothing) Just . B64.decode . TE.encodeUtf8

-- Digest's Show instance renders the digest as lowercase hex.
sha256Hex :: BS.ByteString -> Text
sha256Hex bs = T.pack (show (hash bs :: Digest SHA256))

-- filename = hex( sha256( base64(plaintextHash) ++ base64(localKey) ) )
-- NOTE: In the JSONL export, BOTH plaintextHash and localKey are base64-encoded.
-- (The Signal-Desktop *source* calls fromHex(plaintextHash), but that operates on
-- an already-hex string used internally; the exported JSON value is base64.
-- Verified empirically: base64-decoding both reproduces the real on-disk names.)

-- | The 64-char hex on-disk name for an attachment, or Nothing if the
-- required locator fields are absent.
localBackupFilename :: LocatorInfo -> Maybe Text
localBackupFilename li = do
  ph <- liPlaintextHash li >>= b64Decode
  lk <- liLocalKey li >>= b64Decode
  pure (sha256Hex (ph <> lk))

-- | Pick a file extension from contentType, falling back to the fileName suffix.
extFor :: Maybe Text -> Maybe Text -> Text
extFor contentType fileName
  | Just ext <- fromName, T.length ext <= 5 = T.toLower ext
  | Just ct <- contentType
  , (_, rest) <- T.breakOn "/" ct
  , not (T.null rest) =
      -- normalise a few common ones; otherwise use the subtype verbatim
      let sub = T.drop 1 rest
       in fromMaybe sub (lookup sub [("jpeg", "jpg"), ("svg+xml", "svg"), ("quicktime", "mov")])
  | otherwise = "bin"
  where
    fromName = do
      fn <- fileName
      let (pre, suf) = T.breakOnEnd "." fn
      if T.null pre then Nothing else Just suf

-- | Build the @files/xx/\<hash\>.\<ext\>@ path for one attachment, or Nothing.
attachmentPath :: Attachment -> Maybe Text
attachmentPath att = do
  ptr <- attPointer att
  loc <- pLocatorInfo ptr
  name <- localBackupFilename loc
  let ext = extFor (pContentType ptr) (pFileName ptr)
  pure ("files/" <> T.take 2 name <> "/" <> name <> "." <> ext)

-- | Just the 64-char hex stem (no extension), or Nothing.
attachmentHash :: Attachment -> Maybe Text
attachmentHash att = attPointer att >>= pLocatorInfo >>= localBackupFilename

-- | Walk @\<files-root\>/files@ once and return a map of hash-stem -> absolute
-- path. Turns thousands of per-attachment directory listings into a single walk.
-- Hidden entries (dotfiles like .DS_Store) are skipped.
buildFileIndex :: FilePath -> IO (Map Text FilePath)
buildFileIndex filesRoot = do
  let root = filesRoot </> "files"
  exists <- doesDirectoryExist root
  if not exists
    then pure Map.empty
    else do
      absRoot <- makeAbsolute root
      paths <- walk absRoot
      pure $ Map.fromList [(T.pack (dropExtension (takeFileName p)), p) | p <- paths]
  where
    walk dir = do
      names <- filter (not . ("." `isPrefixOf`)) <$> listDirectory dir
      fmap concat . forM names $ \n -> do
        let p = dir </> n
        isDir <- doesDirectoryExist p
        if isDir
          then walk p
          else do
            isFile <- doesFileExist p
            pure [p | isFile]

-- | Look up an attachment's real path via the prebuilt file index.
resolveOnDisk :: Map Text FilePath -> Attachment -> Maybe FilePath
resolveOnDisk fileIndex att = attachmentHash att >>= (`Map.lookup` fileIndex)

-- ---------------------------------------------------------------------------
-- Time formatting
-- ---------------------------------------------------------------------------

-- | Format epoch millis as @yyyy-MM-dd HH:mm@ in the system zone, applying the
-- zone's rules per instant (DST-correct for historical timestamps).
fmtTs :: Millis -> IO Text
fmtTs (Millis ms) = do
  let utc :: UTCTime
      utc = posixSecondsToUTCTime (fromInteger ms / 1000)
  tz <- getTimeZone utc
  pure . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (utcToLocalTime tz utc)

-- ---------------------------------------------------------------------------
-- Name resolution
-- ---------------------------------------------------------------------------

blank :: Text -> Bool
blank = T.all isSpace

-- | Best display name for an individual contact recipient.
contactName :: Contact -> Text
contactName c =
  let joinParts a b = T.strip (T.intercalate " " (filter (not . blank) (map (fromMaybe "") [a, b])))
      sys = joinParts (cSystemGivenName c) (cSystemFamilyName c)
      prof = joinParts (cProfileGivenName c) (cProfileFamilyName c)
   in if not (T.null sys)
        then sys
        else
          if not (T.null prof)
            then prof
            else maybe "Unknown" unId (cE164 c)

groupTitle :: Group -> Text
groupTitle = fromMaybe "Unknown Group" . grpTitle

-- | Display name for any recipient record (contact, group, or self).
recipientDisplayName :: Recipient -> Text -> Text
recipientDisplayName r selfName
  | isJust (rcpSelf r) = selfName
  | Just c <- rcpContact r = contactName c
  | Just g <- rcpGroup r = groupTitle g
  -- distributionList / callLink etc. -> fall back to id
  | otherwise = "Recipient " <> maybe "" unId (rcpId r)

-- ---------------------------------------------------------------------------
-- Filesystem-safe filenames
-- ---------------------------------------------------------------------------

sanitizeFilename :: Text -> Text
sanitizeFilename s =
  let legal = T.filter (`notElem` ("\\/:*?\"<>|" :: String)) s -- chars illegal on common FS
      collapsed = T.unwords (T.words legal) -- collapse whitespace + trim
   in if T.null collapsed then "conversation" else collapsed

-- ---------------------------------------------------------------------------
-- First pass: build lookup tables
-- ---------------------------------------------------------------------------

data Index = Index
  { idxRecipients :: Map Text Recipient -- id -> recipient
  , idxChats :: Map Text Text -- chatId -> recipientId
  , idxSelfId :: Maybe Text
  , idxSelfName :: Text
  }
  deriving (Show)

buildIndexes :: [Record] -> Index
buildIndexes records =
  let idText = maybe "" unId
      step idx rec = case (recRecipient rec, recChat rec, recAccount rec) of
        (Just r, _, _) ->
          idx
            { idxRecipients = Map.insert (idText (rcpId r)) r (idxRecipients idx)
            , idxSelfId =
                if isJust (rcpSelf r)
                  then unId <$> rcpId r
                  else idxSelfId idx
            }
        (_, Just c, _) ->
          idx {idxChats = Map.insert (idText (chId c)) (idText (chRecipientId c)) (idxChats idx)}
        (_, _, Just a) ->
          let full = T.strip (fromMaybe "" (accGivenName a) <> " " <> fromMaybe "" (accFamilyName a))
           in if T.null full then idx else idx {idxSelfName = full}
        _ -> idx
      base = List.foldl' step (Index Map.empty Map.empty Nothing "Me") records
      -- If the account holder also appears as a recipient with a self marker
      -- carrying a contact name, prefer that (it is usually the fuller
      -- profile/system name).
      selfContactName = do
        sid <- idxSelfId base
        r <- Map.lookup sid (idxRecipients base)
        c <- rcpContact r
        let nm = contactName c
        if nm == "Unknown" then Nothing else Just nm
   in base {idxSelfName = fromMaybe (idxSelfName base) selfContactName}

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

resolveAuthorName :: Index -> Maybe Text -> Text
resolveAuthorName idx authorId
  | Just sid <- idxSelfId idx, authorId == Just sid = idxSelfName idx
  | Just r <- authorId >>= (`Map.lookup` idxRecipients idx) =
      recipientDisplayName r (idxSelfName idx)
  | otherwise = "User " <> fromMaybe "" authorId

renderReactions :: Index -> [Reaction] -> Maybe Text
renderReactions _ [] = Nothing
renderReactions idx reactions =
  Just $
    ("    – reactions: " <>) $
      T.intercalate "  " $
        map one (sortOn reSentTimestamp reactions)
  where
    one r =
      fromMaybe "" (reEmoji r)
        <> " "
        <> resolveAuthorName idx (unId <$> reAuthorId r)

renderAttachments :: Map Text FilePath -> [Attachment] -> Text
renderAttachments fileIndex attachments =
  T.intercalate "\n" (mapMaybe one attachments)
  where
    one att =
      let mhash = attachmentHash att
          msrc = mhash >>= (`Map.lookup` fileIndex)
          ctype = attPointer att >>= pContentType
       in case (mhash, msrc) of
            -- resolved on disk: embed the real filename (correct extension)
            (Just h, Just src) ->
              Just ("![[files/" <> T.take 2 h <> "/" <> T.pack (takeFileName src) <> "]]")
            -- derivable but not present locally: placeholder, no broken link
            (Just _, Nothing) ->
              Just ("*[attachment unavailable" <> maybe "" (": " <>) ctype <> "]*")
            -- no locator at all: nothing renderable
            _ -> Nothing

-- | Render one chatItem to a Markdown block, or Nothing if nothing to show.
renderChatItem :: Index -> Map Text FilePath -> ChatItem -> IO (Maybe Text)
renderChatItem idx fileIndex ci = case ciStandardMessage ci of
  Nothing -> pure Nothing
  Just sm -> do
    time <- traverse fmtTs (ciDateSent ci)
    let author = resolveAuthorName idx (unId <$> ciAuthorId ci)
        headLine = "**" <> author <> "** · " <> fromMaybe "" time
        bodyLines = [b | Just b <- [smBody sm], not (blank b)]
        attLines = [renderAttachments fileIndex (smAttachments sm) | not (null (smAttachments sm))]
        reactLines = maybe [] pure (renderReactions idx (smReactions sm))
    pure (Just (T.intercalate "\n" (headLine : bodyLines ++ attLines ++ reactLines)))

-- ---------------------------------------------------------------------------
-- CLI / conversion
-- ---------------------------------------------------------------------------

data Args = Args
  { argPositional :: [String]
  , argFilesRoot :: Maybe FilePath
  , argCopyMedia :: Bool
  }
  deriving (Show, Eq)

parseArgs :: [String] -> Args
parseArgs = go (Args [] Nothing False)
  where
    go acc [] = acc {argPositional = reverse (argPositional acc)}
    go acc ("--files-root" : rest) = case rest of
      (v : rs) -> go acc {argFilesRoot = Just v} rs
      [] -> go acc {argFilesRoot = Nothing} []
    go acc ("--copy-media" : rs) = go acc {argCopyMedia = True} rs
    go acc (x : rs) = go acc {argPositional = x : argPositional acc} rs

data Options = Options
  { optInPath :: FilePath
  , optOutDir :: FilePath
  , optFilesRoot :: Maybe FilePath
  , optCopyMedia :: Bool
  , optVerbose :: Bool
  }
  deriving (Show)

data Summary = Summary
  { sumConversations :: Int
  , sumMissing :: Int
  , sumIndexed :: Int
  }
  deriving (Show, Eq)

-- | Core conversion. Reads the export, writes one Markdown file per
-- conversation to the output dir, returns a summary. Does NOT print usage or
-- exit — that is the CLI's job. Throws if the input does not exist.
convert :: Options -> IO Summary
convert opts = do
  let inPath = optInPath opts
      outDir = optOutDir opts
  inExists <- doesPathExist inPath
  unless inExists $ ioError (userError ("Input file not found: " <> inPath))
  createDirectoryIfMissing True outDir
  filesRoot <- case optFilesRoot opts of
    Just fr -> pure fr
    Nothing -> takeDirectory <$> makeAbsolute inPath
  fileIndex <- buildFileIndex filesRoot
  when (optVerbose opts) $
    printf "Indexed %d media file(s) under %s/files/\n" (Map.size fileIndex) filesRoot
  raw <- BS.readFile inPath
  records <-
    forM (filter (not . BC.all isSpace) (BC.lines raw)) $
      either (ioError . userError) pure . A.eitherDecodeStrict
  let idx = buildIndexes records
      chatKey ci = maybe "" unId (ciChatId ci)
      -- per chat, items end up newest-first (reverse of file order)
      itemsByChat =
        List.foldl'
          ( \m rec -> case recChatItem rec of
              Just ci -> Map.insertWith (<>) (chatKey ci) [ci] m
              Nothing -> m
          )
          Map.empty
          records
  missingRef <- newIORef (0 :: Int)
  forM_ (Map.toList itemsByChat) $ \(chatId, itemsNewestFirst) -> do
    let mRecipient = Map.lookup chatId (idxChats idx) >>= (`Map.lookup` idxRecipients idx)
        convName = case mRecipient of
          Just r -> recipientDisplayName r (idxSelfName idx)
          Nothing -> "chat-" <> chatId
        fname = T.unpack (sanitizeFilename convName) <> ".md"
        n = length itemsNewestFirst
        header = "# " <> convName <> "\n\n_" <> T.pack (show n) <> " messages_\n"
    blocks <- catMaybes <$> mapM (renderChatItem idx fileIndex) itemsNewestFirst
    forM_ itemsNewestFirst $ \ci ->
      forM_ (maybe [] smAttachments (ciStandardMessage ci)) $ \att ->
        case resolveOnDisk fileIndex att of
          Nothing -> modifyIORef' missingRef (+ 1)
          Just src -> when (optCopyMedia opts) $ do
            let h = fromMaybe "" (attachmentHash att)
                dst = outDir </> "files" </> T.unpack (T.take 2 h) </> takeFileName src
            createDirectoryIfMissing True (takeDirectory dst)
            dstExists <- doesPathExist dst
            unless dstExists $ copyFile src dst
    let content = header <> "\n" <> T.intercalate "\n\n---\n\n" blocks <> "\n"
    BS.writeFile (outDir </> fname) (TE.encodeUtf8 content)
    when (optVerbose opts) $ printf "Wrote %s  (%d messages)\n" fname n
  missing <- readIORef missingRef
  when (optVerbose opts) $ do
    printf "\nDone. %d conversation file(s) in %s\n" (Map.size itemsByChat) outDir
    when (missing > 0) $
      printf
        "Note: %d attachment file(s) referenced were not found under %s/files/ — links are still written.\n"
        missing
        filesRoot
  pure
    Summary
      { sumConversations = Map.size itemsByChat
      , sumMissing = missing
      , sumIndexed = Map.size fileIndex
      }

{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for signal-to-obsidian, ported 1:1 from the original Clojure
-- suite. Run with: cabal test
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import SignalToObsidian
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

-- Decode a JSON literal into one of the export record types. The literals in
-- this file are ASCII-only (emoji written as \uXXXX escapes) because Char8
-- string literals truncate non-Latin-1 characters.
dec :: A.FromJSON a => BL.ByteString -> a
dec s = either (error . ("bad test JSON: " <>)) id (A.eitherDecode s)

-- Position of the first occurrence of needle in hay (length of hay if absent).
indexOf :: Text -> Text -> Int
indexOf needle hay = T.length (fst (T.breakOn needle hay))

infixOfM :: Text -> Maybe Text -> Bool
infixOfM needle = maybe False (needle `T.isInfixOf`)

-- Verified-on-disk locator for files/5e/5ec3...pdf (see original test suite).
locatorPdf :: BL.ByteString
locatorPdf =
  "{\"plaintextHash\":\"7M+2gah3LOVsCOGJJ/x9wy31FGU8fbWp4mNnuMdbFSw=\",\
  \\"localKey\":\"nAdZxo2Hgz+hjD9fiffG+SrPZiQbq8tMbP1SlKHRFoG5fiL8v7+v2owFuG9gk8UalI5OQHCtLorkOIyC0/LgrA==\"}"

hashPdf :: Text
hashPdf = "5ec395356d6e3cab7a8618a5ecf42388a674df19c16818141af65a64fbbde474"

-- Verified-on-disk locator for files/dd/ddaf...jpg.
locatorJpg :: BL.ByteString
locatorJpg =
  "{\"plaintextHash\":\"tOdFThsY7/ELT8TagOBtRLfZY7nVRqMv8br1IfiKZnQ=\",\
  \\"localKey\":\"SveXa4aGRrd91d7XgVrMVZHDJLjZSvG1v53QzFXGgXbs0yVwvONkZCtMFlFm4vA2WT8RbDbHhWFyFgRjj8VKtw==\"}"

attPdf :: Attachment
attPdf =
  dec $
    "{\"pointer\":{\"contentType\":\"application/pdf\",\"fileName\":\"report.pdf\",\"locatorInfo\":"
      <> locatorPdf
      <> "}}"

main :: IO ()
main = hspec $ do
  -- -------------------------------------------------------------------------
  -- 1. Attachment path derivation — the heart of the tool.
  --    These vectors were verified against real on-disk files; they double as
  --    regression tests against the hex-vs-base64 bug hit during development.
  -- -------------------------------------------------------------------------
  describe "localBackupFilename" $ do
    it "derives verified on-disk hex names from base64 plaintextHash + localKey" $ do
      localBackupFilename (dec locatorPdf) `shouldBe` Just hashPdf
      localBackupFilename (dec locatorJpg)
        `shouldBe` Just "ddaf67507faf47ba526e1079a03c845341cbf2deb57e93c71de1c2f425c7451f"
      localBackupFilename
        ( dec
            "{\"plaintextHash\":\"bptmOyPGh6/rFaYNo2kqLfGZ83hdsLBlNMqG48DhF4s=\",\
            \\"localKey\":\"732vBCDL+iwOhEvjK/YQpfiVzfTh39RtBh3q3VGj8wa46zqAej5sBTVrQixD/SrPdYQcPp/+R4aTCq7/cn/g8Q==\"}"
        )
        `shouldBe` Just "42a90b883bbb3e53536687ebf9869765a5b3d332bf7ad64e630faab2a671142d"

    it "output is always 64 lowercase hex chars" $ do
      let n = fromMaybe "" (localBackupFilename (dec locatorJpg))
      T.length n `shouldBe` 64
      n `shouldSatisfy` T.all (`elem` ("0123456789abcdef" :: String))

    it "missing locator fields -> Nothing (not an exception)" $ do
      localBackupFilename (dec "{\"plaintextHash\":\"abc\"}") `shouldBe` Nothing
      localBackupFilename (dec "{\"localKey\":\"abc\"}") `shouldBe` Nothing
      localBackupFilename (dec "{}") `shouldBe` Nothing

  describe "attachmentHash / attachmentPath" $ do
    it "attachmentHash returns the bare 64-char stem" $
      attachmentHash attPdf `shouldBe` Just hashPdf

    it "attachmentPath shards by first two chars and appends extension" $
      attachmentPath attPdf `shouldBe` Just ("files/5e/" <> hashPdf <> ".pdf")

    it "no locator -> Nothing" $ do
      let att = dec "{\"pointer\":{\"contentType\":\"image/png\"}}" :: Attachment
      attachmentHash att `shouldBe` Nothing
      attachmentPath att `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- 2. Extension picking
  -- -------------------------------------------------------------------------
  describe "extFor" $ do
    it "fileName suffix wins when present and short" $ do
      extFor (Just "application/octet-stream") (Just "doc.pdf") `shouldBe` "pdf"
      extFor Nothing (Just "photo.JPG") `shouldBe` "jpg" -- lowercased
    it "falls back to contentType subtype" $ do
      extFor (Just "image/png") Nothing `shouldBe` "png"
      extFor (Just "image/jpeg") Nothing `shouldBe` "jpg" -- normalised jpeg->jpg
      extFor (Just "video/quicktime") Nothing `shouldBe` "mov" -- normalised
      extFor (Just "image/svg+xml") Nothing `shouldBe` "svg" -- normalised
    it "unknown -> bin" $ do
      extFor Nothing Nothing `shouldBe` "bin"
      extFor (Just "garbage-no-slash") Nothing `shouldBe` "bin"
    it "absurdly long 'extension' is ignored in favour of contentType" $
      extFor (Just "image/png") (Just "file.thisisnotanextension") `shouldBe` "png"

  -- -------------------------------------------------------------------------
  -- 3. Name resolution and its fallback chain
  -- -------------------------------------------------------------------------
  describe "contactName" $ do
    it "system name preferred over profile name" $
      contactName
        ( dec
            "{\"systemGivenName\":\"Sys\",\"systemFamilyName\":\"Name\",\
            \\"profileGivenName\":\"Prof\",\"profileFamilyName\":\"Ile\"}"
        )
        `shouldBe` "Sys Name"
    it "profile name used when no system name" $
      contactName (dec "{\"profileGivenName\":\"Prof\",\"profileFamilyName\":\"Ile\"}")
        `shouldBe` "Prof Ile"
    it "partial names join cleanly without stray spaces" $ do
      contactName (dec "{\"systemFamilyName\":\"Jane\"}") `shouldBe` "Jane"
      contactName (dec "{\"profileGivenName\":\"Solo\"}") `shouldBe` "Solo"
    it "e164 used when no names at all" $
      contactName (dec "{\"e164\":\"491635981282\"}") `shouldBe` "491635981282"
    it "Unknown when nothing is present" $
      contactName (dec "{}") `shouldBe` "Unknown"

  describe "groupTitle" $ do
    it "reads snapshot.title.title" $
      groupTitle (dec "{\"snapshot\":{\"title\":{\"title\":\"Jane, Jim and Sally\"}}}")
        `shouldBe` "Jane, Jim and Sally"
    it "Unknown Group when absent" $ do
      groupTitle (dec "{\"snapshot\":{}}") `shouldBe` "Unknown Group"
      groupTitle (dec "{}") `shouldBe` "Unknown Group"

  describe "recipientDisplayName" $ do
    it "self -> provided self-name" $
      recipientDisplayName (dec "{\"self\":{},\"id\":\"166\"}") "Me" `shouldBe` "Me"
    it "contact -> contactName" $
      recipientDisplayName (dec "{\"contact\":{\"profileGivenName\":\"Alice\"},\"id\":\"3\"}") "Me"
        `shouldBe` "Alice"
    it "group -> group title" $
      recipientDisplayName
        (dec "{\"group\":{\"snapshot\":{\"title\":{\"title\":\"Trip\"}}},\"id\":\"9\"}")
        "Me"
        `shouldBe` "Trip"
    it "unknown recipient kind -> Recipient <id>" $
      recipientDisplayName (dec "{\"id\":\"7\",\"distributionList\":{}}") "Me"
        `shouldBe` "Recipient 7"

  describe "resolveAuthorName" $ do
    let idx =
          Index
            { idxRecipients =
                Map.fromList
                  [ ("166", dec "{\"self\":{},\"id\":\"166\"}")
                  , ("22", dec "{\"contact\":{\"profileGivenName\":\"Jane\"},\"id\":\"22\"}")
                  ]
            , idxChats = Map.empty
            , idxSelfId = Just "166"
            , idxSelfName = "Me"
            }
    it "self id -> self name" $ resolveAuthorName idx (Just "166") `shouldBe` "Me"
    it "known recipient -> display name" $ resolveAuthorName idx (Just "22") `shouldBe` "Jane"
    it "unknown author id -> User <id>, never throws" $
      resolveAuthorName idx (Just "999") `shouldBe` "User 999"

  -- -------------------------------------------------------------------------
  -- 4. Filename sanitization
  -- -------------------------------------------------------------------------
  describe "sanitizeFilename" $ do
    it "leaves ordinary names alone" $
      sanitizeFilename "Jane, Jim and Sally" `shouldBe` "Jane, Jim and Sally"
    it "illegal filesystem characters stripped (segments fuse, no space inserted)" $ do
      sanitizeFilename "a/b:c*d" `shouldBe` "abcd"
      sanitizeFilename "name<>:\"/\\|?*part" `shouldBe` "namepart"
    it "whitespace collapsed and trimmed" $
      sanitizeFilename "  a    b  " `shouldBe` "a b"
    it "empty / all-illegal falls back to a default" $ do
      sanitizeFilename "" `shouldBe` "conversation"
      sanitizeFilename "/////" `shouldBe` "conversation"
      sanitizeFilename "   " `shouldBe` "conversation"

  describe "assignFilenames" $ do
    it "unique names are unchanged" $
      assignFilenames [("1", "Alex"), ("2", "Weekend Trip")]
        `shouldBe` Map.fromList [("1", "Alex.md"), ("2", "Weekend Trip.md")]
    it "identical names -> every one suffixed with its chat id" $
      assignFilenames [("5", "Unknown"), ("9", "Unknown"), ("3", "Sam")]
        `shouldBe` Map.fromList [("5", "Unknown (5).md"), ("9", "Unknown (9).md"), ("3", "Sam.md")]
    it "case-only difference collides (case-insensitive filesystems)" $
      assignFilenames [("1", "Alex"), ("2", "alex")]
        `shouldBe` Map.fromList [("1", "Alex (1).md"), ("2", "alex (2).md")]
    it "names equal only after sanitizing collide" $
      assignFilenames [("1", "a/b"), ("2", "ab")]
        `shouldBe` Map.fromList [("1", "ab (1).md"), ("2", "ab (2).md")]
    it "a real name equal to another chat's suffixed name is suffixed too" $
      assignFilenames [("5", "Unknown"), ("6", "Unknown"), ("7", "Unknown (5)")]
        `shouldBe` Map.fromList
          [("5", "Unknown (5).md"), ("6", "Unknown (6).md"), ("7", "Unknown (5) (7).md")]
    it "result does not depend on input order" $
      assignFilenames [("9", "X"), ("1", "x")] `shouldBe` assignFilenames [("1", "x"), ("9", "X")]

  -- -------------------------------------------------------------------------
  -- 5. Timestamp parsing (Millis accepts both string and numeric JSON)
  -- -------------------------------------------------------------------------
  describe "Millis parsing" $ do
    it "parses a string timestamp" $
      A.decode "\"1781607348551\"" `shouldBe` Just (Millis 1781607348551)
    it "tolerates numeric too" $
      A.decode "1781607348551" `shouldBe` Just (Millis 1781607348551)
    it "null field -> Nothing" $ do
      let ci = dec "{\"chatId\":\"1\",\"dateSent\":null}" :: ChatItem
      ciDateSent ci `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- 6. Index building from records (the recipient/chat lookup tables)
  -- -------------------------------------------------------------------------
  describe "buildIndexes" $ do
    let records :: [Record]
        records =
          dec
            "[{\"version\":1},\
            \{\"account\":{\"givenName\":\"Pete\"}},\
            \{\"recipient\":{\"id\":\"166\",\"self\":{},\"contact\":{\"profileGivenName\":\"Pete\",\"profileFamilyName\":\"Stone\"}}},\
            \{\"recipient\":{\"id\":\"22\",\"contact\":{\"profileGivenName\":\"Jane\"}}},\
            \{\"recipient\":{\"id\":\"175\",\"group\":{\"snapshot\":{\"title\":{\"title\":\"Grp\"}}}}},\
            \{\"chat\":{\"id\":\"175\",\"recipientId\":\"175\"}},\
            \{\"chat\":{\"id\":\"7\",\"recipientId\":\"22\"}},\
            \{\"chatItem\":{\"chatId\":\"175\"}}]"
        idx = buildIndexes records
    it "finds the self id" $ idxSelfId idx `shouldBe` Just "166"
    it "self recipient's full contact name is preferred over account givenName" $
      idxSelfName idx `shouldBe` "Pete Stone"
    it "maps chats to recipient ids" $ do
      Map.lookup "175" (idxChats idx) `shouldBe` Just "175"
      Map.lookup "7" (idxChats idx) `shouldBe` Just "22"
    it "stores recipients by id" $
      (contactName <$> (Map.lookup "22" (idxRecipients idx) >>= rcpContact))
        `shouldBe` Just "Jane"
    it "falls back to account name when self recipient has no contact name" $ do
      let idx' =
            buildIndexes $
              dec
                "[{\"account\":{\"givenName\":\"Solo\",\"familyName\":\"Person\"}},\
                \{\"recipient\":{\"id\":\"1\",\"self\":{}}}]"
      idxSelfName idx' `shouldBe` "Solo Person"
    it "defaults to Me when nothing provides a name" $ do
      let idx' = buildIndexes (dec "[{\"recipient\":{\"id\":\"1\",\"self\":{}}}]")
      idxSelfName idx' `shouldBe` "Me"

  -- -------------------------------------------------------------------------
  -- 7. Reaction rendering — chronological order regardless of input order
  -- -------------------------------------------------------------------------
  describe "renderReactions" $ do
    let idx =
          Index
            { idxRecipients =
                Map.fromList
                  [ ("1", dec "{\"contact\":{\"profileGivenName\":\"Early\"},\"id\":\"1\"}")
                  , ("2", dec "{\"contact\":{\"profileGivenName\":\"Late\"},\"id\":\"2\"}")
                  ]
            , idxChats = Map.empty
            , idxSelfId = Nothing
            , idxSelfName = "Me"
            }
        -- deliberately out of order in the input (🎉 = tada, 👍 = thumbs up)
        reactions :: [Reaction]
        reactions =
          dec
            "[{\"emoji\":\"\\ud83c\\udf89\",\"authorId\":\"2\",\"sentTimestamp\":\"2000\"},\
            \{\"emoji\":\"\\ud83d\\udc4d\",\"authorId\":\"1\",\"sentTimestamp\":\"1000\"}]"
        out = fromMaybe "" (renderReactions idx reactions)
    it "earlier reaction appears before later one" $ do
      indexOf "👍" out `shouldSatisfy` (< indexOf "🎉" out)
      out `shouldSatisfy` T.isInfixOf "👍 Early"
      out `shouldSatisfy` T.isInfixOf "🎉 Late"
    it "empty reactions -> Nothing" $
      renderReactions idx [] `shouldBe` Nothing

  -- -------------------------------------------------------------------------
  -- 8. End-to-end rendering of a chat item
  -- -------------------------------------------------------------------------
  describe "renderChatItem" $ do
    it "renders author and body" $ do
      let idx =
            Index
              { idxRecipients = Map.fromList [("166", dec "{\"self\":{},\"id\":\"166\"}")]
              , idxChats = Map.empty
              , idxSelfId = Just "166"
              , idxSelfName = "Me"
              }
      out <-
        renderChatItem idx Map.empty $
          dec
            "{\"chatId\":\"175\",\"authorId\":\"166\",\"dateSent\":\"1781607348551\",\
            \\"outgoing\":{},\"standardMessage\":{\"text\":{\"body\":\"Hello world\"}}}"
      out `shouldSatisfy` infixOfM "**Me**"
      out `shouldSatisfy` infixOfM "Hello world"

    it "renders an incoming message with a reaction" $ do
      let idx =
            Index
              { idxRecipients =
                  Map.fromList
                    [ ("166", dec "{\"self\":{},\"id\":\"166\"}")
                    , ("76", dec "{\"contact\":{\"profileGivenName\":\"Jim\"},\"id\":\"76\"}")
                    ]
              , idxChats = Map.empty
              , idxSelfId = Just "166"
              , idxSelfName = "Me"
              }
      out <-
        renderChatItem idx Map.empty $
          dec
            "{\"chatId\":\"175\",\"authorId\":\"76\",\"dateSent\":\"1781607795234\",\
            \\"incoming\":{},\"standardMessage\":{\"text\":{\"body\":\"Have fun\"},\
            \\"reactions\":[{\"emoji\":\"\\ud83d\\udc4d\",\"authorId\":\"166\",\"sentTimestamp\":\"1781609673539\"}]}}"
      out `shouldSatisfy` infixOfM "**Jim"
      out `shouldSatisfy` infixOfM "Have fun"
      out `shouldSatisfy` infixOfM "👍 Me"

    it "attachment with no matching file -> placeholder, not a broken embed" $ do
      let idx = Index Map.empty Map.empty Nothing "Me"
      out <-
        renderChatItem idx Map.empty $ -- empty index -> nothing resolves
          dec $
            "{\"chatId\":\"1\",\"authorId\":\"5\",\"dateSent\":\"1000\",\"incoming\":{},\
            \\"standardMessage\":{\"attachments\":[{\"pointer\":{\"contentType\":\"image/jpeg\",\"locatorInfo\":"
              <> locatorJpg
              <> "}}]}}"
      out `shouldSatisfy` infixOfM "attachment unavailable"
      out `shouldSatisfy` infixOfM "image/jpeg"
      out `shouldSatisfy` (not . infixOfM "![[")

    it "a chatItem without standardMessage renders nothing" $ do
      let idx = Index Map.empty Map.empty Nothing "Me"
      out <-
        renderChatItem idx Map.empty $
          dec "{\"chatId\":\"1\",\"authorId\":\"1\",\"updateMessage\":{\"some\":\"group-event\"}}"
      out `shouldSatisfy` isNothing

  -- -------------------------------------------------------------------------
  -- 9. Filesystem round-trips: buildFileIndex and resolveOnDisk
  -- -------------------------------------------------------------------------
  describe "buildFileIndex / resolveOnDisk" $ do
    it "indexes real files, skips hidden ones, resolves attachments" $
      withSystemTempDirectory "sig-test-" $ \tmp -> do
        -- Lay out a fake backup tree: files/<xx>/<hash>.<ext> plus a .DS_Store
        let sub = tmp </> "files" </> T.unpack (T.take 2 hashPdf)
        createDirectoryIfMissing True sub
        writeFile (sub </> T.unpack hashPdf <> ".pdf") "fake pdf bytes"
        -- a hidden file that find(1) would count but the walk should skip
        writeFile (tmp </> "files" </> ".DS_Store") "junk"
        index <- buildFileIndex tmp
        -- index keys are extension-stripped hash stems; .DS_Store excluded
        Map.member hashPdf index `shouldBe` True
        Map.size index `shouldBe` 1
        -- resolveOnDisk finds the real file regardless of extension
        let hit = resolveOnDisk index attPdf
        hit `shouldSatisfy` isJust
        maybe "" takeFileName hit `shouldSatisfy` (\f -> ".pdf" `T.isSuffixOf` T.pack f)
        -- unresolvable attachment -> Nothing
        let attOther =
              dec ("{\"pointer\":{\"locatorInfo\":" <> locatorJpg <> "}}") :: Attachment
        resolveOnDisk index attOther `shouldBe` Nothing

    it "missing files/ dir -> empty index, no throw" $
      withSystemTempDirectory "sig-test-" $ \tmp -> do
        index <- buildFileIndex (tmp </> "does-not-exist")
        index `shouldBe` Map.empty

  -- -------------------------------------------------------------------------
  -- 10. Argument parsing
  -- -------------------------------------------------------------------------
  describe "parseArgs" $ do
    it "separates positional args from flags" $ do
      let a = parseArgs ["in.jsonl", "out", "--files-root", "/b", "--copy-media"]
      argPositional a `shouldBe` ["in.jsonl", "out"]
      argFilesRoot a `shouldBe` Just "/b"
      argCopyMedia a `shouldBe` True
    it "defaults" $ do
      let a = parseArgs ["in.jsonl", "out"]
      argFilesRoot a `shouldBe` Nothing
      argCopyMedia a `shouldBe` False

  -- -------------------------------------------------------------------------
  -- 11. End-to-end integration: convert the bundled fixture.
  -- -------------------------------------------------------------------------
  describe "integration (fixture)" $
    it "converts test/fixtures/sample.jsonl" $
      withSystemTempDirectory "sig-int-" $ \out -> do
        -- files-root points at a dir with no files/, so attachments are
        -- "unavailable" — that's fine; we're testing text/structure/naming here.
        summary <-
          convert
            Options
              { optInPath = "test/fixtures/sample.jsonl"
              , optOutDir = out
              , optFilesRoot = Just out
              , optCopyMedia = False
              , optVerbose = False
              }
        -- convert returns a summary
        sumConversations summary `shouldBe` 2
        sumMissing summary `shouldBe` 1 -- the one PDF attachment, no files/ present
        -- expected per-conversation files are written
        doesFileExist (out </> "Alex Doe.md") `shouldReturn` True
        doesFileExist (out </> "Weekend Trip.md") `shouldReturn` True
        -- DM content and reaction rendered, newest-first
        dm <- TE.decodeUtf8 <$> BS.readFile (out </> "Alex Doe.md")
        dm `shouldSatisfy` T.isInfixOf "# Alex Doe"
        dm `shouldSatisfy` T.isInfixOf "_2 messages_"
        dm `shouldSatisfy` T.isInfixOf "Looking forward to it."
        dm `shouldSatisfy` T.isInfixOf "👍 Sam Example" -- reaction by self
        -- newest message ("Looking forward") should appear before the older one
        indexOf "Looking forward to it." dm
          `shouldSatisfy` (< indexOf "are we still on for Saturday" dm)
        -- group title, system-name author, and unavailable attachment
        grp <- TE.decodeUtf8 <$> BS.readFile (out </> "Weekend Trip.md")
        grp `shouldSatisfy` T.isInfixOf "# Weekend Trip"
        grp `shouldSatisfy` T.isInfixOf "**Riley**" -- systemFamilyName
        grp `shouldSatisfy` T.isInfixOf "attachment unavailable"
        grp `shouldSatisfy` T.isInfixOf "application/pdf"

  describe "integration (name collisions)" $
    it "two chats whose names both resolve to Unknown get separate files" $
      withSystemTempDirectory "sig-coll-" $ \tmp -> do
        let inPath = tmp </> "export.jsonl"
            out = tmp </> "out"
        BL.writeFile inPath $
          BL.unlines
            [ "{\"recipient\":{\"id\":\"1\",\"contact\":{}}}"
            , "{\"recipient\":{\"id\":\"2\",\"contact\":{}}}"
            , "{\"chat\":{\"id\":\"10\",\"recipientId\":\"1\"}}"
            , "{\"chat\":{\"id\":\"20\",\"recipientId\":\"2\"}}"
            , "{\"chatItem\":{\"chatId\":\"10\",\"authorId\":\"1\",\"dateSent\":\"1000\",\"standardMessage\":{\"text\":{\"body\":\"first\"}}}}"
            , "{\"chatItem\":{\"chatId\":\"20\",\"authorId\":\"2\",\"dateSent\":\"2000\",\"standardMessage\":{\"text\":{\"body\":\"second\"}}}}"
            ]
        summary <- convert (Options inPath out (Just tmp) False False)
        sumConversations summary `shouldBe` 2
        doesFileExist (out </> "Unknown.md") `shouldReturn` False
        a <- TE.decodeUtf8 <$> BS.readFile (out </> "Unknown (10).md")
        b <- TE.decodeUtf8 <$> BS.readFile (out </> "Unknown (20).md")
        a `shouldSatisfy` T.isInfixOf "first"
        b `shouldSatisfy` T.isInfixOf "second"

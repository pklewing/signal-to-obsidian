-- | Thin CLI wrapper around 'SignalToObsidian.convert'.
--
-- Usage:
--   signal-to-obsidian <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
module Main (main) where

import Control.Monad (unless, void)
import SignalToObsidian
import System.Directory (doesPathExist)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hSetEncoding, stderr, stdout, utf8)

usage :: String
usage = "Usage: signal-to-obsidian <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- parseArgs <$> getArgs
  case argPositional args of
    (inPath : outDir : _) -> do
      inExists <- doesPathExist inPath
      unless inExists $ do
        putStrLn ("Input file not found: " <> inPath)
        exitWith (ExitFailure 1)
      void $
        convert
          Options
            { optInPath = inPath
            , optOutDir = outDir
            , optFilesRoot = argFilesRoot args
            , optCopyMedia = argCopyMedia args
            , optVerbose = True
            }
    _ -> do
      putStrLn usage
      exitWith (ExitFailure 1)

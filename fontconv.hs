{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import           Control.Applicative
import           Control.Monad
import           Shelly
import qualified Data.String        as String
import qualified Data.Text          as Text
import qualified System.Environment as Environment
import qualified Filesystem.Path    as FilePath

default (Text.Text)

data Options = Options
                { mashapeKey    :: Maybe Text.Text
                , inFontPaths   :: [Text.Text]
                , outDirectory  :: Text.Text
                , outFormats    :: [Text.Text]
                }

defaultOptions :: Options
defaultOptions = Options
                    { mashapeKey    = Nothing
                    , inFontPaths   = []
                    , outDirectory  = "."
                    , outFormats    = []
                    }

parseArgs :: Options -> [String] -> Options
parseArgs opts ("-d":outDir:args) = parseArgs (opts { outDirectory = Text.pack outDir }) args
parseArgs opts ("-f":outFmt:args) = parseArgs (opts { outFormats = Text.pack outFmt : outFormats opts }) args
parseArgs opts ("-k":key:args) = parseArgs (opts { mashapeKey = Just (Text.pack key) }) args
parseArgs _ ("-d":[]) = error "option -d requires an argument"
parseArgs _ ("-f":[]) = error "option -f requires an argument"
parseArgs _ ("-k":[]) = error "option -k requires an argument"
parseArgs opts (font:args) = parseArgs (opts { inFontPaths = Text.pack font : inFontPaths opts }) args
parseArgs opts [] = opts

endpoint :: Text.Text
endpoint = "https://ofc.p.mashape.com/directConvert/"

main :: IO ()
main = do
  opts <- parseArgs defaultOptions <$> Environment.getArgs
  case mashapeKey opts of
    Nothing -> error "mashape key required"
    Just key ->
      case (inFontPaths opts, outFormats opts) of
        ([], _) ->
          -- TODO: Should I read font content from stdin?
          error "font path required"
        (_, []) ->
          error "out format required"
        (fonts, formats) -> do
          let dir = outDirectory opts
          shelly $ withTmpDir $ \tmpdir -> do
            forM_ fonts $ \font -> do
              forM_ formats $ \fmt -> do
                tmpdir_ <- toTextWarn tmpdir
                download_path <- toTextWarn (tmpdir </> "download.tar.gz")
                run "curl" [ "-X", "POST", endpoint
                             , "-H", Text.append "X-Mashape-Authorization: " key
                             , "-F", Text.append "file=@" font
                             , "-F", Text.append "format=" fmt
                             , "-o", download_path
                             ]
                run "tar" ["-xf", download_path, "-C", tmpdir_]
            let converted_files = tmpdir </> "onlinefontconverter.com" </> "converted-files"
            mkdir_p (fromText dir)
            ls converted_files >>= mapM_ (\outFont -> cp outFont (fromText dir </> FilePath.filename outFont))

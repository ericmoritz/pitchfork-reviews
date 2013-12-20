{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlArrow (ArrowXml)
import Text.HandsomeSoup
import Control.Arrow
import Text.Read (readMaybe)
import Control.Monad
import Control.Lens
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (sortBy, filter, intercalate)
import Control.Concurrent.Async (mapConcurrently)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)


data Album = Album {
  _artist :: String,
  _title :: String,
  _rank :: Maybe Int
  }
makeLenses ''Album

instance Show Album where
  show a = rankStr ++ " " ++ a^.artist ++ " - " ++ a^.title
    where
      rankStr = show $ fromMaybe 0 (a^.rank)

album = proc div -> do
  artist <- (css "h1" //> getText) -< div
  title <- (css "h2" //> getText) -< div
  rank <- (css ".rank" //> getText) -< div
  returnA -< Album artist title (readMaybe rank)
  

downloadAlbums :: String -> IO [Album]
downloadAlbums url = do
  print $ "downloading " ++ url
  runX $ index_doc >>> album_div_tags >>> album
  where
    index_doc = fromUrl url
    album_div_tags = css ".year-end-review"

main = do
  -- download the albums
  mapConcurrently downloadAlbums urls
  >>= return . join >>= display

  where
    urls = map (("http://pitchfork.com/features/staff-lists/9293-the-top-50-albums-of-2013/"++) . (++ "/") . show) [1..7]
    display :: [Album] -> IO ()
    display = mapM_ print

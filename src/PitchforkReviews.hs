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

data Album = Album {
  _artist :: String,
  _title :: String,
  _score :: Maybe Float,
  _url :: String
  }
makeLenses ''Album

instance Show Album where
  show a = scoreStr ++ " " ++ a^.artist ++ " - " ++ a^.title ++ " " ++ a^.url
    where
      scoreStr = show $ fromMaybe 0 (a^.score)

album = proc li -> do
  artist <- (css "h1" //> getText) -< li
  title <- (css "h2" //> getText) -< li
  url <- (css "a" ! "href") -< li
  returnA -< Album artist title Nothing $ "http://pitchfork.com" ++ url


albumScore :: ArrowXml a => a XmlTree (Maybe Float)
albumScore = css "span.score" //> getText >>> arr readMaybe


downloadAlbums = 
  runX $ index_doc >>> album_li_tags >>> album
  where
    index_doc = fromUrl "http://pitchfork.com/reviews/albums/"
    album_li_tags = css "ul.object-grid ul li"


downloadScore :: String -> IO (Maybe Float)
downloadScore url = 
  (join . listToMaybe) `liftM` runX (album_doc >>> albumScore) 
  where
    album_doc = fromUrl url

setScore :: Maybe Float -> Album -> Album
setScore = set score

main = do
  albums <- downloadAlbums
  -- Fetch the scores concurrently using async
  scores <- mapConcurrently downloadScoreForAlbum albums

  -- Join the albums with what we fetched concurrently,
  -- filter by the minimal score and then sort on the score
  display $ sortBy compareScore $
    filter (scoreGT 7) $
    zipWith setScore scores albums
  where
    downloadScoreForAlbum a = downloadScore (a^.url)
    compareScore b a = compare (a^.score) (b^.score)
    scoreGT n a = a^.score > Just n
    display = mapM_ print

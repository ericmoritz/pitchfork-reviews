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


album_score :: ArrowXml a => a XmlTree (Maybe Float)
album_score = css "span.score" //> getText >>> arr readMaybe


downloadAlbums = 
  runX $ index_doc >>> album_li_tags >>> album
  where
    index_doc = fromUrl "http://pitchfork.com/reviews/albums/"
    album_li_tags = css "ul.object-grid ul li"


downloadScore :: String -> IO (Maybe Float)
downloadScore url = do
  (return . join . listToMaybe) =<< (runX $ album_doc >>> album_score) 
  where
    album_doc = fromUrl $ url

setScore :: (Maybe Float) -> Album -> Album
setScore = set score


main = do
  -- Fetch the scores concurrently using parallel-io
  albums <- mapConcurrently updateScore =<< downloadAlbums

  -- Sort and filter the albums
  display $ sortAndFilter 7 albums
  where
    updateScore album = do
      score' <- (downloadScore $ album^.url)
      return $ setScore score' album
    sortAndFilter n = sortBy (\b a -> compare (a^.score) (b^.score)) .
                    filter (\a -> a^.score > Just n)
    display = mapM_ (putStrLn . show)

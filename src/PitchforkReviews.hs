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


downloadAlbums :: Int -> IO [Album]
downloadAlbums page = 
  runX $ index_doc >>> album_li_tags >>> album
  where
    index_doc = fromUrl $ "http://pitchfork.com/reviews/albums/" ++ show page ++ "/"
    album_li_tags = css "ul.object-grid ul li"


downloadScore :: Album -> IO Album
downloadScore album = do
  maybeScore <- (join . listToMaybe) `liftM` runX (album_doc >>> albumScore)
  return $ setScore maybeScore album
  where
    album_doc = fromUrl $ album^.url

setScore :: Maybe Float -> Album -> Album
setScore = set score

main = 
  -- download the albums
  (downloadAlbums =<< pageOpt)
  -- then fetch the scores for each album concurrently using async
  >>= mapConcurrently downloadScore
  -- then filter, sort and display the albums
  >>= (filter (scoreGT 7) >>> sortBy compareScore >>> display)

  where
    pageOpt :: IO Int
    pageOpt = return . fromMaybe 1 . (readMaybe <=< listToMaybe) =<< getArgs

    compareScore :: Album -> Album -> Ordering
    compareScore b a = compare (a^.score) (b^.score)

    scoreGT :: Float -> Album -> Bool
    scoreGT n a = a^.score > Just n

    display :: [Album] -> IO ()
    display = mapM_ print

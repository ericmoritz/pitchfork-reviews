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


data Album = Album {
  _artist :: String,
  _title :: String,
  _score :: Maybe Float,
  _pubDate :: String,
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
  pubDate <- (css "h4" //> getText) -< li
  returnA -< Album artist title Nothing pubDate $ "http://pitchfork.com" ++ url


albumScore :: ArrowXml a => a XmlTree (Maybe Float)
albumScore = css "span.score" //> getText >>> arr readMaybe


maybeFirstTag arrow = listToMaybe `liftM` runX arrow

nextPageUrl = css "span.next-container .next" ! "href"


downloadAllAlbums :: IO [Album]
downloadAllAlbums  =
  downloadAllAlbums' Nothing
  where 
    downloadAllAlbums' currentPage = do
      (albums, nextPage) <- downloadAlbums currentPage
      rest <- unsafeInterleaveIO $
              case nextPage of
                Nothing -> return []
                _       -> downloadAllAlbums' nextPage
      return $ albums ++ rest
    

downloadAlbums :: Maybe String -> IO ([Album], Maybe String)
downloadAlbums pageUrl = do
  print $ "downloading " ++ url
  albums <- runX $ index_doc >>> album_li_tags >>> album
  nextPage <- maybeFirstTag $ index_doc >>> nextPageUrl
  return (albums, liftM2 (++) (return "http://pitchfork.com") nextPage)
  where
    url = fromMaybe "http://pitchfork.com/reviews/albums/1/" pageUrl
    index_doc = fromUrl url
    album_li_tags = css "ul.object-grid ul li"


downloadScore :: Album -> IO Album
downloadScore album = do
  maybeScore <- return . join =<< maybeFirstTag (album_doc >>> albumScore)
  return $ setScore maybeScore album
  where
    album_doc = fromUrl $ album^.url

setScore :: Maybe Float -> Album -> Album
setScore = set score

main = 
  -- download the albums
  (liftM2 take) count downloadAllAlbums
  -- then fetch the scores for each album concurrently using async
  >>= mapConcurrently downloadScore
  -- then filter, sort and display the albums
  >>= (filter (scoreGT 7) >>> sortBy compareScore >>> display)

  where
    count :: IO Int
    count = return . fromMaybe 20 . (readMaybe <=< listToMaybe) =<< getArgs

    compareScore :: Album -> Album -> Ordering
    compareScore b a = compare (a^.score) (b^.score)

    scoreGT :: Float -> Album -> Bool
    scoreGT n a = a^.score > Just n

    display :: [Album] -> IO ()
    display = mapM_ print

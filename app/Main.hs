{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types (Status, statusCode, statusMessage)
import Data.Char
import Text.HTML.TagSoup

-- 39.3

buildRequest :: BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest host method path = setRequestMethod method
                              $ setRequestHost host
                              -- $ setRequestHeader "token" [token]
                              -- $ setRequestPath path
                              $ setRequestSecure False
                              $ setRequestPort 80
                              $ defaultRequest


request :: BC.ByteString -> BC.ByteString -> Request
request host path = buildRequest host "GET" path

openURL :: BC.ByteString -> BC.ByteString -> IO LC.ByteString -- IO (Either Status LC.ByteString)
openURL host path = do
  response <- httpLBS $ request host path
  let status = getResponseStatusCode response
  -- when (status /= 200) (Left <$> getResponseStatus response)
  putStrLn "Request ok"
  return $ getResponseBody response

lastmod :: String
lastmod = "<li id='footer-info-lastmod'>"

haskellLastModifiedDateTime :: IO ()
haskellLastModifiedDateTime = do
    src <- openURL "wiki.haskell.org" "Haskell"
    -- case src of
      -- Left l -> putStrLn "Error:" >> print l
      -- Right body -> do
    let lastModifiedDateTime = fromFooter $ parseTags src
    putStrLn $ "wiki.haskell.org was last modified on " ++ (LC.unpack lastModifiedDateTime)
      where fromFooter = LC.unwords . drop 6 . LC.words . innerText . take 2 . dropWhile (~/= lastmod)

{-
h xs = (LC.length xs == 2) && (LC.head xs == 'h') && (isDigit $ (LC.head . LC.tail) xs)

tagOpenName (TagOpen x _) = x; tagOpenName _ = ""

tagCloseName (TagClose x) = x; tagCloseName _ = ""

mconcat $ map (LC.words . innerText . takeWhile (~/= "</p>")) $ sections (~== "<p>") $ tags

mconcat $ map (LC.words .innerText . takeWhile (not . h . tagCloseName)) $ sections (h . tagOpenName) $ tags



-}

tagOpenName :: Tag LC.ByteString -> LC.ByteString
tagOpenName (TagOpen x _) = x
tagOpenName _ = ""

tagCloseName :: Tag LC.ByteString -> LC.ByteString
tagCloseName (TagClose x) = x
tagCloseName _ = ""

isNgramSeparator :: Char -> Bool
isNgramSeparator '.' = True
isNgramSeparator ',' = True
isNgramSeparator ':' = True
isNgramSeparator ';' = True
isNgramSeparator '!' = True
isNgramSeparator '?' = True
isNgramSeparator '(' = True
isNgramSeparator ')' = True
isNgramSeparator _ = False


paragraphData :: [Tag LC.ByteString] -> [[LC.ByteString]]
paragraphData = map (LC.words . innerText . takeWhile (~/= pCloseTag)) . sections (~== pOpenTag)
  where pOpenTag :: String
        pOpenTag = "<p>"
        pCloseTag :: String
        pCloseTag = "</p>"

headerData :: [Tag LC.ByteString] -> [LC.ByteString]
headerData = mconcat . map (LC.words .innerText . takeWhile (not . h . tagCloseName)) . sections (h . tagOpenName)
  where h xs = (LC.length xs == 2) && (LC.head xs == 'h') && (isDigit $ (LC.head . LC.tail) xs)

tagsIO = parseTags <$> openURL "wiki.haskell.org" "Haskell"

main :: IO ()
main = haskellLastModifiedDateTime

{-# LANGUAGE OverloadedStrings, OverloadedLabels, ImplicitParams #-}

import Data.Aeson

content1 :: ByteString
content1 = "[\"box.h\", \"Hello \", \"world\"]"

main :: IO ()
main = do
  print $ (decodeStrict content1 :: Maybe Value)

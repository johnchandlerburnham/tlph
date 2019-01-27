{-# LANGUAGE TypeApplications #-}

import           Data.Typeable

test :: IO ()
test = do
  print $ typeOf $ fmap @Maybe

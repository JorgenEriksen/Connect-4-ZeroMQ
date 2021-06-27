-- | Testing module for the connect 4 game. We don't need to have any hspec tests because they would be literal copy of doctests. So we are covered in that aspect.

import Lib ()
import Test.DocTest (doctest)

main :: IO ()
main =
  -- Let us run first all the doctests from our source files
  doctest
    ["-isrc", "app/Main.hs"]

module Kata.TheSumOfThings where

import           Control.Arrow
import           Data.Char     (ord)
import           Data.List     (elemIndex)
import           Data.Maybe    (fromMaybe)
import           Prelude

main = interact
  (lines >>> map (words >>> map readThing >>> sum >>> show) >>> unlines)

unascii = sum . map ord

readThing thing = fromMaybe (unascii thing) (go 0 n)
 where
  n              = drop ps thing
  (ps, alphabet) = case take 2 thing of
    "0b" -> (2, "01")
    "0o" -> (2, ['0' .. '7'])
    "0x" -> (2, ['0' .. '9'] <> ['a' .. 'f'])
    _    -> (0, ['0' .. '9'])
  base = length alphabet
  go s []       = pure s
  go s (d : ds) = do
    thisDigit <- elemIndex d alphabet
    go (s * base + thisDigit) ds

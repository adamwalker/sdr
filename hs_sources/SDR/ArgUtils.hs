{-| Utilities for parsing command line arguments that might be useful when writing a SDR application. Based on the optparse-applicative library. -}
module SDR.ArgUtils (
    parseSize
    ) where

import Options.Applicative
import Data.Decimal

{-| Parse a number that may have a decimal point and a suffix, e.g. 2.56M -}
parseSize :: ReadM Integer
parseSize = eitherReader $ \arg -> case reads arg of
    [(r, suffix)] -> case suffix of 
        []  -> return $ round (r :: Decimal)
        "K" -> return $ round $ r * 1000 
        "M" -> return $ round $ r * 1000000
        "G" -> return $ round $ r * 1000000000
        x   -> Left  $ "Cannot parse suffix: `" ++ x ++ "'"
    _             -> Left $ "Cannot parse value: `" ++ arg ++ "'"


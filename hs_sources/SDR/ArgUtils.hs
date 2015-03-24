module SDR.ArgUtils (
    parseSize
    ) where

import Options.Applicative
import Data.Decimal

parseSize :: ReadM Integer
parseSize = eitherReader $ \arg -> case reads arg of
    [(r, suffix)] -> case suffix of 
        []  -> return $ round (r :: Decimal)
        "K" -> return $ round $ r * 1000 
        "M" -> return $ round $ r * 1000000
        "G" -> return $ round $ r * 1000000000
        x   -> Left  $ "Cannot parse suffix: `" ++ x ++ "'"
    _             -> Left $ "Cannot parse value: `" ++ arg ++ "'"


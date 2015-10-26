module CommonParsers where 

import Control.Monad (MonadPlus(..), liftM)

-- | A generic data type for dealing with parsers.
--   Actually, this is an idiom, being present in several Haskell parsers.
--   Since parsers usually have to perform some kind of IO, the ParserResult 
--   data type shuld be an instance of Monad.
-- 
--   As a final remark, the result of a parser might be either a Sucess or a Fail.
    
data ParserResult a = Success a | Fail String
 deriving (Read, Show, Eq, Ord)

instance Monad ParserResult where 
 return = Success
 fail = Fail
 Success a >>= f = f a
 Fail s    >>= f = Fail s

instance Functor ParserResult where 
 fmap = liftM

instance MonadPlus ParserResult where
  mzero = Fail "Err.mzero"
  mplus (Fail _) y = y
  mplus x _ = x

isSuccess :: ParserResult a -> Bool
isSuccess (Success _) = True
isSuccess (Fail _) = False

showError :: ParserResult a -> String
showError (Success _) = ""
showError (Fail s) = s 


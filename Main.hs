{-# LANGUAGE 
  GeneralizedNewtypeDeriving
#-}

import Prelude hiding (lookup)
import Data.IORef
import Data.Map

newtype Dollar = Dollar Double deriving (Show, Eq, Ord, Num)
newtype AccountId = AccountId Int deriving (Show, Eq, Ord)

type AccountMap = Map AccountId Dollar

modifyAndPrint 
  :: (Show state, Show result) 
  => IORef state
  -> (state -> (state, result))
  -> IO ()
modifyAndPrint state transform = do
  s1 <- readIORef state
  let (s2, result) = transform s1
  writeIORef state s2
  putStrLn $ show result ++ ": " ++ show s2
  

main :: IO ()
main = do
  accountMap <- newIORef (empty :: AccountMap)
  modifyAndPrint accountMap $ createAccount (AccountId 2)
  modifyAndPrint accountMap $ deposit (AccountId 2) (Dollar 3.2)
  modifyAndPrint accountMap $ deposit (AccountId 1) (Dollar 1)

data CreateResult = Created | AlreadyExists deriving Show

deposit :: AccountId -> Dollar -> AccountMap -> (AccountMap, Maybe Dollar)
deposit accId amt m = case accId `lookup` m of 
  Just old -> (adjust (+amt) accId m, Just $ old + amt)
  Nothing -> (m, Nothing)

createAccount :: AccountId -> AccountMap -> (AccountMap, CreateResult)
createAccount accId m = 
  if accId `member` m 
  then (m, AlreadyExists) 
  else (insert accId (Dollar 0) m, Created)

{-# LANGUAGE 
  GeneralizedNewtypeDeriving
#-}

import Prelude hiding (lookup)
import Data.Map

newtype Dollar = Dollar Double deriving (Show, Eq, Ord, Num)
newtype AccountId = AccountId Int deriving (Show, Eq, Ord)

type AccountMap = Map AccountId Dollar

main :: IO ()
main = do
  let m1 = empty :: AccountMap
  putStrLn $ show m1
  let (m2, r2) = createAccount (AccountId 2) m1
  putStrLn $ show r2 ++ ": " ++ show m2
  let (m3, r3) = createAccount (AccountId 3) m2
  putStrLn $ show r3 ++ ": " ++ show m3
  let (m4, r4) = createAccount (AccountId 2) m3
  putStrLn $ show r4 ++ ": " ++ show m4

data CreateResult = Created | AlreadyExists deriving Show

createAccount :: AccountId -> AccountMap -> (AccountMap, CreateResult)
createAccount accId m = 
  if accId `member` m 
  then (m, AlreadyExists) 
  else (insert accId (Dollar 0) m, Created)

{-# LANGUAGE 
  DataKinds,
  TypeOperators,
  GeneralizedNewtypeDeriving,
  FlexibleContexts,
  FlexibleInstances,
  OverloadedStrings
#-}

import Prelude hiding (lookup)

import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import Data.Map
import Network.Wai.Handler.Warp
import Servant

newtype Dollar = Dollar Double deriving (Show, Eq, Ord, Num, FromHttpApiData, ToJSON)
newtype AccountId = AccountId Int deriving (Show, Eq, Ord, FromHttpApiData, ToJSON)

type AccountMap = Map AccountId Dollar

type CreateAPI 
   = "createAccount" 
  :> Capture "accountId" AccountId 
  :> PostNoContent '[JSON] NoContent

type DepositAPI
   = "deposit" 
  :> Capture "accountId" AccountId 
  :> Capture "amount" Dollar
  :> Post '[JSON] Dollar

type ListAPI 
   = "list" 
  :> Get '[JSON] [(AccountId, Dollar)]

main :: IO ()
main = do
  accountMap <- newIORef (empty :: AccountMap)

  let createAccount' :: AccountId -> Handler NoContent
      createAccount' accId = do
        result <- liftIO $ 
          atomicModifyIORef accountMap (createAccount accId)
        case result of
          Created -> return NoContent
          AlreadyExists -> throwError err409

      deposit' accId amt = do
        mbNewAmount <- liftIO $ 
          atomicModifyIORef accountMap (deposit accId amt)
        maybe (throwError err400) return mbNewAmount
      
      list = liftIO $ toList <$> readIORef accountMap

  run 8080 $ serve api $ createAccount' :<|> deposit' :<|> list

type API = CreateAPI :<|> DepositAPI :<|> ListAPI

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

api :: Proxy API
api = Proxy

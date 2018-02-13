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
import Control.Lens hiding (from, to)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Aeson
import Data.IORef
import Data.Map
import Network.Wai.Handler.Warp
import Data.Semigroup
import Data.String (fromString)
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

type TransferAPI
   = "transfer" 
  :> Capture "fromId" AccountId 
  :> Capture "toId" AccountId 
  :> Capture "amount" Dollar
  :> PostNoContent '[JSON] NoContent

type ListAPI 
   = "list" 
  :> Get '[JSON] [(AccountId, Dollar)]

type PureCtx a 
  = StateT AccountMap (Except ServantErr) a

main :: IO ()
main = do
  accountMap <- newIORef (empty :: AccountMap)

  let createAccount' :: AccountId -> Handler NoContent
      createAccount' accId = 
        fromPure $ createAccount accId

      deposit' accId amt = 
        fromPure $ deposit accId amt

      transfer' from to amt = 
        fromPure $ transfer from to amt

      fromPure :: PureCtx r -> Handler r
      fromPure p = do
        result <- liftIO $ atomicModifyIORef accountMap $ \m ->
          case runExcept $ runStateT p m of
            Left err -> (m, Left err)
            Right (out, m') -> (m', Right out)
        case result of
          Left err -> throwError err
          Right r -> return r

      list = liftIO $ toList <$> readIORef accountMap

  run 8080 $ serve api $ 
    createAccount' :<|> deposit' :<|> transfer' :<|> list

type API = CreateAPI :<|> DepositAPI :<|> TransferAPI :<|> ListAPI

data CreateResult = Created | AlreadyExists deriving Show

transfer :: AccountId -> AccountId -> Dollar 
  -> PureCtx NoContent
transfer from to amt = do
  remaining <- at from <%= fmap (subtract amt)
  case remaining of
    Nothing -> doesntExistError "Source account"
    Just r | r < 0 -> throwError $ err412 { errBody = fromString $ "Source account is missing " ++ show (-r) ++ " dollars"}
           | otherwise -> return ()
  newAmt <- at to <%= fmap (+amt)
  case newAmt of
    Nothing -> doesntExistError "Target account"
    Just _ -> return NoContent
  where doesntExistError acc = throwError $ err400 { errBody = acc <> " doesn't exist" }

deposit :: AccountId -> Dollar -> PureCtx Dollar
deposit accId amt = do
  mbNewAmount <- at accId <%= fmap (+amt)
  maybe (throwError err400) return mbNewAmount

createAccount :: AccountId -> PureCtx NoContent
createAccount accId = do
  old <- at accId <<.= Just (Dollar 0)
  case old of
    Just _ -> throwError err409
    Nothing -> return NoContent

api :: Proxy API
api = Proxy

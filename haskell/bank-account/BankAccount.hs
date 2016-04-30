module BankAccount ( BankAccount, openAccount, 
    closeAccount, getBalance, incrementBalance ) where

import Control.Concurrent.MVar

data BankAccount = Account {balance :: MVar Int, open :: MVar Bool}

openAccount :: IO BankAccount
openAccount = do 
    b <- newMVar 0
    o <- newMVar True
    return Account { balance=b, open=o}

closeAccount :: BankAccount -> IO ()
closeAccount account = do
    o <- takeMVar . open $ account
    putMVar (open account) False

getBalance :: BankAccount -> IO (Maybe Int)
getBalance account = do
    o <- readMVar $ open account
    b <- readMVar $ balance account
    return $ if o
                then Just b
                else Nothing

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance account diff = do
    o <- readMVar $ open account
    b <- takeMVar $ balance account
    let newBalance = b + diff
    putMVar (balance account) newBalance
    return $ if o
                then Just newBalance
                else Nothing

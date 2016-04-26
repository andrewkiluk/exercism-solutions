module Robot (robotName, mkRobot, resetName) where

import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Monad (liftM)
import System.Random (randomRIO)

data Robot = Robot (TVar String)

mkRobot :: IO Robot
mkRobot = do 
    name <- generateName
    liftM Robot (atomically . newTVar $ name)

robotName :: Robot -> IO String
robotName (Robot name) = atomically . readTVar $ name

resetName :: Robot -> IO ()
resetName (Robot name) = do
    newName <- generateName
    atomically $ writeTVar name newName

generateName :: IO String
generateName = mapM randomRIO pattern where
    pattern = [letter, letter, number, number, number]
    letter = ('A', 'Z')
    number = ('0', '9')


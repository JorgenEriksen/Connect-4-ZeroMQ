{-# LANGUAGE OverloadedStrings #-}

-- |
-- main entry point that opens the tcp connection for receiving and replying communication.
--
-- Author(s): Elvis Arifagic & Jørgen Eriksen.
-- Date: 07.05.2021
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString.Char8 (unpack)
import Lib (checkIfWon)
import System.ZMQ4.Monadic
import Text.Printf

main :: IO ()
main = runZMQ $ do
  responder <- socket Rep -- Socket to talk to clients
  bind responder "tcp://*:5555"
  let message = ""
  forever $ do
    buffer <- receive responder
    liftIO $ do
      putStrLn (unpack buffer)
      threadDelay 1000000
    let map = unpack buffer

    let player = checkIfWon map '1'
    let player2 = checkIfWon map '2'
    if player || player2
      then send responder [] "won"
      else send responder [] "not won"
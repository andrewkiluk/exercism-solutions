module CryptoSquare (normalizePlaintext,
                     squareSize,
                     plaintextSegments,
                     ciphertext,
                     normalizeCiphertext) where

import Data.Char
import Data.List
import Data.List.Split

squareSize :: String -> Int
squareSize text = head $ dropWhile (squareTooSmall text) [1,2..]
    where squareTooSmall text n = length text > n^2

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

plaintextSegments :: String -> [String]
plaintextSegments input = chunksOf (squareSize normalized) normalized
    where normalized = normalizePlaintext input

ciphertext :: String -> String
ciphertext = concat . encipher

normalizeCiphertext :: String -> String
normalizeCiphertext input = unwords $ encipher input

encipher :: String -> [String]
encipher = transpose . plaintextSegments

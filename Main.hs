module Main where

import GHC.Word
import Data.Foldable
import Data.Bits (testBit)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS

type Model = Set (Word8, Word8, Word8)

loadModel :: BS.ByteString -> Model
loadModel bs = let r = bs `BS.index` 0 in
  Set.fromList 
  [ (x, y, z) 
  | x <- [0..r - 1]
  , y <- [0..r - 1]
  , z <- [0..r - 1]
  , let p = fromEnum x * fromEnum r * fromEnum r + fromEnum y * fromEnum r + fromEnum z
  , let (pw, pb) = p `divMod` 8
  , let b = bs `BS.index` succ pw
  , testBit b pb
  ]

printModel :: Model -> IO ()
printModel model =
  for_ [0 .. 20] $ \y -> do
    for_ [0 .. 20] $ \z -> do
      for_ [0 .. 20] $ \x -> do
        putChar (if (x, y, z) `Set.member` model then '#' else '+')
      putChar '\n'
    putChar '\n'

main :: IO ()
main = do
  contents <- BS.readFile "problemsL/LA001_tgt.mdl"
  let model = loadModel contents
  printModel model
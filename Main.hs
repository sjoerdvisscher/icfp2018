{-# LANGUAGE BinaryLiterals #-}
module Main where

import GHC.Word
import Data.Foldable
import Data.Bits (testBit)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.Serialize as S

type Model = Set (Word8, Word8, Word8)
data Dim = X | Y | Z deriving (Show, Eq, Ord, Enum)
data LLD = LLD Dim Word8 deriving (Show, Eq, Ord)
data SLD = SLD Dim Word8 deriving (Show, Eq, Ord)
data ND = ND Word8 Word8 Word8 deriving (Show, Eq, Ord)
data Command = Halt | Wait | Flip | SMove LLD | LMove SLD SLD | FusionP ND | FusionS ND | Fission ND Word8 | Fill ND deriving (Show, Eq, Ord)

instance S.Serialize Command where
  put Halt = S.putWord8 0b11111111
  put Wait = S.putWord8 0b11111110
  put Flip = S.putWord8 0b11111101
  put (SMove (LLD dim d)) = do 
    S.putWord8 $ (toEnum $ fromEnum dim + 1) * 16 + 0b0100
    S.putWord8 $ d + 15
  put (LMove (SLD dim1 d1) (SLD dim2 d2)) = do 
    S.putWord8 $ (toEnum $ fromEnum dim2 + 1) * 64 + (toEnum $ fromEnum dim1 + 1) * 16 + 0b1100
    S.putWord8 $ (d2 + 5) * 16 + d1 + 5
  put (FusionP (ND dx dy dz)) = S.putWord8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) * 8 + 0b111
  put (FusionS (ND dx dy dz)) = S.putWord8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) * 8 + 0b110
  put (Fission (ND dx dy dz) m) = do
    S.putWord8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) * 8 + 0b101
    S.putWord8 m
  put (Fill (ND dx dy dz)) = S.putWord8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) * 8 + 0b011
  get = undefined

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
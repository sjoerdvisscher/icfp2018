{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving #-}
module Main where

import GHC.Int (Int16)
import Data.Foldable (for_)
import Data.Bits (testBit, shift)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Control.Monad (unless)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except

data Coord = Coord !Int16 !Int16 !Int16 deriving (Show, Eq, Ord)
type Model = Set Coord
data Dim = X | Y | Z deriving (Show, Eq, Ord, Enum)
data LLD = LLD !Dim !Int16 deriving (Show, Eq, Ord)
data SLD = SLD !Dim !Int16 deriving (Show, Eq, Ord)
data ND = ND !Int16 !Int16 !Int16 deriving (Show, Eq, Ord)
data Command = Halt | Wait | Flip | SMove !LLD | LMove !SLD !SLD | FusionP !ND | FusionS !ND | Fission !ND !Int16 | Fill !ND deriving (Show, Eq, Ord)
data Harmonics = High | Low deriving (Show, Eq, Ord, Enum)
data S = S 
  { energy :: Int
  , harmonics :: Harmonics
  , matrix :: Model
  , bots :: [Bot]
  } deriving (Show, Eq)
data Bot = Bot 
  { bid :: Int16
  , pos :: Coord
  , seeds :: [Int16]
  } deriving (Show, Eq)
  
newtype Builder a = Builder { unBuilder :: WriterT S.Put (StateT S (Except String)) a }
  deriving (Functor, Applicative, Monad, MonadWriter S.Put, MonadState S, MonadError String)

runBuilder :: Builder () -> Either String S.Put
runBuilder = fmap (snd . fst) . runExcept . flip runStateT s_init . runWriterT . unBuilder

s_init :: S
s_init = S 0 Low Set.empty [Bot 1 (Coord 0 0 0) [2..20]]

putAsW8 :: Int16 -> S.Put
putAsW8 = S.putWord8 . toEnum . fromEnum

instance S.Serialize Command where
  put Halt = S.putWord8 0b11111111
  put Wait = S.putWord8 0b11111110
  put Flip = S.putWord8 0b11111101
  put (SMove (LLD dim d)) = do 
    putAsW8 $ (toEnum $ fromEnum dim + 1) `shift` 4 + 0b0100
    putAsW8 $ d + 15
  put (LMove (SLD dim1 d1) (SLD dim2 d2)) = do 
    putAsW8 $ (toEnum $ fromEnum dim2 + 1) `shift` 6 + (toEnum $ fromEnum dim1 + 1) `shift` 4 + 0b1100
    putAsW8 $ (d2 + 5) `shift` 4 + d1 + 5
  put (FusionP (ND dx dy dz)) = putAsW8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) `shift` 3 + 0b111
  put (FusionS (ND dx dy dz)) = putAsW8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) `shift` 3 + 0b110
  put (Fission (ND dx dy dz) m) = do
    putAsW8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) `shift` 3 + 0b101
    putAsW8 m
  put (Fill (ND dx dy dz)) = putAsW8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) `shift` 3 + 0b011
  get = undefined


straight :: Dim -> Int16 -> Coord -> Coord
straight X dx (Coord x y z) = Coord (x + dx) y z
straight Y dy (Coord x y z) = Coord x (y + dy) z
straight Z dz (Coord x y z) = Coord x y (z + dz)

addND :: ND -> Coord -> Coord
addND (ND dx dy dz) (Coord x y z) = Coord (x + dx) (y + dy) (z + dz)

out :: Command -> Builder ()
out = tell . S.put

halt :: Builder ()
halt = do
  s <- get
  unless (harmonics s == Low) $ throwError "halt: Harmonics is not low"
  unless (length (bots s) == 1) $ throwError "halt: More than one bot still active"
  let bot = head (bots s)
  unless (pos bot == Coord 0 0 0) $ throwError "halt: Bot is not at origin"
  out Halt
  put $ s { bots = [] }
  
wait :: Builder ()
wait = out Wait

flipHarmonics :: Builder ()
flipHarmonics = do
  s <- get
  out Flip
  put $ s { harmonics = toEnum $ 1 - fromEnum (harmonics s) }

sMove :: LLD -> Builder ()
sMove lld@(LLD dim d) = do
  unless (abs d <= 15) $ throwError "sMove: Distance too big"
  s <- get
  let [bot] = bots s
  out $ SMove lld
  put $ s { 
    bots = [ bot { pos = straight dim d (pos bot) } ],
    energy = energy s + 2 * abs (fromEnum d)
  }

lMove :: SLD -> SLD -> Builder ()
lMove sld1@(SLD dim1 d1) sld2@(SLD dim2 d2) = do
  unless (abs d1 <= 5) $ throwError "lMove: Distance too big"
  unless (abs d2 <= 5) $ throwError "lMove: Distance too big"
  unless (dim1 /= dim2) $ throwError "lMove: no turn"
  s <- get
  -- todo check matrix is empty
  let [bot] = bots s
  out $ LMove sld1 sld2
  put $ s { 
    bots = [ bot { pos = straight dim2 d2 $ straight dim1 d1 $ pos bot } ],
    energy = energy s + 2 * abs (fromEnum d1) + 2 + 2 * abs (fromEnum d2)
  }

fill :: ND -> Builder ()
fill nd = do
  s <- get
  let [bot] = bots s
  let c' = addND nd $ pos bot
  unless (c' `Set.notMember` matrix s) $ throwError "fill: Already filled"
  out $ Fill nd
  put $ s {
    matrix = Set.insert c' (matrix s),
    energy = energy s + 12
  }

sMoveLim :: LLD -> Builder ()
sMoveLim (LLD dim d) = sMove (LLD dim (min 15 d))

moveTo :: Coord -> Builder ()
moveTo c'@(Coord x' y' z') = do
  s <- get
  let [bot] = bots s
  let Coord x y z = pos bot
  let dx = x' - x
  let dy = y' - y
  let dz = z' - z
  if abs dx > 5 then do
    sMoveLim (LLD X dx)
    moveTo c'
  else if abs dy > 5 then do
    sMoveLim (LLD Y dy)
    moveTo c'
  else if abs dz > 5 then do
    sMoveLim (LLD Z dz)
    moveTo c'
  else if dx == 0 then
    lMove (SLD Y dy) (SLD Z dz)
  else if dy == 0 then
    lMove (SLD X dx) (SLD Z dz)
  else if dz == 0 then
    lMove (SLD X dx) (SLD Y dy)
  else do
    sMove (LLD X dx)
    lMove (SLD Y dy) (SLD Z dz)


loadModel :: BS.ByteString -> Model
loadModel bs = 
  Set.fromList 
  [ Coord x y z 
  | x <- [0..r - 1]
  , y <- [0..r - 1]
  , z <- [0..r - 1]
  , let p = fromEnum x * fromEnum r * fromEnum r + fromEnum y * fromEnum r + fromEnum z
  , let (pw, pb) = p `divMod` 8
  , let b = bs `BS.index` succ pw
  , testBit b pb
  ]
  where
    r :: Int16
    r = toEnum . fromEnum $ bs `BS.index` 0

printModel :: Model -> IO ()
printModel model =
  for_ [0 .. 20] $ \y -> do
    for_ [0 .. 20] $ \z -> do
      for_ [0 .. 20] $ \x -> do
        putChar (if Coord x y z `Set.member` model then '#' else '+')
      putChar '\n'
    putChar '\n'

main :: IO ()
main = do
  contents <- BS.readFile "problemsL/LA001_tgt.mdl"
  let model = loadModel contents
  printModel model
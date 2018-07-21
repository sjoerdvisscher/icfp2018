{-# OPTIONS_GHC -funbox-strict-fields -O2 #-}
{-# LANGUAGE BinaryLiterals, GeneralizedNewtypeDeriving #-}
module Main where

import GHC.Int (Int16)
import Data.Foldable (for_)
import Data.Bits (testBit, shift)
import Data.Map.Strict (Map)
import Data.List (sortOn)
import Data.Monoid (Dual(getDual))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Control.Monad (unless)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Debug.Trace

data Coord = Coord !Int16 !Int16 !Int16 deriving (Show, Eq, Ord)
type Model = Set Coord
type AnnModel = Map Coord Int16
data Dim = X | Y | Z deriving (Show, Eq, Ord, Enum)
data LLD = LLD !Dim !Int16 deriving (Show, Eq, Ord)
data SLD = SLD !Dim !Int16 deriving (Show, Eq, Ord)
data ND = ND !Int16 !Int16 !Int16 deriving (Show, Eq, Ord)
data Command = Commence | Halt | Wait | Flip 
  | SMove !LLD | LMove !SLD !SLD 
  | FusionP !ND | FusionS !ND | Fission !ND !Int16 
  | Fill !ND | Void !ND deriving (Show, Eq, Ord)
data Harmonics = High | Low deriving (Show, Eq, Ord, Enum)
data S = S 
  { energy :: Integer
  , harmonics :: Harmonics
  , matrix :: Model
  , floaters :: Model
  , todo :: AnnModel
  , bots :: [Bot]
  } deriving (Show, Eq)
data Bot = Bot 
  { bid :: Int16
  , pos :: Coord
  , seeds :: [Int16]
  } deriving (Show, Eq)
  
newtype Builder a = Builder { unBuilder :: WriterT (S.Put, Dual S.Put) (StateT S (Except String)) a }
  deriving (Functor, Applicative, Monad, MonadWriter (S.Put, Dual S.Put), MonadState S, MonadError String)

runBuilder :: AnnModel -> Builder () -> Either String (S.Put, Dual S.Put)
runBuilder model = fmap (snd . fst) . runExcept . flip runStateT (s_init model) . runWriterT . unBuilder

runBuilderA :: AnnModel -> Builder () -> Either String S.Put
runBuilderA model = fmap fst . runBuilder model

runBuilderD :: AnnModel -> Builder () -> Either String S.Put
runBuilderD model = fmap (getDual . snd) . runBuilder model

s_init :: AnnModel -> S
s_init model = S 0 Low Set.empty Set.empty model [Bot 1 (Coord 0 0 0) [2..20]]

putAsW8 :: Int16 -> S.Put
putAsW8 = S.putWord8 . toEnum . fromEnum

instance S.Serialize Command where
  put Commence = pure ()
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
  put (Void (ND dx dy dz)) = putAsW8 $ ((dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)) `shift` 3 + 0b010
  get = undefined

op :: Command -> Command
op Commence = Halt
op Halt = Commence
op Wait = Wait
op Flip = Flip
op (SMove (LLD dim d)) = SMove (LLD dim (negate d))
op (LMove (SLD dim1 d1) (SLD dim2 d2)) = LMove (SLD dim1 (negate d1)) (SLD dim2 (negate d2))
op (Fill nd) = Void nd
op (Void nd) = Fill nd
op _ = error "op: unsupported"

straight :: Dim -> Int16 -> Coord -> Coord
straight X dx (Coord x y z) = Coord (x + dx) y z
straight Y dy (Coord x y z) = Coord x (y + dy) z
straight Z dz (Coord x y z) = Coord x y (z + dz)

addND :: ND -> Coord -> Coord
addND (ND dx dy dz) (Coord x y z) = Coord (x + dx) (y + dy) (z + dz)

diffND :: Coord -> Coord -> ND
diffND (Coord x1 y1 z1) (Coord x2 y2 z2) = ND (x1 - x2) (y1 - y2) (z1 - z2)

onFloor :: Coord -> Bool
onFloor (Coord _ y _) = y == 0

out :: Command -> Builder ()
out c = tell (S.put c, Dual (S.put (op c)))

commence :: Builder ()
commence = pure ()

halt :: Builder ()
halt = do
  s <- get
  unless (harmonics s == Low) $ throwError "halt: Harmonics is not low"
  unless (Map.null (todo s)) $ throwError $ "halt: Model not finished: " ++ show (todo s) 
  unless (Set.null (floaters s)) $ throwError $ "halt: Still floating cells: " ++ show (floaters s) 
  unless (length (bots s) == 1) $ throwError "halt: More than one bot still active"
  let bot = head (bots s)
  unless (pos bot == Coord 0 0 0) $ throwError "halt: Bot is not at origin"
  put $ s { bots = [] }
  
wait :: Builder ()
wait = out Wait

flipHarmonics :: Builder ()
flipHarmonics = do
  s <- get
  out Flip
  put $ s { harmonics = toEnum $ 1 - fromEnum (harmonics s) }

harmonicsHigh :: Builder ()
harmonicsHigh = do
  s <- get
  unless (harmonics s == High) flipHarmonics

harmonicsLow :: Builder ()
harmonicsLow = do
  s <- get
  unless (Set.null (floaters s)) $ throwError $ "harmonicsLow: Still floating cells: " ++ show (floaters s) 
  unless (harmonics s == Low) flipHarmonics

sMove :: LLD -> Builder ()
sMove lld@(LLD dim d) = do
  unless (abs d <= 15) $ throwError ("sMove: Distance too big: " ++ show d)
  s <- get
  let [bot] = bots s
  out $ SMove lld
  put $ s { 
    bots = [ bot { pos = straight dim d (pos bot) } ],
    energy = energy s + 2 * abs (toInteger d)
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
    energy = energy s + 2 * abs (toInteger d1) + 2 + 2 * abs (toInteger d2)
  }

fill :: ND -> Builder ()
fill nd = do
  s <- get
  let [bot] = bots s
  let c' = addND nd $ pos bot
  let floats = not (onFloor c') && Set.null (neighbors c' `Set.intersection` (matrix s `Set.difference` floaters s))
  when floats harmonicsHigh
  unless (c' `Set.notMember` matrix s) $ throwError "fill: Already filled"
  unless (c' `Map.member` todo s) $ throwError "fill: Not in the model"
  out $ Fill nd
  s <- get
  let s' = s {
    matrix = Set.insert c' (matrix s),
    todo = Map.delete c' (todo s),
    floaters = if floats then Set.insert c' (floaters s) else checkGrounded c' (floaters s),
    energy = energy s + 12
  }
  put s'
  unless (floats || not (Set.null (floaters s'))) harmonicsLow


checkGrounded :: Coord -> Model -> Model
checkGrounded _ model | Set.null model = model
checkGrounded c model = model''
  where
    remove = neighbors c `Set.intersection` model
    model' = model `Set.difference` remove
    model'' = foldr checkGrounded model' remove

sMoveLim :: LLD -> Builder ()
sMoveLim (LLD dim d) = 
  if d < -15 then sMove (LLD dim (-15)) else 
  if d > 15 then sMove (LLD dim 15) else sMove (LLD dim d)

lMove' :: SLD -> SLD -> Builder ()
lMove' (SLD _ 0) (SLD _ 0) = pure ()
lMove' (SLD _ 0) (SLD dim d) = sMove (LLD dim d)
lMove' (SLD dim d) (SLD _ 0) = sMove (LLD dim d)
lMove' s1 s2 = lMove s1 s2

moveTo :: Coord -> Builder ()
moveTo c'@(Coord x' y' z') = do
  s <- get
  let [bot] = bots s
  let Coord x y z = pos bot
  let dx = x' - x
  let dy = y' - y
  let dz = z' - z
  if abs dy > 5 then do
    sMoveLim (LLD Y dy)
    moveTo c'
  else if abs dx > 5 then do
    sMoveLim (LLD X dx)
    moveTo c'
  else if abs dz > 5 then do
    sMoveLim (LLD Z dz)
    moveTo c'
  else if dx == 0 then
    lMove' (SLD Y dy) (SLD Z dz)
  else if dy == 0 then
    lMove' (SLD X dx) (SLD Z dz)
  else if dz == 0 then
    lMove' (SLD Y dy) (SLD X dx)
  else do
    sMove (LLD Y dy)
    lMove (SLD X dx) (SLD Z dz)

cross :: [ND]
cross = [ND 0 (-1) 0, ND 1 (-1) 0, ND 0 (-1) 1, ND (-1) (-1) 0, ND 0 (-1) (-1)]
mlen1 :: [ND]
mlen1 = [ND 0 (-1) 0, ND 1 0 0, ND 0 0 1, ND (-1) 0 0, ND 0 0 (-1), ND 0 1 0]

toModel :: [ND] -> Coord -> Model
toModel nds c = Set.fromList $ map (\nd -> addND nd c) nds

crossBelow :: Coord -> Model
crossBelow = toModel cross

neighbors :: Coord -> Model
neighbors = toModel mlen1

spiral :: [[[(Int16, Int16)]]]
spiral = [[(1, 0)], [(0, 1)], [(-1, 0), (-1, 0)], [(0, -1), (0, -1)]] : 
  map (\[r,d,l,u] -> [[(1, 0), (1, 0)] ++ r, [(0, 1), (0, 1)] ++ d, [(-1, 0), (-1, 0)] ++ l, [(0, -1), (0, -1)] ++ u]) spiral

spiraling :: (Int16, Int16) -> [(Int16, Int16)]
spiraling start = scanl (\(x, y) (dx, dy) -> (x + dx, y + dy)) start $ concat (concat spiral)

solveSimpleCross :: Int16 -> Builder ()
solveSimpleCross r = do
  let r2 = r `div` 2

  commence
  
  for_ [1 .. r] $ \y ->
    for_ (take (fromEnum r * fromEnum r) (spiraling (0, 0))) $ \(x', z') -> do
        let z = z' * 2 - x' + r2
        let x = x' * 2 + z' + r2
        s <- get
        let c = Coord x y z
        let i = todo s `Map.intersection` Map.fromSet (const ()) (crossBelow c)
        unless (Map.null i) $ do
          let pairs = sortOn snd $ Map.toList i
          moveTo (Coord x y z)
          for_ pairs $ fill . (`diffND` c) . fst

  s <- get
  let [Bot _ (Coord _ y _) _] = bots s
  moveTo (Coord 0 y 0)
  moveTo (Coord 0 0 0)
  halt

loadModel :: BS.ByteString -> (Int16, Model)
loadModel bs = 
  (r, Set.fromList 
  [ Coord x y z 
  | x <- [0..r - 1]
  , y <- [0..r - 1]
  , z <- [0..r - 1]
  , let p = fromEnum x * fromEnum r * fromEnum r + fromEnum y * fromEnum r + fromEnum z
  , let (pw, pb) = p `divMod` 8
  , let b = bs `BS.index` succ pw
  , testBit b pb
  ])
  where
    r = toEnum . fromEnum $ bs `BS.index` 0

printModel :: AnnModel -> IO ()
printModel model =
  for_ [0 .. 20] $ \y -> do
    for_ [0 .. 20] $ \z -> do
      for_ [0 .. 20] $ \x -> do
        putStr (maybe "+" show $ Coord x y z `Map.lookup` model)
      putChar '\n'
    putChar '\n'

annModel :: Model -> AnnModel
annModel model = check (Seq.fromList $ Set.toList start) (Map.fromSet (const 0) start)
  where 
    start = Set.filter onFloor model
    check s ann = case Seq.viewl s of 
      Seq.EmptyL -> ann
      c Seq.:< cs -> 
        ann'' where
          d = ann Map.! c + 1
          visit = Set.filter (\c' -> c' `Map.notMember` ann && c' `Set.member` model) (neighbors c)
          ann' = Map.fromSet (const d) visit `Map.union` ann
          ann'' = check (cs Seq.>< Seq.fromList (Set.toList visit)) ann'

load :: String -> IO (Int16, Model)
load name = loadModel <$> BS.readFile name

save :: String -> S.Put -> IO ()
save name p = BS.writeFile name (S.runPut (p <> S.put Halt))

main :: IO ()
main = do
  -- Lightning
  -- for_ [1::Int .. 186] $ \n -> do
  --   let ns = take (3 - length (show n)) "00" ++ show n
  --   putStrLn ns
  --   (r, model) <- load ("problemsL/LA" ++ ns ++ "_tgt.mdl")
  --   let result = runBuilder (annModel model) (solveSimpleCross r)
  --   either putStrLn (save ("outL/LA" ++ ns ++ ".nbt")) result
  -- Full
  for_ [1::Int .. 186] $ \n -> do
    let ns = take (3 - length (show n)) "00" ++ show n
    putStrLn ('A':ns)
    (rA, modelA) <- load ("problemsF/FA" ++ ns ++ "_tgt.mdl")
    let resultA = runBuilder (annModel modelA) (solveSimpleCross rA)
    either putStrLn (save ("outF/FA" ++ ns ++ ".nbt") . fst) resultA
    putStrLn ('D':ns)
    (rD, modelD) <- load ("problemsF/FD" ++ ns ++ "_src.mdl")
    let resultD = if modelA == modelD then resultA else runBuilder (annModel modelD) (solveSimpleCross rD)
    either putStrLn (save ("outF/FD" ++ ns ++ ".nbt") . getDual . snd) resultD
  for_ [1::Int .. 115] $ \n -> do
    let ns = take (3 - length (show n)) "00" ++ show n
    putStrLn ('R':ns)
    (r, modelD) <- load ("problemsF/FR" ++ ns ++ "_src.mdl")
    let resultD = runBuilderD (annModel modelD) (solveSimpleCross r)
    (_, modelA) <- load ("problemsF/FR" ++ ns ++ "_tgt.mdl")
    let resultA = runBuilderA (annModel modelA) (solveSimpleCross r)
    either putStrLn (\outD -> either putStrLn (\outA -> save ("outF/FR" ++ ns ++ ".nbt") (outD <> outA)) resultA) resultD

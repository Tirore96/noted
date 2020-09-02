{-# LANGUAGE GADTs #-}
module TermReducer where

--import qualified Euterpea as Eu
import qualified Data.Map.Strict as Map
--import Data.Ratio
import qualified Calc.Parser as P
import Control.Monad.State.Lazy
--import qualified Data.List as List
--import qualified TypeChecker as T
--import Debug.Trace



data ReducedBase = RIndex Integer 
                 | RDur  Integer 
                 | RLetter Integer
                 | ROctave Integer
                 | RColor [Integer] 
                 | RScale [Integer]
                 | RPlacement (Integer,Integer)
                 | RNote ((Integer,Integer),Integer)
  deriving (Eq,Show)
data ReducedTerm = Single ReducedBase
                 | FlatList (P.CompType,[ReducedTerm])
  deriving (Eq,Show)



type StateData = Map.Map String ReducedTerm

type SEME a = StateT StateData (Either String) a

updateState :: String -> ReducedTerm ->  SEME ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEME ReducedTerm
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

reduce :: P.Program -> Either String ReducedTerm 
reduce program = let reduceM = do mapM_ reduceAssignment program
                                  reduceTerm (P.TVar ("main",(0,0)))
                 in case runStateT reduceM Map.empty of
                      Right (val,_) -> Right val
                      Left err -> Left err

reduceAssignment :: P.Line -> SEME ()
reduceAssignment (P.Assignment (var,_) t) = do
  reduced_t <- reduceTerm t
  updateState var reduced_t

reduceTerm :: P.Term -> SEME ReducedTerm
reduceTerm (P.TIndex val) = return $ Single  $ RIndex (fst val)
reduceTerm (P.TDur val) = return $ Single  $ RDur (fst val)
reduceTerm (P.TLetter val) = return $ Single  $ RLetter (fst val)
reduceTerm (P.TOctave val) = return $ Single $ ROctave (fst val)
reduceTerm (P.TColor val) = return $ Single $ RColor (buildColor (fst val))

reduceTerm (P.TFlatList ((ct,ts),_)) = do
  reduced_ts <- mapM reduceTerm ts
  return $ FlatList (ct,reduced_ts)
reduceTerm (P.TVar (var,_)) = lookupVar var
reduceTerm (P.TWith ((wt,t1,t2),_)) = do
  reduced_t1 <- reduceTerm t1
  reduced_t2 <- reduceTerm t2
  reduceWith wt reduced_t1 reduced_t2


reduceWith :: P.WithType -> ReducedTerm -> ReducedTerm -> SEME ReducedTerm
reduceWith wt t1@(FlatList (ct1,ts1)) t2@(FlatList (ct2,ts2)) =
  if ct1 /= ct2
    then case (ct1,ct2) of
           (P.Serial,P.Parallel) -> do new_cs <- mapM (\ts1_elem -> reduceWith wt ts1_elem t2) ts1
                                       return $ FlatList (P.Serial, new_cs)
           (P.Parallel,P.Serial) -> do new_cs <- mapM (\ts2_elem -> reduceWith wt t1 ts2_elem) ts2
                                       return $ FlatList (P.Serial, new_cs)

    else if (length ts1) /= (length ts2)
           then failM "lengths different"
           else let zipped = zip ts1 ts2
                in do ts_new <- mapM (\(t1,t2) -> reduceWith wt t1 t2) zipped
                      return $ FlatList (ct1,ts_new)
  
reduceWith wt t1@(Single _) (FlatList (ct,ts)) = do
  ts_new <- mapM (\t2 -> reduceWith wt t1 t2) ts
  return $ FlatList (ct,ts_new)

reduceWith wt (FlatList (ct,ts)) t2@(Single _) = do
  ts_new <- mapM (\t1 -> reduceWith wt t1 t2) ts
  return $ FlatList (ct,ts_new)

reduceWith P.WithScale (Single (RIndex n)) (Single (RScale scale)) = 
  let len = fromIntegral $ length scale
  in let safe_index = fromIntegral $ n `mod` len
     in return $ Single $ RLetter (scale!!safe_index)

reduceWith P.WithColor (Single (RLetter letter)) (Single (RColor color )) = 
  return $ Single $ RScale $ buildScale letter color
reduceWith P.WithOctave (Single (RLetter letter)) (Single (ROctave n)) = return $ Single $ RPlacement (letter,n)
reduceWith P.WithDur (Single (RPlacement placement)) (Single (RDur n)) = return $ Single $  RNote (placement,n)

reduceWith _ _ _ = failM "failed in reduceWith"

buildColor :: P.ColorType -> [Integer]
buildColor P.Major = [2,2,1,2,2,2,1]
buildColor P.Minor = [2,1,2,2,1,2,2]

buildScale :: Integer -> [Integer] -> [Integer]
buildScale index color = buildScaleHelper index (0:color)

buildScaleHelper :: Integer -> [Integer] -> [Integer]
buildScaleHelper index (n:rest) = (index+n) : (buildScaleHelper (index+n) rest)
buildScaleHelper _ [] = []




--nextLetter :: (P.Letter,P.Sign) -> (P.Letter,P.Sign)
--nextLetter (letter,P.Flat) = (letter,P.Natural)
--nextLetter (letter,P.Natural) = (letter,P.Sharp)
--nextLetter (P.A,P.Sharp) = (P.B,P.Flat)
--nextLetter (P.B,P.Sharp) = (P.C,P.Flat)
--nextLetter (P.C,P.Sharp) = (P.D,P.Flat)
--nextLetter (P.D,P.Sharp) = (P.E,P.Flat)
--nextLetter (P.E,P.Sharp) = (P.F,P.Flat)
--nextLetter (P.F,P.Sharp) = (P.G,P.Flat)
--nextLetter (P.G,P.Sharp) = (P.A,P.Flat)


failM :: String -> SEME a
failM str = StateT (\_ -> Left str) 


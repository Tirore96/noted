module TypeChecker where
--Possibly type check for repeating labels. ie {quantization=10;quantization=20} shouldn't be allowed

import Data.Map as Map
import Control.Monad.State.Lazy
import Data.Set as Set

import Calc.Lexer (AlexPosn)
import qualified Calc.Parser as P
import qualified Euterpea  as Eu (Dur)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio

data Type = TDur
           | TKey
           | TOctave
           | Fun [Type]
           | Arg [Type]
           | TContext [P.Label]
           | TMusic
  deriving(Eq,Show)

data TSymbol = ByLetter | ByNum
  deriving(Eq,Show)
data TPosition = Positioned | NotPositioned
  deriving(Eq,Show)

type StateData = Map String Type

type SEM a = StateT StateData (Either String) a

updateState :: String -> Type ->  SEM ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEM Type
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

assertSameType :: Type -> Type -> String-> SEM ()
assertSameType t1 t2 s= if t1 == t2
                         then return ()
                         else failM $ "failed asserting "++(show t1)++"=="++(show t2)++"location: "++s

data TypedExp = Assign String TypedTerm TypedExp | TypedTerm TypedTerm
  deriving (Show)

data TypedTerm = Num Type Integer
               | Note Type String
               | NoteN Type String Int
               | Dotted Type TypedTerm
               | Concatted Type TypedTerm TypedTerm
               | Commaed Type TypedTerm TypedTerm
               | Variable Type String
               | Context Type [TypedCtxValue]
               | Application Type TypedTerm TypedTerm
  deriving (Show)

data TypedCtxValue = Dur Integer Integer | Octave Int | Key String
  deriving(Eq,Show)

instance Ord TypedCtxValue where
  compare (Dur _ _ ) (Dur _ _) = EQ
  compare (Key _) (Key _) = EQ
  compare (Octave _) (Octave _) = EQ
  compare (Dur _ _) _ = GT
  compare (Key _) (Dur _ _) = LT
  compare (Key _) _ = LT
  compare (Octave _) _ = LT

getType :: TypedTerm -> Type
getType (Num t _) = t
getType (Note t _) = t
getType (NoteN t _ _ ) = t
getType (Dotted t _ ) = t
getType (Concatted t _ _ ) = t
getType (Commaed t _ _ )= t
getType (Variable t _ ) = t
getType (Context t _ ) = t
getType (Application t _ _) = t

typeCheck :: P.Exp -> Either String TypedExp
typeCheck exp = case runStateT (typeCheckExp exp) Map.empty of
                 Left err -> Left err
                 Right (typedExp,_) -> Right typedExp

typeCheckExp :: P.Exp -> SEM TypedExp
typeCheckExp (P.Assign (str,pos) term exp) = do typedTerm <- typeCheckTerm term
                                                updateState str (getType typedTerm)
                                                typedExp <- typeCheckExp exp
                                                return $ Assign str typedTerm typedExp

typeCheckExp (P.Term term) = do typedTerm <- typeCheckTerm term
                                assertSameType (getType typedTerm) TMusic "1"
                                return $ TypedTerm typedTerm 
                              

--fix getType
typeCheckTerm :: P.Term -> SEM TypedTerm
typeCheckTerm (P.Num (n,_)) = let termType = Fun [TDur,TKey,TOctave,TMusic]
                              in return $ Num termType n

typeCheckTerm (P.Note (str,_)) = let termType = Fun [TDur,TOctave,TMusic]
                                 in return $ Note termType str

typeCheckTerm (P.NoteN (str,n,_)) = let termType = Fun [TDur,TMusic]
                                    in return $ NoteN termType str (fromIntegral n)

typeCheckTerm (P.Dotted term) = do typedTerm <- typeCheckTerm term
                                   return $ Dotted (getType typedTerm) typedTerm

typeCheckTerm (P.Concatted term1 term2) = do typed1 <- typeCheckTerm term1
                                             typed2 <- typeCheckTerm term2
                                             assertSameType (getType typed1) (getType typed2) "2"
                                             return $ Concatted (getType typed1) typed1 typed2

typeCheckTerm (P.Commaed term1 term2)= do typed1<- typeCheckTerm term1
                                          typed2 <- typeCheckTerm term2
                                          assertSameType (getType typed1) (getType typed2) "3"
                                          return $ Commaed (getType typed1) typed1 typed2

typeCheckTerm (P.Variable (str,_)) = do varType <- lookupVar str
                                        return $ Variable varType str

typeCheckTerm (P.Context labelPairs) = do typedArgs <- mapM typeCheckLabelPair labelPairs 
                                          let unzipped =  unzip typedArgs
                                          let types = fst unzipped
                                              args = snd unzipped
                                          return $ Context (Arg types) args


typeCheckTerm (P.Application term1 term2) = do typed1 <- typeCheckTerm term1
                                               typed2 <- typeCheckTerm term2
                                               assertValidApplication (getType typed1) (getType typed2)
                                               let (Fun args) = getType typed1
                                               return $ Application (last args) typed1 typed2

assertValidApplication (Fun fun) (Arg presentArgs) =  
  let requiredArgs = init fun
  in let satisfied = List.foldl (\acc arg -> acc && (elem arg presentArgs)) True requiredArgs
     in if satisfied 
          then
            return ()
          else
            failM "invalid application"

assertValidApplication _ _ = failM "invalid application 2"



typeCheckLabelPair (P.Bars,val) = case val of
                                   P.CNum (n,_) -> return $ (TDur,Dur 1 4)
                                   _ -> failM "bars is associated with non cnum"
typeCheckLabelPair (P.Key,val) = case val of
                                   P.CNote (str,_) -> return $ (TKey,Key str)
                                   _ -> failM "Key is associated with non CNote"
typeCheckLabelPair (P.OctavePos,val) = 
                                 case val of
                                   P.CNum (n,_) -> return $ (TOctave, Octave (fromIntegral n))
                                   _ -> failM "Key is associated with non CNote"


failM str = StateT (\state -> Left str)

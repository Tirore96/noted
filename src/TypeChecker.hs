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

--type Pos = (Int,Int)
--
--data Composition = NoteN String Integer Pos | Dotted Composition | Concatted Composition Composition
--  deriving(Eq,Show)
--
--type Context = Map CtxLabel CtxVal
--
--data CtxLabel = Quantization Pos
--  deriving(Eq,Show,Ord)
--
--data CtxVal = Int AlexPosn
--  deriving(Eq,Show)


--Changes: Note, CtxVal, CtxLabel
data Type = TDur
           | TKey
           | TOctave
           | Fun [Type]
           | Arg [Type]
--           | TMusicExp TSymbol TPosition
           | TContext [P.Label]
           | TMusic
  deriving(Eq,Show)
--Data structure for types
--data Type = TMusic | TComposition | TInt | TKey | TContext
--
data TSymbol = ByLetter | ByNum
  deriving(Eq,Show)
data TPosition = Positioned | NotPositioned
  deriving(Eq,Show)

--data TComposition =  TExp TDirection TSymbol TPosition
--  deriving(Eq,Show)
--data TDirection = TSerial | TParallel | TNote
--  deriving(Show)


--instance Eq TDirection where
--  TParallel  == TParallel = True
--  TParallel == TSerial = False
--  TParallel == TNote = True
--  TSerial == TParallel = False
--  TSerial == TSerial = True
--  TSerial == TNote = True
--  TNote  == TParallel = True
--  TNote  == TSerial = True
--  TNote  == TNote = True


--Data structure for Constraints
--data ConstraintMember = T Type | U Int
--data Constraint = Constraint ConstraintMember ConstraintMember 

--newConstraint :: ConstraintMember -> ConstraintMember -> Constraint
--newConstraint  (T t1) (T t2) = TTCon t1 t2
--newConstraint  (T t) (U u) = TUCon t u
--newConstraint  (U u1) (U u2) = UUCon u1 u2
--newConstraint a b = newConstraint b a



--data Variable = String


--data StateData = StateData {gamma :: Map Variable ConstraintMember,constraints :: [Constraint]}
--data ReadData = Exp | Composition | Context | CtxAssignment
type StateData = Map String Type

type SEM a = StateT StateData (Either String) a

--newtype SEM a =  SEM {runSEM :: StateData -> Either String (a,StateData) } 
--
----emptyStateData :: StateData
----emptyStateData = StateData {gamma=empty,constraints=[]}
--
--instance Monad SEM where
--  return a = SEM (\stateData -> Right (a,stateData) )
--  m >>= f = SEM (\stateData -> do (a1,s1) <- (runSEM m) stateData
--                                  (a2,s2) <- runSEM (f a1) s1
--			          Right (a2,s2) 
--		)
--
--instance Functor SEM where 
--  fmap = liftM
--
--instance Applicative SEM where
--  pure = return;(<*>) = ap

updateState :: String -> Type ->  SEM ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEM Type
lookupVar str = StateT (\s -> case Map.lookup str s of 
                            Just t -> Right (t,s)
                            Nothing -> Left "bad variable lookup")

assertSameType :: Type -> Type -> SEM ()
assertSameType t1 t2 = if t1 == t2
                         then return ()
                         else failM "the types don't work"

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

data TypedCtxValue = Dur Eu.Dur | Octave Int | Key String
  deriving(Eq,Show)

instance Ord TypedCtxValue where
  compare (Dur _) (Dur _) = EQ
  compare (Key _) (Key _) = EQ
  compare (Octave _) (Octave _) = EQ
  compare (Dur _) _ = GT
  compare (Key _) (Dur _) = LT
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
                                assertSameType (getType typedTerm) TMusic
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
                                             assertSameType (getType typed1) (getType typed2)
                                             return $ Concatted (getType typed1) typed1 typed2

typeCheckTerm (P.Commaed term1 term2)= do typed1<- typeCheckTerm term1
                                          typed2 <- typeCheckTerm term2
                                          assertSameType (getType typed1) (getType typed2)
                                          return $ Commaed (getType typed1) typed1 typed2

typeCheckTerm (P.Variable (str,_)) = do varType <- lookupVar str
                                        return $ Variable varType str

typeCheckTerm (P.Context labelPairs) = do typedArgs <- mapM typeCheckLabelPair labelPairs 
                                          let unzipped =  unzip typedArgs
                                          let types = fst unzipped
                                              args = snd unzipped
                                          return $ Context (Arg types) args


typeCheckTerm (P.Application term1 term2) = do typed1 <- typeCheckTerm term1
                                               assertIsFun (getType typed1)
                                               typed2 <- typeCheckTerm term2
                                               retType <- applicationReturnType (getType typed1) (getType typed2)
                                               return $ Application retType typed1 typed2

assertIsFun (Fun _) = return ()
assertIsFun _ = failM "arg is not fun"

applicationReturnType (Fun fun) (Arg arg) =  
                                if (init fun) == arg
                                   then
                                     return (last fun)
                                   else
                                     failM "invalid application"
applicationReturnType_ _ = failM "invalid application 2"

--typeCheckLabelPairHelper (P.Bars,val) expectedT  = do valT <- typeCheckCtxVal val
--                                                      assertSameType valT expectedT
--typeCheckCtxVal (P.CNum _ ) = return TInt
--typeCheckCtxVal (P.CNote _ ) = return TKey
--typeCheckCtxVal (P.Signature _ ) = return TSignature



typeCheckLabelPair (P.Bars,val) = case val of
                                   P.CNum (n,_) -> return $ (TDur,Dur (1%n))
                                   _ -> failM "bars is associated with non cnum"
typeCheckLabelPair (P.Key,val) = case val of
                                   P.CNote (str,_) -> return $ (TKey,Key str)
                                   _ -> failM "Key is associated with non CNote"
typeCheckLabelPair (P.OctavePos,val) = 
                                 case val of
                                   P.CNum (n,_) -> return $ (TOctave, Octave (fromIntegral n))
                                   _ -> failM "Key is associated with non CNote"

--typeCheckLabelPairHelper (P.Bars,val) TInt
--typeCheckLabelPair (P.Key,val) = typeCheckLabelPairHelper (P.Key,val) TKey
--typeCheckLabelPair (P.Time ,val) = typeCheckLabelPairHelper (P.Time,val) TSignature
--typeCheckLabelPair (P.OctavePos,val) = typeCheckLabelPairHelper (P.OctavePos,val) TInt


--assertTCtxContains (TContext tCtx) label = if Set.member label tCtx
--                                    then return ()
--                                    else failM "something went wrong"
--
--assertTCtxContains _ _ = failM "assertTCtxContains applied on non-context"


--assertApplicationSatisfied (TMusicExp ByLetter Positioned) term2T = return TMusic 
--assertApplicationSatisfied (TMusicExp ByNum Positioned) term2T = do assertTCtxContains term2T P.Key
--                                                                    return TMusic
--assertApplicationSatisfied (TMusicExp ByLetter NotPositioned) term2T = do assertTCtxContains term2T P.OctavePos
--                                                                          return TMusic
--assertApplicationSatisfied (TMusicExp ByNum NotPositioned) term2T = do assertTCtxContains term2T P.Key
--                                                                       assertTCtxContains term2T P.OctavePos
--                                                                       return TMusic
--
--asSerial :: TComposition -> TComposition
--asSerial (TExp _ symbol position) = TExp TSerial symbol position


failM str = StateT (\state -> Left str)


--assertNotParallelComp :: TComposition -> String -> SEM ()
--assertNotParallelComp (TExp TParallel _ _) s = failM ("Parallel compositions are not allowed here"++s)
--assertNotParallelComp _ _ = return ()
--assertEqualComps :: TComposition -> TComposition -> String -> SEM ()
--assertEqualComps c1 c2 s = if c1 == c2 then return () else failM ("Compositions are not equal"++s)
--
--
--typeCheck :: Exp -> Either String ((),StateData)
--typeCheck exp = runSEM (typeCheckExp exp) Map.empty
--typeCheckExp :: Exp -> SEM ()
--typeCheckExp (In comp con) = do compT <- typeCheckComp comp
--				conT <- typeCheckCon con
--			        let depL = dependencyLabels compT
--				if depL `isSubsetOf`conT 
--				  then return ()
--			        else 
--				    let violatingL = Set.filter (\elem -> not $ Set.member elem conT) depL
--				    in failM ("Missing labels: "++(show violatingL))
--
--dependencyLabels :: TComposition -> TContext
--dependencyLabels (TExp _ TByLetter TNotPositioned) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByNum TNotPositioned) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByLetter TPositioned ) = Set.fromList [Quantization]
--dependencyLabels (TExp _ TByNum TPositioned) = Set.fromList [Quantization]
--
--showCodeWithPos :: Composition -> String
--showCodeWithPos comp = (showCode comp)++" at: "++(show (findFirstPos comp))
--
--showCode :: Composition -> String
--showCode (NoteN note n p) = note++(show n)
--showCode (Dotted comp) = (showCode comp)++"."
--showCode (Concatted c1 c2) = (showCode c1)++(showCode c2)
--
--findFirstPos :: Composition -> Pos
--findFirstPos (NoteN n s p) = p
--findFirstPos (Dotted comp) = findFirstPos comp
--findFirstPos (Concatted c1 c2) = findFirstPos c1
--
--
--typeCheckComp :: Composition -> SEM TComposition
--typeCheckComp (NoteN _ _ _) = return $ TExp TNote TByLetter TPositioned
--
--typeCheckComp (Dotted comp) = do compT <- typeCheckComp comp
--				 assertNotParallelComp compT (showCodeWithPos comp)
--				 return $ asSerial compT
--
--typeCheckComp (Concatted c1 c2) = do c1T <- typeCheckComp c1
--				     assertNotParallelComp c1T (showCodeWithPos c1)
--				     c2T <- typeCheckComp c2
--			     	     assertNotParallelComp c2T (showCodeWithPos c2)
--				     assertEqualComps c1T c2T (showCodeWithPos (Concatted c1 c2))
--				     return $ asSerial c1T
--
--typeCheckCon :: Context -> SEM TContext
--typeCheckCon con = let labels = [Quantization]
--		       presentLabels = Prelude.filter (\label -> Map.member label con) labels
--		   in do return $ Set.fromList presentLabels

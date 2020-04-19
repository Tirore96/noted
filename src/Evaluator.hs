{-# LANGUAGE GADTs #-}
module Evaluator where

import qualified Euterpea as Eu
import qualified Data.Map as Map
import Data.Ratio
import qualified Calc.Parser as P
import Control.Monad.State.Lazy
import qualified Data.List as List
import qualified TypeChecker as T

--evaluate :: T.TypedExp -> Eu.Music Eu.Pitch
--evaluate typedExp = 
--
--evaluate :: T.TypedExp -> SEM (Eu.Music Eu.Pitch)
--evaluate (T.Assign str typedTerm typedExp) = case T.getType typedTerm of
--                                              TMusicExp sym pos
--                                              TContext labels
--                                              TMusic -> evaluateTerm typedTerm

--data STerm = SNum Integer 
--           | SNote String 
--           | SNoteN String Integer 
--           | SDotted STerm 
--           | SConcatted STerm STerm 
--           | SCommaed STerm STerm 
--           | SContext [(Pa.Label,Pa.CtxValue)]
--           | SApplication STerm STerm

data STerm = SNum T.Type Integer
           | SNote T.Type String
           | SNoteN T.Type String Int
           | SDotted T.Type STerm
           | SConcatted T.Type STerm STerm 
           | SCommaed T.Type STerm STerm
           | SContext T.Type [T.TypedCtxValue]
           | SApplication T.Type STerm STerm
 deriving (Show)

getType :: STerm -> T.Type
getType (SNum t _) = t
getType (SNote t _) = t
getType (SNoteN t _ _ ) = t
getType (SDotted t _ ) = t
getType (SConcatted t _ _ ) = t
getType (SCommaed t _ _ )= t
getType (SContext t _ ) = t


type Env = Map.Map String STerm
type SEM a = StateT Env (Either String) a

failM str = StateT (\s -> Left str)

bindVar :: String -> STerm -> SEM ()
bindVar str sTerm = do env <- get
                       put (Map.insert str sTerm env)

lookupVar :: String -> SEM STerm
lookupVar str = StateT (\s -> case Map.lookup str s of
                                Just val -> Right (val,s)
                                Nothing -> Left "var not present")

substitute :: T.TypedExp -> Either String STerm
substitute exp = case  runStateT (substituteExp exp) Map.empty of
                  Left err -> Left err
                  Right (sTerm,s) -> Right sTerm

substituteExp :: T.TypedExp -> SEM STerm
substituteExp (T.Assign str term exp) = do sTerm <- substituteTerm term
                                           bindVar str sTerm
                                           substituteExp exp

substituteExp (T.TypedTerm term) = substituteTerm term

substituteTerm :: T.TypedTerm -> SEM STerm
substituteTerm (T.Num t n) = return $ SNum t n
substituteTerm (T.Note t str) = return $ SNote t str
substituteTerm (T.NoteN t str n) = return $ SNoteN t str n
substituteTerm (T.Dotted t term) = do sTerm <- substituteTerm term
                                      return $ SDotted t sTerm
substituteTerm (T.Concatted t t1 t2) = do sT1 <- substituteTerm t1 
                                          sT2 <- substituteTerm t2
                                          return $ SConcatted t sT1 sT2

substituteTerm (T.Commaed t t1 t2) = do sT1 <- substituteTerm t1 
                                        sT2 <- substituteTerm t2
                                        return $ SCommaed t sT1 sT2

substituteTerm (T.Variable _ str ) = lookupVar str
substituteTerm (T.Context t val) = return $ SContext t val
substituteTerm (T.Application t t1 t2) = do sT1 <- substituteTerm t1 
                                            sT2 <- substituteTerm t2
                                            updatedsT2 <- ifContextFilter sT2 (getType sT1) ----Filtering of context
                                            return $ SApplication t sT1 updatedsT2 


ifContextFilter :: STerm -> T.Type-> SEM STerm
ifContextFilter (SContext t args) (T.Fun types) = do newArgs <-  filterArgs args (init types)
                                                     return $ SContext t newArgs
ifContextFilter sTerm  _ = return sTerm

typeOfCtxVal (T.Dur _) = T.TDur
typeOfCtxVal (T.Key _) = T.TKey
typeOfCtxVal (T.Octave _) = T.TOctave



filterArgs [] [] = return []
filterArgs [] _ = failM "application not satisfied"
filterArgs _ [] = return []
filterArgs (first:rest) (firstType:restTypes) = if (typeOfCtxVal first) == firstType
                                                        then do rest <- filterArgs rest restTypes
                                                                return (first:rest)
                                                        else filterArgs rest (firstType:restTypes)

--data Fun = NumFun (P.CtxValue -> P.CtxValue -> P.CtxValue -> Eu.Music Eu.Pitch)
--         | NoteFun (P.CtxValue -> P.CtxValue -> Eu.Music Eu.Pitch)
--         | NoteNFun (P.CtxValue-> Eu.Music Eu.Pitch) 
data Fun = NumFun ([T.TypedCtxValue] -> Either String (Eu.Music Eu.Pitch))
         | NoteFun ([T.TypedCtxValue]-> Either String (Eu.Music Eu.Pitch))
         | NoteNFun ([T.TypedCtxValue]->Either String (Eu.Music Eu.Pitch))

         
--instance Eq Fun where
--  (NumFun f1)==(NumFun f2)=True
--  (NoteFun f1)==(NoteFun f2)=True
--  (NoteNFun f1)==(NoteNFun f2)=True
--  _ == _ = False



durKeyOcL = [T.TDur,T.TKey,T.TOctave]
durOcL = [T.TDur,T.TOctave]
durL = [T.TDur]




evaluateToMusic :: STerm -> Either String (Eu.Music Eu.Pitch)
evaluateToMusic (SApplication T.TMusic term1 term2) = 
  case getType term1 of
   T.Fun [T.TDur,T.TKey,T.TOctave,T.TMusic] -> 
                do builtFun <-  buildFun term1 
                   case (builtFun,term2) of
                     (NumFun fun,SContext durKeyOcL args) -> fun args
                     _ -> Left "error 1"

   T.Fun [T.TDur,T.TOctave,T.TMusic] -> 
                do builtFun <-  buildFun term1 
                   case (builtFun,term2) of
                     (NoteFun fun,SContext durOcL args) -> fun args
                     _ -> Left "error 2"


   T.Fun [T.TDur,T.TMusic]-> 
                do builtFun <-  buildFun term1 
                   case (builtFun,term2) of
                     (NoteNFun fun,SContext durL args) -> fun args
                     _ -> Left "error 3"



   _ ->  Left "first argument of application is not a function"



evaluateToMusic (SConcatted T.TMusic term1 term2) = do m1 <- evaluateToMusic term1 
                                                       m2 <- evaluateToMusic term2
                                                       return $ (Eu.:+:) m1 m2

evaluateToMusic (SCommaed T.TMusic term1 term2) = do m1 <- evaluateToMusic term1 
                                                     m2 <- evaluateToMusic term2
                                                     return $ (Eu.:=:) m1 m2 

evaluateToMusic arg = Left $ errMsg arg "Called in evaluate"


evaluate :: T.TypedExp -> Either String (Eu.Music Eu.Pitch)
evaluate typedExp = do substituted <- substitute typedExp
                       evaluateToMusic substituted


--getLabels :: [Pa.Label] -> Map.Map Pa.Labl Pa.CtxValue -> Either String [Pa.CtxValue]
--getLabels labels con = mapM (\l -> case Map.lookup l con of
--                                    Just val -> Right val
--                                    Nothing -> left "label not present") con

--applyArgs :: (TypedCtxValue -> b) ->[TypedCtxValue]
--applyArgs fun (first:rest) = applyArgs (fun first) rest
--applyArgs fun [] = fun

errMsg :: (Show a) => a -> String -> String
errMsg val str = "Failed with "++(show val)++". "++str

--------------------------------------------------------------------------------------------------
--
----dur -> note -> key?
--data KeyOcBars = KeyOcBars 
--data OcBars = OcBars
--data Bars =  Bars
----Comp defined in terms of its' dependencies
--
--
--data Comp =
--    Num (Eu.Dur -> String -> Eu.Octave -> Eu.Music Eu.Pitch)
--  | Note (Eu.Dur -> Eu.Octave -> Eu.Music Eu.Pitch)
--  | NoteN  (Eu.Dur-> Eu.Music Eu.Pitch) 
----    Concatted ::  (Comp a) -> (Comp a) -> Comp a
----    Commaed ::  (Comp a) -> (Comp a) -> Comp a
--
--data Music = ApplicationKeyOcBars Comp String Int Int 
--           | ApplicationOcBars Comp Int Int
--           | ApplicationBars Comp Int 
--           | MConcatted Music  Music   
--           | MCommaed Music  Music 
--
--constrain :: STerm -> Either String Music
--constrain (SApplication comp (Pa.Context labelPairs) ) = 
--        do sComp <- combineComp comp
--           let conMap = simplifyCon labelPairs
--           case sComp of
--            Num fun -> do KeyF k <- key conMap
--                          BarsF b <- bars conMap
--                          OctaveF o <- octave conMap
--                          Right $ ApplicationKeyOcBars sComp k b o
--            Note fun -> do BarsF b <- bars conMap
--                           OctaveF o <- octave conMap
--                           Right $ ApplicationOcBars sComp b o
--            NoteN fun -> do BarsF b <- bars conMap
--                            Right $ ApplicationBars sComp b
--
--constrain arg = Left (errMsg arg "unknown reason")
----Missing case: Concat and Comma of Music without Application
--
-------------------------Context
--data SimpleValue = BarsF Int | OctaveF Int | KeyF String
--
--ctxValMapFun :: (Pa.Label,Pa.CtxValue) -> (Pa.Label,SimpleValue)
--ctxValMapFun (Pa.Bars,Pa.CNum (n,_)) =(Pa.Bars,BarsF $ fromIntegral n)
--ctxValMapFun (Pa.OctavePos,Pa.CNum (n,_)) =(Pa.OctavePos,OctaveF $ fromIntegral n)
--ctxValMapFun (Pa.Key,Pa.CNote (str,_)) =(Pa.Key,KeyF str)
--
--
--simplifyCon :: [(Pa.Label,Pa.CtxValue)]-> Map.Map Pa.Label SimpleValue 
--simplifyCon labelPairs = Map.fromList (map ctxValMapFun labelPairs)
--
--lookupLabel :: Pa.Label -> Map.Map Pa.Label SimpleValue -> Either String SimpleValue
--lookupLabel label con = case Map.lookup label con of
--                         Just val -> Right val
--                         Nothing -> Left (errMsg label) "not present in context"
--
--key = lookupLabel Pa.Key
--bars = lookupLabel Pa.Bars
--octave = lookupLabel Pa.OctavePos
--
----class Combinable a where
----  combine j
---------------------------------------
--
--
---------------------combineComp
--joinComps (Num fun1) (Num fun2) op = Right $ Num (\dur key octave -> (fun1 dur key octave) `op` (fun2 dur key octave))

--class CompFun a where
--  build :: STerm -> Either String a
--  increment :: a -> a
--  dot :: -> a -> a
--  combine :: a -> a-> (Music a -> Music a -> Music a) -> a
--  concat :: a -> a -> a
--  comma :: a -> a -> a

--data NumFun = NumFun (P.CtxValue -> P.CtxValue -> P.CtxValue -> Eu.Music Eu.Pitch)
--data NoteFun = NoteFun (P.CtxValue -> P.CtxValue -> Eu.Music Eu.Pitch)
--data NoteNFun = NoteNFun (P.CtxValue-> Eu.Music Eu.Pitch) 


--numT = TMusicalExp TByNum TNotPositioned
--noteT = TMusicalExp TByLetter TNotPositioned
--noteNT = TMusicalExp TByLetter TPositioned

scale = ["c","d","e","f","g","a","h"]
numToNote :: Integer -> String -> (Int,String)
numToNote n key = let Just offset = List.elemIndex key scale
                  in let (octave,index)= quotRem (offset+(fromIntegral n))  7
                     in (octave,scale!!index)

strToNoteFun :: String -> Eu.Octave -> Eu.Dur -> Eu.Music Eu.Pitch
strToNoteFun "a" = Eu.a
strToNoteFun "c" = Eu.c
strToNoteFun "d" = Eu.d
strToNoteFun "e" = Eu.e
strToNoteFun "f" = Eu.f
strToNoteFun "g" = Eu.g
strToNoteFun "h" = Eu.b



buildFun :: STerm -> Either String Fun
buildFun (SNum t n) = Right $ NumFun (\args -> case args of
                                                 [T.Dur dur,T.Key key,T.Octave octave] -> 
                                                       let (ocOffset,note) = numToNote n key
                                                       in let fun = strToNoteFun note
                                                          in Right $ fun (octave+ocOffset) dur
                                                 _ -> Left "error in NumFun")


--buildFun (SNum t n) = Right $ NumFun (\(T.Dur dur) (T.Key key) (T.Octave octave)-> 
--                                              let note = numToNote n key
--                                              in let fun = strToNoteFun note
--                                                 in fun octave dur)
buildFun (SNote t note) = Right $ NoteFun (\args -> 
                                              case args of
                                                [T.Dur dur,T.Octave octave] ->
                                                      let fun = strToNoteFun note
                                                      in Right $ fun octave dur
                                                _ -> Left "error in NoteFun")

--buildFun (SNote t note) = Right $ NoteFun (\(T.Dur dur) (T.Octave octave) -> 
--                                              let fun = strToNoteFun note
--                                              in fun octave dur)
buildFun (SNoteN t note octave) = Right $ NoteNFun (\args -> 
                                              case args of
                                                [T.Dur dur] ->
                                                      let fun = strToNoteFun note
                                                      in Right $ fun octave dur
                                                val -> Left $ "error in NoteNFun, was given "++(show val))



--buildFun (SNoteN t note n) = Right $ NoteNFun (\(T.Dur dur)-> 
--                                              let fun = strToNoteFun note
--                                              in fun (fromIntegral n) dur)

buildFun (SConcatted _ val1 val2) = 
                                   do fun1 <- buildFun val1 
                                      fun2 <- buildFun val2
                                      concatFuns fun1 fun2
buildFun (SCommaed _ val1 val2) = 
                                   do fun1 <- buildFun val1 
                                      fun2 <- buildFun val2
                                      commaFuns fun1 fun2

buildFun (SDotted t val) = do fun <- buildFun val
                              Right $ dot fun

buildFun _ = Left "error in buildFun"
  

combineInnerFuns innerFun1 innerFun2 op =  (\args -> do val1 <- innerFun1 args
                                                        val2 <- innerFun2 args
                                                        Right $ val1 `op` val2 )

concatInnerFuns innerFun1 innerFun2  = combineInnerFuns innerFun1 innerFun2 (Eu.:+:)
commaInnerFuns innerFun1 innerFun2  = combineInnerFuns innerFun1 innerFun2 (Eu.:=:)



concatFuns (NumFun inner1) (NumFun inner2) = Right $ NumFun $ concatInnerFuns inner1 inner2
concatFuns (NoteFun inner1) (NoteFun inner2) = Right $ NoteFun $ concatInnerFuns inner1 inner2
concatFuns (NoteNFun inner1) (NoteNFun inner2) = Right $ NoteNFun $ concatInnerFuns inner1 inner2
concatFuns _ _ = Left "error in concatFuns"

commaFuns (NumFun inner1) (NumFun inner2) = Right $ NumFun $ commaInnerFuns inner1 inner2
commaFuns (NoteFun inner1) (NoteFun inner2) = Right $ NoteFun $ commaInnerFuns inner1 inner2
commaFuns (NoteNFun inner1) (NoteNFun inner2) = Right $ NoteNFun $ commaInnerFuns inner1 inner2
commaFuns _ _ = Left "error in commaFuns"

dotInner innerFun = (\((T.Dur dur):rest)-> let newDur = T.Dur (incDur dur) 
                                           in let newArgs = newDur:rest
                                              in innerFun newArgs)

dot (NumFun fun) =  NumFun $ dotInner fun 
dot (NoteFun fun) = NoteFun $ dotInner fun
dot (NoteNFun fun) = NoteNFun $ dotInner fun 


incDur :: Eu.Dur -> Eu.Dur
incDur dur = (1+(numerator dur)) % (denominator dur)




--Right $ NumFun (\args -> do val1 <- fun1 args
--                                                                     val2 <- fun2 args
--                                                                     Right $ val1 (Eu.:+:)val2 )

--concatFuns (NoteFun fun1) (NoteFun fun2) = Right $ NoteFun (\args -> do val1 <- fun1 args
--                                                                       val2 <- fun2 args
--                                                                       Right $ val1 (Eu.:=:)val2 )
--
--concatFuns (NoteNFun fun1) (NoteNFun fun2) = Right $ NoteNFun (\dur -> (fun1 dur ) (Eu.:+:) (fun2 dur ))
--concatFuns arg1 arg2 = Left $ errMsg (arg1,arg2) "failed in concat"
--
--commaFuns (NumFun fun1) (NumFun fun2) = Right $ NumFun (\dur key octave -> (fun1 dur key octave) (Eu.:=:) (fun2 dur key octave))
--commaFuns (NoteFun fun1) (NoteFun fun2) = Right $ NoteFun (\dur octave -> (fun1 dur octave) (Eu.:=:) (fun2 dur octave))
--commaFuns (NoteNFun fun1) (NoteNFun fun2) = Right $ NoteNFun (\dur -> (fun1 dur ) (Eu.:=:) (fun2 dur ))
--commaFuns arg1 arg2 = Left $ errMsg (arg1,arg2) "failed in comma"







--buildGeneric :: (CompFun a) => STerm
--instance CompFun NumFun where
--  build (SNum t n) = NumFun (\(Dur dur) (Key key) (Octave octave)-> 
--                                                let note = numToNote n key
--                                                in let fun = strToNoteFun note
--                                                   in fun octave dur)
----maybe specify t?
--  build (SConcatted numT val1 val2) = 
--                                   do fun1 <- build val1 
--                                      fun2 <- build val2
--                                      concat fun1 fun2
--  build (Scommaed numT val1 val2) = do fun1 <- build val1 
--                                       fun2 <- build val2
--                                       comma fun1 fun2
--  build (SDotted numT val) = do fun <- build val
--                                dot fun
--  build _ = Left "tried to build non NumFun"
--  increment (NumFun fun) = NumFun (\(Dur dur)-> fun $ Dur (incDur dur))
--  dot = increment
--  combine (NumFun fun1) (NumFun fun2) op =  
--         NumFun (\dur key octave -> (fun1 dur key octave) `op` (fun2 dur key octave))
--
--  concat val1 val2  = combine val1 val2 (Eu.:+:)
--  comma val1 val2  = combine val1 val2 (Eu.:=:)
--
--
--instance CompFun NoteFun where
--  build (SNote note) = NoteFun (\(Dur dur) (Octave octave) -> 
--                                              let fun = strToNoteFun note
--                                              in fun octave dur)
--  build (SConcatted t val1 val2) = do fun1 <- build val1 
--                                      fun2 <- build val2
--                                      concat fun1 fun2
--  build (Scommaed t val1 val2) = do fun1 <- build val1 
--                                    fun2 <- build val2
--                                    comma fun1 fun2
--  build (SDotted t val) = do fun <- build val
--                             dot fun
--
--  build _ = Left "tried to build non NoteFun"
--  dot = increment
--
--  increment (NoteFun fun) =  NoteFun (\(Dur dur) -> fun $ Dur (incDur dur))
--
--  combine (NumFun fun1) (NumFun fun2) op =  
--         NumFun (\dur octave -> (fun1 dur octave) `op` (fun2 dur octave))
--
--  concat val1 val2  = combine val1 val2 (Eu.:+:)
--  comma val1 val2  = combine val1 val2 (Eu.:=:)
--
--
--instance CompFun NoteFun where
--  build (SNoteN note n) = Right $ NoteFun (\(Dur dur)-> 
--                                                   let fun = strToNoteFun note
--                                                   in fun (fromIntegral n) dur)
--  build (SConcatted t val1 val2) = do fun1 <- build val1 
--                                      fun2 <- build val2
--                                      concat fun1 fun2
--  build (Scommaed t val1 val2) = do fun1 <- build val1 
--                                    fun2 <- build val2
--                                    comma fun1 fun2
--  build (SDotted t val) = do fun <- build val
--                             dot fun
--
--  build _ = Left "tried to build non NoteFun"
--  build _ = Left "tried to build non NoteNFun"
--  dot = increment
--
--  increment (NoteNFun fun) = NoteNFun (\(Dur dur)-> fun (Dur (incDur dur)))
--
--  combine (NumFun fun1) (NumFun fun2) op =  
--         NumFun (\dur -> (fun1 dur ) `op` (fun2 dur ))
--
--  concat val1 val2  = combine val1 val2 (Eu.:+:)
--  comma val1 val2  = combine val1 val2 (Eu.:=:)



----data Comp =
----    Num (Eu.Dur -> String -> Eu.Octave -> Eu.Music Eu.Pitch)
----  | Note (Eu.Dur -> Eu.Octave -> Eu.Music Eu.Pitch)
----  | NoteN  (Eu.Dur-> Eu.Music Eu.Pitch) 
--
--
--incrementTerm :: (Comp a)-> Comp a
--incrementTerm (Num fun) =  Num (\dur -> fun (incDur dur))
--incrementTerm (Note fun) = Note (\dur -> fun (incDur dur))
--incrementTerm (NoteN fun) = NoteN (\dur -> fun (incDur dur))
----incrementTerm arg  = Left (errMsg arg) "at final definition of incrementTerm"
--
--
--
--buildFun :: STerm -> Either String (
--combineComp :: STerm -> Either String (Comp a)
--combineComp (SNum n) = Right $ Num (\dur key octave -> let note = numToNote n key
--                                                         in let fun = strToNoteFun note
--                                                            in fun octave dur)
----remember change pattern match in substitution
--combineComp (SNote str) t = Right $ Note (\dur octave -> let fun = strToNoteFun str
--                                                         in fun octave dur)
--
--combineComp (SNoteN str n) t = Right $ NoteN (\dur ->  let fun = strToNoteFun str
--                                                       in fun (fromIntegral n) dur)
--
--combineComp (SDotted term) t = do sTerm <- combineComp term t
--                                  Right $ incrementTerm sTerm
--
--combineComp (SConcatted t1 t2) t = do sT1 <- combineComp t1 t
--                                      sT2 <- combineComp t2 t
--                                      joinComps sT1 sT2 (Eu.:+:)
--
--combineComp (SCommaed t1 t2) t = do sT1 <- combineComp t1 t
--                                    sT2 <- combineComp t2 t
--                                    joinComps sT1 sT2 (Eu.:=:)
--
--combineComp arg t = Left (errMsg arg "Reason: non-comp given as argument to combineComp")
------------------------------------------------
----
----
----
----
----
----
----evaluateMusic :: Music -> Either String (Eu.Music Eu.Pitch)
----
----joinComps (Num fun1) (Num fun2) op = Right $ Num (\dur key octave -> (fun1 dur key octave) `op` (fun2 dur key octave))
----joinComps (Note fun1) (Note fun2) op = Right $ Note (\dur octave -> (fun1 dur octave) `op` (fun2 dur octave))
----joinComps (NoteN fun1) (NoteN fun2) op = Right $ NoteN (\dur -> (fun1 dur) `op` (fun2 dur))
----joinComps arg1 arg2 = Left $ (errMsg arg1 "")++(errMsg arg2 "")
----
-------reduceComp :: Comp a -> Comp a
---------reduceComp (Concatted sT1 sT2) = joinSimpleTerms sT1 sT2 (Eu.:+:)
---------reduceComp (Commaed sT1 sT2) = joinSimpleTerms sT1 sT2 (Eu.:=:)
-------reduceComp (Num fun) = Num fun
-------reduceComp (Note fun) = Note fun
-------reduceComp (NoteN fun) = NoteN fun
----
----evaluateMusic (ApplicationKeyOcBars (Num fun) k o b) = 
----    Right $ fun k o b
----
----evaluateMusic (ApplicationOcBars (Note fun) o b) = 
----    Right $ fun o b
----
----evaluateMusic (ApplicationBars (NoteN fun) b) = 
----    Right $ fun b
----
----evaluateMusic arg = Left (errMsg arg) "at end of evaluateMusic definition"
----
----
----
----
------case reduceComp comp of 
------                                                 Num fun -> let Bars b = bars lp
------                                                                k = key lp
------                                                                o = octave lp
------                                                            in fun (1%b) k o
------                                                 Note fun -> let Bars b = bars lp
------                                                                 Octave o = octave lp
------                                                             in fun (1%b) o
------                                                 NoteN fun -> let Bars b = bars lp
------                                                              in fun (1%b)
------
----
----
----
----
----
----
----
----evaluate :: Pa.Exp -> Either String (Eu.Music Eu.Pitch)
----evaluate exp = do sTerm <- substitute exp
----                  simpleTerm <- simplify sTerm
----                  evaluate simpleTerm

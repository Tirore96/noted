{-# LANGUAGE GADTs #-}
module Evaluator where

import qualified Data.Map.Strict as Map
import qualified Calc.Parser as P
import Control.Monad.State.Lazy


--data Value = VNum Integer | VLetter String | Tuple Value Value | VDot | VCons P.Dim Integer Value Value | VPlay | VLam String P.Exp
--

data Assignment = Assignment String Exp
  deriving(Eq,Show)

data Exp = Num Integer
           | Letter String
           | Tuple Exp Exp
           | Dot 
           | Cons P.Dim Int Exp Exp
           | Var String
           | Let String Exp Exp
           | Play Exp Exp
           | Lam [Exp] Exp
           | App Exp Exp
           | Case Exp [Exp]
           | PComp P.Dim Exp Exp


  deriving(Eq,Show)

convertAssignment :: P.Assignment -> Assignment
convertAssignment (P.Assignment s e) = Assignment s (convertExp e)

convertAssignments :: [P.Assignment] -> [Assignment]
convertAssignments asses = map convertAssignment asses

convertExp :: P.Exp -> Exp 

convertExp (P.Num (n,_)) = Num n

convertExp (P.Letter (s,_)) = Letter s

convertExp (P.Tuple ((e1,e2),_)) = let e1_con = convertExp e1
                                       e2_con = convertExp e2
                                   in Tuple e1_con e2_con
convertExp (P.Dot _) = Dot

convertExp (P.Cons ((d,n,e1,e2),_)) = let e1_con = convertExp e1
                                          e2_con = convertExp e2
                                      in Cons d n e1_con e2_con

convertExp (P.Var (s,_)) = Var s

convertExp (P.Let ((str,e1,e2),_)) = let e1_con = convertExp e1
                                         e2_con = convertExp e2
                                     in Let str e1_con e2_con

convertExp (P.Play ((e1,e2),_)) = Play (convertExp e1) (convertExp e2)

convertExp (P.Lam ((es,e),_)) = Lam (map convertExp es) (convertExp e)

convertExp (P.App ((e1,e2),_)) = let e1_con = convertExp e1
                                     e2_con = convertExp e2
                                 in App e1_con e2_con

convertExp (P.PComp ((d,e1,e2),_)) =  let e1_con = convertExp e1
                                          e2_con = convertExp e2
                                      in PComp d e1_con e2_con


convertExp (P.Case ((e1,es),_)) =  -- For now don't substitute d and n because they are not P.Exp
  let e1_con = convertExp e1
      es_con = map convertExp es
  in Case e1_con es_con


type StateData = Map.Map String Exp

type SEME a = StateT StateData (Either String) a

updateState :: String -> Exp ->  SEME ()
updateState str t = StateT (\s -> Right ((),Map.insert str t s))

lookupVar :: String -> SEME Exp
lookupVar str = StateT (\s -> case Map.lookup str s of 
                                Just e -> Right (e,s)
                                Nothing -> Left ("bad variable lookup: "++str++" "++(show s)))

reduce :: [Assignment] -> Either String Exp
reduce program = let reduceM = do mapM_ reduceAssignment program
                                  m <- lookupVar "main"
                                  reduceTerm m
                 in case runStateT reduceM Map.empty of
                      Right (val,_) -> Right val
                      Left err -> Left err


reduceAssignment :: Assignment -> SEME ()
reduceAssignment (Assignment s e1 ) = updateState s e1



substitute :: Exp -> String -> Exp -> Exp
substitute (Tuple e1 e2) str sub_e = let e1_sub = substitute e1 str sub_e
                                         e2_sub = substitute e2 str sub_e
                                     in Tuple e1_sub e2_sub
substitute (Cons d n e1 e2) str sub_e  = let e1_sub = substitute e1 str sub_e
                                             e2_sub = substitute e2 str sub_e
                                         in Cons d n e1_sub e2_sub
substitute e@(Var var) str sub_e = if var == str
                                      then sub_e
                                      else e

substitute (Let var e1 e2) str sub_e = 
  let e1_sub = substitute e1 str sub_e
      e2_sub = substitute e2 str sub_e                                             
  in Let var e1_sub e2_sub

substitute (Play e1 e2 ) str sub_e = let e1_sub = substitute e1 str sub_e
                                         e2_sub = substitute e2 str sub_e

                                     in Play e1_sub e2_sub

substitute (Lam var e1) str sub_e = let e1_sub = substitute e1 str sub_e
                                    in Lam var e1_sub


substitute (App e1 e2) str sub_e =
  let e1_sub = substitute e1 str sub_e
      e2_sub = substitute e2 str sub_e
  in App e1_sub e2_sub

substitute (Case e1 es) str sub_e =
  let e1_sub = substitute e1 str sub_e
      es_sub = map (\e -> substitute e str sub_e) es
  in Case e1_sub es_sub

substitute (PComp dim e1 e2) str sub_e =
  let e1_sub = substitute e1 str sub_e
      e2_sub = substitute e2 str sub_e
  in PComp dim e1_sub e2_sub


substitute e _ _ = e

reduceTerm :: Exp -> SEME Exp
reduceTerm e@(Num _) = return e
reduceTerm e@(Letter _) = return e
reduceTerm (Tuple e1 e2) = do v1 <- reduceTerm e1
                              v2 <- reduceTerm e2
                              return $ Tuple v1 v2
reduceTerm Dot = return Dot
reduceTerm (Cons d n e1 e2) = do v1 <- reduceTerm e1
                                 v2 <- reduceTerm e2
                                 return $ Cons d n v1 v2
reduceTerm (Var s) = do e <- lookupVar s
                        res <- reduceTerm e
                        updateState s res
                        return res
reduceTerm (Let str e1 e2) = let e2_sub = substitute e2 str e1
                             in reduceTerm e2_sub


reduceTerm (Play e1 e2) = do v1 <- reduceTerm e1
                             v2 <- reduceTerm e2
                             return $ Play v1 v2

reduceTerm e@(Lam _ _) = return e

reduceTerm (App e1 e2) = do v1 <- reduceTerm e1
                            (p,ps,e0) <- case v1 of
                                          Lam (p:ps) e0 -> return (p,ps,e0)
                                          e -> failM ("not lambda, is: "++show e)
                            v_arg <- reduceTerm e2
                            let e_sub =  applyPattern e0 p v_arg
                            case e_sub of 
                              Right e -> case ps of
                                           [] -> reduceTerm e
                                           l -> return $ Lam l e
                              Left f -> failM f
                 

reduceTerm (Case e1 es ) =  -- For now don't substitute d and n because they are not P.Exp
  do v1 <- reduceTerm e1
     case firstMatchingLam v1 es of 
       Right e -> reduceTerm e
       Left f -> failM f

reduceTerm (PComp dim e1 e2) = do v1 <- reduceTerm e1
                                  v2 <- reduceTerm e2
                                  return $ PComp dim v1 v2

firstMatchingLam :: Exp -> [Exp] -> Either String Exp
firstMatchingLam e [] = Left ("no pattern matched, for e: "++(show e))
firstMatchingLam v ((Lam [var] e_body):ls) = case applyPattern e_body var v of
                                               Right e -> Right e
                                               Left _ -> firstMatchingLam v ls
firstMatchingLam _ _ = Left "bad lambda for case"

                                               

applyPattern :: Exp -> Exp -> Exp -> Either String Exp
applyPattern e_body (Var s) v_arg = Right $ substitute e_body s v_arg
applyPattern e_body (Cons _ _ (Var s1) (Var s2)) (Cons _ _ v1 v2) = let sub1 =  substitute e_body s1 v1
                                                                    in Right $ substitute sub1 s2 v2
applyPattern e_body (Cons _ _ Dot (Var s2)) (Cons _ _ Dot v2) = Right $ substitute e_body s2 v2
applyPattern _ p v = Left ("failed pattern: p"++(show p)++" "++(show v))


failM :: String -> SEME a
failM str = StateT (\_ -> Left str) 


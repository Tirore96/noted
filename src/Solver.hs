module Solver where

import ConstraintGenerator
import qualified Calc.Parser as P

type SolveResult = Either String [Constraint]

solve :: [Constraint] -> Either String [Constraint]
solve cons = do 
  potential_solution <- solveConstraints cons 
  if verifySolution potential_solution
    then return $ potential_solution
    else solve potential_solution

--solveDeferedTests 
verifySolution :: [Constraint] -> Bool
verifySolution [] = True
verifySolution _ = False

solveConstraints :: [Constraint] -> SolveResult
solveConstraints [] = return []
solveConstraints (first@(Equals expected actual _):rest) = --How to handle Equals (expected:fresh) (acftual:t)?
  if sameType expected actual--Maybe subtype here?
    then solveConstraints rest
    else solveConstraintsHelper (first:rest)

--Substitute

solveConstraintsHelper ::  [Constraint]  -> Either String [Constraint]

solveConstraintsHelper [] = Right []

solveConstraintsHelper ((Equals (BFresh n,_) (sub,_) _ ):cons) =
  Right $ (substituteConstraintsB n sub cons)

solveConstraintsHelper ((Equals (_,CFresh n) (_,sub) _ ):cons) =
  Right $ (substituteConstraintsC n sub cons)

solveConstraintsHelper ((Equals (sub,_) (BFresh n,_) _ ):cons) =
  Right $ (substituteConstraintsB n sub cons)

solveConstraintsHelper ((Equals (_,sub) (_,CFresh n) _ ):cons) =
  Right $ (substituteConstraintsC n sub cons)

solveConstraintsHelper ((Equals t1 t2 term):_) = Left (makeErrorMsg t1 t2 term)


makeErrorMsg :: Type -> Type -> P.Term -> String
makeErrorMsg expected actual term =
  "Expected "++(show expected)++", but got "++(show actual)++" at "++(show (P.getPos term))++" in expression "++(show term)

substituteConstraintsB :: Integer -> BaseType -> [Constraint]-> [Constraint]
substituteConstraintsB n sub cons =map (substituteConstraintB n sub) cons

substituteConstraintB :: Integer -> BaseType -> Constraint -> Constraint
substituteConstraintB n sub (Equals t1 t2 term) = Equals (substituteB n sub t1) t2 term

substituteB :: Integer -> BaseType -> Type -> Type
substituteB n sub original@((BFresh n2), t2) = 
  if n == n2
    then (sub,t2) 
    else original

substituteB _ _ t2 = t2

substituteConstraintsC :: Integer -> CardinalType -> [Constraint]-> [Constraint]
substituteConstraintsC n sub cons = map (substituteConstraintC n sub) cons

substituteConstraintC :: Integer -> CardinalType-> Constraint -> Constraint
substituteConstraintC n sub (Equals t1 t2 term) = Equals t1 (substituteC n sub t2) term

substituteC :: Integer -> CardinalType -> Type -> Type
substituteC n sub original@(t1,(CFresh n2)) = 
  if n == n2
    then (t1,sub) 
    else original

substituteC _ _ t2 = t2


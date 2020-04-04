module Definitions(testPos,testAlex,testHappy) where
import Calc.Lexer
import Calc.Parser

--data TestToken =
--  TTNum Integer | TTNote Char | TTChord String | TTOBracket | TTCBracket | TTQuant | TTTempo | TTKey | TTOctavePos 
--  | TTEq | TTInputK | TTChordK | TTQuote | TTComma | TTDot | TTDiv | TTPlus | TTMinus | TTSharp | TTMin | TTFlat | TTUnderscore | TTR | TTVar String
--  deriving (Eq,Show)

testPos = AlexPn 0 0 0


--makeTokenTestable :: Token -> Token
--makeTokenTestable (TNum (_,n)) = TNum (testPos, n)
--makeTokenTestable (TNote(_,c)) = TNote (testPos, c)
--makeTokenTestable (TChord (_,s)) = TChord (testPos, s)
--makeTokenTestable (TOBracket _) = TOBracket testPos
--makeTokenTestable (TCBracket _) = TCBracket testPos
--makeTokenTestable (TCtxWord (_,s)) = TCtxWord (testPos,s)
--makeTokenTestable (TEq _) = TEq testPos
--makeTokenTestable (TInputK _) = TInputK testPos
--makeTokenTestable (TChordK _) = TChordK testPos
--makeTokenTestable (TQuote _) = TQuote testPos
--makeTokenTestable (TComma _) = TComma testPos
--makeTokenTestable (TDot _)  = TDot testPos
--makeTokenTestable (TDiv _)  = TDiv testPos
--makeTokenTestable (TPlus _) = TPlus testPos
--makeTokenTestable (TMinus _)  = TMinus testPos
--makeTokenTestable (TSharp _) = TSharp testPos
--makeTokenTestable (TMin _)  = TMin testPos
--makeTokenTestable (TFlat _) = TFlat testPos
--makeTokenTestable (TUnderscore _) = TUnderscore testPos
--makeTokenTestable (TVar (_,s)) = TVar (testPos,s)
--makeTokenTestable (TR _) = TR testPos
--makeTokenTestable (TEOF _) = TEOF testPos


makeTokenTestable (T _ c s) = T testPos c s


makeTokensTestable :: Either String [Token] ->  Either String [Token]
makeTokensTestable (Right tokens) = Right $ map makeTokenTestable tokens
makeAbSynTestable (Left s) = Left s

testAlex = makeTokensTestable . scanner

testHappy str = do tokens <- testAlex str 
                   parseTokens tokens


--makeAbSynTestable :: a ->  a 
--makeAbSynTestable (stmt:stmts) = (makeAbSynTestable stmt):(makeAbSynTestable(stmts))
--makeAbSynTestable [] = []
--makeAbSynTestable (Assignment (Var _ s) term) = 
--    let testableTerm = makeAbSynTestable term
--        newVariable = Variable testPos s
--    in Assignment newVariable testableTerm
--makeAbSynTestable (Assignment (Quant _) term) =
--makeAbSynTestable (Num _ n) = Num testPos n
--makeAbSynTestable (Note _ c) = Note testPos c
--makeAbSynTestable (ContextWord _ bCtxword) = ContextWord testPos bCtxword
--makeAbSynTestable (Sharp term) = 
--    let testableTerm = makeAbSynTestable term
--    in Sharp testableTerm
--makeAbSynTestable (Struct l) = Struct $ map makeAbSynTestable l



--makeTestable :: [Token] -> [TestToken]
--makeTestable [] = []
--makeTestable (token:tokens)= 
--    case token of
--        TNum (p,n)-> TTNum n:  makeTestable tokens
--        TNote (p,s)-> TTNote s:  makeTestable tokens
--        TChord (p,s)-> TTChord s:  makeTestable tokens
--        TOBracket p-> TTOBracket:  makeTestable tokens
--        TCBracket p-> TTCBracket:  makeTestable tokens
--        TQuant p -> TTQuant:  makeTestable tokens
--        TTempo p -> TTTempo: makeTestable tokens
--        TKey p-> TTKey:  makeTestable tokens
--        TOctavePos p-> TTOctavePos:  makeTestable tokens
--        TEq p-> TTEq:  makeTestable tokens
--        TInputK p-> TTInputK:  makeTestable tokens
--        TChordK p-> TTChordK:  makeTestable tokens
--        TQuote p-> TTQuote:  makeTestable tokens
--        TComma p-> TTComma:  makeTestable tokens
--        TDot p-> TTDot:  makeTestable tokens
--        TDiv p-> TTDiv:  makeTestable tokens
--        TPlus p-> TTPlus:  makeTestable tokens
--        TMinus p-> TTMinus:  makeTestable tokens
--        TSharp p-> TTSharp:  makeTestable tokens
--        TMin p-> TTMin:  makeTestable tokens
--        TFlat p-> TTFlat:  makeTestable tokens
--        TUnderscore p-> TTUnderscore:  makeTestable tokens
--        TVar (p,s)-> TTVar s:  makeTestable tokens
--        TR p-> TTR:  makeTestable tokens

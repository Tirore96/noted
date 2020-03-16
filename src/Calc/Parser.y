{
module Parser where
}

%parseTokens
%tokentype { Token }
%error { parseError }

%token
  schar    {TSChar $$ }
  input    {TInput}		
  chord    {TChord}
  num      {TNum $$}
  op	   {TOp $$}
  var	   {TVar $$}
  note	   {TNote $$}
  eof	   {TEOF}

Program = Stmts 
Stmts = Stmt newline Stmts | Stmt
Stmt = Comment | Term                                                                                                   
Term = Term '=' Term | 'chord' Term Term | 'input' Term Term | 'repeat' Term Term | Term Term | Term Op Term | String 
		| '"\' Term '->' Term '"' | '"' Items '"' | Constant
Items = Constant Items | Delimiter Items | Constant

--Op = '+' | '-' | '*' | '%' | '=='

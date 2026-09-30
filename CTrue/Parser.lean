import CTrue.Lexer

namespace CTrue

/-! ### AST (Listing 2-5)

```
program             = Program(function_definition)
function_definition = Function(identifier name, statement body)
statement           = Return(exp)
exp                 = Constant(int) | Unary(unary_operator, exp)
unary_operator      = Complement | Negate
```
-/

inductive UnaryOperator where
  | complement
  | negate
deriving Repr, DecidableEq, BEq, Inhabited

inductive Expression where
  | constant (value : Nat)
  | unary (operator : UnaryOperator) (inner : Expression)
deriving Repr, DecidableEq, BEq, Inhabited

inductive Statement where
  | ret (value : Expression)
deriving Repr, DecidableEq, BEq, Inhabited

structure FunctionDef where
  name : String
  body : Statement
deriving Repr, DecidableEq, BEq, Inhabited

structure Program where
  function : FunctionDef
deriving Repr, DecidableEq, BEq, Inhabited

def UnaryOperator.pretty : UnaryOperator → String
  | .complement => "Complement"
  | .negate     => "Negate"

def Expression.pretty (indent : String) (expression : Expression) : String :=
  match expression with
  | .constant value => s!"Constant({value})"
  | .unary operator inner =>
    s!"Unary({operator.pretty},\n\
       {indent}  {inner.pretty (indent ++ "  ")}\n\
       {indent})"

def Statement.pretty (indent : String) (statement : Statement) : String :=
  match statement with
  | .ret value =>
    s!"Return(\n{indent}  {value.pretty (indent ++ "  ")}\n{indent})"

def FunctionDef.pretty (indent : String) (functionDef : FunctionDef) : String :=
  let innerIndent := indent ++ "    "
  s!"Function(\n\
     {innerIndent}name=\"{functionDef.name}\",\n\
     {innerIndent}body={functionDef.body.pretty innerIndent}\n\
     {indent})"

def Program.pretty (program : Program) : String :=
  s!"Program(\n    {program.function.pretty "    "}\n)"

instance : ToString Program := ⟨Program.pretty⟩

/-! ### Recursive descent parser (Listing 2-6)

```
<program>    ::= <function>
<function>   ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
<statement>  ::= "return" <exp> ";"
<exp>        ::= <int> | <unop> <exp> | "(" <exp> ")"
<unop>       ::= "-" | "~"
<identifier> ::= ? An identifier token ?
<int>        ::= ? A constant token ?
```

Each parser takes the tokens it has not consumed yet and returns the AST node
it built together with the tokens still left over.
-/

def describeNextToken : List Token → String
  | []          => "end of input"
  | token :: _  => s!"\"{token}\""

def expect (expected : Token) (tokens : List Token) : Except String (List Token) :=
  match tokens with
  | token :: remainingTokens =>
      if token == expected then .ok remainingTokens
      else .error s!"Expected \"{expected}\" but found {describeNextToken tokens}"
  | [] => .error s!"Expected \"{expected}\" but found end of input"

def parseIdentifier (tokens : List Token) : Except String (String × List Token) :=
  match tokens with
  | .ident name :: remainingTokens => .ok (name, remainingTokens)
  | _ => .error s!"Expected an identifier but found {describeNextToken tokens}"

def parseExpression : List Token → Except String (Expression × List Token)
  | .const value :: remainingTokens => .ok (.constant value, remainingTokens)
  | .telda :: remainingTokens => do
      let (inner, remainingTokens) ← parseExpression remainingTokens
      return (.unary .complement inner, remainingTokens)
  | .negate :: remainingTokens => do
      let (inner, remainingTokens) ← parseExpression remainingTokens
      return (.unary .negate inner, remainingTokens)
  | .lparen :: remainingTokens => do
      let (inner, remainingTokens) ← parseExpression remainingTokens
      let remainingTokens ← expect .rparen remainingTokens
      return (inner, remainingTokens)
  | tokens => .error s!"Expected an expression but found {describeNextToken tokens}"

def parseStatement (tokens : List Token) : Except String (Statement × List Token) := do
  let tokens ← expect .kwReturn tokens
  let (value, tokens) ← parseExpression tokens
  let tokens ← expect .semi tokens
  return (.ret value, tokens)

def parseFunction (tokens : List Token) : Except String (FunctionDef × List Token) := do
  let tokens ← expect .kwInt tokens
  let (name, tokens) ← parseIdentifier tokens
  let tokens ← expect .lparen tokens
  let tokens ← expect .kwVoid tokens
  let tokens ← expect .rparen tokens
  let tokens ← expect .lbrace tokens
  let (body, tokens) ← parseStatement tokens
  let tokens ← expect .rbrace tokens
  return ({ name, body }, tokens)

def parse (tokens : List Token) : Except String Program := do
  let (functionDef, remainingTokens) ← parseFunction tokens
  if !remainingTokens.isEmpty then
    throw s!"Expected end of input but found {describeNextToken remainingTokens}"
  return { function := functionDef }

-- Tests
private def astOfSource (source : String) : Option Program :=
  (lex source >>= parse).toOption

private def parseErrorOfSource (source : String) : Option String :=
  match lex source >>= parse with
  | .error message => some message
  | .ok _          => none

private def mainReturns2 : Program :=
  { function := { name := "main", body := .ret (.constant 2) } }

#guard astOfSource "int main(void) { return 2; }" == some mainReturns2

#guard astOfSource "int main(void) {\n    return 2;\n}\n" == some mainReturns2
#guard astOfSource "int  main ( void )  {  return  2  ;  }" == some mainReturns2

#guard astOfSource "int foo(void) { return 0; }"
        == some { function := { name := "foo", body := .ret (.constant 0) } }
#guard astOfSource "int _f1(void) { return 1000; }"
        == some { function := { name := "_f1", body := .ret (.constant 1000) } }

private def astFromTokenList : Option Program :=
  (parse [.kwInt, .ident "main", .lparen, .kwVoid, .rparen,
          .lbrace, .kwReturn, .const 2, .semi, .rbrace]).toOption
#guard astFromTokenList == some mainReturns2

#guard (astOfSource "int main(void) { return 2; }").map toString
        == some "Program(\n    Function(\n        name=\"main\",\n        body=Return(\n          Constant(2)\n        )\n    )\n)"

#guard parseErrorOfSource "main(void) { return 2; }"
        == some "Expected \"int\" but found \"ident(main)\""
#guard parseErrorOfSource "int 3(void) { return 2; }"
        == some "Expected an identifier but found \"constant(3)\""
#guard parseErrorOfSource "int main) { return 2; }"
        == some "Expected \"(\" but found \")\""
#guard parseErrorOfSource "int main() { return 2; }"
        == some "Expected \"void\" but found \")\""
#guard parseErrorOfSource "int main(void) return 2;"
        == some "Expected \"{\" but found \"return\""
#guard parseErrorOfSource "int main(void) { 2; }"
        == some "Expected \"return\" but found \"constant(2)\""
#guard parseErrorOfSource "int main(void) { return foo; }"
        == some "Expected an expression but found \"ident(foo)\""
#guard parseErrorOfSource "int main(void) { return 2 }"
        == some "Expected \";\" but found \"}\""
#guard parseErrorOfSource "int main(void) { return 2;"
        == some "Expected \"}\" but found end of input"

#guard parseErrorOfSource "int main(void) {" == some "Expected \"return\" but found end of input"
#guard parseErrorOfSource "" == some "Expected \"int\" but found end of input"

#guard parseErrorOfSource "int main(void) { return 2; } foo"
        == some "Expected end of input but found \"ident(foo)\""
#guard parseErrorOfSource "int main(void) { return 2; } int f(void) { return 3; }"
        == some "Expected end of input but found \"int\""

-- Chapter 2: unary operators

private def retExp (expression : Expression) : Program :=
  { function := { name := "main", body := .ret expression } }

#guard astOfSource "int main(void) { return -2; }"
        == some (retExp (.unary .negate (.constant 2)))
#guard astOfSource "int main(void) { return ~0; }"
        == some (retExp (.unary .complement (.constant 0)))
#guard astOfSource "int main(void) { return ~-3; }"
        == some (retExp (.unary .complement (.unary .negate (.constant 3))))
#guard astOfSource "int main(void) { return -~-1; }"
        == some (retExp (.unary .negate (.unary .complement (.unary .negate (.constant 1)))))

#guard astOfSource "int main(void) { return (2); }" == some mainReturns2
#guard astOfSource "int main(void) { return (((2))); }" == some mainReturns2
#guard astOfSource "int main(void) { return -(2); }"
        == some (retExp (.unary .negate (.constant 2)))
#guard astOfSource "int main(void) { return (-2); }"
        == some (retExp (.unary .negate (.constant 2)))
#guard astOfSource "int main(void) { return -(~(-~-(-4))); }"
        == some (retExp (.unary .negate (.unary .complement (.unary .negate
             (.unary .complement (.unary .negate (.unary .negate (.constant 4))))))))

private def prettyOfComplementNegate2 : String :=
  String.intercalate "\n"
    [ "Program(",
      "    Function(",
      "        name=\"main\",",
      "        body=Return(",
      "          Unary(Complement,",
      "            Unary(Negate,",
      "              Constant(2)",
      "            )",
      "          )",
      "        )",
      "    )",
      ")" ]

#guard (astOfSource "int main(void) { return ~-2; }").map toString
        == some prettyOfComplementNegate2

#guard parseErrorOfSource "int main(void) { return --2; }"
        == some "Expected an expression but found \"--\""
#guard parseErrorOfSource "int main(void) { return -(--2); }"
        == some "Expected an expression but found \"--\""

#guard parseErrorOfSource "int main(void) { return -; }"
        == some "Expected an expression but found \";\""
#guard parseErrorOfSource "int main(void) { return ~; }"
        == some "Expected an expression but found \";\""
#guard parseErrorOfSource "int main(void) { return -2" == some "Expected \";\" but found end of input"
#guard parseErrorOfSource "int main(void) { return (2; }"
        == some "Expected \")\" but found \";\""
#guard parseErrorOfSource "int main(void) { return (); }"
        == some "Expected an expression but found \")\""
#guard parseErrorOfSource "int main(void) { return 2); }"
        == some "Expected \";\" but found \")\""

end CTrue

import CTrue.Lexer

namespace CTrue

/-! ### AST (Listing 1-5)

```
program             = Program(function_definition)
function_definition = Function(identifier name, statement body)
statement           = Return(exp)
exp                 = Constant(int)
```
-/

inductive Exp where
  | constant (value : Nat)
deriving Repr, DecidableEq, BEq, Inhabited

inductive Stmt where
  | ret (value : Exp)
deriving Repr, DecidableEq, BEq, Inhabited

structure FunctionDef where
  name : String
  body : Stmt
deriving Repr, DecidableEq, BEq, Inhabited

structure Program where
  function : FunctionDef
deriving Repr, DecidableEq, BEq, Inhabited

-- Pretty pritning for debugging
def Exp.pretty (_indent : String) : Exp → String
  | constant v => s!"Constant({v})"

def Stmt.pretty (indent : String) : Stmt → String
  | ret value =>
    s!"Return(\n{indent}  {value.pretty (indent ++ "  ")}\n{indent})"

def FunctionDef.pretty (indent : String) (f : FunctionDef) : String :=
  let inner := indent ++ "    "
  s!"Function(\n{inner}name=\"{f.name}\",\n{inner}body={f.body.pretty inner}\n{indent})"

def Program.pretty (p : Program) : String :=
  s!"Program(\n    {p.function.pretty "    "}\n)"

instance : ToString Program := ⟨Program.pretty⟩

/-! ### Recursive descent parser (Listing 1-6)

```
<program>   ::= <function>
<function>  ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp>       ::= <int>
```
-/

def found : List Token → String
  | []     => "end of input"
  | t :: _ => s!"\"{t}\""

def expect (expected : Token) (ts : List Token) : Except String (List Token) :=
  match ts with
  | t :: rest => if t == expected then .ok rest
                 else .error s!"Expected \"{expected}\" but found {found ts}"
  | []        => .error s!"Expected \"{expected}\" but found end of input"

def parseIdentifier (ts : List Token) : Except String (String × List Token) :=
  match ts with
  | .ident name :: rest => .ok (name, rest)
  | _ => .error s!"Expected an identifier but found {found ts}"

def parseExp (ts : List Token) : Except String (Exp × List Token) :=
  match ts with
  | .const value :: rest => .ok (.constant value, rest)
  | _ => .error s!"Expected a constant but found {found ts}"

def parseStatement (ts : List Token) : Except String (Stmt × List Token) := do
  let ts ← expect .kwReturn ts
  let (value, ts) ← parseExp ts
  let ts ← expect .semi ts
  return (.ret value, ts)

def parseFunction (ts : List Token) : Except String (FunctionDef × List Token) := do
  let ts ← expect .kwInt ts
  let (name, ts) ← parseIdentifier ts
  let ts ← expect .lparen ts
  let ts ← expect .kwVoid ts
  let ts ← expect .rparen ts
  let ts ← expect .lbrace ts
  let (body, ts) ← parseStatement ts
  let ts ← expect .rbrace ts
  return ({ name, body }, ts)

def parse (ts : List Token) : Except String Program := do
  let (function, rest) ← parseFunction ts
  if !rest.isEmpty then
    throw s!"Expected end of input but found {found rest}"
  return { function }

-- Tests
private def astOf (source : String) : Option Program :=
  (lex source >>= parse).toOption

private def parseErr (source : String) : Option String :=
  match lex source >>= parse with
  | .error e => some e
  | .ok _    => none

private def main2 : Program :=
  { function := { name := "main", body := .ret (.constant 2) } }

#guard astOf "int main(void) { return 2; }" == some main2

#guard astOf "int main(void) {\n    return 2;\n}\n" == some main2
#guard astOf "int  main ( void )  {  return  2  ;  }" == some main2

#guard astOf "int foo(void) { return 0; }"
        == some { function := { name := "foo", body := .ret (.constant 0) } }
#guard astOf "int _f1(void) { return 1000; }"
        == some { function := { name := "_f1", body := .ret (.constant 1000) } }

private def parseTokens : Option Program :=
  (parse [.kwInt, .ident "main", .lparen, .kwVoid, .rparen,
          .lbrace, .kwReturn, .const 2, .semi, .rbrace]).toOption
#guard parseTokens == some main2

#guard (astOf "int main(void) { return 2; }").map toString
        == some "Program(\n    Function(\n        name=\"main\",\n        body=Return(\n          Constant(2)\n        )\n    )\n)"

#guard parseErr "main(void) { return 2; }"
        == some "Expected \"int\" but found \"ident(main)\""
#guard parseErr "int 3(void) { return 2; }"
        == some "Expected an identifier but found \"constant(3)\""
#guard parseErr "int main) { return 2; }"
        == some "Expected \"(\" but found \")\""
#guard parseErr "int main() { return 2; }"
        == some "Expected \"void\" but found \")\""
#guard parseErr "int main(void) return 2;"
        == some "Expected \"{\" but found \"return\""
#guard parseErr "int main(void) { 2; }"
        == some "Expected \"return\" but found \"constant(2)\""
#guard parseErr "int main(void) { return foo; }"
        == some "Expected a constant but found \"ident(foo)\""
#guard parseErr "int main(void) { return 2 }"
        == some "Expected \";\" but found \"}\""
#guard parseErr "int main(void) { return 2;"
        == some "Expected \"}\" but found end of input"

#guard parseErr "int main(void) {" == some "Expected \"return\" but found end of input"
#guard parseErr "" == some "Expected \"int\" but found end of input"

#guard parseErr "int main(void) { return 2; } foo"
        == some "Expected end of input but found \"ident(foo)\""
#guard parseErr "int main(void) { return 2; } int f(void) { return 3; }"
        == some "Expected end of input but found \"int\""

end CTrue

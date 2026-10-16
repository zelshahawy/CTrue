import CTrue.Parser

namespace CTrue

namespace Tacky

inductive UnaryOperator where
  | complement
  | negate
deriving Repr, DecidableEq, BEq, Inhabited

inductive Val where
  | constant (value : Nat)
  | var (name : String)
deriving Repr, DecidableEq, BEq, Inhabited

inductive Instruction where
  | ret (value : Val)
  | unary (operator : UnaryOperator) (source destination : Val)
deriving Repr, DecidableEq, BEq, Inhabited

structure FunctionDef where
  name : String
  body : List Instruction
deriving Repr, DecidableEq, BEq, Inhabited

structure Program where
  function : FunctionDef
deriving Repr, DecidableEq, BEq, Inhabited

def UnaryOperator.pretty : UnaryOperator → String
  | .complement => "Complement"
  | .negate     => "Negate"

def Val.pretty : Val → String
  | .constant value => s!"Constant({value})"
  | .var name       => s!"Var(\"{name}\")"

def Instruction.pretty : Instruction → String
  | .ret value => s!"Return({value.pretty})"
  | .unary operator source destination =>
    s!"Unary({operator.pretty}, {source.pretty}, {destination.pretty})"

def FunctionDef.pretty (indent : String) (functionDef : FunctionDef) : String :=
  let innerIndent := indent ++ "    "
  let itemIndent := innerIndent ++ "    "
  let renderedBody :=
    match functionDef.body with
    | [] => "[]"
    | instructions =>
      let items := String.intercalate s!",\n{itemIndent}" (instructions.map Instruction.pretty)
      s!"[\n{itemIndent}{items}\n{innerIndent}]"
  s!"Function(\n\
     {innerIndent}name=\"{functionDef.name}\",\n\
     {innerIndent}body={renderedBody}\n\
     {indent})"

def Program.pretty (program : Program) : String :=
  s!"Program(\n    {program.function.pretty "    "}\n)"

instance : ToString Program := ⟨Program.pretty⟩

end Tacky

namespace TackyGen

-- Thread one counter through the whole program. The dot cannot occur in a C
-- identifier, so generated names cannot collide with user-defined names.
def makeTemporary : StateM Nat String := do
  let next ← get
  set (next + 1)
  return s!"tmp.{next}"

def convertUnaryOperator : UnaryOperator → Tacky.UnaryOperator
  | .complement => .complement
  | .negate     => .negate

-- The returned value names the result; the array contains the instructions
-- needed to compute it. Constants do not emit instructions or allocate names.
def emitExpression (expression : Expression) (instructions : Array Tacky.Instruction) :
    StateM Nat (Tacky.Val × Array Tacky.Instruction) := do
  match expression with
  | .constant value => return (.constant value, instructions)
  | .unary operator inner =>
    let (source, instructions) ← emitExpression inner instructions
    let destination := Tacky.Val.var (← makeTemporary)
    return (destination,
      instructions.push (.unary (convertUnaryOperator operator) source destination))

def genStatement (statement : Statement) : StateM Nat (List Tacky.Instruction) := do
  match statement with
  | .ret expression =>
    let (value, instructions) ← emitExpression expression #[]
    return (instructions.push (.ret value)).toList

def genFunction (functionDef : FunctionDef) : StateM Nat Tacky.FunctionDef := do
  let body ← genStatement functionDef.body
  return { name := functionDef.name, body }

end TackyGen

def generateTacky (ast : Program) : Tacky.Program :=
  { function := (TackyGen.genFunction ast.function).run' 0 }

-- Tests: check the actual IR, since the external stage tests only check exits.
private def tackyOfSource (source : String) : Option Tacky.Program :=
  (lex source >>= parse).toOption.map generateTacky

#guard tackyOfSource "int foo(void) { return 3; }"
        == some { function := { name := "foo", body := [.ret (.constant 3)] } }

#guard tackyOfSource "int main(void) { return ~2; }"
        == some { function := { name := "main", body := [
          .unary .complement (.constant 2) (.var "tmp.0"),
          .ret (.var "tmp.0")] } }

#guard tackyOfSource "int main(void) { return -(~2); }"
        == some { function := { name := "main", body := [
          .unary .complement (.constant 2) (.var "tmp.0"),
          .unary .negate (.var "tmp.0") (.var "tmp.1"),
          .ret (.var "tmp.1")] } }

#guard tackyOfSource "int main(void) { return -~-8; }"
        == some { function := { name := "main", body := [
          .unary .negate (.constant 8) (.var "tmp.0"),
          .unary .complement (.var "tmp.0") (.var "tmp.1"),
          .unary .negate (.var "tmp.1") (.var "tmp.2"),
          .ret (.var "tmp.2")] } }

#guard ((TackyGen.emitExpression (.constant 42) #[]).run 7).run
        == ((.constant 42, #[]), 7)

private def twoFunctions : StateM Nat (Tacky.FunctionDef × Tacky.FunctionDef) := do
  let first ← TackyGen.genFunction { name := "f", body := .ret (.unary .negate (.constant 1)) }
  let second ← TackyGen.genFunction { name := "g", body := .ret (.unary .complement (.constant 2)) }
  return (first, second)

#guard (twoFunctions.run 0).run == ((
        { name := "f", body := [.unary .negate (.constant 1) (.var "tmp.0"), .ret (.var "tmp.0")] },
        { name := "g", body := [.unary .complement (.constant 2) (.var "tmp.1"), .ret (.var "tmp.1")] }), 2)

end CTrue

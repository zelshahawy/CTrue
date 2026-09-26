import CTrue.Parser

namespace CTrue

/-! ### Assembly AST (Listing 1-8)

```
program             = Program(function_definition)
function_definition = Function(identifier name, instruction* instructions)
instruction         = Mov(operand src, operand dst) | Ret
operand             = Imm(int) | Register
```

This mirrors the C AST in `CTrue.Parser`, so the names collide; the assembly
side lives in its own `Asm` namespace.
-/

namespace Asm

inductive Operand where
  | imm (value : Nat)
  | reg
deriving Repr, DecidableEq, BEq, Inhabited

inductive Instruction where
  | mov (source destination : Operand)
  | ret
deriving Repr, DecidableEq, BEq, Inhabited

structure FunctionDef where
  name : String
  instructions : List Instruction
deriving Repr, DecidableEq, BEq, Inhabited

structure Program where
  function : FunctionDef
deriving Repr, DecidableEq, BEq, Inhabited

-- Pretty printing for debugging
def Operand.pretty (operand : Operand) : String :=
  match operand with
  | .imm value => s!"Imm({value})"
  | .reg       => "Register"

def Instruction.pretty (instruction : Instruction) : String :=
  match instruction with
  | .mov source destination => s!"Mov({source.pretty}, {destination.pretty})"
  | .ret                    => "Ret"

def FunctionDef.pretty (indent : String) (functionDef : FunctionDef) : String :=
  let innerIndent := indent ++ "    "
  let itemIndent  := innerIndent ++ "    "
  let renderedInstructions :=
    match functionDef.instructions with
    | [] => "[]"
    | instructions =>
      let items := String.intercalate s!",\n{itemIndent}" (instructions.map Instruction.pretty)
      s!"[\n{itemIndent}{items}\n{innerIndent}]"
  s!"Function(\n\
     {innerIndent}name=\"{functionDef.name}\",\n\
     {innerIndent}instructions={renderedInstructions}\n\
     {indent})"

def Program.pretty (program : Program) : String :=
  s!"Program(\n    {program.function.pretty "    "}\n)"

instance : ToString Program := ⟨Program.pretty⟩

end Asm

/-! ### Assembly generation (Table 1-2)

| AST node                       | Assembly construct             |
| ------------------------------ | ------------------------------ |
| `Program(function_definition)` | `Program(function_definition)` |
| `Function(name, body)`         | `Function(name, instructions)` |
| `Return(exp)`                  | `Mov(exp, Register)`, `Ret`    |
| `Constant(int)`                | `Imm(int)`                     |
-/

def genExpression (expression : Expression) : Asm.Operand :=
  match expression with
  | .constant value => .imm value

def genStatement (statement : Statement) : List Asm.Instruction :=
  match statement with
  | .ret value => [Asm.Instruction.mov (genExpression value) .reg, Asm.Instruction.ret]

def genFunction (functionDef : FunctionDef) : Asm.FunctionDef :=
  { name := functionDef.name, instructions := genStatement functionDef.body }

def codegen (ast : Program) : Asm.Program :=
  { function := genFunction ast.function }

-- Tests
private def assemblyOfSource (source : String) : Option Asm.Program :=
  (lex source >>= parse).toOption.map codegen

private def mainReturns2 : Asm.Program :=
  { function := { name := "main", instructions := [.mov (.imm 2) .reg, .ret] } }

#guard genExpression (.constant 7) == Asm.Operand.imm 7
#guard genStatement (.ret (.constant 0)) == [Asm.Instruction.mov (.imm 0) .reg, .ret]
#guard genFunction { name := "f", body := .ret (.constant 1) }
        == { name := "f", instructions := [.mov (.imm 1) .reg, .ret] }

#guard assemblyOfSource "int main(void) { return 2; }" == some mainReturns2
#guard assemblyOfSource "int main(void) {\n    return 2;\n}\n" == some mainReturns2
#guard assemblyOfSource "int foo(void) { return 1000; }"
        == some { function := { name := "foo", instructions := [.mov (.imm 1000) .reg, .ret] } }

#guard (assemblyOfSource "int main(void) { return 2; }").map (·.function.instructions.length)
        == some 2

#guard (assemblyOfSource "int main(void) { return 2; }").map toString
        == some "Program(\n    Function(\n        name=\"main\",\n        instructions=[\n            Mov(Imm(2), Register),\n            Ret\n        ]\n    )\n)"

end CTrue

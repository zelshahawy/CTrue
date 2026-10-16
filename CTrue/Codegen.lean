import CTrue.Tacky

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

/-! ### Assembly generation

| TACKY node                     | Assembly construct             |
| ------------------------------ | ------------------------------ |
| `Program(function_definition)` | `Program(function_definition)` |
| `Function(name, body)`         | `Function(name, instructions)` |
| `Return(val)`                  | `Mov(val, Register)`, `Ret`    |
| `Constant(int)`                | `Imm(int)`                     |


TACKY generation supports unary operators. Lowering their temporary variables
to assembly will need the remaining Chapter 2 backend passes; for now, assembly
generation reports an error for them.
-/

def genValue (value : Tacky.Val) : Except String Asm.Operand :=
  match value with
  | .constant value => .ok (.imm value)
  | .var name => .error s!"assembly generation for Var(\"{name}\") is not implemented yet"

def genInstruction (instruction : Tacky.Instruction) : Except String (List Asm.Instruction) := do
  match instruction with
  | .ret value =>
    let operand ← genValue value
    return [Asm.Instruction.mov operand .reg, Asm.Instruction.ret]
  | .unary operator _ _ =>
    .error s!"assembly generation for Unary({operator.pretty}, ...) is not implemented yet"

def genFunction (functionDef : Tacky.FunctionDef) : Except String Asm.FunctionDef := do
  let instructions ← functionDef.body.mapM genInstruction
  return { name := functionDef.name, instructions := instructions.flatten }

def codegen (tacky : Tacky.Program) : Except String Asm.Program := do
  let function ← genFunction tacky.function
  return { function }

-- Tests
private def assemblyOfSource (source : String) : Option Asm.Program :=
  (lex source >>= parse >>= fun ast => codegen (generateTacky ast)).toOption

private def codegenErrorOfSource (source : String) : Option String :=
  match lex source >>= parse >>= fun ast => codegen (generateTacky ast) with
  | .error message => some message
  | .ok _          => none

private def mainReturns2 : Asm.Program :=
  { function := { name := "main", instructions := [.mov (.imm 2) .reg, .ret] } }

#guard (genValue (.constant 7)).toOption == some (Asm.Operand.imm 7)
#guard (genInstruction (.ret (.constant 0))).toOption
        == some [Asm.Instruction.mov (.imm 0) .reg, .ret]
#guard (genFunction { name := "f", body := [.ret (.constant 1)] }).toOption
        == some { name := "f", instructions := [.mov (.imm 1) .reg, .ret] }

#guard assemblyOfSource "int main(void) { return 2; }" == some mainReturns2
#guard assemblyOfSource "int main(void) {\n    return 2;\n}\n" == some mainReturns2
#guard assemblyOfSource "int foo(void) { return 1000; }"
        == some { function := { name := "foo", instructions := [.mov (.imm 1000) .reg, .ret] } }

#guard (assemblyOfSource "int main(void) { return 2; }").map (·.function.instructions.length)
        == some 2

#guard (assemblyOfSource "int main(void) { return 2; }").map toString
        == some "Program(\n    Function(\n        name=\"main\",\n        instructions=[\n            Mov(Imm(2), Register),\n            Ret\n        ]\n    )\n)"

#guard codegenErrorOfSource "int main(void) { return -2; }"
        == some "assembly generation for Unary(Negate, ...) is not implemented yet"
#guard codegenErrorOfSource "int main(void) { return ~2; }"
        == some "assembly generation for Unary(Complement, ...) is not implemented yet"

end CTrue

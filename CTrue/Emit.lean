import CTrue.Codegen

namespace CTrue


structure Target where
  -- macOS prefixes every symbol with an underscore; Linux does not.
  labelPrefix : String
  -- Linux wants a note saying the program needs no executable stack.
  emitsStackNote : Bool
deriving Repr, DecidableEq, BEq, Inhabited

def Target.macOS : Target := { labelPrefix := "_", emitsStackNote := false }
def Target.linux : Target := { labelPrefix := "",  emitsStackNote := true }

def hostTarget : Target :=
  if System.Platform.isOSX then .macOS else .linux -- No support for windows folks :(

def emitOperand (operand : Asm.Operand) : String :=
  match operand with
  | .imm value => s!"${value}"
  | .reg       => "%eax"

def emitInstruction (instruction : Asm.Instruction) : String :=
  match instruction with
  | .mov source destination => s!"\tmovl\t{emitOperand source}, {emitOperand destination}"
  | .ret                    => "\tret"

def emitFunction (target : Target) (functionDef : Asm.FunctionDef) : String :=
  let label := target.labelPrefix ++ functionDef.name
  let lines :=
    s!"\t.globl {label}" :: s!"{label}:" :: functionDef.instructions.map emitInstruction
  String.intercalate "\n" lines

def emitProgram (target : Target) (program : Asm.Program) : String :=
  let body := emitFunction target program.function ++ "\n"
  if target.emitsStackNote then
    body ++ "\t.section .note.GNU-stack,\"\",@progbits\n"
  else
    body

-- Tests
private def assemblyFor (target : Target) (source : String) : Option String :=
  ((lex source >>= parse).toOption.map codegen).map (emitProgram target)

#guard emitOperand (.imm 0) == "$0"
#guard emitOperand (.imm 42) == "$42"
#guard emitOperand .reg == "%eax"

#guard emitInstruction .ret == "\tret"
#guard emitInstruction (.mov (.imm 2) .reg) == "\tmovl\t$2, %eax"

#guard assemblyFor .macOS "int main(void) { return 2; }"
        == some "\t.globl _main\n_main:\n\tmovl\t$2, %eax\n\tret\n"

#guard assemblyFor .linux "int main(void) { return 2; }"
        == some "\t.globl main\nmain:\n\tmovl\t$2, %eax\n\tret\n\t.section .note.GNU-stack,\"\",@progbits\n"

#guard assemblyFor .macOS "int foo(void) { return 0; }"
        == some "\t.globl _foo\n_foo:\n\tmovl\t$0, %eax\n\tret\n"

#guard (assemblyFor .macOS "int main(void) { return 2; }").map
          (fun text => (text.splitOn "\n").filter (fun line =>
             !line.isEmpty && !line.startsWith "\t"))
        == some ["_main:"]

end CTrue

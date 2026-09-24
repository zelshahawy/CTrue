import CTrue.Lexer

open CTrue

/-!
# Compiler driver

The driver the book asks for (Sandler, *Writing a C Compiler*, ch. 1):

1. preprocess `FILE.c` into `FILE.i` with `gcc -E -P`
2. compile `FILE.i`, deleting it afterwards
3. assemble and link `FILE.s` into an executable with `gcc`, deleting the `.s`

It takes exactly one input file, and exits with a nonzero status if any stage
fails. The stage flags stop the pipeline early and must not write output files.
-/

/-- Where to stop. `full` runs the whole pipeline through assembling and linking. -/
inductive Stage where
  /-- Stop early and emit no files. The test suite passes these as `--<stage>`. -/
  | lex | parse | validate | tacky | codegen
  /-- `-S`: emit `FILE.s` and stop. -/
  | assembly
  /-- `-c`: emit `FILE.o` and stop. Used by the multi-file library tests, where
      gcc compiles the other translation units and does the linking. -/
  | object
  /-- Emit a linked executable. -/
  | full
deriving Repr, DecidableEq, Inhabited

/-- Does this run stop before producing any files? -/
def Stage.isDryRun : Stage → Bool
  | .lex | .parse | .validate | .tacky | .codegen => true
  | .assembly | .object | .full => false

structure Options where
  input : System.FilePath := ""
  stage : Stage := .full
deriving Inhabited

def usage : String :=
  "usage: ctrue [--lex|--parse|--validate|--tacky|--codegen|-S|-c] FILE.c"

def parseArgs (args : List String) : Except String Options := do
  let mut opts : Options := {}
  let mut seenInput := false
  for arg in args do
    match arg with
    | "--lex"      => opts := { opts with stage := .lex }
    | "--parse"    => opts := { opts with stage := .parse }
    | "--validate" => opts := { opts with stage := .validate }
    | "--tacky"    => opts := { opts with stage := .tacky }
    | "--codegen"  => opts := { opts with stage := .codegen }
    | "-S"         => opts := { opts with stage := .assembly }
    | "-c"         => opts := { opts with stage := .object }
    | _ =>
      if arg.startsWith "-" then
        throw s!"unknown flag: {arg}"
      else if seenInput then
        throw "expected exactly one input file"
      else
        opts := { opts with input := arg }
        seenInput := true
  if !seenInput then throw "no input file"
  if opts.input.extension != some "c" then throw s!"input must be a .c file: {opts.input}"
  return opts

/-- Run a subcommand, returning an error message if it fails. -/
def runCmd (cmd : String) (args : Array String) : IO (Except String Unit) := do
  let out ← IO.Process.output { cmd, args }
  if out.exitCode == 0 then
    return .ok ()
  else
    return .error s!"{cmd} failed with exit code {out.exitCode}\n{out.stderr}"

def cli (args : List String) : IO UInt32 := do
  match parseArgs args with
  | .error msg =>
      IO.eprintln s!"ctrue: {msg}"
      IO.eprintln usage
      return 1
  | .ok opts =>
    let input := opts.input
    let preprocessed := input.withExtension "i"
    -- Step 1: preprocess.
    match ← runCmd "gcc" #["-E", "-P", input.toString, "-o", preprocessed.toString] with
    | .error msg => IO.eprintln s!"ctrue: {msg}"; return 1
    | .ok () => pure ()
    let src ← IO.FS.readFile preprocessed
    IO.FS.removeFile preprocessed
    -- Step 2: compile. Only the lexer exists so far.
    match lex src with
    | .error msg =>
        IO.eprintln s!"{input}:{msg}"
        return 1
    | .ok tokens =>
      if opts.stage == .lex then
        return 0
      IO.eprintln s!"ctrue: {input}: lexed {tokens.length} tokens, \
                    but stage {repr opts.stage} is not implemented yet"
      return 1

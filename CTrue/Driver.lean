import CTrue.Lexer
import CTrue.Parser
import CTrue.Codegen
import CTrue.Emit

open CTrue

inductive Stage where
  | lex | parse | validate | tacky | codegen
  | assembly
  | object
  | full
deriving Repr, DecidableEq, Inhabited

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

def hostArchArgs : Array String :=
  if System.Platform.isOSX && System.Platform.target.startsWith "arm64" then
    #["-arch", "x86_64"] -- my best effort to make this work in non x86 machines.
  else
    #[]

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

    match ← runCmd "gcc" #["-E", "-P", input.toString, "-o", preprocessed.toString] with
    | .error msg => IO.eprintln s!"ctrue: {msg}"; return 1
    | .ok () => pure ()
    let src ← IO.FS.readFile preprocessed
    IO.FS.removeFile preprocessed

    match lex src with
    | .error msg =>
        IO.eprintln s!"{input}:{msg}"
        return 1
    | .ok tokens =>
      IO.FS.writeFile "lexer_result.txt" tokens.toString
      if opts.stage == .lex then
        return 0

      match parse tokens with
      | .error msg =>
          IO.eprintln s!"{input}: {msg}"
          return 1
      | .ok ast =>
        IO.FS.writeFile "parser_result.txt" (toString ast)
        if opts.stage == .parse then
          return 0

        let asm := codegen ast
        IO.FS.writeFile "codegen_result.txt" (toString asm)
        if opts.stage == .codegen then
          return 0

        let assemblyPath := input.withExtension "s"
        IO.FS.writeFile assemblyPath (emitProgram hostTarget asm)
        if opts.stage == .assembly then
          return 0

        let outputPath :=
          if opts.stage == .object then input.withExtension "o" else input.withExtension ""
        let compileOnly := if opts.stage == .object then #["-c"] else #[]
        let result ← runCmd "gcc"
          (hostArchArgs ++ compileOnly ++ #[assemblyPath.toString, "-o", outputPath.toString])
        IO.FS.removeFile assemblyPath
        match result with
        | .error msg => IO.eprintln s!"ctrue: {msg}"; return 1
        | .ok () => return 0

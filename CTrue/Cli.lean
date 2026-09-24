inductive Stage where
  | lex
  | parse
  | codegen
  | assembly
  | full
deriving Repr, DecidableEq

structure Options where
  inputs : List System.FilePath := []
  output : Option System.FilePath := none
  stage  : Stage := .full

def usage : String :=
  "usage: ctrue [--lex|--parse|--codegen|-S] [-o OUT] FILE.c..."

def parseArgs : List String → Options → Except String Options
  | [], opts =>
      if opts.inputs.isEmpty then
        .error "no input file"
      else if opts.output.isSome && opts.inputs.length > 1 then
        .error "-o cannot be used with multiple input files"
      else
        .ok { opts with inputs := opts.inputs.reverse }
  | "--lex"     :: rest, o => parseArgs rest { o with stage := .lex }
  | "--codegen" :: rest, o => parseArgs rest { o with stage := .codegen }
  | "-S"        :: rest, o => parseArgs rest { o with stage := .assembly }
  | "-o" :: path :: rest, o => parseArgs rest { o with output := some path }
  | ["-o"], _ => .error "-o requires an argument"
  | arg :: rest, o =>
      if arg.startsWith "-" then
        .error s!"unknown flag: {arg}"
      else
        parseArgs rest { o with inputs := arg :: o.inputs }

def cli (args : List String) : IO UInt32 := do
  match parseArgs args {} with
  | .error msg =>
      IO.eprintln s!"ctrue: {msg}"
      IO.eprintln usage
      return 1
  | .ok opts =>
      let mut failed := false
      for input in opts.inputs do
        try
          let src ← IO.FS.readFile input
          IO.println s!"compiling {input}, stage {repr opts.stage}"
        catch e =>
          IO.eprintln s!"ctrue: cannot open {input}: {e}"
          failed := true
      return if failed then 1 else 0

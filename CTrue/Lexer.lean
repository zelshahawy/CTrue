namespace CTrue

inductive Token where
  | kwInt
  | kwVoid
  | kwReturn
  | ident (name : String)
  | const (value : Nat)
  | lparen
  | rparen
  | lbrace
  | rbrace
  | semi
deriving Repr, DecidableEq, BEq, Inhabited

namespace Token

def toString : Token → String
  | kwInt     => "int"
  | kwVoid    => "void"
  | kwReturn  => "return"
  | ident n   => s!"ident({n})"
  | const v   => s!"constant({v})"
  | lparen    => "("
  | rparen    => ")"
  | lbrace    => "{"
  | rbrace    => "}"
  | semi      => ";"

instance : ToString Token := ⟨toString⟩

end Token

def isIdentStart (c : Char) : Bool := c.isAlpha || c == '_'

def isIdentCont (c : Char) : Bool := c.isAlphanum || c == '_'

def classify (text : String) : Token :=
  match text with
  | "int"    => .kwInt
  | "void"   => .kwVoid
  | "return" => .kwReturn
  | _        => .ident text

def digitsToNat (ds : List Char) : Nat :=
  ds.foldl (fun acc d => acc * 10 + (d.toNat - '0'.toNat)) 0

partial def tokenizeAux (line col : Nat) : (input : List Char) → Except String (List Token) -- Will skip the proof of termination for now
  | [] => .ok []
  | c :: cs =>
    if c == '\n' then
      tokenizeAux (line + 1) 1 cs
    else if c.isWhitespace then
      tokenizeAux line (col + 1) cs
    else if isIdentStart c then
      let text := String.ofList (c :: cs.takeWhile isIdentCont)
      let rest := cs.dropWhile isIdentCont
      (tokenizeAux line (col + text.length) rest).map (classify text :: ·)
    else if c.isDigit then
      let digits := c :: cs.takeWhile Char.isDigit
      let rest := cs.dropWhile Char.isDigit
      match rest with
      | r :: _ =>
        if isIdentCont r then
          .error s!"{line}:{col}: invalid token: constant '{String.ofList digits}' \
                    is followed by '{r}'"
        else
          (tokenizeAux line (col + digits.length) rest).map (.const (digitsToNat digits) :: ·)
      | [] => .ok [.const (digitsToNat digits)]
    else
      let tok? : Option Token :=
        match c with
        | '(' => some .lparen
        | ')' => some .rparen
        | '{' => some .lbrace
        | '}' => some .rbrace
        | ';' => some .semi
        | _   => none
      match tok? with
      | none => .error s!"{line}:{col}: unexpected character '{c}'"
      | some t => (tokenizeAux line (col + 1) cs).map (t :: ·)


def lex (source : String) : Except String (List Token) :=
  tokenizeAux 1 1 source.toList

-- Tests

private def toks (source : String) : Option (List Token) :=
  (lex source).toOption

private def errOf : Except String (List Token) -> Option String
  | .error e => some e
  | .ok _    => none

#guard toks "int main(void) { return 2; }"
        == some [.kwInt, .ident "main", .lparen, .kwVoid, .rparen,
                 .lbrace, .kwReturn, .const 2, .semi, .rbrace]

#guard toks "int void return" == some [.kwInt, .kwVoid, .kwReturn]
#guard toks "integer voided returns"
        == some [.ident "integer", .ident "voided", .ident "returns"]
#guard toks "_foo bar1 int2"
        == some [.ident "_foo", .ident "bar1", .ident "int2"]

#guard toks "0 7 42 1000" == some [.const 0, .const 7, .const 42, .const 1000]

#guard toks "(){};" == some [.lparen, .rparen, .lbrace, .rbrace, .semi]

#guard toks "" == some []
#guard toks "   \n\t " == some []

#guard toks "int\nmain" == some [.kwInt, .ident "main"]

#guard errOf (lex "123abc")
        == some "1:1: invalid token: constant '123' is followed by 'a'"
#guard errOf (lex "return 1foo;")
        == some "1:8: invalid token: constant '1' is followed by 'f'"

#guard errOf (lex "int main(void) { return 1@; }")
        == some "1:26: unexpected character '@'"
#guard errOf (lex "int main(void) {\n  return @;\n}")
        == some "2:10: unexpected character '@'"

end CTrue

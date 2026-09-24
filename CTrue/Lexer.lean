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

end CTrue

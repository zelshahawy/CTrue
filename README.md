# CTrue

I have been interested in compilers, and after realizing that UChicago is not offering a
compilers course in my senior year, I decided to learn it on my own.

I am mainly following Nora Sandler's *Writing a C Compiler*.

## Why did I choose Lean

Lean is a functional language, and from writing [Pyleft](https://github.com/zelshahawy/pyleft), I know that functional languages usually
have the upper hand in writing expressions, evaluating the AST of the program and so on. I also wanted to get more experience with Lean, which may become
my main functional language instead of Haskell.

There is also the rather ambitious extension where I use Lean not just to
build the compiler but to reason about the programs it compiles.
Because Lean is a proof assistant as well as a programming
language, the same file that defines what a C program means
can also host claims about a particular C program: that this sorting routine
returns a permutation of its input, that this parser never
reads outside its buffer, that this loop terminates.

For now, the license is BSD-3-Clause license.

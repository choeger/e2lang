(*
 * Copyright (c) 2014, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

open SmartPrint
open E2lang

let arg2doc = function
    BArg n -> !^ "%b_" ^-^ !^ (string_of_int n)
  | FArg n -> !^ "%f_" ^-^ !^ (string_of_int n)
  | IArg n -> !^ "%i_" ^-^ !^ (string_of_int n)
  | DArg n -> !^ "%d_" ^-^ !^ (string_of_int n)

let f2doc = function
    FloatLit f -> !^ (string_of_float f)
  | FloatVar v -> arg2doc (FArg v)

let i2doc = function
    IntLit n -> !^ (string_of_int n)
  | IntVar v -> arg2doc (IArg v)

let b2doc = function
    BoolLit true -> !^ "true"
  | BoolLit false -> !^ "false"
  | BoolVar v -> arg2doc (BArg v)


let instr2doc = function    
    DMul(a,b) -> arg2doc (DArg a) ^^ !^ "~*" ^^ (arg2doc (DArg b))
  | DAdd(a,b) -> arg2doc (DArg a) ^^ !^ "~+" ^^ (arg2doc (DArg b))
  | DLoadF(f) ->  f2doc f
  | DCopy(a) -> arg2doc (DArg a)
  | DPwr(n, a) -> arg2doc (DArg a) ^^ !^ "~^" ^^ (i2doc n)

  | FAdd (a, b) -> (f2doc a) ^^ !^ ".+" ^^ (f2doc b)
  | FMul (a, b) -> (f2doc a) ^^ !^ ".*" ^^ (f2doc b)
  | FCopyI (a) -> (i2doc a)
  | FCopy  (a) -> (f2doc a)

  | IAdd (a, b) -> (i2doc a) ^^ !^ "+" ^^ (i2doc b)
  | IMul (a, b) -> (i2doc a) ^^ !^ "*" ^^ (i2doc b)
  | IEquals (a, b) -> (i2doc a) ^^ !^ "==" ^^ (i2doc b)
  | ICopy (a) -> i2doc a
  | ICopyB (b) -> b2doc b

  | BAnd(a, b) -> (b2doc a) ^^ !^ "or" ^^ (b2doc b)
  | BOr(a, b) -> (b2doc a) ^^ !^ "and" ^^ (b2doc b)
  | BNot(a) -> !^ "not" ^^ (b2doc a)
  | BCopy(a) -> b2doc a

  | Call(s, args) -> !^ s ^^ (parens (separate (!^ ",") (List.map arg2doc (Array.to_list args))))

let stmt2doc = function    
    Debug -> !^ "DEBUG" ^^ !^ ";"
  | Ret v -> !^ "RETURN" ^^ (arg2doc v) ^^ !^ ";"
  | Jmp s -> !^ "GOTO" ^^ !^ s ^^ !^ ";"
  | CJmp (s,x) -> !^ "IF" ^^ (arg2doc (BArg x)) ^^ !^ "GOTO" ^^ !^ s ^^ !^ ";"
  | Store(arg, instr) -> (arg2doc arg) ^^ !^ ":=" ^^ (instr2doc instr) ^^ !^ ";"
  | Label s -> !^ s ^^ !^ ":" 

let prog2doc stmts = (separate newline (List.map stmt2doc stmts)) ^^ newline


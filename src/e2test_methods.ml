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
open E2link
open E2lang
open E2intpr
open E2poly

let test_proto = { ivars = 4; bvars = 1; dvars =0; fvars=0; args=[| |]; ret=IRet }

let meta = { order = 0; params = 0 }

let empty_procs = StrMap.empty

(** A very simple test-loop *)
let test_program = [|
    Store( IArg 0, ICopy(IntLit 10)  ) ;  
    Store( IArg 1, ICopy(IntLit  0)  ) ;
    Store( IArg 2, ICopy(IntLit  1)  ) ;
    Store( IArg 3, ICopy(IntLit(-1))  ) ;
    Label("Loop") ;
    Store( IArg 1, IAdd( (IntVar 1), (IntVar 2) ) ) ; (* x1 := x1 + 1 *)
    Store( IArg 0, IAdd( (IntVar 0), (IntVar 3) ) ) ; (* x0 := x0 - 1 *)
    Store( BArg 0, IEquals( (IntVar 0), (IntLit 0) )  ) ;
    Store( BArg 0, BNot (BoolVar 0) ) ;
    CJmp( "Loop", 0) ;
    Ret (IArg 1) 
   |]

(** The obvious example: The faculty function *)
let fac_proto = { ivars = 3; bvars = 2; dvars = 0; fvars = 0; args = [| IArg 0 |]; ret=IRet }

let test_fac = [|
    Store( IArg 1, ICopy(IntLit 1) ) ;
    Store( BArg 0, IEquals ( (IntVar 0), IntLit 0 ) ) ; (* x0 = 0 ? *)
    Store( BArg 1, IEquals ( (IntVar 0), IntLit 1 ) ) ; (* x0 = 1 ? *)
    Store( BArg 0, BOr( (BoolVar 0), (BoolVar 1) ) ) ;
    CJmp ( "Ret", 0 ) ;

    Label("Loop") ;
    Store( IArg 1, IMul ( (IntVar 1), (IntVar   0 ) ) ) ;
    Store( IArg 0, IAdd ( (IntVar 0), (IntLit (-1)) ) ) ;

    Store( BArg 0, IEquals ( (IntVar 0), IntLit 1 ) ) ; 
    Store( BArg 0, BNot (BoolVar 0) ) ; (* x0 |= 1 ? *)
    CJmp( "Loop", 0 );
    
    Label ("Ret") ;
    Ret (  IArg 1)
  |]

(** Some Polynomials *)
let test_polynomials = [
  Variable(0, 2, Number(1.0), []) ; (* x² *)
  Variable(0, 2, Number(1.0), [Variable(1, 2, Number(1.0), [])] ) ; (* x² + y² *)  
]

let test_proc = Proc(test_proto, test_program)

let faculty_proc = Proc(fac_proto, test_fac)

let procedures = (StrMap.add "test" test_proc
			     (StrMap.add "fac" faculty_proc empty_procs))


(* call the linker *)
let test_methods = link procedures

(* get executable methods *)
let test_method = StrMap.find "test" test_methods

let fac_method = StrMap.find "fac" test_methods
		
(* evaluate them *)
let eval_test () = eval_method meta test_method [||]

let eval_fac n = eval_method meta fac_method [| IVal n |]

(* for comparison: faculty function in haskell *)
let rec fac = function
    (0|1) -> 1
  | n -> n * fac(n-1)


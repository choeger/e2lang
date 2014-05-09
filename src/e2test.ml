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
open E2test_methods
open E2lang
open Kaputt.Abbreviations

(** Output provider for test results (to see the actual result if something went wrong *)
let print_val = function
  IVal n -> string_of_int n 
  | FVal f -> string_of_float f
  | BVal true -> "true"
  | BVal false -> "false"
  | Void -> "void"
  | _ -> "other value"

let faculty_identity (a, IVal(b)) = (fac a) = b

let () =
  Printf.printf("Running...\n") ;
  (* simple tests are provided using this syntax: *)
  Test.add_simple_test
    ~title:"counting loop" (* test name *)
    
    (fun () -> (* actual test evaluation *)
     (* assert equality *)
     Assert.equal ~prn:print_val (IVal 10) (eval_test ()) ) ;
  
  (* and so on ... *)
  Test.add_simple_test
    ~title:"fac(0)"
    (fun () -> Assert.equal ~prn:print_val (IVal 1) (eval_fac 0) );
  Test.add_simple_test
    ~title:"fac(1)"
    (fun () -> Assert.equal ~prn:print_val (IVal 1) (eval_fac 1));
  Test.add_simple_test
    ~title:"fac(2)"
    (fun () -> Assert.equal ~prn:print_val (IVal 2) (eval_fac 2));
  
  (* test a whole specification by enumerating over several inputs *)
  Test.add_enum_test
    ~title:"faculty interpreted" (* title again *)
    (Enum.int 1 20)  (* inputs *)
    (eval_fac)       (* test-method *)
    [Spec.always => faculty_identity]  (* specification as logical implication *);

  (* finally run our tests *)
  Test.launch_tests ()



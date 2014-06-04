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
open E2poly
open Kaputt.Abbreviations
open E2jit
open E2intpr

external twoInts : int -> int -> unit = "ints"

(** Output provider for test results (to see the actual result if something went wrong *)
let print_val = function
  IVal n -> string_of_int n 
  | FVal f -> string_of_float f
  | BVal true -> "true"
  | BVal false -> "false"
  | Void -> "void"
  | _ -> "other value"

let faculty_identity (a, IVal(b)) = (fac a) = b

let poly1_id (a, DVal(b)) = ((eval_poly [|a|] poly1) = b.(0)) && ((2. *. a) = b.(1))
let poly2_id ((a1, a2), FVal(b)) = (eval_poly [|a1; a2|] poly2) = b
let poly3_id ((a1, a2, a3), FVal(b)) = (eval_poly [|a1; a2; a3|] poly3) = b
let poly4_id ((a1, a2), FVal(b)) = (eval_poly [|a1; a2|] poly4) = b

let () =
  (*  twoInts 1 2;
    test_tnp_constant () ;
    build_test "fac" (Proc (fac_proto, test_fac));*)
    build_module (Proc (fac_proto, test_fac));
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

  (*Test.add_simple_test
    ~title:"poly1(0.1)"
    (fun () -> Assert.equal ~prn:print_val (FVal (eval_poly [|0.1|] (List.nth test_polynomials 0))) (eval_poly1 0.1));*)
  
  (* test a whole specification by enumerating over several inputs *)
  Test.add_enum_test
    ~title:"faculty interpreted" (* title again *)
    (Enum.int 1 20)  (* inputs *)
    (eval_fac)       (* test-method *)
    [Spec.always => faculty_identity]  (* specification as logical implication *);
  
 Test.add_enum_test
    ~title:"poly1"
    (Enum.float (-1.0) (1.0) 50)
    (eval_poly1)
    [Spec.always => poly1_id];

  (*Test.add_enum_test
    ~title:"poly2"
    (Enum.zip2 (Enum.float (-1.0) (1.0) 50) (Enum.float (-1.0) (1.0) 50))
    (fun (a, b) -> eval_poly2 a b)
    [Spec.always => poly2_id];

  Test.add_enum_test
    ~title:"poly3"
    (Enum.zip3 (Enum.float (-1.0) (1.0) 50) (Enum.float (-1.0) (1.0) 50) (Enum.float (-1.0) (1.0) 50))
    (fun (a, b, c) -> eval_poly3 a b c)
    [Spec.always => poly3_id];

  Test.add_enum_test
    ~title:"poly4"
    (Enum.zip2 (Enum.float (-1.0) (1.0) 50) (Enum.float (-1.0) (1.0) 50))
    (fun (a, b) -> eval_poly4 a b)
    [Spec.always => poly4_id]; *)

  (* finally run our tests *)
  Test.launch_tests ()



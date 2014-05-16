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
open E2lang

open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

(**
 OCaml interface to the thin C-layer for method invocation
 *)
type method_ptr

external pointer_to_method : ExecutionEngine.t -> Llvm.llvalue -> method_ptr = "LLVMRecompileAndRelinkFunction"

external eval_method_int : method_ptr -> int = "eval_method_int"

external eval__i_i : method_ptr -> int -> int = "eval__i_i"

external eval_method_bool : method_ptr -> bool = "eval_method_bool"

external eval_method_float : method_ptr -> float = "eval_method_float"

external eval_method_obj : method_ptr -> 'a = "eval_method_obj"


type jit_compiler = {
  jit_context : llcontext ;
  builder : llbuilder ;

  double_type : lltype ;
  int_type : lltype ;
  bool_type : lltype ;

  the_module : llmodule ;

  (* Create the JIT. *)
  the_execution_engine : ExecutionEngine.t ;
  the_fpm : [ `Function ] PassManager.t ;
}

(**
 JIT-compiler using llvm
 *) 
let optimizing_jit_compiler = 
  let jit_context = global_context () in

  let builder = builder jit_context in
  let double_type = double_type jit_context in
  let int_type = i64_type jit_context in
  let bool_type = i1_type jit_context in

  let the_module = create_module jit_context "e2lang procedures" in

  (* Create the JIT. *)
  let the_execution_engine = (ignore(initialize_native_target ()) ; ExecutionEngine.create_jit the_module 3) in

  let the_fpm = (
    let fpm = PassManager.create_function the_module in
    (* Set up the optimizer pipeline.  Start with registering info about how the
     * target lays out data structures. *)
    DataLayout.add_to_pass_manager fpm (ExecutionEngine.data_layout the_execution_engine);

    (* Promote allocas to registers. *)
    add_memory_to_register_promotion fpm;

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    add_instruction_combination fpm;

    (* reassociate expressions. *)
    add_reassociation fpm;

    (* Eliminate Common SubExpressions. *)
    add_gvn fpm;

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    add_cfg_simplification fpm;
    
    ignore (PassManager.initialize fpm);
    fpm ) in
  { jit_context ; builder ; double_type ; int_type ; 
    bool_type ; the_module ; the_execution_engine ;
    the_fpm ;
  }
      
(**
  Every local variable of a e2l procedure needs to be allocated on the stack.
  See http://llvm.org/docs/tutorial/LangImpl7.html
 *)
type var_table = {
  local_int_vars : llvalue array ;
  local_float_vars : llvalue array ;
  local_bool_vars : llvalue array ;
}

let build_local_vars proto jit = 
  let make_ivar i = build_alloca jit.int_type (Printf.sprintf "int%d" i) jit.builder in
  let make_fvar i = build_alloca jit.double_type (Printf.sprintf "flt%d" i) jit.builder in
  let make_bvar i = build_alloca jit.bool_type (Printf.sprintf "bool%d" i) jit.builder in

   { local_int_vars = Array.init proto.ivars make_ivar ; 
     local_bool_vars = Array.init proto.bvars make_bvar; 
     local_float_vars = Array.init proto.fvars make_fvar }

let llvm_type jit = function
    IArg _ -> jit.int_type
  | FArg _ -> jit.double_type
  | BArg _ -> jit.bool_type

let llvm_rettype jit = function
    IRet -> jit.int_type
  | FRet -> jit.double_type
  | BRet -> jit.bool_type


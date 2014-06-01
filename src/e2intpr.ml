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

open E2link

(* Interpreter state *)
type state = {
  der_vars : der_val array ;
  int_vars : int array ; 
  float_vars : float array ;
  bool_vars : bool array ;
  procedures : proc_map ;
  locations : loc_map;
}

(* load the value of an argument (call-site resolution) *)
let load_arg state arg = match arg with
    (* TODO: catch unbound variable error *)
    IArg i -> IVal state.int_vars.(i)
  | FArg i -> FVal state.float_vars.(i)
  | DArg i -> DVal state.der_vars.(i)
  | BArg i -> BVal state.bool_vars.(i)

(* Pretty print a state *)
let string_from_state state =
    let sis = String.concat ", " (Array.to_list (Array.map string_of_int state.int_vars)) in
    let sfs = String.concat ", " (Array.to_list (Array.map string_of_float state.float_vars)) in
    let sbs = String.concat ", " (Array.to_list (Array.map string_of_bool state.bool_vars)) in

    Printf.sprintf("is=[%s] ; fs=[%s] ; bs=[%s]") sis sfs sbs
		  		  
(* store updates *)
let store_int state i = function
    IVal n -> Array.set state.int_vars i n 
   | _ -> raise (Failure "type error. Expected int")

let store_bool state i = function
    BVal b -> Array.set state.bool_vars i b 
  | _ -> raise (Failure "type error. Expected bool")

let store_float state i = function
    FVal f -> Array.set state.float_vars i f
  | _ -> raise (Failure "type error. Expected float")

let store_der state i = function
    DVal array -> Array.blit array 0 state.der_vars.(i) 0 (Array.length array)
  | _ -> raise (Failure "type error. Expected np-number")

let store_arg state v = function
    IArg j -> store_int state j v
  | FArg j -> store_float state j v
  | BArg j -> store_bool state j v
  | DArg j -> store_der state j v 

let store_arg_i state proto_args i v = store_arg state v proto_args.(i)

(* value readers *)
let bool state = function BoolVar i -> state.bool_vars.(i)
			| BoolLit b -> b

let int state = function IntVar i -> state.int_vars.(i)
		       | IntLit i -> i

let float state = function FloatVar i -> state.float_vars.(i)
			 | FloatLit f -> f

let proc state f = StrMap.find f state.procedures

let location state s = StrMap.find s state.locations  

(** automatic differentiation *)
external tnp_add : int -> int -> der_val -> der_val -> der_val -> unit = "op_tnp_number_add"
external tnp_mult : int -> int -> der_val -> der_val -> der_val -> unit = "op_tnp_number_mult"
external tnp_pow : int -> int -> der_val -> der_val -> int -> unit = "op_tnp_number_pow"
external tnp_constant : int -> int -> der_val -> float -> unit = "op_tnp_number_write_constant"
external tnp_variable : int -> int -> der_val -> float -> int -> unit = "op_tnp_number_write_variable"

let rec eval_method meta proc args =   
  let proto = proc.p_prototype in
  
  (* allocate registers *)
  let fs = Array.make proto.fvars 0.0 in
  let is = Array.make proto.ivars 0 in
  let bs = Array.make proto.bvars false in
  
  (* der-variables have a size depending on the meta-lifting *)
  let new_der_var params order = Array.make ((params+1) * (order+1)) 0.0 in

  let ds = Array.init proto.dvars (fun _ -> new_der_var meta.params meta.order) in

  let state = { der_vars = ds ; int_vars = is ; 
		float_vars = fs ; bool_vars = bs ;
		procedures = Lazy.force proc.p_procedures ; 
		locations = proc.p_locations } in  

  (* load arguments *)
  let _ = Array.mapi (store_arg_i state proto.args) args in

  (* start instruction evaluation *)
  eval meta state proc.p_statements 0


and eval meta state stmts i = match stmts.(i) with
    (* Debug *)
    Debug -> Printf.printf("Instr: %d State: %s\n") i (string_from_state state) ; eval meta state stmts (i+1)
										       
  (* return *)
  | Ret v -> load_arg state v

  (* unconditional jump *)
  | Jmp s -> let j = location state s in
	     eval meta state stmts j

  (* conditional jump *)
  | CJmp (s,x) -> if bool state (BoolVar x) then (
		    let j = location state s in
		    eval meta state stmts j)
		  else (
		    eval meta state stmts (i+1)
		  )
			 
  (* Exec of tnp-instructions *)
  | Store((DArg var), DAdd (x, y)) -> tnp_add meta.params meta.order state.der_vars.(var) state.der_vars.(x) state.der_vars.(y) ; eval meta state stmts (i+1)
  | Store((DArg var), DMul (x, y)) -> tnp_mult meta.params meta.order state.der_vars.(var) state.der_vars.(x) state.der_vars.(y) ; eval meta state stmts (i+1)
  | Store((DArg var), DPwr (n, x)) -> tnp_pow meta.params meta.order state.der_vars.(var) state.der_vars.(x) (int state n) ; eval meta state stmts (i+1)
  | Store((DArg var), DLoadF (f)) -> tnp_constant meta.params meta.order state.der_vars.(var) (float state f) ; eval meta state stmts (i+1)
  | Store((DArg var), DCopy (d)) -> Array.blit state.der_vars.(d) 0 state.der_vars.(var) 0 ((meta.params + 1) * (meta.order + 1)) ; eval meta state stmts (i+1)
  

  (* Exec of float instructions *)
  | Store((FArg var), FAdd (x, y)) -> state.float_vars.(var) <- ((float state x) +. (float state y)) ; eval meta state stmts (i+1)
  | Store((FArg var), FMul (x, y)) -> state.float_vars.(var) <- ((float state x) *. (float state y)) ; eval meta state stmts (i+1)
  | Store((FArg var), FCopy f)     -> state.float_vars.(var) <- (float state f) ; eval meta state stmts (i+1)
  | Store((FArg var), FCopyI n)    -> state.float_vars.(var) <- float_of_int (int state n) ; eval meta state stmts (i+1)

  (* Exec of int instructions *)
  | Store((BArg var), IEquals (x, y)) -> state.bool_vars.(var) <- ((int state x) = (int state y)) ; eval meta state stmts (i+1)
  | Store((IArg var), ICopy x)        -> state.int_vars.(var) <- (int state x) ; eval meta state stmts (i+1)
  | Store((IArg var), IAdd (x, y))    -> state.int_vars.(var) <- ((int state x) + (int state y)) ; eval meta state stmts (i+1)
  | Store((IArg var), IMul (x, y))    -> state.int_vars.(var) <- ((int state x) * (int state y)) ; eval meta state stmts (i+1)
  | Store((IArg var), ICopyB b)       -> state.int_vars.(var) <- (match (bool state b) with true -> 1 | false -> 0) ; eval meta state stmts (i+1)

  (* Exec of boolean instructions *)			 
  | Store((BArg var), BAnd (x, y)) ->  state.bool_vars.(var) <- ((bool state x) && (bool state y)) ; eval meta state stmts (i+1)
  | Store((BArg var), BOr (x, y))  -> state.bool_vars.(var) <- ((bool state x) || (bool state y))  ; eval meta state stmts (i+1)	       
  | Store((BArg var), BNot x)      -> state.bool_vars.(var) <- (not(bool state x)) ; eval meta state stmts (i+1)
  | Store((BArg var), BCopy b)     -> state.bool_vars.(var) <-  (bool state b) ; eval meta state stmts (i+1)

  (* Procedure calls *)
  | Store(arg, Call (f, args)) -> store_arg state (eval_method meta (proc state f) (Array.map (load_arg state) args)) arg ; eval meta state stmts (i+1)

  | Store (FArg(_),_) -> raise (Failure "expected a float expression")
  | Store (IArg(_),_) -> raise (Failure "expected a int expression")
  | Store (DArg(_),_) -> raise (Failure "expected a tnp expression")
  | Store (BArg(_),_) -> raise (Failure "expected a bool expression")

  (* Jump over labels *)
  | Label _ -> eval meta state stmts (i+1)
				     

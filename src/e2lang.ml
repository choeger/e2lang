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

(**
  e2lang is an implementation of a 3-address like language for embedded equations.
  The language is intended to support automatic differentiation as described in 
  "Operational Semantics for a Modular Equation Language"

  @author Christoph Höger <christoph.hoeger@tu-berlin.de>  
 *)

(** Meta-evaluation context (lifting for AD) *)
type meta = {
  params : int; (** number of parameters for partial derivation *)
  order : int;  (** level of total derivation *)
}

(** derivable variables are just plain arrays *)
type der_val = float array

(** all other variables are offsets in state array *)
type ivar = int
 and bvar = int
 and dvar = int
 and fvar = int

(** int atoms are either literals or variables *)
type iatom = IntVar of ivar
	   | IntLit of int

(** bool atoms, either literals or variables *)
type batom = BoolVar of bvar
	   | BoolLit of bool

(** float atoms, either literals or variables *)
type fatom = FloatVar of fvar
	   | FloatLit of float

(** generic wrapper for arguments of different type *)
type arg = IArg of ivar
	 | BArg of bvar
	 | FArg of fvar
	 | DArg of dvar

(** return type indicators *)
type rettype = IRet | BRet | FRet | DRet | ORet

(** e2lang rhs expressions *)				  
type expr = DMul of dvar * dvar      (** derivable multiplication *)
	  | DAdd of dvar * dvar      (** derivable addition *)
	  | DPwr of iatom * dvar     (** derivable exponentiation *)
	  (* | DPrim of string * dvar   (** primitive functions *) *)
	  | DLoadF of fatom          (** float to derivable *)
	  | DCopy of dvar            (** copying *)

	  | FAdd of fatom * fatom    (** floating point addition *)
	  | FMul of fatom * fatom    (** floating point multiplication *)
	  | FCopyI of iatom          (** int to float conversion *)
	  | FCopy of fatom           (** copying *)

	  | IAdd of iatom * iatom      (** int addition *)
	  | IEquals of iatom * iatom   (** int equality *)
	  | IMul of iatom * iatom      (** int multiplication *)
	  | ICopy of iatom             (** int copying *)
	  | ICopyB of batom            (** bool to int conversion *)

	  | BAnd  of batom * batom     (** logical and *)
	  | BOr   of batom * batom     (** logical or *)
	  | BNot  of batom             (** logical not *)
	  | BCopy of batom             (** copying *)

	  | Call of string * arg array  (** Procedure call arguments given as [arg array] *)
	  (*| CallE of string * arg array*)

(** e2lang 3-address-like statements *)
type stmt = Store of arg * expr (** evaluate the expression and write the result in the variable *) 
	  | CJmp of string * bvar (** conditional jump *)
	  | Jmp of string         (** unconditional jump *)
	  | Label of string       (** label the following statement *)
	  | Ret of arg            (** return from procedure *)
	  | Debug                 (** print machine state to stdout *)

(** OCaml encoding of e2lang values *)
type value = IVal of int
	   | BVal of bool
	   | DVal of der_val
	   | FVal of float
	   | Void

module StrMap = Map.Make(String) 

(** Prototype of a e2lang procedure *)
type _prototype = {
  ivars : int; (** number of int variables *)
  bvars : int; (** number of bool variables *)
  dvars : int; (** number of der variables *)
  fvars : int; (** number of float variables *)
  args : arg array; (** formal arguments *)				  
  ret : rettype (** return value type *)
}

(** non-executable e2l procedure *)
type proc = Proc of _prototype * stmt array 

(** Non-executable symbol table *)
type ne_proc_map = proc StrMap.t
       

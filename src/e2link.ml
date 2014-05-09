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
module IntMap = Map.Make(struct type t = int let compare = compare end)

open E2lang

(** locations in e2l code *)
type loc_map = int StrMap.t

(** executable e2l procedure *)
type proc_map = procedure StrMap.t

and procedure = {
  p_prototype : _prototype ;
  p_statements : stmt array ;
  p_locations : loc_map ;
  p_procedures : proc_map Lazy.t ;
}

(* linking *)
let label2loc i = function
    (Label s) -> Some ((s, i))
  |  _ -> None

let locMap map loc = match loc with 
    Some((s,i)) -> StrMap.add s i map
  | _ -> map

(* symbol resolution *)
let locations stmts = 
  let labellocs = Array.mapi label2loc stmts in
  Array.fold_left locMap StrMap.empty labellocs 

open Lazy

(* link a bunch of procedures *)
let link ne_procs =
  
  let link_proc table (Proc(proto, stmts)) = 
    { p_prototype = proto ; p_statements = stmts ; 
      p_locations = locations stmts ; 
      p_procedures = table ;
    }
  in
  
  let rec proc_table = lazy (StrMap.map (link_proc proc_table) ne_procs)
 
  in force proc_table

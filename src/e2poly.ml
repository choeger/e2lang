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
  This module contains a definition of polynomials of multiple variables.
  They serve as an example for the usage of e2l
  @author Christoph Höger <christoph.hoeger@tu-berlin.de>
 *)

open E2lang

(** 
  Polynomial data type, "Horner Style"
 *)
type poly = 
  | Number of float   (** a constant number *)		  
  | Variable of int * int * poly * poly list  (** x_i^power(h) + g_1 + .. + g_k *)

(** 
  somewhat optimized int power function 
*)
let rec pow a : int -> float = function
  | 0 -> 1.
  | 1 -> a
  | n -> 
     let b = pow a (n / 2) in
     b *. b *. (if n mod 2 = 0 then 1. else a)

(**
 Evaluation of a polynomial
 @param args the concrete arguments
 *) 
let rec eval_poly args = function
  | Number f -> f
  | Variable (i, p, h, gs) -> let r = (pow args.(i) p) *. (eval_poly args h) in 
			      List.fold_left (fun a b -> a +. b) r (List.map (eval_poly args) gs)

(**
  Multiply a polynomial with a constant 
  @param f the constant value
 *)
let rec poly_mult f = function
    Number f' -> Number (f *. f')
  | Variable (x, n, h, gs) -> Variable(x, n, poly_mult f h, List.map (poly_mult f) gs)

(**
  Calculate the {i total} derivative of a polynomial.
  The unknowns x{_i} are interpreted as functions over a 
  independent variable (i.e. {i x{_i}(t)}) and the derivative
  is calculated w.r.t. that variable.
  Total derivatives are assumed to be [delta] indices away from its base.
  @param delta the width of the argument field, i.e. {i dx{_i}/dt = x{_(i+[delta])}}
  @return total derivative as a polynomial
 *)
let rec diff_total delta = function
    Number _ -> Number 0.0
  | Variable(idx, n, h, gs) -> 
     (* (d)/(dt)(x(t)^n h(t)+g(t)) = g'(t)+x(t)^(n-1) (x(t) h'(t)+n h(t) x'(t)) *)
     let h' = diff_total delta h in
     let gs' = List.map (diff_total delta) gs in
     (*      |----- x^n-1 ----|        |--- x h'---|             |---              n x'h                 ---|             *)
     Variable(    idx, n-1,    Variable( idx, 1, h',   [ Variable(idx+delta, 1, poly_mult (float_of_int n) h, [])]), gs')

(**
 Calculate a partial derivative of a polynomial w.r.t. the given variable
 @param var the variable
 *)
let rec diff_partial var = function
    Number _ -> Number 0.0
  | Variable (x, 0, h, gs) -> Variable(x, 0, Number 0.0, List.map (diff_partial var) (h::gs))
  | Variable (x, n, h, gs) when x = var -> 

     Variable(x, n, diff_partial var h, 
	      Variable(x, n-1, poly_mult (float_of_int n) h, [])::(List.map (diff_partial var) gs))

  | Variable (x, n, h, gs) -> Variable(x, n, diff_partial var h, List.map (diff_partial var) gs)

(**
 Merge a list of polynomials with a constant value.
 *)
let rec merge f = function
  | [] -> Number(f)
  | Number(f') :: [] -> Number(f +. f')
  | Number(f') :: ps -> merge (f +. f') ps
  | Variable(x, n, h, qs) :: ps -> let m = merge f (qs@ps) in Variable(x,n,h,[m])

(**
  Construct a single polynomial from a list (i.e. sum) of polynomials
 *)
let flatten = function
    [] -> Number (0.0)
  | Number(f) :: ps -> (merge f ps)
  | Variable(x, n, h, qs) :: ps -> Variable(x, n, h, qs@ps)

(**
  Simplify a sum of polynomials
 *)
let rec simplify_sum = function
    [] -> []
  | Number(0.0) :: rst -> simplify_sum rst
  | Number(f) :: rst -> [merge f (simplify_sum rst)]
  | p :: rst -> simplify p :: simplify_sum rst

(**
  Apply arithmetic simplification to polynomials
 *)
and simplify = function    
  | Variable(_, _, Number(0.0), ps) -> flatten (simplify_sum ps)
  | Variable(_, 0, h, ps) -> flatten((simplify h) :: (simplify_sum ps))
  | Variable(x, n, h, ps) -> Variable(x, n, simplify h, (simplify_sum ps))
  | Number(f) -> Number(f)

(**
  The fixed-point over simplification, i.e. simplify until nothing changes
 *)
let rec simplify_fix p = let s = simplify p in if s = p then p else simplify_fix s

open E2poly
open E2lang
open E2optimize
open E2jit
open E2basicblock

external eval_dd : method_ptr -> der_val -> der_val -> der_val -> der_val -> unit = "eval_method_dd3"

let () =
    let poly = Variable(0, 2, Variable(1, 1, Number 1., []), [Variable (0, 1, Variable(1,1, Variable(2,4, Number 1., []), []), [])]) in
    let Proc(proto1, stmts) = poly_to_e2 poly 3 in
    let bbs1 = build stmts in
    let (proto2, bbs2) = optimize (Proc (proto1, stmts)) in
    let fmap = StrMap.add "optimized" (proto2, bbs2) (StrMap.add "unoptimized" (proto1, bbs1) StrMap.empty) in
    build_module fmap;
    let pointer = get_pointer "set_meta" in
    eval_ii pointer 0 0;
    let pointer = get_pointer "optimized" in
    let res = [|0.; 0.; 0.; 0.|] in
    eval_dd pointer [|2.; 1.; 0.; 0.|] [|3.; 0.; 1.; 0.|] [|1.; 0.; 0.; 1.|] res;
    Printf.printf "result optimized %f %f %f %f\n" res.(0) res.(1) res.(2) res.(3);
    let pointer = get_pointer "unoptimized" in
    let res = [|0.; 0.; 0.; 0.|] in
    eval_dd pointer [|2.; 1.; 0.; 0.|] [|3.; 0.; 1.; 0.|] [|1.; 0.; 0.; 1.|] res;
    Printf.printf "result unoptimized %f %f %f %f\n" res.(0) res.(1) res.(2) res.(3);
    


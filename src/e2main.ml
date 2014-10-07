open E2poly
open E2lang
open E2optimize
open E2jit
open E2basicblock

open BatPervasives

(*external eval_dd : method_ptr -> der_val -> der_val -> unit = "eval_method_dd1"*)
external eval_dd : method_ptr -> der_val -> der_val -> der_val -> der_val -> unit = "eval_method_dd3"

let sampleE2 = [|Ret(DArg 0)|]
let sampleProto = {ivars=0; bvars=0; dvars=2; fvars=0; args=[|DArg 0; DArg 1|]; ret=DRet}

(*let () =
    let bbs = build sampleE2 in
    Printf.printf "pre optimize\n%!";
    let (proto, bbs2) = optimize (Proc (sampleProto, sampleE2)) in
    Printf.printf "post optimize\n%!";
    let fmap = StrMap.add "test" (proto, bbs2) StrMap.empty in
    build_module fmap*)

let () =
    let poly = Variable(0, 2, Variable(1, 1, Number 1., [Variable(2, 3, Number 1.,[])]), [
               Variable(1, 2, Variable(0, 1, Variable(2, 1, Number 1., []), []), []);
               Variable(2, 2, Variable(1, 1, Number 1., [Variable(2, 1, Number 1., [])]), [])]) in
    let Proc(proto, stmts) = poly_to_e2 poly 3 in
    Printf.printf "%s" (dump stmts)


(*<<<<<<< HEAD
    let poly = Variable(0, 2, Variable(1, 1, Number 1., []), [Variable (0, 1, Variable(1,1, Variable(2,4, Number 1., []), []), [])]) in
    (*let poly = Variable(0, 2, Number 1., []) in*)
    let Proc(proto1, stmts) = poly_to_e2 poly 3 in
(*=======
(*    let poly = Variable(0, 2, Number 1., []) in
    let Proc(proto1, stmts) = poly_to_e2 poly 1 in
>>>>>>> 348cf8efbc88530599d445839ef67d84522b325d*)*)
    let bbs1 = build stmts in
    let (proto2, bbs2) = optimize (Proc (proto1, stmts)) in
    let fmap = StrMap.add "optimized" (proto2, bbs2) (StrMap.add "unoptimized" (proto1, bbs1) StrMap.empty) in
    build_module fmap;
    let pointer = get_pointer "set_meta" in
    eval_ii pointer 3 0;
    let pointer = get_pointer "optimized" in
(*<<<<<<< HEAD*)
    let res = [|0.; 0.; 0.; 0.|] in
    eval_dd pointer [|3.; 1.; 0.; 0.|] [|3.; 0.; 1.; 0.|] [|1.; 0.; 0.; 1.|] res;
    Printf.printf "result optimized %f %f %f %f\n" res.(0) res.(1) res.(2) res.(3);
    let pointer = get_pointer "unoptimized" in
(*=======
    let res = [|0.; 0.|] in
    eval_dd pointer [|3.; 1.|] res;
    Printf.printf "result optimized %f %f %f %f\n" res.(0) res.(1) res.(0) res.(0);
    let pointer = get_pointer "unoptimized" in
    let res = [|0.; 0.|] in
    eval_dd pointer [|3.; 1.|] res;
    Printf.printf "result unoptimized %f %f %f %f\n" res.(0) res.(1) res.(0) res.(0);



    let poly = Variable(0, 2, Variable(1, 1, Number 1., []), [Variable (0, 1, Variable(1,1, Variable(2,4, Number 1., []), []), [])]) in
    let Proc(proto1, stmts) = poly_to_e2 poly 3 in
    let bbs1 = build stmts in
    let (proto2, bbs2) = optimize (Proc (proto1, stmts)) in
    let fmap = StrMap.add "optimized" (proto2, bbs2) (StrMap.add "unoptimized" (proto1, bbs1) StrMap.empty) in
    build_module fmap;
    let pointer = get_pointer "set_meta" in
    eval_ii pointer 3 0;
    let pointer = get_pointer "optimized" in
    let res = [|0.; 0.; 0.; 0.|] in
    eval_dd pointer [|3.; 1.; 0.; 0.|] [|3.; 0.; 1.; 0.|] [|1.; 0.; 0.; 1.|] res;
    Printf.printf "result optimized %f %f %f %f\n" res.(0) res.(1) res.(2) res.(3);
    let pointer = get_pointer "unoptimized" in
>>>>>>> 348cf8efbc88530599d445839ef67d84522b325d*)
    let res = [|0.; 0.; 0.; 0.|] in
    eval_dd pointer [|3.; 1.; 0.; 0.|] [|3.; 0.; 1.; 0.|] [|1.; 0.; 0.; 1.|] res;
    Printf.printf "result unoptimized %f %f %f %f\n" res.(0) res.(1) res.(2) res.(3);*)

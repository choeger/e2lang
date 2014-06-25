open E2poly
open E2lang
open E2optimize
open E2jit
open E2basicblock

let () =
    let poly = Variable(0, 2, Number 1., [Variable (1, 2, Number 1., [])]) in
    let Proc(proto1, stmts) = poly_to_e2 poly 2 in
    let bbs1 = build stmts in
    let (proto2, bbs2) = optimize (Proc (proto1, stmts)) in
    let fmap = StrMap.add "optimized" (proto2, bbs2) (StrMap.add "unoptimized" (proto1, bbs1) StrMap.empty) in
    build_module fmap


open E2lang
open E2basicblock
open E2liveness

(* Building of basic blocks for given statements and prototype. Then followed by liveness analysis and graph construction and coloring. The minimum number of registers is determined, these being equal to the minimum number of colors used for graph coloring. *)
let optimize (Proc(proto, stmts)) =
    let bbs = build stmts in
    let lbs = build_lbs bbs proto.args in
    let g = build_graph proto.dvars lbs in
    let (coloring, cmax) = color_graph g proto.args in
    print_graph g;
    let colored_bbs = apply_coloring coloring bbs in
    let colored_proto = {proto with dvars=cmax+1} in
    (colored_proto, colored_bbs)

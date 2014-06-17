open E2basicblock
open Batteries
open E2lang

type next_lblock = 
    | NoLBlock
    | OneLBlock of lblock
    | TwoLBlocks of lblock * lblock

and lblock = {
    in_sets : BitSet.t array;
    out_sets : BitSet.t array;
    gen_sets : BitSet.t array;
    kill_sets : BitSet.t array;
    id_sets : BitSet.t array;
    return_set : BitSet.t;
    next : next_lblock Lazy.t;
} 

let stmts_live stmts next =
    Printf.printf "new block ------\n%!";
    let gen_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ()) in
    let kill_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ()) in
    let id_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ()) in
    let return_set = BitSet.empty () in
    let parse_stmts i = function
        | Store (DArg d, DMul (d1,d2)) -> BitSet.set gen_sets.(i) d1;
                                          BitSet.set gen_sets.(i) d2;
                                          BitSet.set kill_sets.(i) d
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Store (DArg d, DAdd (d1,d2)) -> BitSet.set gen_sets.(i) d1;  
                                          BitSet.set gen_sets.(i) d2;
                                          BitSet.set kill_sets.(i) d
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Store (DArg d, DPwr(_,d1))   -> BitSet.set gen_sets.(i) d1;
                                          BitSet.set kill_sets.(i) d
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Store (DArg d, DLoadF _)     -> BitSet.set kill_sets.(i) d
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Store (DArg d, DCopy d1)     -> BitSet.set gen_sets.(i) d1;
                                          BitSet.set kill_sets.(i) d;
                                          BitSet.set id_sets.(i) d1
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Store (DArg d, Call (s, args)) ->
                Array.iter (function 
                    | DArg d -> BitSet.set gen_sets.(i) d
                    | _ -> () ) args;
                BitSet.set kill_sets.(i) d
                                          ;Printf.printf "setting ks entry %d in %d\n" d i;
                                          BitSet.print IO.stdout kill_sets.(i);
                                          Printf.printf "\n%!"
        | Ret (DArg d) -> BitSet.set gen_sets.(i) d;
                          BitSet.set return_set d;
        | _ -> ()
    in
        Array.iteri parse_stmts stmts;
        {in_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ()); 
         out_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ());
         gen_sets = gen_sets; kill_sets = kill_sets; id_sets = id_sets;
         return_set = return_set; next = next}

let block_live blist =
    let bbmap = List.fold_left (fun map bb -> StrMap.add bb.name bb map ) StrMap.empty blist in
    let next map bbn = match bbn with
        | NoBlock -> NoLBlock
        | OneBlock bb -> OneLBlock (StrMap.find bb map)
        | CondBlocks (_,b1,b2) -> TwoLBlocks (StrMap.find b1 map,StrMap.find b2 map) in
    let rec map = lazy (StrMap.mapi (fun name bb -> stmts_live bb.stmts (lazy (nextc bb))) bbmap)
    and nextc bb = next (Lazy.force map) bb.next in
    List.map (fun bb -> StrMap.find bb.name (Lazy.force map)) blist

let rec succ_in lb = match (Lazy.force lb.next) with
    | NoLBlock -> lb.return_set
    | OneLBlock lbn -> if (Array.length lbn.in_sets) = 0
                       then succ_in lbn      (* fails on invalid (non terminating) programs *)
                       else lbn.in_sets.(0)
    | TwoLBlocks (lbn1, lbn2) ->
            let set1 = if (Array.length lbn1.in_sets) = 0
                       then succ_in lbn1     (* fails on invalid (non terminating) programs *)
                       else lbn1.in_sets.(0) in
            let set2 = if (Array.length lbn2.in_sets) = 0
                       then succ_in lbn2     (* fails on invalid (non terminating) programs *)
                       else lbn2.in_sets.(0) in
            BitSet.union set1 set2

let update_stmt lb i =
    (*Printf.printf "out_set before: ";
    BitSet.print IO.stdout lb.out_sets.(i);
    Printf.printf " - kill_set before: ";
    BitSet.print IO.stdout lb.kill_sets.(i);
    Printf.printf "- gen_set before: ";
    BitSet.print IO.stdout lb.gen_sets.(i);
    Printf.printf "\n%!";*)
    let new_in = BitSet.union lb.gen_sets.(i) ( BitSet.diff lb.out_sets.(i) lb.kill_sets.(i)) in
    let in_changed = new_in <> lb.in_sets.(i) in
    (*Printf.printf "stmt %d - old_in:" i;
    BitSet.print IO.stdout lb.in_sets.(i);
    Printf.printf "; new_in:";
    BitSet.print IO.stdout new_in;
    Printf.printf "\n%!";*)
    lb.in_sets.(i) <- new_in;
    let new_out = if i = ((Array.length lb.in_sets) - 1) then succ_in lb else lb.in_sets.(i+1) in
    (*Printf.printf "stmt %d - old_out:" i;
    BitSet.print IO.stdout lb.out_sets.(i);
    Printf.printf "; new_out:";
    BitSet.print IO.stdout new_out;
    Printf.printf "\n%!";*)
    let out_changed = new_out <> lb.out_sets.(i) in
    lb.out_sets.(i) <- new_out;
    in_changed || out_changed

let iterate_stmts lb =
    let rec it_rec i = if i = Array.length lb.in_sets
                       then false
                       else update_stmt lb i || it_rec (i+1)
    in 
    it_rec 0

let iterate_blocks lbs =
    List.exists (fun x -> x) (List.map iterate_stmts lbs)

let rec iterate_fp lbs =
    if iterate_blocks lbs then iterate_fp lbs else () 

open Graph

module IntMod = struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
end

module G = Imperative.Graph.Concrete(IntMod)

let build_nodes g n = 
    let new_node i = let v = G.V.create i in G.add_vertex g v; v in
    Array.init n new_node

let build_edges g nodes lb i = 
    let out_set = BitSet.diff lb.out_sets.(i) lb.id_sets.(i) in
    let edge_to = BitSet.enum out_set in
    let make_edge ki oi =
        if ki <> oi then G.add_edge g nodes.(ki) nodes.(oi) else () in
    Enum.iter (fun ki -> Enum.iter ( fun oi -> make_edge ki oi) edge_to) (BitSet.enum lb.kill_sets.(i))

let build_graph n lbs =
    let g = G.create() in 
    let nodes = build_nodes g n in
    let build_all_edges lb = 
        Array.iteri (fun i _ -> build_edges g nodes lb i) lb.in_sets in 
    List.iter build_all_edges lbs;
    g

module Dot = Graph.Graphviz.Dot(struct
    include G (* use the graph module from above *)
    let edge_attributes e = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = [`Shape `Circle]
    let vertex_name v = string_of_int v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
end)

let print_graph g = Dot.output_graph Pervasives.stdout g

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
        | Store (DArg d, DAdd (d1,d2)) -> BitSet.set gen_sets.(i) d1;  
                                          BitSet.set gen_sets.(i) d2;
                                          BitSet.set kill_sets.(i) d
        | Store (DArg d, DPwr(_,d1))   -> BitSet.set gen_sets.(i) d1;
                                          BitSet.set kill_sets.(i) d
        | Store (DArg d, DLoadF _)     -> BitSet.set kill_sets.(i) d
        | Store (DArg d, DCopy d1)     -> BitSet.set gen_sets.(i) d1;
                                          BitSet.set kill_sets.(i) d;
                                          BitSet.set id_sets.(i) d1
        | Store (DArg d, Call (s, args)) ->
                Array.iter (function 
                    | DArg d -> BitSet.set gen_sets.(i) d
                    | _ -> () ) args;
                BitSet.set kill_sets.(i) d
        | Ret (DArg d) -> BitSet.set gen_sets.(i) d;
                          BitSet.set return_set d;
        | _ -> ()
    in
        Array.iteri parse_stmts stmts;
        {in_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ()); 
         out_sets = Array.init (Array.length stmts) (fun _ -> BitSet.empty ());
         gen_sets = gen_sets; kill_sets = kill_sets; id_sets = id_sets;
         return_set = return_set; next = next}

let increase_array a =
    Array.init ((Array.length a)+1) (fun i -> if i == 0 then BitSet.empty () else a.(i-1))

(*ks is inintial kill set *)
let init_kill map ks =
    let first_block = StrMap.find "start" map in
    let gen_sets = increase_array first_block.gen_sets in
    let kill_sets = increase_array first_block.kill_sets in
    let id_sets = increase_array first_block.id_sets in
    let in_sets = increase_array first_block.in_sets in
    let out_sets = increase_array first_block.out_sets in
    BitSet.unite kill_sets.(0) ks; 
    let new_start = {in_sets; out_sets; gen_sets; kill_sets; id_sets; return_set = first_block.return_set; next = first_block.next} in
    StrMap.add "start" new_start map


let block_live blist ks =
    let bbmap = List.fold_left (fun map bb -> StrMap.add bb.name bb map ) StrMap.empty blist in
    let next map bbn = match bbn with
        | NoBlock -> NoLBlock
        | OneBlock bb -> OneLBlock (StrMap.find bb map)
        | CondBlocks (_,b1,b2) -> TwoLBlocks (StrMap.find b1 map,StrMap.find b2 map) in
    let rec map = lazy (StrMap.mapi (fun name bb -> stmts_live bb.stmts (lazy (nextc bb))) bbmap)
    and nextc bb = next (Lazy.force map) bb.next in
    let new_map = init_kill (Lazy.force map) ks in
    List.map (fun bb -> StrMap.find bb.name new_map) blist


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
    let new_in = BitSet.union lb.gen_sets.(i) ( BitSet.diff lb.out_sets.(i) lb.kill_sets.(i)) in
    let in_changed = new_in <> lb.in_sets.(i) in
    lb.in_sets.(i) <- new_in;
    let new_out = if i = ((Array.length lb.in_sets) - 1) then succ_in lb else lb.in_sets.(i+1) in
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

let build_lbs blist args =
    let ks = BitSet.empty() in
    Array.iter ( fun arg -> match arg with 
                            DArg d -> BitSet.set ks d
                            | _ -> ()) args; 
    let lbs = block_live blist ks in
    iterate_fp lbs;
    lbs

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
        if ki <> oi
        then (
            G.add_edge g nodes.(ki) nodes.(oi);
            G.add_edge g nodes.(oi) nodes.(ki)
        ) else ()
    in
    Enum.iter (fun ki -> Enum.iter ( fun oi -> make_edge ki oi) edge_to) (BitSet.enum lb.kill_sets.(i))

let build_graph n lbs =
    let g = G.create() in 
    let nodes = build_nodes g n in
    let build_all_edges lb = 
        Array.iteri (fun i _ -> build_edges g nodes lb i) lb.in_sets in 
    List.iter build_all_edges lbs;
    g

let default_colors = [|0x00FF00; 0x0000FF; 0xFF0000; 0x990099; 0x999900; 0xFFFFFF; 0x000000; 0x999999; 0x44FF44; 0x4444FF; 0xFF4444|]

module Dot =
    struct
        let colors : int array = Array.make 50 0;
        include Graph.Graphviz.Dot(struct
            include G (* use the graph module from above *)
            let edge_attributes e = []
            let default_edge_attributes _ = []
            let get_subgraph _ = None
            let vertex_attributes v =
                let color = default_colors.(colors.(v)) in
                [`Shape `Circle; `Fillcolor color; `Style `Filled]
            let vertex_name v = string_of_int v
            let default_vertex_attributes _ = []
            let graph_attributes _ = []
        end)
    end

let print_graph g =
    let oc = Pervasives.open_out "graph.dot" in
    Dot.output_graph oc g;
    Pervasives.close_out oc

let sat_degree g v coloring = 
     let list_neigh succ l = if List.mem coloring.(succ) l || coloring.(succ) = -1 then l else coloring.(succ)::l in
     let neighbours = G.fold_succ list_neigh g v [] in
     List.length neighbours

let degree g v = 
     let neighbours = G.fold_succ ( fun succ l -> succ::l) g v [] in
     List.length neighbours

let color g index coloring =
    let list_neigh v l = if coloring.(v) <> -1 then coloring.(v)::l else l in
    let neighbours = List.sort (-) (G.fold_succ list_neigh g index []) in
    let vertex_color = List.fold_left (fun newcolor color -> 
                                if newcolor = color then newcolor+1 else newcolor) 0 neighbours in
    Array.set coloring index vertex_color;
    Dot.colors.(index) <- vertex_color;
    vertex_color
            
let color_graph g = 
    let size_g = G.nb_vertex g in
    let coloring = Array.make size_g (-1) in
    let rec c_node ncn = 
        if ncn = size_g then 0
        else
            let find_index v (max,index) = 
                if coloring.(v) = -1 then
                    let d = sat_degree g v coloring in
                    if d > max then (d,v) else 
                    if d = max then
                        if degree g v > degree g index then (d,v)
                        else (d,index)
                    else (max,index)
                else (max,index) in 
            let (_,index) = G.fold_vertex find_index g (-1,0) in
            let c1 = color g index coloring in
            let c2 = c_node (ncn+1) in
            if c1 > c2 then c1 else c2
    in
    let max = c_node 0 in
    (coloring, max)

let apply_coloring_to_expr coloring =
    let col i = coloring.(i) in
    function
        | DMul (a1, a2) -> DMul (col a1, col a2)
        | DAdd (a1, a2) -> DAdd (col a1, col a2)
        | DPwr (i, a)   -> DPwr (i, col a)
        | DCopy (a)     -> DCopy (col a)
        | Call (n, args) -> 
                let col_args = Array.map (fun arg -> match arg with
                                                     DArg i -> DArg (col i)
                                                     | _ -> arg  ) args in
                Call (n, col_args)
        | e             -> e

let apply_coloring_to_stmt coloring =
    let col i = coloring.(i) in
    function
        | Store (DArg c, e) -> Store (DArg (col c), apply_coloring_to_expr coloring e)
        | Ret (DArg c)   -> Ret (DArg (col c))
        | s                 -> s

let apply_coloring_to_block coloring bb = 
    let stmts = Array.map (apply_coloring_to_stmt coloring) bb.stmts in
    {name=bb.name; stmts=stmts; next=bb.next}

let apply_coloring coloring =
    List.map (apply_coloring_to_block coloring)


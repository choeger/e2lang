open E2basicblock

type next_lblock = 
    | NoLBlock
    | OneLBlock of lblock
    | TwoLBlocks of lblock * lblock

and lblock = {
    in_sets : BitSet.t array;
    out_sets : BitSet.t array;
    gen_sets : BitSet.t array;
    kill_sets :BitSet.t array;
    next : next_lblock Lazy.t;
} 

let block_live blist =
    let bbmap = List.fold_left (fun map bb -> StrMap.add bb.name bb map ) StrMap.empty blist in
    let next map bbn = match bbn with
        | NoBlock -> NoLBlock
        | OneBlock bb -> OneLBlock (StrMap.lookup bb map)
        | TwoBlocks (_,b1,b2) -> TwoLBlocks (StrMap.lookup b1 map,StrMap.lookup b2 map) in
    let rec map = StrMap.map (fun name bb -> stmts_live bb.stmts (lazy (next map bb.next))) bbmap in
    List.map (fun bb -> StrMap.lookup bb.name map) blist


let stmts_live stmts next =
    let gen_sets = Array.init (Array.length stmts) BitSet.empty in
    let kill_sets = Array.init (Array.length stmts) BitSet.empty in
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
                                          BitSet.set kill_sets.(i) d
        | Store (DArg d, Call (s, args)) ->
                Array.iter (function 
                    | DArg d -> BitSet.set gen_sets.(i) d
                    | _ -> () ) args;
                BitSet.set kill_sets.(i) d
        | Ret (DArg d) -> BitSet.set gen_sets.(i) d 
        | _ -> ()
    in
        Array.iteri parse_stmts stmts;
        {in_sets = Array.init (Array.length stmts) BitSet.empty; 
         out_sets = Array.init (Array.length stmts) BitSet.empty;
         gen_sets = gen_sets; kill_sets = kill_sets;
         next = next}

 
    

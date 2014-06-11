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
    kill_sets :BitSet.t array;
    next : next_lblock Lazy.t;
} 

let stmts_live stmts next =
    let gen_sets = Array.make (Array.length stmts) (BitSet.empty ()) in
    let kill_sets = Array.make (Array.length stmts) (BitSet.empty ()) in
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
        {in_sets = Array.make (Array.length stmts) (BitSet.empty ()); 
         out_sets = Array.make (Array.length stmts) (BitSet.empty ());
         gen_sets = gen_sets; kill_sets = kill_sets;
         next = next}

let block_live blist =
    let bbmap = List.fold_left (fun map bb -> StrMap.add bb.name bb map ) StrMap.empty blist in
    let next map bbn = match bbn with
        | NoBlock -> NoLBlock
        | OneBlock bb -> OneLBlock (StrMap.find bb map)
        | CondBlocks (_,b1,b2) -> TwoLBlocks (StrMap.find b1 map,StrMap.find b2 map) in
    let rec map = lazy (StrMap.mapi (fun name bb -> stmts_live bb.stmts (lazy (nextc bb))) bbmap)
    and nextc bb = next (Lazy.force map) bb.next in
    List.map (fun bb -> StrMap.find bb.name (Lazy.force map)) blist

let iterate lb i =
    lb.in_sets.(i) <- BitSet.union lb.gen_sets.(i) ( BitSet.diff lb.out_sets(i) lb.kill_sets.(i)) ;
    lb.out_sets.(i) <- if i = ((Array.length lb.in_sets) -1) then succ_in lb else lb.in_sets.(i+1)
     
     

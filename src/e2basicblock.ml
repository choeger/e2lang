open E2lang

(* Structure to identify the next block by its name *)
type next_block =
    | NoBlock
    | OneBlock of string
    | CondBlocks of int * string * string

(* Basic block representation *)
type bblock = {
    name : string;
    stmts : stmt array;
    next : next_block;
}

(* Recursive function for build the basic blocks, having as input the statements and a name. Each block is assigned a different name, useful for identifying the next block or blocks*)
let rec build_blocks name stmts = function
    (si, ei) ->
        if ei >= Array.length(stmts)
        then
            if si = ei then [] else [{name = name; stmts = Array.sub stmts si (ei - si); next = NoBlock}]
        else
            match stmts.(ei) with
            | Label str ->
                    let lblname = "lbl$" ^ str in
                    let nb = build_blocks lblname stmts (ei+1, ei+1) in
                    {name = name; stmts = Array.sub stmts si (ei - si); next = OneBlock lblname} :: nb
            | Jmp str ->
                    let new_name = "postj$" ^ name in
                    let lblname = "lbl$" ^ str in
                    let nb = build_blocks new_name stmts (ei+1, ei+1) in
                    {name = name; stmts = Array.sub stmts si (ei - si); next = OneBlock lblname}::nb
            | CJmp (str, cond) -> 
                    let new_name = "postcj$" ^ name in
                    let lblname = "lbl$" ^ str in
                    let nb = build_blocks new_name stmts (ei+1, ei+1) in
                    {name = name; stmts = Array.sub stmts si (ei - si); next = CondBlocks (cond, lblname, new_name)}::nb
            | Ret arg -> 
                    let new_name = "postret$" ^ name in
                    let nb = build_blocks new_name stmts (ei+1, ei+1) in
                    {name = name; stmts = Array.sub stmts si (ei - si + 1); next = NoBlock}::nb
            | _ -> build_blocks name stmts (si, ei + 1)

(* Calling of the basick block building function with the first block named "start"*)
let build stmts = build_blocks "start" stmts (0, 0)


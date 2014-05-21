open E2lang

type next_block =
    | NoBlock
    | OneBlock of string
    | CondBlocks of int * string * string

type bblock = {
    name : string;
    (*start_pos : int;
    end_pos : int;*)
    stmts : stmt array;
    next : next_block;
}

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
                    {name = name; stmts = Array.sub stmts si (ei - si); next = CondBlocks (cond, new_name, lblname)}::nb
            | Ret arg -> 
                    let new_name = "postret$" ^ name in
                    let nb = build_blocks new_name stmts (ei+1, ei+1) in
                    {name = name; stmts = Array.sub stmts si (ei - si + 1); next = NoBlock}::nb
            | _ -> build_blocks name stmts (si, ei + 1)

let build stmts = build_blocks "start" stmts (0, 0)


open E2lang

type next_block =
    | NoBlock
    | OneBlock of string
    | TwoBlocks of string * string

type bblock = {
    name : string;
    start_pos : int;
    end_pos : int;
    next : next_block;
}

let rec build_blocks name stmts = function
    (si, ei) ->
        if ei > Array.length(stmts)
        then
            [{name = name; start_pos = si; end_pos = ei; next = NoBlock}]
        else
            match stmts.(ei) with
            | Label str ->
                    let lblname = "$lbl$" ^ str in
                        let nb = build_blocks lblname stmts (ei+1, ei+1) in
                        {name = name; start_pos = si+1; end_pos = ei+1; next = OneBlock lblname} :: nb
            | _ -> build_blocks name stmts (si, ei + 1)

let build stmts = build_blocks "start" stmts (0, 0)


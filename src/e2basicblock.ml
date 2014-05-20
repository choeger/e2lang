open E2lang

type next_block =
    | NoBlock
    | OneBlock of bblock
    | TwoBlocks of bblock * bblock 
and bblock = {
    name : string;
    start_pos : int;
    end_pos : int;
    next : next_block;
}



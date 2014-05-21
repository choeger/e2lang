#require "smart_print" ;;
#directory "_build/src" ;;
#load "e2lang.cmo" ;;
#load "e2poly.cmo" ;;
#load "e2pretty.cmo" ;;
#load "e2basicblock.cmo" ;;

open E2lang ;;
open E2pretty ;;
open E2poly ;;
open E2basicblock ;;

let p = Variable(0, 2, Variable(1, 2, Number(1.0), []), []) ;;

let test_program = [|
    Store( IArg 0, ICopy(IntLit 10)  ) ;  
    Store( IArg 1, ICopy(IntLit  0)  ) ;
    Store( IArg 2, ICopy(IntLit  1)  ) ;
    Store( IArg 3, ICopy(IntLit(-1))  ) ;
    Label("Loop") ;
    Store( IArg 1, IAdd( (IntVar 1), (IntVar 2) ) ) ; (* x1 := x1 + 1 *)
    Store( IArg 0, IAdd( (IntVar 0), (IntVar 3) ) ) ; (* x0 := x0 - 1 *)
    Store( BArg 0, IEquals( (IntVar 0), (IntLit 0) )  ) ;
    Store( BArg 0, BNot (BoolVar 0) ) ;
    CJmp( "Loop", 0) ;
    Ret (IArg 1) 
   |]

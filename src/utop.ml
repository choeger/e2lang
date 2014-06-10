#require "smart_print" ;;
#require "batteries" ;;
#directory "_build/src" ;;
#load "e2lang.cmo" ;;
#load "e2poly.cmo" ;;
#load "e2pretty.cmo" ;;
#load "e2basicblock.cmo" ;;
#load "e2liveness.cmo" ;;

open E2lang ;;
open E2pretty ;;
open E2poly ;;
open E2basicblock ;;
open E2liveness ;;

let p = Variable(0, 2, Variable(1, 2, Number(1.0), []), []) ;;


let test_fac = [|
    Store( IArg 1, ICopy(IntLit 1) ) ;
    Store( BArg 0, IEquals ( (IntVar 0), IntLit 0 ) ) ; (* x0 = 0 ? *)
    Store( BArg 1, IEquals ( (IntVar 0), IntLit 1 ) ) ; (* x0 = 1 ? *)
    Store( BArg 0, BOr( (BoolVar 0), (BoolVar 1) ) ) ;
    CJmp ( "Ret", 0 ) ;

    Label("Loop") ;
    Store( IArg 1, IMul ( (IntVar 1), (IntVar   0 ) ) ) ;
    Store( IArg 0, IAdd ( (IntVar 0), (IntLit (-1)) ) ) ;

    Store( BArg 0, IEquals ( (IntVar 0), IntLit 1 ) ) ; 
    Store( BArg 0, BNot (BoolVar 0) ) ; (* x0 |= 1 ? *)
    CJmp( "Loop", 0 );
    
    Label ("Ret") ;
    Ret (  IArg 1)
  |]

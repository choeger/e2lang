#require "smart_print" ;;
#directory "_build" ;;
#load "e2lang.cmo" ;;
#load "e2poly.cmo" ;;
#load "e2pretty.cmo" ;;

open E2lang ;;
open E2pretty ;;
open E2poly ;;

let p = Variable(0, 2, Variable(1, 2, Number(1.0), []), []) ;;

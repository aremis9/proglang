(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* val all_except_option = fn : string * string list -> string list option
   val get_substitutions1 = fn : string list list * string -> string list
   val get_substitutions2 = fn : string list list * string -> string list
   val similar_names = fn : string list list * {first:string, last:string, middle:string}
   -> {first:string, last:string, middle:string} list *)
	     
(* put your solutions for problem 1 here *)
fun all_except_option (str, lst) =
    case lst of
	[] => NONE
      | s::lst' =>
	if same_string(str, s)
	then SOME lst'
	else case all_except_option(str, lst') of
		 NONE => NONE
	       | SOME tail => SOME (s::tail)

fun get_substitutions1 (lsts, str) =
    case lsts of
	[] => []
      | lst::lsts' =>
	case all_except_option(str, lst) of
	    NONE => get_substitutions1(lsts', str)
	  | SOME x => x @ get_substitutions1(lsts', str)
					   
fun get_substitutions2 (lsts, str) =
    let
	fun get (lsts, str, acc) =
	    case lsts of
		[] => acc
	      | lst::lsts' =>
		case all_except_option(str, lst) of
		    NONE => get(lsts', str, acc)
		  | SOME x => get(lsts', str, x @ acc)
    in
	get(lsts, str, [])
    end
	
fun similar_names (lsts, {first=x, middle=y, last=z}) =
    let
	fun generate(similars, y, z, acc) =
	    case similars of
		[] => acc
	      | s::similars' => generate(similars', y, z, acc @ [{first=s, middle=y, last=z}])
    in
	generate(get_substitutions1(lsts, x), y, z, [{first=x, middle=y, last=z}])
    end
	
    
	
	     
	     

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

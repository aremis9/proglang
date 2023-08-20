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


(* val card_color = fn : card -> color
   val card_value = fn : card -> int
   val remove_card = fn : card list * card * exn -> card list
   val all_same_color = fn : card list -> bool
   val sum_cards = fn : card list -> int
   val score = fn : card list * int -> int
   val officiate = fn : card list * move list * int -> int *)
	      
(* put your solutions for problem 2 here *)

fun card_color (suit, rank) =
    case suit of
	 Spades => Black 
       | Clubs  => Black 
       | _  => Red  

fun card_value (suit, rank) =
    case rank of
	Num i => i 
      | Ace => 11
      | _ => 10 

fun remove_card (cs, c, e) =
    case cs of
	[] => []
      | c0::cs' =>
	if c0 = c
	then cs'
	else case remove_card(cs', c, e) of
		 [] => raise e
	       | x => c0::x

fun all_same_color (cs) =
    case cs of
	[] => true
      | _::[] => true 
      | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

fun sum_cards (cs) =
    let
	fun sum(cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => sum(cs', (card_value(c) + acc)) 
    in
	sum(cs, 0)
    end

fun score (hc, g) =
    let val prelim_score =
	if sum_cards(hc) > g
	then 3 * (sum_cards(hc) - g)
	else g - sum_cards(hc)
    in
	if all_same_color(hc)
	then prelim_score div 2
	else prelim_score
    end
	
fun officiate (cs, moves, g) =
    let
	fun game_state(held_cards, move_list, card_list) =
	    if sum_cards(held_cards) > g
	    then score(held_cards, g)
	    else
		case move_list of
		    [] => score(held_cards, g)
		  | m::move' =>
		    (case m of
			 Draw =>
			 (case card_list of
			      [] => score(held_cards, g)
			    | (c0,c1)::card_list' => game_state(held_cards @ [(c0,c1)], move', card_list'))
		       | Discard d => game_state(remove_card(held_cards, d, IllegalMove), move', card_list))
    in
	game_state([], moves, cs)
    end

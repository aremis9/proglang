(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = List.filter (fn s => Char.isUpper(String.sub(s, 0))) xs

fun longest_string1 xs =
    let val init = ""
    in List.foldl (fn (s, it) => if String.size(s) > String.size(it) then s else it) init xs
    end

fun longest_string2 xs =
    let val init = ""
    in List.foldl (fn (s, it) => if String.size(s) >= String.size(it) then s else it) init xs
    end

fun longest_string_helper f = fn xs =>
				 List.foldl (fn(s1, s2) => if f(String.size(s1), String.size(s2))
							   then s1 else s2) "" xs
					    
val longest_string3 =
    longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 =
    longest_string_helper (fn (s1, s2) => s1 >= s2)

fun longest_capitalized xs = (longest_string1 o only_capitals) xs

fun rev_string s = (String.implode o List.rev o String.explode) s

fun first_answer f = fn xs =>
			case xs of
			    [] => raise NoAnswer
			  | x::xs' => case f x of
					  NONE => first_answer f xs'
					| SOME v => v

fun all_answers f = fn xs => let fun append (lst, acc) =
				     case lst of
					 [] => SOME acc
				       | l::lst' => (case f l of
							 NONE => NONE
						       | SOME v => append(lst', (v @ acc)))
			     in
				 append(xs, [])
			     end
	
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
	fun var_names q =
	    case q of	    
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, i) => i @ (var_names p)) [] ps
	      | ConstructorP(_,p) => var_names p
	      | _ => []
	fun check_repeats xs =
	    case xs of
		[] => false
	      | x::xs' => List.exists (fn s => x = s) xs'
    in
	not ((check_repeats o var_names) p)
    end

fun match (v, p) = 
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME [] 	(* UnitP matches only Unit *)
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple(vs), TupleP(ps)) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2 then match(v, p) else NONE
      | (_,_) => NONE

fun first_match v = fn pl =>
		       SOME (first_answer (fn p => match(v, p)) pl)
		       handle NoAnswer => NONE

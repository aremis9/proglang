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
				 (* case xs of *)
				 (*     [] => s *)
				 (*   | x::xs' => if f(String.size(x), String.size(s)) *)
				 (* 	       then longest_string_helper f xs' x *)
				 (* 	       else longest_string_helper f xs' s *)
								     
val longest_string3 =
    longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 =
    longest_string_helper (fn (s1, s2) => s1 >= s2)

			  

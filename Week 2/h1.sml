(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
(* val number_in_month = fn : (int * int * int) list * int -> int *)
(* val number_in_months = fn : (int * int * int) list * int list -> int *)
(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
(* val get_nth = fn : string list * int -> string *)
(* val date_to_string = fn : int * int * int -> string *)
(* val number_before_reaching_sum = fn : int * int list -> int *)
(* val what_month = fn : int -> int *)
(* val month_range = fn : int * int -> int list *)
(* val oldest = fn : (int * int * int) list -> (int * int * int) option *)

(* data format: YEAR, MONTH, DAY *)

fun is_older (d1: (int * int *int), d2: (int * int *int)) =
    if #1 d1 = 0 andalso #1 d2 = ~1
    then false
    else
	if #1 d1 < #1 d2
	then true
	else
	    if #1 d1 > #1 d2
	    then false
	    else is_older((#2 d1, #3 d1, 0), (#2 d2, #3 d2, ~1))


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

(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
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

(* val number_in_month = fn : (int * int * int) list * int -> int *)
fun number_in_month (d: (int * int * int) list, m: int) =
    if null d
    then 0
    else
	if m = #2 (hd d)
	then 1 + number_in_month(tl d, m)
	else number_in_month(tl d, m)

(* val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months (d: (int * int * int) list, m: int list) =
    if null d orelse null m
    then 0
    else
	let val n = number_in_month(d, hd m)
	in
	    if n > 0
	    then n + number_in_months(d, tl m)
	    else number_in_months(d, tl m)
	end

(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (d: (int * int * int) list, m: int) =
    if null d
    then []
    else
	if m = #2 (hd d)
	then hd d :: dates_in_month(tl d, m)
	else dates_in_month(tl d, m)

(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (d: (int * int * int) list, m: int list) =
    if null d orelse null m
    then []
    else dates_in_month(d, hd m) @ dates_in_months(d, tl m)

(* val get_nth = fn : string list * int -> string *)
fun get_nth (s: string list, n: int) =
    if null s
    then ""
    else
	if n = 1
	then hd s
	else get_nth(tl s, n - 1)
			    
val months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

(* val date_to_string = fn : int * int * int -> string *)
fun date_to_string (YY: int, MM: int, DD: int) =
    



			 

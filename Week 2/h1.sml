
(* date format: YEAR, MONTH, DAY *)

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

(* val date_to_string = fn : int * int * int -> string *)
fun date_to_string (YY: int, MM: int, DD: int) =
    let val months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth (months, MM) ^ " " ^ Int.toString(DD) ^ ", " ^ Int.toString(YY)
    end

(* val number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (sum: int, nums: int list) =
    if null nums
    then 0
    else
	if sum <= hd nums
	then 0
	else 1 + number_before_reaching_sum (sum - hd nums, tl nums)

(* val what_month = fn : int -> int *)
fun what_month (day: int) =
    let val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
	number_before_reaching_sum (day, days_in_months) + 1
    end

(* val month_range = fn : int * int -> int list *)
fun month_range (d1: int, d2: int) =
    if d1 > d2
    then []
    else
	what_month d1 :: month_range(d1 + 1, d2)
	
(* val oldest = fn : (int * int * int) list -> (int * int * int) option *)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty (dates: (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else let val tl_ans = oldest_nonempty(tl dates)
		     in
			 if is_older(hd dates, tl_ans)
			 then hd dates
			 else tl_ans
		     end
	in
	    SOME (oldest_nonempty dates)
	end
	    
	

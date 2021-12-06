fun is_older (date1: int*int*int, date2: int*int*int) =
    ((#1 date2 * 365) + (#2 date2 * 31) + (#3 date2)) > ((#1 date1 * 365) + (#2 date1 * 31) + (#3 date1))

fun number_in_month (dates: (int*int*int) list, month: int) = 
	if null dates
	then 0
	else if (#2 (hd dates)) = month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)		

fun dates_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then []
	else if (#2 (hd dates)) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
	if null months
	then []
	else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)	
			     
fun get_nth (strings: string list, pos: int) =
	let fun find_nth (strs: string list, currentPos: int) =
			if currentPos = pos
			then hd strs
			else find_nth(tl strs, currentPos + 1)
	in
		find_nth(strings, 1)
	end				     

fun date_to_string (date: int*int*int) =
	let val monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	in
		get_nth(monthNames, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
	end

fun number_before_reaching_sum (sum: int, numList: int list) =
	let fun addListNums (numListLocal: int list, currSum: int, count: int) =
			if null numListLocal
			then currSum
			else
				let val newSum = currSum + hd numListLocal
				in
					if newSum >= sum
					then count
					else addListNums(tl numListLocal, newSum, count + 1)
				end
	in
		addListNums(numList, 0, 0)
	end

fun maxnumber_before_reaching_sum (sum: int, numList: int list) =
	let fun addListNums (numListLocal: int list, currSum: int, count: int) =
			if null numListLocal
			then currSum
			else
				let val newSum = currSum + hd numListLocal
				in
					if newSum >= sum
					then currSum
					else addListNums(tl numListLocal, newSum, count + 1)
				end
	in
		addListNums(numList, 0, 0)
	end	

fun what_month (day: int) =
	let val monthsDays = [31,28,31,30,31,30,31,31,30,31,30,31]
	in
		let val fullMonths = number_before_reaching_sum(day, monthsDays)
		in
			let val fullDays = maxnumber_before_reaching_sum(day, monthsDays)
			in	
				if day > fullDays
				then fullMonths + 1
				else fullMonths
			end		
		end		
	end

fun month_range (day1: int, day2: int) =
	let fun countMonths (currMonths: int list) =
			if day1 > day2
			then currMonths
			else what_month(day1) :: month_range(day1 + 1, day2)
	in
		countMonths([])
	end				

fun oldest (dates: (int*int*int) list) =
	if null dates
	then NONE
	else
		let fun oldest_nonempty (dates: (int*int*int) list) =
				if null (tl dates)
				then hd dates
				else 
					let val tl_ans = oldest_nonempty(tl dates)
					in
						if is_older(hd dates, tl_ans)
						then hd dates
						else tl_ans
				end
		in
			SOME (oldest_nonempty(dates))
		end		
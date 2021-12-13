exception NoAnswer

fun only_capitals (strList) =
	List.filter (fn str => Char.isUpper(String.sub (str, 0))) strList

fun longest_string1 (strList) = 
	List.foldl (fn (str, acc) => if String.size(str) > String.size(acc)
								 then str
								 else acc) "" strList

fun longest_string2 (strList) = 
	List.foldl (fn (str, acc) => if String.size(str) >= String.size(acc)
								 then str
								 else acc) "" strList

fun longest_string_helper compare strList =
	List.foldl (fn (str, acc) => if  compare (String.size(str), String.size(acc))
								 then str
								 else acc) "" strList

val longest_string3 = longest_string_helper op>

val longest_string4 = longest_string_helper op>=

val longest_capitalized =
	longest_string1 o only_capitals

val rev_string =
	String.implode o List.rev o String.explode

fun first_answer question lst =
	case lst of
		  [] => raise NoAnswer
	    | x::xs => case question(x) of
	    				  SOME y => y
	    				| NONE => first_answer question xs	

fun all_answers question lst =
	let fun all_answ_helper(remLst, acc) = 
		case remLst of
			[] => acc
			| x::xs => case question(x) of
						      NONE => NONE
						    | SOME y => all_answ_helper(xs, SOME(y @ Option.valOf acc))

	in 
		all_answ_helper(lst, SOME [])
	end	 

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

fun count_wildcards p = 
	let val count = ref 0
	in
		((g (fn _ => (count := (!count) + 1; !count)) (fn _ => 0) p); !count)
	end

fun count_wild_and_variable_lengths p =
	let val wildCount = ref 0
		val varLen = ref 0
	in
		((g (fn _ => (wildCount := !wildCount + 1; !wildCount)) (fn str => (varLen := !varLen + String.size(str); !varLen)) p); !wildCount + !varLen)
	end			

fun count_some_var (str, p) =
	let val appearCount = ref 0
	in
		((g (fn _ => 0) (fn s => if str = s then (appearCount := !appearCount + 1; !appearCount) else !appearCount) p); !appearCount)	
	end

fun check_pat pat =
	let fun getAllVars (p, acc) =
		case p of
			  Variable v => v::acc
			| TupleP ps => List.foldl (fn (pt,i) => getAllVars(pt, i)) acc ps
			| ConstructorP(_,p) => getAllVars(p, acc)
			| _ => acc
		fun duplicates l =
			case l of
				  [] => true
				| _::[] => true
				| x::xs => if List.exists (fn el => el = x) xs
						   then false
						   else duplicates(xs)
	in
		duplicates(getAllVars(pat, []))
	end



fun match (valu, pattern) =
	let fun match_help (vall, patt, acc) =
		case patt of
		   Wildcard => acc
		 | Variable v => SOME ((v, vall) :: Option.valOf acc)
		 | UnitP => (case vall of
		 		 					  Unit => acc
		 		 					| _ => NONE)
		 | ConstP c => (case vall of
		 		 					Const v => if c = v then acc else NONE
		 		 					| _	 => NONE)
		 | TupleP ps => (case vall of
		 						Tuple vl => if List.length ps = List.length vl
		 									then ListPair.foldl (fn (vl, ps, accLocal) => SOME (Option.valOf (match_help(vl, ps, accLocal)))) acc (vl, ps)
		 									else NONE	
		 						| _ => NONE)
		 | ConstructorP (s1, p) => (case vall of
		 		 								  Constructor (s2, v) => if s1 = s2
		 		 								  						 then match_help(v, p, acc)
		 		 								  						 else NONE	
		 		 								| _ => NONE)
	in
		match_help(valu, pattern, SOME [])
	end		

fun first_match valu pattList =
	SOME (first_answer (fn patt => match(valu, patt)) pattList) handle NoAnswer => NONE

 
fun same_string(s1 : string, s2 : string) =
    s1 = s2

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* problem 1 *)

fun all_except_option(string_option, strList) = 
    case strList of
        [] => NONE
        | x::xs => case same_string(string_option, x) of
                        false => (case all_except_option(string_option, xs) of
                                        NONE => NONE
                                      | SOME resX => SOME (x::resX))
                        | true => SOME xs

fun get_substitutions1(substitutions, name) =
    case substitutions of
          [] => []
        | x::xs => case all_except_option(name, x) of
                                  NONE => get_substitutions1(xs, name)
                                | SOME y => y @ get_substitutions1(xs, name)

fun get_substitutions2(substitutions, name) =
    let fun get_subst(substitutions, acc) =
        case substitutions of
          [] => acc
        | x::xs => case all_except_option(name, x) of
                                  NONE => get_subst(xs, acc)
                                | SOME y => get_subst(xs, y @ acc)
    in
        get_subst(substitutions, [])
    end                                    

(* helper *)
fun construct_names(substitutions, fullName: {first:string,middle:string,last:string}) =
    case substitutions of 
          [] => []
        | x::xs => { first=x, last=(#last fullName), middle=(#middle fullName) } :: construct_names(xs, fullName)   

fun similar_names(substitutions, fullName) =
    case get_substitutions1(substitutions, #first fullName) of
        [] => [fullName]
        | lst => fullName::construct_names(lst, fullName)

(* problem 2 *)

fun card_color(card) =
    case card of
          (Spades, _) => Black
        | (Clubs, _) => Black
        | _ => Red

fun card_value(card) =
    case card of
          (_, Num x) => x
        | (_, Ace) => 11
        | _ => 10 

fun remove_card(cardList, card, e) =
    case cardList of
          [] => raise e
        | x::xs => case (x = card) of
                      true => xs
                    | false => remove_card(xs, card, e)

fun all_same_color(cards) =
    case cards of
          [] => true
        | _::[] => true
        | head::(neck::rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck::rest))

fun sum_cards(cards) =
    let fun sum_helper(crds, acc) = 
            case crds of
                  [] => acc
                | x::xs => sum_helper(xs, acc + card_value(x))
    in
        sum_helper(cards, 0)
    end     

fun score(heldCards, goal) =
    let val cardsSum = sum_cards(heldCards)
        val prelScore = if cardsSum > goal
                        then 3 * (cardsSum - goal)
                        else goal - cardsSum
    in
        if all_same_color(heldCards)
        then prelScore div 2
        else prelScore
    end

fun officiate(cards, moves, goal) = 
    let fun make_move(remainigMoves, remainingCards, heldCards) = 
            case remainigMoves of
                  [] => score(heldCards, goal)
                | x::xs => case x of 
                                  Discard c => make_move(xs, remainingCards, remove_card(remainingCards, c, IllegalMove))
                                | Draw => case remainingCards of
                                                  [] => score(heldCards, goal)
                                                | y::ys => if sum_cards(y::heldCards) > goal
                                                           then score(y::heldCards, goal)
                                                           else make_move(xs, ys, y::heldCards) 
    in
        make_move(moves, cards, [])
    end        



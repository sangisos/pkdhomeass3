
use "helper.sml";

(* split l

*)

fun split [] =  []
  | split cl = List.take(cl,5)::split(List.drop(cl,5))

fun letterToNum c = ord c - ord #"A" + 1 (* A = 1 not 0, hopefully optimized at compile *)

fun numToLetter n = chr (n + ord #"A" - 1) (* A = 1 not 0, hopefully optimized at compile *)

fun enDecLetter opr (x,y) = numToLetter ( ( opr (letterToNum x, letterToNum y) - 1) mod 26 + 1) (* fix for 0 = Z *)

fun enDecrypt opr l = split (List.map (enDecLetter opr) ( ListPair.zip (List.concat l, fakekeystream (length l * 5)) ))

(*
preprocess s
TYPE: string -> char list list
PRE: the string can only contain letter A-Z and the words can be at most 5 characters.
POST: a list containg one list of every character of all the words in the string.
EXAMPLE: preprocess "Live long and prosper!" =
[[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"], [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
*)

fun preprocess s =
    let
        val charlist = List.map Char.toUpper (
                           List.filter Char.isAlpha (explode s))
        val padding = 4 - ((length charlist)-1) mod 5;
        fun padd' 0 (cl:char list) = rev cl
          | padd' n cl = padd' (n-1) (#"X"::cl)
        fun padd cl = padd' padding (rev cl)
    in
        split (padd charlist)
    end;

(* encrypt l
   TYPE: char list list -> char list list
   PRE:  l consists of lists of length exactly 5 containing only letters A-Z.
   POST: l encrypted according to specifications.
*)

val encrypt = enDecrypt op+

(*
decrypt l
TYPE: char list list -> char list list
PRE:lists of length 5 and only containing letters A-Z
POST: l decrypted according to specifications
EXAMPLE:
*)

val decrypt = enDecrypt op-

(* keystream n
   TYPE: int -> char list
   PRE:  n > 0
   POST: the first n elements of the key stream.
*)

datatype card = Card of int | JokerA | JokerB

fun value (Card n) = n
  | value _ = 53

fun keyedDeck' 53 = [JokerA,JokerB]
  | keyedDeck' n = Card(n)::keyedDeck'(n+1)
val keyedDeck = keyedDeck' 1;

fun moveJoker joker steps revFirst last =
    let
        val ll = length last
        val first = rev revFirst
    in
        if steps <= ll then
            first@(List.take(last, steps))@(joker::(List.drop(last, steps)))
        else
            (List.take(first, steps - ll))@(joker::(List.drop(first, steps - ll)@last))
    end;

fun moveJokerADownOneCard' revFirst (JokerA::last) = moveJoker JokerA 1 revFirst last
  | moveJokerADownOneCard' revFirst (card::last) = moveJokerADownOneCard' (card::revFirst) last;
val moveJokerADownOneCard = moveJokerADownOneCard' [];


fun moveJokerBDownTwoCards' revFirst (JokerB::last) = moveJoker JokerB 2 revFirst last
  | moveJokerBDownTwoCards' revFirst (card::last) = moveJokerBDownTwoCards' (card::revFirst) last;
val moveJokerBDownTwoCards = moveJokerBDownTwoCards' [];

fun splitDeck' buf last ((card as Card(_))::deck) = splitDeck' (card::buf) last deck
  | splitDeck' buf last (j::deck) =
    if null last andalso not (null buf) then
        splitDeck' [j] (rev buf) deck
    else
        deck@(rev (j::buf))@last
val splitDeck = splitDeck' [] [];

fun countCut deck =
    let
        val lc = List.last deck
        val lv = value lc;
        val first = List.drop(deck, lv)
        val lf = length first
    in
        List.take(List.drop(deck, lv), lf-1)@List.take(deck, lv)@[lc]
    end;

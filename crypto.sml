
val noOfCards = 52;
val chunkSize = 5;

(* split l
   TYPE: a' list -> a' list
   PRE:  chunkSize must be a devisor in length l
   POST: a list splitted in in several lists of chunkSize elements.
   VARIANT: length l
*)

fun split [] =  []
  | split cl = List.take(cl,chunkSize)::split(List.drop(cl,chunkSize))

(*  letterToNum c
    TYPE: char -> int
    PRE: #"A" <= c <= #"Z"
    POST: the number of c in the alphabet.
*)

fun letterToNum c = ord c - ord #"A" + 1 (* A = 1 not 0, hopefully optimized at compile *)

(*  numToLetter n
    TYPE: int -> char
    PRE: n > 0
    POST: the n:th letter of the alphabet, where n rolls around to 1 after 26.
*)

fun numToLetter n = chr ( (n-1) mod 26 + ord #"A") (* A = 1 not 0, hopefully optimized at compile *)

(* preprocess s
   TYPE: string -> char list list
   PRE:  true
   POST: a list of chunkSize character lists containg every alpha character of
         the string s in uppercase.
   EXAMPLE: With chunkSize=5 : preprocess "Live long and prosper!" =
      [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
         [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
*)

fun preprocess s =
    let
        (* preprocess' chunkSize chunk full cl
           TYPE: int -> char list -> char list list -> char list
                 -> char list list
           PRE:  chunkSize > 0
           POST: a list of 5 character lists containg every alpha
                 character of the string s in uppercase.
           EXAMPLE: preprocess "Live long and prosper!" =
           [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
           [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
           VARIANT: first cl then chunkSize
         *)
        fun preprocess' _ [] full [] = rev full
          | preprocess' 0 chunk full [] = rev ((rev chunk)::full)
          | preprocess' n chunk full [] = preprocess' (n-1) (#"X"::chunk) full []
          | preprocess' 0 chunk full cl = preprocess' chunkSize [] ((rev chunk)::full) cl
          | preprocess' n chunk full (c::cl) =
            if (Char.isAlpha c) then
                preprocess' (n-1) ((Char.toUpper c)::chunk) full cl
            else
                preprocess' n chunk full cl
    in
        preprocess' chunkSize [] [] (explode s)
    end

(*  REPRESENTATION CONVENTION: a deck of cards with cards with a int and two jokers
    REPRESENTATION INVARIANT: 
*)

datatype card = Card of int | JokerA | JokerB

(*  value
    TYPE: card -> int
    PRE: true
    POST: the int of a card or 53 if card is a joker
*)

fun value (Card n) = n
  | value _ = noOfCards+1

(*  keyedDeck' n
    TYPE: int -> card list
    PRE: n >= 0
    POST: a keyed deck of cards from the card n until card 53.
    VARIANT: n = 0
*)

fun keyedDeck' buf 0 = buf@[JokerA,JokerB]
  | keyedDeck' buf n = keyedDeck' ((Card(n))::buf) (n-1)
val keyedDeck = keyedDeck' [] noOfCards;

(*  moveJoker joker, steps, revFirst, last
    TYPE: fn: 'a -> int -> 'a list -> 'a list -> 'a list
    PRE:  0 < steps <= noOfCards+1
    POST: A deck of cards with joker moved from the gap between reversed
          revFirst and last, to steps cards down in the deck consisting of the
          reversed revFirst concatenated with last. joker will cycle around to
          the top of reversed revFirst after the end of last.
*)

fun moveJoker joker steps revFirst last =
    let
        val ll = length last
        val first = rev revFirst
    in
        if steps <= ll then
            first@((List.take(last, steps))@(joker::(List.drop(last, steps))))
        else
            (List.take(first, steps - ll))@(joker::(List.drop(first, steps - ll)@last))
    end;

(*  moveJokerADownOneCard' revFirst l
    TYPE: card list -> card list -> card list
    PRE: true
    POST: the joker A moved one step down in a deck of cards.
    VARIANT: length l
*)

fun moveJokerADownOneCard' revFirst (JokerA::last) = moveJoker JokerA 1 revFirst last
  | moveJokerADownOneCard' revFirst (card::last) = moveJokerADownOneCard' (card::revFirst) last;
val moveJokerADownOneCard = moveJokerADownOneCard' [];

(*  moveJokerBDownTwoCards' l
    TYPE: card list -> card list
    PRE: true
    POST: the joker B moved two steps in a deck of cards.
    VARIANT: JokerB in list l
*)

fun moveJokerBDownTwoCards' revFirst (JokerB::last) = moveJoker JokerB 2 revFirst last
  | moveJokerBDownTwoCards' revFirst (card::last) = moveJokerBDownTwoCards' (card::revFirst) last;
val moveJokerBDownTwoCards = moveJokerBDownTwoCards' [];

(*  tripleCut' last joker1 middle first
    TYPE: fn: card list -> card list -> card list -> card list -> card list
    PRE:  if the first joker in the deck is in joker1, first is a deck with one
          joker else first is a deck with two jokers.
    POST: the deck first@joker1@(rev middle)@(rev last) with all cards on top
          of first joker swapped with all cards below second joker.
    VARIANT: first
*)

fun tripleCut' last [] [] ((card as (Card _))::first) = tripleCut' (card::last) [] [] first
  | tripleCut' last [] [] (joker::first) = tripleCut' last [joker] [] first
  | tripleCut' last joker1 middle ((card as (Card _))::first) = tripleCut' last joker1 (card::middle) first
  | tripleCut' last [joker1] middle (joker::first) = first@((joker1::(rev middle))@(joker::(rev last)))
val tripleCut = tripleCut' [] [] [];

(*  countCut deck
    TYPE: card list -> card list
    PRE: true
    POST: the deck with the bottom card's value in cards off the top of the deck,
          moved to just above the bottom card.
*)

fun countCut deck =
    let
        val lc = List.last deck
        val lv = value lc;
        val first = List.drop(deck, lv)
        val lf = length first
    in
        List.take(first, lf-1)@List.take(deck, lv)@[lc]
    end;

exception Joker

(* findOutputLetter deck
   TYPE: card list -> char
   PRE: true
   POST: the value of the card at the place of the value of the top card in deck deck
   EXCEPTION: raises Joker when the card found is a joker.
*)

fun findOutputLetter deck =
    case (List.nth (deck,value (hd deck))) (* behövs ingen fix då 0-räkning och vi ska ha kortet EFTER *)
     of (Card n) => numToLetter n
      | _ => raise Joker;

(* keystream n
   TYPE: int -> char list
   PRE:  n > 0
   POST: the first n elements of the key stream.
   VARIANT: n (even tought n does not decrease when findOutputLetter raises joker).
*)

fun keystream' deck 0 = []
  | keystream' deck n =
    let
        val cutDeck = (countCut (tripleCut (moveJokerBDownTwoCards (moveJokerADownOneCard deck))))
    in
        findOutputLetter(cutDeck)::(keystream' cutDeck (n-1))
        handle Joker =>  keystream' cutDeck (n) (* Do it again on the cut deck on same n. *)
    end;
val keystream = keystream' keyedDeck;

(*  enDecLetter opr (x,y)
    TYPE: fn: (int * int -> int) -> char * char -> char
    PRE: true
    POST: applies the input function opr on x,y
*)

fun enDecLetter opr (x,y) = numToLetter ( ( opr (letterToNum x, letterToNum y) - 1) mod 26 + 1) (* fix for 0 = Z *)

(*  enDecrypt opr l
    TYPE: fn: (int * int -> int) -> char list list -> char list list
    PRE: true
    POST: applies the input function opr on list l
*)

fun enDecrypt opr l = ListPair.map (ListPair.map (enDecLetter opr)) ( l, split(keystream (length l * chunkSize)) )

(* encrypt l
   TYPE: char list list -> char list list
   PRE:  l consists of lists of length exactly 5 containing only letters A-Z.
   POST: l encrypted according to specifications.
*)

val encrypt = enDecrypt op+

(*  decrypt l
    TYPE: char list list -> char list list
    PRE:  l is a list of lists each only containing 5 characters A-Z
    POST: l decrypted according to specifications
    EXAMPLE:
*)

val decrypt = enDecrypt op-

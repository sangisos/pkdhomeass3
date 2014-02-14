
val noOfCards = 52;
val chunkSize = 5;

exception JokerNotFound
exception Joker


(* REPRESENTATION CONVENTION: A playing card:
               Card n -> the card with value n
               JokerA -> the first joker
               JokerB -> the second joker
   REPRESENTATION INVARIANT: n > 0
*)
datatype card = Null | Card of int | JokerA | JokerB

(* split l
   TYPE: a' list -> a' list
   PRE:  chunkSize must be a divisor in length l
   POST: a list split in in several lists of chunkSize elements.
   VARIANT: length l
*)

fun split [] =  []
  | split cl = List.take(cl,chunkSize)::split(List.drop(cl,chunkSize))

(*  letterToNum c
    TYPE: char -> int
    PRE: #"A" <= c <= #"Z"
    POST: the position of c in the alphabet.
*)

fun letterToNum c = ord c - ord #"A" + 1 (* A = 1 not 0, hopefully optimised at compile *)

(*  numToLetter n
    TYPE: int -> char
    PRE: n > 0
    POST: the n:th letter of the alphabet, where n rolls around to 1 after 26.
*)

fun numToLetter n = chr ( (n-1) mod 26 + ord #"A") (* A = 1 not 0, hopefully optimised at compile *)

(* preprocess s
   TYPE: string -> char list list
   PRE:  true
   POST: a list of chunkSize character lists containing every alpha character of
         the string s in uppercase.
   EXAMPLE: With chunkSize=5 : preprocess "Live long and prosper!" =
      [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
         [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
*)

fun preprocess s =
    let
        (* preprocess' n chunk full cl
           TYPE: int -> char list -> char list list -> char list
                 -> char list list
           PRE:  n = chunkSize - (length chunk), length chunk <= chunkSize.
           POST: a list of character lists of length chunkSize containing every
                 alpha character in uppercase of the string s in order, padded
                 with (~(length cl) mod chunkSize) #"X":s at end of last chunk.
           EXAMPLE: preprocess "Live long and prosper!" =
           [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
           [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
           VARIANT: first length cl, when cl empty then n
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

(* value c
   TYPE: card -> int
   PRE:  true
   POST: value of card c
*)

fun value (Card n) = n
  | value _ = noOfCards+1

(* keyedDeck' n
   TYPE: int -> card list
   PRE: n >= 0
   POST: a keyed deck of cards from 1 to noOfCards and JokerA and JokerB at end.
   VARIANT: n
*)

val keyedDeck = ref (Vector.tabulate (noOfCards+3,fn x => ref (Card x)));
(Vector.sub((!keyedDeck),noOfCards+1)) := JokerA;
(Vector.sub((!keyedDeck),noOfCards+2)) := JokerB;
(* moveJoker joker, steps, revFirst, last
   TYPE: 'a -> int -> 'a list -> 'a list -> 'a list
   PRE:  0 < steps <= noOfCards+1
   POST: A deck of cards with joker moved from the gap between reversed
         revFirst and last, to steps cards down in the deck consisting of the
         reversed revFirst concatenated with last. joker will cycle around to
         the top of reversed revFirst after the end of last.
*)

fun cardTup n = (Vector.sub(!keyedDeck,n),n)
            handle Subscript => (ref Null,n)

val firstCard = cardTup(1)
val secondCard = cardTup(2)
val cardBeforeJoker1 = cardTup(noOfCards)
val joker1 = cardTup(noOfCards+1)
val cardAfterJoker1 = cardTup(noOfCards+2)
val card2AfterJoker1 = cardTup(noOfCards+3)
val cardBeforeJoker2 = cardTup(noOfCards+1)
val joker2 = cardTup(noOfCards+2)
val cardAfterJoker2 = cardTup(noOfCards+3)
val card2AfterJoker2 = cardTup(noOfCards+4)
val secondToLastCard = cardTup(noOfCards+1)
val lastCard = cardTup(noOfCards+2)

fun cutDeck () = ();


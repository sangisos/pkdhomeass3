
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
datatype card = Card of int | JokerA | JokerB

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

fun keyedDeck' buf 0 = buf@[JokerA,JokerB]
  | keyedDeck' buf n = keyedDeck' ((Card(n))::buf) (n-1)
val keyedDeck = keyedDeck' [] noOfCards;

(* moveJoker joker, steps, revFirst, last
   TYPE: 'a -> int -> 'a list -> 'a list -> 'a list
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

(* moveJokerADownOneCard' revFirst rest
   TYPE: card list -> card list -> card list
   PRE:  revFirst is the reversed first part of the deck, rest is the last part
         of the deck containing JokerA
   POST: the deck (rev revFirst)@rest with JokerA moved one step down.
   EXCEPTION: raises JokerNotFound if JokerA was not found in the deck.
   VARIANT: length rest
*)

fun moveJokerADownOneCard' revFirst (JokerA::rest) = moveJoker JokerA 1 revFirst rest
  | moveJokerADownOneCard' revFirst (card::rest) = moveJokerADownOneCard' (card::revFirst) rest
  | moveJokerADownOneCard' _ [] = raise JokerNotFound
val moveJokerADownOneCard = moveJokerADownOneCard' [];

(* moveJokerBDownTwoCards' revFirst rest
   TYPE: card list -> card list -> card list
   PRE:  revFirst is the reversed first part of the deck, rest is the last part
         of the deck containing JokerB
   POST: the deck (rev revFirst)@rest with JokerB moved two steps down.
   EXCEPTION: raises JokerNotFound if JokerB was not found in the deck.
   VARIANT: length rest
*)

fun moveJokerBDownTwoCards' revFirst (JokerB::rest) = moveJoker JokerB 2 revFirst rest
  | moveJokerBDownTwoCards' revFirst (card::rest) = moveJokerBDownTwoCards' (card::revFirst) rest
  | moveJokerBDownTwoCards' _ [] = raise JokerNotFound 
val moveJokerBDownTwoCards = moveJokerBDownTwoCards' [];

(* tripleCut' last joker1 middle first
   TYPE: card list -> card list -> card list -> card list -> card list
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
  | tripleCut' _ _ _ _ = raise JokerNotFound
val tripleCut = tripleCut' [] [] [];

(* countCut deck
   TYPE: card list -> card list
   PRE:  length deck > max value of cards in deck
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

(* findOutputLetter deck
   TYPE: card list -> char
   PRE:  length deck > max value of cards in deck
   POST: the value of the card at the place of the value of the top card in deck deck
   EXCEPTION: raises Joker when the card found is a joker.
*)

fun findOutputLetter deck =
    case (List.nth (deck,value (hd deck))) (* no fix needed as List.nth indexes from 0 and we want the card at position (value(hd deck) + 1) *)
     of (Card n) => numToLetter n
      | _ => raise Joker;

(* keystream' buf deck n
   TYPE: char list -> card list -> int -> char list
   PRE:  n > 0, deck contains at least 1 regular card and 2 jokers
   POST: the first n elements of the encryption key generated by card crypt.
   VARIANT: n (though n does not decrease when findOutputLetter raises joker).
*)

fun keystream' buf deck 0 = rev buf
  | keystream' buf deck n =
    let
        val cutDeck = (countCut (tripleCut (moveJokerBDownTwoCards (moveJokerADownOneCard deck))))
    in
        keystream' ((findOutputLetter(cutDeck))::buf) cutDeck (n-1)
        handle Joker => keystream' buf cutDeck n (* Do it again on the cut deck on same n. *)
    end;
val keystream = keystream' [] keyedDeck;

(* enDecLetter opr (plainChar,keystreamChar)
   TYPE: (int * int -> int) -> char * char -> char
   PRE:  opr is op+ for encrypt and op- for decrypt.
   POST: the character representing the number resulting of applying opr on
         the numbers representing plainChar and keystreamChar, looping around
         if the result is more then 26 or less then 1.
*)

fun enDecLetter opr (plainChar,keystreamChar) = numToLetter ( ( opr (letterToNum plainChar, letterToNum keystreamChar) - 1) mod 26 + 1) (* - 1,+ 1 fix for 0 =/= Z *)

(* enDecrypt opr l
   TYPE: (int * int -> int) -> char list list -> char list list
   PRE:  opr is op+ for encrypt and op- for decrypt. l is split in chunks of 
         chunkSize.
   POST: l encrypted if opr is op+, l decrypted if opr is op-.
*)

fun enDecrypt opr plainTextChunks = ListPair.map (ListPair.map (enDecLetter opr)) ( plainTextChunks, split(keystream (length plainTextChunks * chunkSize)) )

(* encrypt l
   TYPE: char list list -> char list list
   PRE:  l consists of lists of length chunkSize containing only letters A-Z.
   POST: l encrypted according to specifications.
   EXAMPLE: 
     encrypt [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
       [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]] =
     [[#"P", #"F", #"F", #"C", #"T"], [#"N", #"F", #"M", #"E", #"U"],
        [#"X", #"C", #"K", #"W", #"I"], [#"K", #"Z", #"J", #"V", #"H"]];
*)

val encrypt = enDecrypt op+

(* decrypt l
   TYPE: char list list -> char list list
   PRE:  l consists of lists of length chunkSize containing only letters A-Z.
   POST: l decrypted according to specifications
   EXAMPLE:
     decrypt [[#"P", #"F", #"F", #"C", #"T"], [#"N", #"F", #"M", #"E", #"U"],
       [#"X", #"C", #"K", #"W", #"I"], [#"K", #"Z", #"J", #"V", #"H"]] =
     [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
       [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]];
*)

val decrypt = enDecrypt op-

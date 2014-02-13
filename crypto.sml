
(* split l
   TYPE: a' list -> a' list
   PRE: true
   POST: a list splitted in in several lists of five elements.
*)

fun split [] =  []
  | split (i1::i2::i3::i4::i5::cl) = ([i1,i2,i3,i4,i5])::split(cl)

(*  letterToNum c
    TYPE: char -> int
    PRE: true
    POST: the value of c
*)

fun letterToNum c = ord c - ord #"A" + 1 (* A = 1 not 0, hopefully optimized at compile *)

(*  numToLetter n
    TYPE: int -> char
    PRE: true
    POST: the char connected to the value of n.
*)

fun numToLetter n = chr ( (n-1) mod 26 + ord #"A") (* A = 1 not 0, hopefully optimized at compile *)

(* preprocess s
   TYPE: string -> char list list
   PRE:  true
   POST: a list of 5 character lists containg every alpha character of the
         string s in uppercase.
   EXAMPLE: preprocess "Live long and prosper!" =
      [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
         [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
   VARIANT: s
*)
fun preprocess' _ [] full [] = rev full
  | preprocess' 0 chunk full [] = rev ((rev chunk)::full)
  | preprocess' n chunk full [] = preprocess' (n-1) (#"X"::chunk) full []
  | preprocess' 0 chunk full cl = preprocess' 5 [] ((rev chunk)::full) cl
  | preprocess' n chunk full (c::cl) =
    if (Char.isAlpha c) then
        preprocess' (n-1) ((Char.toUpper c)::chunk) full cl
    else
        preprocess' n chunk full cl
fun preprocess s = preprocess' 5 [] [] (explode s)

(* keystream n
   TYPE: int -> char list
   PRE:  n > 0
   POST: the first n elements of the key stream.
*)

datatype card = Card of int | JokerA | JokerB

(*  value
    TYPE: card -> int
    PRE: true
    POST: the int of a card or 53 if card is a joker
*)

fun value (Card n) = n
  | value _ = 53

(*  keyedDeck' n
    TYPE: int -> card list
    PRE: n <= 53
    POST: a keyed deck of cards from the card n until card 53.
*)

fun keyedDeck' 53 = [JokerA,JokerB]
  | keyedDeck' n = Card(n)::keyedDeck'(n+1)
val keyedDeck = keyedDeck' 1;

(*  moveJoker joker, steps, revFirst, last
    TYPE: fn: 'a -> int -> 'a list -> 'a list -> 'a list
    PRE: true
    POST: A deck of cards with the joker inserted in the place steps in the list reversed revFirst concatinated with last.
*)

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

(*  moveJokerADownOneCard' 
    TYPE: card list -> card list
    PRE: true
    POST: the joker A moved one step down in a deck of cards.
*)

fun moveJokerADownOneCard' revFirst (JokerA::last) = moveJoker JokerA 1 revFirst last
  | moveJokerADownOneCard' revFirst (card::last) = moveJokerADownOneCard' (card::revFirst) last;
val moveJokerADownOneCard = moveJokerADownOneCard' [];

(*  moveJokerBDownTwoCards'
    TYPE: card list -> card list
    PRE: true
    POST: the joker B moved two steps in a deck of cards.
*)

fun moveJokerBDownTwoCards' revFirst (JokerB::last) = moveJoker JokerB 2 revFirst last
  | moveJokerBDownTwoCards' revFirst (card::last) = moveJokerBDownTwoCards' (card::revFirst) last;
val moveJokerBDownTwoCards = moveJokerBDownTwoCards' [];

(*  tripleCut' buf last deck
    TYPE: fn: card list -> card list -> card list -> card list
    PRE: true
    POST:
*)

fun tripleCut' buf last ((card as Card(_))::deck) = tripleCut' (card::buf) last deck
  | tripleCut' buf last (j::deck) =
    if null last andalso not (null buf) then
        tripleCut' [j] (rev buf) deck
    else
        deck@(rev (j::buf))@last
val tripleCut = tripleCut' [] [];

(*  countCut deck
    TYPE: card list -> card list
    PRE: true
    POST: 
*)

fun countCut deck =
    let
        val lc = List.last deck
        val lv = value lc;
        val first = List.drop(deck, lv)
        val lf = length first
    in
        List.take(List.drop(deck, lv), lf-1)@List.take(deck, lv)@[lc]
    end;

exception Joker

(* findOutputLetter deck
   TYPE: card list -> char
   PRE: true
   POST: 
*)

fun findOutputLetter deck =
    case (List.nth (deck,value (hd deck))) (* behövs ingen fix då 0-räkning och vi ska ha kortet EFTER *)
     of (Card n) => numToLetter n
      | _ => raise Joker;

fun keystream' deck 0 = []
  | keystream' deck n =
    let
        val cutDeck = (countCut (tripleCut (moveJokerBDownTwoCards (moveJokerADownOneCard deck))))
    in
        findOutputLetter(cutDeck)::(keystream' cutDeck (n-1))
        handle Joker =>  keystream' cutDeck (n) (* Do it again on the cut deck on same n. *)
    end;
val keystream = keystream' keyedDeck;

(*  enDecLetter
    TYPE: fn: (int * int -> int) -> char * char -> char
    PRE:
    POST:
*)

fun enDecLetter opr (x,y) = numToLetter ( ( opr (letterToNum x, letterToNum y) - 1) mod 26 + 1) (* fix for 0 = Z *)

(*  enDecrypt
    TYPE: fn: (int * int -> int) -> char list list -> char list list
    PRE: 
    POST:
*)

fun enDecrypt opr l = split (List.map (enDecLetter opr) ( ListPair.zip (List.concat l, keystream (length l * 5)) ))

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

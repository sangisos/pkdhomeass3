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
        fun split [] =  []
          | split cl = List.take(cl,5)::split(List.drop(cl,5))
    in
        split (padd charlist)
    end;

(* encrypt l
   TYPE: char list list -> char list list
   PRE:  l consists of lists of length exactly 5 containing only letters A-Z.
   POST: l encrypted according to specifications.
*)

fun ecrypt' [] = []

(*
decrypt l
TYPE: char list list -> char list list
PRE:lists of length 5 and only containing letters A-Z
POST: l decrypted according to specifications
EXAMPLE:
*)

(* keystream n
   TYPE: int -> char list
   PRE:  n > 0
   POST: the first n elements of the key stream.
*)


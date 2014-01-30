(*
Training test cases for PKD assignment 3 (Cryptography), HT 13
Developed by Dave Clarke

To run these training cases:
   1) launch PolyML shell [poly]
   2) load hand-in [use "crypto.sml"]
   3) load training set [use "assignment3_tests.sml"]
   4) run tests [test ()]
*)


(******************************************************************************)
(* To run the code below, the functions preprocess, encrypt, decrypt and      *)
(* keystream must be declared in cryptosml and have the correct type.         *)
(******************************************************************************)


(* let's make sure these functions are in fact declared and have the correct type *)
preprocess: string -> char list list;
encrypt: char list list -> char list list;
decrypt: char list list -> char list list;
keystream : int -> char list;



(* result (n, f)
   TYPE: int * (int -> bool) -> unit
   PRE:  f is well-defined (i.e., terminates without error) for the call f n
   POST: ()
   SIDE-EFFECTS: Prints a report stating whether test n was successful or not
                 (where f n = true iff the test was successful)
*)
fun result (n, f) =
    print ("Test #" ^ Int.toString n ^ 
	   ((if f n then " successful." else " FAILED!")
	    handle _ =>
		   " raised an (unwanted) exception!") ^ "\n");

(* test ()
   TYPE: unit -> unit
   PRE:  true
   POST: ()
   SIDE-EFFECTS: Prints a report, stating whether each test case performed as
                 expected.
 *)
fun test () =
    let
	(* test n
       TYPE: int -> bool
       PRE:  1<=n<=4
       POST: true iff test n executes correctly
	 *)
	fun test 1 =
	    let
		val input1  = "@#@*(H@#$@#()e@#)9$)l$#@p!!"
		val output1 = [[#"H", #"E", #"L", #"P", #"X"]]
		val input2  = "Live long and prosper!"
		val output2 = [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
			       [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
		val input3  = "Ample Juice"
		val output3 = [[#"A", #"M", #"P", #"L", #"E"], [#"J", #"U", #"I", #"C", #"E"]]
		val input4  = ""
		val output4 = []
	    in
		preprocess input1 = output1 andalso
		preprocess input2 = output2 andalso
		preprocess input3 = output3 andalso
		preprocess input4 = output4
	    end
	  | test 2 =
	    let
		val input1 =  [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
			       [#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
		val output1 =  [[#"P", #"F", #"F", #"C", #"T"], [#"N", #"F", #"M", #"E", #"U"],
				[#"X", #"C", #"K", #"W", #"I"], [#"K", #"Z", #"J", #"V", #"H"]]
		val input2 = []
		val output2 = []
	    in
		encrypt input1 = output1 andalso
		encrypt input2 = output2
	    end
	  | test 3 = 
	    let
		val input1 =  [[#"P", #"F", #"F", #"C", #"T"], [#"N", #"F", #"M", #"E", #"U"],
			       [#"X", #"C", #"K", #"W", #"I"], [#"K", #"Z", #"J", #"V", #"H"]]
		val output1 =  [[#"L", #"I", #"V", #"E", #"L"], [#"O", #"N", #"G", #"A", #"N"],
				[#"D", #"P", #"R", #"O", #"S"], [#"P", #"E", #"R", #"X", #"X"]]
		val input2 = []
		val output2 = []
	    in
		decrypt input1 = output1 andalso
		decrypt input2 = output2
	    end
	  | test 4 =
	    keystream 0 = [] andalso
	    keystream 7 =  [#"D", #"W", #"J", #"X", #"H", #"Y", #"R"]
	  | test _ = raise Domain
    in
      List.app result [(1, test), (2, test), (3, test), (4, test)]
    end;



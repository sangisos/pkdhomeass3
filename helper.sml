
val format = (foldl (fn (s, r) => s ^ " " ^ r) "") o map String.implode

fun fakekeystream n = 
    let val fakelist = String.explode "DWJXHYRFDGTMSHPUURXJ"  
    in 
	List.take (fakelist, n)
    end


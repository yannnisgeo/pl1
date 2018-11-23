
fun fair_parts(fileName) = 
let

	fun parse file =
		let
		(* a function to read an integer from an input stream *)
			fun next_int input =
			Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) input)
		(* open input file and read the two integers in the first line *)
			val stream = TextIO.openIn file
			val M = next_int stream
			val N = next_int stream
			val MM = IntInf.toLarge(M)
			val NN = IntInf.toLarge(N)
		val _ = TextIO.inputLine stream
		(* a function to read the pair of integer & real in subsequent lines *)
			fun scanner 0 acc = acc
			  | scanner i acc =
				let
					val d = next_int stream
					val dd = IntInf.toLarge(d)
				in
					scanner (i - 1) (dd :: acc)
				end
		in
			(MM, NN, rev(scanner MM []))
		end

	val (MM, NN, LLIST) = parse fileName

	fun fair (M, N, MainList) = 
	let
		

		val MINIMUM = LargeInt.toLarge(1)
		val MAXIMUM = LargeInt.toLarge(M)*LargeInt.toLarge(10000000)
		fun check (w) = 
		let
			fun checkrem([], Nrem, current) = true
			|	checkrem(_, 0, current) = false 
			|	checkrem((r::rs), Nrem, current) = if (current + r > w) then checkrem((r::rs), Nrem-1, 0) else checkrem(rs, Nrem, current+r)
		in
			checkrem(MainList, N, 0)
		end
		
		fun minmax(down, up) = if down = up then down else
		let
			val middle = ((up+down) div 2)
		in
			if check(middle) then minmax(down, middle) else minmax(middle+1, up)
		end
		
		fun fix (w) = 
		let
			fun element ([], _, _, currentList, _) = currentList
			|   element((r::rs), Nrem, current, currentList, Mrem) =  
				
				if Nrem=Mrem then element(rs, Nrem-1, r, (  LargeInt.toString(r)::("|"::currentList)), Mrem-1) 

				else if current+r>w then element(rs, Nrem-1, r, 
				(LargeInt.toString(r)::("|"::currentList)), Mrem-1) 
				
				else element(rs, Nrem, current+r, 
				(LargeInt.toString(r)::currentList), Mrem-1)												
		in
			element((rev (MainList)), N-1, 0, [], M)
		end
	in 
		let
			val best = minmax(MINIMUM, MAXIMUM)
			val resultlist = fix(best)
			fun prinspa (a,b) = a^" "^b
			fun print (lista) = foldr prinspa "" lista
		in
			print (resultlist)
		end
	end
in
	fair (MM, NN, LLIST)
end
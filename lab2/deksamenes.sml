fun deksamenes inputfile =
let
	fun parse file =
	let
	(* a function to read an integer from an input stream *)
		fun next_int input =
		Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) input)
	(* open input file and read the two integers in the first line *)
		val stream = TextIO.openIn file
		fun mergesort [] = []
		|	mergesort [x] = [x]
		|	mergesort L =
		let
			fun halve [] = ([],[])
			|	halve [a] = ([a],[])
			|	halve (a::(b::cs)) = 
			let
				val (aa, bb) = halve cs
			in
				(a::aa, b::bb)
			end
			val (Lleft, Lright) = halve L
			val Lleftsorted = mergesort Lleft
			val Lrightsorted = mergesort Lright
			fun bigger ((Ha, _), (Hb, _)) = if Ha > Hb then true else false
			fun merge ([], L) = L
			|	merge (L, []) = L
			|	merge ((l::ls), (r::rs)) = if bigger (r,l) then (l::(merge(ls,(r::rs)))) else (r::(merge((l::ls), rs)))
		in
			merge (Lleftsorted, Lrightsorted)
		end 
	(* a function to read the pair of integer & real in subsequent lines *)
		fun scanner 0 acc = acc
		|	scanner i acc =
		let
			val b = next_int stream			
			val h = next_int stream
			val w = next_int stream
			val l = next_int stream
		in
			scanner (i-1) ((b, w*l)::((b+h, ~1*w*l)::acc))
		end
	in
		let 
			val Ninp = next_int stream
			val _ = TextIO.inputLine stream
			val InputList = scanner Ninp []
			val Volume = next_int stream
		in
			(Volume, IntInf.toLarge(0), IntInf.toLarge(0), mergesort(InputList))
		end
	end

	fun answer hlast V L = if L = 0 then Real.fromLargeInt(hlast) else Real.fromLargeInt(hlast)+real(round(Real.fromLargeInt(V)*100.0/Real.fromLargeInt(L)))/100.0
	
	
	fun find (V, L, h, []) = if V > 0 then ~1.0 else Real.fromLargeInt(h) 
	|	find (V, L, hlast, (h, e)::rest) = if L*(h-hlast) < V then find (V-L*(h-hlast), L+e, h, rest) else (answer hlast V L)
in
	find (parse inputfile)
end


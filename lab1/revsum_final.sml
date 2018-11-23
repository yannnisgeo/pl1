fun revsum fileName = 
let
	fun parse file =
	  let
		val input = TextIO.openIn file

		fun read_digits (NONE, acc) = acc (* end of stream *)
		  | read_digits (SOME ch, acc) =
			let
			  val str = Char.toString ch
			in
			  case (Int.fromString str) of
				  SOME digit => read_digits (TextIO.input1 input, digit::acc)
				| NONE => acc (* return on first non-digit *)
			end
	  in
		rev (read_digits (TextIO.input1 input, [])) 
	end

	fun element ([], [], A, B, C, D) = (A, B, C, D)
	|	element ((l::ls), (r::rs), lcarry, rcarry, crleft, crright) = 
	if lcarry = false andalso rcarry = false then 
		if l=r then element(ls, rs, false, false, l::crleft, 0::crright)
		else if l=((r+1) mod 10) then element(ls, rs, true, false, r::crleft, 0::crleft)
		else (false, false, [], [])
	else if lcarry = false andalso rcarry = true then 
		if l=r then if l > 0 then element(ls, rs, true, false, ((l-1) mod 10)::crleft, 0::crright)
					else element(ls, rs, false, true, 0::crleft, 9::crright)
		else if l=((r-1) mod 10) then element(ls, rs, false, false, l::crleft, 0::crright)
		else (false, false, [], [])
	else if lcarry = true andalso rcarry = false then 
		if l=r then if l<9 then element(ls, rs, false, true, 9::crleft, (l+1)::crright)
					else (false, false, [], [])
		else 
		if l=((r+1) mod 10) then if l>0 then element(ls, rs, true, true, 9::crleft, r::crright)
							else (false, false, [], [])
		else (false, false, [], [])
	else if lcarry = true andalso rcarry = true then
		if l=r then element(ls, rs, true, true, 9::crleft, l::crright)
		else
		if l=((r-1) mod 10) then if l<9 then element(ls, rs, false, true, 9::crleft, (l+1)::crright)
							else (false, false, [], [])
		else (false, false, [], [])
	else (false, false, [], [])
	|	element(_,_,_,_,_,_) = (true, true, [], []) 

	fun head [] = 11
	|	head (r::rs) = r

	fun finalcheck (lcarry, rcarry, L, R, middle) = 
	if middle = 11 then
		if lcarry = rcarry then ((rev L) @ R) else []
	else
		if lcarry = false andalso rcarry = false then 
			if (middle mod 2) = 0 then ((rev L) @ ((middle div 2)::R)) else []
		else
		if lcarry = false andalso rcarry = true then
			if (middle mod 2) = 1 then ((rev L) @ ((middle div 2)::R)) else []
		else
		if lcarry = true andalso rcarry = false then
			if (middle mod 2) = 0 then ((rev L) @ (((middle div 2)+5)::R)) else []
		else
		if lcarry = true andalso rcarry = true then
			if (middle mod 2) = 1 then ((rev L) @ (((middle div 2)+5) :: R)) else []
		else []

	fun wholecarry (GnputList, carry) =
	let
		val (y::MedianList) = GnputList
		val InputList = if carry then MedianList else GnputList
	in
	if carry=false orelse y=1 then
		let
			val leng = length (InputList)
			val midscan = (leng div 2)
			val middle = if (leng mod 2) = 0 then 11 else
			let
				fun findmiddle (l::ls, 0) = l
				|	findmiddle (l::ls, rem) = findmiddle (ls, rem-1)
				|	findmiddle (_, _) = 12
			in
				findmiddle (InputList, midscan)
			end
			val (L, R) = 
			let
				fun sepermid (ls, 0, forming) = if middle=11 then (forming, ls) else let val (y::ys) = ls in (forming, ys) end
				|	sepermid (l::ls, rem, forming) = sepermid (ls, rem-1, l::forming)
				|	sepermid (_,_,_) = ([],[])
			in
				sepermid(InputList, midscan, [])
			end
		in
			let
				val (lcarry, rcarry, Lan, Ran) = element ((rev L), (rev R), carry, false, [], [])
			in
				if Lan=[] andalso Ran=[] andalso L<>[] andalso R<>[] then "0" else
				let
					val Terminal = finalcheck(lcarry, rcarry, Lan, Ran, middle)
					fun printfin ([], cur) = cur
					|	printfin ((x::xs), cur) = printfin(xs, cur^(Int.toString(x)))
				in
					if Terminal=[] then "0" else printfin(Terminal, "")
				end
			end
		end
	else "0"
	end

	fun finalfunc (InputList) = 
	let
		val Answer = wholecarry(InputList, false)
	in
		if Answer="0" then wholecarry(InputList, true) else Answer 
	end
in
	finalfunc (parse fileName)
end
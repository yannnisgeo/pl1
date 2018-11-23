fun dominos File = 
let
	fun parse file =
	let
		fun next_int input =
			Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

		val stream = TextIO.openIn file
		
		
		fun readaline 0 sofar = rev sofar
		|	readaline j sofar = 
		let
			val element = next_int stream
		in
			readaline (j-1) (element::sofar)
		end
		fun readall 0 sofar = rev sofar 
		|	readall i sofar = readall (i-1) ((readaline 8 [])::sofar) 
	in
		readall 7 []
	end


	fun takeone(l::ls, 1) = l
	|	takeone(l::ls, n) = takeone(ls, n-1)

	fun take(Sit, (i, j)) = if (i > 7 orelse j > 8) then ~1 else
	let
		val row = takeone(Sit, i)
	in
		takeone(row, j)
	end

	fun nextsquare (i, j) = if (j<8 andalso i<8) then (i, j+1) else if i < 7 then (i+1, 1) else (~1, ~1)



	fun neighhor (i, j) = if j = 8 then (~1, ~1) else (i, j+1)
	fun neighver (i, j) = if i = 7 then (~1, ~1) else (i+1, j)


	fun takeout([], normal) = []
	|	takeout(u::us, normal) = if u=normal then us else (u::(takeout(us, normal)))

	fun checkifin([], normal) = false
	|	checkifin(l::ls, normal) = if l=normal then true else checkifin(ls, normal)

	fun takehor(Situation, (i, j), Constant, Unused, a, b) = 
	let
		val cur = take(Situation, (i, j))
		val neicur = take(Situation, (i+a, j+b))	
	in
		if (cur = ~1 orelse neicur = ~1 orelse cur = 1 orelse neicur = 1) then (false, Unused) else 
		let
			val this = take(Constant, (i, j))
			val neigh = take(Constant, (i+a, j+b))
			val normal = if this < neigh then (this, neigh) else (neigh, this)
			val check = checkifin(Unused, normal)
			val NewUnused = takeout(Unused, normal)
		in 
			(check, NewUnused) 
		end
	end


	fun refreshll(A::AS, j) = if j=1 then (1::AS) else (A::refreshll(AS, j-1))
	|	refreshll(_,_) = []
	fun refresh(S::Situation, (i, j)) = if i=1 then (refreshll(S, j)::Situation) else (S::(refresh(Situation, (i-1, j))))
	|	refresh(_,_) = [[]]



	fun refreshhor(Situation, (i, j)) = refresh(refresh(Situation, (i,j)), (i,j+1))
	fun refreshver(Situation, (i, j)) = refresh(refresh(Situation, (i,j)), (i+1,j))



	fun scan((i, j), Unused, Situation, Constant) = 
	if take(Situation, (i, j)) = 1 
	then
		let
			val nextsq = nextsquare (i, j)
		in
			if nextsq = (~1, ~1) then 1 else scan(nextsq, Unused, Situation, Constant)
		end
	else 
		let
			val (checkhor, NewUnusedhor) = takehor(Situation, (i, j), Constant, Unused, 0, 1)  (*Oi takehor, takever frontizoyn gia to an einai piasmenes ki oi theseis.*)
			val (checkver, NewUnusedver) = takehor(Situation, (i, j), Constant, Unused, 1, 0)
			val NewSithor = if checkhor then refreshhor(Situation, (i, j)) else Situation
			val NewSitver = if checkver then refreshver(Situation, (i, j)) else Situation
			val nextsq = nextsquare (i, j)
			val scanhor = if checkhor then scan(nextsq, NewUnusedhor, NewSithor, Constant) else 0
			val scanver = if checkver then scan(nextsq, NewUnusedver, NewSitver, Constant) else 0
		in 
			scanhor + scanver
		end

		
	val UnusedStart = [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (2,2), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6), (4,4), (4,5), (4,6), (5,5), (5,6), (6,6)]
	val SituationStart = [[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0],[0,0,0,0,0,0,0,0]]
in
	scan((1,1), UnusedStart, SituationStart, (parse File))
end


fun balance N (W : IntInf.int) =
let
	fun triadic n = 
	let
		fun triad n sofar = if n > 0 then triad (n div 3) ((n mod 3)::sofar) else sofar
	in
		triad n []
	end

	fun check ([], bit, lcur, rcur, curpower) = if bit = 1 then check ([1], 0, lcur, rcur, curpower) else if (curpower-1) <= N then (rev lcur, rev rcur) else ([], [])  
	|	check ((r::rs), bit, lcur, rcur, curpower) = 
	if bit = 0 then
		if r = 0 then check (rs, 0, lcur, rcur, curpower+1) else
		if r = 1 then check (rs, 0, lcur, (curpower::rcur), curpower+1) else
		check (rs, 1, (curpower::lcur), rcur, curpower+1)
	else
		if r = 0 then check (rs, 0, lcur, (curpower::rcur), curpower+1) else
		if r = 1 then check (rs, 1, (curpower::lcur), rcur, curpower+1) else
		check (rs, 1, lcur, rcur, curpower+1)
in
	check (rev (triadic W), 0, [], [], 1)
end
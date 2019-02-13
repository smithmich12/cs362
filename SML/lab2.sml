fun last(lst) = 
	if null (tl lst) then (hd lst)
	else last (tl lst);


fun middle(lst) =
	let
		fun helper(x, y) =
			if null (tl x) then hd y
			else if null (tl (tl x)) then hd y
			else helper(tl (tl x), tl y)
	in
		helper(lst, lst)
	end;

fun median(a, b, c) = 
	let
		fun max(d, e) =
			if d > e then d
			else if e > d then e
			else d
		fun min(d, e) =
			if d < e then d
			else if e < d then e
			else d
		val largest = max(a, max(b, c));
		val smallest = min(a, min(b, c));
	in
		(a + b + c) - (largest + smallest)
	end;

fun partition(nil, p) = (nil, nil)
	|	partition(x::xs, p) =
		let
			val (smaller, larger) = partition(xs, p)
		in	
			if x < p then (x::smaller, larger)
			else if x > p then (smaller, x::larger)
			else (smaller, larger)
		end;

fun quicksort(nil) = (nil)
	| quicksort [n] = [n]
	| quicksort(x::xs) =
	let
		val pivot = median(x, middle(x::xs), last(x::xs))
		val (small, large) = partition(x::xs, pivot)
	in
		quicksort(small) @ [pivot] @ quicksort(large)
	end;
		

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*checks if the element is in the list*)
(*from discussion*)
let rec contains e l = 
    match l with
    [] -> false
    | h::t -> if h=e then true
              else contains e t

(*subset*)
let rec subset a b = 
	match a with 
	[] -> true (*empty set is a subset of itself*) 
	| h::t -> if (contains h b) then 
			  subset t b 
			  else false 

(*euqal_sets*)
(*if a is a subset of b and b is a subset of a then
a and b are equal*)
let rec equal_sets a b =
	if (subset a b) && (subset b a) then true
	else false

(*set_union*)
let rec set_union a b =  
	match a with 
	[] -> b (*if a is empty set, then return b*)
	| h::t -> if (contains h b) then (*if a is in b then go to next*)
			  (set_union t b)
			  else (set_union t b) @ [h]  (*if a is not in b, then concat a*) 

(*set_intersection*)
(*a and b intersection is what a and b have in common only --> 
c*)
let rec set_intersection a b = 
	match a with 
	[] -> [] (*if a is empty set then return nothing*)
	| h::t -> if (contains h b) then (*if a is in b then concat a to list*)
			  (set_intersection t b) @ [h]
			  else (set_intersection t b) (*if a is not in b then continue with the rest*)

(*set_diff a - b*)
(*a and b difference is the elements in a not in b*)
let rec set_diff a b = 
	match a with 
	[] -> [] (*if a is empty then return empty *)
	| h::t -> if (contains h b) then (*if a is in b then continue with the rest*) 
			  (set_diff t b) 
			  else h::(set_diff t b)  (*if a is not in b then go from the rest *)

(*computed fixed point*)
let rec computed_fixed_point eq f x = 
	if (eq (f x) x) then (* if f(x) equals x then the answer is x; eq first to follow eq f x syntax*)
	x
	else computed_fixed_point eq f (f x) (*other wise, move onto the next and check for the next x as f(x)*)
	(*if no fixed point don't do anything*) 

(*computed_periodic_point*) 
let rec recur_p f p x = 
	match p with 
	0 -> x 
	| _-> (f (recur_p f (p-1) x))   (*if p is not 0, then keep calling f(x) with
									the previous answer of f(x) until p is 0*)

let rec computed_periodic_point eq f p x = 
	match p with 
	0 -> x 
	| 1 -> (computed_fixed_point eq f x)
	| _-> if (eq x (recur_p f p x)) then x (*if x is equal to the function after doing
											it p times then return x*)
		  else computed_periodic_point eq f p (f x) (*else keep going with f x*)
	

(*while away*)
let rec while_away s p x = 
	if not (p x) then  (*if p x is false return [] *)
	[] 
	else x::(while_away s p (s x))	(*otherwise, if p (s x) is false return [x]; start from x*)	

(*cloning function from TA's discussion*)
let rec clone el num = 
	match num with 
	0 -> [] 
	| _-> el::(clone el (num-1))
(*how to extract nth element of a tuple*)
(*get the number*)
let getLeft (x,_) = x 
(*get the element*)
let getRight (_,x) = x
(*rle_decode lp*)
let rec rle_decode lp =
	 match lp with (*if list is empty return empty*)
	 [] -> []
	 | h::t -> (clone (getRight h) (getLeft h)) @ rle_decode t
	 (*clone rightsideof head leftsideof head and append the rest*)

(*checks if the symbol is terminal*)
let check_if_terminal sym = 
	match sym with 
	| T _-> true
	| N s -> false 

let returnExpr symLst =	
	match symLst with
	| N e1 -> e1 
	| T e2 -> e2  

let rec recur_helper rule_lst safe_lst = 
	match rule_lst with
	[] -> true 
	| (N e)::t -> if contains e safe_lst then 
				  recur_helper t safe_lst
				  else false 
	| (T e)::t -> recur_helper t safe_lst 

(*list of terminals/"good" rules*) 
let rec terminal_lst rule safe_lst= 
	match rule with 
	[] -> safe_lst
	| (start,rule_lst)::t -> 
		if ((recur_helper (rule_lst) (safe_lst)) || rule_lst = []) && not (contains start (safe_lst)) then 
		(terminal_lst t (safe_lst@[start]))
		else (terminal_lst t safe_lst)

(*from TA OH: use equal sets and computed fixed point*) 
let is_equal lst1 lst2 =
	if equal_sets lst1 lst2 then
	true
	else false

(*currying*) 
let repTermLst x = fun y -> (terminal_lst x y)

(*keep checking the grammar list; 
this will give completed list of all terminal rules*) 
let rec recur_tLst lst = 
	computed_fixed_point (is_equal) (repTermLst lst) ([])

(*filter out the rules by the names; if it is not in the terminal list remove it*)
let rec filt_TLst original_lst safe_lst = 
	match original_lst with 
	[] -> [] 
	| h::t -> if (not (contains (fst h) safe_lst)) || (not (recur_helper (snd h) safe_lst)) then
			  (filt_TLst t safe_lst)
			 else h::(filt_TLst t safe_lst)


let filter_blind_alleys g = 
	match g with 
	| (lhs,rhs) -> (lhs, (filt_TLst (rhs) (recur_tLst (rhs)) ) ) 

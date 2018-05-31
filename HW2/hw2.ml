type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*convert_grammar makes HW2 style like HW1 style*)
(*given an input of a tuple of (nonterminal, list of rules) HW1 and 
returns HW2 style (nonterm, production function)*)
(*below is the function for checking the key*) 
let rec find_rules nonterms rules = 
	match rules with 
	| [] -> []
	| (key,values)::t -> if (key=nonterms) then
					 values::(find_rules nonterms t) (*adds rules to list*) 
					 else (find_rules nonterms t) (*moves onto next key*)

let rec convert_grammar gram1 =
	match gram1 with
	| (startsym,rules) -> (startsym, fun key -> (find_rules key rules))
	(*hw2 style is (starting symbol, production function)*)

(*begins functions for parse_prefix and parse_prefix below*) 
let matcher ss pfunc rulesofss acceptor deriv frag =  
	let rec match_rules pfunc individual_rules acceptor deriv frag = 
		match individual_rules with 
		| [] -> (acceptor deriv frag) (*if done, call accepton d and s*) 
		| (N x)::t -> (match_rhs x pfunc (pfunc x) (match_rules pfunc t acceptor) deriv frag)(* (matcher x p_func (p_func x) deriv acceptor frag) *) (*if nonterminal, go through that NT's rules*) 
		| (T x)::t -> match frag with (*if terminal, see if matches frag*) 
					  | f::tl -> if (x=f) then (match_rules pfunc t acceptor deriv tl) (*if match frag, then move onto next el of frag*) 
					  			else None (*if doesn't match frag then it's not there so return None*) 
					  | [] -> None
	 (*use "and" so match_rules can call match_rhs*) 
	 and match_rhs ss pfunc rulesofss acceptor deriv frag = (*goes through the current starting symbol's rhs rules*) 
	 		match rulesofss with 
			| [] -> None (*if there are no rhs rules then return None bc there's nothing*) 
			| hd::tl -> match (match_rules pfunc hd acceptor (deriv@[ss, hd]) frag) with
						| None -> (match_rhs ss pfunc tl acceptor deriv frag) (*if none, then loop --> check next rule for the rhs*) 
						| Some(d, suffix) -> Some(d, suffix)
	in (*call match_rhs to use it*) 
	match_rhs ss pfunc rulesofss acceptor deriv frag

let parse_prefix gram acceptor frag = (*mainrules is production function that takes in NT or the new ss*)
	match gram with 
	| (ss, pfunc) -> matcher ss pfunc (pfunc ss) acceptor [] frag (*the rhs is a function that takes in a NT value*)






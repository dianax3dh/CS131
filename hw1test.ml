let subset_test0 = subset [] []
let subset_test1 = not (subset [3;1;10;3] [1;2;3])
let subset_test2 = not (subset [1;3;7] [4;1;3])
let subset_test3 = subset [1;1;1;1] [2;3;4;1]

let equal_sets_test0 = equal_sets [1;3;1;3;1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3;2])

let set_union_test0 = equal_sets (set_union [4;5;6] [1;2;3]) [1;2;3;4;5;6]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [9;10;11] []) [9;10;11]

let set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) []
let set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3]
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;10;3;4] [3;1;2;4]) [4;3;2;1]

let set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) []
let set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4]
let set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4]
let set_diff_test3 = equal_sets (set_diff [] [4;3;1]) []

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)

let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.
let computed_periodic_point_test2 =
  computed_periodic_point (=) (fun x -> (1./.x)) 2 3. = 3.

let while_away_test0 = 
	while_away ((+) 3) ((>) 10) 0 = [0; 3; 6; 9]
let while_away_test1 = 
	while_away ((+) 2) ((>) 10) 0 = [0; 2; 4; 6; 8]
let while_away_test2 = 
	while_away ((+) 1) ((>) 5) 2 = [2; 3; 4]

let rle_decode_test0 = 
	rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"] = ["w"; "w"; "w"; "x"; "z"; "z"]
let rle_decode_test1 = 
	rle_decode [0,"a"; 1,"b"; 2,"c"; 3,"d"] = ["b"; "c"; "c"; "d"; "d"; "d"]
let rle_decode_test2 = 
	rle_decode [2,10; 1,8; 0,11] = [10; 10; 8]

type gram_nonterminals = 
| A | B | C | P | Q | Z

let gram_rules = 
	[A, [T"h"; N B; T"i"]; 
	 Z, [N A];
	 P, [N Q]; 
	 B, [T"i"]; 
	 C, [N Z]; 
	 C, [T"q"]]

let gram_grammar = A, gram_rules

let gram_test0 = 
	filter_blind_alleys gram_grammar = (A, [A, [T"h"; N B; T"i"]; Z, [N A]; B, [T"i"]; C, [N Z]; C, [T"q"]])

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet | Laugh | Sing

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Sentence; T","; N Conversation; N Laugh]; 
   Laugh, [N Sing]]

let g_test0 = 
	filter_blind_alleys giant_grammar = (Conversation, [Snore, [T"ZZZ"]; Quiet, []; Grunt, [T"khrgh"]; Shout, [T"aooogah!"]; Sentence, [N Quiet]; Sentence, [N Grunt]; Sentence, [N Shout]])



let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

(* An example grammar for a small subset of Awk.
   This grammar is not the same as Homework 1; it is
   instead the same as the grammar under
   "Theoretical background" above.  *)

type awksub_nonterminals =
  | Outfit | Outfit2 | Outfit3 | Outfit4 | Tops | Tshirt | Blouse | Sleeveless |
  Bottoms | Shoes | Heels | Sneakers | OpenToed | Accessories | Hat | Jewlery | Etc

let awkish_grammar =
  (Outfit,
   function
     | Outfit ->  
            [[N Tops; N Bottoms; N Accessories]; 
             [N Tops; N Bottoms]; 
             [N Tops]]
     | Tops -> 
            [[N Tshirt]; 
             [N Blouse]; 
             [N Sleeveless]]
     | Tshirt -> 
            [[T "graphic tee"]; 
             [T "plain white t"]; 
             [T "tshirt dress"]]
     | Blouse -> 
            [[T "dress shirts"]; 
             [T "crop-tops"]]
     | Sleeveless -> 
            [[T "tank top"]; 
             [T "strapless"]; 
             [T "spaghetti strap"]]
     | Bottoms -> 
            [[T "Jeans"]; 
             [T "Pants"]; 
             [T "Shorts"; T "Skirts"]]
     | Shoes ->
            [[N Heels]; 
             [N Sneakers]; 
             [N OpenToed]]
     | Heels -> 
            [[T "1inch"]; 
             [T"2inch"]]
     | Sneakers -> 
            [[T"Converse"]; 
             [T"Vans"]]
     | OpenToed -> 
            [[T"flats"]; 
             [T"flip-flops"]]
     | Accessories -> 
            [[N Hat]; 
             [N Jewlery]; 
              [N Etc]]
     | Hat -> 
            [[T"Beanie"]; 
             [T"BaseballCap"]]
     | Jewlery -> 
            [[T"Earrings"]; 
             [T"Necklace"]; 
             [T"Bracelet"]]
     | Etc -> 
            [[T"Belt"]; 
             [T"Scarf"]]) 


let test_1 =
  ((parse_prefix awkish_grammar accept_all ["tshirt dress"])
   = Some ([(Outfit, [N Tops]); (Tops, [N Tshirt]); (Tshirt, [T "tshirt dress"])], []))


let test_2 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["dress shirts"; "Pants"; "Bracelet"])
   = Some
       ([(Outfit, [N Tops; N Bottoms; N Accessories]); (Tops, [N Blouse]); (Blouse, [T "dress shirts"]); (Bottoms, [T "Pants"]); 
        (Accessories, [N Jewlery]); (Jewlery, [T "Bracelet"])], 
        []))

open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =  let rec moveH deltalst s = match deltalst with 
  |[]->[]
  |h::t -> match h with 
    |(from, c, toNode) -> if c = s && elem from qs then insert toNode (moveH t s) else (moveH t s) 
 in moveH nfa.delta s ;; 

  
let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = let mvlst = move nfa qs None in 
if union mvlst qs = qs then qs else e_closure nfa (union mvlst qs) ;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let rec acceptH start str = match str with 
|[]-> let check = e_closure nfa start in not (intersection check  nfa.fs = [])
|h::t-> acceptH (move nfa (e_closure nfa start) (Some h)) t 
 in acceptH [nfa.q0] (explode s) 


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = let rec new_statesH some = match some with 
|[]->[]
|h::t -> (e_closure nfa (move nfa qs (Some h)))::new_statesH t 
 in new_statesH nfa.sigma;;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = let rec new_transH some = match some with 
|[]->[]
|h::t->(qs, Some h, ((e_closure nfa (move nfa qs (Some h)))))::new_transH t
 in new_transH nfa.sigma;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = let rec new_finalsH lst = match lst with 
|[]->[]
|h::t -> if elem h nfa.fs then [qs] else new_finalsH t
 in new_finalsH qs;;

 let rec sort lst = match lst with 
 []->[]
 |h::t-> if elem h t then sort t else h::(sort t)
;; 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t = match work with 
    |[]-> dfa
    |h::t-> nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = sort (insert_all (new_states nfa h) dfa.qs);
      q0 = dfa.q0; delta = (insert_all   (new_trans nfa h) dfa.delta); fs = insert_all (new_finals nfa h) dfa.fs} (insert_all (diff (sort (new_states nfa h)) dfa.qs) t)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = let dfa = {sigma = nfa.sigma; qs = [(e_closure nfa [nfa.q0])] ;
 q0 = (e_closure nfa [nfa.q0]); fs = []; delta = []} in let newdfa = nfa_to_dfa_step nfa dfa [dfa.q0] in 
 {sigma = newdfa.sigma; qs = newdfa.qs; q0 = newdfa.q0; delta = newdfa.delta; fs = newdfa.fs}

 
type 'a prop = 
  | Top
  | Bot
  | V of 'a
  | Not of 'a prop
  | And of 'a prop * 'a prop
  | Or of 'a prop * 'a prop
  | Impl of 'a prop * 'a prop
;;

type 'a sequent = {
  gamma : 'a prop list ;
  delta : 'a prop list ;
  gamma_var : 'a prop list ;
  delta_var : 'a prop list ;
}
;;



let create_sequent l_gamma l_delta = 
  let res = { gamma = l_gamma; delta = l_delta; gamma_var = []; delta_var = [] } in
  res
;;

let rec member x l = match l with
  | [] -> false
  | x'::s when x' = x -> true
  | _::s -> member x s
;;

let bot seq = 
  member Bot seq.gamma || member Bot seq.gamma_var
;;


let top seq = 
  member Top seq.delta || member Top seq.delta_var
;;

let axiom seq = 
  let rec iter l = match l with
    | [] -> false
    | x::s when member x seq.delta -> true
    | x::s when member x seq.delta_var -> true
    | _::s -> iter s
  in 
  iter seq.gamma || iter seq.gamma_var
;;

exception Wrong_rule of string;;

let and_gamma seq = 
  assert (seq.gamma <> []);
  let el::s = seq.gamma in
  match el with 
  | And (phi, psi) -> {gamma = psi::phi::s; delta = seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var}
  | _ -> raise (Wrong_rule "And Gamma")
;;

let or_gamma seq = 
  assert (seq.gamma <> []);
  let el::s = seq.gamma in
  match el with 
  | Or(phi, psi) -> ({gamma = phi::s; delta = seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var},
                     {gamma = psi::s; delta = seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var})
  | _ -> raise (Wrong_rule "Or Gamma")
;;

let impl_gamma seq = 
  assert (seq.gamma <> []);
  let el::s = seq.gamma in
  match el with 
  | Impl (phi, psi) -> ({gamma = s; delta = phi::seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var},
                        {gamma = psi::s; delta = seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var})
  | _ -> raise (Wrong_rule "Impl Gamma")
;;           

let not_gamma seq = 
  assert (seq.gamma <> []);
  let el::s = seq.gamma in
  match el with 
  | Not phi -> {gamma = s; delta = phi::seq.delta; gamma_var = seq.gamma_var; delta_var = seq.delta_var}
  | _ -> raise (Wrong_rule "Not Gamma")
;;

let and_delta seq = 
  assert (seq.delta <> []);
  let el::s = seq.delta in
  match el with 
  | And (phi, psi) -> ({gamma = seq.gamma; delta = phi::s; gamma_var = seq.gamma_var; delta_var = seq.delta_var},
                       {gamma = seq.gamma; delta = psi::s; gamma_var = seq.gamma_var; delta_var = seq.delta_var})
  | _ -> raise (Wrong_rule "And Delta") 
;;

let or_delta seq = 
  assert (seq.delta <> []);
  let el::s = seq.delta in
  match el with 
  | Or (phi, psi) -> {gamma = seq.gamma; delta = phi::psi::s; gamma_var = seq.gamma_var; delta_var = seq.delta_var}
  | _ -> raise (Wrong_rule "Or Delta") 
;;

let impl_delta seq = 
  assert (seq.delta <> []);
  let el::s = seq.delta in
  match el with 
  | Impl (phi, psi) -> {gamma = phi::seq.gamma; delta = psi::s; gamma_var = seq.gamma_var; delta_var = seq.delta_var}
  | _ -> raise (Wrong_rule "Impl Delta") 
;;

let not_delta seq = 
  assert (seq.delta <> []);
  let el::s = seq.delta in
  match el with 
  | Not phi -> {gamma = phi::seq.gamma; delta = s; gamma_var = seq.gamma_var; delta_var = seq.delta_var}
  | _ -> raise (Wrong_rule "Not Delta") 
;;


(*let applique_gamma seq = 
   try let seq' = and_gamma seq in seq' with (Wrong_rule "And Gamma") 
     -> try let seq' = or_gamma seq in seq' with (Wrong_rule "Or Gamma") 
       -> try let seq' = impl_gamma seq in seq' with (Wrong_rule "Impl Gamma")
         -> try let seq' = not_gamma seq in seq' with (Wrong_rule "Not Gamma")
           -> assert(false)
 ;;*)
            
            


let rec proof_search seq = 
  (*print_endline "s";*)
  if bot seq || top seq || axiom seq then true
  else match seq.gamma with
    | [] ->  (match seq.delta with 
        | [] -> false
        | x::s when x = Top || x = Bot 
          -> proof_search {gamma = seq.gamma; delta = s; gamma_var = seq.gamma_var; delta_var = x::seq.delta_var}
        | (V i)::s 
          -> proof_search {gamma = seq.gamma; delta = s; gamma_var = seq.gamma_var; delta_var = (V i)::seq.delta_var}
        | l ->  ( try let seq' = and_delta seq in proof_search (fst seq') && proof_search (snd seq') with (Wrong_rule "And Delta") 
            -> try let seq' = or_delta seq in proof_search seq' with (Wrong_rule "Or Delta") 
              -> try let seq' = impl_delta seq in proof_search seq' with (Wrong_rule "Impl Delta")
                -> try let seq' = not_delta seq in proof_search seq' with (Wrong_rule "Not Delta")
                  -> print_endline "coucou1"; assert(false)) )
    | x::s when x = Top || x = Bot
      -> proof_search {gamma = s; delta = seq.delta; gamma_var = x::seq.gamma_var; delta_var = seq.delta_var}
    | (V i)::s 
      -> proof_search {gamma = s; delta = seq.delta; gamma_var = (V i)::seq.gamma_var; delta_var = seq.delta_var}
    | l -> ( try let seq' = and_gamma seq in proof_search seq' with (Wrong_rule "And Gamma") 
        -> try let seq' = or_gamma seq in proof_search (fst seq') && proof_search (snd seq') with (Wrong_rule "Or Gamma") 
          -> try let seq' = impl_gamma seq in proof_search (fst seq') && proof_search (snd seq') with (Wrong_rule "Impl Gamma")
            -> try let seq' = not_gamma seq in proof_search seq' with (Wrong_rule "Not Gamma")
              -> print_endline "coucou2"; assert(false))
;;


(* Exemples renvoyant false *)

proof_search (create_sequent [] [Or(V 1, V 1)]) ;;

proof_search (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 2)]) ;;

(*
  impl_gamma (impl_delta (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 2)]));;
  proof_search (snd (impl_gamma (impl_delta (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 2)])))) ;;*)

(* Exemples renvoyant true *)


proof_search (create_sequent [] [Or(V 1, Not(V 1))]) ;;

proof_search (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 1)]) ;;

proof_search (create_sequent [And(And(V 1, V 2), V 3)] [And(V 1, And(V 2, V 3))]) ;;

proof_search (create_sequent [Or(Or(V 1, V 2), V 3)] [Or(V 1, Or(V 2, V 3))]) ;;

proof_search (create_sequent [Impl(V 1, V 2)] [Impl(Not(V 2), Not(V 1))]) ;;

proof_search (create_sequent [Impl(Not(V 2), Not(V 1))] [Impl(V 1, V 2)]) ;;

proof_search (create_sequent [V 1; V 2] [Impl(V 3, And(V 1, V 3))]) ;;









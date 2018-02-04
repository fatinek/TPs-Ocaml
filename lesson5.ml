(* *Fist-order functions on lists : *)

let rec nth alist n =
  match alist with
    | [] -> failwith "List too small"
    | x :: rest -> if (n = 0)
        then x
        else nth rest (n-1)
;;

let alist1 = [1;2;3;4];;
let alist2 = [5;6];;

nth alist1 3;;
nth alist1 0;;
(*nth alist2 2;;*)

let rec rev_ac l acu =
  match l with
    | [] -> acu
    | x :: rest -> rev_ac rest (x :: acu)
;;

let rev l =
  rev_ac l []
;;

rev alist1;;
rev alist2;;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | x :: rest -> if (rest = []) 
        then x :: l2
        else x :: append rest l2
;;

append alist1 alist2;;

let rec rev_append l1 l2 =
  match l1 with
    | [] -> l2
    | x :: rest -> if (rest = [])
        then x :: l2
        else rev_append rest (x :: l2)
;;

rev_append alist1 alist2;;



(* **Higher-order functions on lists : *)

let rec rev_map_ac f l acu =
  match l with
    | [] -> acu
    | x :: rest -> rev_map_ac f rest (f(x) :: acu)
;;

let rev_map f l =
  rev_map_ac f l []
;;

rev_map (fun x -> x + 1) [ 5 ; 10 ; 15 ];;



let rec iter f l =
  match l with
    | [] -> ()
    | x :: rest -> f(x) ; (iter f rest)
;;

iter (Printf.printf "Element : %d\n%!") [ 2 ; 4 ; 6 ; 8 ];;



let rec print_list sep conv alist =
  match alist with
    | [] -> ""
    | x :: rest -> if (rest = []) 
        then conv(x)
        else conv(x) ^ sep ^ print_list sep conv rest
;;


print_list ", " string_of_int [ 4 ; 8 ; 99 ];;
print_list " ++ " (fun x -> x) [ "aa" ; "bb" ; "cc" ];;



let rec fold (op: int->int->int) (acu: int) = function
  | [] -> acu
  | x :: rest -> fold op (op acu x) rest
;;

fold (+) 0 [ 1 ; 2 ; 3 ; 4 ];;
fold ( * ) 1 [ 1 ; 2 ; 3 ; 4 ];;

let rec fold op acu = function
  | [] -> acu
  | x :: rest -> fold op (op acu x) rest
;;

fold (fun a b -> a ^ " " ^ string_of_int b) "" [ 1 ; 2 ; 3 ; 4];;
fold (fun a b -> if a < b then b else a) 0 [ 10 ; 40 ; 20 ; 30 ];;
fold (fun a b -> b :: a) [] [ 4 ; 3 ; 2 ; 1 ];;



let exists pred alist = 
  fold (fun a b -> (a || pred(b))) false alist
;;

exists (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists (fun x -> x < 10) [];;


let rec exists pred alist =
  match alist with
    | [] -> false
    | x :: rest -> pred(x) || exists pred rest
;;

exists (fun x -> x < 10) [ 20 ; 5 ; 30 ];;
exists (fun x -> x < 10) [ 20 ; 40 ; 30 ];;
exists (fun x -> x < 10) [];;



let (++) g f nb =
  let a = f(nb) in
    g(a)
;;

((fun x -> x - 10) ++ abs) (-20);;
(abs ++ (fun x -> x - 10)) (-20);;



(* let forall pred alist =  
   !!!!!!!!!! *)


(* *Association lists : *)

let rec assoc key assoc_list =
  match assoc_list with
    | [] -> raise Not_found
    | (x,y) :: rest ->
        if (x = key)
        then y
        else assoc key rest
;;

let assoc1 = [ ("Lucy", true) ; ("Mike", false) ; ("Hilary", false) ; ("Donald", true) ];;

assoc "Donald" assoc1;;
assoc "Mike" assoc1;;
(*assoc "donald" assoc1;;*)

let pastrouve = true;;

let rec remove_assoc key assoc_list =
  match assoc_list with
    | [] -> []
    | (x,y) :: rest ->
        if (x = key) 
        then remove_assoc key rest
        else (x,y) :: remove_assoc key rest
;;

remove_assoc "Hilary" assoc1;;

(* j'arrive pas a mettre l'exception *)



























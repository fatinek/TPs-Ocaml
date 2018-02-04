(** Exercice 2 : **)

(* Lists *)


let rec switch = function
  | [] -> []
  | a :: [] -> a :: []
  | a :: b :: rest -> b :: a :: switch rest
;;

switch [];;
switch [1];;
switch [1;2];;
switch [1;2;3];;
switch [1;2;3;4];;
switch [1;2;3;4;5];;
switch [1;2;3;4;5;6];;


let rec unpair = function
  | [] -> []
  | (a,b) :: rest -> a :: b :: unpair rest
;;

unpair [];;
unpair [ (3,4) ];;
unpair [ (3,4) ; (10,11) ];;
unpair [ (3,4) ; (10,11) ; (20,30) ];;


let rec remove_succ = function
  | [] -> []
  | a :: [] -> a :: []
  | a :: b :: rest ->
      if (b = a+1)
      then remove_succ (b :: rest)
      else a :: remove_succ (b :: rest)
;;

remove_succ [];;
remove_succ [10];;
remove_succ [10 ; 20];;
remove_succ [10 ; 20 ; 21];;
remove_succ [10 ; 20 ; 21 ; 22 ; 23 ; 24 ; 100 ; 101 ; 110];;
remove_succ [20 ; 21 ; 22 ; 21 ; 22 ; 23];;


let rec combine l1 l2 =
  match (l1,l2) with
    | ([],[]) -> []
    | (_::_, []) -> failwith "The lists have different lengths"
    | ([], _::_) -> failwith "The lists have different lengths"
    | (a :: rest1 , b :: rest2) -> (a,b) :: combine rest1 rest2
;;

combine [10;20;30] [4;5;6];;
(*combine [10;20;30] [4;5;6;9];;*)


let rec keep alist boollist =
  match (alist, boollist) with
    | ([],[]) -> []
    | (_::_, []) -> failwith "The lists have different lengths"
    | ([], _::_) -> failwith "The lists have different lengths"
    | (a :: rest1 , b :: rest2) -> 
        if b then a :: keep rest1 rest2
        else keep rest1 rest2
;;

keep [ 1 ; 2 ; 3 ; 4 ; 5 ] [ true ; true ; false ; false ; false ];;
keep [ 1 ; 2 ; 3 ; 4 ; 5 ] [ false ; false ; false ; false ; false ];;
keep [ 1 ; 2 ; 3 ; 4 ; 5 ] [ false ; true ; false ; true ; false ];;


let rec map2 f l1 l2 =
  match (l1,l2) with
    | ([],[]) -> []
    | (_::_, []) -> failwith "The lists have different lengths"
    | ([], _::_) -> failwith "The lists have different lengths"
    | (a :: rest1 , b :: rest2) -> (f a b) :: map2 f rest1 rest2
;;

map2 (-) [ 100 ; 200 ; 300] [ 1 ; 2 ; 3 ];;
map2 (fun a b -> a ^ string_of_int b) [ "AA" ; "BB" ] [ 20 ; 30 ];;


let rec interleave l1 l2 =
  match (l1,l2) with
    | ([],[]) -> []
    | (a :: rest , []) -> a :: interleave rest []
    | ([], a :: rest ) -> a :: interleave [] rest
    | (a :: rest1 , b :: rest2) -> a :: b :: interleave rest1 rest2
;; 

interleave [ 10 ] [ 1 ; 2 ; 3 ; 4 ];;
interleave [ 10 ; 20 ; 30 ; 40 ] [ 1 ; 2 ; 3 ; 4 ; 5 ];;
interleave [] [ 1 ; 2 ; 3 ; 4 ; 5 ];;


type bool3 = BTrue | BFalse | Unknown;;

let and3 a b =
  match (a,b) with 
    | (BTrue,BTrue) -> BTrue
    | (BTrue, Unknown) -> Unknown
    | (Unknown, BTrue) -> Unknown
    | (_,_) -> BFalse
;;

and3 BTrue Unknown;;
and3 BFalse Unknown;;


let not3 = function
  | BTrue -> BFalse
  | BFalse -> BTrue
  | Unknown -> Unknown
;;


type instruction = Plus of int | Mul of int;;

let rec apply_instructions n l =
  match l with
    | [] -> n
    | inst :: rest -> 
        match inst with
          | Plus a -> apply_instructions (n+a) rest
          | Mul b -> apply_instructions (n*b) rest
;;

apply_instructions 100 [];;
apply_instructions 100 [ Plus 5 ];;
apply_instructions 100 [ Plus 1 ; Mul 3 ];;
apply_instructions 100 [ Plus 1 ; Mul 3 ; Plus 10 ];;



let rec to_funlist l =
  match l with
    | [] -> []
    | i :: rest -> 
        match i with
          | Plus a -> (fun x -> x+a) :: to_funlist rest
          | Mul b -> (fun x -> x*b) :: to_funlist rest
;;

to_funlist [ Plus 4 ];;
to_funlist [ Mul 3 ; Plus 1 ];;
List.map (fun f -> f 100) (to_funlist [ Mul 3 ; Plus 1 ]);;


let to_fun l n =
  let f = to_funlist l in
  let rec loop f n =
    match f with
      | [] -> n
      | a :: rest -> loop rest (a(n))
  in 
    loop f n
;;

to_fun [] 100;;
to_fun [ Plus 5 ] 100;;
to_fun [ Plus 1 ; Mul 3 ] 100;;
to_fun [ Plus 1 ; Mul 3 ; Plus 10 ] 100;;


let rec compact l =
  match l with 
    | [] -> []
    | Plus a :: rest ->
        (match rest with
          | Plus b :: restb -> Plus (a+b) :: compact restb
          | Mul b :: restb -> Plus a :: Mul b :: compact restb
          | [] -> Plus a :: [])
    | Mul a :: rest ->
        (match rest with
          | Plus b :: restb -> Mul a :: Plus b :: compact restb
          | Mul b :: restb -> Mul (a*b) :: compact restb
          | [] -> Mul a :: [])
;;

compact [ Plus 10 ; Plus 20 ];;
compact [ Mul 3 ; Mul 100 ; Plus 50 ; Plus 10 ] ;;


let rec to_string s = function
  | [] -> s
  | Plus a :: rest -> to_string (s^" + "^string_of_int(a)) rest
  | Mul b :: rest -> to_string ("("^s^")"^" * "^string_of_int(b) ) rest
;;

to_string "x" [ Plus 5 ];;
to_string "y" [ Plus 1 ; Mul 3 ] ;;
to_string "u" [ Plus 1 ; Mul 3 ; Plus 10 ];;


type 'a element = Single of 'a | Pair of 'a * 'a
type 'a fonct = One_arg of ('a -> 'a) | Two_args of ('a -> 'a -> 'a)


let rec count_elements = function
  | [] -> 0
  | Single _ :: rest -> 1 + count_elements rest
  | Pair _ :: rest -> 2 + count_elements rest
;;

let rec apply_list fl l =
  match (fl,l) with
    | ([],[]) -> []
    | (One_arg f :: restf, Single a :: resta) -> f a :: apply_list restf resta
    | (Two_args f :: restf, Pair (a,b) :: restab) -> f a b :: apply_list restf restab

    | (One_arg _ :: _, []) | (Two_args _ :: _, []) | ([], Single _ :: _) | ([], Pair _ :: _) -> failwith "The lists don't have the same size"
    | (One_arg _ :: _, Pair _ :: _) -> failwith "One arg function applied to a pair"
    | (Two_args _ :: _, Single _ :: _) -> failwith "Two args function applied to a single"
;;

apply_list [] [];;
apply_list [ Two_args (+) ; One_arg abs ] [ Pair (100,50) ; Single (-99) ];;
(*apply_list [ Two_args (+) ; One_arg abs ] [ Single (-99) ; Pair (100,50) ];;*)



let rec partial_apply x = function
  | [] -> []
  | One_arg f :: rest -> f :: partial_apply x rest
  | Two_args f :: rest -> f x :: partial_apply x rest
;;

partial_apply 10 [];;
partial_apply 99 [ Two_args (+) ; One_arg abs ];;
let pl = partial_apply true [ One_arg not ; Two_args (&&) ];;
List.map (fun f -> f false) pl;;




(* Records *)

type people = 
    { name : string;
      age : int }
;;

let rec mk_people_list = function
  | 0 -> []
  | n -> {name = "John-"^string_of_int(n); age = 10*n} :: mk_people_list (n-1)
;;

mk_people_list 0;;
mk_people_list 1;;
mk_people_list 2;;


let rec age_most cmp guy liste =
  match liste with
    | [] -> guy
    | a :: rest -> if (cmp a.age guy.age) then age_most cmp a rest else age_most cmp guy rest
;;

age_most (<) { name = "Max" ; age = 100 } [];;
age_most (<) { name = "Max" ; age = 100 } (mk_people_list 8);;
age_most (>) { name = "Max" ; age = 100 } (mk_people_list 8);;
age_most (>) { name = "Max" ; age = 0 } (mk_people_list 8);;


type author = Anonymous | Someone of people;;

type 'a contribution =
    { date : float;
      author : author;
      content : 'a }
;;

let mike = { name = "Mike" ; age = 30 }
let rihanna = { name = "Rihanna" ; age = 28 }

let test_contr = [ { date = 100.0 ; author = Anonymous ; content = "afoo1" } ;
                   { date = 90.0  ; author = Someone mike ; content = "bar1" } ;
                   { date = 105.0 ; author = Anonymous ; content = "afoo2" } ;
                   { date = 107.0 ; author = Someone rihanna ; content ="bar2" } ;
                   { date = 102.0 ; author = Someone mike ; content = "bar3" } ]
;;

let rec filter_contributions = function
  | [] -> []
  | a :: rest -> 
      match a.author with
        | Anonymous -> filter_contributions rest
        | Someone x -> a :: filter_contributions rest
;;

filter_contributions test_contr;;


let rec map_contributions f l =
  match l with
    | [] -> []
    | a :: rest -> let b = {a with content = f(a.content)} in b :: map_contributions f rest
;;

map_contributions String.length test_contr;;


let latest l =
  let rec loop ac = function
    | [] -> ac
    | a :: rest ->
        let comp =
          match ac with
            | None -> Some a
            | Some b -> if (a.date > b.date) then Some a else ac 
        in loop comp rest
  in loop None l
;;

latest [];;
latest test_contr;;


let rec contents_of pers liste =
  match liste with
    | [] -> []
    | a :: rest ->
        match a.author with
          | Anonymous -> contents_of pers rest
          | Someone x -> if (x = pers) then a.content :: contents_of pers rest
              else contents_of pers rest
;;

contents_of mike test_contr;;
contents_of rihanna test_contr;;
contents_of { name = "Mike" ; age = 50 } test_contr;;



































let rec count_ones = function
  | [] -> 0
  | 1 :: rest -> 1 + count_ones rest
  | x :: rest -> count_ones rest
;;

let rec count_ones_acu acu = function
  | [] -> acu
  | 1 :: rest -> count_ones_acu(acu+1) rest
  | x :: rest -> count_ones_acu(acu) rest
;;

let rec sum = function
  | [] -> 0
  | x :: rest -> x + sum rest
;;
let rec sum_acu acu = function
  | [] -> acu
  | x :: rest -> x + sum_acu(acu) rest
;;
sum_acu 10 [1;1;1;9;1];;

let rec perms = function 
  | [] -> []
  | (x,y) :: rest -> (y,x) :: perms rest
;;

perms [];;
perms [(1,true)];;
perms[('a',8);('b',7)];;

(*avec accumulateur : *)
let rec perms_acu acu = function
  | [] -> acu
  | (x,y) :: rest -> perms_acu((y,x) :: acu) rest
;;

perms_acu [] [('a',8);('b',7)];;

let rec mk_list = function
  | 0 -> []
  | x -> x :: mk_list(x-1)
;;
mk_list 0;;
mk_list 6;;

let rec mk_aculist acu = function
  | 0 -> acu
  | x -> mk_aculist(x :: acu) (x-1)
;;
mk_aculist [] 0;;
mk_aculist [] 5;;
mk_aculist [99] 5;;

let l1 = mk_list 100000;;
let l2 = mk_aculist [] 1000000;;
sum_acu 0 l1;;
sum_acu 0 l2;;


let fun_list = [ count_ones ; sum];;
let fun_pair = (count_ones, sum);;

let fsum f = f 0 + f 1 + f 2 + f 3;;
fsum abs;;
fsum (fun x -> 2*x);;
fsum (fun y -> if y>2 then 1 else -1);;

let flist f = [ f 0 ; f 1 ; f 2 ; f 3 ];;
flist string_of_int;;
flist (fun x -> x +1);;
flist (fun x -> x);;

let rec fsumlist f = function
  | [] -> 0
  | x :: rest -> f x + fsumlist f rest
;;

fsumlist (fun x -> x+1) [0;1;2;3];;

let rec fsumlist_acu f acu = function
  | [] -> acu
  | x :: rest -> fsumlist_acu f (f x + acu) rest
;;

fsumlist_acu (fun x -> x+1) 10 [0;1;2;3];;

let rec map f = function
  | [] -> []
  | x :: rest -> f x :: map f rest
;;

map (fun x -> x+1) [];;
map (fun x -> x+1) [5];;
map (fun x -> x + 1) [ 5 ; 10 ; 15 ];;
map string_of_int [ 5 ; 10 ; 15 ] ;;
map (fun x -> (x, string_of_int x)) [ 5 ; 10 ; 15 ]

let rec find f = function
  | [] -> raise Not_found
  | x :: rest -> if (f x = true) then x else find f rest
;;

(*find (fun x-> true) [];;*)
find (fun x -> x>10) [ 5 ; 12 ; 7 ; 8 ];;
(*find (fun x -> x > 10) [ 5 ; 6 ; 7 ; 8 ];;*)
find (fun x -> true) [ 5 ; 10 ; 15 ];;

let omap f = function
  | None -> None
  | Some x -> Some (f x)
;;
















































































































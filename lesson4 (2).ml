
let create x = { contents = x};;
let read rf = rf.contents;;
let write rf v = rf.contents <-v;;

let test () =
  let x1 = create 0 in
    write x1 (x1.contents+1) ;
    write x1 (x1.contents+1) ;
    read x1
;;

test();;

let test2() =
  let x2 = ref 0 in
    x2 := x2.contents + 1;
    x2 := x2.contents + 1;
    !x2
;;

test2();;

let gen =
  let count = ref 0 in
    fun () ->
      count := !count + 1;
      !count
;;

gen();;
gen();;
let count = ref 5;;
gen();;



type color = White | Yellow | Green | Blue | Red ;;
type role = Player of color*int | Referee ;;

let role1 = Referee ;;
let role2 = Player (Green, 8) ;;
let role3 = Player (Yellow, 10);;
let role4 = Player (Yellow, 5);;

let get_number = function
  | Referee -> 0
  | Player (_,nb) -> nb
;;

type people = 
    { name : string;
      role : role;
      age : int }
;;

let same_team p1 p2 = match (p1.role,p2.role) with
  | (Player(col1,_) , Player(col2,_) ) -> (col1=col2)
  | (_,_) -> false
;;

let people1 = { name = "Arbitre"; role = role1 ; age = 41};;
let people2 = { name = "Joueur 2"; role = role2 ; age = 25};;
let people3 = { name = "Joueur 3"; role = role3 ; age = 26};;
let people4 = { name = "Joueur 4"; role = role4 ; age = 25};;

same_team people1 people2;;
same_team people2 people3;;
same_team people3 people4;;
same_team people4 people1;;

let is_number p nb = match p.role with
  | Player (_,num) -> (num=nb)
  | Referee -> false
;;

is_number people1 5;;
is_number people2 7;;
is_number people3 16;;
is_number people4 5;;



type 'a mylist = Empty | Cell of 'a * 'a mylist ;;

let mylist1 = Cell( 10, Cell(20, Empty));;
let mylist2 = Empty;;

let myhd = function
  | Empty ->  failwith "empty list"
  | Cell(a,_) -> a
;;

let rec mytl = function
  | Empty -> failwith "empty list"
  | Cell(a, Empty) -> a
  | Cell(b, rest) -> mytl(rest)
;;

let rec mylength = function
  | Empty -> 0
  | Cell(a, rest) -> 1 + mylength rest
;;

let rec mylength acu = function
  | Empty -> acu
  | Cell(a, rest) -> 1 + mylength(acu) rest
;;

let mylength l =
  (* This is your tail-recursive function *)
  let rec loop acu l = match l with
    | Empty -> acu
    | Cell(a, rest) -> 1 + loop(acu) rest
  in
    loop 0 l
;;

let ohd = function
  | [] -> None
  | x :: _ -> Some x
;;

let rec otl = function
  | [] -> None
  | x :: rest -> match rest with
    | [] -> Some x
    | _ -> otl rest
;;



(* Exercise : Ad-Hoc function *)
let rec get_referees l = match l with
  | [] -> []
  | x :: rest -> match x.role with
    | Referee -> x :: get_referees rest
    | _ -> get_referees rest
;;

let rec get_younger l max = match l with
  | [] -> []
  | x :: rest -> if (x.age <= max) then x :: get_younger rest max
      else get_younger rest max
;;

let rec find_color l col1 =
  match l with
    | [] -> None
    | x :: rest -> match x.role with
      | Referee -> find_color rest col1
      | Player (col2,_) -> if (col1 = col2) then Some x
          else find_color rest col1
;;

let people_list = [people1; people2; people3; people4];;

get_referees people_list;;
get_younger people_list 26;;
find_color people_list Yellow;;
find_color people_list Blue;;



let rec filter pred alist = 
  match alist with
    | [] -> []
    | x :: rest -> if (pred(x) = true)
        then x :: filter pred rest
        else filter pred rest
;;

filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6];;
filter (fun x -> x < 10) [100;5;15;6;16;7];;
filter (fun x -> x < 0) [100;5;15;6;16;7];;

let get_referees2 l = filter (fun x -> x.role = Referee) l;;

let get_younger2 l max = filter (fun x -> x.age <= max) l;;

get_referees2 people_list;;
get_younger2 people_list 26;;


let rec find pred alist =
  match alist with
    | [] -> None
    | x :: rest -> if (pred(x) = true)
        then Some x
        else find pred rest
;;

let has_color col p = match p.role with
  | Referee -> false
  | Player(col1,_) -> if (col1 = col) then true else false
;;

let find_color2 l c = find (fun x -> (has_color c x)) l;;

find_color2 people_list Yellow;;
















































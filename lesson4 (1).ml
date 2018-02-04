type coordinates = 
    { long : float;
      lat : float }
;;

type path = coordinates list;;

type region = 
    { region_name : string;
      borders : path;
      has_coastline : bool }
;;
(* partie 1 : 
   type test =
   {
   fon : (int -> int);
   arg : int;
   expect : int }
   ;;
*)
(* partie 2.1 :

   type 'a test =
   { fon: ('a -> int);
   arg: 'a;
   expect : int }
   ;; *)
(* partie 2.2 : *)
type ('a, 'b) test =
    { fon : ('a -> 'b);
      arg : 'a;
      expect : 'b }
;;


let point1 = { long = -0.3 ;
               lat = 42.5 }
;;

let point2 = { point1 with long = -1.0};;

let get_lat c = c.lat;;

let get_lat { lat = ll } = ll;;
let get_lat { lat } = lat;;

let get_all { long ; lat } = (long, lat);;

(* Checks if a region is well defined: it must have a region name and at least one border. *)
let is_good = function
  | { region_name = "" } -> false
  | { borders = [] } -> false
  | _ -> true
;;

let apply a = 
  if (a.fon(a.arg) = a.expect) then true else false
;;

let test1 = { fon = (fun x -> x * 2) ; 
              arg = 6 ; 
              expect = 12};;
let test2 = { fon = (fun x -> 3+x)  ; 
              arg = 10; 
              expect = 3};;

let result1 = apply test1;;
let result2 = apply test2;;

let test3 = { fon = (fun x -> 12) ; 
              arg = true ; 
              expect = 12};;

let result3 = apply test3;;

let test4 = { fon = (fun x -> if x then "bonjour x" else "bonjour pas x");
              arg = true;
              expect ="bonjour x"};;

let result4 = apply test4;;



let john = Array.make 100 true;;

john.(5);;
john.(5) <- false;;
john.(5);;
john;;

let foo a i = a.(i);;

let bar a i v = a.(i) <- v;;



type player =
    { name : string;
      age : int;
      mutable points : int} 
;;

let show_player a =
  Printf.printf"Nom : %s, age : %d, points : %d%!" a.name a.age a.points
;;

let new_player newname newage =
  let nouveau = { name = newname;
                  age = newage;
                  points = 0} in
    nouveau;;

let add_points playerx pts =
  playerx.points <- playerx.points + pts
;;

let player1 = new_player "Clara" 25;;
let player2 = new_player "Lea" 18;;

add_points player1 100;;
add_points player1 84;;
add_points player2 61;;
add_points player2 22;;

show_player player1;;
show_player player2;;


type iplayer =
    { iname : string;
      iage : int;
      ipoints : int}
;;

let show_iplayer a =
  Printf.printf"Nom : %s, age : %d, points : %d%!" a.iname a.iage a.ipoints
;;

let new_iplayer newname newage =
  let nouveau = { iname = newname;
                  iage = newage;
                  ipoints = 0} in
    nouveau;;

let add_ipoints playerx pts =
  let nouveaupoints = { playerx with ipoints = playerx.ipoints + pts } in
    nouveaupoints
;;

let test () =
  (* Create an iplayer and add some points. *)
  let p1_a = new_iplayer "Carole" 42 in
  let p1_b = add_ipoints p1_a 50 in
  let p1_c = add_ipoints p1_b 46 in
    show_iplayer p1_c
;;

test();;







































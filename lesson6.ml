(* Compilation *)

let showdir() =
  Array.sort compare (Sys.readdir(Sys.getcwd())) ;
  Array.iter (fun x -> Printf.printf " - %s\n%!" x) (Sys.readdir(Sys.getcwd()))
;;

let () = showdir ();;


(* Exceptions *)

let a = Not_found;;
let b = Failure "arrh";;
let c = End_of_file;;
let d = Division_by_zero;;

[ Not_found ; Not_found ; Failure "boo!" ];;

(*raise Not_found;;*)

let a = Not_found;;
(*let b = raise Not_found;;*)

let a () = Not_found;;
let b () = raise Not_found;;

raise;;

exception Horrible_error;;
Horrible_error;;
exception Bad_bad_thing of int * string;;


(* Function that returns (x,y) if the list given in argument starts with x and y, or fails with an exception if the list does not have two elements. *)
let take_two = function
  | [] | [_] -> raise Not_found
  | x :: y :: _ -> (x,y)
;;

let take_two_opt l =
  try Some (take_two l)
  with Not_found -> None
;;

let test_raise x =
  try
    if x < 0 then raise Not_found
    else if x = 0 then failwith "Zero"
    else if x > 100 then raise Horrible_error
    else if x > 10 then raise (Bad_bad_thing (x, "Too big."))
    else [ string_of_int x ]
  with
    | Not_found -> []
    | Failure s -> [s]
;;

test_raise (-6);;
test_raise 0;;
test_raise 5;;
(*test_raise 90;;*)
(*test_raise 105;;*)



let call f arg =
  try
    f arg
  with
      e -> let a = (Printexc.to_string e) in 
          Printf.eprintf "%s\n%!" a ; raise e
;;

(*call test_raise 90;;*)

type 'a result = Ok of 'a | Error of exn;;

let eval f arg =
  try
    let x = f arg in
      Ok x
  with
      ex -> Error ex
;;

let rec check_all f test_list = 
  match test_list with
    | [] -> true
    | (a,b) :: rest ->
        if (eval f a = b) then true && check_all f rest
        else false
;;

check_all take_two [];;
check_all take_two [ ([1], Error Not_found) ];;
check_all take_two [ ([1], Ok (1,1)) ];;
check_all take_two [ ([1], Error Not_found) ; ([4;3;2;1], Ok (4,3)) ];;
check_all take_two [ ([1], Error Not_found) ; ([4;3;2;1], Error Not_found) ];;
































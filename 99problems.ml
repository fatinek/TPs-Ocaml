(** Working with lists **)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last = function
  | [] -> None
  | a :: rest -> if (rest = []) then Some a
      else last rest
;;

last [ "a" ; "b" ; "c" ; "d" ];;
last [];;


(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two = function
  | [] -> None
  | [a] -> None
  | a :: b :: rest -> if (rest = []) then Some (a,b) else last_two (b :: rest)
;;

last_two ["a";"b";"c";"d"];;
last_two ["a"];;


(* 3. Find the k'th element of a list. (easy) *)

let rec at k l =
  match l with
    | [] -> None
    | a :: rest -> match k with
      | 1 -> Some a 
      | n -> at (n-1) rest
;;

at 3 ["a";"b";"c";"d";"e"];;
at 3 ["a"];;


(* 4. Find the number of elements of a list. (easy) *)

let rec length = function
  | [] -> 0
  | a :: rest -> 1 + length rest
;;

length ["a";"b";"c"];;
length [];;


(* 5. Reverse a list. (easy) *)

let rev l =
  let rec loop l ac =
    match l with
      | [] -> ac
      | a :: rest -> loop rest (a :: ac)
  in
    loop l []
;;

rev ["a";"b";"c"];;


(* 6. Find out whether a list is a palindrome. (easy) *)

let is_palindrome l =
  l = List.rev l
;;

is_palindrome ["x";"a";"m";"a";"x"];;
is_palindrome ["a";"b"];;


(* 7. Flatten a nested list structure. (medium) *)

type 'a node = One of 'a | Many of 'a node list;;

let rec flattenac liste =
  let rec loop l ac =
    match l with
      | [] -> ac
      | a :: rest ->
          match a with
            | One x -> loop rest (x :: ac)
            | Many y -> loop rest (loop y ac)
  in
    List.rev (loop liste [])
;;

flattenac [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;


(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress liste =
  match liste with
    | [] -> []
    | [a] -> [a]
    | a :: b :: rest ->
        if (a=b) 
        then compress (b :: rest)
        else a :: compress (b :: rest)
;;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;


(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack liste =
  let rec loop l current ac =
    match l with
      | [] -> []
      | [a] -> (a :: current) :: ac
      | a :: b :: rest ->
          if (a=b)
          then loop (b :: rest) (a :: current) ac
          else loop (b :: rest) [] ((a :: current)::ac)
  in
    List.rev (loop liste [] [])
;;

pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;


(* 10. Run-length encoding of a list. (easy) *)

let encode liste =
  let rec loop liste ac n=
    match liste with
      | [] -> ac
      | [a] -> (n+1,a) :: ac
      | a :: b :: rest ->
          if (a=b)
          then loop (b :: rest) ac (n+1)
          else loop (b :: rest) ((n+1,a) :: ac) 0
  in
    List.rev (loop liste [] 0)
;;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;


(* 11. Modified run-length encoding. (easy) *)

type 'a rle = One of 'a | Many of int * 'a;;

let encode2 liste =
  let a = encode liste in
  let rec loop l =
    match l with
      | [] -> []
      | x :: rest -> 
          match x with
            | (1,x) -> One x :: loop rest
            | (n,x) -> Many (n,x) :: loop rest
  in
    loop a
;;

encode2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;


(* 12. Decode a run-length encoded list. (medium) *)

let rec decode liste =
  match liste with
    | [] -> []
    | One x :: rest -> x :: decode rest
    | Many (n,x) :: rest -> 
        match n with
          | 1 -> x :: decode rest
          | n -> x :: decode (Many ((n-1),x) :: rest)
;;

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;


(* 13. Run-length encoding of a list (direct solution). (medium) *)

(* 14. Duplicate the elements of a list. (easy) *)

let rec duplicate liste =
  match liste with
    | [] -> []
    | a :: rest -> a :: a :: duplicate rest
;;

duplicate ["a";"b";"c";"c";"d"];;


(* 15. Replicate the elements of a list a given number of times. (medium) *)

let rec replicate liste n =
  match liste with
    | [] -> []
    | a :: rest ->
        let rec loop l k =
          match k with
            | 1 -> a :: replicate rest n
            | n -> a :: loop (a :: rest) (k-1)
        in 
          loop liste n
;;

replicate ["a";"b";"c"] 3;;


(* 16. Drop every N'th element from a list. (medium) *)

let rec drop liste n =
  match liste with
    | [] -> []
    | a :: rest ->
        let rec loop k =
          match k with
            | 1 -> drop rest n
            | k -> a :: loop (k-1)
        in
          loop n
;;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

let n = 4;;



















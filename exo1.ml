(** Exercice1 : **)

(* Basic types *)

let rec e1q1 n s =
  match n with
    | 0 -> ""
    | a -> s ^ (e1q1 (a-1) s)
;;

e1q1 0 "ab";;
e1q1 1 "ab";;
e1q1 2 "ab";;


let rec e1q4 n =
  match n with
    | 0 -> 0
    | a -> 1 + e1q4 (n/10)
;;

e1q4 0;;
e1q4 1;;
e1q4 10;;
e1q4 99;;
e1q4 999;;


let rec e1q2 n s =
  match n with
    | 0 -> ""
    | a -> string_of_int(a mod 10)^s^(e1q2 (a/10) s)
;;

e1q2 0 ":";;
e1q2 7 ":";;
e1q2 72 ":";;
e1q2 987 ":";;


let e1q3 n s =
  let rec loop n ac =
    match n with
      | 0 -> ac
      | a -> loop (a/10) (string_of_int(a mod 10) ^ s ^ ac)
  in
    loop n ""
;;

e1q3 987 "#";;



let rec e1q5 n b =
  match (n,b) with
    | (0,_) -> true
    | (a, true) -> ((a mod 10) mod 2) = 0 && e1q5 (a/10) b
    | (a, false) -> ((a mod 10) mod 2) = 1 && e1q5 (a/10) b
;;

e1q5 17359 false;;
e1q5 17369 false;;
e1q5 288062 true;;
e1q5 298462 true;;


let rec e1q6 n m =
  match (n,m) with
    | (0,0) -> true
    | (a,b) -> ( (a mod 10) <= (b mod 10) ) && e1q6 (n/10) (m/10)
;;

e1q6 0 0;;
e1q6 1234 2345;;
e1q6 1234 999;;
e1q6 999 1648;;
e1q6 333 1444;;



(* Curried functions *)

let curry3 f a b c = f(a,b,c);;

let ccat (a,b,c) = a^b^c;;
let unccat = curry3 ccat;;

let uncurry3 f(a,b,c) = f a b c;;
uncurry3 unccat;;

let apply3 (f1,f2,f3) (a,b,c) = (f1(a),f2(b),f3(c));;

let sort2 f g x =
  if (f(x) < g(x))
  then (f,g)
  else (g,f)
;;

let comp3 f g h x = f(g(h(x)));;


(* Pattern-matching *)

let ext_and a b c =
  match c with
    | true -> 
        (match (a,b) with
          | (true,true) -> true
          | (_,_) -> false)

    | false -> 
        (match (a,b) with
          | (true,true) -> false
          | (_,_) -> true)
;;

ext_and false true true;;
ext_and false true false;;
ext_and true true false;;
ext_and true true true;;


let encode_char = function
  | 'a' -> 'e'
  | 'A' -> 'E'

  | 'e' -> 'i'
  | 'E' -> 'I'

  | 'i' -> 'y'
  | 'I' -> 'Y'

  | 'o' -> 'a'
  | 'O' -> 'A'

  | 'u' -> 'o'
  | 'U' -> 'O'

  | 'y' -> 'u'
  | 'Y' -> 'U'

  | x -> x
;;

let encode s = String.map encode_char s;;

encode "I am a superhero.";;
encode "Les bons etudiants";;



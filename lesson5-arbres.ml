
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;

let rec depth atree =
  match atree with
    | Leaf(_) -> 0
    | Node(branche1,branche2) -> 1 + max (depth(branche1)) (depth(branche2))
;;

depth (Leaf 100);;
depth (Node (Leaf 100, Leaf 200));;


let rec build n x =
  if n = 0 then Leaf x
  else
    let sub = build (n-1) x in
      Node(sub,sub)
;;

let atree1 = build 5 20;;
depth atree1;;


let print_tree tos tree =
  let rec loop margin = function
    | Leaf x -> Printf.printf "___ %s\n%!" (tos x)
    | Node (a,b) ->
        Printf.printf "____" ;
        loop (margin ^ "|   ") a ;
        Printf.printf "%s|\n%s|" margin margin ;
        loop (margin ^ "    ") b
  in
    loop "   " tree
;;

print_tree string_of_int atree1;;



let build_fold n init f =
  let rec loop ac n =
    if n = 0
    then (Leaf ac, f(ac))
    else 

      let (sub1, ac1) = loop ac (n-1) in
      let (sub2, ac2) = loop ac1 (n-1) in

        (Node(sub1,sub2), ac2) 
  in 
  let (tree,_) = loop init n in
    tree

;;

let tree1 = build_fold 3 10 (fun x -> x+2);;
print_tree string_of_int tree1;;

let tree2 = build_fold 2 "o" ( fun x -> "(" ^ x ^ ")" );;
print_tree (fun x -> x) tree2;;



let tmap f atree =
  let rec loop t = 
    match t with
      | Leaf x -> Leaf (f x)
      | Node (a,b) -> 
          let sub1 = loop a in
          let sub2 = loop b in
            Node(sub1,sub2)
  in
    loop atree
;;

print_tree string_of_int (tmap (fun x -> x*2) tree1);;


let rec tfind pred atree =
  match atree with
    | Leaf x ->
        if pred(x) = true
        then Some x
        else None
    | Node (a,b) -> 
        match tfind pred a with
          | None -> tfind pred b
          | r -> r
;;



let rec contains x atree =
  match atree with
    | Leaf a -> 
        if (a=x)  
        then true
        else false
    | Node (b,c) -> contains x b || contains x c
;;


(*let contains x tree = tfind (fun y -> y=x) tree;;*)







































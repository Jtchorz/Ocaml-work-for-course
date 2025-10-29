(* To parse command-line arguments, checkout OCamls `Arg` module
   (https://ocaml.org/manual/5.1/api/Arg.html). To control the exit code on
   errors you can use `Arg.parse_argv` instead of `Arg.parse` *)
open Printf

type tree =
  | Node of string * tree * tree
  | Leaf of int

let prettys lst = 
   let rec loop lst =
      match lst with
      | [] -> ""
      | [x] -> (*string_of_int*) x
      | x :: l -> (*string_of_int*) x ^ "\n" ^ loop l
   in 
      loop lst

let prettyi lst = 
   let rec loop lst =
      match lst with
      | [] -> ""
      | [x] -> string_of_int x
      | x :: l -> string_of_int x ^ "," ^ loop l
   in 
      loop lst

let rec pre_order tree =
   match tree with
   | Node(s,t1,t2) -> List.append (("Node:"^s) :: pre_order t1) (pre_order t2)
   | Leaf(i) -> ["Leaf:" ^ string_of_int i]


let rec in_order tree =
   match tree with
   | Node(s,t1,t2) -> List.append (List.append (in_order t1) ["Node:"^s]) (in_order t2)
   | Leaf(i) -> ["Leaf:" ^ string_of_int i]

let rec post_order tree =
   match tree with
   | Node(s,t1,t2) -> List.append (List.append (post_order t1) (post_order t2)) ["Node:"^s]
   | Leaf(i) -> ["Leaf:" ^ string_of_int i]

let rec list tree =
   match tree with
   | Node(s,t1,t2) -> List.append (list t1) (list t2)
   | Leaf (i) -> [i]

let size tree = List.length (pre_order tree)

let rec depth tree =
   match tree with
   | Node(s,t1,t2) -> (Int.max (depth t1) (depth t2)) + 1
   | Leaf(i) -> 0


let tr = Node("a",Node("b",Leaf(1),Leaf(2)),Leaf(3))

   let read_input = 
      let rec work str =
            match (String.split_on_char ':' str) with
            | [name; s] when name = "Leaf" -> 
               (try (Leaf (int_of_string s)) with 
               | Failure(_) -> exit 1)
            | [name; s] when name = "Node" -> 
               let ltree = (work (input_line stdin)) in 
               let rtree = (work (input_line stdin)) in 
               Node(s,ltree,rtree)
            | _ -> exit 1
        
      in
      try work (input_line stdin) with
       |End_of_file -> exit 1
let main =
   let len = Array.length Sys.argv in 
   if len <> 2 then 
      exit 1
   else
      let t = read_input in let s =
      match (Sys.argv.(1)) with
      | "pre-order" -> prettys (pre_order t)
      | "in-order" -> prettys (in_order t)
      | "post-order" -> prettys (post_order t)
      | "list" -> prettyi (list t)
      | "size" -> string_of_int (size t)
      | "depth" -> string_of_int (depth t)
      | _ -> exit 1
      in printf "%s\n" s
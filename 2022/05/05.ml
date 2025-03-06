let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then [] else x :: take (n-1) xs
in

let rec drop n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then (x::xs) else drop (n-1) xs
in

let rec find_cutoff_index i ls =
  match ls with
  | []    -> assert false
  | l::ls -> if (String.get (String.trim l) 0) = '['
             then find_cutoff_index (i+1) ls else i
in

let rec remove_whitespace s =
  List.fold_right (fun s acc -> s ^ acc) (String.split_on_char ' ' s) ""
in

let rec eval_index_line s =
  [1] @
    List.init ((String.length (remove_whitespace s))-1) (fun i -> 1 + (i+1) * 4)
in

let rec parse_stack idx ls = 
  match ls with
  | []    -> []
  | l::ls -> match String.get l idx with
             | ' ' -> parse_stack idx ls
             | c   -> c :: parse_stack idx ls
in

let parse_move_line s =
  let ss = String.split_on_char ' ' s in
  (int_of_string (List.nth ss 1),
   int_of_string (List.nth ss 3)-1,
   int_of_string (List.nth ss 5)-1)
in

let parse_lines ls =
  let i = find_cutoff_index 0 ls in
  let idxs = eval_index_line (List.nth ls i) in
  let stack_lines = take i ls in
  let stacks = Array.init (List.length idxs)
                 (fun i -> parse_stack (List.nth idxs i) stack_lines) in
  let moves = List.map parse_move_line (drop (i+2) ls) in
  (stacks,moves)
in

let eval_move f stacks (n,fidx,tidx) =
  let fstack = Array.get stacks fidx in
  let tstack = Array.get stacks tidx in
  Array.set stacks fidx (drop n fstack);
  Array.set stacks tidx (f (take n fstack) @ tstack);
in

let ls = read_lines () in
let (stacks1,moves) = parse_lines ls in
let stacks2 = Array.copy stacks1 in
List.iter (eval_move List.rev stacks1) moves;
List.iter (eval_move Fun.id stacks2) moves;
Printf.printf "Part 1: ";
Array.iter (fun stack -> Printf.printf "%c" (List.hd stack)) stacks1;
Printf.printf "\n";
Printf.printf "Part 2: ";
Array.iter (fun stack -> Printf.printf "%c" (List.hd stack)) stacks2;
Printf.printf "\n";

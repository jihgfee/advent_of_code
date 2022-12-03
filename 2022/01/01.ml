
let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let rec print_lines ls =
  match ls with
  | [] -> ()
  | l::ls' -> print_string (l ^ "\n"); print_lines ls'
in

let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum xs
in

let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 0 then [] else x :: take (n-1) xs
in

let ls = read_lines () in
let (_,xxs) = List.fold_left (fun (c,ls) l -> if l = "" then ([],c :: ls) else ((int_of_string l) :: c,ls)) ([],[]) ls in
let sums = List.map sum xxs in
let sorted_sum = List.sort (fun x y -> compare x y * -1) sums in
let sum = sum (take 3 sorted_sum) in
Printf.printf "%i" sum

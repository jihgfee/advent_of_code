
let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let parse_line s =
  let [s1;s2] = String.split_on_char ',' s in
  let [i11;i12] = List.map int_of_string (String.split_on_char '-' s1) in
  let [i21;i22] = List.map int_of_string (String.split_on_char '-' s2) in
  ((i11,i12),(i21,i22))
in

let check_inside ((i11,i12),(i21,i22)) =
  i21 <= i11 && i12 <= i22
in

let check_contains (p1,p2) =
  check_inside (p1,p2) || check_inside (p2,p1)
 in

let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum xs
in

let ls = read_lines () in
let xs = List.map parse_line ls in
let result = sum (List.map (fun x -> if check_contains x then 1 else 0) xs) in
Printf.printf "%i\n" result

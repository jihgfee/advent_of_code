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

let check_overlaps ((i11,i12),(i21,i22)) =
  if i11 <= i21 then i21 <= i12 else i11 <= i22
in

let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum xs
in

let ls = read_lines () in
let xs = List.map parse_line ls in
let result = sum (List.map (fun x -> if check_overlaps x then 1 else 0) xs) in
Printf.printf "%i\n" result

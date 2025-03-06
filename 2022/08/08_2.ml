let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let explode s = List.init (String.length s) (String.get s) in
let implode cs = String.of_seq (List.to_seq cs) in

let char_to_int c = int_of_string (String.make 1 c) in

let iteri_matrix f g xxs =
  Array.iteri (fun i xs -> Array.iteri (fun j x -> f i j x) xs; g i xs) xxs
in

let parse_lines ls =
  let n = String.length (List.hd ls) in
  let m = List.length ls in
  let left = Array.make_matrix n m false in
  let right = Array.make_matrix n m false in
  let top = Array.make_matrix n m false in
  let bottom = Array.make_matrix n m false in
  let forest = Array.make_matrix n m 0 in
  
  List.iteri (fun i l ->
    List.iteri (fun j c -> forest.(i).(j) <- char_to_int c)
      (explode l)) ls;

  (left,right,top,bottom,forest)
in

let rec eval_left forest seen max i j =
  if i >= Array.length forest then eval_left forest seen (-1) 0 (j+1) else
  if j >= Array.length forest then () else
  let cur = forest.(i).(j) in
  if (max < cur) then (seen.(i).(j) <- true; eval_left forest seen cur (i+1) j)
  else eval_left forest seen max (i+1) j
in

let rec eval_right forest seen max i j =
  if i < 0 then eval_right forest seen (-1) ((Array.length forest)-1) (j+1) else
  if j >= Array.length forest then () else
  let cur = forest.(i).(j) in
  if max < cur then (seen.(i).(j) <- true; eval_right forest seen cur (i-1) j)
  else eval_right forest seen max (i-1) j
in

let rec eval_top forest seen max i j =
  if j >= Array.length forest then eval_top forest seen (-1) (i+1) 0 else
  if i >= Array.length forest then () else
  let cur = forest.(i).(j) in
  if max < cur then (seen.(i).(j) <- true; eval_top forest seen cur i (j+1))
  else eval_top forest seen max i (j+1)
in

let rec eval_bot forest seen max i j =
  if j < 0 then eval_bot forest seen (-1) (i+1) ((Array.length forest.(0))-1) else
  if i >= Array.length forest then () else
  let cur = forest.(i).(j) in
  if max < cur then (seen.(i).(j) <- true; eval_bot forest seen cur i (j-1))
  else eval_bot forest seen max i (j-1)
in

let rec count_visible left right top bot i j acc =
  if i >= Array.length left then count_visible left right top bot 0 (j+1) acc else
  if j >= Array.length left then acc else
  if (left.(i).(j) || right.(i).(j) || top.(i).(j) || bot.(i).(j))
  then count_visible left right top bot (i+1) j (acc + 1)
  else count_visible left right top bot (i+1) j acc
in

let ls = read_lines () in
let (left,right,top,bot,forest) = parse_lines ls in

(* iteri_matrix (fun i j x -> Printf.printf "%i" x) *)
(*   (fun _ _ -> Printf.printf "\n") forest; *)

eval_left forest left (-1) 0 0;
eval_right forest right (-1) ((Array.length forest)-1) 0;
eval_top forest top (-1) 0 0;
eval_bot forest bot (-1) 0 ((Array.length forest)-1);
Printf.printf "%i\n" (count_visible left right top bot 0 0 0)

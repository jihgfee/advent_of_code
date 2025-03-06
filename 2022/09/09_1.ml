
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module PairSet = Set.Make(IntPairs);;

type direction = Left | Right | Up | Down;;

let rec read_lines _ =
  try
    let l = read_line () in
    l :: read_lines ()
  with End_of_file ->
    []
in

let explode s = List.init (String.length s) (String.get s) in

let parse_direction c =
  match c with
  | 'L' -> Left
  | 'R' -> Right
  | 'U' -> Up
  | 'D' -> Down
  | _   -> assert false
in

let rec parse_lines ls =
  match ls with
  | []    -> []
  | l::ls -> let [d;n] = String.split_on_char ' ' l in
             (parse_direction (List.hd (explode d)), int_of_string n) :: parse_lines ls
in

let direction_delta d =
  match d with
  | Right -> (1,0)
  | Left  -> (-1,0)
  | Up    -> (0,1)
  | Down  -> (0,-1)
in

let check_tail (hx,hy) (tx,ty) =
  let dx = Int.abs (hx - tx) in
  let dy = Int.abs (hy - ty) in
  if dx = 0 then dy > 1
  else if dy = 0 then dx > 1
  else dx + dy > 2
in

let rec parse_instruction visited (hx,hy) tl (d,n) =
  if n = 0 then (visited, (hx,hy), tl) else
  let (dx,dy) = direction_delta d in
  let (hx',hy') = (hx+dx,hy+dy) in
  let (tx',ty') = (if check_tail (hx',hy') tl then (hx,hy) else tl) in
  parse_instruction (PairSet.add (tx',ty') visited) (hx',hy') (tx',ty') (d,n-1)
in

let rec parse_instructions visited hd tl ds =
  match ds with
  | [] -> visited
  | d::ds -> let (visited', hd', tl') = parse_instruction visited hd tl d in
             parse_instructions visited' hd' tl' ds
in

let ls = read_lines () in
let ds = parse_lines ls in
let visited = parse_instructions (PairSet.(empty |> add (0,0))) (0,0) (0,0) ds in

Printf.printf "%i\n" (PairSet.cardinal visited)

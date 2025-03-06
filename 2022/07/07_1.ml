
type cmd = Cd | Ls;;

module MyMap = Map.Make(String);;

let execute_ls (cur_dir,) ls =
  match ls with
  | [] -> ()
  | l::ls -> if String.get 0 l = '$' then cur_dir
             else let [s1;s2] = String.split_on_char ' ' l in
                  cur_dir
in

let execute_command cmd =
  match cmd with
  | cd => execute_cd
  | ls => execute_ls
in

let m = MyMap.empty in
()

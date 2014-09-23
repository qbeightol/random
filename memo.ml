
let memoized_fs = Hashtbl.create 10

(*looks up (and returns) the value of f x, adding f x to the table if not 
  found. Fails if f hasn't been memoized *)
let (/>/) x f  = 
  let f_table = Hashtbl.find memoized_fs f in
    if Hashtbl.mem f_table x then 
      Hashtbl.find f_table x
    else 
      let result = f x in
        Hashtbl.add f_table x result;
        result

(*like |> but enables memoization*)
let (/>) x f = 
  if Hashtbl.mem memoized_fs f then x />/ f
  else f x

let rec square x = 
  if x = 0 then 0 else  ((x - 1) /> square) + 2*x - 1

let memoize f = 
  if Hashtbl.mem memoized_fs f then 
    failwith "already memoized"
  else 
    Hashtbl.add memoized_fs f (Hashtbl.create 10)

(*
let to_list () = 
  let f k v acc = (k,v)::acc in
    List.map (fun (f,v) -> (f, Hashtbl.fold f v [])) (Hashtbl.fold f memoized_fs [])
*)



open Lang_maths
open Lang_parser

type maths = Maths.math list

let with_cache : (string -> maths) -> string -> maths =
  let cache = Hashtbl.create 101 in
  let with_cache f s =
    try Hashtbl.find cache s with Not_found ->
      (* Printf.printf "Parsing %S\n%!" s; *)
      let res = f s in
      (* Printf.printf "Done.\n%!"; *)
      Hashtbl.add cache s res; res
  in with_cache

let v : string -> maths = with_cache (fun s -> v2m (parse_valu s))
let t : string -> maths = with_cache (fun s -> t2m (parse_term s))
let c : string -> maths = with_cache (fun s -> c2m (parse_ctxt s))
let s : string -> maths = with_cache (fun s -> s2m (parse_stac s))
let p : string -> maths = with_cache (fun s -> p2m (parse_proc s))
let f : string -> maths = with_cache (fun s -> f2m (parse_form s))
let o : string -> maths = with_cache (fun s -> o2m (parse_ordi s))

let subs : string -> maths = with_cache (fun s -> subs2m (parse_subs s))

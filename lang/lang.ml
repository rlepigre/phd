open Lang_maths
open Lang_parser

type maths = Maths.math list

let v : string -> maths = fun s -> v2m (parse_valu s)
let t : string -> maths = fun s -> t2m (parse_term s)
let c : string -> maths = fun s -> c2m (parse_ctxt s)
let s : string -> maths = fun s -> s2m (parse_stac s)
let p : string -> maths = fun s -> p2m (parse_proc s)
let f : string -> maths = fun s -> f2m (parse_form s)

let subs : string -> maths = fun s -> subs2m (parse_subs s)

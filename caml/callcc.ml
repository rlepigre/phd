module CC :
  sig
    type 'a cont
    val callcc : ('a cont -> 'a) -> 'a
    val throw  : 'a cont -> 'a -> 'b
  end =
  struct
    type 'a cont
    let callcc _  = Obj.magic (Some None)
    let throw _ _ = Obj.magic (Some None)
  end

open CC

let id x = x
let no x = ()

let c = callcc (fun k -> (id, (fun f -> throw k (f, no))))
(*
let _ = print_string ((fst c) "Hello world\n"); (snd c) (fun x -> x+2)
*)

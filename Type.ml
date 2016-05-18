open Typography
open DefaultFormat
open Maths
open Decap
open KAM

type form =
  | FFunc of form * form        (* A ⇒ B *)
  | FGrou of form               (* (A)   *)
  | FMeta of vari               (* A, B  *)
  | FEPrd of (vari * form) list (* {l_1 : A_1 ; ⋯ ; l_n : A_n} *)
  | FESum of (vari * form) list (* {C_1 : A_1 ; ⋯ ; C_n : A_n} *)

let fmeta = vari ["A"; "B"; "C"]

let rec ffunc = function
  | []    -> assert false
  | [a]   -> a
  | a::fs -> FFunc(a, ffunc fs)

type fprio = FAtom | FFull

let parser form prio =
  | a:fmeta                                  when prio = FAtom -> FMeta(a)
  | '(' a:(form FFull) ')'                   when prio = FAtom -> FGrou(a)
  | a:(form FAtom) fs:{"⇒" b:(form FAtom)}*$ when prio = FFull -> ffunc (a::fs)
  | '{' fs:{label ':' (form FFull) ';'}* '}' when prio = FAtom -> FEPrd(fs)
  | '[' fs:{const ':' (form FFull) ';'}* ']' when prio = FAtom -> FESum(fs)

let form = form FFull

let parse_form = parse form

let rec f2m : form -> Maths.math list = function
  | FFunc(a,b) -> bin' 2 "⇒" (f2m a, f2m b)
  | FGrou(a)   -> (str "(") @ (f2m a) @ (str ")")
  | FMeta(a)   -> vari2m a
  | FEPrd(ls)  -> assert false
  | FESum(ls)  -> assert false

let f : string -> Maths.math list = fun s -> f2m (parse_form s)

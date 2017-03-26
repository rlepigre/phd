(* This file is distributed as part of Rodolphe Lepigre's PhD thesis (see *)
(* http://lepigre.fr/these) and it is only interesting as such.  It gives *)
(* a partial proof of Lemmas 2.6.45 and 2.6.46 using OCaml's exhaustivity *)
(* checker for pattern matching.                                          *)
(*                                                                        *)
(* Proof checked using OCaml 4.03.0 with the command:                     *)
(*   $> ocaml classification.ml                                           *)
(*                                                                        *)
(* It generates no output (and thus no warning).                          *)

type vari = string
type cons = string
type name = string

type value =
  | VVar of vari                       (* λ-variable    *)
  | VAbs of vari * term                (* λ-abstraction *)
  | VCns of cons * value               (* Constructor   *)
  | VRec of (name * value) list        (* Record        *)
  | VBox                               (* Box           *)
and  term =
  | TVar of vari                       (* Term variable *)
  | TVal of value                      (* Value         *)
  | TApp of term * term                (* Application   *)
  | TAbs of vari * term                (* μ-abstraction *)
  | TNam of stack * term               (* Named term    *)
  | TPrj of value * name               (* Projection    *)
  | TCas of value * (cons * term) list (* Case analysis *)
  | TRec of term * value               (* Fixpoint      *)
  | TIsR of value * term               (* R instruction *)
  | TDel of value * value              (* δ instruction *)
and  stack =
  | SVar of vari                       (* μ-variable    *)
  | SEmp                               (* Empty stack   *)
  | SPsh of value * stack              (* Pused value   *)
  | SFrm of term * stack               (* Stack frame   *)

type process = term * stack

type sort =
  | Reduc (* Can be reduced.  *)
  | Final (* Final process.   *)
  | Delta (* δ-like process.  *)
  | Stuck (* Stuck process.   *)
  | Block (* Blocked process. *)

let classify : process -> sort = function
  (* Final. *)
  | (TVal(v)           , SEmp      ) -> Final
  (* δ-like. *)
  | (TDel(v,w)         , pi        ) -> Delta
  (* 14 forms of stuck processes. *)
  | (TPrj(VCns(c,v),l) , pi        ) -> Stuck
  | (TPrj(VAbs(x,t),l) , pi        ) -> Stuck
  | (TVal(VCns(c,v))   , SPsh(w,pi)) -> Stuck
  | (TVal(VRec(m))     , SPsh(w,pi)) -> Stuck
  | (TCas(VAbs(x,t),m) , pi        ) -> Stuck
  | (TCas(VRec(m1),m2) , pi        ) -> Stuck
  | (TPrj(VRec(m),l)   , pi        ) when not (List.mem_assoc l m) -> Stuck
  | (TCas(VCns(c,v),m) , pi        ) when not (List.mem_assoc c m) -> Stuck
  | (TIsR(VAbs(x,t),u) , pi        ) -> Stuck
  | (TIsR(VCns(c,v),u) , pi        ) -> Stuck
  | (TIsR(VBox,u)      , pi        ) -> Stuck
  (* 7 forms of processes that are blocked, but not stuck. *)
  | (TPrj(VVar(x),l)   , pi        ) -> Block
  | (TVal(VVar(x))     , SPsh(v,pi)) -> Block
  | (TCas(VVar(x),l)   , pi        ) -> Block
  | (TVal(VVar(x))     , SFrm(t,pi)) -> Block
  | (TVar(a)           , pi        ) -> Block
  | (TIsR(VVar(x),u)   , pi        ) -> Block
  | (TVal(v)           , SVar(a)   ) -> Block
  (* 13 forms of processes that reduces. *)
  | (TVal(VBox)        , SFrm(t,pi)) -> Reduc
  | (TVal(VBox)        , SPsh(v,pi)) -> Reduc
  | (TCas(VBox,m)      , pi        ) -> Reduc
  | (TPrj(VBox,l)      , pi        ) -> Reduc
  | (TApp(t,u)         , pi        ) -> Reduc
  | (TVal(v)           , SFrm(t,pi)) -> Reduc (* v not Box nor VVar(x) *)
  | (TVal(VAbs(x,t))   , SPsh(v,pi)) -> Reduc
  | (TAbs(a,t)         , pi        ) -> Reduc
  | (TNam(r,t)         , pi        ) -> Reduc
  | (TPrj(VRec(m),l)   , pi        ) -> Reduc (* l in m *)
  | (TCas(VCns(c,v),m) , pi        ) -> Reduc (* c in m *)
  | (TRec(t,v)         , pi        ) -> Reduc
  | (TIsR(VRec(m),u)   , pi        ) -> Reduc

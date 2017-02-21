type vari = string
type cons = string
type name = string

type valu =
  | VVar of vari
  | VAbs of vari * term
  | VCns of cons * valu
  | VRec of (name * valu) list
  | VBox
and  term =
  | TVar of vari
  | TVal of valu
  | TApp of term * term
  | TAbs of vari * term
  | TNam of stac * term
  | TPrj of valu * name
  | TCas of valu * (cons * term) list
  | TRec of term * valu
  | TIsR of valu * term
  | TIsF of valu * term
  | TDel of valu * valu
and  stac =
  | SVar of vari
  | SEmp
  | SPsh of valu * stac
  | SFrm of term * stac

type proc = term * stac

type sort =
  | Reduc
  | Final
  | Delta
  | Stuck
  | Block

let classify : proc -> sort = function
  (* Processes that reduces. *)
  | (TApp(t,u)         , pi        ) -> Reduc
  | (TVal(v)           , SFrm(t,pi)) -> Reduc
  | (TVal(VAbs(x,t))   , SPsh(v,pi)) -> Reduc
  | (TAbs(a,t)         , pi        ) -> Reduc
  | (TNam(r,t)         , pi        ) -> Reduc
  | (TPrj(VRec(m),l)   , pi        ) when List.mem_assoc l m -> Reduc
  | (TCas(VCns(c,v),m) , pi        ) when List.mem_assoc c m -> Reduc
  | (TRec(t,v)         , pi        ) -> Reduc
  | (TIsR(VRec(m),u)   , pi        ) -> Reduc
  | (TIsF(VAbs(x,t),u) , pi        ) -> Reduc
  | (TVal(VBox)        , SPsh(v,pi)) -> Reduc
  | (TCas(VBox,m)      , pi        ) -> Reduc
  | (TPrj(VBox,l)      , pi        ) -> Reduc
  (* Final. *)
  | (TVal(v)           , SEmp      ) -> Final
  (* δ-like. *)
  | (TDel(v,w)         , pi        ) -> Delta
  (* Stuck. *)
  | (TPrj(VCns(c,v),l) , pi        ) -> Stuck
  | (TPrj(VAbs(x,t),l) , pi        ) -> Stuck
  | (TVal(VCns(c,v))   , SPsh(w,pi)) -> Stuck
  | (TVal(VRec(m))     , SPsh(w,pi)) -> Stuck
  | (TCas(VAbs(x,t),m) , pi        ) -> Stuck
  | (TCas(VRec(m1),m2) , pi        ) -> Stuck
  | (TPrj(VRec(m),l)   , pi        ) -> Stuck
  | (TCas(VCns(c,v),m) , pi        ) -> Stuck
  | (TIsR(VAbs(x,t),u) , pi        ) -> Stuck
  | (TIsR(VCns(c,v),u) , pi        ) -> Stuck
  | (TIsR(VBox,u)      , pi        ) -> Stuck
  | (TIsF(VRec(m),u)   , pi        ) -> Stuck
  | (TIsF(VCns(c,v),u) , pi        ) -> Stuck
  | (TIsF(VBox,u)      , pi        ) -> Stuck
  | (TCas(VVar(x),[])  , pi        ) -> Stuck
  (* Blocked but not stuck. *)
  | (TPrj(VVar(x),l)   , pi        ) -> Block
  | (TVal(VVar(x))     , SPsh(v,pi)) -> Block
  | (TCas(VVar(x),_::_), pi        ) -> Block
  | (TVar(a)           , pi        ) -> Block
  | (TIsR(VVar(x),u)   , pi        ) -> Block
  | (TIsF(VVar(x),u)   , pi        ) -> Block
  | (TVal(v)           , SVar(a)   ) -> Block

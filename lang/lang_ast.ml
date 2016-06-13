(* Type of a variable, or meta variable. Name and optional index. *)
type vari = string * string option

(* Several possible way of displaying a list of things. *)
type 'a fset =
  (* Explicit list of all the elements. *)
  | Expli of 'a list
  (* Implicit list with an elipsis (None) or an index set (Some ...). *)
  | Impli of 'a * (string * vari * bool) option

(* Type of a variable substitution. Boolean to true means capture avoiding. *)
type 'a vsub = vari * bool * 'a

(* Types for all the syntactic entities. *)
type valu =
  | VVari of vari                      (* x, y   *)
  | VMeta of vari                      (* v, w   *)
  | VLAbs of vari * term               (* λx t   *)
  | VCons of vari * valu               (* C[v]   *)
  | VReco of (vari * valu) fset        (* {l₁ = v₁; l₂ = v₂;} *)
  | VGrou of valu                      (* (v)    *)
  | VSubs of valu * subs               (* vσ     *)
and  term =
  | TVari of vari                      (* a, b   *)
  | TMeta of vari                      (* t, u   *)
  | TValu of valu                      (* v, w   *)
  | TGrou of term                      (* (t)    *)
  | TAppl of term * term               (* t u    *)
  | TSave of vari * term               (* μα t   *)
  | TRest of stac * term               (* [π]t   *)
  | TCtxt of ctxt * term               (* E[t]   *)
  | TProj of valu * vari               (* v.l    *)
  | TUnit of valu                      (* U_v    *)
  | TDelt of valu * valu               (* δ(v,w) *)
  | TFixp of term * valu               (* Y(t,v) *)
  | TCase of valu * patt fset          (* [v | C₁[x₁] → t₁ | C₂[x₂] → t₂] *)
  | TSubt of term * subs               (* tσ     *)
and  stac =
  | SEmpt                              (* ε      *)
  | SVari of vari                      (* α, β   *)
  | SMeta of vari                      (* π, ρ   *)
  | SPush of valu * stac               (* v.π    *)
  | SFram of term * stac               (* [t]π   *)
  | SGrou of stac                      (* (π)    *)
  | SSubs of stac * subs               (* sσ     *)
and  proc =
  | PMeta of vari                      (* p, q   *)
  | PProc of term * stac               (* t ∗ π  *)
  | PGrou of proc                      (* (p)    *)
  | PSubs of proc * subs               (* pσ     *)
and  ctxt =
  | CHole                              (* [-]    *)
  | CPlug of ctxt * ctxt               (* E[F]   *)
  | CMeta of vari                      (* E, F   *)
  | CGrou of ctxt                      (* (E)    *)
  | CLAbs of vari * ctxt               (* λx E   *)
  | CAppL of ctxt * term               (* E t    *)
  | CAppR of term * ctxt               (* t E    *)
and  subs =
  | SubsV of subs * valu vsub          (* σ[x≔v] *)
  | SubsT of subs * term vsub          (* σ[a≔t] *)
  | SubsS of subs * stac vsub          (* σ[α≔π] *)
  | SubsF of subs * form vsub          (* σ[X≔A] *)
  | SubsM of vari                      (* σ, ρ   *)
  | NoSub                              (* ∅      *)
and patt = vari * vari * term
and form =
  | FMeta of vari                      (* A, B    *)
  | FVari of vari                      (* χ       *)
  | FLamb of vari * form               (* (χ ↦ A) *)
  | FSubs of form * subs               (* Aσ      *)
  | FAppl of form * form               (* A(B)    *)
  | FFunc of form * form               (* A ⇒ B   *)
  | FGrou of form                      (* (A)     *)
  | FProd of (vari * form) fset        (* {l_1 : A_1 ; ⋯ ; l_n : A_n} *)
  | FDSum of (vari * form) fset        (* [C_1 : A_1 ; ⋯ ; C_n : A_n] *)
  | FUniv of vari * vari option * form (* ∀χ^s A  *)
  | FExis of vari * vari option * form (* ∃χ^s A  *)
  | FLFix of vari * form               (* μX A    *)
  | FGFix of vari * form               (* νX A    *)
  | FMemb of term * form               (* t∈A     *)
  | FRest of form option * equa        (* A | t≡u *)
and equa = term * term

(* Smart constructor for multiple application of terms. *)
let tappl = List.fold_left (fun t u -> TAppl(t,u))

(* Smart constructor for several arrow types. *)
let rec ffunc = function
  | []    -> assert false
  | [a]   -> a
  | a::fs -> FFunc(a, ffunc fs)

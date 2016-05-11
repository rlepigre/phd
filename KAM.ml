open Typography
open DefaultFormat
open Maths
open Decap

type vari = string * string option

type valu =
  | VVari of vari               (* x, y   *)
  | VMeta of vari               (* v, w   *)
  | VLAbs of vari * term        (* λx t   *)
  | VCons of vari * valu        (* C[v]   *)
  | VERec of (vari * valu) list (* {l₁ = v₁; l₂ = v₂;} *)
  | VIRec of vari * vari        (* {... l_i = v_i ...} *)
  | VGrou of valu               (* (v)    *)
and  term =
  | TVari of vari               (* a, b   *)
  | TMeta of vari               (* t, u   *)
  | TValu of valu
  | TGrou of term               (* (t)    *)
  | TAppl of term * term        (* t u    *)
  | TSave of vari * term        (* μα t   *)
  | TRest of stac * term        (* [π]t   *)
  | TCtxt of ctxt * term        (* E[t]   *)
  | TVSub of term * (vari * bool * valu) (* t[x≔v] of t[x←v] *)
  | TSSub of term * (vari * bool * stac) (* t[α≔π] of t[α←π] *)
  | TProj of valu * vari        (* v.l    *)
  | TUnit of valu               (* U_v    *)
  | TFixp of term * valu        (* Y(t,v) *)
  | TECas of valu * (vari * valu) list (* [v | C₁ → v₁ | C₂ → v₂] *)
  | TICas of valu * vari * vari (* [v | ... C_i → v_i ...] *)
and  ctxt =
  | CHole                       (* [-]    *)
  | CPlug of ctxt * ctxt        (* E[F]   *)
  | CMeta of vari               (* E, F   *)
  | CGrou of ctxt               (* (E)    *)
  | CLAbs of vari * ctxt        (* λx E   *)
  | CAppL of ctxt * term        (* E t    *)
  | CAppR of term * ctxt        (* t E    *)
and  stac =
  | SEmpt                       (* ε      *)
  | SVari of vari               (* α, β   *)
  | SMeta of vari               (* π, ρ   *)
  | SPush of valu * stac        (* v.π    *)
  | SFram of term * stac        (* [t]π   *)
and  proc =
  | PMeta of vari               (* p, q   *)
  | PProc of term * stac        (* t ∗ π  *)

let parser index =
  | "₀" -> "0" | "₁" -> "1" | "₂" -> "2" | "₃" -> "3" | "₄" -> "4"
  | "₅" -> "5" | "₆" -> "6" | "₇" -> "7" | "₈" -> "8" | "₉" -> "9"
  | "i" -> "i" | "j" -> "j" | "k" -> "k"

let parser vari p = x:p i:index? _:relax
let vari ns = vari (alternatives (List.map (fun n -> string n n) ns))

let vvari = vari ["x"; "y"; "z"]
let vmeta = vari ["v"; "w"]
let tvari = vari ["a"; "b"; "c"]
let tmeta = vari ["t"; "u"; "Ω"]
let cmeta = vari ["E"; "F"]
let svari = vari ["α"; "β"; "γ"]
let smeta = vari ["π"; "ρ"]
let pmeta = vari ["p"; "q"]
let const = vari ["C"]
let label = vari ["l"]

let appl = List.fold_left (fun t u -> TAppl(t,u))

type vprio = Simp | Proj | Comp
type tprio = Atom | Subs | Appl

let parser valu prio =
  | x:vvari                        when prio = Simp -> VVari(x)
  | v:vmeta                        when prio = Simp -> VMeta(v)
  | "λ" x:vvari t:(term Appl)      when prio = Comp -> VLAbs(x,t)
  | c:const '[' v:(valu Comp) ']'  when prio = Comp -> VCons(c,v)
  | "{⋯" l:label "=" v:vmeta "⋯}"  when prio = Simp -> VIRec(l,v)
  | "{" ls:{l:label "=" v:(valu Comp) ";"}* "}"
                                   when prio = Simp -> VERec(ls)
  | '(' v:(valu Comp) ')'          when prio = Proj -> VGrou(v)
  | v:(valu Simp)                  when prio = Comp -> v
  | v:(valu Simp)                  when prio = Proj -> v
and        term prio =
  | a:tvari                        when prio = Atom -> TVari(a)
  | t:tmeta                        when prio = Atom -> TMeta(t)
  | v:(valu Comp)                  when prio = Atom -> TValu(v)
  | '(' t:(term Appl) ')'          when prio = Atom -> TGrou(t)
  | t:(term Subs) ts:(term Subs)*$ when prio = Appl -> appl t ts
  | "μ" a:svari t:(term Appl)      when prio = Atom -> TSave(a,t)
  | '[' s:stac ']' t:(term Appl)   when prio = Atom -> TRest(s,t)
  | t:(term Atom) '[' x:vvari s:{"≔" -> true | "←" -> false} v:(valu Comp) ']'
                                   when prio = Subs -> TVSub(t,(x,s,v))
  | t:(term Atom) '[' x:svari s:{"≔" -> true | "←" -> false} pi:stac ']'
                                   when prio = Subs -> TSSub(t,(x,s,pi))
  | c:(ctxt Atom) '[' t:(term Appl) ']'
                                   when prio = Atom -> TCtxt(c,t)
  | v:(valu Proj) '.' l:label      when prio = Atom -> TProj(v,l)
  | "U(" v:(valu Comp) ")"         when prio = Atom -> TUnit(v)
  | "Y(" t:(term Appl) "," v:(valu Comp) ")"
                                   when prio = Atom -> TFixp(t,v)
  | '[' v:(valu Comp) ls:{'|' c:const "→" w:(valu Comp)}+ ']'
                                   when prio = Atom -> TECas(v,ls)
  | '[' v:(valu Comp) '|' "⋯" c:const "→" w:vmeta "⋯" ']'
                                   when prio = Atom -> TICas(v,c,w)
  | t:(term Atom)                  when prio = Subs -> t
and        ctxt prio =
  | "[]"                           when prio = Atom -> CHole
  | e:(ctxt Atom) '[' f:(ctxt Appl) ']'
                                   when prio = Atom -> CPlug(e,f)
  | e:cmeta                        when prio = Atom -> CMeta(e)
  | '(' e:(ctxt Appl) ')'          when prio = Atom -> CGrou(e)
  | "λ" x:vvari e:(ctxt Appl)      when prio = Atom -> CLAbs(x,e)
  | e:(ctxt Subs) t:(term Atom)    when prio = Appl -> CAppL(e,t)
  | t:(term Atom) e:(ctxt Subs)    when prio = Appl -> CAppR(t,e)
  | e:(ctxt Atom)                  when prio = Subs -> e
  | e:(ctxt Subs)                  when prio = Appl -> e
and        stac =
  | "ε"                                             -> SEmpt
  | a:svari                                         -> SVari(a)
  | s:smeta                                         -> SMeta(s)
  | v:(valu Comp) "·" s:stac                        -> SPush(v,s)
  | '[' t:(term Appl) ']' s:stac                    -> SFram(t,s)
and        proc =
  | p:pmeta                                         -> PMeta(p)
  | t:(term Appl) "∗" s:stac                        -> PProc(t,s)

let term = term Appl
let ctxt = ctxt Appl
let valu = valu Comp

let parse g =
  let parse = parse_string g (blank_regexp ''[ ]*'') in
  handle_exception parse

let parse_valu = parse valu
let parse_term = parse term
let parse_ctxt = parse ctxt
let parse_stac = parse stac
let parse_proc = parse proc

let str s = [Maths.Ordinary (Maths.node (Maths.glyphs s))]
let asana n i = [Maths.Ordinary (Maths.node (MathFonts.asana n i))]
let bin p n (l, r) =
  [bin p (Normal(false, Maths.node n, false)) l r]
let bin' p n c = bin p (Maths.glyphs n) c

let vari2m (s, io) =
  match io with
  | None   -> str s
  | Some i -> let n = Maths.node (Maths.glyphs s) in
              let subscript_right = str i in
              let n = { n with subscript_right } in
              [Maths.Ordinary n]

let rec v2m : valu -> Maths.math list = function
  | VVari(x)   -> vari2m x
  | VMeta(v)   -> vari2m v
  | VLAbs(x,t) -> (str "λ") @ (vari2m x) @ (str ".") @ (t2m t)
  | VCons(c,v) -> (vari2m c) @ (str "[") @ (v2m v) @ (str "]")
  | VERec(fs)  -> let build (l,v) =
                    (bin' 2 "=" (vari2m l, v2m v)) @ (str ";")
                  in
                  let fs = List.map build fs in
                  (str "{") @ (List.concat fs) @ (str "}")
  | VIRec(l,v) -> let l = vari2m l and v = vari2m v in
                  (str "{⋯") @ (bin' 2 "=" (l, v)) @ (str ";") @ (str "⋯}")
  | VGrou(v)   -> (str "(") @ (v2m v) @ (str ")")
and     t2m : term -> Maths.math list = function
  | TVari(a)   -> vari2m a
  | TMeta(t)   -> vari2m t
  | TValu(v)   -> v2m v
  | TGrou(t)   -> (str "(") @ (t2m t) @ (str ")")
  | TAppl(t,u) -> (t2m t) @ (str " ") @ (t2m u)
  | TSave(a,t) -> (str "μ") @ (vari2m a) @ (str ".") @ (t2m t)
  | TRest(s,t) -> (str "[") @ (s2m s) @ (str "]") @ (t2m t)
  | TVSub(t,s) -> let (x,s,v) = s in
                  let sub l r =
                    let b =
                      if s then MathFonts.asana "\\defeq" 798
                      else Maths.glyphs "←"
                    in
                    bin 2 b (l, r)
                  in
                  let sub = sub (vari2m x) (v2m v) in
                  (t2m t) @ (str "[") @ sub @ (str "]")
  | TSSub(t,s) -> let (x,s,pi) = s in
                  let b =
                    if s then MathFonts.asana "\\defeq" 798
                    else Maths.glyphs "←"
                  in
                  let sub = bin 2 b (vari2m x, s2m pi) in
                  (t2m t) @ (str "[") @ sub @ (str "]")
  | TCtxt(e,t) -> (c2m e) @ (str "[") @ (t2m t) @ (str "]")
  | TProj(v,l) -> (v2m v) @ (str ".") @ (vari2m l)
  | TUnit(v)   -> let n = Maths.node (Maths.glyphs "unit") in
                  let n = { n with subscript_right = v2m v } in
                  [Maths.Ordinary n]
  | TFixp(t,v) -> let n = Maths.node (Maths.glyphs "Y") in
                  let subscript_right = (t2m t) @ (str ",") @ (v2m v) in
                  let n = { n with subscript_right } in
                  [Maths.Ordinary n]
  | TECas(v,l) -> let build (c,v) =
                    (str "|") @ (vari2m c) @ (str "→") @ (v2m v)
                  in
                  let l = List.map build l in
                  (str "[") @ (v2m v) @ (List.concat l) @ (str "]")
  | TICas(v,c,w) ->
                  let mid = bin' 2 "→" (vari2m c, vari2m w) in
                  (str "[") @ (v2m v) @ (str "|⋯") @ mid @ (str "⋯]")
and     c2m : ctxt -> Maths.math list = function
  | CHole      -> (str "[") @ (asana "wild card" 382) @ (str "]")
  | CPlug(e,f) -> (c2m e) @ (str "[") @ (c2m f) @ (str "]")
  | CMeta(e)   -> vari2m e
  | CGrou(e)   -> (str "(") @ (c2m e) @ (str ")")
  | CLAbs(x,e) -> (str "λ") @ (vari2m x) @ (str ".") @ (c2m e)
  | CAppL(e,t) -> (c2m e) @ (str " ") @ (t2m t)
  | CAppR(t,e) -> (t2m t) @ (str " ") @ (c2m e)
and     s2m : stac -> Maths.math list = function
  | SEmpt      -> str "ε"
  | SVari(a)   -> vari2m a
  | SMeta(s)   -> vari2m s
  | SPush(v,s) -> bin 2 (Maths.glyphs ".") (v2m v, s2m s)
  | SFram(t,s) -> (str "[") @ (t2m t) @ (str "]") @ (s2m s)
and     p2m : proc -> Maths.math list = function
  | PMeta(p)   -> vari2m p
  | PProc(t,s) -> bin 2 (Maths.glyphs "∗") (t2m t, s2m s)

let v : string -> Maths.math list = fun s -> v2m (parse_valu s)
let t : string -> Maths.math list = fun s -> t2m (parse_term s)
let c : string -> Maths.math list = fun s -> c2m (parse_ctxt s)
let s : string -> Maths.math list = fun s -> s2m (parse_stac s)
let p : string -> Maths.math list = fun s -> p2m (parse_proc s)

open Lang_ast

(* Parser for symbols allowed as index. *)
let parser index =
  | "₀" -> "0" | "₁" -> "1" | "₂" -> "2" | "₃" -> "3" | "₄" -> "4"
  | "₅" -> "5" | "₆" -> "6" | "₇" -> "7" | "₈" -> "8" | "₉" -> "9"
  | "i" -> "i" | "j" -> "j" | "k" -> "k" | "n" -> "n" | "m" -> "m"
  | "i+1" -> "i+1" | "α" -> "α" | "f" -> "f"

(* Generic parser for variables. *)
let parser vari p   = x:p {- i:index}?$
let parser wildcard = "_" -> ("_", None)
let vari ns =
  let str s = Earley.string s s in
  let normal = vari (Earley.alternatives (List.map str ns)) in
  Earley.alternatives [wildcard; normal]

(* Predifined parsers for all kind of variables and metavariables. *)
let parser vvari = (vari ["x"; "y"; "z"; "f"; "g"; "h" ; "r"; "⋯"])
let parser vmeta = (vari ["v"; "w"; "Φ"])
let parser tvari = (vari ["a"; "b"; "c"])
let parser tmeta = (vari ["t"; "u"; "Ω"])
let parser cmeta = (vari ["E"; "F"])
let parser svari = (vari ["α"; "β"; "γ"; "ε"])
let parser smeta = (vari ["π"; "ξ"; "ρ"])
let parser pmeta = (vari ["p"; "q"; "ψ"])
let parser const = (vari ["C"; "D" ; "S" ; "Z"; "T"; "F"])
let parser label = (vari ["l"; "k"])
let parser subsm = (vari ["σ"; "ρ"])
let parser vfset = (vari ["I"; "J"; "K"])
let parser fmeta = (vari ["A"; "B"; "C"])
let parser fvari = (vari ["χ"])
let parser ovari = (vari ["X"; "Y"; "Z"])
let parser stvar = (vari ["s"; "ο"])
let parser vordi= (vari ["θ"; "η"])
let parser ometa = (vari ["τ"; "υ"; "o"])

let parser qvari  = ovari | fvari | tvari | vvari
let parser fovari = fvari | ovari

let parser posvari = (vari ["γ"])

let blank = EarleyStr.blank_regexp ''[ ]*''

(* Generic parsing function with exception handling. *)
let parse name g =
  let parse s = Earley.parse_string ~filename:s g blank s in
  (fun s ->
    try Earley.handle_exception parse s
    with _ ->
      Printf.eprintf "Could not parse the %s \"%s\".\n%!" name s;
      exit 1)

(* Types for priorities in the parsers. *)
type vprio = VSimp | VProj | VComp
type tprio = TAtom | TSubs | TAppl
type sprio = SAtom | SFull
type pprio = PAtom | PFull
type fprio = FAtom | FInte | FFull

(* Auxiliary parsers. *)
let vsub var elt = parser v:var b:{"≔" -> true | "←" -> false} e:elt

let parser cond = i:index "∈" s:vfset c:{"≠" "∅"}? -> (i,s,c <> None)
let fset elt sep =
  let sep = parser _:STR(sep) in
  parser
  | "⋯" e:elt "⋯"            -> Impli(e,None)
  | '(' e:elt ')' c:cond     -> Impli(e,Some c)
  | e:elt es:{_:sep e:elt}*$ -> Expli(e::es)
  | EMPTY                    -> Expli([])

let fset_ne elt sep =
  let sep = parser _:STR(sep) in
  parser
  | "⋯" e:elt "⋯"            -> Impli(e,None)
  | '(' e:elt ')' c:cond     -> Impli(e,Some c)
  | e:elt es:{_:sep e:elt}*$ -> Expli(e::es)

(* Parsing function for all the syntactic entities. *)
let parser vaux (sa,prio) =
  | x:vvari                                   when prio = VSimp -> VVari(x)
  | v:vmeta                                   when prio = VSimp -> VMeta(v)
  | "⟦" v:(vaux (sa,VComp)) "⟧"               when prio = VSimp
                                                          && sa -> VSema(v)
  | "λ" x:vvari t:(term TAppl)$               when prio = VComp -> VLAbs(x,t)
  | c:const '[' v:(vaux (true,VComp)) ']'     when prio = VSimp -> VCons(c,v)
  | "{" fs:(fset (field sa) ";") ';'? "}"     when prio = VSimp -> VReco(fs)
  | '(' v:(vaux (true,VComp)) ')'             when prio = VProj -> VGrou(v)
  | v:(vaux (sa,VSimp)) g:subs$               when prio = VSimp -> VSubs(v,g)
  | v:(vaux (sa,VSimp))                       when prio = VComp -> v
  | v:(vaux (sa,VSimp))                       when prio = VProj -> v
  | s:subs '(' x:vvari ')'                    when prio = VSimp -> VASub(s,x)
  | "ε" x:vvari "∈" a:(form FFull) '(' t:(term TAppl) "∉" b:(form FFull) ')'
      when prio = VSimp -> VWitn(x,a,t,b)
  | "□"                                       when prio = VSimp -> VWBox
and field sa = l:label "=" v:(vaux (true,VComp))
and valu prio = v:(vaux (true,prio))
and valu_ns prio = v:(vaux (false,prio))
and        term prio =
  | a:tvari                                   when prio = TAtom -> TVari(a)
  | t:tmeta                                   when prio = TAtom -> TMeta(t)
  | "⟦" t:(term TAppl) "⟧"                    when prio = TAtom -> TSema(t)
  | v:(valu_ns VComp)$                        when prio = TAtom -> TValu(v)
  | '(' t:(term TAppl) ')'                    when prio = TAtom -> TGrou(t)
  | t:(term TSubs) ts:(term TSubs)*$          when prio = TAppl -> tappl t ts
  | "μ" a:svari t:(term TAppl)$               when prio = TAtom -> TSave(a,t)
  | '[' s:(stac SFull) ']' t:(term TAppl)$    when prio = TAtom -> TRest(s,t)
  | t:(term TAtom) s:subs                     when prio = TSubs -> TSubt(t,s)
  | c:(ctxt TAtom) "<[" t:(term TAppl) "]>"   when prio = TAtom -> TCtxt(c,t)
  | v:(valu VProj) '.' l:label                when prio = TAtom -> TProj(v,l)
  | "R(" v:(valu VComp) "," t:(term TAppl) ")" when prio = TAtom -> TIsRe(v,t)
  | "δ(" v:(valu VComp) "," w:(valu VComp) ")" when prio = TAtom -> TDelt(v,w)
  | "Y(" t:(term TAppl) "," v:(valu VComp) ")" when prio = TAtom -> TFixp(t,v)
  | '[' v:(valu VComp) '|' ps:(fset patt "|") ']'
                                              when prio = TAtom -> TCase(v,ps)
  | t:(term TAtom)                            when prio = TSubs -> t
and patt = c:const '[' x:vvari ']' "→" t:(term TAppl)
and        ctxt prio =
  | "[]"                                      when prio = TAtom -> CHole
  | e:(ctxt TAtom) '[' f:(ctxt TAppl) ']'     when prio = TAtom -> CPlug(e,f)
  | e:cmeta                                   when prio = TAtom -> CMeta(e)
  | '(' e:(ctxt TAppl) ')'                    when prio = TAtom -> CGrou(e)
  | "λ" x:vvari e:(ctxt TAppl)                when prio = TAtom -> CLAbs(x,e)
  | e:(ctxt TSubs) t:(term TAtom)             when prio = TAppl -> CAppL(e,t)
  | t:(term TAtom) e:(ctxt TSubs)             when prio = TAppl -> CAppR(t,e)
  | e:(ctxt TAtom)                            when prio = TSubs -> e
  | e:(ctxt TSubs)                            when prio = TAppl -> e
and        stac prio =
  | a:svari                                   when prio = SAtom -> SVari(a)
  | s:smeta                                   when prio = SAtom -> SMeta(s)
  | "⟦" s:(stac SFull) "⟧"                    when prio = SAtom -> SSema(s)
  | v:(valu VComp) "·" s:(stac SFull)         when prio = SFull -> SPush(v,s)
  | '[' t:(term TAppl) ']' s:(stac SFull)     when prio = SFull -> SFram(t,s)
  | '(' s:(stac SFull) ')'                    when prio = SAtom -> SGrou(s)
  | s:(stac SAtom) g:subs                     when prio = SAtom -> SSubs(s,g)
  | s:(stac SAtom)                            when prio = SFull -> s
  | s:subs '(' a:svari ')'                    when prio = SAtom -> SASub(s,a)
  | "ε" al:svari "∈" a:(form FFull) '(' t:(term TAppl) "∉" b:(form FFull) ')'
      when prio = SAtom -> SWitn(al,a,t,b)
and        proc prio =
  | p:pmeta                                   when prio = PAtom -> PMeta(p)
  | t:(term TAppl) "∗" s:(stac SFull)         when prio = PFull -> PProc(t,s)
  | '(' p:(proc PFull) ')'                    when prio = PAtom -> PGrou(p)
  | p:(proc PAtom) g:subs                     when prio = PAtom -> PSubs(p,g)
  | p:(proc PAtom)                            when prio = PFull -> p
and        ordi =
  | x:vordi     -> OVari(x)
  | x:ometa     -> OMeta(x)
  | "∞"         -> OInfi
  | o:ordi "+1" -> OSucc(o)
  | "ε" x:vordi '<' o:ordi '(' t:(term TAppl)
    b:{"∈" -> true | "∉" → false} a:(form FFull) ')'
                -> OWitn(x,o,t,b,a)
and        form prio =
  | t:(term TAppl) u:{"≡" u:(term TAppl)}?$   when prio = FAtom ->
      (match u with None -> FTerm(t) | Some u -> FRest(None,Eq (t,u)))
  | s:(stac SFull)                            when prio = FAtom -> FStac(s)
  | a:fmeta                                   when prio = FAtom -> FMeta(a)
  | x:qvari s:{"^" stvar}?                    when prio = FAtom -> FVari(x,s)
  | '(' x:qvari s:{"^" stvar}? "↦" a:(form FFull) ')'
                                              when prio = FAtom -> FLamb(x,s,a)
  | a:(form FAtom) s:subs$                    when prio = FAtom -> FSubs(a,s)
  | a:(form FAtom) '(' b:(form FFull) ')'     when prio = FAtom -> FAppl(a,b)
  | '(' a:(form FFull) ')'                    when prio = FAtom -> FGrou(a)
  | a:(form FInte) fs:{"⇒" b:(form FInte)}*$  when prio = FFull -> ffunc (a::fs)
  | a:(form FAtom) "×" b:(form FAtom)$        when prio = FFull -> FBPrd(a,b)
  | '{' fs:(fset_ne ffield ";") e:{';'"⋯"}?$ '}' when prio = FAtom ->
                                                   FProd(fs,e<>None)
  | '[' ps:(fset fpatt  "|") ']'              when prio = FAtom -> FDSum(ps)
  | "∀" x:qvari s:{"^" stvar}? a:(form FFull) when prio = FAtom -> FUniv(x,s,a)
  | "∃" x:qvari s:{"^" stvar}? a:(form FFull) when prio = FAtom -> FExis(x,s,a)
  | "Π_(" x:vvari "∈" a:(form FFull) ")" b:(form FFull)
                                              when prio = FAtom -> FDVFn(x,a,b)
  | "Π_(" x:tvari "∈" a:(form FFull) ")" b:(form FFull)
                                              when prio = FAtom -> FDTFn(x,a,b)
  | "Σ_(" x:vvari "∈" a:(form FFull) ")" b:(form FFull)
                                              when prio = FAtom -> FDVPd(x,a,b)
  | "Σ_(" x:tvari "∈" a:(form FFull) ")" b:(form FFull)
                                              when prio = FAtom -> FDTPd(x,a,b)
  | "μ" o:ordi? x:ovari a:(form FFull)        when prio = FAtom -> FLFix(x,o,a)
  | "ν" o:ordi? x:ovari a:(form FFull)        when prio = FAtom -> FGFix(x,o,a)
  | t:(term TAppl) "∈" a:(form FAtom)$        when prio = FAtom -> FMemb(t,a)
  | a:(form FAtom) e:{"∧" e:equa}?$           when prio = FInte ->
      (match e with None -> a | Some e -> FRest(Some a,e))
  | e:equa "↪" a:(form FAtom)                 when prio = FAtom -> FImpl(e,a)
  | s:subs '(' x:qvari ')'                    when prio = FAtom -> FASub(s,x)
  | "ε" x:qvari s:{"∈" stvar}? '(' t:(term TAppl)
      m:{"∉" -> true | "∈" -> false} b:(form FFull) ')'
      when prio = FAtom -> FWitn(x,s,t,m,b)
and ffield = l:label ':' a:(form FFull)$
and fpatt  = c:const ':' a:(form FFull)$
and equa   =
  | t:(term TAppl) "≡" u:(term TAppl) -> Eq(t,u)
  | x:posvari                         -> Or(x)
and        subs =
  | '(' s1:subs "∘" s2:subs ')'                         -> SubCm(s1,s2)
  | s:subsm "⁻¹"                                        -> SubIv(s)
  | s:subsm                                             -> SubsM(s)
  | s:subs?[NoSub] '[' v:(vsub vvari  (valu VComp)) ']' -> SubsV(s,v)
  | s:subs?[NoSub] '[' t:(vsub tvari  (term TAppl)) ']' -> SubsT(s,t)
  | s:subs?[NoSub] '[' p:(vsub svari  (stac SFull)) ']' -> SubsS(s,p)
  | s:subs?[NoSub] '[' f:(vsub fovari (form FFull)) ']' -> SubsF(s,f)
  | s:subs?[NoSub] '[' f:(vsub vordi  ordi        ) ']' -> SubsO(s,f)

let term = term TAppl
let ctxt = ctxt TAppl
let valu = valu VComp
let stac = stac SFull
let proc = proc PFull
let form = form FFull

let parse_valu = parse "value"        valu
let parse_term = parse "term"         term
let parse_ctxt = parse "context"      ctxt
let parse_stac = parse "stack"        stac
let parse_proc = parse "process"      proc
let parse_form = parse "formula"      form
let parse_subs = parse "substitution" subs
let parse_ordi = parse "ordinal"      ordi

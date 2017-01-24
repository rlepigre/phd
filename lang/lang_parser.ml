open Lang_ast

(* Parser for symbols allowed as index. *)
let parser index =
  | "₀" -> "0" | "₁" -> "1" | "₂" -> "2" | "₃" -> "3" | "₄" -> "4"
  | "₅" -> "5" | "₆" -> "6" | "₇" -> "7" | "₈" -> "8" | "₉" -> "9"
  | "i" -> "i" | "j" -> "j" | "k" -> "k" | "n" -> "n" | "m" -> "m"
  | "i+1" -> "i+1" | "α" -> "α" | "f" -> "f"

(* Generic parser for variables. *)
let parser vari p   = x:p {- i:index}?
let parser wildcard = "_" -> ("_", None)
let vari ns =
  let str s = Earley.string s s in
  let normal = vari (Earley.alternatives (List.map str ns)) in
  Earley.alternatives [wildcard; normal]

(* Predifined parsers for all kind of variables and metavariables. *)
let vvari = vari ["x"; "y"; "z"; "f"; "g"; "h"]
let vmeta = vari ["v"; "w"; "r"; "Φ"]
let tvari = vari ["a"; "b"; "c"]
let tmeta = vari ["t"; "u"; "Ω"]
let cmeta = vari ["E"; "F"]
let svari = vari ["α"; "β"; "γ"]
let smeta = vari ["π"; "ξ"; "ρ"]
let pmeta = vari ["p"; "q"; "ψ"]
let const = vari ["C"; "D" ; "S" ; "Z"; "T"; "F"]
let label = vari ["l"; "k"]
let subsm = vari ["σ"; "ρ"]
let vfset = vari ["I"; "J"; "K"]
let fmeta = vari ["A"; "B"; "C"]
let fvari = vari ["χ"]
let ovari = vari ["X"; "Y"; "Z"]
let stvar = vari ["s"]

let parser qvari  = ovari | fvari | tvari | vvari
let parser fovari = fvari | ovari

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
type fprio = FAtom | FFull

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

(* Parsing function for all the syntactic entities. *)
let parser valu prio =
  | x:vvari                                   when prio = VSimp -> VVari(x)
  | v:vmeta                                   when prio = VSimp -> VMeta(v)
  | "⟦" v:(valu VComp) "⟧"                    when prio = VSimp -> VSema(v)
  | "λ" x:vvari t:(term TAppl)$               when prio = VComp -> VLAbs(x,t)
  | c:const '[' v:(valu VComp) ']'            when prio = VSimp -> VCons(c,v)
  | "{" fs:(fset field ";") ';'? "}"          when prio = VSimp -> VReco(fs)
  | '(' v:(valu VComp) ')'                    when prio = VProj -> VGrou(v)
  | v:(valu VSimp) g:subs$                    when prio = VSimp -> VSubs(v,g)
  | v:(valu VSimp)                            when prio = VComp -> v
  | v:(valu VSimp)                            when prio = VProj -> v
  | s:subs '(' x:vvari ')'                    when prio = VSimp -> VASub(s,x)
  | "ε" x:vvari "∈" a:(form FFull) '(' t:(term TAppl) "∉" b:(form FFull) ')'
      when prio = VSimp -> VWitn(x,a,t,b)
and field = l:label "=" v:(valu VComp)
and        term prio =
  | a:tvari                                   when prio = TAtom -> TVari(a)
  | t:tmeta                                   when prio = TAtom -> TMeta(t)
  | "⟦" t:(term TAppl) "⟧"                    when prio = TAtom -> TSema(t)
  | v:(valu VComp)$                           when prio = TAtom -> TValu(v)
  | '(' t:(term TAppl) ')'                    when prio = TAtom -> TGrou(t)
  | t:(term TSubs) ts:(term TSubs)*$          when prio = TAppl -> tappl t ts
  | "μ" a:svari t:(term TAppl)$               when prio = TAtom -> TSave(a,t)
  | '[' s:(stac SFull) ']' t:(term TAppl)$    when prio = TAtom -> TRest(s,t)
  | t:(term TAtom) s:subs                     when prio = TSubs -> TSubt(t,s)
  | c:(ctxt TAtom) '[' t:(term TAppl) ']'     when prio = TAtom -> TCtxt(c,t)
  | v:(valu VProj) '.' l:label                when prio = TAtom -> TProj(v,l)
  | "U(" v:(valu VComp) ")"                   when prio = TAtom -> TUnit(v)
  | "δ(" v:(valu VComp) "," w:(valu VComp) ")"
                                              when prio = TAtom -> TDelt(v,w)
  | "Y(" t:(term TAppl) "," v:(valu VComp) ")"
                                              when prio = TAtom -> TFixp(t,v)
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
  | "ε"                                       when prio = SAtom -> SEmpt
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
and        form prio =
  | t:(term TAppl)                            when prio = FAtom -> FTerm(t)
  | s:(stac SFull)                            when prio = FAtom -> FStac(s)
  | a:fmeta                                   when prio = FAtom -> FMeta(a)
  | x:qvari s:{"^" stvar}?                    when prio = FAtom -> FVari(x,s)
  | '(' x:qvari s:{"^" stvar}? "↦" a:(form FFull) ')'
                                              when prio = FAtom -> FLamb(x,s,a)
  | a:(form FAtom) s:subs$                    when prio = FAtom -> FSubs(a,s)
  | a:(form FAtom) '(' b:(form FFull) ')'     when prio = FAtom -> FAppl(a,b)
  | '(' a:(form FFull) ')'                    when prio = FAtom -> FGrou(a)
  | a:(form FAtom) fs:{"⇒" b:(form FAtom)}*$  when prio = FFull -> ffunc (a::fs)
  | a:(form FAtom) "×" b:(form FAtom)$        when prio = FFull -> FBPrd(a,b)
  | '{' fs:(fset ffield ";") '}'              when prio = FAtom -> FProd(fs)
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
  | "μ" x:ovari a:(form FFull)                when prio = FAtom -> FLFix(x,a)
  | "ν" x:ovari a:(form FFull)                when prio = FAtom -> FGFix(x,a)
  | t:(term TAppl) "∈" a:(form FAtom)         when prio = FAtom -> FMemb(t,a)
  | a:{a:(form FAtom) "∧"}? e:equa            when prio = FAtom -> FRest(a,e)
  | s:subs '(' x:qvari ')'                    when prio = FAtom -> FASub(s,x)
  | "ε" x:qvari s:{"∈" stvar}? '(' t:(term TAppl)
      m:{"∉" -> true | "∈" -> false} b:(form FFull) ')'
      when prio = FAtom -> FWitn(x,s,t,m,b)
and ffield = l:label ':' a:(form FFull)$
and fpatt  = c:const ':' a:(form FFull)$
and equa   = t:(term TAppl)$ "≡" u:(term TAppl) (* $ Bug "Aρ | u₁ρ≡u₂ρ" *)
and        subs =
  | '(' s1:subs "∘" s2:subs ')'                         -> SubCm(s1,s2)
  | s:subsm                                             -> SubsM(s)
  | s:subs?[NoSub] '[' v:(vsub vvari  (valu VComp)) ']' -> SubsV(s,v)
  | s:subs?[NoSub] '[' t:(vsub tvari  (term TAppl)) ']' -> SubsT(s,t)
  | s:subs?[NoSub] '[' p:(vsub svari  (stac SFull)) ']' -> SubsS(s,p)
  | s:subs?[NoSub] '[' f:(vsub fovari (form FFull)) ']' -> SubsF(s,f)

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

open Typography
open DefaultFormat
open Maths
open Lang_ast

type maths = Maths.math list

let str s = [Maths.Ordinary (Maths.node (Maths.glyphs s))]
let asana n i = [Maths.Ordinary (Maths.node (Maths.asana n i))]
let bin p n (l, r) =
  [bin p (Normal(false, Maths.node n, false)) l r]
let bin' p n c = bin p (Maths.glyphs n) c
let sp w =
  let glue w =
    match Box.glue w w w with
    | Box.Glue dr -> dr
    | _           -> assert false
  in
  [Maths.Glue (glue w)]

let vari2m (s, io) =
  match io with
  | None   -> str s
  | Some i -> let n = Maths.node (Maths.glyphs s) in
              let subscript_right = str i in
              let n = { n with subscript_right } in
              [Maths.Ordinary n]

let subst2bin s (l,r) =
  let b =
    if s then Maths.asana "\\defeq" 798
    else Maths.glyphs "←"
  in
  bin 2 b (l, r)

let with_rsup m s =
  let m = Maths.node (fun env _ -> Maths.draw [env] m) in
  [Maths.Ordinary {m with superscript_right = s}]

let fset2m : ('a -> maths) -> maths -> 'a fset -> maths = fun f s -> function
  | Expli([]   ) -> []
  | Expli(e::es) -> let aux e = (sp 0.6) @ s @ (sp 0.6) @ (f e) in
                    (f e) @ (List.concat (List.map aux es))
  | Impli(e,cnd) ->
      begin
        match cnd with
        | None        -> (str "⋯") @ (sp 0.6) @ (f e) @ (sp 0.6) @ (str "⋯")
        | Some(i,s,n) -> let s = vari2m s in
                         let s = if n then bin' 2 "≠" (s, str "∅") else s in
                         let s = bin' 2 "∈" (str i, s) in
                         let mid = (str "(") @ (f e) @ (str ")") in
                         let mid =
                           Maths.node (fun env _ -> Maths.draw [env] mid)
                         in
                         let mid = {mid with subscript_right = s} in
                         [Maths.Ordinary mid]
      end

let semantic_brackets : Maths.math list -> Maths.math list = fun ms ->
  (*
  let l = asana "\\left_white_square_bracket"  3320 in
  let r = asana "\\right_white_square_bracket" 3324 in
  r @ ms @ r
  *)
  let l =
    Maths.fix_asana_delimiters "\\left_white_square_bracket"
      [3320; 3321; 3322; 3323]
  in
  let r =
    Maths.fix_asana_delimiters "\\left_white_square_bracket"
      [3324; 3325; 3326; 3327]
  in
  [Maths.Decoration (Maths.open_close l r, ms)]

let rec v2m : valu -> Maths.math list = function
  | VVari(x)   -> vari2m x
  | VMeta(v)   -> vari2m v
  | VSema(v)   -> semantic_brackets (v2m v)
  | VLAbs(x,t) -> (str "λ") @ (vari2m x) @ (str ".") @ (t2m t)
  | VCons(c,v) -> (vari2m c) @ (str "[") @ (v2m v) @ (str "]")
  | VReco(fs)  -> let aux (l,v) = bin' 2 "=" (vari2m l, v2m v) in
                  (str "{") @ (fset2m aux (str ";") fs) @ (str "}")
  | VGrou(v)   -> (str "(") @ (v2m v) @ (str ")")
  | VSubs(v,s) -> (v2m v) @ (subs2m s)
  | VASub(s,x) -> (subs2m s) @ (str "(") @ (vari2m x) @ (str ")")
  | VWitn(x,a,t,b) ->
                  let n = Maths.node (Maths.glyphs "ε") in
                  let subscript_right = bin' 2 "∈" (vari2m x, f2m a) in
                  let n = [Maths.Ordinary {n with subscript_right}] in
                  n @ (str "(") @ (bin' 2 "∉" (t2m t, f2m b)) @ (str ")")
  | VWBox      -> asana "□" 1046 (* 1052 *) (* 1388 *)
and     t2m : term -> Maths.math list = function
  | TVari(a)   -> vari2m a
  | TMeta(t)   -> vari2m t
  | TSema(t)   -> semantic_brackets (t2m t)
  | TValu(v)   -> v2m v
  | TGrou(t)   -> (str "(") @ (t2m t) @ (str ")")
  | TAppl(t,u) -> (t2m t) @ (str " ") @ (t2m u)
  | TSave(a,t) -> (str "μ") @ (vari2m a) @ (str ".") @ (t2m t)
  | TRest(s,t) -> (str "[") @ (s2m s) @ (str "]") @ (t2m t)
  (* | TRest(s,t) -> (str "⟨") @ (s2m s) @ (str "⟩") @ (t2m t) *)
  | TCtxt(e,t) -> (c2m e) @ (str "[") @ (t2m t) @ (str "]")
  | TProj(v,l) -> (v2m v) @ (str ".") @ (vari2m l)
  | TIsRe(v,t) -> let n = Maths.node (Maths.glyphs "R") in
                  let subscript_right = (v2m v) @ (str ",") @ (t2m t) in
                  let n = { n with subscript_right} in
                  [Maths.Ordinary n]
  | TDelt(v,w) -> let n = Maths.node (Maths.glyphs "δ") in
                  let subscript_right = (v2m v) @ (str ",") @ (v2m w) in
                  let n = { n with subscript_right } in
                  [Maths.Ordinary n]
  | TFixp(t,v) -> let n = Maths.node (Maths.glyphs "Y") in
                  let subscript_right = (t2m t) @ (str ",") @ (v2m v) in
                  let n = { n with subscript_right } in
                  [Maths.Ordinary n]
  | TCase(v,p) -> (str "[") @ (v2m v) @ (sp 0.6) @ (str "|") @ (sp 0.6) @
                  (fset2m patt2m (str "|") p) @ (str "]")
  | TSubt(t,s) -> (t2m t) @ (subs2m s)
and  patt2m : patt -> Maths.math list = fun (c,x,t) ->
  let pat = (vari2m c) @ (str "[") @ (vari2m x) @ (str "]") in
  bin' 2 "→" (pat, t2m t)
and     s2m : stac -> Maths.math list = function
  | SVari(a)   -> vari2m a
  | SMeta(s)   -> vari2m s
  | SSema(s)   -> semantic_brackets (s2m s)
  | SPush(v,s) -> bin 2 (Maths.glyphs ".") (v2m v, s2m s)
  | SFram(t,s) -> (str "[") @ (t2m t) @ (str "]") @ (s2m s)
  (* | SFram(t,s) -> (str "⟨") @ (t2m t) @ (str "⟩") @ (s2m s) *)
  | SGrou(s)   -> (str "(") @ (s2m s) @ (str ")")
  | SSubs(p,s) -> (s2m p) @ (subs2m s)
  | SASub(s,a) -> (subs2m s) @ (str "(") @ (vari2m a) @ (str ")")
  | SWitn(x,a,t,b) ->
                  let n = Maths.node (Maths.glyphs "ε") in
                  let subscript_right =
                    bin' 2 "∈" (vari2m x, (str "¬") @ (f2m a))
                  in
                  let n = [Maths.Ordinary {n with subscript_right}] in
                  n @ (str "(") @ (bin' 2 "∉" (t2m t, f2m b)) @ (str ")")
and     p2m : proc -> Maths.math list = function
  | PMeta(p)   -> vari2m p
  | PProc(t,s) -> bin 2 (Maths.glyphs "∗") (t2m t, s2m s)
  | PGrou(p)   -> (str "(") @ (p2m p) @ (str ")")
  | PSubs(p,s) -> (p2m p) @ (subs2m s)
and     c2m : ctxt -> Maths.math list = function
  | CHole      -> (str "[") @ (asana "wild card" 382) @ (str "]")
  | CPlug(e,f) -> (c2m e) @ (str "[") @ (c2m f) @ (str "]")
  | CMeta(e)   -> vari2m e
  | CGrou(e)   -> (str "(") @ (c2m e) @ (str ")")
  | CLAbs(x,e) -> (str "λ") @ (vari2m x) @ (str ".") @ (c2m e)
  | CAppL(e,t) -> (c2m e) @ (str " ") @ (t2m t)
  | CAppR(t,e) -> (t2m t) @ (str " ") @ (c2m e)
and  subs2m : subs -> Maths.math list =
  let aux elt2m (x,b,e) =
    (str "[") @ (subst2bin b (vari2m x, elt2m e)) @ (str "]")
  in
  function
  | SubCm(s1,s2) -> (str "(") @ (bin' 3 "∘" (subs2m s1, subs2m s2)) @ (str ")")
  | SubIv(s)     -> let n = Maths.node (Maths.glyphs (fst s)) in
                    assert (snd s = None);
                    let superscript_right = str "-1" in
                    [Maths.Ordinary {n with superscript_right}]
  | SubsV(s,e)   -> (subs2m s) @ aux v2m e
  | SubsT(s,e)   -> (subs2m s) @ aux t2m e
  | SubsS(s,e)   -> (subs2m s) @ aux s2m e
  | SubsF(s,e)   -> (subs2m s) @ aux f2m e
  | SubsO(s,e)   -> (subs2m s) @ aux o2m e
  | NoSub        -> []
  | SubsM(s)     -> vari2m s
and o2m : ordi -> Maths.math list = function
  | OInfi          -> str "∞"
  | OSucc(o)       -> bin' 3 "+" (o2m o, str "1")
  | OVari(x)       -> vari2m x
  | OMeta(x)       -> vari2m x
  | OWitn(x,o,t,b,a) ->
      let n = Maths.node (Maths.glyphs "ε") in
      let subscript_right = bin' 2 "<" (vari2m x, o2m o) in
      let n = [Maths.Ordinary {n with subscript_right}] in
      n @ (str "(") @
        (bin' 2 (if b then "∈" else "∉") (t2m t, f2m a)) @ (str ")")

and f2m : form -> Maths.math list = function
  | FTerm(t)       -> t2m t
  | FStac(s)       -> s2m s
  | FMeta(a)       -> vari2m a
  | FVari(x,s)     -> begin
                        let x = vari2m x in
                        match s with
                        | None   -> x
                        | Some s -> with_rsup x (vari2m s)
                      end
  | FLamb(x,s,a)   -> let x =
                        let x = vari2m x in
                        match s with
                        | None   -> x
                        | Some s -> with_rsup x (vari2m s)
                      in (str "(") @ (bin' 2 "↦" (x, f2m a)) @ (str ")")
  | FAppl(a,b)     -> (f2m a) @ (str "(") @ (f2m b) @ (str ")")
  | FFunc(a,b)     -> bin' 1 "⇒" (f2m a, f2m b)
  | FSubs(a,s)     -> (f2m a) @ (subs2m s)
  | FGrou(a)       -> (str "(") @ (f2m a) @ (str ")")
  | FBPrd(a,b)     -> bin' 2 "×" (f2m a, f2m b)
  | FProd(fs,e)    -> let aux (l,a) = bin' 2 ":" (vari2m l, f2m a) in
                      let e =
                        if e then (sp 0.6) @ (str ";") @ (sp 0.6) @ (str "⋯")
                        else []
                      in
                      (str "{") @ (fset2m aux (str ";") fs) @ e @ (str "}")
  | FDSum(ps)      -> let aux (c,a) = bin' 2 ":" (vari2m c, f2m a) in
                      (str "[") @ (fset2m aux (str "|") ps) @ (str "]")
  | FUniv(x,s,a)   -> let c =
                        let x = vari2m x in
                        match s with
                        | None   -> x
                        | Some s -> with_rsup x (vari2m s)
                      in (str "∀") @ c @ (str ".") @ (f2m a)
  | FExis(x,s,a)   -> let c =
                        let x = vari2m x in
                        match s with
                        | None   -> x
                        | Some s -> with_rsup x (vari2m s)
                      in (str "∃") @ c @ (str ".") @ (f2m a)
  | FDVFn(x,a,b)
  | FDTFn(x,a,b)   -> let subscript_right = bin' 3 "∈" (vari2m x, f2m a) in
                      let n = Maths.node (Maths.glyphs "Π") in
                      let n = [Maths.Ordinary {n with subscript_right}] in
                      n @ (f2m b)
  | FDVPd(x,a,b)
  | FDTPd(x,a,b)   -> let subscript_right = bin' 3 "∈" (vari2m x, f2m a) in
                      let n = Maths.node (Maths.glyphs "Σ") in
                      let n = [Maths.Ordinary {n with subscript_right}] in
                      n @ (f2m b)
  | FLFix(x,o,a)   -> let subscript_right =
                        match o with
                        | None    -> []
                        | Some(o) -> o2m o
                      in
                      let n = Maths.node (Maths.glyphs "μ") in
                      let n = [Maths.Ordinary {n with subscript_right}] in
                      n @ (vari2m x) @ (str ".") @ (f2m a)
  | FGFix(x,o,a)   -> let subscript_right =
                        match o with
                        | None    -> []
                        | Some(o) -> o2m o
                      in
                      let n = Maths.node (Maths.glyphs "ν") in
                      let n = [Maths.Ordinary {n with subscript_right}] in
                      n @ (vari2m x) @ (str ".") @ (f2m a)
  | FMemb(t,a)     -> bin' 3 "∈" (t2m t, f2m a)
  | FRest(a,Eq(t,u)) -> let eq = bin' 2 "≡" (t2m t, t2m u) in
                      begin
                        match a with
                        | None   -> eq
                        | Some a -> let sep = Maths.euler "↾" 248 in
                                    bin 2 sep (f2m a, eq)
                      end
  | FRest(a,Or(g))  -> let sep = Maths.euler "↾" 248 in
                       begin
                         match a with
                         | None   -> assert false
                         | Some a -> bin 2 sep (f2m a, vari2m g)
                       end
  | FImpl(Eq(t,u),a)-> let eq = bin' 2 "≡" (t2m t, t2m u) in
                       let sep = Maths.euler "↪" 245 in
                       bin 2 sep (eq, f2m a)
  | FImpl(Or(g),a)  -> let sep = Maths.euler "↪" 245 in
                       bin 2 sep (vari2m g, f2m a)
  | FASub(s,x)      -> (subs2m s) @ (str "(") @ (vari2m x) @ (str ")")
  | FWitn(x,so,t,m,b) ->
                      let n = Maths.node (Maths.glyphs "ε") in
                      let subscript_right =
                        match so with
                        | None   -> vari2m x
                        | Some s -> bin' 2 ":" (vari2m x, vari2m s)
                      in
                      let n = [Maths.Ordinary {n with subscript_right}] in
                      let s = if not m then "∈" else "∉" in
                      n @ (str "(") @ (bin' 2 s (t2m t, f2m b)) @ (str ")")

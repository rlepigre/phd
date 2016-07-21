open Typography
open DefaultFormat
open Maths
open Lang_ast

type maths = Maths.math list

let str s = [Maths.Ordinary (Maths.node (Maths.glyphs s))]
let asana n i = [Maths.Ordinary (Maths.node (MathFonts.asana n i))]
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
    if s then MathFonts.asana "\\defeq" 798
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

let rec v2m : valu -> Maths.math list = function
  | VVari(x)   -> vari2m x
  | VMeta(v)   -> vari2m v
  | VLAbs(x,t) -> (str "λ") @ (vari2m x) @ (str ".") @ (t2m t)
  | VCons(c,v) -> (vari2m c) @ (str "[") @ (v2m v) @ (str "]")
  | VReco(fs)  -> let aux (l,v) = bin' 2 "=" (vari2m l, v2m v) in
                  (str "{") @ (fset2m aux (str ";") fs) @ (str "}")
  | VGrou(v)   -> (str "(") @ (v2m v) @ (str ")")
  | VSubs(v,s) -> (v2m v) @ (subs2m s)
  | VASub(s,x) -> (subs2m s) @ (str "(") @ (vari2m x) @ (str ")")
and     t2m : term -> Maths.math list = function
  | TVari(a)   -> vari2m a
  | TMeta(t)   -> vari2m t
  | TValu(v)   -> v2m v
  | TGrou(t)   -> (str "(") @ (t2m t) @ (str ")")
  | TAppl(t,u) -> (t2m t) @ (str " ") @ (t2m u)
  | TSave(a,t) -> (str "μ") @ (vari2m a) @ (str ".") @ (t2m t)
  | TRest(s,t) -> (str "[") @ (s2m s) @ (str "]") @ (t2m t)
  | TCtxt(e,t) -> (c2m e) @ (str "[") @ (t2m t) @ (str "]")
  | TProj(v,l) -> (v2m v) @ (str ".") @ (vari2m l)
  | TUnit(v)   -> let n = Maths.node (Maths.glyphs "U") in
                  let n = { n with subscript_right = v2m v } in
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
  | SEmpt      -> str "ε"
  | SVari(a)   -> vari2m a
  | SMeta(s)   -> vari2m s
  | SPush(v,s) -> bin 2 (Maths.glyphs ".") (v2m v, s2m s)
  | SFram(t,s) -> (str "[") @ (t2m t) @ (str "]") @ (s2m s)
  | SGrou(s)   -> (str "(") @ (s2m s) @ (str ")")
  | SSubs(p,s) -> (s2m p) @ (subs2m s)
  | SASub(s,a) -> (subs2m s) @ (str "(") @ (vari2m a) @ (str ")")
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
  | SubsV(s,e)   -> (subs2m s) @ aux v2m e
  | SubsT(s,e)   -> (subs2m s) @ aux t2m e
  | SubsS(s,e)   -> (subs2m s) @ aux s2m e
  | SubsF(s,e)   -> (subs2m s) @ aux f2m e
  | NoSub        -> []
  | SubsM(s)     -> vari2m s
and f2m : form -> Maths.math list = function
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
  | FProd(fs)      -> let aux (l,a) = bin' 2 ":" (vari2m l, f2m a) in
                      (str "{") @ (fset2m aux (str ";") fs) @ (str "}")
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
  | FLFix(x,a)     -> (str "μ") @ (vari2m x) @ (str ".") @ (f2m a)
  | FGFix(x,a)     -> (str "ν") @ (vari2m x) @ (str ".") @ (f2m a)
  | FMemb(t,a)     -> bin' 3 "∈" (t2m t, f2m a)
  | FRest(a,(t,u)) -> let eq = bin' 2 "≡" (t2m t, t2m u) in
                      begin
                        match a with
                        | None   -> eq
                        | Some a -> let sep = MathFonts.euler "↾" 248 in
                                    bin 2 sep (f2m a, eq)
                      end
  | FASub(s,x)      -> (subs2m s) @ (str "(") @ (vari2m x) @ (str ")")

open Typography
open DefaultFormat
open Maths
open Earley

type vari = string * int option

type term =
  | Var of vari
  | Abs of vari * term
  | App of term * term
  | Par of term
  | Sub of term * (vari * bool * term)
  | Ctx of ctxt * term
and ctxt =
  | Hole of vari option
  | CPar of ctxt
  | CAbs of vari * ctxt
  | LApp of ctxt * term
  | RApp of term * ctxt

type stac =
  | Alp of string
  | Psh of term * stac
  | Frm of term * stac

let build_app = List.fold_left (fun t u -> App(t,u))
let build_sub t = function
  | None   -> t
  | Some s -> Sub(t,s)

type prio = Atom | Subs | Appl
let parser lvar = ''[a-z]'' {- {"₀" -> 0 | "₁" -> 1 | "₂" -> 2 | "₃" -> 3
  | "₄" -> 4 | "₅" -> 5}}?
let parser cvar = ''[A-Z]'' {- {"₀" -> 0 | "₁" -> 1 | "₂" -> 2 | "₃" -> 3
  | "₄" -> 4 | "₅" -> 5}}?

let parser lterm prio =
  | x:lvar                             when prio = Atom -> Var(x)
  | '(' t:(lterm Appl) ')'             when prio = Atom -> Par(t)
  | "λ" x:lvar t:(lterm Appl)          when prio = Atom -> Abs(x,t)
  | t:(lterm Subs) ts:(lterm Subs)*$   when prio = Appl -> build_app t ts
  | t:(lterm Atom) s:{'[' subst ']'}?$ when prio = Subs -> build_sub t s
  | e:(lctxt Atom) '[' t:(lterm Appl) ']' when prio = Atom -> Ctx(e,t)
and subst =
  | x:lvar s:{"←" -> false | "≔" -> true} u:(lterm Appl)
and lctxt prio =
  | "[]"                          when prio = Atom -> Hole(None)
  | e:cvar                        when prio = Atom -> Hole(Some e)
  | '(' e:(lctxt Appl) ')'        when prio = Atom -> CPar(e)
  | "λ" x:lvar e:(lctxt Appl)     when prio = Atom -> CAbs(x,e)
  | e:(lctxt Subs) t:(lterm Atom) when prio = Appl -> LApp(e,t)
  | t:(lterm Atom) e:(lctxt Subs) when prio = Appl -> RApp(t,e)
  | e:(lctxt Atom)                when prio = Subs -> e
  | e:(lctxt Subs)                when prio = Appl -> e

let parser mvar = "ε" -> "ε" | "α" -> "α" | "β" -> "β" | "γ" -> "γ"
  | "π" -> "π" | "ρ" -> "ρ"

let parser stack =
  | a:mvar                          -> Alp(a)
  | t:(lterm Appl) "·" pi:stack     -> Psh(t,pi)
  | '[' t:(lterm Appl) ']' pi:stack -> Frm(t,pi)

let parser proc =
  | t:(lterm Appl) "∗" pi:stack

let lterm = lterm Appl
let lctxt = lctxt Appl

let blank = EarleyStr.blank_regexp ''[ ]*''

let parse_term s =
  let parse = parse_string ~filename:s lterm blank in
  handle_exception parse s

let parse_ctxt s =
  let parse = parse_string ~filename:s lctxt blank in
  handle_exception parse s

let parse_stac s =
  let parse = parse_string ~filename:s stack blank in
  handle_exception parse s

let parse_proc s =
  let parse = parse_string ~filename:s proc blank in
  handle_exception parse s

let str s = [Maths.Ordinary (Maths.node (Maths.glyphs s))]
let asana n i = [Maths.Ordinary (Maths.node (MathFonts.asana n i))]

let v2m (s, io) =
  match io with
  | None   -> str s
  | Some i -> let n = Maths.node (Maths.glyphs s) in
              let subscript_right = str (string_of_int i) in
              let n = { n with subscript_right } in
              [Maths.Ordinary n]

let rec t2m = function
  | Var(x)   -> v2m x
  | Abs(x,t) -> (str "λ") @ (v2m x) @ (str ".") @ (t2m t)
  | App(t,u) -> (t2m t) @ (str " ") @ (t2m u)
  | Par(t)   -> (str "(") @ (t2m t) @ (str ")")
  | Sub(t,s) -> let (x, s, u) = s in
                let sub l r =
                  let b =
                    if s then MathFonts.asana "\\defeq" 798
                    else Maths.glyphs "←";
                  in
                  [bin 2 (Normal(false, Maths.node b,false)) l r]
                in
                (t2m t) @ (str "[") @ (sub (v2m x) (t2m u)) @ (str "]")
  | Ctx(e,t) -> (c2m e) @ (str "[") @ (t2m t) @ (str "]")
and c2m = function
  | Hole(None)   -> (str "[") @ (asana "wild card" 382) @ (str "]")
  | Hole(Some e) -> v2m e
  | CPar(e)      -> (str "(") @ (c2m e) @ (str ")")
  | CAbs(x,e)    -> (str "λ") @ (v2m x) @ (str ".") @ (c2m e)
  | LApp(e,t)    -> (c2m e) @ (str " ") @ (t2m t)
  | RApp(t,e)    -> (t2m t) @ (str " ") @ (c2m e)

let rec s2m = function
  | Alp(a)    -> v2m (a, None)
  | Psh(v,pi) -> let sym = Maths.node (Maths.glyphs "·") in
                 [bin 2 (Normal(false, sym, false)) (t2m v) (s2m pi)]
  | Frm(t,pi) -> (str "[") @ (t2m t) @ (str "]") @ (s2m pi)

let p2m (t,pi) =
  let sym = Maths.glyphs "∗" in
  let sym = Maths.node sym in
  [bin 2 (Normal(false, sym, false)) (t2m t) (s2m pi)]

module Lang =
  struct
    let lterm s = t2m (parse_term s)
    let lctxt s = c2m (parse_ctxt s)
    let stack s = s2m (parse_stac s)
    let proc  s = p2m (parse_proc s)
  end

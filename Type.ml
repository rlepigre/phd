open Typography
open DefaultFormat
open Maths
open Decap
open KAM

type form =
  | FMeta of vari                      (* A, B    *)
  | FVari of vari                      (* χ       *)
  | FLamb of vari * form               (* (χ ↦ A) *)
  | FSubs of form * vari * form        (* A[χ≔B]  *)
  | FTSub of form * vari * term        (* A[a≔t]  *)
  | FVSub of form * vari * valu        (* A[x≔v]  *)
  | FGSub of form * vari               (* Aρ      *)
  | FAppl of form * form               (* A(B)    *)
  | FFunc of form * form               (* A ⇒ B   *)
  | FGrou of form                      (* (A)     *)
  | FEPrd of (vari * form) list        (* {l_1 : A_1 ; ⋯ ; l_n : A_n} *)
  | FIPrd of vari * vari * cond
  | FESum of (vari * form) list        (* {C_1 : A_1 ; ⋯ ; C_n : A_n} *)
  | FISum of vari * vari * cond
  | FUniv of vari * vari option * form (* ∀χ^s A  *)
  | FExis of vari * vari option * form (* ∃χ^s A  *)
  | FLFix of vari * form               (* μX A    *)
  | FGFix of vari * form               (* νX A    *)
  | FMemb of term * form               (* t∈A     *)
  | FRest of form option * term * term (* A | t≡u *)

let fmeta = vari ["A"; "B"; "C"]
let fvari = vari ["χ"]
let ovari = vari ["X"; "Y"; "Z"]
let svari = vari ["s"]
let parser qvari = ovari | fvari | tvari | vvari
let parser fovari = fvari | ovari

let rec ffunc = function
  | []    -> assert false
  | [a]   -> a
  | a::fs -> FFunc(a, ffunc fs)

type fprio = FAtom | FFull

let parser form prio =
  | a:fmeta                                   when prio = FAtom -> FMeta(a)
  | x:qvari                                   when prio = FAtom -> FVari(x)
  | '(' x:fvari "↦" a:(form FFull) ')'        when prio = FAtom -> FLamb(x,a)
  | a:(form FAtom) '[' x:fovari "≔" b:(form FFull) ']'
                                              when prio = FAtom -> FSubs(a,x,b)
  | a:(form FAtom) s:subsm                    when prio = FAtom -> FGSub(a,s)
  | a:(form FAtom) '[' x:vvari "≔" b:valu ']' when prio = FAtom -> FVSub(a,x,b)
  | a:(form FAtom) '[' x:tvari "≔" b:term ']' when prio = FAtom -> FTSub(a,x,b)
  | a:(form FAtom) '(' b:(form FFull) ')'     when prio = FAtom -> FAppl(a,b)
  | '(' a:(form FFull) ')'                    when prio = FAtom -> FGrou(a)
  | a:(form FAtom) fs:{"⇒" b:(form FAtom)}*$  when prio = FFull -> ffunc (a::fs)
  | '{' fs:{label ':' (form FFull) ';'}* '}'  when prio = FAtom -> FEPrd(fs)
  | "{(" l:label ':' a:fmeta ")" cd:cond '}'  when prio = FAtom -> FIPrd(l,a,cd)
  | '[' fs:{const ':' (form FFull) ';'}* ']'  when prio = FAtom -> FESum(fs)
  | "[(" c:const ':' a:fmeta ")" cd:cond ']'  when prio = FAtom -> FISum(c,a,cd)
  | "∀" x:qvari s:{"^" svari}? a:(form FFull) when prio = FAtom -> FUniv(x,s,a)
  | "∃" x:qvari s:{"^" svari}? a:(form FFull) when prio = FAtom -> FExis(x,s,a)
  | "μ" x:ovari a:(form FFull)                when prio = FAtom -> FLFix(x,a)
  | "ν" x:ovari a:(form FFull)                when prio = FAtom -> FGFix(x,a)
  | t:term "∈" a:(form FAtom)                 when prio = FAtom -> FMemb(t,a)
  | a:{a:(form FAtom) '|'}? t:term "≡" u:term when prio = FAtom -> FRest(a,t,u)

let form = form FFull

let parse_form = parse form

let with_rsup m s =
  let m = Maths.node (fun env _ -> Maths.draw [env] m) in
  [Maths.Ordinary {m with superscript_right = s}]

let rec f2m : form -> Maths.math list = function
  | FMeta(a)       -> vari2m a
  | FVari(x)       -> vari2m x
  | FLamb(x,a)     -> (str "(") @ (bin' 2 "↦" (vari2m x, f2m a)) @ (str ")")
  | FAppl(a,b)     -> (f2m a) @ (str "(") @ (f2m b) @ (str ")")
  | FFunc(a,b)     -> bin' 2 "⇒" (f2m a, f2m b)
  | FSubs(a,x,b)   -> let s = MathFonts.asana "\\defeq" 798 in
                      let s = bin 2 s (vari2m x, f2m b) in
                      (f2m a) @ (str "[") @ s @ (str "]")
  | FTSub(a,x,b)   -> let s = MathFonts.asana "\\defeq" 798 in
                      let s = bin 2 s (vari2m x, t2m b) in
                      (f2m a) @ (str "[") @ s @ (str "]")
  | FVSub(a,x,b)   -> let s = MathFonts.asana "\\defeq" 798 in
                      let s = bin 2 s (vari2m x, v2m b) in
                      (f2m a) @ (str "[") @ s @ (str "]")
  | FGSub(a,s)     -> (f2m a) @ (vari2m s)
  | FGrou(a)       -> (str "(") @ (f2m a) @ (str ")")
  | FEPrd(ls)      -> let build_field (l,a) =
                        bin' 2 ":" (vari2m l, f2m a)
                      in
                      let ls = List.map build_field ls in
                      let c =
                        match ls with
                        | []    -> []
                        | [a]   -> a
                        | f::fs -> let fn m =
                                     (sp 0.6) @ (str ";") @ (sp 0.6) @ m
                                   in
                                   f @ (List.concat (List.map fn fs))
                      in
                      (str "{") @ c @ (str "}")
  | FIPrd(l,a,cnd) -> let c = bin' 2 ":" (vari2m l, vari2m a) in
                      let c =
                        match cnd with
                        | None          ->
                            (str "⋯") @ (sp 0.6) @ c @ (sp 0.6) @ (str "⋯")
                        | Some (i,s,ne) ->
                            let c = (str "(") @ c @ (str ")") in
                            let c =
                              Maths.node (fun env _ -> Maths.draw [env] c)
                            in
                            let s = vari2m s in
                            let s =
                              if ne then bin' 2 "≠" (s, str "∅") else s
                            in
                            let sub = bin' 2 "∈" (str i, s) in
                            let c = { c with subscript_right = sub } in
                            [Maths.Ordinary c]
                      in
                      (str "{") @ c @ (str "}")
  | FESum(ls)      -> let build_field (l,a) =
                        bin' 2 ":" (vari2m l, f2m a)
                      in
                      let ls = List.map build_field ls in
                      let c =
                        match ls with
                        | []    -> []
                        | [a]   -> a
                        | f::fs -> let fn m =
                                     (sp 0.6) @ (str ";") @ (sp 0.6) @ m
                                   in
                                   f @ (List.concat (List.map fn fs))
                      in
                      (str "[") @ c @ (str "]")
  | FISum(l,a,cnd) -> let c = bin' 2 ":" (vari2m l, vari2m a) in
                      let c =
                        match cnd with
                        | None          ->
                            (str "⋯") @ (sp 0.6) @ c @ (sp 0.6) @ (str "⋯")
                        | Some (i,s,ne) ->
                            let c = (str "(") @ c @ (str ")") in
                            let c =
                              Maths.node (fun env _ -> Maths.draw [env] c)
                            in
                            let s = vari2m s in
                            let s =
                              if ne then bin' 2 "≠" (s, str "∅") else s
                            in
                            let sub = bin' 2 "∈" (str i, s) in
                            let c = { c with subscript_right = sub } in
                            [Maths.Ordinary c]
                      in
                      (str "[") @ c @ (str "]")

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
  | FLFix(x,a)     -> (str "μ") @ (vari2m x) @ (f2m a)
  | FGFix(x,a)     -> (str "ν") @ (vari2m x) @ (f2m a)
  | FMemb(t,a)     -> bin' 2 "∈" (t2m t, f2m a)
  | FRest(a,t,u)   -> let eq = bin' 2 "≡" (t2m t, t2m u) in
                      begin
                        match a with
                        | None   -> eq
                        | Some a -> let sep = MathFonts.euler "↾" 248 in
                                    bin 2 sep (f2m a, eq)
                      end


let f : string -> Maths.math list = fun s ->
  try f2m (parse_form s) with _ ->
    Printf.eprintf "Unable to parse the formula \"%s\"\n%!" s; exit 1

open Typography
open Fonts
open FTypes
open Typography.Document
open Util
open UsualMake
open Typography.Box
open Printf

module Cover = struct
  open Driver
  open RawContent

  (* (nom, institution) d'un membre du jury. *)
  type person = string * string
  
  (* Métadonnées pour la couverture. *)
  type metadata =
    { author    : string
    ; title     : string list
    ; def_date  : string
    ; lab       : string
    ; advisor   : person
    ; coadvisor : person option
    ; jury_pres : person
    ; reviewers : person list
    ; examinors : person list }
  
  (* Paramètres de la couverture. *)
  let logo_UG     = "logo_UG.png"
  let logo_header = "logo_header.png"
  let col         = Color.colorHex "#DFDFDF"
  let greyw       = 60.
  let l_span      = 20.
  let r_span      = 10.
  let bot_span    = 20.
  let top_span    = 10.
  let logo_span   = 2.
  let text_pad    = 4.
  
  (* Composition du jury à partir des métadonnées. *)
  let build_jury data =
    let president = [(data.jury_pres, "président")] in
    let advisor   = [(data.advisor, "directeur de thèse")] in
    let coadvisor =
      match data.coadvisor with
      | None   -> []
      | Some p -> [(p, "codirecteur de thèse")]
    in
    let reviewers = List.map (fun p -> (p, "rapporteur")) data.reviewers in
    let examinors = List.map (fun p -> (p, "examinateur")) data.examinors in
    president @ advisor @ coadvisor @ reviewers @ examinors
  
  (* Construit la couverture dans le style de l'UGA. *)
  let cover data env =
    let fill_square (x1, y1) (x2, y2) col =
      let xm = (x1 +. x2) /. 2. in
      let w  = Pervasives.abs_float (x2 -. x1) in
      let lines = [ [| [| xm ; xm |] , [| y1; y2 |] |] ] in
      let param =
        { path_order    = 0
        ; close         = false
        ; strokingColor = Some col
        ; fillColor     = Some col
        ; lineCap       = Butt_cap
        ; lineJoin      = Miter_join
        ; lineWidth     = w
        ; dashPattern   = []
        }
      in Path (param, lines)
    in
  
    let (w4, h4) = Util.a4 in
  
    (* Grey strip on the left and images *)
    let left_grey = fill_square (0., 0.) (greyw, h4 +. 1.) col in
    let (wlogo, hlogo) = ImageLib.size logo_UG in
    let w = greyw -. 2. *. logo_span -. l_span in
    let logo = Image
      { image_file         = logo_UG
      ; image_width        = w
      ; image_height       = w
      ; image_pixel_width  = wlogo
      ; image_pixel_height = hlogo
      ; image_x            = l_span +. logo_span
      ; image_y            = bot_span +. logo_span
      ; image_order        = 1 }
    in
    let (wheader, hheader) = ImageLib.size logo_header in
    let w = w4 /. 2. in
    let h = (float_of_int hheader *. w) /. (float_of_int wheader) in
    let header = Image
      { image_file         = logo_header
      ; image_width        = w
      ; image_height       = h
      ; image_pixel_width  = wlogo
      ; image_pixel_height = hlogo
      ; image_x            = w4 -. w -. r_span
      ; image_y            = h4 -. h -. top_span
      ; image_order        = 1 }
    in
  
    (* Text ... *)
    let x_text = greyw +. text_pad in
    let y_text = ref (0.8 *. h4) in
    let text = ref [] in
  
    let add_line ?(off=0.) fsize txt =
      let txt = draw {env with size = fsize} (sc [tT txt]) in
      let f = translate (x_text +. off) !y_text in
      let txt = List.map f txt in
      text := !text @ txt
    in
  
    let hspace sz =
      y_text := !y_text -. sz
    in
  
    let jury_member ((name,inst), func) =
      hspace 6.;
      add_line ~off:8. 4. (Printf.sprintf "%s (%s)" name func);
      hspace 4.;
      add_line ~off:16. 4. inst
    in
  
    let add_centered_line fsize txt =
      let txt = draw {env with size = fsize} (sc [tT txt]) in
      let txtw =
        let (x0,_,x1,_) = bounding_box txt in
        Pervasives.abs_float (x1 -. x0)
      in
      let off = (w4 -. greyw -. txtw -. r_span) /. 2. in
      let f = translate (x_text +. off) !y_text in
      let txt = List.map f txt in
      text := !text @ txt
    in
  
    let title l =
      let title_line i txt =
        if i > 0 then hspace 8.;
        add_centered_line 7. txt
      in
      List.iteri title_line l
    in
  
    add_line 7. "Thèse";
    hspace 10.;
    add_line 4. "pour obtenir le grade de";
    hspace 10.;
    add_line 7. "Docteur de la Communauté Université";
    hspace 7.;
    add_line 7. "Grenoble Alpes";
    hspace 8.;
    add_line 4. "spécialité : Informatique";
    hspace 14.;
    add_line 4. "présentée par";
    hspace 5.;
    add_line 6. data.author;
    hspace 10.;
    add_line 4. ("dirigée par " ^ fst data.advisor);
    begin
      match data.coadvisor with
      | None       -> ()
      | Some (n,_) -> (hspace 4.; add_line 4. ("et codirigée par " ^ n))
    end;
    hspace 10.;
    add_line 4. ("préparée au sein du " ^ data.lab);
    hspace 4.;
    add_line 4. "et de l'école doctorale MSTII";
  
    hspace 14.;
    title data.title;
    hspace 14.;
  
    add_line 4. ("Thèse soutenue publiquement le " ^ data.def_date);
    hspace 4.;
    add_line 4. "devant un jury composé de";
    hspace 4.;
    List.iter jury_member (build_jury data);
  
    let contents = [left_grey; logo; header] @ !text in
    let contents = List.map (translate (-.35.) (-.248.)) contents in
    let dr =
      { drawing_min_width     = 0.
      ; drawing_nominal_width = 0.
      ; drawing_max_width     = 0.
      ; drawing_width_fixed   = true
      ; drawing_adjust_before = false
      ; drawing_y0            = 0.
      ; drawing_y1            = 0.
      ; drawing_badness       = (fun _ -> 0.)
      ; drawing_contents      = (fun _ -> contents)
      ; drawing_break_badness = 0.0
      ; drawing_states        = []}
    in
    [bB (fun _ -> [Drawing dr])]
end

let id x = x

let mathsT t0=
  let t1=if t0.[String.length t0-1]=' ' then tT t0::[tT" "] else [tT t0] in
  let t=if t0.[0]=' ' then tT" "::t1 else t1 in
  let l=
    Maths.Ordinary (Maths.node (fun env st->boxify_scoped
                                  { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                  t ))
  in
  [l]

let mathsText t0=
  [Maths.Ordinary (Maths.node (fun env st->boxify_scoped
                                 { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                 t0 ))]


let skip x=bB (fun env->let w=env.size*.x in [glue w w w])

module Format (D:DocumentStructure) = struct
  module Default = DefaultFormat.Format(D)

  let parameters = Default.parameters
  let verbatim = Default.verbatim
  module Make_theorem    = Default.Make_theorem
  module Env_itemize     = Default.Env_itemize
  module Env_env         = Default.Env_env
  module Env_center      = Default.Env_center
  module Env_raggedRight = Default.Env_raggedRight
  module Env_proof       = Default.Env_proof
  module Env_noindent    = Default.Env_noindent
  let figure_here = Default.figure_here
  let figure = Default.figure
  let verbs_default = Default.verbs_default
  let env_stack = Default.env_stack

  (* Secial page. *)
  let special_page pages_after contents =
    let param a b c d e f g line =
      { (parameters a b c d e f g line) with
        min_page_before = 0;
        min_page_after = 1 + pages_after;
        left_margin = 0.0
      }
    in
    let environment env = {env with par_indent = []} in
    newPar ~environment D.structure Complete.normal param contents

  module Output (M:Driver.OutputDriver) = struct
    include Default.Output(M)

    let rec sectionize path = function
      | Node n when List.mem_assoc "structural" n.node_tags ->
          let numbered = List.mem_assoc "numbered" n.node_tags in
          let get_nums env =
            let (_,l) =
              try StrMap.find "_structure" env.counters
              with Not_found -> (-1,[0])
            in
            let to_str x = string_of_int (x + 1) in
            String.concat "." (List.rev_map to_str (drop 1 l))
          in
          let minichap = List.mem_assoc "minichap" n.node_tags in
          let section_name =
            if path = [] && minichap then
              (* Mini chapter. *)
              let marker = bB (fun _ -> [Marker (Structure path)]) in
              marker :: n.displayname
            else if path = [] then
              (* Chapter level. *)
              let contents env =
                let h= -.env.size/.phi in
                let sz=2.5 in
  
                let buf=ref [||] in
                let nbuf=ref 0 in
                let env'=boxify buf nbuf env n.displayname in
                let boxes=Array.to_list (Array.sub !buf 0 !nbuf) in
                let users=
                  List.filter (fun x->match x with
                      Marker _->true | _->false)
                    boxes
                in
  
                let num =
                  if numbered then
                    let nums = get_nums env in
                    List.map (RawContent.in_order 1)
                      (draw {env with size=env.size*.(sz-.h);fontColor=Color.gray }
                      [tT nums])
                  else []
                in
                let x0,x1=if num=[] then 0.,0. else
                    let x0,_,x1,_=RawContent.bounding_box num in x0,x1
                in
                let w=if num=[] then 0. else env.size in
  
                let text=
                  let dr =
                    try
                      snd (IntMap.min_binding (
                        let d,_,_ = (* FIXME: lost Marker(Label ...) ?? *)
                          OutputDrawing.minipage'
                            { env with hyphenate = (fun _->[||])
                            ; normalLeftMargin=0.
                            ; normalMeasure=env.normalMeasure-.(x1-.x0)/.2.-.w
                            ; size=env.size*.sz }
                            (DefaultFormat.paragraph n.displayname)
                         in d))
                    with Not_found -> empty_drawing_box
                  in
                  List.map (RawContent.in_order 1)
                    (dr.drawing_contents dr.drawing_nominal_width)
                in

                let trs = RawContent.translate (-.w-.x1) h in
                let num = List.map trs num in
                let dr = drawing (num @ text) in
                let dr =
                  let drawing_contents w =
                    let trs = RawContent.translate ((x0-.x1)/.2.) 0. in
                    List.map trs (dr.drawing_contents w)
                  in Drawing { dr with drawing_contents }
                in
                let path = Structure path in
                let title = users @ [Marker path; Marker AlignmentMark; dr] in
                [bB (fun _ -> title); Env(fun _ -> env')]
              in [C contents]
            else
              (* Deeper level. *)
              let mark = [bB (fun _ -> [Marker (Structure path)])] in
              let numb =
                if numbered then [C (fun env -> [tT (get_nums env ^ " ")])]
                else []
              in
              let name = n.displayname in
              mark @ (size 7.5 (color Color.gray numb)) @ size 6.0 name
          in
          let par = Paragraph
            { par_contents = section_name
            ; par_paragraph = (-1)
            ; par_states = []
            ; par_env = (fun env ->
                let len =
                  try List.length (snd (StrMap.find "_structure" env.counters))
                  with Not_found -> 1
                in
                let alt = if len >= 4 then Regular else Caps in
                let feats = Opentype.oldStyleFigures :: env.fontFeatures in
                let hyphenate _ = [||] in
                let size = env.size *.
                  match len with
                  | 1 -> sqrt phi
                  | 2 -> sqrt (sqrt phi)
                  | 3 -> sqrt (sqrt (sqrt phi))
                  | _ -> 1.0
                in
                let size =
                  if minichap then sqrt phi *. sqrt phi *. size
                  else size
                in
                { (envAlternative feats alt env) with hyphenate
                ; size ; par_indent = [] })
            ; par_post_env = (fun env1 env2 ->
                { env1 with names = env2.names
                ; counters = env2.counters
                ; user_positions = env2.user_positions })
            ; par_badness = badness
            ; par_parameters = (fun env a1 a2 a3 a4 a5 a6 line ->
                let param = parameters env a1 a2 a3 a4 a5 a6 line in
                if path = [] then
                  if not param.absolute && line.lineStart = 0 then
                    let boxes = a1.(line.paragraph) in
                    let rec findMark w j =
                      if j >= line.lineEnd then 0.0
                      else if boxes.(j) = Marker AlignmentMark then w
                      else
                        let (_,ww,_) = box_interval boxes.(j) in
                        findMark (w +. ww) (j + 1)
                    in
                    let w = findMark 0.0 0 in
                    let min_lines_after =
                      if line.lineEnd >= Array.length boxes then 4 else 1
                    in
                    let min_page_before =
                      if line.paragraph = 0 then 0
                      else if line.lineStart > 0 then param.min_page_before
                      else
                        let minimal = max param.min_page_before 1 in
                        minimal + ((1 + max 0 (layout_page line) + minimal) mod 2)
                    in
                    let left_margin =
                      if minichap then
                        let w = param.measure -. line.nom_width in
                        param.left_margin +. w /. 2.0
                      else param.left_margin -. w
                    in
                    { param with left_margin ; min_lines_after
                    ; min_page_before ; measure = param.measure +. w }
                  else param
                else
                  let min_lines_after =
                    if line.lineEnd>=Array.length a1.(line.paragraph) then 2
                    else 0
                  in
                  { param with min_page_before = param.min_page_before
                  ; min_lines_before = 2 ; min_lines_after
                  ; not_last_line = true })
            ; par_completeLine = Complete.normal }
          in
          let children =
            let k =
              try fst (IntMap.min_binding n.children) - 1
              with Not_found -> 0
            in
            let aux k a = sectionize (k :: path) a in
            IntMap.add k par (IntMap.mapi aux n.children)
          in
          Node { n with children }
      | Node n ->
          let children = IntMap.map (sectionize path) n.children in
          Node { n with children }
      | leaf   -> leaf

    let output params str env file =
      let postprocess_tree = sectionize [] in
      basic_output params (postprocess_tree str) env file
  end



  let minipage = OutputDrawing.minipage
  let displayedFormula = Default.displayedFormula
  let node = DefaultFormat.node
  let paragraph = DefaultFormat.paragraph
  let alegreya = DefaultFormat.alegreya

  let replace_utf8 x y z=
    Str.global_replace x
      (UTF8.init 1 (fun _->UChar.chr y)) z

  let defaultEnv =
    let env = envFamily alegreya Default.defaultEnv in
    let (size, lead) = (env.size, env.lead) in
    (* let (size, lead) = (3.5, 5.0) in (* 10pt *) *)
    { env with size ; lead
    ; show_boxes = false
    ; word_substitutions=
        (fun x->List.fold_left (fun y f->f y) x
           [
             replace_utf8 (Str.regexp_string "``") 8220;
             replace_utf8 (Str.regexp_string "''") 8221
           ]
        )
    ; counters = StrMap.add "figure" (2,[]) StrMap.empty
    ; math_break_badness = infinity
    }

  let title=Default.title

  open Util

  let utf8Char x=[tT (UTF8.init 1 (fun _->UChar.chr x))]
  let glyph x=
    bB (fun env->
         let code={glyph_utf8=""; glyph_index=x } in
           [glyphCache env.font code env.fontColor env.size]
      )
  let q _=utf8Char 8220
  let qq _=utf8Char 8221

  module Env_minichap(M : sig val arg1 : content list end) =
    struct
      let do_begin_env () =
        let extra_tags = [("minichap", "")] in
        newStruct ~numbered:false ~extra_tags D.structure M.arg1

      let do_end_env () =
        go_up D.structure
    end

  let table_of_contents tree depth =
    let table_of_contents env =
      let margin  = env.size *. phi in
      let spacing = env.size /. phi in

      let rec toc env0 path tree =
        let level = List.length path in
        match tree with
        | Node s when level <= depth && List.mem_assoc "intoc" s.node_tags ->
           let rec flat_children env1 = function
             | []                    -> []
             | (_,(FigureDef _))::s  -> flat_children env1 s
             | (_,(Paragraph _))::s  -> flat_children env1 s
             | (k,(Node h as tr))::s ->
                 let env' = h.node_env env1 in
                 (toc env' (k::path) tr)
                 @ flat_children (h.node_post_env env1 env') s
           in
           let numbered = List.mem_assoc "numbered" s.node_tags in

           let chi =
             if numbered || path = [] then
               flat_children env0 (IntMap.bindings s.children)
             else []
           in
           let count =
             let (_,l) =
               try StrMap.find "_structure" (env0.counters)
               with Not_found -> (-1,[0])
             in List.rev (drop 1 l)
           in
           if (* numbered && *) count <> [] then (
             let labl=String.concat "_" ("_"::List.map string_of_int path) in
             let page =
               let ups = user_positions env0 in
               try 1 + layout_page (MarkerMap.find (Label labl) ups)
               with Not_found -> 0
             in
             let env'= add_features [Opentype.oldStyleFigures] env in
             let fontColor = if level = 1 then Color.red else Color.black in
             let num = boxify_scoped { env' with fontColor }
               [tT (String.concat "." (List.map
                 (fun x->string_of_int (x+1)) count))] in
             let name = boxify_scoped env' s.displayname in
             let w=List.fold_left (fun w b->
               let (_,w',_)=box_interval b in w+.w') 0. num in

             let numb =
               if not numbered then [] else
               let numb = draw_boxes env num in
               List.map (RawContent.translate (-.w-.spacing) 0.0) numb
             in
             let text =
               let text = draw_boxes env name in
               let pad = if numbered then 0.0 else -.w -. spacing in
               List.map (RawContent.translate pad 0.0) text
             in
             let (wpage, page) =
               let page = [tT (string_of_int page)] in
               let page =
                 let env' = (* envItalic true *) {env' with fontColor} in
                 draw_boxes env (boxify_scoped env' page)
               in
               let (x1,_,x2,_) = RawContent.bounding_box page in
               let pad = env.normalMeasure +. x1 -. x2 in
               (x2 -. x1, List.map (RawContent.translate pad 0.0) page)
             in
             let (wcont, cont) =
               let cont = numb @ text in
               let (x1,_,x2,_) = RawContent.bounding_box cont in
               let level = float_of_int (level - 1) in
               let pad = margin +. spacing *. 3.0 *. level in
               let cont = List.map (RawContent.translate pad 0.0) cont in
               (x2 -. x1 +. pad, cont)
             in
             let line =
               let p2 = (0.0, 0.0) in
               let p1 = (env.normalMeasure -. 6.0 -. wcont, 0.0) in
               let path = [[|RawContent.line p1 p2|]] in
               let param =
                 let open RawContent in
                 let (dashPattern, lineWidth) =
                   if level = 1 then ([0.2; 1.0], 0.2)
                   else ([0.2; 1.0], 0.2)
                 in
                 { default_path_param with dashPattern ; lineWidth }
               in
               let line = [RawContent.Path(param, path)] in
               let voff = 0.0 in (* Vertical offset of the line. *)
               List.map (RawContent.translate wcont voff) line
             in
             let cont = cont @ page @ line in
             let (a,b,c,d)=RawContent.bounding_box cont in
             Marker (BeginLink (Intern labl))::
               Drawing {
                 drawing_min_width=env.normalMeasure;
                 drawing_nominal_width=env.normalMeasure;
                 drawing_max_width=env.normalMeasure;
                 drawing_width_fixed = true;
                 drawing_adjust_before = false;
                 drawing_y0=b;
                 drawing_y1=d;
                 drawing_break_badness=0.;
                 drawing_badness=(fun _->0.);
                 drawing_states=[];
                 drawing_contents = (fun _-> cont)
               }::Marker EndLink::(glue 0. 0. 0.)::chi
           )
           else chi
        | _ -> []
      in
      let counters = StrMap.add "_structure" (-1,[0]) env.counters in
      toc { env with counters } [] (fst (top !tree))
    in
    let env x = { x with par_indent = [] } in
    let toc = [bB table_of_contents] in
    newPar tree ~environment:env Complete.normal Default.center toc

  module TableOfContents = struct
    let do_begin_env () = table_of_contents D.structure 3
    let do_end_env () = ()
  end
end


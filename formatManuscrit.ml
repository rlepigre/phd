open Typography
open Fonts
open FTypes
open Typography.Document
open Util
open UsualMake
open Typography.ConfigUtil
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
    let reviewers = List.map (fun p -> (p, "raporteur")) data.reviewers in
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
    let (wlogo, hlogo) = ReadImg.size logo_UG in
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
    let (wheader, hheader) = ReadImg.size logo_header in
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
    let y_text = ref (0.75 *. h4) in
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
    add_line 7. "Docteur de l'Université de Grenoble";
    hspace 5.;
    add_line 4. "spécialité : Mathématiques et Informatique";
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
    Maths.Ordinary (Maths.noad (fun env st->boxify_scoped
                                  { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                  t ))
  in
  [l]

let mathsText t0=
  [Maths.Ordinary (Maths.noad (fun env st->boxify_scoped
                                 { env with size=env.size*.(Maths.env_style env.mathsEnvironment st).Mathematical.mathsSize }
                                 t0 ))]


let skip x=bB (fun env->let w=env.size*.x in [glue w w w])

module Format (D:DocumentStructure) = struct
  module Default = DefaultFormat.Format(D)

  (* Secial page. *)
  let special_page pages_after contents =
    let param a b c d e f g line =
      { (parameters a b c d e f g line) with
        min_page_before = 0;
        min_page_after = 1 + pages_after;
        left_margin = 0.0
      }
    in
    newPar D.structure Complete.normal param contents

  module TableOfContents = Default.TableOfContents

  module Output (M:Driver.OutputDriver) = struct
    include Default.Output(M)

    let postprocess_tree tree =
      let rec sectionize path numbered=function
          Node n when List.mem_assoc "structural" n.node_tags ->
              let numbered'=numbered && List.mem_assoc "numbered" n.node_tags in
            let section_name=
              if path=[] then (
                [C (fun env->
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
    
                  let num=
                    if List.mem_assoc "numbered" n.node_tags then
                      let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                      List.map (RawContent.in_order 1)
                        (draw {env with size=env.size*.(sz-.h);fontColor=Color.gray }
                           [tT (String.concat "." (List.map (fun x->string_of_int (x+1))
                                                     (List.rev (drop 1 b))))])
                    else
                      []
                  in
                  let x0,x1=if num=[] then 0.,0. else
                      let x0,_,x1,_=RawContent.bounding_box num in x0,x1
                  in
                  let w=if num=[] then 0. else env.size in
    
                  let text=
                    let dr=try
                             snd (IntMap.min_binding (
                               let d,_,_ = (* FIXME: lost Marker(Label ...) ?? *)
                               OutputDrawing.minipage' {env with hyphenate=(fun _->[||]);
                                 normalLeftMargin=0.;
                                 normalMeasure=env.normalMeasure-.(x1-.x0)/.2.-.w;
                                 size=env.size*.sz}
                                 (DefaultFormat.paragraph n.displayname) in d
                             ))
                      with
                          Not_found->empty_drawing_box
                    in
                    List.map (RawContent.in_order 1)
                      (dr.drawing_contents dr.drawing_nominal_width)
                  in
                  let dr=drawing (
                    (List.map (RawContent.translate (-.w-.x1) h) num)@
                      text
                  )
                  in
                  let dr={dr with drawing_contents=(fun w_->
                    List.map (RawContent.translate ((x0-.x1)/.2.) 0.) (dr.drawing_contents w_)
                  )}
                  in
                  bB (fun _->
                    users@
                      [Marker (Structure path);
                       Marker AlignmentMark;
                       Drawing (dr)])::
                    Env(fun _->env')::[]
                )]
              ) else (
                if List.mem_assoc "numbered" n.node_tags  then
                  [C (fun env->
                    let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
                    bB (fun _->[Marker (Structure path)])
                    ::tT (String.concat "." (List.map (fun x->string_of_int (x+1)) (List.rev (drop 1 b))))
                    ::tT " "
                    ::n.displayname
                  )]
                else
                  bB (fun env->[Marker (Structure path)])::
                    n.displayname
              )
            in
            let par=Paragraph {
              par_contents=section_name;
              par_paragraph=(-1);par_states=[];
              par_env=(fun env->
                         let a,b=try StrMap.find "_structure" env.counters with Not_found -> -1,[0] in
    
                         { (envAlternative (Opentype.oldStyleFigures::env.fontFeatures)
                              (if List.length b>=4 then Regular else Caps) env) with
                           hyphenate=(fun _->[||]);
                             size=(if List.length b=1 then sqrt phi else
                                     if List.length b <= 2 then sqrt (sqrt phi) else
                                       if List.length b = 3 then sqrt (sqrt (sqrt phi)) else 1.)*.env.size;
                         });
              par_post_env=(fun env1 env2 -> { env1 with names=env2.names; counters=env2.counters;
                                                 user_positions=env2.user_positions });
              par_badness=badness;
              par_parameters=
                if path=[] then
                  (fun env a1 a2 a3 a4 a5 a6 line->
                    let p=parameters env a1 a2 a3 a4 a5 a6 line in
                    if not p.absolute && line.lineStart=0 then (
                      let rec findMark w j=
                        if j>=line.lineEnd then 0. else
                          if a1.(line.paragraph).(j) = Marker AlignmentMark then w else
                            let (_,ww,_)=box_interval a1.(line.paragraph).(j) in
                            findMark (w+.ww) (j+1)
                      in
                      let w=findMark 0. 0 in
                      { p with
                        left_margin=p.left_margin-.w;
                        min_lines_after=if line.lineEnd>=Array.length a1.(line.paragraph) then 4 else 1;
                        min_page_before = (
                          if line.paragraph=0 then 0 else
                          if path=[] && line.lineStart<=0 then (
                            let minimal=max p.min_page_before 1 in
                            minimal+((1+max 0 (layout_page line)+minimal) mod 2)
                          ) else p.min_page_before
                        );
                        measure=p.measure+.w }
                    ) else
                      p
                  )
                else
                  (fun a b c d e f g line->
                    let param=parameters a b c d e f g line in
                    { param with
                      min_page_before = (
                        if path=[] && line.lineStart<=0 then (
                          let minimal=max param.min_page_before 1 in
                          minimal+((layout_page g+minimal) mod 2)
                        ) else param.min_page_before
                      );
                      min_lines_before=2;
                      min_lines_after=
                        if path=[] then
                          if line.lineEnd>=Array.length b.(line.paragraph) then 3 else 0
                        else
                          if line.lineEnd>=Array.length b.(line.paragraph) then 2 else 0;
                      not_last_line=true }
                  );
              par_completeLine=Complete.normal }
            in
              Node { n with children=
                  IntMap.add
                    (try fst (IntMap.min_binding n.children)-1 with Not_found->0)
                    par
                    (IntMap.mapi (fun k a->sectionize (k::path) numbered' a) n.children)
                   }
        | Node n->
           Node { n with children=IntMap.map (sectionize path numbered) n.children }
        | a->a
      in
      match tree with
      | Node n-> Node { n with children=IntMap.map (sectionize [] true) n.children }
      | _     -> tree

    let output out_params structure defaultEnv file =
      basic_output out_params (postprocess_tree structure) defaultEnv file
  end



  let minipage=OutputDrawing.minipage
  let displayedFormula=Default.displayedFormula
  let node=DefaultFormat.node
  let paragraph=DefaultFormat.paragraph


  let alegreya=
    [ Regular,
      (Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Roman; weight=Regular}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
                make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
                make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
                make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
               ]),
            (fun x->x)),
       Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Italic; weight=Regular}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
                make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
                make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
                make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
               ]),
            (fun x->x)));
      Bold,
      (Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Roman; weight=Bold}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [168;175] {glyph_utf8="fi";glyph_index=245};
                make_ligature [168;181] {glyph_utf8="fl";glyph_index=246};
                make_ligature [168;177] {glyph_utf8="fj";glyph_index=383};
                make_ligature [175;177] {glyph_utf8="ij";glyph_index=176};
               ]),
            (fun x->x)),
       Lazy.lazy_from_fun
         (fun ()->
            (Fonts.loadFont
              (findFont FontPattern.({family="Alegreya"; slant=Italic; weight=Bold}))
            ),
            (fun x->x),
            (fun x->List.fold_left (fun a f->f a) x
               [make_ligature [162;170] {glyph_utf8="fi";glyph_index=477};
                make_ligature [162;175] {glyph_utf8="fl";glyph_index=478};
                make_ligature [162;171] {glyph_utf8="fj";glyph_index=482};
                make_ligature [170;171] {glyph_utf8="ij";glyph_index=476};
               ]),
            (fun x->x)));

      Caps,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Alegreya SC"; slant=Roman; weight=Regular})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Alegreya SC"; slant=Italic; weight=Regular})
        ))
      );

      Regular,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Roman; weight=Regular})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Italic; weight=Regular})
        ))
      );

      Bold,
      (
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Roman; weight=Bold})
        )),
        simpleFamilyMember (fun ()->Fonts.loadFont (findFont
              FontPattern.({family="Philosopher"; slant=Italic; weight=Bold})
        ))
      );
    ]

  let replace_utf8 x y z=
    Str.global_replace x
      (UTF8.init 1 (fun _->UChar.chr y)) z
  let defaultEnv=
    let size=3.8 in
    let env=(envFamily alegreya Default.defaultEnv) in
    {  env with
         size=size;
         show_boxes=false;
         lead=5.5;
         mathsEnvironment=
        (* Array.map (fun x->{x with Mathematical.kerning=false }) *)
          env.mathsEnvironment;
         word_substitutions=
        (fun x->List.fold_left (fun y f->f y) x
           [
             replace_utf8 (Str.regexp_string "``") 8220;
             replace_utf8 (Str.regexp_string "''") 8221
           ]
        );
         counters=StrMap.add "figure" (2,[]) StrMap.empty
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






  let boxes_width env contents =
    let boxes = boxify_scoped env contents in
    let w = List.fold_left
      (fun w x -> let _,a,_ = Box.box_interval x in w +. a)
      0.
      boxes
    in
    boxes, w

  let boxes_y0 boxes =
    List.fold_left
      (fun res box -> min res (Box.lower_y box))
      0.
      boxes

  let boxes_y1 boxes =
    List.fold_left
      (fun res box -> max res (Box.upper_y box))
      0.
      boxes

  let equation contents =
    let pars a b c d e f g line={(parameters a b c d e f g line) with
                              min_height_before=
        if line.lineStart=0 then a.lead else 0.;
                              min_height_after=
        if line.lineEnd>=Array.length b.(line.paragraph) then a.lead else 0.
                           }
    in
    newPar ~environment:(fun env -> { env with par_indent = [] })
      D.structure Complete.normal pars
      [ Env (fun env ->Document.incr_counter "equation" env) ;
        C (fun env ->
             let _,w = boxes_width env contents in
             let _,x = try StrMap.find "equation" env.counters with _-> -1,[] in
             let num,w' = boxes_width env
               (italic [tT "(";
                        tT (string_of_int (1 + List.hd x));
                        tT ")" ]) in
             let w0=(env.normalMeasure -. w)/.2. in
             let w1=env.normalMeasure -. w'-.w0-.w in
             bB(fun _->[glue w0 w0 w0])::
               contents@
               [bB (fun _->glue w1 w1 w1 :: num)]
          )];
    []

end

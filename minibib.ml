(* A very basic datatype for a bibliographic entry. *)
type entry =
  { key     : string
  ; authors : string
  ; title   : string
  ; notes   : string
  ; year    : string
  ; url     : string }

type bibliography =
  (string * (entry * bool ref)) list

(* Low level storage of the bibliography. *)
module File =
  struct
    let write : string -> bibliography -> unit = fun fn b ->
      let oc = open_out fn in
      output_value oc b;
      close_out oc

    let read : string -> bibliography = fun fn ->
      try
        let ic = open_in fn in
        let b = input_value ic in
        close_in ic; b
      with _ -> []

    let create : string -> unit = fun fn ->
      write fn []

    let insert : string -> entry -> unit = fun fn e ->
      let b = read fn in
      if List.mem_assoc e.key b then
        Printf.eprintf "[Minibib] Conflicting key %S.\n%!" e.key
      else
        write fn ((e.key, (e, ref false)) :: b)

    let touch : string -> string -> unit = fun fn key ->
      let b = read fn in
      try
        let (_,r) = List.assoc key b in
        r := true; write fn b;
      with Not_found ->
        Printf.eprintf "[Minibib] Not found %S.\n%!" key
  end

type biblio = string

let create : string -> biblio =
  fun name ->
    let bib = ".patobuild/" ^ name ^ ".minibib" in
    File.create bib; bib

let from_name : string -> biblio =
  fun name ->
    let bib = ".patobuild/" ^ name ^ ".minibib" in
    let _ = File.read bib in
    bib

let insert : entry -> biblio -> unit =
  fun e bib ->
    File.insert bib e

let insert_many : entry list -> biblio -> unit =
  fun es bib ->
    List.iter (fun e -> insert e bib) es

let entries : biblio -> entry list =
  fun bib ->
    let es = File.read bib in
    let es = List.filter (fun (_,(_,u)) -> !u) es in
    let es = List.map (fun (_,(e,_)) -> e) es in
    List.sort (fun e1 e2 -> String.compare e1.key e2.key) es

let cite : biblio -> (string list -> 'a) -> string list -> 'a =
  fun bib act ks ->
    if List.length ks < 1 then
      Printf.eprintf "[Minibib] No keys provided.\n%!";
    List.iter (File.touch bib) ks;
    act ks

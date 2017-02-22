(* Type of a bibliography entry. *)
type entry =
  { key     : string   (* Key used for citation. *)
  ; authors : string   (* Authors. *)
  ; title   : string   (* Title. *)
  ; notes   : string   (* Additional data (e.g. publisher). *)
  ; year    : string   (* Year. *)
  ; url     : string } (* URL (usually DOI). *)

(* Abstract bibliography type. *)
type biblio

(* [create name] creates a new bibliography with the name [name]. The name
   sould be unique for a given document. Unspecified behaviours are to be
   expected otherwise. *)
val create : string -> biblio

(* [from_name name] acquires a bibliography that was previously created with
   the name [name]. *)
val from_name : string -> biblio

(* [insert e b] inserts the entry [e] in the bibliography [b]. If the key
   for the entry is already used, a warning message is displayed. In this
   case, the given entry is not inserted. *)
val insert : entry -> biblio -> unit

(* Similar to [insert] with several different entries at once. *)
val insert_many : entry list -> biblio -> unit

(* [entries b] produces a list of entries sorted by author name. Only the
   entries that were previously referenced using [cite] or [mcite] are in
   this list. *)
val entries : biblio -> entry list

(* [cite bib act ks] produces a reference to the entries of bibliography
   [b] having their key listed in [ks]. The function [act] is used to
   actually produce a reference to display. *)
val cite : biblio -> (string list -> 'a) -> string list -> 'a

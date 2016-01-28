let bibliography =
  [ ( "Church1941"
    , ( "Alonzo Church"
      , "The Calculi of Lambda-Conversion"
      , "Princeton University Press"
      , "1941" ) )
  ; ( "Turing1937"
    , ( "Alan Turing"
      , "Computability and Lambda-Definability"
      , "Journal of Symbolic Logic"
      , "1937" ) )
  ; ( "Krivine1990"
    , ( "Jean-Louis Krivine"
      , "Lambda-calcul, types et modèles"
      , "Masson"
      , "1990" ) )
  ; ( "Krivine2007"
    , ( "Jean-louis Krivine"
      , "A call-by-name lambda-calculus machine"
      , "Higher Order and Symbolic Computation"
      , "2007" ) )
  ; ( "Parigot1992"
    , ( "Michel Parigot"
      , "λμ-calculus: an algorithic interpretation of classical natural deduction"
      , "Lecture Notes in Computer Science"
      , "1992" ) )
  ]

let init () =
  let open Minibib in
  let biblio = create "biblio" in
  let f (key, (authors, title, notes, year)) =
    { key ; authors ; title ; notes ; year }
  in
  Minibib.insert_many (List.map f bibliography) biblio

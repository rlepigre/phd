open Minibib

let (++) s1 s2 = String.concat "" [s1;s2]

let biblio =
  [

  { key     = "Church1941"
  ; authors = "Alonzo Church"
  ; title   = "The Calculi of Lambda-Conversion"
  ; notes   = "Princeton University Press"
  ; year    = "1941" };

  { key     = "Turing1937"
  ; authors = "Alan Turing"
  ; title   = "Computability and Lambda-Definability"
  ; notes   = "Journal of Symbolic Logic"
  ; year    = "1937" };

  { key     = "Krivine1990"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Lambda-calcul, types et modèles"
  ; notes   = "Masson"
  ; year    = "1990" };

  { key     = "Krivine2007"
  ; authors = "Jean-louis Krivine"
  ; title   = "A call-by-name lambda-calculus machine"
  ; notes   = "Higher Order and Symbolic Computation"
  ; year    = "2007" };

  { key     = "Parigot1992"
  ; authors = "Michel Parigot"
  ; title   = "λμ-calculus: an algorithic interpretation of classical "
              ++ "natural deduction"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "1992" };

  { key     = "Garrigue1998"
  ; authors = "Jacques Garrigue"
  ; title   = "Programming with Polymorphic Variants"
  ; notes   = "Proceedings of the ML Workshop"
  ; year    = "1998" };

  { key     = "Girard1972"
  ; authors = "Jean-Yves Girard"
  ; title   = "Interprétation fonctionnelle et élimination des coupures "
              ++ "de l'arithmétique d'ordre supérieur"
  ; notes   = "Thèse de l'Université Paris 7"
  ; year    = "1972" };

  { key     = "Reynolds1974"
  ; authors = "John C. Reynolds"
  ; title   = "Towards a Theory of Type Structure"
  ; notes   = "Proceedings Colloque sur la Programmation"
  ; year    = "1974" };

  { key     = "Griffin1990"
  ; authors = "Timothy G. Griffin"
  ; title   = "A Formulae-as-Types Notion of Control"
  ; notes   = "Proceedings of POPL"
  ; year    = "1990" };

  { key     = "Wright1994"
  ; authors = "Andrew K. Wright and Matthias Felleisen"
  ; title   = "A Syntactic Approach to Type Soundness"
  ; notes   = "Information and Computation (Volume 15, Issue 1)"
  ; year    = "1994" };

  { key     = "Wright1995"
  ; authors = "Andrew K. Wright"
  ; title   = "Simple Imperative Polymorphism"
  ; notes   = "Lisp and Symbolic Computation (Volume 8, Number 4)"
  ; year    = "1995" };

  { key     = "Garrigue2004"
  ; authors = "Jacques Garrigue"
  ; title   = "Relaxing the Value Restriction"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "2004" };

  { key     = "Tofte1990"
  ; authors = "Mads Tofte"
  ; title   = "Type Inference for Polymorphic References"
  ; notes   = "Information and Computation (Volume 89, Issue 1)"
  ; year    = "1990" };

  { key     = "Damas1982"
  ; authors = "Luís Damas and Robin Milner"
  ; title   = "Principal Type-Schemes for Functional Programs"
  ; notes   = "Proceedings of POPL"
  ; year    = "1982" };

  { key     = "Harper1991"
  ; authors = "Robert Harper and Mark Lillibridge"
  ; title   = "ML with callcc is unsound"
  ; notes   = "Message posted to the SML mailing list"
  ; year    = "1991" };

  ]

let init () =
  insert_many biblio (create "biblio")

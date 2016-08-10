open Minibib

let (++) s1 s2 = String.concat "" [s1;s2]

let biblio =
  [

  { key     = "Church1941"
  ; authors = "Alonzo Church"
  ; title   = "The Calculi of Lambda-Conversion"
  ; notes   = "Princeton University Press"
  ; year    = "1941" };

  { key     = "Barendregt1981"
  ; authors = "Hendrik Pieter Barendregt"
  ; title   = "The Lambda Calculus - Its Syntax and Semantics"
  ; notes   = "North-Holland"
  ; year    = "1981" };

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

  { key     = "Krivine2009"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Realizability in Classical Logic"
  ; notes   = "Panoramas et Synthèses (Volume 27)"
  ; year    = "2009" };

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
  ; notes   = "Thèse de l'Université Paris VII"
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

  { key     = "Leroy1991"
  ; authors = "Xavier Leroy and Pierre Weis"
  ; title   = "Polymorphic Type Inference and Assignment"
  ; notes   = "Proceedings of POPL"
  ; year    = "1991" };

  { key     = "Leroy1993"
  ; authors = "Xavier Leroy"
  ; title   = "Polymorphism by Name for References and Continuations"
  ; notes   = "Proceedings of POPL"
  ; year    = "1993" };

  { key     = "Miquel2001"
  ; authors = "Alexandre Miquel"
  ; title   = "Le Calcul des Constructions Implicites : Syntaxe et Sémantique"
  ; notes   = "Thèse de l'Université Paris VII"
  ; year    = "2001" };

  { key     = "Miquel2011"
  ; authors = "Alexandre Miquel"
  ; title   = "Existential witness extraction in classical realizability and"
              ++  " via a negative translation"
  ; notes   = "Logical Methods in Computer Science"
  ; year    = "2011" };

  { key     = "Xi2004"
  ; authors = "Hongwei Xi"
  ; title   = "Applied Type System (extended abstract)"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "2004" };

  { key     = "Xi1999"
  ; authors = "Hongwei Xi and Frank Pfenning"
  ; title   = "Dependent Types in Practical Programming"
  ; notes   = "Proceedings of POPL"
  ; year    = "1999" };

  { key     = "Licata2009"
  ; authors = "Daniel Licata and Robert Harper"
  ; title   = "Positively Dependent Types"
  ; notes   = "Proceedings of PLPV"
  ; year    = "2009" };

  { key     = "Swamy2011"
  ; authors = "Nikhil Swamy, Juan Chen, C. Fournet, P.-Y. Strub, "
              ++ "K. Bhargavan and J. Yang"
  ; title   = "Secure Distributed Programming with Value-Dependent Types"
  ; notes   = "Proceedings of ICFP"
  ; year    = "2011" };

  { key     = "Munch2009"
  ; authors = "Guillaume Munch-Maccagnoni"
  ; title   = "Focalisation and Classical Realisability"
  ; notes   = "Proceedings of CSL"
  ; year    = "2009" };

  { key     = "Casinghino2014"
  ; authors = "Chris Casinghino, Vilhelm Sjöberg and Stephanie Weirich"
  ; title   = "Combining Proofs and Programs in a Dependently Typed Language"
  ; notes   = "Proceedings of PLPV"
  ; year    = "2014" };

  { key     = "Jia2008"
  ; authors = "L. Jia, J. Vaughan, Karl Mazurak, J. Zhao, L. Zarko, "
              ++ "J. Schorr and S. Zdancewic"
  ; title   = "AURA: a Programming Language for Authorization and Audit"
  ; notes   = "Proceedings of ICFP"
  ; year    = "2008" };

  { key     = "Howe1989"
  ; authors = "Douglas J. Howe"
  ; title   = "Equality in Lazy Computation Systems"
  ; notes   = "Proceedings of LICS"
  ; year    = "1989" };

  { key     = "Constable1986"
  ; authors = "R. L. Constable, S. F. Allen, H. M. Bromley, W. R. Cleaveland,"
              ++ " et al."
              (* J. F Cremer, R. W. Harper, D. J. Howe, T. B. Knoblock,
                 N. P. Mendler, P. Panangaden, J. T Sasaki, S. F. Smith *)
  ; title   = "Implementing Mathematics with the Nuprl proof development "
              ++ "system"
  ; notes   = "Prentice Hall"
  ; year    = "1986" };

  { key     = "Coquand1988"
  ; authors = "Thierry Coquand and Gérard Huet"
  ; title   = "The Calculus of Constructions"
  ; notes   = "Information and Computation (Volume 76, Issue 2-3)"
  ; year    = "1988" };

  { key     = "Martin-Löf1982"
  ; authors = "Per Martin-Löf"
  ; title   = "Constructive Mathematics and Computer Programming"
  ; notes   = "Studies in Logic the Foundations of Mathematics (Volume 104)"
  ; year    = "1982" };

  { key     = "CoqTeam2004"
  ; authors = "The Coq Development Team"
  ; title   = "The Coq Proof Assistant Reference Manual"
  ; notes   = "LogiCal Project (http://coq.inria.fr)"
  ; year    = "2004" };

  { key     = "Norell2008"
  ; authors = "Ulf Norell"
  ; title   = "Dependently Typed Programming in Agda"
  ; notes   = "Lecture notes from the Summer School in Advanced FP"
  ; year    = "2008" };

  { key     = "Owre1996"
  ; authors = "Sam Owre, Sreeranga Rajan, John Rushby, Natarajan Shankar, "
              ++ "M. Srivas"
  ; title   = "PVS: Combining Specification, Proof Checking and Model "
              ++ "Checking"
  ; notes   = "Lecture Notes In Computer Science"
  ; year    = "1996" };

  { key     = "Lepigre2016"
  ; authors = "Rodolphe Lepigre"
  ; title   = "A Classical Realizability Model for a Semantical Value "
              ++ "Restriction"
  ; notes   = "Proceedings of ESOP, LNCS Volume 9632"
  ; year    = "2016" };

  { key     = "Church1936"
  ; authors = "Alonzo Church and John Barkley Rosser Sr."
  ; title   = "Some properties of conversion"
  ; notes   = "Transactions of the American Mathematical Society, "
              ++ "Volume 36, Number 3, pages 472–482"
  ; year    = "1936" };

  { key     = "Huet1997"
  ; authors = "Gérard Huet"
  ; title   = "The Zipper"
  ; notes   = "Journal of Functional Programming, Volume 7, Number 5, "
              ++ "pages 549-554"
  ; year    = "1997" };

  ]

let init () =
  insert_many biblio (create "biblio")

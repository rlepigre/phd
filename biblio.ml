open Minibib

let (++) s1 s2 = String.concat "" [s1;s2]

let biblio =
  [

  { key     = "Church1941"
  ; authors = "Alonzo Church"
  ; title   = "The Calculi of Lambda-Conversion"
  ; notes   = "Princeton University Press"
  ; year    = "1941"
  ; url     = "" }; (* FIXME *)

  { key     = "Barendregt1981"
  ; authors = "Hendrik Pieter Barendregt"
  ; title   = "The Lambda Calculus - Its Syntax and Semantics"
  ; notes   = "North-Holland"
  ; year    = "1981"
  ; url     = "" }; (* FIXME *)

  { key     = "Turing1937"
  ; authors = "Alan Turing"
  ; title   = "Computability and Lambda-Definability"
  ; notes   = "Journal of Symbolic Logic"
  ; year    = "1937"
  ; url     = "" }; (* FIXME *)

  { key     = "Krivine1990"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Lambda-calcul, types et modèles"
  ; notes   = "Masson"
  ; year    = "1990"
  ; url     = "" }; (* FIXME *)

  { key     = "Krivine2009"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Realizability in Classical Logic"
  ; notes   = "Panoramas et Synthèses (Volume 27)"
  ; year    = "2009"
  ; url     = "" }; (* FIXME *)

  { key     = "Krivine2007"
  ; authors = "Jean-louis Krivine"
  ; title   = "A call-by-name lambda-calculus machine"
  ; notes   = "Higher Order and Symbolic Computation"
  ; year    = "2007"
  ; url     = "" }; (* FIXME *)

  { key     = "Parigot1992"
  ; authors = "Michel Parigot"
  ; title   = "λμ-calculus: an algorithic interpretation of classical "
              ++ "natural deduction"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "1992"
  ; url     = "" }; (* FIXME *)

  { key     = "Garrigue1998"
  ; authors = "Jacques Garrigue"
  ; title   = "Programming with Polymorphic Variants"
  ; notes   = "In the proceedings of the ML Workshop"
  ; year    = "1998"
  ; url     = "" }; (* FIXME *)

  { key     = "MacQueen1984"
  ; authors = "David MacQueen"
  ; title   = "Modules for Standard ML"
  ; notes   = "In the proceedings of LFP"
  ; year    = "1984"
  ; url     = "" }; (* FIXME *)

  { key     = "Girard1972"
  ; authors = "Jean-Yves Girard"
  ; title   = "Interprétation fonctionnelle et élimination des coupures "
              ++ "de l'arithmétique d'ordre supérieur"
  ; notes   = "Thèse de l'Université Paris VII"
  ; year    = "1972"
  ; url     = "" }; (* FIXME *)

  { key     = "Girard1989"
  ; authors = "Jean-Yves Girard, Paul Taylor and Yves Lafont"
  ; title   = "Proofs and Types"
  ; notes   = "Cambridge University Press"
  ; year    = "1989"
  ; url     = "" }; (* FIXME *)

  { key     = "Reynolds1974"
  ; authors = "John C. Reynolds"
  ; title   = "Towards a Theory of Type Structure"
  ; notes   = "In the proceedings Colloque sur la Programmation"
  ; year    = "1974"
  ; url     = "" }; (* FIXME *)

  { key     = "Griffin1990"
  ; authors = "Timothy G. Griffin"
  ; title   = "A Formulae-as-Types Notion of Control"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1990"
  ; url     = "" }; (* FIXME *)

  { key     = "Wright1994"
  ; authors = "Andrew K. Wright and Matthias Felleisen"
  ; title   = "A Syntactic Approach to Type Soundness"
  ; notes   = "Information and Computation (Volume 15, Issue 1)"
  ; year    = "1994"
  ; url     = "" }; (* FIXME *)

  { key     = "Wright1995"
  ; authors = "Andrew K. Wright"
  ; title   = "Simple Imperative Polymorphism"
  ; notes   = "Lisp and Symbolic Computation (Volume 8, Number 4)"
  ; year    = "1995"
  ; url     = "" }; (* FIXME *)

  { key     = "Garrigue2004"
  ; authors = "Jacques Garrigue"
  ; title   = "Relaxing the Value Restriction"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "2004"
  ; url     = "" }; (* FIXME *)

  { key     = "Tofte1990"
  ; authors = "Mads Tofte"
  ; title   = "Type Inference for Polymorphic References"
  ; notes   = "Information and Computation (Volume 89, Issue 1)"
  ; year    = "1990"
  ; url     = "" }; (* FIXME *)

  { key     = "Damas1982"
  ; authors = "Luís Damas and Robin Milner"
  ; title   = "Principal Type-Schemes for Functional Programs"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1982"
  ; url     = "" }; (* FIXME *)

  { key     = "Harper1991"
  ; authors = "Robert Harper and Mark Lillibridge"
  ; title   = "ML with callcc is unsound"
  ; notes   = "Message posted to the SML mailing list"
  ; year    = "1991"
  ; url     = "" }; (* FIXME *)

  { key     = "Leroy1991"
  ; authors = "Xavier Leroy and Pierre Weis"
  ; title   = "Polymorphic Type Inference and Assignment"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1991"
  ; url     = "" }; (* FIXME *)

  { key     = "Leroy1993"
  ; authors = "Xavier Leroy"
  ; title   = "Polymorphism by Name for References and Continuations"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1993"
  ; url     = "" }; (* FIXME *)

  { key     = "Miquel2001"
  ; authors = "Alexandre Miquel"
  ; title   = "Le Calcul des Constructions Implicites : Syntaxe et Sémantique"
  ; notes   = "Thèse de l'Université Paris VII"
  ; year    = "2001"
  ; url     = "" }; (* FIXME *)

  { key     = "Miquel2011"
  ; authors = "Alexandre Miquel"
  ; title   = "Existential witness extraction in classical realizability and"
              ++  " via a negative translation"
  ; notes   = "Logical Methods in Computer Science"
  ; year    = "2011"
  ; url     = "" }; (* FIXME *)

  { key     = "Xi2004"
  ; authors = "Hongwei Xi"
  ; title   = "Applied Type System (extended abstract)"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "2004"
  ; url     = "" }; (* FIXME *)

  { key     = "Xi1999"
  ; authors = "Hongwei Xi and Frank Pfenning"
  ; title   = "Dependent Types in Practical Programming"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1999"
  ; url     = "" }; (* FIXME *)

  { key     = "Licata2009"
  ; authors = "Daniel Licata and Robert Harper"
  ; title   = "Positively Dependent Types"
  ; notes   = "In the proceedings of PLPV"
  ; year    = "2009"
  ; url     = "" }; (* FIXME *)

  { key     = "Swamy2011"
  ; authors = "Nikhil Swamy, Juan Chen, C. Fournet, P.-Y. Strub, "
              ++ "K. Bhargavan and J. Yang"
  ; title   = "Secure Distributed Programming with Value-Dependent Types"
  ; notes   = "In the proceedings of ICFP"
  ; year    = "2011"
  ; url     = "" }; (* FIXME *)

  { key     = "Munch2009"
  ; authors = "Guillaume Munch-Maccagnoni"
  ; title   = "Focalisation and Classical Realisability"
  ; notes   = "In the proceedings of CSL"
  ; year    = "2009"
  ; url     = "" }; (* FIXME *)

  { key     = "Casinghino2014"
  ; authors = "Chris Casinghino, Vilhelm Sjöberg and Stephanie Weirich"
  ; title   = "Combining Proofs and Programs in a Dependently Typed Language"
  ; notes   = "In the proceedings of PLPV"
  ; year    = "2014"
  ; url     = "" }; (* FIXME *)

  { key     = "Jia2008"
  ; authors = "L. Jia, J. Vaughan, Karl Mazurak, J. Zhao, L. Zarko, "
              ++ "J. Schorr and S. Zdancewic"
  ; title   = "AURA: a Programming Language for Authorization and Audit"
  ; notes   = "In the proceedings of ICFP"
  ; year    = "2008"
  ; url     = "" }; (* FIXME *)

  { key     = "Howe1989"
  ; authors = "Douglas J. Howe"
  ; title   = "Equality in Lazy Computation Systems"
  ; notes   = "In the proceedings of LICS"
  ; year    = "1989"
  ; url     = "" }; (* FIXME *)

  { key     = "Constable1986"
  ; authors = "R. L. Constable, S. F. Allen, H. M. Bromley, W. R. Cleaveland,"
              ++ " et al."
              (* J. F Cremer, R. W. Harper, D. J. Howe, T. B. Knoblock,
                 N. P. Mendler, P. Panangaden, J. T Sasaki, S. F. Smith *)
  ; title   = "Implementing Mathematics with the Nuprl proof development "
              ++ "system"
  ; notes   = "Prentice Hall"
  ; year    = "1986"
  ; url     = "" }; (* FIXME *)

  { key     = "Coquand1988"
  ; authors = "Thierry Coquand and Gérard Huet"
  ; title   = "The Calculus of Constructions"
  ; notes   = "Information and Computation (Volume 76, Issue 2-3)"
  ; year    = "1988"
  ; url     = "" }; (* FIXME *)

  { key     = "Martin-Löf1982"
  ; authors = "Per Martin-Löf"
  ; title   = "Constructive Mathematics and Computer Programming"
  ; notes   = "Studies in Logic the Foundations of Mathematics (Volume 104)"
  ; year    = "1982"
  ; url     = "" }; (* FIXME *)

  { key     = "CoqTeam2004"
  ; authors = "The Coq Development Team"
  ; title   = "The Coq Proof Assistant Reference Manual"
  ; notes   = "LogiCal Project (http://coq.inria.fr)"
  ; year    = "2004"
  ; url     = "http://coq.inria.fr" };

  { key     = "Norell2008"
  ; authors = "Ulf Norell"
  ; title   = "Dependently Typed Programming in Agda"
  ; notes   = "Lecture notes from the Summer School in Advanced FP"
  ; year    = "2008"
  ; url     = "" }; (* FIXME *)

  { key     = "Owre1996"
  ; authors = "Sam Owre, Sreeranga Rajan, John Rushby, Natarajan Shankar, "
              ++ "M. Srivas"
  ; title   = "PVS: Combining Specification, Proof Checking and Model "
              ++ "Checking"
  ; notes   = "Lecture Notes In Computer Science"
  ; year    = "1996"
  ; url     = "" }; (* FIXME *)

  { key     = "Lepigre2016"
  ; authors = "Rodolphe Lepigre"
  ; title   = "A Classical Realizability Model for a Semantical Value "
              ++ "Restriction"
  ; notes   = "In the proceedings of ESOP, Lecture Notes in Computer Science"
              ++ ", Volume 9632"
  ; year    = "2016"
  ; url     = "http://dx.doi.org/10.1007/978-3-662-49498-1_19" };

  { key     = "Church1936"
  ; authors = "Alonzo Church and John Barkley Rosser Sr."
  ; title   = "Some properties of conversion"
  ; notes   = "Transactions of the American Mathematical Society, "
              ++ "Volume 36, Number 3, pages 472–482"
  ; year    = "1936"
  ; url     = "" }; (* FIXME *)

  { key     = "Huet1997"
  ; authors = "Gérard Huet"
  ; title   = "The Zipper"
  ; notes   = "Journal of Functional Programming, Volume 7, Number 5, "
              ++ "pages 549-554"
  ; year    = "1997"
  ; url     = "" }; (* FIXME *)

(* http://www.cs.cmu.edu/~joshuad/papers/letnormal/Dunfield10_letnormal.pdf *)
  { key     = "Moggi1989"
  ; authors = "Eugenio Moggi"
  ; title   = "Computational Lambda-Calculus and Monads"
  ; notes   = "In the proceedings of LICS"
  ; year    = "1989"
  ; url     = "" }; (* FIXME *)

  { key     = "Flanagan1993"
  ; authors = "Cormac Flanagan, Amr Sabry, Bruce Duba and Matthias Felleisen"
  ; title   = "The Essence of Compiling with Continuations"
  ; notes   = "In the proceedings of PLDI"
  ; year    = "1993"
  ; url     = "" }; (* FIXME *)

  { key     = "Tarditi1996"
  ; authors = "David Tarditi, Gregory Morrisett, Perry Cheng and "
              ++ "Christopher Stone, Robert Harper and Peter Lee"
  ; title   = "TIL: A Type-Directed Optimizing Compiler for ML"
  ; notes   = "In the proceedings of PLDI"
  ; year    = "1996"
  ; url     = "" }; (* FIXME *)

  { key     = "Chlipala2005"
  ; authors = "Adam Chlipala, Leaf Petersen and Robert Harper"
  ; title   = "Strict bidirectional type checking"
  ; notes   = "In the proceedings of TLDI"
  ; year    = "2005"
  ; url     = "" }; (* FIXME *)

  { key     = "Mitchell1996"
  ; authors = "John C. Mitchell"
  ; title   = "Foundations for Programming Languages"
  ; notes   = "MIT Press"
  ; year    = "1996"
  ; url     = "" }; (* FIXME *)

  { key     = "Mitchell1991"
  ; authors = "L. Cardelli, S. Martini, J. C. Mitchell and A. Scedrov"
  ; title   = "An Extension of System F with Subtyping"
  ; notes   = "In the proceedings of TACS"
  ; year    = "1991"
  ; url     = "" }; (* FIXME *)

  { key     = "Castagna2016"
  ; authors = "Giuseppe Castagna, Tommaso Petrucciani and Kim Nguyen"
  ; title   = "Set-Theoretic Types for Polymorphic Variants"
  ; notes   = "http://arxiv.org/abs/1606.01106"
  ; year    = "2016"
  ; url     = "" }; (* FIXME *)

  { key     = "Lepigre2017"
  ; authors = "Rodolphe Lepigre and Christophe Raffalli"
  ; title   = "Practical Subtyping for System F with Sized (Co-)Induction"
  ; notes   = "Unpublished"
  ; year    = "2017"
  ; url     = "" }; (* FIXME *)

  { key     = "Hilbert1934"
  ; authors = "David Hilbert and Paul Bernays"
  ; title   = "Grundlagen der Mathematik I"
  ; notes   = "Grundlehren der mathematischen Wissenschaften"
  ; year    = "1934 / 1939"
  ; url     = "http://www.isbnsearch.org/isbn/978-3-540-04134-4" };

  { key     = "Hughes1996"
  ; authors = "John Hughes, Lars Pareto and Amr Sabry"
  ; title   = "Proving the Correctness of Reactive Systems Using Sized Types"
  ; notes   = "In the proceedings of POPL"
  ; year    = "1996"
  ; url     = "http://dx.doi.org/10.1145/237721.240882" };

  { key     = "Abel2008"
  ; authors = "Andreas Abel"
  ; title   = "Semi-Continuous Sized Types and Termination"
  ; notes   = "Logical Methods in Computer Science, Volume 4, Number 2"
  ; year    = "2008"
  ; url     = "http://dx.doi.org/10.2168/LMCS-4(2:3)2008" };

  ]

let init () =
  insert_many biblio (create "biblio")

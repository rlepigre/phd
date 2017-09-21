open Minibib

let (++) s1 s2 = String.concat "" [s1;s2]

let biblio =
  [

  { key     = "Church1941"
  ; authors = "Alonzo Church"
  ; title   = "The Calculi of Lambda-Conversion"
  ; notes   = "Princeton University Press"
  ; year    = "1941"
  ; url     = "" }; (* FIXME introuvable *)

  { key     = "Barendregt1981"
  ; authors = "Hendrik Pieter Barendregt"
  ; title   = "The Lambda Calculus - Its Syntax and Semantics"
  ; notes   = "North-Holland"
  ; year    = "1981"
  ; url     = "" }; (* FIXME introuvable *)

  { key     = "Turing1937"
  ; authors = "Alan Turing"
  ; title   = "Computability and Lambda-Definability"
  ; notes   = "Journal of Symbolic Logic"
  ; year    = "1937"
  ; url     = "https://doi.org/10.2307/2268280" };

  { key     = "Krivine1990"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Lambda-calcul, types et modèles"
  ; notes   = "Masson"
  ; year    = "1990"
  ; url     = "https://www.irif.fr/~krivine/articles/Lambda.pdf" };

  { key     = "Krivine2009"
  ; authors = "Jean-Louis Krivine"
  ; title   = "Realizability in Classical Logic"
  ; notes   = "Panoramas et Synthèses (Volume 27)"
  ; year    = "2009"
  ; url     = "https://hal.archives-ouvertes.fr/hal-00154500" };

  { key     = "Krivine2007"
  ; authors = "Jean-louis Krivine"
  ; title   = "A call-by-name lambda-calculus machine"
  ; notes   = "Higher Order and Symbolic Computation"
  ; year    = "2007"
  ; url     = "https://doi.org/10.1007/s10990-007-9018-9" };

  { key     = "Parigot1992"
  ; authors = "Michel Parigot"
  ; title   = "Lambda-Mu-calculus: an algorithmic interpretation of classical"
              ++ " natural deduction"
  ; notes   = "in the proceedings of LPAR"
  ; year    = "1992"
  ; url     = "https://doi.org/10.1007/BFb0013061" };

  { key     = "Garrigue1998"
  ; authors = "Jacques Garrigue"
  ; title   = "Programming with Polymorphic Variants"
  ; notes   = "in the proceedings of the ML Workshop"
  ; year    = "1998"
  ; url     = "https://caml.inria.fr/pub/papers/garrigue-polymorphic_"
              ++ "variants-ml98.pdf" };

  { key     = "MacQueen1984"
  ; authors = "David MacQueen"
  ; title   = "Modules for Standard ML"
  ; notes   = "in the proceedings of LFP"
  ; year    = "1984"
  ; url     = "http://dl.acm.org/citation.cfm?id=802036" };

  { key     = "Girard1972"
  ; authors = "Jean-Yves Girard"
  ; title   = "Interprétation fonctionnelle et élimination des coupures "
              ++ "de l'arithmétique d'ordre supérieur"
  ; notes   = "Thèse de l'Université Paris VII"
  ; year    = "1972"
  ; url     = "" }; (* FIXME introuvable *)

  { key     = "Girard1989"
  ; authors = "Jean-Yves Girard, Paul Taylor and Yves Lafont"
  ; title   = "Proofs and Types"
  ; notes   = "Cambridge University Press"
  ; year    = "1989"
  ; url     = "http://www.paultaylor.eu/stable/Proofs+Types.html" };

  { key     = "Reynolds1974"
  ; authors = "John C. Reynolds"
  ; title   = "Towards a Theory of Type Structure"
  ; notes   = "in the proceedings Colloque sur la Programmation"
  ; year    = "1974"
  ; url     = "http://repository.cmu.edu/compsci/1290/" };

  { key     = "Griffin1990"
  ; authors = "Timothy G. Griffin"
  ; title   = "A Formulae-as-Types Notion of Control"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1990"
  ; url     = "http://doi.acm.org/10.1145/96709.96714" };

  { key     = "Wright1994"
  ; authors = "Andrew K. Wright and Matthias Felleisen"
  ; title   = "A Syntactic Approach to Type Soundness"
  ; notes   = "Information and Computation (Volume 15, Issue 1)"
  ; year    = "1994"
  ; url     = "https://doi.org/10.1006/inco.1994.1093" };

  { key     = "Wright1995"
  ; authors = "Andrew K. Wright"
  ; title   = "Simple Imperative Polymorphism"
  ; notes   = "Lisp and Symbolic Computation (Volume 8, Number 4)"
  ; year    = "1995"
  ; url     = "https://link.springer.com/article/10.1007/BF01018828" };

  { key     = "Garrigue2004"
  ; authors = "Jacques Garrigue"
  ; title   = "Relaxing the Value Restriction"
  ; notes   = "in the proceedings of FLOPS"
  ; year    = "2004"
  ; url     = "https://doi.org/10.1007/978-3-540-24754-8_15" };

  { key     = "Tofte1990"
  ; authors = "Mads Tofte"
  ; title   = "Type Inference for Polymorphic References"
  ; notes   = "Information and Computation (Volume 89, Issue 1)"
  ; year    = "1990"
  ; url     = "https://doi.org/10.1016/0890-5401(90)90018-D" };

  { key     = "Damas1982"
  ; authors = "Luís Damas and Robin Milner"
  ; title   = "Principal Type-Schemes for Functional Programs"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1982"
  ; url     = "http://doi.acm.org/10.1145/582153.582176" };

  { key     = "Harper1991"
  ; authors = "Robert Harper and Mark Lillibridge"
  ; title   = "ML with callcc is unsound"
  ; notes   = "Message posted to the SML mailing list"
  ; year    = "1991"
  ; url     = "http://www.seas.upenn.edu/~sweirich/types/archive/1991/msg00034.html" };

  { key     = "Leroy1991"
  ; authors = "Xavier Leroy and Pierre Weis"
  ; title   = "Polymorphic Type Inference and Assignment"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1991"
  ; url     = "http://doi.acm.org/10.1145/99583.99622" };

  { key     = "Leroy1993"
  ; authors = "Xavier Leroy"
  ; title   = "Polymorphism by Name for References and Continuations"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1993"
  ; url     = "http://doi.acm.org/10.1145/158511.158632" };

  { key     = "Miquel2001"
  ; authors = "Alexandre Miquel"
  ; title   = "Le Calcul des Constructions Implicites : Syntaxe et Sémantique"
  ; notes   = "Thèse de l'Université Paris VII"
  ; year    = "2001"
  ; url     = "https://www.fing.edu.uy/~amiquel/publis/these.pdf" };

  { key     = "Miquel2007"
  ; authors = "Alexandre Miquel"
  ; title   = "Classical Program Extraction in the Calculus of Constructions"
  ; notes   = "in the proceedings of CSL"
  ; year    = "2007"
  ; url     = "https://doi.org/10.1007/978-3-540-74915-8_25" };

  { key     = "Miquel2011"
  ; authors = "Alexandre Miquel"
  ; title   = "Existential witness extraction in classical realizability and"
              ++  " via a negative translation"
  ; notes   = "Logical Methods in Computer Science"
  ; year    = "2011"
  ; url     = "https://doi.org/10.2168/LMCS-7(2:2)2011" };

  { key     = "Xi2003"
  ; authors = "Hongwei Xi"
  ; title   = "Applied Type System: Extended Abstract"
  ; notes   = "in the proceedings of TYPES"
  ; year    = "2003"
  ; url     = "https://doi.org/10.1007/978-3-540-24849-1_25" };

  { key     = "Xi1999"
  ; authors = "Hongwei Xi and Frank Pfenning"
  ; title   = "Dependent Types in Practical Programming"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1999"
  ; url     = "http://doi.acm.org/10.1145/292540.292560" };

  { key     = "Licata2009"
  ; authors = "Daniel Licata and Robert Harper"
  ; title   = "Positively Dependent Types"
  ; notes   = "in the proceedings of PLPV"
  ; year    = "2009"
  ; url     = "http://doi.acm.org/10.1145/1481848.1481851" };

  { key     = "Swamy2011"
  ; authors = "Nikhil Swamy, Juan Chen, C. Fournet, P.-Y. Strub, "
              ++ "K. Bhargavan and J. Yang"
  ; title   = "Secure Distributed Programming with Value-Dependent Types"
  ; notes   = "in the proceedings of ICFP"
  ; year    = "2011"
  ; url     = "http://doi.acm.org/10.1145/2034773.2034811" };

  { key     = "Munch2009"
  ; authors = "Guillaume Munch-Maccagnoni"
  ; title   = "Focalisation and Classical Realisability"
  ; notes   = "in the proceedings of CSL"
  ; year    = "2009"
  ; url     = "https://doi.org/10.1007/978-3-642-04027-6_30" };

  { key     = "Casinghino2014"
  ; authors = "Chris Casinghino, Vilhelm Sjöberg and Stephanie Weirich"
  ; title   = "Combining Proofs and Programs in a Dependently Typed Language"
  ; notes   = "in the proceedings of POPL"
  ; year    = "2014"
  ; url     = "http://doi.acm.org/10.1145/2535838.2535883" };

  { key     = "Jia2008"
  ; authors = "L. Jia, J. Vaughan, Karl Mazurak, J. Zhao, L. Zarko, "
              ++ "J. Schorr and S. Zdancewic"
  ; title   = "AURA: a Programming Language for Authorization and Audit"
  ; notes   = "in the proceedings of ICFP"
  ; year    = "2008"
  ; url     = "http://doi.acm.org/10.1145/1411204.1411212" };

  { key     = "Howe1989"
  ; authors = "Douglas J. Howe"
  ; title   = "Equality in Lazy Computation Systems"
  ; notes   = "in the proceedings of LICS"
  ; year    = "1989"
  ; url     = "https://doi.org/10.1109/LICS.1989.39174" };

  { key     = "Constable1986"
  ; authors = "R. L. Constable, S. F. Allen, H. M. Bromley, W. R. Cleaveland,"
              ++ " et al."
              (* J. F Cremer, R. W. Harper, D. J. Howe, T. B. Knoblock,
                 N. P. Mendler, P. Panangaden, J. T Sasaki, S. F. Smith *)
  ; title   = "Implementing Mathematics with the Nuprl proof development "
              ++ "system"
  ; notes   = "Prentice Hall"
  ; year    = "1986"
  ; url     = "http://dl.acm.org/citation.cfm?id=10510" };

  { key     = "Coquand1988"
  ; authors = "Thierry Coquand and Gérard Huet"
  ; title   = "The Calculus of Constructions"
  ; notes   = "Information and Computation (Volume 76, Issue 2-3)"
  ; year    = "1988"
  ; url     = "https://doi.org/10.1016/0890-5401(88)90005-3" };

  { key     = "Martin-Löf1982"
  ; authors = "Per Martin-Löf"
  ; title   = "Constructive Mathematics and Computer Programming"
  ; notes   = "Studies in Logic the Foundations of Mathematics (Volume 104)"
  ; year    = "1982"
  ; url     = "http://rsta.royalsocietypublishing.org/content/312/1522/501" };

  { key     = "CoqTeam2004"
  ; authors = "The Coq Development Team"
  ; title   = "The Coq Proof Assistant Reference Manual"
  ; notes   = "LogiCal Project"
  ; year    = "2004"
  ; url     = "https://coq.inria.fr" };

  { key     = "Norell2008"
  ; authors = "Ulf Norell"
  ; title   = "Dependently Typed Programming in Agda"
  ; notes   = "Lecture notes from the Summer School in Advanced FP"
  ; year    = "2008"
  ; url     = "https://doi.org/10.1007/978-3-642-04652-0_5" };

  { key     = "Owre1996"
  ; authors = "Sam Owre, Sreeranga Rajan, John Rushby, Natarajan Shankar, "
              ++ "M. Srivas"
  ; title   = "PVS: Combining Specification, Proof Checking and Model "
              ++ "Checking"
  ; notes   = "Lecture Notes In Computer Science"
  ; year    = "1996"
  ; url     = "https://doi.org/10.1007/3-540-61474-5_91" };

  { key     = "Lepigre2016"
  ; authors = "Rodolphe Lepigre"
  ; title   = "A Classical Realizability Model for a Semantical Value "
              ++ "Restriction"
  ; notes   = "in the proceedings of ESOP, Lecture Notes in Computer Science"
              ++ ", Volume 9632"
  ; year    = "2016"
  ; url     = "https://dx.doi.org/10.1007/978-3-662-49498-1_19" };

  { key     = "Church1936"
  ; authors = "Alonzo Church and John Barkley Rosser Sr."
  ; title   = "Some properties of conversion"
  ; notes   = "Transactions of the American Mathematical Society, "
              ++ "Volume 36, Number 3, pages 472–482"
  ; year    = "1936"
  ; url     = "http://www.ams.org/journals/tran/1936-039-03/S0002-9947-1936-"
              ++ "1501858-0/S0002-9947-1936-1501858-0.pdf" };

  { key     = "Huet1997"
  ; authors = "Gérard Huet"
  ; title   = "The Zipper"
  ; notes   = "Journal of Functional Programming, Volume 7, Number 5, "
              ++ "pages 549-554"
  ; year    = "1997"
  ; url     = "http://journals.cambridge.org/action/displayAbstract?aid=44121" };

(* https://www.cs.cmu.edu/~joshuad/papers/letnormal/Dunfield10_letnormal.pdf *)
  { key     = "Moggi1989"
  ; authors = "Eugenio Moggi"
  ; title   = "Computational Lambda-Calculus and Monads"
  ; notes   = "in the proceedings of LICS"
  ; year    = "1989"
  ; url     = "https://doi.org/10.1109/LICS.1989.39155" };

  { key     = "Flanagan1993"
  ; authors = "Cormac Flanagan, Amr Sabry, Bruce Duba and Matthias Felleisen"
  ; title   = "The Essence of Compiling with Continuations"
  ; notes   = "in the proceedings of PLDI"
  ; year    = "1993"
  ; url     = "http://doi.acm.org/10.1145/155090.155113" };

  { key     = "Tarditi1996"
  ; authors = "David Tarditi, Gregory Morrisett, Perry Cheng and "
              ++ "Christopher Stone, Robert Harper and Peter Lee"
  ; title   = "TIL: A Type-Directed Optimizing Compiler for ML"
  ; notes   = "in the proceedings of PLDI"
  ; year    = "1996"
  ; url     = "http://doi.acm.org/10.1145/231379.231414" };

  { key     = "Chlipala2005"
  ; authors = "Adam Chlipala, Leaf Petersen and Robert Harper"
  ; title   = "Strict bidirectional type checking"
  ; notes   = "in the proceedings of TLDI"
  ; year    = "2005"
  ; url     = "http://doi.acm.org/10.1145/1040294.1040301" };

  { key     = "Mitchell1996"
  ; authors = "John C. Mitchell"
  ; title   = "Foundations for Programming Languages"
  ; notes   = "MIT Press"
  ; year    = "1996"
  ; url     = "https://mitpress.mit.edu/books/foundations-programming-languages" };

  { key     = "Mitchell1991"
  ; authors = "L. Cardelli, S. Martini, J. C. Mitchell and A. Scedrov"
  ; title   = "An Extension of System F with Subtyping"
  ; notes   = "in the proceedings of TACS"
  ; year    = "1991"
  ; url     = "https://doi.org/10.1007/3-540-54415-1_73" };

  { key     = "Castagna2016"
  ; authors = "Giuseppe Castagna, Tommaso Petrucciani and Kim Nguyen"
  ; title   = "Set-Theoretic Types for Polymorphic Variants"
  ; notes   = "in the proceedings of ICFP"
  ; year    = "2016"
  ; url     = "https://arxiv.org/abs/1606.01106" };

  { key     = "Lepigre2017"
  ; authors = "Rodolphe Lepigre and Christophe Raffalli"
  ; title   = "Practical Subtyping for System F with Sized (Co-)Induction"
  ; notes   = "Submitted"
  ; year    = "2017"
  ; url     = "https://arxiv.org/abs/1604.01990" };

  { key     = "Hilbert1934"
  ; authors = "David Hilbert and Paul Bernays"
  ; title   = "Grundlagen der Mathematik I"
  ; notes   = "Grundlehren der mathematischen Wissenschaften"
  ; year    = "1934 / 1939"
  ; url     = "https://www.isbnsearch.org/isbn/978-3-540-04134-4" };

  { key     = "Hughes1996"
  ; authors = "John Hughes, Lars Pareto and Amr Sabry"
  ; title   = "Proving the Correctness of Reactive Systems Using Sized Types"
  ; notes   = "in the proceedings of POPL"
  ; year    = "1996"
  ; url     = "https://dx.doi.org/10.1145/237721.240882" };

  { key     = "Abel2008"
  ; authors = "Andreas Abel"
  ; title   = "Semi-Continuous Sized Types and Termination"
  ; notes   = "Logical Methods in Computer Science, Volume 4, Number 2"
  ; year    = "2008"
  ; url     = "https://dx.doi.org/10.2168/LMCS-4(2:3)2008" };

  { key     = "Microsoft2012"
  ; authors = "Microsoft"
  ; title   = "TypeScript - Javascript that scales"
  ; notes   = "Open source project"
  ; year    = "2012"
  ; url     = "https://www.typescriptlang.org" };

  { key     = "Facebook2014"
  ; authors = "Facebook Inc."
  ; title   = "Flow - A static type checker for Javascript"
  ; notes   = "Open source project"
  ; year    = "2012"
  ; url     = "https://flowtype.org/" };

  { key     = "Lehtosalo2014"
  ; authors = "Jukka Lehtosalo"
  ; title   = "mypy - Optional static typing for Python"
  ; notes   = "Open source project"
  ; year    = "2014"
  ; url     = "https://mypy-lang.org" };

  { key     = "Wadler2003"
  ; authors = "Philip Wadler"
  ; title   = "Call-by-value is dual to call-by-name"
  ; notes   = "SIGPLAN Notices 38(9), 189-201"
  ; year    = "2003"
  ; url     = "https://doi.acm.org/10.1145/944746.944723" };

  { key     = "Manoury1992"
  ; authors = "Pascal Manoury, Michel Parigot and Marianne Simonot"
  ; title   = "ProPre A Programming Language with Proofs"
  ; notes   = "Lecture Notes in Computer Science, Volume 624"
  ; year    = "1992"
  ; url     = "https://dx.doi.org/10.1007/BFb0013095" };

  { key     = "Chargueraud2010"
  ; authors = "Arthur Charguéraud"
  ; title   = "Program verification through characteristic formulae"
  ; notes   = "in the proceedings of ICFP"
  ; year    = "2010"
  ; url     = "https://doi.acm.org/10.1145/1863543.1863590" };

  { key     = "Chargueraud2011"
  ; authors = "Arthur Charguéraud"
  ; title   = "Characteristic formulae for the verification of imperative programs"
  ; notes   = "in the proceedings of ICFP"
  ; year    = "2011"
  ; url     = "https://doi.acm.org/10.1145/2034773.2034828" };

  { key     = "Baro2003"
  ; authors = "Sylvain Baro"
  ; title   = "Conception et implémentation d'un système d'aide à la "
              ++ "spécification et à la preuve de programmes ML"
  ; notes   = "Thèse de l'Université Paris Diderot, France"
  ; year    = "2003"
  ; url     = "https://tel.archives-ouvertes.fr/tel-00008416" };

  { key     = "Régis-Gianas2007"
  ; authors = "Yann Régis-Gianas"
  ; title   = "Des types aux assertions logiques : preuve automatique ou "
              ++ "assistée de propriétés sur les programmes fonctionnels"
  ; notes   = "Thèse de l'Université Paris Diderot, France"
  ; year    = "2007"
  ; url     = "https://tel.archives-ouvertes.fr/tel-01238703" };

  { key     = "Filliâtre2013"
  ; authors = "Jean-Christophe Filliâtre and Andrei Paskevich"
  ; title   = "Why3 - Where Programs Meet Provers"
  ; notes   = "in the proceedings of ESOP, Lecture Notes in Computer Science"
              ++ ", Volume 7792"
  ; year    = "2013"
  ; url     = "https://dx.doi.org/10.1007/978-3-642-37036-6_8" };

  { key     = "Tiuryn1996"
  ; authors = "Jerzy Tiuryn and Pawel Urzyczyn"
  ; title   = "The Subtyping Problem for Second-Order Types is Undecidable"
  ; notes   = "Proceedings of LICS"
  ; year    = "1996"
  ; url     = "https://dx.doi.org/10.1109/LICS.1996.561306" };

  { key     = "Tiuryn2002"
  ; authors = "Jerzy Tiuryn and Pawel Urzyczyn"
  ; title   = "The Subtyping Problem for Second-Order Types is Undecidable"
  ; notes   = "Information and Computation, Volume 179"
  ; year    = "2002"
  ; url     = "https://dx.doi.org/10.1006/inco.2001.2950" };

  { key     = "Wells1994"
  ; authors = "Joe B. Wells"
  ; title   = "Typability and Type-Checking in the Second-Order "
              ++ "lambda-Calculus are Equivalent and Undecidable"
  ; notes   = "Proceedings of LICS"
  ; year    = "1994"
  ; url     = "https://dx.doi.org/10.1006/inco.2001.2950" };

  { key     = "Wells1999"
  ; authors = "Joe B. Wells"
  ; title   = "Typability and type checking in System F are equivalent "
              ++ "and undecidable"
  ; notes   = "Annals of Pure and Applied Logic, Volume 98"
  ; year    = "1999"
  ; url     = "https://dx.doi.org/10.1016/S0168-0072(98)00047-5" };

  { key     = "Raffalli1998"
  ; authors = "Christophe Raffalli"
  ; title   = "System F-eta"
  ; notes   = "Unpublished"
  ; year    = "1998"
  ; url     = "https://www.lama.univ-savoie.fr/~raffalli/FTP/Papers/Feta-partial.pdf" };

  { key     = "Raffalli1999"
  ; authors = "Christophe Raffalli"
  ; title   = "An optimized complete semi-algorithm for system F-eta"
  ; notes   = "Unpublished"
  ; year    = "1999"
  ; url     = "https://www.lama.univ-savoie.fr/~raffalli/FTP/Papers/Feta-total.pdf" };

  { key     = "de Groote1994"
  ; authors = "Philippe de Groote"
  ; title   = "On the Relation between the Lambda-Mu-Calculus and the "
              ++ "Syntactic Theory of Sequential Control"
  ; notes   = "Lecture Notes in Computer Science"
  ; year    = "1994"
  ; url     = "https://doi.org/10.1007/3-540-58216-9_27" };

  ]

let init () =
  insert_many biblio (create "biblio")

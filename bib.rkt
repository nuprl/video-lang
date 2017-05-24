#lang at-exp racket

(require scriblib/autobib
         scribble/base)

(provide (all-defined-out))

(define-cite cite citet gen-bib
  #:cite-author cite-author
  #:cite-year cite-year)

(define short? #f)
(define-syntax define/short
  (syntax-rules ()
    [(_ i e e*) (define i (if short? e e*))]
    [(_ i e) (define i e)]))

(define IEEE "IEEE ")
(define ACM "ACM ")
(define International "Intl. ")
(define Conference "Conf. ")
(define Workshop "Wksp. ")
(define Journal "J. ")
(define Symposium "Sym. ")
(define Transactions "Trans. ")
(set! Transactions "")
(set! Conference "")

(define/short lfp "LFP" "LISP and Functional Programming")
(define/short popl "POPL" (string-append ACM Symposium "Principles of Programming Languages"))

(define kffd:hygiene
  (make-bib #:title "Hygienic macro expansion"
            #:author (authors "Eugene Kohlbecker"
                              "Daniel P. Friedman"
                              "Matthias Felleisen"
                              "Bruce Duba")
            #:is-book? #f
            #:location (proceedings-location lfp)
            #:date "1986"))

(define kw:mbe
  (make-bib #:title "Macro-by-example: deriving syntactic transformations from their specifications"
            #:author (authors "Eugene Kohlbecker" "Mitchell Wand")
            #:location (proceedings-location popl #:pages '(77 84))
            #:date "1987"))


(define bo:ffi
  (make-bib
    #:author (authors "Eli Barzilay" "Dimitry Orlovsky")
    #:title "Foreign Interface for PLT Scheme"
    #:location (proceedings-location "Workshop on Scheme and Funcational Programming")
    #:date 2004))

(define culpepper-scp
  (make-bib #:title "Debugging hygienic macros"
            #:author (authors "Ryan Culpepper" "Matthias Felleisen")
            #:location (journal-location "Science of Computer Programming"
                                         #:pages '(496 515)
                                         #:number "7"
                                         #:volume "75")
            #:date "2010"))

(define bc:lazy
  (make-bib
    #:author (authors "Eli Barzilay" "John Clements")
    #:title "Laziness without all the hard work"
    #:location (proceedings-location "Workshop on Funcational and Declarative Programming in Education" 
		 #:pages '(9 13))
    #:date 2005))

(define SK-PhD
  (make-bib
   #:title "Linguistic Reuse"
   #:author "Shriram Krishnamurthi"
   #:is-book? #t
   #:location (dissertation-location #:institution "Rice University")
   #:date 2001))

(define fowler
  (make-bib #:title "Domain-specific Languages"
            #:author (authors "Martin Fowler" "Rebecca Parsons")
            #:date "2010"
            #:location (book-location #:publisher "Addison-Wesley")))

(define manifesto
  (make-bib
   #:title "The Racket Manifesto"
   #:author (authors "Matthias Felleisen"
		      "Robert Bruce Findler"
		      "Matthew Flatt"
		      "Shriram Krishnamurthi"
		      "Eli Barzilay"
		      "Jay McCarthy"
		      "Sam Tobin-Hochstadt")
   #:date "2015"
   #:location (proceedings-location "Summit on Advances in Programming Languages"
                                    #:pages '(113 128))))

(define plt-tr1
  (make-bib #:title    "Reference: Racket"
            #:author   (authors "Matthew Flatt" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-1")
            #:url      "https://racket-lang.org/tr1/"))

(define plt-tr2
  (make-bib #:title    "DrRacket: Programming Environment"
            #:author   (authors "Robert Bruce Findler" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-2")
            #:url      "https://racket-lang.org/tr2/"))

(define plt-tr3
  (make-bib #:title    "GUI: Racket Graphics Toolkit"
            #:author   (authors "Matthew Flatt" "Robert Bruce Findler" "John Clements")
            #:date     "2010"
            #:location (techrpt-location #:institution "PLT Design Inc."
                                         #:number "PLT-TR-2010-3")
            #:url      "https://racket-lang.org/tr3/"))

(define essential-blender
  (make-bib #:title "The Essential Blender: Guide to 3D Creation with the Open Source Suite Blender "
            #:author (authors "Ton Roosendaal" "Roland Hess")
            #:date "2007"
            #:location (book-location #:publisher "No Starch Press")))

(define adobe-premiere
  (make-bib #:title "Adobe Premiere Pro CC Classroom in a Book"
            #:author (authors "Maxim Jago" (org-author-name "Adobe Creative Team"))
            #:date "2017"
            #:location (book-location #:publisher "Adobe Press")))

(define slideshow-jfp
  (make-bib #:title "Slideshow: Functional Presentations"
            #:author (authors "Matthew Flatt" "Robby Findler")
            #:date "2006"
            #:location (journal-location "Journal of Functional Programming"
                                         #:pages '(593 619)
                                         #:volume 16)))

(define scribble-icfp
  (make-bib #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
            #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
            #:date "2009"
            #:location (proceedings-location "International Conference on Functional Programming"
                                             #:pages '(109 120)
                                             #:volume 14)))

(define applescript-hopl
  (make-bib #:title "Applescript"
            #:author (authors "William R. Cook")
            #:date "2007"
            #:location (proceedings-location "History of Programming Languages"
                                             #:pages '("1-1" "1-21")
                                             #:volume 3)))

(define technique-of-video-editing
  (make-bib #:title "The Technique of Film and Video Editing: History, Theory, and Practice"
            #:author (authors "Ken Dancyger")
            #:date "2010"
            #:location (book-location #:edition "fifth"
                                      #:publisher "Focal Press")))

(define xslt-tr
  (make-bib #:title "XSL Transformations"
            #:author (authors "James Clark")
            #:date "1999"
            #:location (techrpt-location #:institution "World Wide Web Consortium (W3C)"
                                         #:number "1.0")
            #:url "https://www.w3.org/TR/xslt"))

(define smil-tr
  (make-bib #:title "Synchronized Multimedia Integration Language"
            #:author (authors "Dick Bulterman" "Jack Jansen" "Pablo Cesar" "Sjoerd Mullender"
                              "Eric Hyche" "Marisa DeMeglio" "Julien Quint" "Hiroshi Kawamura"
                              "Daniel Weck" "Xabiel García Pañeda" "David Melendi" "Samuel Cruz-Lara"
                              "Marcin Hanclik" "Daniel F. Zucker" "Thierry Michel")
            #:date "2008"
            #:location (techrpt-location #:institution "World Wide Web Consortium (W3C)"
                                         #:number "3.0")
            #:url "https://www.w3.org/TR/2008/REC-SMIL3-20081201/"))

(define macros-icfp
  (make-bib #:title "Composable and Compilable Macros, You Want It when?"
            #:author (authors "Matthew Flatt")
            #:date "2002"
            #:location (proceedings-location "International Conference on Functional Programming"
                                             #:pages '(72 83)
                                             #:volume 7)))

(define lal-pldi
  (make-bib #:title "Languages As Libraries"
            #:author (authors "Sam Tobin-Hochstadt"
                              "Vincent St-Amour"
                              "Ryan Culpepper"
                              "Matthew Flatt"
                              "Matthias Felleisen")
            #:date "2011"
            #:location (proceedings-location "Programming Languages Design and Implementation"
                                             #:pages '(132 141)
                                             #:volume 32)))

(define contracts-icfp
  (make-bib #:title "Contracts for Higher-order Functions"
            #:author (authors "Robert Bruce Findler"
                              "Matthias Felleisen")
            #:date "2002"
            #:location (proceedings-location "International Conference on Functional Programming"
                                             #:pages '(48 59)
                                             #:volume 7)))

(define ats-pldi
  (make-bib
   #:title "Eliminating Array Bound Checking Through Dependent Types"
   #:author (authors "Hongwei Xi" "Frank Pfenning")
   #:location (proceedings-location "Programming Languages Design and Implementation" #:pages '(249 257))
   #:date 1998))

(define tsam-popl
  (make-bib
   #:title "Type Systems as Macros"
   #:author (authors "Stephen Chang" "Alex Knauth" "Ben Greenman")
   #:location (proceedings-location "Principles of Programming Languages" #:pages '(694 705))
   #:date 2017))

#;
(define stxparse-icfp
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:location (proceedings-location "International Conference on Functional Programming" #:pages '(235 246))
   #:date 2010))

; other "DSL" stuff
(define gadt-icfp
  (make-bib
   #:title "Simple unification-based type inference for GADTs"
   #:author (authors "Simon Peyton Jones"
                     "Dimitrios Vytiniotis"
                     "Stephanie Weirich"
                     "Geoffrey Washburn")
   #:location (proceedings-location "International Conference on Functional Programming" #:pages '(50 61))
   #:date 2006))
(define deep-shallow-icfp
  (make-bib
   #:title "Folding domain-specific languages: deep and shallow embeddings (functional Pearl)"
   #:author (authors "Jeremy Gibbons" "Nicolas Wu")
   #:location (proceedings-location "International Conference on Functional Programming" #:pages '(339 347))
   #:date 2014))
(define gadt-popl
  (make-bib
   #:title "Guarded recursive datatype constructors"
   #:author (authors "Hongwei Xi" "Chiyan Chen" "Gang Chen")
   #:location (proceedings-location "Principles of Programming Languages" #:pages '(224 235))
   #:date 2003))
(define tagless-jfp
  (make-bib
   #:title "Finally Tagless, Partially Evaluated"
   #:author (authors "Jacques Carette" "Oleg Kiselyov" "Chung-chieh Shan")
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(509 543)
                                #:volume 19
                                #:number 5)
   #:date 2009))
(define hudak-dsl
  (make-bib
   #:title "Building Domain-Specific Embedded Languages"
   #:author "Paul Hudak"
   #:location (journal-location "ACM Computing Surveys"
                                #:volume 28
                                #:number "4es")
   #:date 1996))
(define meijer-jfp
  (make-bib
   #:title "Server side web scripting in Haskell"
   #:author "Erik Meijer"
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(1 18)
                                #:volume 10
                                #:number 1)
   #:date 2000))
(define meijer-icfp
  (make-bib
   #:title "Calling hell from heaven and heaven from hell"
   #:author (authors "Sigbjorn Finne" "Daan Leijen" "Erik Meijer" "Simon Peyton Jones")
   #:location (proceedings-location "International Conference on Functional Programming" #:pages '(114 125))
   #:date 1999))
(define haskell-scripting-cufp
  (make-bib
   #:title "Light-weight and type-safe scripting with Haskell"
   #:author "Gabriel Gonzalez"
   #:location (proceedings-location "Commercial Users of Functional Programming Tutorials")
   #:date 2015))

(define fortifying-jfp
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper")
   #:date "2009"
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(439 476)
                                #:volume 22)))

(define drscheme-jfp
  (make-bib
   #:title "DrScheme: A Programming Environment for Scheme"
   #:author (authors "Robert Bruce Findler"
                     "John Clements"
                     "Cormac Flanagan"
                     "Matthew Flatt"
                     "Shriram Krishnamurthi"
                     "Paul Steckler"
                     "Matthias Felleisen")
   #:date "2002"
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(159 182)
                                #:volume 12)))

(define hygenic-lisp
  (make-bib
   #:title "Hygienic Macros Through Explicit Renaming"
   #:author (authors "William Clinger")
   #:date "1991"
   #:location (proceedings-location "Lisp Pointers"
                                    #:pages '(25 28))))

(define closures-lfp
  (make-bib
   #:title "Syntactic Closures"
   #:author (authors "Alan Bawden"
                     "Jonathan Rees")
   #:date "1988"
   #:location (proceedings-location "Lisp and Functional Programming"
                                    #:pages '(86 95))))

(define syntax-lfp
  (make-bib
   #:title "Syntactic Abstraction in Scheme"
   #:author (authors "R. Kent Dybvig"
                     "Robert Hieb"
                     "Carl Bruggeman")
   #:date "1993"
   #:location (journal-location "Lisp and Symbolic Computation"
                                    #:volume 5
                                    #:number 4
                                    #:pages '(295 326))))

(define racket-workbench-challenge
  (make-bib
   #:title "Languages the Racket Way"
   #:author (authors "Daniel Feltey"
                     "Spencer P. Florence"
                     "Tim Knutson"
                     "Vincent St-Amour"
                     "Ryan Culpepper"
                     "Matthew Flatt"
                     "Robert Bruce Findler"
                     "Matthias Felleisen")
   #:date "2016"
   #:location (proceedings-location "Language Workbench Challenge")))

(define gstreamer-man
  (make-bib
   #:title "GStreamer Application Development Manual"
   #:author (authors "Wim Taymans"
                     "Steve Baker"
                     "Andy Wingo"
                     "Rondald S. Bultje"
                     "Kost Stefan")
   #:date "2013"
   #:url "https://gstreamer.freedesktop.org/data/doc/gstreamer/head/manual/manual.pdf"))

(define mlt-url (url "mltframework.org/"))
(define shotcut-url (url "shotcutapp.com/"))
(define openshot-url (url "openshot.org/"))
(define avisynth-url (url "avisynth.nl"))
(define mlt-xml-url (url "mltframework.org/docs/mltxml/"))
(define rsound-url (url "docs.racket-lang.org/rsound/index.html"))
(define racketcon-url (url "con.racket-lang.org/"))

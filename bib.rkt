#lang at-exp racket

(require scriblib/autobib
         scribble/base
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         setup/main-collects)

(provide (all-defined-out))

(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))
(define bib-columns-style (make-style #f autobib-style-extras))

(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define-cite cite citet gen-bib
  #:cite-author cite-author
  #:cite-year cite-year
  #:style (new
           (class object%
             (define/public (bibliography-table-style) bib-single-style)
             (define/public (entry-style) bibentry-style)
             (define/public (disambiguate-date?) #t)
             (define/public (collapse-for-date?) #t)
             (define/public (get-cite-open) "[")
             (define/public (get-cite-close) "]")
             (define/public (get-group-sep) "; ")
             (define/public (get-item-sep) ", ")
             (define/public (render-citation date-cite i)
               (make-element
                (make-style "Thyperref" (list (command-extras (list (make-label i)))))
                (list date-cite)))
             (define (make-label i)
               (string-append "autobiblab:" (number->string i)))
             (define/public (render-author+dates author dates) (list* author " " dates))
             (define/public (bibliography-line i e)
               (list (make-paragraph plain
                                     (make-element "label" #;(make-style "hypertarget"
                                                               (list (command-extras '("hello"))))
                                                   (make-label i)))
                     e))
             (super-new))))

(define short? #f)
(define-syntax define/short
  (syntax-rules ()
    [(_ i e e*) (define i (if short? e e*))]
    [(_ i e) (define i e)]))

;; implementation copied from scriblib/autobib so we can delete "Proc"
(define (to-string v) (format "~a" v))
(define (proceedings-location
         location
         #:pages [pages #f]
         #:series [series #f]
         #:volume [volume #f])
  (let* ([s @elem{In @italic{@elem{@to-string[location]}}}]
         [s (if series
                @elem{@|s|, @to-string[series]}
                s)]
         [s (if volume
                @elem{@|s| volume @to-string[volume]}
                s)]
         [s (if pages
                @elem{@|s|, pp. @(to-string (car pages))--@(to-string (cadr pages))}
                s)])
    s))

(define IEEE "IEEE")
(define ACM "ACM")
(define SIGPLAN "SIGPLAN")
(define International "International")
(define Conference "Conference on")
(define Workshop "Workshop on")
(define Journal "Journal of")
(define Symposium "Symposium on")
(define Transactions "Transactions")
(set! Transactions "")
(set! Symposium "")
(set! Workshop "")
(set! ACM "")
(set! SIGPLAN "")

(define (++ . strs) (string-join strs))

(define plt "PLT Design Inc.")
(define fp "Functional Programming")
(define pl "Programming Languages")
(define comm-acm "Communications of the ACM")
(define scheme-workshop (++ Workshop "Scheme and" fp))

(define/short lfp "LFP" (++ "Lisp and" fp))
(define/short jfp "JFP" (++ Journal fp))
(define/short popl "POPL" (++ ACM Symposium "Principles of" pl))
(define/short oopsla "OOPSLA" "Object Oriented Programming Systems, Languages, and Applications")
(define/short icfp "ICFP" (++ International Conference fp))
(define/short pldi "PLDI" (++ pl "Design and Implementation"))

(define kffd:hygiene
  (make-bib #:title "Hygienic Macro Expansion"
            #:author (authors "Eugene Kohlbecker"
                              "Daniel P. Friedman"
                              "Matthias Felleisen"
                              "Bruce Duba")
            #:is-book? #f
            #:location (proceedings-location lfp)
            #:date "1986"))

(define kw:mbe
  (make-bib #:title "Macro-by-example: Deriving Syntactic Transformations From Their Specifications"
            #:author (authors "Eugene Kohlbecker" "Mitchell Wand")
            #:location (proceedings-location popl #:pages '(77 84))
            #:date "1987"))


(define bo:ffi
  (make-bib
    #:author (authors "Eli Barzilay" "Dimitry Orlovsky")
    #:title "Foreign Interface for PLT Scheme"
    #:location (proceedings-location scheme-workshop #:pages '(63 74))
    #:date 2004))

(define culpepper-scp
  (make-bib #:title "Debugging Hygienic Macros"
            #:author (authors "Ryan Culpepper" "Matthias Felleisen")
            #:location (journal-location "Science of Computer Programming"
                                         #:pages '(496 515)
                                         #:number "7"
                                         #:volume "75")
            #:date "2010"))

(define bc:lazy
  (make-bib
    #:author (authors "Eli Barzilay" "John Clements")
    #:title "Laziness Without All the Hard Work"
    #:location (proceedings-location "Functional and Declarative Programming in Education"
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
            #:is-book? #t
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
            #:location (techrpt-location #:institution plt
                                         #:number "PLT-TR-2010-1")
            #:url      "https://racket-lang.org/tr1/"))

(define plt-tr2
  (make-bib #:title    "DrRacket: Programming Environment"
            #:author   (authors "Robert Bruce Findler" "PLT")
            #:date     "2010"
            #:location (techrpt-location #:institution plt
                                         #:number "PLT-TR-2010-2")
            #:url      "https://racket-lang.org/tr2/"))

(define plt-tr3
  (make-bib #:title    "GUI: Racket Graphics Toolkit"
            #:author   (authors "Matthew Flatt" "Robert Bruce Findler" "John Clements")
            #:date     "2010"
            #:location (techrpt-location #:institution plt
                                         #:number "PLT-TR-2010-3")
            #:url      "https://racket-lang.org/tr3/"))

(define essential-blender
  (make-bib #:title "The Essential Blender: Guide to 3D Creation with the Open Source Suite Blender"
            #:is-book? #t
            #:author (authors "Ton Roosendaal" "Roland Hess")
            #:date "2007"
            #:location (book-location #:publisher "No Starch Press")))

(define adobe-premiere
  (make-bib #:title "Adobe Premiere Pro CC Classroom in a Book"
            #:is-book? #t
            #:author (authors "Maxim Jago" (org-author-name "Adobe Creative Team"))
            #:date "2017"
            #:location (book-location #:publisher "Adobe Press")))

(define slideshow-jfp
  (make-bib #:title "Slideshow: Functional Presentations"
            #:author (authors "Matthew Flatt" "Robby Findler")
            #:date "2006"
            #:location (journal-location jfp #:pages '(583 619)
                                             #:volume 16 #:number "4-5")))

(define scribble-icfp
  (make-bib #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
            #:author (authors "Matthew Flatt"
                              "Eli Barzilay"
                              "Robert Bruce Findler")
            #:date "2009"
            #:location (proceedings-location icfp #:pages '(109 120))))

(define applescript-hopl
  (make-bib #:title "Applescript"
            #:author (authors "William R. Cook")
            #:date "2007"
            #:location (proceedings-location "History of Programming Languages"
                                             #:pages '("1-1" "1-21"))))

(define technique-of-video-editing
  (make-bib #:title "The Technique of Film and Video Editing: History, Theory, and Practice"
            #:is-book? #t
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
  (make-bib #:title "Composable and Compilable Macros, You Want It When?"
            #:author (authors "Matthew Flatt")
            #:date "2002"
            #:location (proceedings-location icfp #:pages '(72 83))))

(define lal-pldi
  (make-bib #:title "Languages As Libraries"
            #:author (authors "Sam Tobin-Hochstadt"
                              "Vincent St-Amour"
                              "Ryan Culpepper"
                              "Matthew Flatt"
                              "Matthias Felleisen")
            #:date "2011"
            #:location (proceedings-location pldi #:pages '(132 141))))

(define contracts-icfp
  (make-bib #:title "Contracts for Higher-order Functions"
            #:author (authors "Robert Bruce Findler"
                              "Matthias Felleisen")
            #:date "2002"
            #:location (proceedings-location icfp #:pages '(48 59))))

(define ats-pldi
  (make-bib
   #:title "Eliminating Array Bound Checking Through Dependent Types"
   #:author (authors "Hongwei Xi" "Frank Pfenning")
   #:location (proceedings-location pldi #:pages '(249 257))
   #:date 1998))

(define tsam-popl
  (make-bib
   #:title "Type Systems as Macros"
   #:author (authors "Stephen Chang" "Alex Knauth" "Ben Greenman")
   #:location (proceedings-location popl #:pages '(694 705))
   #:date 2017))

#;
(define stxparse-icfp
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:location (proceedings-location icfp #:pages '(235 246))
   #:date 2010))

; other "DSL" stuff
(define gadt-icfp
  (make-bib
   #:title "Simple Unification-based Type Inference for GADTs"
   #:author (authors "Simon Peyton Jones"
                     "Dimitrios Vytiniotis"
                     "Stephanie Weirich"
                     "Geoffrey Washburn")
   #:location (proceedings-location icfp #:pages '(50 61))
   #:date 2006))
(define deep-shallow-icfp
  (make-bib
   #:title "Folding Domain-specific Languages: Deep and Ahallow Embeddings (Functional Pearl)"
   #:author (authors "Jeremy Gibbons" "Nicolas Wu")
   #:location (proceedings-location icfp #:pages '(339 347))
   #:date 2014))
(define gadt-popl
  (make-bib
   #:title "Guarded Recursive Datatype Constructors"
   #:author (authors "Hongwei Xi" "Chiyan Chen" "Gang Chen")
   #:location (proceedings-location popl #:pages '(224 235))
   #:date 2003))
(define tagless-jfp
  (make-bib
   #:title "Finally Tagless, Partially Evaluated"
   #:author (authors "Jacques Carette" "Oleg Kiselyov" "Chung-chieh Shan")
   #:location (journal-location jfp #:pages '(509 543)
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
   #:title "Server Side Web Scripting in Haskell"
   #:author "Erik Meijer"
   #:location (journal-location jfp
                                #:pages '(1 18)
                                #:volume 10
                                #:number 1)
   #:date 2000))
(define meijer-icfp
  (make-bib
   #:title "Calling hell from heaven and heaven from hell"
   #:author (authors "Sigbjorn Finne" "Daan Leijen" "Erik Meijer" "Simon Peyton Jones")
   #:location (proceedings-location icfp #:pages '(114 125))
   #:date 1999))
(define haskell-scripting-cufp
  (make-bib
   #:title "Light-weight and Type-safe Scripting with Haskell"
   #:author "Gabriel Gonzalez"
   #:location (proceedings-location (++ "Commercial Users of" fp "Tutorials"))
   #:date 2015))

(define fortifying-jfp
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper")
   #:date "2012"
   #:location (journal-location jfp #:pages '(439 476)
                                    #:volume 22 #:number "4-5")))

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
   #:location (journal-location jfp #:pages '(159 182) #:volume 12 #:number 2)))

(define hygenic-lisp
  (make-bib
   #:title "Hygienic Macros Through Explicit Renaming"
   #:author (authors "William Clinger")
   #:date "1991"
   #:location (journal-location (++ SIGPLAN "Lisp Pointers")
                                #:volume "IV" #:number 4
                                #:pages '(25 28))))

(define closures-lfp
  (make-bib
   #:title "Syntactic Closures"
   #:author (authors "Alan Bawden" "Jonathan Rees")
   #:date "1988"
   #:location (proceedings-location lfp #:pages '(86 95))))

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

;; Language Workbenches

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

(define flatt-acm
  (make-bib
   #:title "Creating Languages in Racket"
   #:author (authors "Matthew Flatt")
   #:date "2012"
   #:location (journal-location comm-acm
                                #:volume 55
                                #:number 1
                                #:pages '(48 56))))

(define language-workbenches-survey
  (make-bib
   #:title "Evaluating and Comparing Language Workbenches: Existing Results and Benchmarks for the Future"
   #:author (authors
             "Sebastian Erdweg"
             "Tijs van der Storm"
             "Markus Völter"
             "Laurence Tratt"
             "Remi Bosman"
             "William R. Cook"
             "Albert Gerritsen"
             "Angelo Hulshout"
             "Steven Kelly"
             "Alex Loh"
             "Gabriël Konat"
             "Pedro J. Molina"
             "Martin Palatnik"
             "Risto Pohjonen"
             "Eugen Schindler"
             "Klemens Schindler"
             "Riccardo Solmi"
             "Vlad Vergu"
             "Eelco Visser"
             "Kevin van der Vlist"
             "Guido Wachsmuth"
             "Jimi van der Woning")
   #:date "2015"
   #:location (journal-location "Computer Languages, Systems and Structures"
                                #:volume 44
                                #:number "Part A"
                                #:pages '(24 47))))

(define spoofax
  (make-bib
   #:title "The Spoofax Language Workbench: Rules for Declarative Specification of Languages and IDEs"
   #:author (authors "Lennart C.L. Kats" "Eelco Visser")
   #:date "2010"
   #:location (proceedings-location oopsla #:pages '(444 463))))

(define sugarj
  (make-bib
   #:title "SugarJ: Library-based Syntactic Language Extensibility"
   #:author (authors 
             "Sebastian Erdweg"
             "Tillmann Rendel"
             "Christian Kästner"
             "Klaus Ostermann")
   #:date "2011"
   #:location (proceedings-location oopsla #:pages '(391 406))))

(define metaedit
  (make-bib
   #:title "MetaEdit+: A Fully Configurable Multi-User and Multi-Tool CASE and CAME Environment"
   #:author (authors "Steven Kelly" "Kalle Lyytinen" "Matti Rossi")
   #:date "1996"
   #:location (proceedings-location "Conference on Advances Information System Engineering" #:pages '(1 21))))

(define lop-ward
  (make-bib
   #:title "Language Oriented Programming"
   #:author (authors "Martin P. Ward")
   #:date "1994"
   #:location (journal-location "Software---Concepts and Tools"
                                #:volume 15 #:pages '(147 161))))

(define lop-dmitriev
  (make-bib
   #:title "Language Oriented Programming: The Next Programming Paradigm"
   #:author (authors "Sergey Dmitriev")
   #:date "2004"
   #:location (journal-location "JetBrains onBoard Electronic Magazine"
                                #:volume 1
                                #:number 1)))
   
(define unix
  (make-bib
   #:title "The Art of UNIX Programming"
   #:author (authors "Eric S. Raymond")
   #:date "2003"
   #:is-book? #t
   #:location (book-location #:edition "first"
                             #:publisher "Addison-Wesley")))
(define little-languages
  (make-bib
   #:title "Little Languages"
   #:author (authors "Jon Bentley")
   #:date "1986"
   #:location (journal-location comm-acm #:volume 29 #:number 8
                                         #:pages '(711 21))))

 
 
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

(define linking-types
  (make-bib
   #:title "Linking Types for Multi-Language Software: Have Your Cake and Eat It Too"
   #:author (authors "Daniel Patterson"
                     "Amal Ahmed")
   #:date "2017"
   #:location (proceedings-location "Summit on Advances in Programming Languages"
                                    #:pages '("12-1" "12-15"))))


(define mlt-url (url "mltframework.org/"))
(define shotcut-url (url "shotcutapp.com/"))
(define openshot-url (url "openshot.org/"))
(define avisynth-url (url "avisynth.nl"))
(define mlt-xml-url (url "mltframework.org/docs/mltxml/"))
(define rsound-url (url "docs.racket-lang.org/rsound/index.html"))
(define racketcon-url (url "con.racket-lang.org/"))
(define kdenlive-url (url "kdenlive.org/"))

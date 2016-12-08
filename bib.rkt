#lang at-exp racket

(require scriblib/autobib
         scribble/base)

(provide (all-defined-out))

(define-cite cite citet gen-bib)

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

(define slideshow-jfp
  (make-bib #:title "Slideshow: functional presentations"
            #:author (authors "Matthew Flatt" "Robby Findler")
            #:date "2006"
            #:location (journal-location "JFP"
                                         #:pages '(593 619)
                                         #:volume 16)))

(define scribble-icfp
  (make-bib #:title "Scribble: closing the book on ad hoc documentation tools"
            #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
            #:date "2009"
            #:location (proceedings-location "ICFP"
                                             #:pages '(109 120)
                                             #:volume 14)))

(define applescript-hopl
  (make-bib #:title "Applescript"
            #:author (authors "William R. Cook")
            #:date "2007"
            #:location (proceedings-location "HOPL"
                                             #:pages '("1-1" "1-21")
                                             #:volume 3)))

(define gstreamer-url (url "https://gstreamer.freedesktop.org/"))
(define mlt-url (url "https://www.mltframework.org/}}"))
(define shotcut-url (url "https://www.shotcutapp.com/"))
(define applescript-use-url (url "http://www.davidheidelberger.com/blog/?p=107"))
(define openshot-url (url "http://www.openshot.org/"))
(define avisynth-url (url "http://avisynth.nl"))
(define mlt-xml-url (url "https://www.mltframework.org/docs/mltxml/"))
(define rsound-url (url "https://docs.racket-lang.org/rsound/index.html"))

#lang scribble/sigplan

@require[scriblib/figure
         (except-in scribble/manual cite)
         pict
         "utils.rkt"
         "bib.rkt"]

@title[#:tag "overview"]{Editing with Video}

@Figure-ref{hello-color} shows ...

@figure["hello-color" "A Simple Video"]{
 @codeblock{
  #lang video
  (clip "green")
 }
  @(scale (bitmap "sample.png") 0.15)
}

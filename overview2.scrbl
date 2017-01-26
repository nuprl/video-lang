#lang scribble/sigplan

@require[scriblib/figure
         pict
         pict/code
         racket/format
         "pictures.rkt"
         "utils.rkt"
         "bib.rkt"]

@(current-code-font code-font)

@title[#:tag "overview"]{The Design of Video}

The overview of the state of the art suggests that non-linear video editing
consists of a description and an action. Specifically, a video editor
demands a description of what the final video should look like in terms of
the given pieces.  The action of creating and rendering this video is a
distinct second step. Furthermore, experience tells us that like programs,
descriptions need abstractions; for example, a description may employ a
comprehension to apply a watermark to all images, or it may employ of one
module per ICFP presentation to make up a complete ICFP channel. 

The Video language gets to the heart of the problem. Each Video program
is a complete module that intermingles descriptions of video clips and
functions. It denotes a module that exports a single item: a playlist
description of the complete video. One way to use a Video module is to
create a video with a renderer. Another way is to import it into another
Video module and to incorporate the exported video clip description into a
larger one. 

@;{
@figure["hello-color" "A Video Program (top) and output (bottom)"]{
 @hello-green}

@section{Clips}
@section{Playlists}
@section{Filters}
@section{Multitracks}
@section{Playing and Rendering}
@section{From APIs to Languages}
}

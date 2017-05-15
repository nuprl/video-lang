#lang scribble/manual

@title{Artifact: Super8, Languages for Making Movies---A Functional Pearl}
@(author (author+email "Leif Andersen" "leif@ccs.neu.edu")
         (author+email "Stephen Chang" "stchang@ccs.neu.edu")
         (author+email "Matthias Felleisen" "matthias@ccs.neu.edu"))

@(require scribble/core)

This is the README file for the artifact that accompanies:
``Super8, Languages for Making Movies---A Functional Pearl'' in ICFP 2017

Our artifact consists of a VM image that contains:
@itemlist[
 #:style 'compact
 @item{a distribution of the Racket programming language (version 6.9),}
 @item{a distribution of the Video programming language,}
 @item{the required libraries to support Video's renderer,}
 @item{example programs written in Video,}
 @item{a copy of a draft of the paper,}
 @item{and a distribution of the Typed Video variant of Video.}]

The goals of this artifact are to:
@itemlist[
 #:style 'compact
 @item{provide a simple environment to create and run Video programs, and}
 @item{provide an archival copy of Video.}]

At the time of this artifact's creation, the video language can also be found at:
@url["https://github.com/LeifAndersen/racket-video"]

The RacketCon source files are too large to include in this
image. The ones used for this paper can be found at:
@url["http://ccs.neu.edu/~leif/icfp-2017/rcon.tar.gz"]

Finally, the edited RacketCon 2016 videos created using
Video can be found
@hyperlink["https://www.youtube.com/watch?v=nOyIKCszNeI&list=PLXr4KViVC0qKSiKGO6Vz9EtxUfKPb1Ma0"]{
 on Youtube}.

@elem[#:style (style #f (list (color-property "red")))]{
 The Video scripts in this VM will occasionally segfault
 during execution. This is caused by the version of libmlt
 included in the VM. If this happens, simply re-run the
 script.}

@section{Setting up and installing the artifact}

The artifact is available as a virtual machine appliance for VirtualBox. If
you are already reading this README in the VM, feel free to ignore the
rest of this section.

To run the artifact image, open the given @tt{.ovf} file using the
@tt{File->Import Appliance} menu item. This will create a new VM
that can be launched after import. We recommend giving the VM at least
4GB of RAM.

The image is configured to automatically login to the @tt{artifact} user
account. The account has root privileges using @tt{sudo} without a password.
The password for the account is @tt{artifact}.

@section{Artifact Overview}

The relevant files for this artifact are in
@filepath["/home/artifact/Desktop"], which contains:
@itemlist[
 #:style 'compact
 @item{@filepath{README.html}--this file,}
 @item{@filepath{VIDEO-MANUAL.html}---a copy of the user manual for video,}
 @item{@filepath{super8.pdf}--the paper,}
 @item{@filepath{video/}--a copy of Video,}
 @item{@filepath{typed-video/}--a copy of Typed Video,}
 @item{@filepath{examples/}--example Video programs,}
 @item{@filepath{DrRacket}--an IDE for editing Video programs,}
 @item{and @filepath{run-examples.rkt}--a script to run the examples in this repo.}]

@section{Video Examples}

Video programs can be run within DrRacket or from the
terminal. For this artifact, we recommend using the
terminal. To play a video, simply run @exec{raco video} from
the command line. For example, to run
@filepath["1-hellocolor.rkt"], simple execute:

@nested[@exec{raco video 1-hellocolor.rkt}]

If you instead run the videos from DrRacket, click on the
green camera icon that says: @onscreen["Preview Video"].

The provided @filepath{run-examples.rkt} script plays each
video in succession. Close one video to play the next
example.

The @filepath{examples/} folder contains several examples of
small video programs using many of Video's features. Here is
a list of each of these programs:

@itemlist[
 #:style 'compact
 @item{@filepath{1-hellocolor.rkt}--A simple program to
  ensure Video is running. The resulting video is a green screen.}
 @item{@filepath{2-colorfade.rkt}--Demonstrates transitions in
  video. Specifically, this video fades from green to blue.}
 @item{@filepath{3-clips.rkt}--Introduces @racket[clip], for
  importing video files.}
 @item{@filepath{4-filters.rkt}--Introduces filters, same as
  @filepath{3-clips.rkt} except now in grayscale.}
 @item{@filepath{5-project.rkt}--Show how to use
  @racket[#:start] and @racket[#:end] to cut a clip.}
 @item{@filepath{6-multitrack.rkt}--Introduces multitracks
  and explicit playlists, starts with the clip and immediate
  jumps to a red frame.}
 @item{@filepath{7-watermark.rkt}--Uses transitions in a
  multitrack to add a watermark to the video.}
 @item{@filepath{8-image.rkt}--A movie of a static circle,
  using @racket[image].}
 @item{@filepath{9-doubletransition.rkt}--Similar to
  @filepath{7-watermark.rkt}, but places two watermarks on the
  screen instead of one, through the use of the
  @racket[#:transitions] keyword.}
 @item{@filepath{10-properties}--Introduces properties, and
  uses them to determine where the watermark should be placed
  on the screen.}
 @item{@filepath{11-include.rkt}--Uses the
  @racket[external-video] function to reproduce
  @filepath["2-colorfade.rkt"].}
 @item{@filepath{12-cut.rkt}--Similar to
  @filepath["5-project.rkt"], but now uses an external filter
  to cut the clip. Also demonstrates calling filters as functions.}]

Each of these examples can be inspected in the provided
DrRacket IDE in
@filepath["/home/artifact/Desktop/DrRacket"]. The IDE has a
@onscreen["Preview Video"] button for watching the resulting
video file.

Additionally, the @exec{raco video} tool can render the
videos. Flags set the frame rate, resolution, output format,
etc. of the resulting video. As an example: you can preview
one of the files above with:

@nested{@exec{raco video 1-hellocolor.rkt}}

@section{Video Implementation}

The following is a brief list of the major files in Video's
implementation, and how they come together to make the
language. Every file in the list is relative to the Video
implementation
(@filepath["/home/artifact/Desktop/video/video/"]).

@itemlist[
 #:style 'compact
 @item{@filepath{private/}--Anything in this folder is
  specific to the implementation. Video users are not meant to
  link directly to it as its interface can and will change.}
 @item{@filepath{private/mlt.rkt}--FFI bindings for libmlt,
  which is used for video rendering.}
 @item{@filepath{private/init-mlt.rkt}--The libmlt library requires some
  initialization, this file takes care of that. It is split
  into a seperate file from @filepath["private/mlt.rkt"] so it
  can manged Video specific state independent of the FFI bindings.}
 @item{@filepath{private/semaphore.rkt}--An atomic
  semaphore used exclusively in
  @filepath["private/init-mlt.rkt"]}
 @item{@filepath{private/video.rkt}--This file
  simultaneously defines the internal DSL used to define Video
  and the structures used in the core Video language. Each
  video structure defined in this file additionally knows how
  it can be rendered. This file does not contain any of
  Video's syntax.}
 @item{@filepath{private/surface.rkt}--A tiny DSL to make
  writing Video surface syntax forms easier. It is used to
  make the surface syntax for Video. And eventually will be
  public so that users can make their own surface forms as
  well as new rendering backends.}
 @item{@filepath{private/editor.rkt}--This file implements
  the graphical NLVE widgets that can be placed directly in
  Video files using DrRacket.}
 @item{@filepath{private/tool.rkt}--This registers the NLVE
  editors with DrRacket so users can add them.}
 @item{@filepath{private/camera-icon.rkt}--This implements
  the @onscreen["Preview"]  button in DrRacket.}
 @item{@filepath{private/utils.rkt}--Miscellaneous utilities
  that multiple files can use.}
 @item{@filepath{examples/}--Contains the examples described above.}
 @item{@filepath{tests/}--Tests for the Video language.}
 @item{@filepath{scribblings/}--Source for Video's documentation.}
 @item{@filepath{lang/reader.rkt}--Installs Video as a
  proper @racket[#,hash-lang] in Racket.}
 @item{@filepath{main.rkt}--Works with
  @filepath["lang/reader.rkt"] to define the file level syntax
  used in Video. Also defines the syntax for users to create
  video dedicated procedures as described in the paper. This
  file does not define the Video related functions.}
 @item{@filepath{base.rkt}--Defines the video level
  functions described in this paper. This file is separated
  from @racket["main.rkt"] so users can use Video as a library
  as well as a language. In effect, @racket[video/base] is a
  library, while @racket[video] is a language which makes use
  of that library.}
 @item{@filepath{lib.rkt}--Extra libraries for Video. These
  are separate from @filepath["base.rkt"] as they are
  convenience utilities that are not essential to writing Video scripts.}
 @item{@filepath{surface.rkt}--Publicly available versions
  of the video-syntax extenstendors defined in @filepath["private/surface.rkt"].}
 @item{@filepath{core.rkt}--Publicly available core forms
  for video as defined in @filepath["private/video.rkt"].
  Making these public allows users to go one level lower,
  operating on Video's internal structures, without having to
  go all the way to the FFI. Ideally users shouldn't need to
  use this file.}
 @item{@filepath{init.rkt}--Runs @filepath["private/init-mlt.rkt"].}
 @item{@filepath{player.rkt}--The player that appears
  whenever previewing a video.}
 @item{@filepath{render.rkt}--The base class for Video
  renderers. The default renderer opens up a preview window,
  but different variants (found in @filepath["renderer/"])
  change the output format.}
 @item{@filepath{render/}--The different renderers used by
  @filepath["render.rkt"].}
 @item{@filepath{raco.rkt}--The implementation of the
  command line tool @exec{raco video} that Video uses.}
 @item{@filepath{info.rkt}--A basic config file used by the
  Racket package manager.}]

@;{Typed Video}
@include-section{typed-video.scrbl}

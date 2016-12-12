#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{The Video Editing Process}

Interactive video editing is primarily done by tools called
non-linear video editors@cite[technique-of-video-editing].
In the context of film production, @emph{non-linear} means
not destructive, the source videos will not slowly degrade
in quality during editing.@note{Digital editors achieve this
 result by operating on references to videos, rather than
 operating on the videos themselves.}
A non-linear video editor, shown
in @figure-ref{nlve-demo}, is a graphical tool with a @emph{
 time line} comprised of @emph{tracks}. Each track stores
video and audio clips. Finally, filters and transitions
indicate how to @emph{composite} videos, or how the various
clips should be combined together.

Video editing, while creative, is often a repetitive task.
For example, putting conference videos together often takes
a significant amount of repetative manual effort. Editing
each conference talk involves compositing a camera feed with
a screen capture feed and one for audio capture. Compositing
these feeds takes several steps, each feed must be shrunk
and placed on the appropriate part of the screen and be
synchronized, one at a time. On top of all of this, each
video also includes the conference logo at the start and end
of the video, and include a watermark throughout the video.
Each composited feed only takes a few mouse clicks to set
up, but each feed must be set up individually. Additionally,
the application must process each clip after every
operation, requiring more time. In aggregate, this process
become a large task. Worse still, this entire process is
repeated for every video, significantly increasing the work
load.

Professionals have developed tools and design patterns to
reduce the amount of repetitive manual labor in video
editing. They frequently develop macros@note{In the context
 of video editing, a macro is a series of buttons that are
 automatically pressed in sequence.} using languages like
AppleScript@cite[applescript-hopl] for the tools they
use.@note{@applescript-use-url} Most professional tools, and
some casual ones, even include an API to create new plugins.

Unfortunately, existing means of extending these tools are
insufficient. Macros are extremely brittle, and frequently
break, even from version to version of the same video
editor. Using a tool's official plug-in interface is more
robust, but is still tied specifically to that tool.
Blender@cite[essential-blender] for example, is only
scriptable with a modified Python interpreter that runs when
Blender is launched.

Another alternative is a general purpose multimedia
framework such as GStreamer,@note{@gstreamer-url} or the MLT
Framework.@note{@mlt-url} These frameworks are APIs for C
style languages that provide data types for building and
rendering videos. These frameworks are powerful enough that
video editing applications such as
Shotcut@note{@shotcut-url} use them as a supporting library.
The appeal of these frameworks is the ability to create
abstractions, such as functions, to handle otherwise
repetitive tasks. For example, a professional using one of
these frameworks can write a function to do the composeting
for a video of a conference talk.

Unfortunately, media frameworks for general purpose
languages introduce a new set of problems when used directly
for video editing. Anyone using them must focus on managing
the language rather than the actual video being created.
Design patterns reduces time spent managing the language,
but even that is a distraction from editing videos. Imagine
if authors tried to write a paper with Java rather than
@exact{\LaTeX}. Even if they had an API for document
writing, they would still spend a large portion of their
time managing the Java part of their work. Somewhere in
their project, they would still need to have a main class,
main function, create a document object, design a nice way
to imperatively add elements to the document object, and
eventually render the document object. Thus, an API is not
adequate for interactive video editing.

@figure["nlve-demo" "A Non-linear Video Editor, part of the Blender suite"]{
 @nlve-sample}

Another alternative is to use existing domain-specific
languages for video editing, which fall into one of two
categories. The first type is XML based DSLs such as MLT
XML@note{@mlt-xml-url} and the now deprecated
SMIL@cite[smil-tr]. The second type is scripting language
based DSLs such as AVISynth.@note{@avisynth-url} Both types
are better suited for interactive video editing, but
have their own set of problems.

XML allows people to declaratively express videos, but
removes the ability to make new procedures. The lack of
abstractions is adequate for storing a video representation,
but not for interactive video editing. Using the XML
processing capabilities of another language that does have
abstractions is not an option, because this leads back to
the problem of managing either a generic programming
language or an XML processing language like
XSLT@cite[xslt-tr]. Worse still, the XML facilities in most
programming languages are designed to process generic XML
files. For this reason, XML style languages for video
editing are frequently used only by enthusiasts and
tinkerers, who are more interested in playing with video
editor technology than actually editing videos

In principle, scripting language style DSLs are the right approach
for video editing, but current languages fail here. These
languages allow for some abstraction, but are not robust
enough for anything but simple tasks. Additionally, these
languages have an ad-hoc grammar, making robust use of them
even more difficult. AVISynth, for example, allows
programmers to create simple functions, but lacks control
flow constructs such as conditional branching.

An ideal environment for video editing includes the power of
a programming language, without losing the best parts of a
traditional non-linear video editor. Its users must be able
to describe the video without managing a program, in a
similar fashion to @exact{\LaTeX} or
Scribble@cite[scribble-icfp]. Video objects must be first
class, and in general they also need linguistic support. The
language must also be able to use more powerful linguistic
constructs when useful. Finally, the programming mechanism
it provides should not be ad-hoc. A functional DSL that
offers a certain amount of literate programming can solve
these constraints.

@bold{Video} is a Racket based DSL for video editing. Users
can express their videos without having to handle all of the
boilerplate of setting up maintaining a video object and
rendering code. Rather, programs in Video are declarative
descriptions of a video. Additionally, if a tasks become
repetitive, they have access to Racket's abstraction
facilities. This technique proves useful for editing many
videos that have similar templates, but are not always
identical, such as conference recordings or screencasts.

Video is also able to leverage the Racket programming
environment DrRacket@cite[plt-tr2] and graphical
toolkits@cite[plt-tr3]. Broadly speaking, video editors
benefit from what you see is what you get (WYSIWYG)
environments. DrRacket enables Video to allow to enable
WYSIWYG video editing when it is appropriate, and textual
editing when beneficial. Racket also contains an existing
API for creating pictures@cite[slideshow-jfp] and
sounds@note{@rsound-url} that Video can leverage.

This paper describes the design and implementation of Video.
We introduce the design of Video in @secref["overview"].
Next, @secref["implementation"] discusses how Video is
implemented. In @secref["extensions"] we discuss graphical
extensions to video to aid in editing, and how it fits into
DrRacket's work flow. We compare Video to related work in
@secref["related"] and finally conclude in
@secref["conclusion"].

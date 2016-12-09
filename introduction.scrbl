#lang scribble/sigplan

@require[scriblib/footnote
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Video Editing Process}

Video editing, while creative, is often a repetitive task.
For example, the first and second authors of this paper
found that they were spending a significant amount of
repetitive manual effort when putting together conference
recordings. Editing each conference talk involves
compositing a camera feed with a screen capture feed and one
for audio capture. Compositing these feeds takes several
steps, each feed must be shrunk and placed on the
appropriate part of the screen and be synchronized, one at a
time. Each composited feed only takes a few clicks to set
up, but each feed must be set up individually. Additionally,
a small amount of processing is required at each step,
requiring more time. On top of all of this, each video also
includes the conference logo at the start and end of the
video, and include a watermark throughout the video. Each
step only takes a few mouse clicks, but in aggregate they
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
framework such as GStreamer@note{@gstreamer-url}, or the MLT
Framework@note{@mlt-url}. These frameworks are APIs for C
style languages that provide data types for building and
rendering videos. These frameworks are powerful enough that
video editing applications such as
Shotcut@note{@shotcut-url} use them as a supporting library.

Unfortunately, media frameworks for general purpose
languages introduce a new set of problems when used directly
for video editing. Anyone using them must focus on managing
the language rather than the actual video being created.
Design patterns reduces time spent managing the language,
but even that is a distraction from editing videos. This
process is analogous to a authors trying to write a paper
using Java. Even if they had access to nice string literals
and the best API for document writing, they would still
spend a large portion of their time managing the Java part
of their work. Somewhere in their project, they would still
need to have a main class, main function, create a document
object, design a nice way to imperatively add elements to
the document object, and eventually render the document
object. Thus, an API is not adequate for interactive video
editing.

Another alternative is to use existing domain-specific
languages for video editing, which fall into one of two
categories. The first type is XML based DSLs such as MLT
XML@note{@mlt-xml-url}. The second type is scripting
language based DSLs such as AVISynth.@note{@avisynth-url}
Both types are better suited for interactive video editing,
but introduce a new class of problems.

XML allows people to express videos directly, but removes
the ability to make new abstractions. The lack of
abstractions is adequate for storing a video representation,
but not for interactive video editing. Using the XML
processing capabilities of another language that does have
abstractions is not an option, because this leads back to
the problem of managing a generic programming language.
Worse still, the XML facilities in most programming
languages are designed to process generic XML files. For
this reason, XML-style languages for video editing are
frequently used only by enthusiasts and tinkerers, who are
more interested in the world of video editing, rather than
editing their own videos.

Scripting language style video editing DSLs allow for some
abstraction, but are not robust enough for anything but
simple tasks. Additionally, these languages have an ad-hoc
grammar, making robust use of them even more difficult.
AVISynth, for example, allows programmers to create simple
functions, but lacks control flow constructs (such as
conditional branching) for writing anything complicated.

An ideal environment for video editing includes the power of
a programming language, without loosing the best parts of a
traditional non-linear video editor. Its users must be able
to describe the video without managing a program, in a
similar fashion to @exact{\LaTeX} or
Scribble@cite[scribble-icfp]. But it must also be able to
break out into abstraction mechanisms when useful. Finally,
the programming mechanism it provides should not be ad-hoc.
A functional DSL that offers a certain amount of literate
programming can solve these constraints.

Video is a Racket based DSL for video editing. Users can
express their videos without having to handle all of the
boilerplate of setting up and maintaining video object and
rendering code. They simply describe the video they wish to
produce. Additionally, if a tasks become repetitive, they
have access to Racket's abstraction facilities. This
technique proves useful for editing many videos that have
similar templates, but are not always identical, such as
conference recordings or screencasts.

Video is also able to leverage the Racket programming
environment DrRacket@cite[plt-tr2] and graphical
toolkits@cite[plt-tr3]. Broadly speaking, video editors
benefit from what you see is what you get (WYSIWYG)
environments. DrRacket enables Video to allow to enable
WYSIWYG video editing when it is appropriate, and textual
editing when beneficial. Racket also contains an existing
API for creating pictures@cite[slideshow-jfp] and
sounds@note{@rsound-url} that Video can leverage.

This paper describes the design and implementation of Video,
a Racket based DSL for video editing. We introduce the
design of Video in @secref["overview"]. Next,
@secref["implementation"] discusses how Video is
implemented. In @secref["extensions"] we discuss graphical
extensions to video to aid in editing, and how it fits into
DrRacket's work flow. We compare Video to related work in
@secref["related"] and finally conclude in
@secref["conclusion"].

#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "background"]{The State of Video Editing}

Interactive video editing is primarily done by tools called
non-linear video editors@cite[technique-of-video-editing],
or NLVEs. In the context of film production, @emph{
 non-linear} means not destructive, the source videos will
not slowly degrade in quality during editing.@note{Digital
 editors achieve this result by operating on references to
 videos, rather than operating on the videos themselves.} A
non-linear video editor, shown in @figure-ref{nlve-demo}, is
a graphical tool with a @emph{ time line} comprised of
@emph{tracks}. Each track is a time line of the video and
contains video and audio clips. stores video and audio
clips. All of these tracks exist on the same timeline, and
various transitions indicate how to @emph{composite} the
clips---how they should be combined together. Finally, the
NLVE @emph{renders} the video by playing the tracks
simultaneously and applying any additional filters.

@figure["nlve-demo" "A Non-linear Video Editor, part of the Blender suite"]{
 @nlve-sample}

Professionals have developed tools and design patterns to
reduce the amount of repetitive manual labor in video
editing. They frequently develop so-called macros---a
scripted sequence of user interface elements---in languages
like AppleScript@cite[applescript-hopl] for the tools they
use.@note{@applescript-use-url} Most professional tools,
such as Adobe Premiere, @note{@premiere-api-url} even
include an API to create plug-ins directly.

Extending tools in this fashion, however, has its limits.
Macros are extremely brittle and frequently break, even
within a single application. Using a tool's official plug-in
interface is more robust, but is still tied specifically to
that tool. Blender@cite[essential-blender] for example, is
only scriptable with a modified Python interpreter that runs
when Blender is launched.

An alternative class for reducing project boilerplate are
general purpose multimedia frameworks such as
GStreamer@note{@gstreamer-url} or the MLT
Framework@note{@mlt-url}. These frameworks are APIs for C
like languages that provide data types for building and
rendering videos. The appeal of these frameworks is the
ability to create abstractions, such as functions, to handle
otherwise repetitive tasks. For example, a professional
using one of these frameworks can write a function to do the
composeting for a video of a conference talk.

These frameworks are primarily used in two situations.
First, these frameworks often used as a back-end to
traditional NLVEs. As an example, Shotcut@note{@shotcut-url}
uses the MLT framework as a back-end. Second, professionals
use these frameworks to batch process videos, particularly
when interactive development is not desired.

Professionals also use domain-specific languages for video
editing. These DSls primarily fall into two categories: XML
style DSLs and Scripting Language style DSLs.

XML style DSLs, such as MLT XML@note{@mlt-xml-url} and the
now deprecated SMIL@cite[smil-tr], offer a declarative
language for videos. The entire program is the video being
described. These languages generally do not have functions
or any other type of abstraction. Professionals tend not to
deal with these XML languages directly when video editing.
Rather, NLVEs use these languages to save video projects.

The second type is scripting language based DSLs, such as
AVISynth.@note{@avisynth-url} Unlike XML based languages,
these DSLs do have functions and other abstractions.
A Program in these languages, however, are declarative
description of a video.

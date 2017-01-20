#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "background"]{The State of Video Editing}

People use tools called non-linear video editors (NLVEs) to
compose video clips@cite[technique-of-video-editing]. In the
context of film production, @emph{non-linear} means not
destructive, the source videos do not slowly degrade in
quality during editing.@note{Digital editors achieve this
 result by operating on references to videos, rather than
 operating on the videos themselves.} A non-linear video
editor is a graphical tool with a @emph{time line} comprised
of @emph{tracks}. Each track is a time line of the video
and contains video and audio clips. All of these tracks
exist on the same timeline, and various transitions indicate
how to @emph{composite} the clips.
Finally, the NLVE @emph{renders} the video by playing the
tracks simultaneously and applying additional filters.

Professionals have developed tools and design patterns to
reduce the amount of repetitive manual labor in video
editing. They frequently develop so-called macros---a
scripted sequence of user interface elements---in languages
such as AppleScript@cite[applescript-hopl] for the tools
they use.@note{@applescript-use-url} Some professional
tools, such as Adobe Premiere, @note{@premiere-api-url} even
include an API to create script-style plug-ins directly.

Extending tools in this fashion, however, has its limits.
Macros are extremely brittle and frequently break, even
within a single application, because they do not understand
the underlying tools. Using a tool's official plug-in
interface produces more robust software, but leads to a
plug-in that is tightly coupled with its tool. It can only
be used in settings where the entire tool is present.
Blender@cite[essential-blender] for example, is only
scriptable with a modified Python interpreter that runs when
Blender is launched.

An alternative approach to reducing boilerplate actions are
general purpose multimedia frameworks such as
GStreamer@note{@gstreamer-url} or the MLT
Framework.@note{@mlt-url} These frameworks are APIs for
C-like languages that provide data types for building and
rendering videos. The appeal of these frameworks is the
ability to create abstractions, such as functions, to handle
otherwise repetitive tasks. For example, a professional
using one of these frameworks can write a function to create
the video for a conference talk.

These frameworks are primarily used in two situations.
First, they make up the back-ends to
traditional NLVEs. For example, Shotcut@note{@shotcut-url}
uses MLT. Second, professionals
use these frameworks to batch process videos, particularly
when interactive development is not desired.

Professionals also use domain-specific languages for video
editing. These DSLs primarily fall into two categories: XML
based DSLs and scripting language based DSLs.

XML DSLs such as MLT XML and the now deprecated
SMIL@cite[smil-tr] offer a declarative language for
processing videos. The entire program is the video being
described. These languages generally do not have functions
or any other type of abstraction. Professionals tend not to
deal with these XML languages directly when video editing.
Rather, NLVEs use these languages to save video projects.

Scripting-based DSLs, such as AVISynth@note{@avisynth-url}
are declarative but have their own limitations. Unlike XML
based languages, these DSLs do have functions and other
abstractions. However, they are only robust enough for the
simplest tasks, having ad-hoc grammars and little to no code
reuse features. AVISynth, for example, allows programmers to
create simple functions, but lacks control flow constructs
such as conditional branching.

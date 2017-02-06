#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "background"]{The State of Video Editing}

People use so-called non-linear video editors (NLVEs) to
compose video clips@cite[technique-of-video-editing]. In the
context of film production, @emph{non-linear} means
non-destructive, the source videos do not degrade in
quality due to editing.@note{Digital editors achieve this
 result by operating on references to videos, rather than
 operating on the videos themselves.} A NLVE is a graphical
tool with a @emph{time line} comprised of @emph{tracks}.
Each track is a time line of the video and contains video
and audio clips. All of these tracks exist on the same
timeline, and various transitions indicate how to
@emph{composite} the clips. Finally, the NLVE @emph{renders} the
tracks as a complete video suitably edited.

Over time, professionals have developed tools and design
patterns to reduce the amount of repetitive manual labor in
video editing. They frequently develop so-called macros---a
scripted sequence of user interface elements---in languages
such as AppleScript@cite[applescript-hopl] for their
tools.@note{@applescript-use-url} Some professional tools,
such as Adobe Premiere, @note{@premiere-api-url} even
include an API to create script-style plug-ins directly.

Extending tools in this fashion, however, has limits.
Macros are extremely brittle and frequently break, even
within a single application, because they do not understand
the underlying tools. Using a tool's official plug-in
interface produces reasonably robust scripts but yields a
plug-in that is tightly coupled with its tool. It can only
be used in settings where the entire toolchain is present.
Blender@cite[essential-blender] for example, is only
scriptable with a Blender-specific Python interpreter that runs when
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
First, they are the back-ends to NLVEs. For example,
Shotcut@note{@shotcut-url} uses MLT. Second, professionals
use these frameworks to batch-process videos, particularly
when interactive development is not desired. While the
frameworks work well for these use cases, they fall short
when there is a need to combine interactive and programmatic work flows.
Studios tend to stick with NLVEs, and only use these
frameworks sparingly.

Professionals also use domain-specific languages for video
editing. These DSLs primarily fall into two categories:
XML-based DSLs and scripting-based DSLs.
XML DSLs such as MLT XML and the now-deprecated
SMIL@cite[smil-tr] offer a declarative language for
processing videos. These languages generally do not have
functions or any other type of abstraction. Professionals
tend not to deal with these XML languages directly when
video editing. Rather, NLVEs use these languages as a file
format to save video projects.

Scripting-based DSLs, such as AVISynth@note{@avisynth-url}
are declarative but have their own limitations. Unlike
XML-based languages, these DSLs support functions and other
abstractions. However, they are only robust enough for the
simplest tasks: playing videos in sequence with transitions
and a minor visual effects. They also have ad-hoc grammars
and no code reuse features except for functions. AVISynth,
for example, allows programmers to create simple functions,
but lacks control flow constructs such as conditional
branching.

#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "background"]{Primer}

As mentioned in the introduction, people use so-called
non-linear video editors (NLVEs) to compose video
clips@cite[technique-of-video-editing]. In the context of
film production, @emph{non-linear} means non-destructive,
that is, the source videos do not degrade in quality due to
editing.@note{Digital editors achieve this result by
 operating on references to videos, rather than operating on
 the videos themselves.} A NLVE is a graphical tool with a
@emph{time line} of @emph{tracks}, each lade on top of the
other. Each track is a composition of video clips, audio
clips, and effects playing in sequence. The NLVE @emph{
 renders} these tracks by playing them all simultaneously,
placing one track on top of the other. Obviously a screen
displaying the result can only play a single track for
video;@note{Audio tracks can actually be played together;
 audio editors do this.} by convention this is the last
or top track. Video editors use effects to @emph{composite}
tracks together. That is, effects splice two or more tracks
together so that they appear on the screen at the same time.
As a result, the renderer uses these effects to combine
tracks as they play and either output the result to a file
or play it on a screen.

Over time, professionals have developed tools and design
patterns to reduce the amount of repetitive manual labor in
video editing. They frequently develop so-called macros---a
scripted sequence of user interface elements---in languages
such as AppleScript@cite[applescript-hopl].
Some professional tools,
such as Adobe Premiere, @note{@premiere-api-url} even
include an API to create script-style plug-ins directly.

Extending tools in this fashion has limits.
Macros are extremely brittle and frequently break, even
within a single application, because macro languages do not understand
the underlying tools. Using a tool's official plug-in
interface produces reasonably robust scripts but yields a
plug-in that is tightly coupled with its tool. It can only
be used in settings where the entire toolchain is present.
Blender@cite[essential-blender] for example, is only
scriptable with a Blender-specific Python interpreter that runs when
Blender is launched.

Alternative approaches to reducing boilerplate actions use
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
Studios tend to stick with NLVEs, and use these
frameworks only sparingly.

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
abstractions. However, they support only the simplest of
tasks: playing videos in sequence with transitions and a
minor visual effects. They also tend to lack any formal
grammars and use a small script for a parser. Finally, they lack many code
reuse features. AVISynth, for example, allows programmers to
create simple functions, but lacks control flow constructs
such as conditional branching.

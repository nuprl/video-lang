#lang scribble/sigplan

@require[scriblib/footnote
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "introduction"]{Video Editing Process}

Video editing, while creative, is often a repetitive task.
For example, editing a conference talk frequently involves
compositing a camera feed with a screen capture feed and one
for audio capture. Additionally, the video may also include
the conference logo at the start and end of the video. While
this process is straightforward, it takes time and manual
effort. This is a problem when a conference has many talks,
all of which have the exact same process for editing.

Professionals have developed both tools and design patterns
to speed up the process for video editing. They frequently
develop macros@note{In the context of video editing, a macro
 is a series of buttons that are automatically pressed in
 sequence.} for the tools they use. Most professional
tools even include an API to create new plugins.

Unfortunately, existing means of extending these tools are
insufficient. Macros are extremely brittle, and frequently
break. Using a tool's official plug-in interface is more
robust, but is still tied specifically to that tool.
Blender@cite[essential-blender] for example, is only
scriptable with a modified Python interpreter that runs when
Blender is launched.

A professional might then turn to a more generic framework,
only to find that they introduce a new set of problems.
These frameworks are frequently written for imperative C
like languages. Professional video editors are forced to
focus on managing the language rather than the actual
product. A professional may follow a design pattern to spend
less time managing the language, but then they are spending
time simply following the pattern. This process becomes
analogous to an author trying to write a paper using Java.
Even if they had access to nice string literals, they would
still spend a large portion of their time managing the java
side of their work. Thus, these frameworks are not
frequently used for interactive video editing, and are
generally used as a library for other video editors.

Existing XML style languages for video editing are better
suited for interactive video editing, but have their own
problems. Using XML directly removes abstraction, the very
reason why the professional left a traditional non-linear
video editor. They can regain abstraction by using the XML
processing capabilities of another programming language.
However, professionals that do this are back to the problem
of managing a generic programming language. Worse still, the
XML facilities in most programming languages are designed to
process generic XML files. So any developer wanting to take
this route will inevitably need to build up their own
standard library for useful XML based compositors.
Maintaining and extending this library becomes non-trivial,
even assuming that the professional knew about and followed
good software practices. Remember that while they are
spending all of this effort making abstractions, they are
not focusing on their actual work, editing videos. For this
reason, XML-style languages for video editing are frequently
used only by enthusiasts and tinkerers, who are more
interested in the world of video editing, rather than
editing their own videos.

A functional DSL for editing videos reduces the repetition
in video editing, while not adding the overhead of a
managing a traditional programming language.

This paper describes the design and implementation of Video,
a Racket based DSL for video editing.

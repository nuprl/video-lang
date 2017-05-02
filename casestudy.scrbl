#lang scribble/acmart

@require[scriblib/footnote
         scriblib/figure
         (except-in scribble/manual cite)
         (except-in pict blank)
         (except-in pict/code typeset-code code)
         racket/format
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@(current-code-font code-font)
@(get-current-code-font-size (λ () font-size))
@(current-code-line-sep code-line-sep)

@title[#:tag "case-study"]{Kill Bill: Vol. 2}

Video shines in situations where films are similar with
slight variations, such as conference recordings. Each talk
has a recording of the speaker, a screen capture feed, and
audio feeds for both the speaker and audience questions.
Post production composites these feed into one stream and
inserts relevant watermarks. Each talk recording plays the
feeds in lockstep, but slight artifacts in recording vary
from talk to talk.

The high amount of repeatability makes Video an ideal
language for processing conference recordings. Every video
draws from a single library which automatically converts the
raw feeds into a processed video. Additionally, Video's
terse syntax enables the post-producer to focus on the
unique aspects of each feed, such as combining multiple
files together, noise reduction, and setting cut points.

This section is devoted to evaluating Videos ease of
use.@note{Readers interested in the mechanics of how to use
 Video to edit conference recordings can look in the previous
 section (@secref{overiew}).} Specifically, we show that
creating videos is so easy that the author for Video
actually is able to create both Video and edit a bundle of
conference recordings with less effort than it takes to edit
those same talks by hand. This result is significant in two
ways. First, it shows the power of embedded DSLs, and how
they serve as a powerful mechanism for producing both
simpler and more robust code with little effort. Second, it
shows that the Racket design enables programmers to
construct their own embedded DSLs with very little effort.

@section{Editing Videos}

As with many Racket-style DSLs, Video exists to ease an
otherwise repetitive task; in this case, video editing. To
demonstrate this, we compare the editing time for editing
conference recordings for RacketCon 2015 and 2016. RacketCon
2015 recordings are hand edited, while the 2016 ones are made
with Video. Furthermore, Video's maiden program is the 2016
recordings, which gives an idea for the ratio of effort
Videos saves and the required to actually make Video.

The RacketCon 2015 recordings are edited with Kdenlive, a
traditional non-linear video editor. Additionally, two
people are responsible for editing these videos. The
conference date is Sept. 27, 2015 and the publish date for
the conference recordings is Nov. 9, 2015. Thus, the delta
from recording to publishing is 43 days.

Unlike the previous year, the RacketCon 2016 recordings are
edited with Video. Additionally, only one person is
responsible for editing the 2016 videos. The conference date
is Sept. 18, 2016 and the publish date is Nov. 11, 2016.
Thus, the delta for 2016 is 54 days, or 11 days longer than
the previous year. On top of editing, this delta also
represents the time spent creating the Video language.

@section{Itemized Effort}

The two years have different production patterns. Precise
records for the effort spent in 2015 do not exist, but the
editors themselves provide anecdotes. First, picking an
editing environment, and then creating a template for that
environment takes a large amount of time. Once the template
is set up, and a work-flow is established, editing can
proceed reasonably quickly. The work, however, is monotonous
and repetitive. Each editor must follow the exact same
script of button presses to edit each recording. While there
are plenty of talks, there are too few to merit the effort
to create a script to press the buttons. Furthermore, each
video has slightly different qualities, such as start and
end times, that further complicates such a script.

More precise records for 2016 do exist and are shown in
@figure-ref["video-lenghts"]. This figure shows the length
of each talk, the lines of code taken to implement them, and
the time spend editing each video. The first video took
significantly longer than the rest as it is the first real
video created with the language. Additionally, equipment
failure caused the morning slides to be lost; piecing them
together takes additional time. Editing the later talks is significantly faster,
with the longest bottleneck being previewing the talk.

@figure["video-lengths" "Editing Effort for RacketCon Video Programs"]{
@exact|{\begin{tabular}{@{}llrrr@{}}\toprule
 Talk Title                           & Presenter    & Length (min) & Lines of Code & Edit Time (h) \\
 \midrule
 Synthesis and Verification for All   & Torlak       & 60:07        & 9             & 7 \\
 Languages in an Afternoon            & King         & 18:30        & 8             & 2 \\
 Generative Art with Racket           & Setti        & 12:53        & 10            & 1 \\
 Racket is my Mjolnir                 & Knauth       & 28:26        & 8             & 0.67 \\
 Functional Lighting                  & Steinberg    & 14:14        & 10            & 0.5 \\
 Contracts for Security               & Moore        & 20:37        & 7             & 0.33 \\
 Type Systems as Macros               & Chang/Knauth & 22:06        & 7             & 0.5 \\
 The Making of ``Beautiful Racket''   & Butterick    & 18:38        & 5             & 0.1 \\
 Population game simulation in Racket & Nguyen       & 18:07        & 7             & 0.33 \\
 Spelunking through JPEG with Racket  & Wingo        & 23:20        & 7             & 0.5 \\
 R-r-r-r-REMIX                        & McCarthy     & 20:25        & 8             & 0.5 \\
 Racket Does Dijkstra                 & Davies       & 22:25        & 7             & 0.5 \\
 Language Integrated Nitpicking       & Firth        & 17:23        & 8             & 0.67 \\
 \bottomrule
 \end{tabular}}|}

In addition to the line counts in the previous figure, the
talk recordings relied on a common @racketmodname[utils]
module. This module is 326 lines long and provides a host of
helper functions for building RacketCon videos. Most
notably, this file defines @racket[make-conference-video],
used to construct the conference video in the previous talk.
This function is 66 lines long, and is primarily a
description of a conference video given feeds for the
presenter, slides, and audio.

While the editing process for the 2016 recordings takes 11
days longer than the proceeding year, only one person,
rather than two people, worked on the videos. This gives a
strong indication that not only is using Video significantly
faster than a traditional NLVE, but creating Video is simple
enough that it saves time, even if it is only used for
editing the 2016 talks.

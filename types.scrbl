#lang scribble/sigplan

@title[#:tag "types"]{Inspector Gadget}
@(require scribble/manual)


@Secref{implementation} shows how the Racket ecosystem fosters the creation of
DSLs like Video. The effectiveness of a language, however, often hinges on both
its design and the programming environment around the language. Thus, we
enhance Video with two additional features that support a Video programmer's
workflow: a static type system that helps with debugging (described in this
section) and a graphical interface (described in @secref{extensions}), and show
that Racket supports implementation of such features in a straighforward manner.

Video utilizes two primary types of data, producers and transitions. A Video
program slices and combines these values together so common programmer error
involves video lengths. For example, the following piece of code tries to
extract 15 frames from a producer that is only 10 frames long.

@racketblock[(cut-producer (color "green" #:length 10) #:start 5 #:end 20)]

Worse, since the video editing is non-linear, the errors (or crashes) do not
appear until the last compilation step, when the final video is rendered.

To address this problem, we added to Video lightweight dependent types 
(reminiscent of Xi and Pfenning's ATS) @;cite
where the types of producers are indexed by its length. With our Typed Video
language, we can equip the @racket[make-speaker-slide-composite] function from
@secref{case-study} with a signature:

@racketblock[(code:comment "Combine the video feed for a conference recording")
             (define (make-speaker-slide-composite {n} [speaker : (Producer n)]
                                                       [slides  : (Producer n)]
                                                       -> (Producer n))
               @multitrack[speaker slides logo background
                                   #:transitions
                             @list[@composite-transition[0 0 3/10 1
                                    #:top speaker
                                    #:bottom background]
                                   @composite-transition[0 1/2 3/10 1
                                    #:top logo
                                    #:bottom background]
                                   @composite-transition[1/3 0 2/3 1
                                    #:top slides
                                    #:bottom background]]]
                       @(define background
                              @blank[@properties-ref[speaker
                                                     'length]]))]

In addition, a programmer may specify constraints on functions, e.g., @racket[make-talk-video]:

@racketblock[(code:comment "Add conference logos to the front and end of a video.")
             (define (make-talk-video {n} [main-talk : (Producer n)]
                                          #:when (>= n 400)
                                          -> (Producer (+ n 600)))
               (playlist begin-clip
                         @fade-transition[200]
                         main-talk
                         @fade-transition[200]
                         end-clip)
               (define begin-clip @image[logo #:length 500])
               (define end-clip @image[logo #:length 500]))]

Producers in Typed Video have type @tt{Producer} and are indexed by their
length. The typed @racket[make-talk-video] function specifies that its input
must be a producer of at least 400 frames (due to the two 200-frame
transitions), and that the output adds 600 frames to the input (due to the
added intro and outro, minus the transition frames).

Constraints may propagate to other functions. For example, if a programmer
writes the following signature for @racket[make-conference-talk]:

@racketblock[(define (make-conference-talk {n} [speaker : (Producer n)]
                                               [slides : (Producer n)]
                                               [audio : Procucer]
                                               [offset : Int]
                                               -> (Producer (+ n 600)))
               @attach-audio[video audio offset]
               @(define* _ @make-speaker-slides-composite[speaker slides])
               @(define* _ @make-talk-video[_])
               @(define video @make-talk-video[_]))]

The function assumes an additional constraint (@racket[(<= n 400)]) due to the
call to @racket[make-talk-video]. Thus calling @racket[make-conference-talk]
with less than 400 frames of slides results in a type error.

We do not give a full formal presentation of our type system since (1) it
utilizes mostly known type system technology, and (2) it is not the object of
study in this paper, but the rest of this section will present a few key
aspects.

Typed Video uses a subtyping relation. For example here is the rule for producers:

@;mathpar[@tt{Producer n}][@tt{Producer n}]
@verbatim|{
              m >= n
   ----------------------------
   (Producer m) <: (Producer n)
}|

A producer with type @tt{(Producer m)} typechecks with a producer of type
@tt{(Producer n)} only if @tt{m >= n}. Intuitively, if a function requires a
producer of length @tt{n}, it is acceptable to give the function a producer of
length greater or equal to @tt{n}. Thus our type system ensures that producer
values do not flow into positions where the length of the producer is less than
expected.

@(require scribble/core)
@(define (inferrule top bottom)
   (make-element (make-style "inferrule" '(exact-chars))
                 (list top "}{" bottom)))
@inferrule["m >= n"]{(\texttt{Producer } m) <: (\texttt{Producer } n)}

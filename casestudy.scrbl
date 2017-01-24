#lang scribble/sigplan

@require[scriblib/footnote
         scriblib/figure
         (except-in scribble/manual cite)
         (except-in pict blank)
         racket/format
         "pictures.rkt"
         "bib.rkt"
         "utils.rkt"]

@title[#:tag "case-study"]{Case Study: Conference Videos}

This section gives case studies for uses of Video, focusing
particularly on editing conference videos.

@(split-minipage
  #:split-location 0.7
  @codeblock|{@where[make-speaker-slide-composite <-
                     (λ (speaker slides)
                       @multitrack[speaker slides background
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
                       @where[background <- @blank[@properties-ref[speaker 'length]]])]}|
  (centered
   (scale-1080p (bitmap "res/stephen.jpg") 150)))

Some text...

@codeblock|{@where[make-talk-video <-
                   (λ (main-talk)
                     @playlist[begin-clip
                               @fade-transition[200]
                               main-talk
                               @fade-transition[200]
                               end-clip]
                     @where[begin-clip <- @image[logo #:length 500]]
                     @where[end-clip <- @image[logo #:length 500]])]}|
@(centered
  (make-playlist-timeline
   (clip-scale (bitmap "res/rcon.png"))
   (ellipses)
   (clip-scale (bitmap "res/geoffrey.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/stephen.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/stephen2.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/stephen3.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/alexis.jpg"))
   (ellipses)
   (clip-scale (bitmap "res/rcon.png"))))

More text

@(split-minipage
  #:split-location 0.6
  @codeblock|{@where[attach-conference-talk <-
                     (λ (video audio offset)
                       @multitrack[video cleaned-audio
                         #:length (property-ref video 'length)]
                       @where[cleaned-audio <-
                              @attach-filters[
                               audio
                               @list[@project-filter[#:in offset]
                                     @envelope-filter[50
                                      #:direction 'in]
                                     @envelope-filter[50
                                      #:direction 'out]]]])]}|
  (vc-append
   25
   (filled-rectangle 200 50)
   (make-playlist-timeline
    (clip-scale (bitmap "res/rcon.png"))
    (ellipses)
    (clip-scale (bitmap "res/stephen.jpg"))
    (ellipses)
    (clip-scale (bitmap "res/rcon.png")))))

And some more text...

@modblock->pict["chang.vid" "video"
                @~a|{
                     
                     @make-conference-talk[video audio 125]

                     @require["conference-lib.vid"]
                     @where[slides <- @project-filter[@clip["0005.MTS"]]
                                                      #:in 2900 #:out 80000]
                     @where[_ <- @playlist[@clip["0001.mp4"] @clip["0002.mp4"]]]
                     @where[_ <- @project-filter[_ #:in 3900 #:out 36850]]
                     @where[_ <- @make-speaker-slides-composite[_]]
                     @where[_ <- @make-talk-video[_ slides]]
                     @where[video <- @make-talk-video[_]]
                     @where[audio <- @playlist[@clip["0001.wav"] @clip["0002.wav"]]]}|]

@exec{raco video --width 1920 --height 1080 --fps 48 --mp4 chang.vid}

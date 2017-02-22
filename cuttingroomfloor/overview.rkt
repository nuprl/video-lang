@; Keep?
@;{
An alternative is to apply multiple transitions to the same
track. To support this alternative, multitracks support the
@racket[#:transitions] keyword, which takes a list of
transitions. Each transition in this list composites a pair
of producers, top and bottom, in the multitrack. These
producers are specified with the @racket[#:top] and
@racket[#:bottom] keywords respectively, and must be
producers found in the multitrack.

The @racket[#:transitions] keyword also applies to
playlists.
This allows abstracting over both playlist and multitrack
functions that insert transitions into both of these data
representations without having to bother with list
operations directly. Here is an example:}

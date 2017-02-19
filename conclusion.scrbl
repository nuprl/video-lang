#lang scribble/sigplan

@title[#:tag "conclusion"]{Star Trek: Beyond}

This
introduces a common design patterns for embedded DSLs.
First, authors create or find an existing API for a task.
Next, if this API is not implemented in Racket, they create
Racket bindings for this API. As they create these bindings,
they must ensure that the invariants assumed by the Racket
environment are not violated. For example, if a function
will segfault when given bad data, the author must ensure
that this data is never given to the foreign function.
Finally, authors add cleaner front-end primitives and a
surface syntax for their language. Out of the box, the
Racket ecosystem makes this final task trivial. This leaves
would-be language authors with the task of creating bindings
for their favorite API.

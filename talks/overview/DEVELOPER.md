Developer Notes for psaifr/talks/overview
=========================================

* I'm having trouble getting stuff to run from the command line.  And
  I'm having trouble convincing Racket about where to put files.  So ...
  to build the images for the talk.

    1. Open GIMP.
    2. Start the Mediascript server.
    3. Open primer-images.rkt in DrRacket.  
    4. Click "Run"
    5. Evaluate `(make-all-small)`
    6. Evaluate `(make-all-large)

* You can also create individual slides with `(make-small DESCRIPTION)` and
  `(make-large DESCRIPTION)`.
  
* Those slides are created in /tmp.  You'll need to copy them over by hand.

    cp /tmp/*SMALL.png small
    cp /tmp/*LARGE.png large

* I've written custom scripts to generate the handout and talk.  They
  can be found in the tools directory.

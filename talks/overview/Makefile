
# +-----------------------+-------------------------------------------
# | Converting file types |
# +-----------------------+

%.html: %.talk
	cat $^ | ../tools/talk2handout > $@

%.ps: %.html
	html2ps -D -o $*.ps $<

%.pdf: %.ps
	ps2pdf $<  $*.pdf

# +------------------+------------------------------------------------
# | Standard targets |
# +------------------+

default: primer.html handout.html slides.html


# +-----------------+-------------------------------------------------
# | Special targets |
# +-----------------+

handout.html: intro.talk primer.talk followup.talk
	cat $^ | ../tools/talk2handout > $@

slides.html: intro.talk primer.talk followup.talk
	cat $^ | ../tools/talk2slides > $@

slides.ps: slides.html
	html2ps -D -f slides -o $*.ps $<

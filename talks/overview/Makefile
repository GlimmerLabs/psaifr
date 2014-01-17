
%.html: %.talk
	cat $^ | ../tools/talk2handout > $@
	
handout.html: intro.talk primer.talk followup.talk
	cat $^ | ../tools/talk2handout > $@

slides.html: intro.talk primer.talk followup.talk
	cat $^ | ../tools/talk2slides > $@

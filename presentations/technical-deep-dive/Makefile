# Makefile for Technical Deep Dive presentation

.PHONY: all pdf html clean watch

PRESENTATION = technical-deep-dive
EMACS = emacs

all: pdf

pdf: $(PRESENTATION).pdf

$(PRESENTATION).pdf: $(PRESENTATION).org
	@echo "Generating PDF presentation..."
	$(EMACS) --batch \
		--eval "(require 'ox-beamer)" \
		--eval "(setq org-export-allow-bind-keywords t)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(find-file \"$(PRESENTATION).org\")" \
		--eval "(org-beamer-export-to-pdf)"

html: $(PRESENTATION).html

$(PRESENTATION).html: $(PRESENTATION).org
	@echo "Generating HTML presentation..."
	$(EMACS) --batch \
		--eval "(require 'ox-reveal)" \
		--eval "(find-file \"$(PRESENTATION).org\")" \
		--eval "(org-reveal-export-to-html)"

watch:
	@echo "Watching for changes..."
	@while true; do \
		inotifywait -e modify $(PRESENTATION).org 2>/dev/null || sleep 2; \
		$(MAKE) pdf; \
	done

clean:
	rm -f $(PRESENTATION).pdf
	rm -f $(PRESENTATION).tex
	rm -f $(PRESENTATION).html
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb
	rm -f *.png

diagrams:
	@echo "Generating diagrams..."
	ditaa layers.txt layers.png
	ditaa test-pyramid.txt test-pyramid.png
	ditaa security.txt security.png
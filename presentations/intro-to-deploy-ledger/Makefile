# Makefile for presentation generation

.PHONY: all pdf slides clean watch

PRESENTATION = intro-to-deploy-ledger
EMACS = emacs

all: pdf

pdf: $(PRESENTATION).pdf

$(PRESENTATION).pdf: $(PRESENTATION).org
	@echo "Generating PDF presentation..."
	$(EMACS) --batch \
		--eval "(require 'ox-beamer)" \
		--eval "(setq org-export-allow-bind-keywords t)" \
		--eval "(find-file \"$(PRESENTATION).org\")" \
		--eval "(org-beamer-export-to-pdf)"

slides: $(PRESENTATION).html
	@echo "Generating HTML slides..."
	$(EMACS) --batch \
		--eval "(require 'ox-reveal)" \
		--eval "(find-file \"$(PRESENTATION).org\")" \
		--eval "(org-reveal-export-to-html)"

watch:
	@echo "Watching for changes..."
	@while true; do \
		inotifywait -e modify $(PRESENTATION).org; \
		$(MAKE) pdf; \
	done

clean:
	rm -f $(PRESENTATION).pdf
	rm -f $(PRESENTATION).tex
	rm -f $(PRESENTATION).html
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb

install-deps:
	@echo "Installing LaTeX dependencies..."
	@echo "Please ensure you have:"
	@echo "  - texlive-full or equivalent"
	@echo "  - beamer package"
	@echo "  - metropolis theme"
# Makefile for Executive Overview presentation

.PHONY: all pdf html clean watch assets

PRESENTATION = executive-overview
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

assets:
	@echo "Generating placeholder charts..."
	@echo "Creating ROI chart..."
	@convert -size 800x400 xc:white -fill black -pointsize 24 \
		-draw "text 350,200 'ROI Analysis'" roi-chart.png
	@echo "Creating market segments chart..."
	@convert -size 800x400 xc:white -fill black -pointsize 24 \
		-draw "text 300,200 'Market Segments'" market-segments.png
	@echo "Creating projection chart..."
	@convert -size 800x400 xc:white -fill black -pointsize 24 \
		-draw "text 300,200 '3-Year Projection'" 3year-projection.png

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
# Makefile for Live Demo presentation

EMACS := emacs
PRESENTATION := live-demo
ORG_FILE := $(PRESENTATION).org
PDF_FILE := $(PRESENTATION).pdf
TEX_FILE := $(PRESENTATION).tex

.PHONY: all pdf tex clean view present

all: pdf

pdf: $(PDF_FILE)

$(PDF_FILE): $(ORG_FILE)
	@echo "Generating PDF presentation..."
	@$(EMACS) --batch \
		--eval "(require 'ox-beamer)" \
		--eval "(setq org-export-allow-bind-keywords t)" \
		--eval "(setq org-confirm-babel-evaluate nil)" \
		--eval "(find-file \"$(ORG_FILE)\")" \
		--eval "(org-beamer-export-to-pdf)"

tex: $(TEX_FILE)

$(TEX_FILE): $(ORG_FILE)
	@echo "Generating LaTeX file..."
	@$(EMACS) --batch \
		--eval "(require 'ox-beamer)" \
		--eval "(find-file \"$(ORG_FILE)\")" \
		--eval "(org-beamer-export-to-latex)"

view: $(PDF_FILE)
	@echo "Opening PDF presentation..."
	@open $(PDF_FILE) 2>/dev/null || xdg-open $(PDF_FILE) 2>/dev/null || echo "Please open $(PDF_FILE) manually"

present: $(PDF_FILE)
	@echo "Starting presentation mode..."
	@pdfpc $(PDF_FILE) 2>/dev/null || \
	impressive $(PDF_FILE) 2>/dev/null || \
	echo "Install pdfpc or impressive for presentation mode"

clean:
	@echo "Cleaning generated files..."
	@rm -f $(PDF_FILE) $(TEX_FILE) *.aux *.log *.nav *.out *.snm *.toc *.vrb

help:
	@echo "Available targets:"
	@echo "  pdf     - Generate PDF presentation (default)"
	@echo "  tex     - Generate LaTeX source"
	@echo "  view    - Open PDF in viewer"
	@echo "  present - Start presentation mode"
	@echo "  clean   - Remove generated files"
	@echo "  help    - Show this help message"
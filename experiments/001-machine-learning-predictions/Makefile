# Makefile for ML Predictions Experiment

.PHONY: all clean extract train evaluate visualize

all: extract train evaluate visualize

extract:
	@echo "Extracting features from deployment data..."
	guile -l collect-data.scm -c "(extract-and-save)"

train:
	@echo "Training ML models..."
	python3 train_model.py

evaluate:
	@echo "Evaluating model performance..."
	python3 evaluate_model.py > evaluation-metrics.json

visualize:
	@echo "Generating visualizations..."
	python3 visualize_results.py

clean:
	rm -f training-data.csv model.pkl evaluation-metrics.json *.png

tangle:
	emacs --batch --eval "(progn (require 'org) (org-babel-tangle-file \"README.org\"))"
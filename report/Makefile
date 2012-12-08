BUILD=./build
FILENAME=racket-cas

all:
	latex -output-directory=$(BUILD) -draftmode $(FILENAME).tex
	bibtex $(BUILD)/$(FILENAME).aux
	latex -output-directory=$(BUILD) -draftmode $(FILENAME).tex
	latex -output-directory=$(BUILD) -output-format=pdf $(FILENAME).tex

clean:
	rm -rf $(BUILD); mkdir $(BUILD)

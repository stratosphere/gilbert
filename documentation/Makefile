SRC:=documentation.tex
DOCUMENT:=$(SRC:.tex=.pdf)

.PHONY: $(DOCUMENT)

# First rule should always be the default "all" rule, so both "make all" and
# "make" will invoke it.
all: $(DOCUMENT)

# MAIN LATEXMK RULE

# -pdf tells latexmk to generate PDF directly (instead of DVI).
# -pdflatex="" tells latexmk to call a specific backend with specific options.
# -use-make tells latexmk to call make for generating missing files.

# -interactive=nonstopmode keeps the pdflatex backend from stopping at a
# missing file reference and interactively asking you for an alternative.

$(DOCUMENT): $(SRC)
	latexmk -pdf -pdflatex="pdflatex -interactive=nonstopmode" -use-make $^

clean:
	latexmk -CA

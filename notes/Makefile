GENERATED=pdf fdb_latexmk fls log aux out tex toc
.DEFAULT_GOAL=all

.PHONY: clean all setup-record setup-normal transcript.tex transcript.org

HOSTNAME=$(shell hostname -s)

all: transcript.pdf

transcript.pdf: transcript.tex
	latexmk -f -pdfxe transcript.tex

.PHONY: transcript.tex
%.tex: %.org
	emacs $< --batch -f org-latex-export-to-latex --kill
%.html: %.org
	emacs $< --batch -f org-html-export-to-html --kill

clean:
	rm -f $(addprefix transcript., ${GENERATED})

setup-record:
	sudo alsactl restore -f alsa-setting/${HOSTNAME}/record.conf

setup-normal:
	sudo alsactl restore -f alsa-setting/${HOSTNAME}/normal.conf

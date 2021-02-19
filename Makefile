SRC := src
BINS := tc

all: tc
	@echo "Use \"./tc\" to run the generated binary file."

tc:
	mlton -output $@ ${SRC}/$@.sml

clean:
	rm -rf ${BINS}

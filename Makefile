TIG_BIN := ./tc

# Important parser files.
TIG_PARSE := $(addprefix tiger/, *.grm.sml *.lex.sml)

# Source files.
SRC :=	$(addprefix tiger/, ast.sml errormsg.sml symbol.sml tiger.grm tiger.lex) \
		$(addprefix target/, mips.sml pp.sml print_tig_ast.sml) \
		tc.sml


# Test cases.
TIG_TEST_DIR := tests/
TIG_TEST_SCRIPT := ./tig_test.sh
TIG_CUSTOM_TEST := tests/custom.tig
TIG_TEST_OUT := tig_test.out

# Files to be cleaned.
CLEAN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig) ${TIG_BIN} ${TIG_TEST_OUT}

.PHONY: all clean tests test

all: ${TIG_BIN}

${TIG_BIN}: ${SRC} ${TIG_PARSE}
	mlton -output $@ tc.mlb

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

run: ${TIG_BIN}
	@${TIG_BIN}

test: ${TIG_BIN}
	@${TIG_BIN} ${TIG_CUSTOM_TEST}


tests: ${TIG_BIN} | permissions
	$(shell ${TIG_TEST_SCRIPT} ${TIG_TEST_DIR} ${TIG_BIN} ${TIG_TEST_OUT})
	@echo "Check ${TIG_TEST_OUT} for the outputs, each file's ast is below the respective input file name."

permissions:
	@chmod u+x ${TIG_TEST_SCRIPT}

clean:
	rm -f ${CLEAN}
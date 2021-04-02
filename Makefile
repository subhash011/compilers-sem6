TIG_BIN := ./tc

# Important parser files.
TIG_PARSE := $(addprefix tiger/, *.grm.sml *.lex.sml)

# Source files.
SRC :=	$(addprefix tiger/, ast.sml errormsg.sml symbol.sml tiger.grm tiger.lex) \
		$(addprefix target/, mips.sml pp.sml print_tig_ast.sml) \
		tc.sml


# Test cases.
TIG_TEST_DIR := tests/
TIG_CUSTOM_TEST := tests/custom.tig
TIG_TESTS := $(filter-out ${TIG_CUSTOM_TEST}, $(wildcard tests/*))
TIG_TEST_SCRIPT := ./tig_test.sh
TIG_TEST_OUT := tig_test.out

# Files to be cleaned.
CLEAN := 	$(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig) ${TIG_BIN} ${TIG_TEST_OUT} \
			$(shell find -type d -name *.cm)

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
	@${TIG_BIN} -pc ${TIG_CUSTOM_TEST}


tests: ${TIG_BIN} ${TIG_TESTS}
	@echo "Check ${TIG_TEST_OUT} for the outputs or error logs."

%.tig: ${TIG_BIN} | setup_out
	@echo - $(notdir $@) >> ${TIG_TEST_OUT}
	@${TIG_BIN} -p $@ >> ${TIG_TEST_OUT} 2>&1||:
	@-${TIG_BIN} -p $@ 2>/dev/null | ${TIG_BIN} > /dev/null 2>&1 \
	&& (echo "$(notdir $@) .... \033[0;32mOK\033[0m"; exit 0) \
	|| (echo "$(notdir $@) .... \033[0;31mFAIL\033[0m"; exit 0)
	@echo "" >> ${TIG_TEST_OUT}

setup_out:
	@rm -f ${TIG_TEST_OUT}
	@echo "Test Directory - ${TIG_TEST_DIR}\n" >> ${TIG_TEST_OUT}

clean:
	rm -rf ${CLEAN}
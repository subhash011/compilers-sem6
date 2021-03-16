TIG_BIN := tiger/tiger
TIG := $(addprefix tiger/, tiger.mlb ast.sml tiger.grm.sml tiger.lex.sml tiger.sml printast.sml)
TIG_TEST_DIR := tests/
TIG_TEST_FILE := tests/custom.tig
TIG_TEST_OUT := tig_test.out
TIG_GEN := $(addprefix tiger/, *.grm.sml *.lex.sml *.grm.desc *.grm.sig)

.PHONY: all clean tests test

all: ${TIG_BIN}
	@echo "\n============================================="
	@echo "Binary file for tiger written to tiger/tiger"
	@echo "=============================================\n"

${TIG_BIN}: ${TIG}
	mlton -output $@ $<

%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<

test: ${TIG_BIN}
	@${TIG_BIN} ${TIG_TEST_FILE}


tests: ${TIG_BIN} | permissions
	$(shell ./tig_test.sh ${TIG_TEST_DIR} ${TIG_BIN} ${TIG_TEST_OUT})
	@echo "Check ${TIG_TEST_OUT} for the outputs, each file's ast is below the respective input file name."

permissions:
	$(chmod +x ${TIG_TESTS})

clean:
	rm -f ${TIG_BIN} ${TIG_GEN} ${TIG_TEST_OUT}
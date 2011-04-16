include version.mk

comma := ,
space := $(empty) $(empty)

EBIN_DIR = ebin
EDOC_DIR = docs
EMK_FILE = Emakefile
APP_FILE = $(EBIN_DIR)/fw.app
VSN_FILE = include/version.hrl
MOD_LIST = $(patsubst src/%.erl,%,$(wildcard src/*.erl))

TEST_DIR = reports
DO_TESTS ?= n

ifeq (${DO_TESTS}, y)
	RUN_TEST = escript eunit.escript $(MOD_LIST)
	TEST_SRC = ,\"tests/*\"
	TEST_OPT = ,{d, \'TEST\'}
endif


all:	build

build:	$(VSN_FILE) $(APP_FILE) $(EMK_FILE)
	erl -make
	$(RUN_TEST)

tests:	$(TEST_DIR)
	$(MAKE) DO_TESTS=y

edocs:	$(EDOC_DIR) $(VSN_FILE)
	escript edoc.escript $(APP_VSN)

clean:
	rm -f erl_crash.dump $(VSN_FILE) $(EMK_FILE)
	rm -rf $(EBIN_DIR) $(EDOC_DIR) $(TEST_DIR)

$(EMK_FILE):	Emakefile.in
	sed \
		-e "s:@EBIN_DIR@:$(EBIN_DIR):" \
		-e "s:@TEST_SRC@:$(TEST_SRC):" \
		-e "s:@TEST_OPT@:$(TEST_OPT):" \
		$< > $@

$(APP_FILE):	fw.app.in $(EBIN_DIR)
	sed \
		-e 's:@MOD_LIST@:$(subst $(space),$(comma),$(MOD_LIST)):' \
		-e 's:@APP_VSN@:$(APP_VSN):' \
		$< > $@

$(VSN_FILE):	version.mk
	echo '-define(VERSION, "$(APP_VSN)").' > $@

$(EBIN_DIR) $(EDOC_DIR) $(TEST_DIR):
	mkdir $@

.PHONY:		all docs build tests clean

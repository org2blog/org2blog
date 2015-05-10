EMACS=emacs

EMACS_CLEAN=-Q
EMACS_BATCH=$(EMACS_CLEAN) --batch
TESTS=

CURL=curl --silent -L
WORK_DIR=$(shell pwd)
PACKAGE_NAME=$(shell basename $(WORK_DIR))
TRAVIS_FILE=.travis.yml
TEST_DIR=tests

ORG_URL=http://orgmode.org/org-latest.tar.gz
ORG_TAR=org-latest.tar.gz
XML_RPC_URL=https://launchpad.net/xml-rpc-el/trunk/1.6.8/+download/xml-rpc.el
XML_RPC=xml-rpc
METAWEBLOG_URL=https://raw.githubusercontent.com/punchagan/metaweblog/master/metaweblog.el
METAWEBLOG=metaweblog

test: clean
	@cd $(TEST_DIR)                                   && \
	(for test_lib in *-tests.org; do                       \
	    $(EMACS) $(EMACS_BATCH) -L . -L .. -L ../org-mode/lisp  \
	    -l $(XML_RPC) -l cl -l $(METAWEBLOG) --eval \
	    "(progn                                          \
              (org-babel-do-load-languages 'org-babel-load-languages  '((emacs-lisp . t) (python . t))) \
              (fset 'ert--print-backtrace 'ignore)           \
              (setq org-confirm-babel-evaluate nil)          \
              (org-babel-load-file \"$$test_lib\")           \
              (ert-run-tests-batch-and-exit '(and \"$(TESTS)\" (not (tag :interactive)))))" || exit 1; \
	done)

download-org :
	$(CURL) '$(ORG_URL)' > '$(WORK_DIR)/$(ORG_TAR)'
	tar xzf $(WORK_DIR)/$(ORG_TAR)
	rm $(WORK_DIR)/$(ORG_TAR)

download-xml-rpc :
	$(CURL) '$(XML_RPC_URL)' > '$(WORK_DIR)/$(XML_RPC).el'

download-metaweblog :
	$(CURL) '$(METAWEBLOG_URL)' > '$(WORK_DIR)/$(METAWEBLOG).el'

download-deps : download-xml-rpc download-metaweblog download-org

clean :
	rm -rf $(TEST_DIR)/*.el

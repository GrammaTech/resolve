.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = resolve
DOC_PACKAGES =				\
		resolve/alist		\
		resolve/ast-diff	\
		resolve/commands	\
		resolve/html		\
		resolve/string

LISP_DEPS =				\
	$(wildcard *.lisp)

BINS = ast-diff ast-merge auto-merge

BIN_TEST_DIR = test/bin
BIN_TESTS =					\
	ast-diff-on-json-has-lines		\
	clang-diff-on-gcd			\
	java-diff-on-gcd			\
	javascript-diff-on-gcd			\
	lisp-diff-on-gcd			\
	lisp-diff-on-gcd-raw			\
	clang-diff-on-gcd-edit-tree		\
	clang-diff-on-gcd-edit-tree-coherence	\
	octomap-diff-count			\
	octomap-diff-edit-tree			\
	ast-merge-json-has-conflict

# TODO: Fix these failing tests.
# ast-diff-on-simple-has-lines
# octomap-diff

include cl.mk

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
	$(wildcard *.lisp)		\
	$(wildcard software/*.lisp)

BINS = ast-diff ast-merge auto-merge serve-ast-diff serve-auto-merge

BIN_TEST_DIR = test/bin
BIN_TESTS =						\
	ast-diff-on-json-has-lines			\
	ast-diff-on-lisp-has-lines			\
	ast-diff-on-simple-has-lines			\
	clang-diff-on-gcd				\
	javascript-diff-on-gcd				\
	lisp-diff-on-gcd				\
	lisp-diff-on-gcd-raw				\
	clang-diff-on-gcd-edit-tree			\
	clang-diff-on-gcd-edit-tree-coherence		\
	octomap-diff					\
	octomap-diff-count				\
	octomap-diff-edit-tree				\
	ast-merge-json-has-conflict			\
	can-auto-merge-gcd-single-file			\
	can-auto-merge-gcd-single-file-evolve		\
	can-auto-merge-gcd-project			\
	can-auto-merge-gcd-project-evolve		\
	can-auto-merge-gcd-project-insert-file-evolve	\
	can-auto-merge-gcd-project-delete-file-evolve	\
	can-auto-merge-text-single-file			\
	can-auto-merge-text-single-file-evolve		\
	can-auto-merge-text-project			\
	can-auto-merge-text-project-evolve		\
	can-auto-merge-text-project-insert-file-evolve	\
	can-auto-merge-text-project-delete-file-evolve	\

# REST tests are only executed for SBCL builds.
# CCL reports an error on rest server startup related to the use of
# "#$O_NONBLOCK" in the cl-plus-ssl library at
# https://github.com/cl-plus-ssl/cl-plus-ssl/blame/master/src/streams.lisp#L180.
# This issue may be related to an incorrect installation of CCL; see also
# https://lists.clozure.com/pipermail/openmcl-devel/2011-January/008283.html.
# For now, we are punting and disabling these tests on CCL builds.
ifneq (,$(findstring sbcl, $(LISP)))
BIN_TESTS +=					\
	rest-ast-diff-returns-json		\
	rest-ast-diff-takes-full-strings	\
	rest-ast-diff-can-return-and-serve-link
endif

include .cl-make/cl.mk

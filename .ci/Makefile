# A Makefile used to run the Nightly tests inside of GrammaTech.
PROJECT=resolve
TEST_DIR=test/bin/
TESTS=	ast-diff-on-json-has-lines		\
	ast-merge-json-has-conflict		\
	clang-diff-on-gcd			\
	clang-diff-on-gcd-edit-tree		\
	clang-diff-on-gcd-edit-tree-coherence	\
	java-diff-on-gcd			\
	javascript-diff-on-gcd			\
	lisp-diff-on-gcd			\
	lisp-diff-on-gcd-raw			\
	octomap-diff-count			\
	octomap-diff-edit-tree

all: check

QUICK_LISP ?= /root/quicklisp/

include $(QUICK_LISP)/local-projects/testbot/testbot.mk

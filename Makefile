.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = auto-merge
DOC_PACKAGES =			\
		auto-merge/core	\
		auto-merge/auto-merge

LISP_DEPS =				\
	$(wildcard *.lisp)

BINS = auto-merge

include cl.mk

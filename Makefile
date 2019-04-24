.PHONY: doc api

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

PACKAGE_NAME = resolve
DOC_PACKAGES =			\
		resolve/core	\
		resolve/resolve

LISP_DEPS =				\
	$(wildcard *.lisp)

BINS = resolve

include cl.mk

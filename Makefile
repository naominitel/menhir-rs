# This is the main Makefile that is shipped as part of the source package.

# Keep in mind that the hierarchy that is shipped is not identical to the
# hierarchy within the git repository. Some sub-directories are not shipped.
# The documentation (manual.pdf, menhir.1) is pre-built and stored at the root.

# This Makefile can also be used directly in the repository. In that case,
# the documentation and demos are not installed.

# The hierarchy that is shipped includes:
#   demos
#   menhir.1
#   manual.pdf
#   src
#   Makefile (this one)

# ----------------------------------------------------------------------------

# The following variables must/can be configured.

ifndef PREFIX
  $(error Please define PREFIX)
endif

ifndef TARGET
  TARGET := native
endif

# ----------------------------------------------------------------------------

# By default, we attempt to use ocamlfind (if present in the PATH), but it
# is possible to prevent that externally by setting USE_OCAMLFIND to false.

# USE_OCAMLFIND is used only at build time (i.e., by "make all"). At
# (un)installation time, instead, we query menhir using --suggest-ocamlfind.
# This should protect us against people who pass USE_OCAMLFIND at build time
# and forget to pass it at (un)installation time.

ifndef USE_OCAMLFIND
  USE_OCAMLFIND = ocamlfind ocamlc -v >/dev/null 2>&1
endif

# ----------------------------------------------------------------------------
# Installation paths.
# These may be overridden from outside; e.g., our opam package description
# provides its own values of docdir, libdir, and mandir.

bindir          := $(PREFIX)/bin
docdir		:= $(PREFIX)/share/doc/menhir
libdir	        := $(PREFIX)/share/menhir
mandir          := $(PREFIX)/share/man/man1
MANS            := menhir.1
DOCS            := manual.pdf demos
MLYLIB          := src/standard.mly

# ----------------------------------------------------------------------------

# A few settings differ on Windows versus Unix.

# If the compiler is MSVC, then the name of the executable file ends in .exe,
# and object file names end in .obj instead of .o.

ifneq (,$(shell ocamlc -config | grep -E "ccomp_type: msvc"))
  MENHIREXE    := menhir.exe
  OBJ          := obj
# LIBSUFFIX    := lib
else
  MENHIREXE    := menhir
  OBJ          := o
# LIBSUFFIX    := a
endif

# The path $(installation_libdir), which is recorded in src/installation.ml (see
# below), must sometimes be translated using cygpath.

# This one is tricky. To summarize, if I understood correctly, we can assume
# that Cygwin always exists when Menhir is compiled and installed (because
# executing a Makefile, like this one, requires Cygwin), but we cannot assume
# that Menhir will be executed under Cygwin. If the OCaml compiler is
# configured to produce a Cygwin executable, then, yes, Cygwin is there at
# execution time, so path translation is not necessary (and should not be
# performed). On the other hand, if the OCaml compiler is configured to
# produce a native Windows executable, then Cygwin is not there at execution
# time and path translation is required. In summary, path translation must be
# performed if "os_type" is "Win32" or "Win64", and must not be performed if
# "os_type" is "Cygwin" or "Unix".

ifneq (,$(shell ocamlc -config | grep -E "os_type: (Win32|Win64)"))
installation_libdir := $(shell cygpath -m $(libdir))
else
installation_libdir := $(libdir)
endif

# -------------------------------------------------------------------------

# The names of the modules in MenhirLib are obtained by reading the
# non-comment lines in menhirLib.mlpack.

MENHIRLIB_MODULES := $(shell grep -ve "^[ \t\n\r]*\#" src/menhirLib.mlpack)

# ----------------------------------------------------------------------------

# The directory where things are built.

BUILDDIR := src/_stage2

# ----------------------------------------------------------------------------

# Compilation.

.PHONY: all install uninstall

all:
# Installation time settings are recorded within src/installation.ml.
# This file is recreated every time so as to avoid becoming stale.
	@ rm -f src/installation.ml
	@ echo "let libdir = \"$(installation_libdir)\"" > src/installation.ml
	@ if $(USE_OCAMLFIND) ; then \
	  echo "let ocamlfind = true" >> src/installation.ml ; \
	else \
	  echo "let ocamlfind = false" >> src/installation.ml ; \
	fi
# Compile the library modules and the Menhir executable.
	@ $(MAKE) -C src library bootstrap
# The source file menhirLib.ml is created by concatenating all of the source
# files that make up MenhirLib. This file is not needed to compile Menhir or
# MenhirLib. It is installed at the same time as MenhirLib and is copied by
# Menhir when the user requests a self-contained parser (one that is not
# dependent on MenhirLib).
	@ echo "Creating menhirLib.ml"
	@ rm -f $(BUILDDIR)/menhirLib.ml
	@ for m in $(MENHIRLIB_MODULES) ; do \
	  echo "module $$m = struct" >> $(BUILDDIR)/menhirLib.ml ; \
	  cat src/$$m.ml >> $(BUILDDIR)/menhirLib.ml ; \
	  echo "end" >> $(BUILDDIR)/menhirLib.ml ; \
	done
# The source file menhirLib.mli is created in the same way. If a module
# does not have an .mli file, then we assume that its .ml file contains
# type (and module type) definitions only, so we copy it instead of the
# (non-existent) .mli file.
	@ echo "Creating menhirLib.mli"
	@ rm -f $(BUILDDIR)/menhirLib.mli
	@ for m in $(MENHIRLIB_MODULES) ; do \
	  echo "module $$m : sig" >> $(BUILDDIR)/menhirLib.mli ; \
	  if [ -f src/$$m.mli ] ; then \
	    cat src/$$m.mli >> $(BUILDDIR)/menhirLib.mli ; \
	  else \
	    cat src/$$m.ml >> $(BUILDDIR)/menhirLib.mli ; \
	  fi ; \
	  echo "end" >> $(BUILDDIR)/menhirLib.mli ; \
	done

# -------------------------------------------------------------------------

# The files that should be installed as part of menhirLib.

MENHIRLIB       := menhirLib.mli menhirLib.ml menhirLib.cmi menhirLib.cmo
ifneq ($(TARGET),byte)
MENHIRLIB       := $(MENHIRLIB) menhirLib.cmx menhirLib.$(OBJ)
endif

# ----------------------------------------------------------------------------
# Installation.

install:
# Install the executable.
	mkdir -p $(bindir)
	install $(BUILDDIR)/menhir.$(TARGET) $(bindir)/$(MENHIREXE)
# Install the library.
	mkdir -p $(libdir)
	install -m 644 $(MLYLIB) $(libdir)
	@if `$(BUILDDIR)/menhir.$(TARGET) --suggest-ocamlfind` ; then \
	  echo Installing MenhirLib via ocamlfind. ; \
	  ocamlfind install menhirLib src/META $(patsubst %,$(BUILDDIR)/%,$(MENHIRLIB)) ; \
	else \
	  echo Installing MenhirLib manually. ; \
	  install -m 644 $(patsubst %,$(BUILDDIR)/%,$(MENHIRLIB)) $(libdir) ; \
	fi
	install src/libmenhir_runtime.rlib $(libdir)
# Install the documentation, if it has been built.
	if [ -f manual.pdf ] ; then \
	  mkdir -p $(docdir) ; \
	  mkdir -p $(mandir) ; \
	  cp -r $(DOCS) $(docdir) ; \
	  cp -r $(MANS) $(mandir) ; \
	fi

uninstall:
	@if `$(bindir)/$(MENHIREXE) --suggest-ocamlfind` ; then \
	  echo Un-installing MenhirLib via ocamlfind. ; \
	  ocamlfind remove menhirLib ; \
	fi
	rm -rf $(bindir)/$(MENHIREXE)
	rm -rf $(libdir)
	rm -rf $(docdir)
	rm -rf $(mandir)/$(MANS)

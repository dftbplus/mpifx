############################################################################
#
# Makefile to build the library.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

ROOT = ..

SRCDIR = $(ROOT)/src

include ../make.arch

.PHONY: _FORCED_SUBMAKE_
_FORCED_SUBMAKE_:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
            SRCDIR="$(SRCDIR)" -f Makefile.lib

.PHONY: clean distclean
clean distclean:
	$(MAKE) SRCDIR="$(SRCDIR)" -f Makefile.lib $@

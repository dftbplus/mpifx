############################################################################
#
# Makefile to build the library.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

ROOT = ..

include ../make.arch

.PHONY: _FORCED_SUBMAKE_
_FORCED_SUBMAKE_:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
            -f Makefile.lib

.PHONY: clean distclean
clean distclean:
	$(MAKE) -f Makefile.lib $@

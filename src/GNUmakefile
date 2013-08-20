############################################################################
#
# Makefile to build the library.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

include ../make.arch

libmpifx.a:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            M4="$(M4)" M4OPT="$(M4OPT)" \
            -f Makefile.lib

.PHONY: clean realclean
clean:
	$(MAKE) -f Makefile.lib clean

realclean: clean
	$(MAKE) -f Makefile.lib realclean

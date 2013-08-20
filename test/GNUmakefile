############################################################################
#
# Makefile to demonstrate, how to incorporate the library makefile from
# an external makefile by passing the appropriate variables.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

# Directory where library source can be found
SRCDIR_MPIFX = ../src

include ../make.arch


############################################################################
# Building the test programs.
#
# You can replace this part with your projects makefile. Make sure, that
# you introduce at least one dependency on the library file (see below).
############################################################################

.SUFFIXES:
.SUFFIXES: .f90 .F90 .o .m4

BINARIES = test_bcast test_send_recv

all: $(BINARIES)


test_bcast: test_bcast.o
	$(LN) $(LNOPT) -o $@ $^ -L./ -lmpifx

test_send_recv: test_send_recv.o
	$(LN) $(LNOPT) -o $@ $^ -L./ -lmpifx



%.o: %.f90
	$(FXX) $(FXXOPT) -c $<

.PHONY: clean realclean
clean:
	$(MAKE) -f $(SRCDIR_MPIFX)/Makefile.lib clean
	rm -f *.mod *.o

realclean: clean
	$(MAKE) -f $(SRCDIR_MPIFX)/Makefile.lib realclean
	rm -f $(BINARIES)


# Dependencies: test programs can only be compiled after library is done as
# the compiler needs the .mod files
test_bcast.o: libmpifx.a
test_send_recv: libmpifx.a


############################################################################
# Invoking the makefile of the library to build it in place.
############################################################################
libmpifx.a:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            M4="$(M4)" M4OPT="-I $(SRCDIR_MPIFX) $(M4OPT)" \
	    VPATH="$(SRCDIR_MPIFX)" \
            -f "$(SRCDIR_MPIFX)/Makefile.lib"

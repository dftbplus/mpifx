############################################################################
#
# Makefile to demonstrate, how to incorporate the library makefile from
# an external makefile by passing the appropriate variables.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

# Directory where library source can be found
SRCDIR = ../src

include ../make.arch


############################################################################
# Building the test programs.
#
# You can replace this part with your projects makefile. Make sure, that
# you introduce at least one dependency on the library file (see below).
############################################################################

.SUFFIXES:
.SUFFIXES: .f90 .F90 .o .m4

BINARIES = test_bcast test_send_recv test_comm_split test_reduce \
    test_allreduce test_gather test_allgather test_scatter

all: $(BINARIES)

# General rule for executables (without suffix)
%: %.o
	$(LN) $(LNOPT) -o $@ $^ -L./ -lmpifx


%.o: %.f90
	$(FXX) $(FXXOPT) -c $<

.PHONY: clean realclean
clean:
	$(MAKE) -f $(SRCDIR)/Makefile.lib clean
	rm -f *.mod *.o

realclean: clean
	$(MAKE) -f $(SRCDIR)/Makefile.lib realclean
	rm -f $(BINARIES)


# Dependencies: test programs can only be compiled after library is done as
# the compiler needs the .mod files
$(BINARIES): libmpifx.a


############################################################################
# Invoking the makefile of the library to build it in place.
############################################################################
libmpifx.a:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            M4="$(M4)" M4OPT="-I $(SRCDIR) $(M4OPT)" \
	    VPATH="$(SRCDIR)" \
            -f "$(SRCDIR)/Makefile.lib"

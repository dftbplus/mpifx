############################################################################
#
# Makefile to demonstrate, how to incorporate the library makefile from
# an external makefile by passing the appropriate variables.
#
# Edit "../make.arch" to adapt it to your system.
#
############################################################################

include ../make.arch

# Root directory
ROOT = ..

# Directory where library source can be found
SRCDIR = $(ROOT)/src

############################################################################
# Building the test programs.
#
# You can replace this part with your projects makefile. Make sure, that
# you introduce at least one dependency on the library file (see below).
############################################################################

.SUFFIXES:
.SUFFIXES: .f90 .o

TARGETS = test_bcast test_send_recv test_comm_split test_reduce \
    test_allreduce test_gather test_allgather test_scatter

all: $(TARGETS)

# Create dependencies (make sure every targets .o file additionally depends on
# the external libary, as the library modfiles must been created first)
include Makefile.dep
$(TARGETS:=.o): _extlib_mpifx

# Include linking rules for targets
define link-target
$(LN) $(LNOPT) -o $@ $(filter-out _%,$^) -L$(SRCDIR) -lmpifx
endef
include Makefile.targets

%.o: %.f90
	$(FXX) $(FXXOPT) -I$(SRCDIR) -c $<

.PHONY: clean realclean
clean:
	$(MAKE) -C $(SRCDIR) -f Makefile.lib clean
	rm -f *.mod *.o _*

distclean: clean
	$(MAKE) -C $(SRCDIR) -f Makefile.lib distclean
	rm -f $(TARGETS)


############################################################################
# Invoking the makefile of the library to build it in its directory
############################e################################################
.PHONY: _FORCED_SUBMAKE_

_extlib_mpifx: _FORCED_SUBMAKE_
	touch -r $(SRCDIR)/libmpifx.a $@

_FORCED_SUBMAKE_:
	$(MAKE) \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" \
            LN="$(LN)" LNOPT="$(LNOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" \
	    -C $(SRCDIR) -f Makefile.lib

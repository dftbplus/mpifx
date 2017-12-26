
ROOT := $(PWD)

.PHONY: all
all: lib

include $(ROOT)/make.arch

.PHONY: lib
lib:
	mkdir -p $(BUILDDIR)/lib
	$(MAKE) -C $(BUILDDIR)/lib ROOT=$(ROOT) SRCDIR=$(ROOT)/lib \
            FXX="$(FXX)" FXXOPT="$(FXXOPT)" LN="$(LN)" LNOPT="$(LNOPT)" \
            FYPP="$(FYPP)" FYPPOPT="$(FYPPOPT)" -f $(ROOT)/lib/make.build

.PHONY: install
install: lib
	mkdir -p $(INSTALLDIR)/lib
	cp $(BUILDDIR)/lib/*.a $(INSTALLDIR)/lib
	mkdir -p $(INSTALLDIR)/include
	cp $(BUILDDIR)/lib/*.mod $(INSTALLDIR)/include

.PHONY: test
test: lib
	mkdir -p $(BUILDDIR)/test
	$(MAKE) -C $(BUILDDIR)/test ROOT=$(ROOT) BUILDROOT=$(BUILDDIR) \
            -f $(ROOT)/test/make.build


.PHONY: distclean
distclean:
	rm -rf $(BUILDDIR)

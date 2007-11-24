include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags`

.f90.exe: m_handlers.$(OBJEXT)
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) $(LINK_O_FLAG) $@ $< m_handlers.$(OBJEXT) `../../FoX-config --libs --sax`

check: clean m_handlers.$(OBJEXT)
	./run_tests.sh
clean:
	rm -f *.$(OBJEXT) *.exe

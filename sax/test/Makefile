include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags --sax`

.f90.exe: ../libFoX_wxml.a m_handlers.o
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ $< m_handlers.o `../../FoX-config --libs --sax`
	
clean:
	rm -f *.o *.exe

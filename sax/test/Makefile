include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags --sax`

.f90.exe: ../libFoX_wxml.a
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ $< `../../FoX-config --libs --sax`
	
clean:
	rm -f *.o *.exe

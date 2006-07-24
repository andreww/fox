include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags --wxml`

.f90.exe:
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ $< `../../FoX-config --libs --wxml`
	
clean:
	rm -f *.o *.exe

include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags --wkml`

.f90.exe: ../libFoX_wkml.a
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ $< `../../FoX-config --libs --wkml`

clean:
	rm -f *.o *.exe

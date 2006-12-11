include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags --wcml`

.f90.exe: ../libFoX_wcml.a
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ $< `../../FoX-config --libs --wcml`

clean:
	rm -f *.o *.exe

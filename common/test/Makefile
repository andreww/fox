include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags`

test_str.exe: test_str.f90
	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) -o $@ test_str.f90 `../../FoX-config --libs --common`
	
clean:
	rm -f *.o *.exe

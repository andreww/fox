include ../../arch.make

INCFLAGS=`../../FoX-config --fcflags`

NetCDF_FCFLAGS=-I/usr/include
# Need both the fortran and C shared libs (in /usr/lib, so don't need -L)
NetCDF_LIBS=-lnetcdff -lnetcdf

# When we are hooked up to FoX!
# .f90.exe:
# 	$(FC) $(FFLAGS) $(INCFLAGS) $(LDFLAGS) $(FCFLAGS_free_f90) $(LINK_O_FLAG) $@ $< `../../FoX-config --libs --wncml`

.f90.exe:
	$(FC) $(FFLAGS) -I.. $(INCFLAGS) $(NetCDF_FCFLAGS) $(LDFLAGS) $(NetCDF_LIBS) $(FCFLAGS_free_f90) $(LINK_O_FLAG) $@ $< ../m_wncml_xml.o ../m_wncml_util.o `../../FoX-config --libs ` 


check: clean
	./run_tests.sh

clean:
	rm -f *.$(OBJEXT) *.exe

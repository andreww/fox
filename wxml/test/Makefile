include ../../arch.make

INCFLAGS=$(MOD_PREFIX)../../objs/finclude

.o.exe:
	$(FC) $(LDFLAGS) -o $@ $< `sh ../../xmlf90-config.sh --libs`
	
clean:
	rm -f *.o *.exe

FC = f95

default: ncdfcat

NetCDF_FCFLAGS=-I/usr/include
# Need both the fortran and C shared libs (in /usr/lib, so don't need -L)
NetCDF_LIBS=-lnetcdff -lnetcdf


# FoX whatnot
#
FoXloc=../
FoX_configured=$(FoXloc)/.config
FoX_built=$(FoXloc)/.FoX

# This is how we pick up modules and libraries for FoX:
FoX_FCFLAGS=`$(FoXloc)/FoX-config --fcflags`
FoX_LIBS=`$(FoXloc)/FoX-config --libs --wxml`

$(FoX_configured):
	(cd $(FoXloc); FC="$(FC)" ./configure; touch .config)

$(FoX_built): $(FoX_configured)
	(cd $(FoXloc); $(MAKE))

distclean:
	$(MAKE) clean
	rm text.xml
	(cd $(FoXloc); make distclean)

clean:
	rm *.o *.mod ncdfcat 

wncml.o: wncml.f90 m_wncml_xml.o m_wncml_util.o $(FoX_built) 
	$(FC) $(FoX_FCFLAGS) $(NetCDF_FCFLAGS) -c wncml.f90 

m_wncml_xml.o: m_wncml_xml.f90 m_wncml_util.o  $(FoX_built) 
	$(FC) $(FoX_FCFLAGS) -c m_wncml_xml.f90 

m_wncml_util.o: m_wncml_util.f90 
	$(FC) $(NetCDF_FCFLAGS) -c m_wncml_util.f90 

ncdfcat.o: ncdfcat.f90 wncml.o $(FoX_built) 
	$(FC) $(FoX_FCFLAGS) $(NetCDF_FCFLAGS) -c ncdfcat.f90 

ncdfcat: ncdfcat.o wncml.o
	$(FC) -o ncdfcat m_wncml_xml.o m_wncml_util.o wncml.o  ncdfcat.o $(NetCDF_LIBS) $(FoX_LIBS)


F90=f95
FFLAGS=-g -gline -C=all -I../dom -I../sax -I../wxml -I../fsys
LDFLAGS=-L../dom/libxmlf90_dom.a -L../sax/libxmlf90_sax.a -L../wxml/libxmlf90_sax.a -L../fsys/libfsys.a
INCFLAGS=-I../dom -I../sax -I../wxml -I../fsys

include ../arch.make

xmlf90_xpath.o: m_xpath_number.o m_xpath_string.o m_xpath_boolean.o m_xpath_nodeset.o m_xpath_types.o m_xpath_error.o xpathparser.tab.o tokenizer.o

m_xpath_number.o: m_xpath_types.o m_xpath_error.o
m_xpath_string.o: m_xpath_types.o m_xpath_error.o
m_xpath_boolean.o: m_xpath_types.o m_xpath_error.o
m_xpath_nodeset.o: m_xpath_types.o m_xpath_error.o

testParser: testparser.o m_xpath_types.o
	$(F90) $(FFLAGS) $(LDFLAGS) -o testParser testparser.o m_xpath_types.o xpathparser.tab.o tokenizer.o

testparser.o: testparser.f90 xpathparser.tab.o

tokenizer.o: tokenizer.f90 tokenizer.h m_xpath_types.o

testTokenizer: testtokenizer.o m_xpath_types.o tokenizer.o
	$(F90) $(FFLAGS) -o testTokenizer testtokenizer.o m_xpath_types.o tokenizer.o

testtokenizer.o: testtokenizer.f90 m_xpath_types.o tokenizer.o

xpathparser.tab.o: xpathparser.tab.f90 tokenizer.o

xpathparser.tab.f90: xpathparser.y
	~/devel/bison/bison--fortran--1.16--base-0/bison -f xpathparser.y

clean:
	rm -f testTokenizer testParser *.mod *.o

#  ---------------------------------------------------------------
#  Makefile
#
#  Atul Varma
#  Python C Compiler - Makefile
#  $Id: Makefile,v 1.8 2004/06/02 21:11:57 varmaa Exp $
#
#  This just makes all the sample code and lets you clean up
#  intermediate/output files.
#  ---------------------------------------------------------------

FLAGS=-annotate -ast
PYTHON=python

compile-samples:
	${PYTHON} c.py samples/foo.c samples/foo_lib.c ${FLAGS}
	gcc samples/foo.s samples/foo_lib.s -o samples/foo

clean:
	rm -f parsetab.py parser.out *.pyc samples/*.ast \
              samples/*.s samples/*.exe samples/foo

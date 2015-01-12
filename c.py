#  ---------------------------------------------------------------
#  c.py
#
#  Atul Varma
#  Python C Compiler - Front-end
#  $Id: c.py,v 1.3 2004/05/27 17:52:19 varmaa Exp $
#
#  This is the main program for the compiler, which just parses
#  command-line options, figures out which source files to read
#  and write to, and invokes the different stages of the
#  compiler proper.
#  ---------------------------------------------------------------

import yacc

import cparse, cvisitors, cx86

import sys

class Compiler:
    """This object encapsulates the front-end for the compiler and
    serves as a facade interface to the 'meat' of the compiler
    underneath."""
    
    class CompileError(Exception):
        """Exception raised when there's been a compilation error."""

        pass

    def __init__(self):
        self.total_errors = 0
        self.total_warnings = 0

    def _parse(self):
        """Parses the source code."""
        self.ast = yacc.parse(self.code)

    def _compile_phase(self, visitor):
        """Applies a visitor to the abstract syntax tree."""
        
        visitor.visit(self.ast)
        self.total_errors += visitor.errors
        self.total_warnings += visitor.warnings
        if visitor.has_errors():
            raise Compiler.CompileError()

    def _do_compile(self, outfile, ast_file, show_comments):
        """Compiles the code to the given file object.  Enabling
        show_ast prints out the abstract syntax tree."""
        
        self._parse()
        self._compile_phase(cvisitors.SymtabVisitor())
        self._compile_phase(cvisitors.TypeCheckVisitor())
        self._compile_phase(cvisitors.FlowControlVisitor())
        self._compile_phase(cx86.CodeGenVisitor(outfile,
                                                show_comments))
        if ast_file != None:
            self._compile_phase(cvisitors.ASTPrinterVisitor(ast_file))

    def _print_stats(self):
        """Prints the total number of errors/warnings from compilation."""
        
        print "%d errors, %d warnings." % (self.total_errors, self.total_warnings)

    def compile(self, code, outfile, show_ast, show_comments):
        """Compiles the given code string to the given file object."""

        self.code = code
        try:
            self._do_compile(outfile, show_ast, show_comments)
        except cparse.ParseError:
            print "Errors encountered, bailing."
            return 1            
        except Compiler.CompileError:
            self._print_stats()
            print "Errors encountered, bailing."
            return 1
        self._print_stats()
        print "Compile successful."
        return 0

def run_compiler():
    """Runs the command-line compiler."""
    
    if len(sys.argv) < 2:
        print "Usage: c.py <source-file-1> [[source-file-2] ...] [-ast] [-annotate]"
        sys.exit(1)

    show_ast = 0
    show_comments = 0

    params = sys.argv[1:]
    files = sys.argv[1:]

    for param in params:
        if param[0] == '-':
            if param == '-ast':
                show_ast = 1
            elif param == '-annotate':
                print "Annotated assembly generation enabled."
                show_comments = 1
            else:
                print "Unknown option: %s" % param
                sys.exit(1)
            files.remove(param)

    for file in files:
        source_filename = file
        dest_filename = file[:-2]+'.s'
        print "Compiling %s -> %s." % (source_filename, dest_filename)
        open_files = []
        ast_file = None
        if show_ast:
            ast_filename = file[:-2]+'.ast'
            print "Outputting AST to %s." % ast_filename
            ast_file = open(ast_filename, 'w')
            open_files.append(ast_file)
        source = open(source_filename, 'r')
        code = source.read()
        source.close()
        dest = open(dest_filename, 'w')
        open_files.append(dest)
        retval = Compiler().compile(code, dest, ast_file, show_comments)
        for f in open_files:
            f.close()
        if retval != 0:
            sys.exit(retval)
        print

    sys.exit(retval)

if __name__ == '__main__':
    run_compiler()

#  ---------------------------------------------------------------
#  End of c.py
#  ---------------------------------------------------------------

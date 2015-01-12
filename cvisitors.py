#  ---------------------------------------------------------------
#  cvisitors.py
#
#  Atul Varma
#  Python C Compiler - Visitors
#  $Id: cvisitors.py,v 1.3 2004/05/27 17:51:47 varmaa Exp $
#
#  The Visitor is a pattern outlined in "Design Patterns" by
#  Gamma et al., used here to encapsulate different parts of parsing 
#  and compilation into separate classes via a mechanism called 
#  double dispatching.
#
#  In this compiler, the yacc grammar rules in cparse.py just create
#  the abstract syntax tree, and visitors do the bulk of parsing
#  and compilation.
#  ---------------------------------------------------------------

# TODO: make it so functions can return void.
# TODO: mark all statements with an 'ignore return value' flag
#       to enable some optimizations if the statement is an
#       expression.
# TODO: move extern, static indicators in functions to their
#       Type object, maybe.
#
# Possible things to do:
#   Add compilation to JVM/python bytecode/z-machine...
#   Implement arrays
#   Pass line numbers to constructors for nodes
#
# Faults so far:
#   * doesn't check for variable initialization before use.
#   * const number ranges aren't being checked.

import cparse

class Visitor:
    """The base visitor class.  This is an abstract base class."""

    def __init__(self):
        self.warnings = 0
        self.errors = 0

    def _visitList(self, list):
        """Visit a list of nodes.  'list' should be an actual list,
        not a cparse.NodeList object."""
        
        last = None
        for i in list:
            last = i.accept(self)
        return last
    
    def visit(self, node):
        """Visits the given node by telling the node to call the
        visitor's class-specific visitor method for that node's
        class (i.e., double dispatching)."""
        
        return node.accept(self)

    def warning(self, str):
        """Output a non-fatal compilation warning."""
        
        print "warning: %s" % str
        self.warnings += 1

    def error(self, str):
        """Output a fatal compilation error."""
        
        print "error: %s" % str
        self.errors += 1

    def has_errors(self):
        """Returns whether the visitor has encountered any
        errors."""
        
        return self.errors > 0

#  ---------------------------------------------------------------
#  ABSTRACT SYNTAX TREE PRINTER (for debugging)
#  ---------------------------------------------------------------

class ASTPrinterVisitor(Visitor):
    """Simple visitor that outputs a textual representation of
    the abstract syntax tree, for debugging purposes, to an
    output file."""
    
    def __init__(self, ast_file, indent_amt=2):
        self.ast_file = ast_file
        Visitor.__init__(self)
        self._indent = 0
        self._indent_amt = indent_amt

    def indent(self):
        self._indent += self._indent_amt

    def unindent(self):
        self._indent -= self._indent_amt

    def p(self, str):
        self.ast_file.write(
            (' ' * (self._indent_amt * self._indent) ) + str + "\n" )

    def pNodeInfo(self, node):
        # Print out the name of the node's class.
        self.p('+ ' + node.__class__.__name__)

        # If the node has a type associated with it,
        # print the string of the type.
        if node.__dict__.has_key("type"):
            self.p("  Type-string: %s" % node.type.get_string())

        # Find all attributes of the node that are ints or
        # strings and aren't 'private' (i.e., don't begin with
        # '_'), and print their values.
        for key in node.__dict__.keys():
            if key[0] == '_':
                continue
            val = node.__dict__[key]
            if (isinstance(val, str) or
                isinstance(val, int)):
                self.p("  %s: %s" % (key, str(val)))

    def pSubnodeInfo(self, subnode, label):
        if not subnode.is_null():
            self.p("  %s:" % label)
            self.indent()
            subnode.accept(self)
            self.unindent()

    def vNullNode(self, node):
        self.pNodeInfo(node)

    def vArrayExpression(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.expr, "Expression")
        self.pSubnodeInfo(node.index, "Index")

    def vStringLiteral(self, node):
        self.pNodeInfo(node)
        self.p('  Value: "%s"' % node.get_sanitized_str())

    def vId(self, node):
        self.pNodeInfo(node)

    def vUnaryop(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.expr, "Expression")

    def vFunctionExpression(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.function, "Function")
        self.pSubnodeInfo(node.arglist, "Arguments")

    def vConst(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.type, "Type")

    def vBinop(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.left, "Left operand")
        self.pSubnodeInfo(node.right, "Right operand")

    def vNodeList(self, node):
        self.pNodeInfo(node)
        self.indent()
        self._visitList(node.nodes)
        self.unindent()

    def vCompoundStatement(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.declaration_list, "Declaration list")
        self.pSubnodeInfo(node.statement_list, "Statement list")        

    def vBaseType(self, node):
        self.pNodeInfo(node)

    def vFunctionType(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.params, "Parameters:")
        self.pSubnodeInfo(node.child, "Child:")

    def vPointerType(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.child, "Child:")

    def vDeclaration(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.type, "Type")

    def vReturnStatement(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.expr, "Expression")

    def vFunctionDefn(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.type, "Type")
        self.pSubnodeInfo(node.body, "Body")

    def vIfStatement(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.expr, "Expression")
        self.pSubnodeInfo(node.then_stmt, "Then statement")
        self.pSubnodeInfo(node.else_stmt, "Else statement")

    def vWhileLoop(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.expr, "Expression")
        self.pSubnodeInfo(node.stmt, "Statement")

    def vForLoop(self, node):
        self.pNodeInfo(node)
        self.pSubnodeInfo(node.begin_stmt, "Begin statement")
        self.pSubnodeInfo(node.expr, "Test expression")
        self.pSubnodeInfo(node.end_stmt, "End statement")
        self.pSubnodeInfo(node.stmt, "Statement")

#  ---------------------------------------------------------------
#  SYMBOL TABLE GENERATION
#  ---------------------------------------------------------------

class Symtab:
    """A symbol table.  This is a simple object that just keeps a
    hashtable of symbol names and the Declaration or FunctionDefn
    nodes that they refer to.

    There is a separate symbol table for each code element that
    has its own scope (for instance, each compound statement will
    have its own symbol table).  As a result, symbol tables can
    be nested if the code elements are nested, and symbol table
    lookups will recurse upwards through parents to represent
    lexical scoping rules."""

    class SymbolDefinedError(Exception):
        """Exception raised when the code tries to add a symbol
        to a table where the symbol has already been defined.
        Note that 'defined' is used in the C sense here--i.e.,
        'space has been allocated for the symbol', as opposed
        to a declaration."""

        pass

    class SymbolConflictError(Exception):
        """Exception raised when the code tries to add a
        symbol to a tamble where the symbol already exists
        and its type differs from the previously existing
        one."""
        
        pass

    def __init__(self, parent=None):
        """Creates an empty symbol table with the given
        parent symbol table."""
        
        self.entries = {}
        self.parent = parent
        if self.parent != None:
            self.parent.children.append(self)
        self.children = []
    
    def add(self, name, value):
        """Adds a symbol with the given value to the symbol table.
        The value is usually an AST node that represents the
        declaration or definition of a function/variable (e.g.,
        Declaration or FunctionDefn)."""
        
        if self.entries.has_key(name):
            if not self.entries[name].extern:
                raise Symtab.SymbolDefinedError()
            elif self.entries[name].type.get_string() != \
                 value.type.get_string():
                raise Symtab.SymbolConflictError()
        self.entries[name] = value

    def get(self, name):
        """Retrieves the symbol with the given name from the symbol
        table, recursing upwards through parent symbol tables if it is
        not found in the current one."""

        if self.entries.has_key(name):
            return self.entries[name]
        else:
            if self.parent != None:
                return self.parent.get(name)
            else:
                return None

class SymtabVisitor(Visitor):
    """Visitor that creates and attaches symbol tables to the AST."""
    
    def push_symtab(self, node):
        """Pushes a new symbol table onto the visitor's symbol table
        stack and attaches this symbol table to the given node.  This
        is used whenever a new lexical scope is encountered, so the
        node is usually a CompoundStatement object."""

        self.curr_symtab = Symtab(self.curr_symtab)
        node.symtab = self.curr_symtab

    def pop_symtab(self):
        """Pops a symbol table off the visitor's symbol table stack.
        This is used whenever a new lexical scope is exited."""
        
        self.curr_symtab = self.curr_symtab.parent

    def vNode(self, node):
        pass

    def vArrayExpression(self, node):
        node.expr.accept(self)
        node.index.accept(self)

    def vFunctionExpression(self, node):
        node.function.accept(self)
        node.arglist.accept(self)
    
    def vId(self, node):
        symbol = self.curr_symtab.get(node.name)
        if symbol != None:
            node.symbol = symbol
            node.symbol.is_used = 1
            node.set_has_address()
        else:
            self.error("Line %d: Unknown identifier '%s'." % (node.lineno, node.name))

    def vUnaryop(self, node):
        node.expr.accept(self)

    def vBinop(self, node):
        node.left.accept(self)
        node.right.accept(self)

    def vNodeList(self, node):
        self._visitList(node.nodes)

    def vParamList(self, node):
        # Assign a number to each parameter.  This will later be
        # useful for the code generation phase.
        #
        # TODO: might be best to just move this to the code
        # generation phase, since this doesn't have anything to
        # do with symbol table generation.
        param_num = 0
        for param in node.nodes:
            param.accept(self)
            param.param_num = param_num
            param_num += 1

    def vTranslationUnit(self, node):
        self.root_symtab = Symtab()
        self.curr_symtab = self.root_symtab
        self.vNodeList(node)
        node.symtab = self.root_symtab
        
    def vCompoundStatement(self, node):
        self.push_symtab(node)
        node.declaration_list.accept(self)
        node.statement_list.accept(self)
        self.pop_symtab()

    def _add_symbol(self, node):
        """Attempts to add a symbol for the given node to the current
        symbol table, catching any exceptions that occur and printing
        errors if necessary."""
        
        try:
            self.curr_symtab.add(node.name, node)
        except Symtab.SymbolDefinedError:
            self.error("Symbol '%s' already defined." % node.name)
        except Symtab.SymbolConflictError:
            self.error("Symbol '%s' has multiple differing declarations." % node.name)

    def vDeclaration(self, node):
        self._add_symbol(node)

    def vReturnStatement(self, node):
        node.expr.accept(self)

    def vFunctionType(self, node):
        node.params.accept(self)

    def vFunctionDefn(self, node):
        self._add_symbol(node)
        self.push_symtab(node)
        node.type.accept(self)
        node.body.accept(self)
        self.pop_symtab()

    def vIfStatement(self, node):
        node.expr.accept(self)
        node.then_stmt.accept(self)
        node.else_stmt.accept(self)
    
    def vWhileLoop(self, node):
        node.expr.accept(self)
        node.stmt.accept(self)

    def vForLoop(self, node):
        node.begin_stmt.accept(self)
        node.expr.accept(self)
        node.end_stmt.accept(self)
        node.stmt.accept(self)

#  ---------------------------------------------------------------
#  TYPE CHECKING
#  ---------------------------------------------------------------

class TypeCheckVisitor(Visitor):
    """Visitor that performs type checking on the AST, attaching a
    Type object subclass to every eligible node and making sure these
    types don't conflict."""

    def _process_conditional(self, expr):
        """Does simple type checking for an expression that is
        supposed to be the expression for a conditional
        statement (e.g., the conditional clause of an if/then
        statement or a loop)."""
        
        if expr.type.get_outer_string() not in ['int', 'char']:
            self.error("Conditional expression doesn't evaluate to an int/char/etc.")

    def _coerce_consts(self, var1, var2):
        """Looks at two typed terminals to see if one of them
        is a constant integral.  If it is, then coerce it to
        the type of the other terminal.

        Note that both terminals cannot be constant integrals, or else
        they would have already been reduced to one node by the node's
        calculate() method in the parsing stage."""
        
        if var1.is_const():
            self._coerce_const(var1, var2.type)
        elif var2.is_const():
            self._coerce_const(var2, var1.type)

    def _coerce_const(self, var, type):
        """If the given typed terminal is a constant, coerces it to
         the given type."""
        
        if var.is_const() and type.get_string() in ['int', 'char']:
            var.type = type
            
    def _check_const_range(self, var, type):
        """Checks the given integral constant to make sure its value
        is within the bounds of the given type."""
        
        val = var.value
        type_str = type.get_outside_string()
        # TODO: implement this!
        if type_str == 'char':
            pass
        elif type_str == 'int':
            pass

    def _compare_types(self, name_str, from_type, to_type, raise_errors=1):
        """Compares the two types to see if it's possible to perform a
        binary operation on them.  If it is not, then the appropriate
        errors/warnings are raised, unless raise_errors is set to
        0."""

        WARNING = 1
        ERROR = 2
        conflict = 0
        from_str = from_type.get_string()
        to_str = to_type.get_string()
        if (from_str != to_str):
            if from_str == 'char':
                if to_str == 'int':
                    pass
                else:
                    conflict = ERROR
            elif from_str == 'int':
                if to_str == 'char':
                    conflict = WARNING
                else:
                    conflict = ERROR
            else:
                conflict = ERROR
        if not raise_errors:
            return conflict
        if conflict == WARNING:
            self.warning("%s: Conversion from %s to %s may result in data loss." % (name_str, from_str, to_str))            
        elif conflict == ERROR:
            self.error("%s: Cannot convert from %s to %s." % (name_str, from_str, to_str))

    def vNode(self, node):
        pass

    def vId(self, node):
        node.type = node.symbol.type

    def vNegative(self, node):
        node.expr.accept(self)
        node.type = node.expr.type
        # TODO: check to make sure expr is a signed type?

    def vAddrOf(self, node):
        node.expr.accept(self)
        if not node.expr.has_address():
            self.error("Address-of (&) target has no address!")
        else:
            node.expr.output_addr = 1
            node.type = cparse.PointerType(node.expr.type)

    def vPointer(self, node):
        node.expr.accept(self)
        if node.expr.type.get_outer_string() == 'pointer':
            node.type = node.expr.type.child
            node.set_has_address()
        else:
            self.error("Pointer dereference (*) target is not a pointer!")

    def vBinop(self, node):
        node.left.accept(self)
        node.right.accept(self)
        if node.op in cparse.Binop.ASSIGN_OPS:
            if not node.left.has_address():
                self.error("Invalid lvalue: not an address!")
            node.left.output_addr = 1
            self._coerce_const(node.right, node.left.type)
            # TODO: re-implement this!
            # elif node.left.symbol.is_constant:
            #    self.error("Invalid lvalue: lvalue is constant!")
            self._compare_types("Assignment", node.right.type, node.left.type)
            node.right.coerce_to_type = node.left.type
            node.type = node.left.type
        else:
            # TODO: not sure if this results in the ANSI C
            # specification for binary operand type coercion.
    
            self._coerce_consts(node.left, node.right)
            left_conflicts = self._compare_types("", node.right.type, node.left.type, raise_errors=0)
            right_conflicts = self._compare_types("", node.left.type, node.right.type, raise_errors=0)
            if left_conflicts < right_conflicts:
                from_node = node.right
                to_node = node.left
            else:
                from_node = node.left
                to_node = node.right
            self._compare_types("Binop '%s'" % node.op, from_node.type, to_node.type)
            from_node.coerce_to_type = to_node.type
            to_node.coerce_to_type = to_node.type
            node.type = to_node.type
            
    def vNodeList(self, node):
        self._visitList(node.nodes)

    def vCompoundStatement(self, node):
        node.statement_list.accept(self)

    def vReturnStatement(self, node):
        node.expr.accept(self)
        return_type = self.curr_func.type.get_return_type()
        self._coerce_const(node.expr, return_type)
        self._compare_types("Return expression", node.expr.type, return_type)
        node.expr.coerce_to_type = return_type

    def vArrayExpression(self, node):
        node.expr.accept(self)
        node.index.accept(self)
        if node.index.type.get_outer_string() not in ['int', 'char']:
            self.error("Array index is not an int or char!")
        elif node.expr.type.get_outer_string() != 'pointer':
            self.error("Array expression is not a pointer!")
        else:
            node.type = node.expr.type.child
            node.set_has_address()

    def vFunctionExpression(self, node):
        node.function.accept(self)
        if not node.function.type.is_function():
            self.error("Target of function expression is not a function!")
        node.type = node.function.symbol.type.get_return_type()
        node.arglist.accept(self)
        params = node.function.symbol.type.get_params()
        num_args = len(node.arglist.nodes)
        num_params = len(params.nodes)
        if (not params.has_ellipsis) and (num_args > num_params):
            self.error("Too many arguments passed to function.")
        elif num_args < num_params:
            self.error("Too few arguments passed to function.")
        for arg, param in zip(node.arglist.nodes, params.nodes):
            self._coerce_const(arg, param.type)
            self._compare_types("Function call argument", arg.type, param.type)
            arg.coerce_to_type = param.type
        # If this function takes a variable number of args and
        # we've got more args than required parameters, we need
        # to set some of the extra arguments' field(s) properly.
        if (params.has_ellipsis) and (num_args > num_params):
            for arg in node.arglist.nodes[num_params:]:
                arg.coerce_to_type = arg.type

    def vFunctionDefn(self, node):
        self.curr_func = node
        node.body.accept(self)

    def vIfStatement(self, node):
        node.expr.accept(self)

        self._process_conditional(node.expr)
        node.then_stmt.accept(self)
        node.else_stmt.accept(self)

    def vWhileLoop(self, node):
        node.expr.accept(self)
        self._process_conditional(node.expr)
        node.stmt.accept(self)

    def vForLoop(self, node):
        node.begin_stmt.accept(self)
        node.expr.accept(self)
        self._process_conditional(node.expr)
        node.end_stmt.accept(self)
        node.stmt.accept(self)

#  ---------------------------------------------------------------
#  FLOW CONTROL
#  ---------------------------------------------------------------

class FlowControlVisitor(Visitor):
    """Performs flow control checking on the AST.  This makes sure
    that functions return properly through all branches, that
    break/continue statements are only present within loops, and so
    forth."""
    
    def vNode(self, node):
        node.has_return_stmt = 0

    def vStatementList(self, node):
        node.has_return_stmt = 0
        for stmt in node.nodes:
            if node.has_return_stmt:
                self.warning("Function %s has at least one unreachable statement." % self.curr_func.name)
            stmt.accept(self)
            if stmt.has_return_stmt:
                node.has_return_stmt = 1

    def vTranslationUnit(self, node):
        self._visitList(node.nodes)
        
    def vWhileLoop(self, node):
        old_in_loop = self.in_loop
        self.in_loop = 1
        node.stmt.accept(self)
        self.in_loop = old_in_loop
        node.has_return_stmt = node.stmt.has_return_stmt

    def vForLoop(self, node):
        self.vWhileLoop(node)

    def vBreakStatement(self, node):
        node.has_return_stmt = 0        
        if not self.in_loop:
            self.error("Break statement outside of loop.")

    def vContinueStatement(self, node):
        node.has_return_stmt = 0        
        if not self.in_loop:
            self.error("Continue statement outside of loop.")
            
    def vIfStatement(self, node):
        node.then_stmt.accept(self)
        node.else_stmt.accept(self)
        if node.then_stmt.has_return_stmt and node.else_stmt.has_return_stmt:
            node.has_return_stmt = 1
        else:
            node.has_return_stmt = 0
            
    def vFunctionDefn(self, node):
        self.curr_func = node
        self.in_loop = 0
        node.body.accept(self)
        if not node.body.has_return_stmt:
            self.warning("Function %s doesn't return through all branches." % node.name)

    def vReturnStatement(self, node):
        node.has_return_stmt = 1

    def vCompoundStatement(self, node):
        node.statement_list.accept(self)
        node.has_return_stmt = node.statement_list.has_return_stmt

#  ---------------------------------------------------------------
#  End of cvisitors.py
#  ---------------------------------------------------------------

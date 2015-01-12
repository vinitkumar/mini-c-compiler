#  ---------------------------------------------------------------
#  cx86.py
#
#  Atul Varma
#  Python C Compiler - Intel x86 Code Generator
#  $Id: cx86.py,v 1.3 2004/06/02 21:05:23 varmaa Exp $
#  ---------------------------------------------------------------

import cparse
from cvisitors import Visitor

#  ---------------------------------------------------------------
#  CONSTANTS
#  ---------------------------------------------------------------

# Size of the 'int' type.
INT_SIZE = 4

# Size of the 'char' type.
CHAR_SIZE = 1

# The machine's word size.  Note that making this different
# from INT_SIZE may cause serious problems.
WORD_SIZE = 4

# This is a strange multiplier that needs to be used in the allocation
# of global variables for the GNU Assembler.  Not sure exactly what it
# represents.
WEIRD_MULTIPLIER = 4

#  ---------------------------------------------------------------
#  STACK MACHINE ABSTRACTION
#  ---------------------------------------------------------------

class x86Registers:
    """This class attempts to abstract the x86 registers into a stack
    machine.  Calling push() gives you a register that isn't currently
    in use by the stack machine, pop() gives you a register with the
    value of the most recently pushed element.

    Through this method the stack machine can be used to compute
    values the same way a reverse polish notation (RPN) calculator
    does.

    When push() and pop() are called, it may be the case that no
    registers are currently available; if this happens, the least
    recently used register is 'spilled' into a temporary local
    variable on the process' stack and freed for use.  Note that the
    process' stack is not to be confused with this stack machine
    abstraction--the two are completely different entities.

    Currently, push() and pop() also implement a little bit of
    implicit type conversion, so they take as parameters a cparse.Type
    object; currently conversion is done between char and int types,
    so depending on the pushed and popped types, some type conversion
    assembly code may be generated.

    Finally, an additional method, done(), should be called whenever
    the stack machine is done popping values for the current
    operation.  This is because when pop is called, the returned
    register is not immediately made 'free' for another call to pop or
    push.  If this were the case, then the following situation could
    occur:

             rightOp.calc()      # calc val of right op, put on stack
             leftOp.calc()       # calc val of left op, put on stack
             l = leftOp.pop()    # pop left val from stack
             r = rightOp.pop()   # pop right val from stack
             output('addl %s, %s' % (r, l))

    The problem with this approach is that we don't know how many
    registers will be used by leftOp's calc() method--it may use all
    the remaining registers, in which case the value that rightOp's
    calc() method put on the stack is no longer stored in a register.
    If leftOp.pop() returned register %eax and immediately marked the
    %eax register as being 'free for use', then the call to
    rightOp.pop() could very well generate code that moves rightOp's
    value from a temporary variable into %eax, thereby overwriting
    leftOp's value!

    So, instead, the pop() method places the %eax register (in this
    example) into an internal list of 'almost free' registers;
    registers that have just been returned by pop() but shouldn't be
    used by the stack machine until a call to done() is made.  The
    done() method simply moves the registers in the 'almost free' list
    over to the 'free' list."""
    
    def __init__(self, parent, base_fp):
        # A list of all registers on the machine.
        self.all_regs = ['%ebx','%esi','%edi','%eax','%ecx','%edx']

        # A list of the registers currently free.  Note that this
        # is a *copy* of the list of all registers on the machine.
        self.regs_free = self.all_regs[:]

        # A list of all the registers that are "almost" free
        # (see the docstring for this class).
        self.regs_almost_free = []

        # A list of all the temporary variable memory locations
        # that are currently unused.
        self.mem_free = []

        # A list corresponding to the actual stack of the stack
        # machine.  The item at the top of the stack is the
        # last element of this list.
        self.stack = []

        # A list that stores the Type objects of each corresponding
        # element on the stack machine's stack.  e.g., type_stack[0]
        # represents the type of the element at stack[0].
        self.type_stack = []

        # The location of the next memory location to be used for
        # temporary variables, relative to the current function's
        # frame pointer.
        self.next_temp = base_fp - WORD_SIZE

        # The parent CodeGenVisitor object of this stack machine.
        self.parent = parent

        # A list of the callee-save registers that have been used
        # so far by this function.  Once processing is finished,
        # these registers will be pushed onto the process' stack
        # at the beginning of the function and popped off just
        # before the function terminates.
        self.callee_save_regs_used = []

        # A list of the caller-save registers on the machine.
        self.caller_save_regs = ['%eax', '%ecx', '%edx']

        # A list of the callee-save registers on the machine.
        self.callee_save_regs = ['%ebx', '%esi', '%edi']

        # A list of the registers on the machine that have
        # sub-registers allowing access to their low-order bytes.
        self.byte_compat_regs = ['%eax', '%ebx', '%ecx', '%edx']

        # The default type of an element that is pushed onto
        # the stack machine without a 'type' object passed.
        self.default_type = cparse.BaseType('int')
        
    def o(self, str, comment=None):
        """Wrapper for the parent CodeGenVisitor's o() method."""
        
        self.parent.o(str, comment)

    def save_caller_saves(self):
        """Saves the caller-save registers, which should be done
        before the current function makes a function call, so that
        the registers don't get corrupted by the called function.

        Normally, this is done by pushing the caller-save registers
        onto the stack just before the function call is made and
        popping them off afterwards; however, due to the workings of
        this particular stack machine it's much easier to just move
        the contents of the caller-save registers, if they are
        currently being used, into temporary variables."""
        
        for reg in self.caller_save_regs:
            if reg not in self.regs_free:
                self._copy_reg_to_temp([reg],
                                       "Save caller-save register to temp")
                self.regs_free.append(reg)

    def save_callee_saves(self):
        """Emits code that pushes the callee-save registers used by
        the stack machine onto the process' stack."""
        
        for reg in self.callee_save_regs_used:
            self.o("  pushl %s" % reg,
                   "Save callee-save register")

    def load_callee_saves(self):
        """Emits code that pops the callee-save registers used by
        the stack machine off the process' stack."""
        
        for reg in self.callee_save_regs_used:
            self.o("  popl %s" % reg,
                   "Restore callee-save register")

    def _copy_reg_to_temp(self, valid_regs, comment_str=None):
        """Copy the least recently used register on the stack into a
        temporary variable.  The register must be in the valid_regs
        list."""
        
        # if no free temp variables exist,
        # create a new one.
        if len(self.mem_free) == 0:
            self.mem_free.append("%d(%%ebp)" % self.next_temp)
            self.next_temp -= WORD_SIZE        

        # get an unused temp var
        mem = self.mem_free.pop()
        
        # find the least recently used register on the stack
        reg = None
        index = 0
        for i in self.stack:
            if i in valid_regs:
                reg = i
                break
            index += 1
        if reg == None:
            raise Exception("No free registers inside OR outside of stack!")

        # emit code to copy the register to the memory location.
        if comment_str == None:
            comment_str = "Stack machine: copy register to temp"
        self.o("  movl %s, %s" % (reg, mem),
               comment_str)

        # Modify the element's stack machine position to reflect
        # its new location.
        self.stack[index] = mem
        return reg

    def _get_free_reg(self, valid_regs, preferred_reg=None):
        """Returns a free register that is in the valid_regs list.  If
        no registers are available, the most least-recently used
        eligible one is freed (by moving its contents to a temporary
        variable) and returned."""

        # If we have a register free, return it.
        if len(self.regs_free) > 0:
            reg = None
            if preferred_reg != None and preferred_reg in self.regs_free:
                reg = preferred_reg
            else:
                for r in self.regs_free:
                    if r in valid_regs:
                        reg = r
            if reg != None:
                self.regs_free.remove(reg)
                # If this register is a callee-save register that
                # we haven't used before, add it to our list
                # of used callee-save registers.
                if reg in self.callee_save_regs and reg not in self.callee_save_regs_used:
                    self.callee_save_regs_used.append(reg)
                return reg
        # copy a register into a temp var and return the register.
        return self._copy_reg_to_temp(valid_regs)

    def _get_type_valid_regs(self, type):
        """Returns the valid registers that an element of the given
        type can occupy.  For instance, 8-bit chars should only be
        placed in %eax/%ebx/%ecx/%edx because these are the only
        registers with low-order byte sub-registers
        (%al/%bl/%cl/%dl)."""
        
        type_str = type.get_outer_string()
        if type_str == 'char':
            return self.byte_compat_regs
        elif type_str in ['int', 'pointer']:
            return self.all_regs

    def push(self, type=None, preferred_reg=None, valid_regs=None):
        """Finds a free eligible register (or frees one if all are
        being used) and returns it, pushing the register onto the
        stack machine's stack.

        This method associates the stack entry with the given Type
        object; if none is supplied, then an 'int' type is used
        by default.

        If preferred_reg is passed, this function will try its
        best to return preferred_reg, if it's available."""

        if type == None:
            type = self.default_type
        self.type_stack.append(type)
        if valid_regs == None:
            valid_regs = self._get_type_valid_regs(type)
        reg = self._get_free_reg(valid_regs, preferred_reg)
        self.stack.append(reg)
        return reg

    def _coerce_type(self, curr_reg, from_type, to_type):
        """Attempts to coerce the element in the current register
        from the given type to the given type."""
        
        from_str = from_type.get_outer_string()
        to_str = to_type.get_outer_string()
        comment_str = "Implicit cast: %s -> %s" % (from_str, to_str)
        if from_str == to_str:
            return curr_reg
        if from_str == 'char':
            if to_str == 'int':
                return curr_reg
        elif from_str == 'int':
            if to_str == 'char':
                self.o("  movzbl %s, %s" % (self.lo(curr_reg),
                                            curr_reg),
                       comment_str)
                return curr_reg

    def pop(self, type=None, valid_regs=None):
        """Pops the top element off the stack machine's stack, coerces
        it to the given type if necessary, and returns a register in
        which the element's value now resides.

        If no type is specified, pop() returns the value of the
        element as-is."""
        
        prev_type = self.type_stack.pop()
        if type != None:
            if valid_regs == None:
                valid_regs = self._get_type_valid_regs(type)
            reg = self._pop(valid_regs)
            return self._coerce_type(reg, prev_type, type)
        else:
            return self._pop(self.all_regs)

    def _pop(self, valid_regs):
        """Pops the top element of the stack into a free register
        that is also in valid_regs and returns the register name.  If
        no registers are free, the least recently used one is first
        copied into a temporary variable and then used."""
        
        loc = self.stack.pop()
        
        # If the top of the stack is a register, just return the
        # name of the register and add the register to our free
        # register list.
        if loc in valid_regs:
            self.regs_almost_free.append(loc)
            return loc
        
        # Otherwise, copy the temp variable at the top of the stack
        # into a free register, possibly requiring us to spill the
        # current contents of the memory register into another temp
        # variable.
        reg = self._get_free_reg(valid_regs)
        self.o("  movl %s, %s" % (loc, reg),
               "Stack machine: copy temp to register")

        # if our location was a register but not in valid_regs,
        # make the register free for use.
        if loc in self.all_regs:
            self.regs_free.append(loc)
            
        self.regs_almost_free.append(reg)
        return reg

    def peek(self):
        """Returns the top element of the stack, but doesn't pop
        it.  Note that this is not guaranteed to be a register; it
        could be a memory location!"""
        
        return self.stack[-1]

    def is_empty(self):
        """Returns whether the stack machine is empty."""
        
        return len(self.stack) == 0

    def done(self):
        """Frees all registers that are marked as being in
        intermediate use (i.e., have been pop()'d)."""
        
        self.regs_free.extend(self.regs_almost_free)
        self.regs_almost_free = []

    def get_max_fp(self):
        """Returns the maximum point in the process' stack, relative
        to the current function's frame pointer, that the stack
        machine is using for temporary variables."""
        
        return self.next_temp + WORD_SIZE

    def lo(self, reg):
        """Returns the low-order byte of the given register.  If the
        register isn't byte-compatible (i.e., isn't %eax, %ebx, %ecx,
        or %edx), then an exception is raised.

        Example: stack.lo('%eax') == '%al'."""
        
        if reg[0] == '$':
            return reg
        if reg not in self.byte_compat_regs:
            raise Exception("Register %s is not byte-compatible!" % reg)
        return '%' + reg[2] + 'l'

    def force_type_change(self, type):
        """Forces a type change of the top element of the stack."""

        self.type_stack[-1] = type

#  ---------------------------------------------------------------
#  CODE GENERATOR
#  ---------------------------------------------------------------

class CodeGenVisitor(Visitor):
    """Visitor that generates x86 assembly code for the abstract
    syntax tree."""
    
    def __init__(self, file, show_comments=0):
        """Constructor.  'file' is the file object to output the
        resulting code to.  If 'show_comments' is true, then annotated
        comments are produced for the generated assembly code."""
        
        Visitor.__init__(self)

        # The current label number we're on, for generating
        # jump labels in the assembly code (e.g., 'LO', 'L1', etc).
        self.__label = 0

        # Current label number for generating string literal labels.
        self.__str_literal_label = 0

        # Current assembly code for string literals.
        self.__str_literal_str = ""

        # Whether we should show comments or not.
        self.show_comments = show_comments

        # The file we're outputting the generated code to.
        self.file = file

        # A hashtable of binary operators and the assembly
        # instructions corresponding to them.  Certain instructions
        # are just the 'base' instruction and require a suffix
        # corresponding to the size of the operands; for instance,
        # addition can be accomplished with the 'addl' instruction
        # for 32-bit integers and 'addb' for 8-bit integers.
        #
        # In such cases, the code adds the appropriate suffixes on its
        # own.
        self.binop_instructions = \
                                { '==' : 'sete',
                                  '!=' : 'setne',
                                  '>=' : 'setge',
                                  '<=' : 'setle',
                                  '>'  : 'setg',
                                  '<'  : 'setl',
                                  '+'  : 'add',
                                  '-'  : 'sub',
                                  '*'  : 'imul',
                                  '='  : 'mov'
                                  }

        # Windows' C linkage prepends a '_' before symbol
        # names, whereas Unix doesn't.  This is particularly
        # critical if the source file is linking to external
        # libraries that we're not compiling.  Figure out
        # which one to use here.
        import sys
        if sys.platform == 'win32':
            self.symbol_prepend = "_"
        else:
            self.symbol_prepend = ""
            
    def new_label(self):
        """Generate a new jump label and return it."""
        
        label = ".L%d" % self.__label
        self.__label += 1
        return label

    def o(self, str, comment=None):
        """Output a line of assembly code to the output file,
        with an optional annotated comment (if comments are
        enabled)."""
        
        if self.show_comments and comment != None:
            comment = "# %s" % comment
            self.curr_str += "%-35s %s\n" % (str, comment)
        else:
            if str == "":
                return
            self.curr_str += str + "\n"

    def c(self, str, indent_amt=2):
        """Output a single-line comment to the output file, if
        comments are enabled."""

        indent = " " * indent_amt
        
        if self.show_comments:
            self.o("\n%s# %s\n" % (indent, str))
            
    def vNodeList(self, node):
        self._visitList(node.nodes)

    def _empty_stack(self, node):
        """Pops the top value from the stack machine's stack and
        discard it.  This is used when a statement has a return
        value (for instance, the line 'a = b + 1;') and its
        return value has been pushed onto the stack but there's
        nothing to pop it off."""
        
        # if the statement was also an expression, then its return
        # value is still on the stack, so empty it (throw away
        # the return value).
        if not self.stack.is_empty():
            self.stack.pop(node.type)
            self.stack.done()
            if not self.stack.is_empty():
                raise Exception("PANIC! Register stack isn't empty!")

    def _accept_and_empty_stack(self, node):
        """Visit the node and then empty the stack machine of the
        node's return value, if one exists."""
        
        node.accept(self)
        self._empty_stack(node)

    def vStatementList(self, node):
        for n in node.nodes:
            self._accept_and_empty_stack(n)

    def _generate_global_variable_definitions(self, node):
        """Generate and return a list of global variable
        definitions."""
        
        globals_str = ".global_vars:\n"
        for symbol in node.symtab.entries.values():
            symbol.compile_loc = self.symbol_prepend + symbol.name
            if not symbol.type.is_function() and not symbol.extern:
                globals_str += "  .comm %s,%d\n" % \
                (symbol.compile_loc, \
                 self._calc_var_size(symbol.type)*WEIRD_MULTIPLIER)
        return globals_str
    
    def vTranslationUnit(self, node):
        """Outputs the entire assembly source file."""
        
        self.curr_str = ""
        self.o("# Generated by c.py")
        self.o("# Atul Varma (Spring 2004)\n")
        self.o("    .text")

        globals_str = self._generate_global_variable_definitions(node)

        # Generate the main code.
        self._visitList(node.nodes)
        
        # Append global variable definitions.
        self.o(globals_str)

        # Append string literal definitions.
        self.o(self.__str_literal_str)

        # Output the entire file.
        self.file.write(self.curr_str)

    def _calc_var_size(self, type):
        """Calculate and return the size of the given type, in
        bytes."""
        
        type_str = type.get_outer_string()
        if type_str == "int":
            return INT_SIZE
        elif type_str == "char":
            return CHAR_SIZE
        elif type_str == "pointer":
            return WORD_SIZE
        else:
            self.error("Unknown type: %s" % type_str)

    def _calc_var_align(self, type):
        """Calculate and return the alignment of the given type,
        in bytes."""
        
        return self._calc_var_size(type)

    def _calc_function_var_addrs(self, symtab, last_fp_loc):
        """Calculate the addresses of all local variables in the
        function and attach them to their respective symbols in
        the function's symbol table(s)."""
        
        self._calc_function_arg_addrs(symtab)
        return self._calc_local_var_addrs(symtab.children[0], last_fp_loc)

    def _calc_function_arg_addrs(self, symtab):
        """Calculate the addresses of all the arguments passed to
        the function."""
        
        for symbol in symtab.entries.values():
            symbol.compile_loc = "%d(%%ebp)" % (WORD_SIZE*2+(symbol.param_num*WORD_SIZE))
            if not symbol.is_used:
                self.warning("function argument '%s' is never used." % symbol.name)

    def _calc_local_var_addrs(self, symtab, last_fp_loc):
        """Calculate the locations of all the local variables defined
        in the function's body and all nested scopes therein.

        This model of allocation assumes a 'worst-case' scenario
        where all branches and nested scopes of the function are
        executed; thus the space required for all the local
        variables is allocated on the process' stack at the
        beginning of the function.

        Note, however, that lexical scopes that cannot exist
        at the same time may overlap in memory.  For instance,
        examine the following 'if' statement:

          if (a > 1) {
            int i;
          } else {
            int j;
          }

        Here 'i' and 'j' will actually occupy the same place in
        memory because it is impossible for both of them to
        exist in memory at the same time."""
          
        for symbol in symtab.entries.values():
            if symbol.extern:
                symbol.compile_loc = self.symbol_prepend + symbol.name
                continue
            last_fp_loc -= self._calc_var_size(symbol.type)
            
            # adjust location for alignment
            align = self._calc_var_align(symbol.type)
            bytes_overboard = (-last_fp_loc) % align
            if bytes_overboard != 0:
                last_fp_loc -= (align - bytes_overboard)

            symbol.compile_loc = "%d(%%ebp)" % last_fp_loc
            if not symbol.is_used:
                self.warning("local variable '%s' is never used." % symbol.name)
        max_last_fp = last_fp_loc
        for kid in symtab.children:
            curr_last_fp = self._calc_local_var_addrs(kid, last_fp_loc)
            if curr_last_fp < max_last_fp:
                max_last_fp = curr_last_fp

        # adjust location for alignment, to keep the stack aligned
        # on a word-sized boundary.
        align = self._calc_var_align(cparse.PointerType())
        bytes_overboard = (-max_last_fp) % align
        if bytes_overboard != 0:
            max_last_fp -= (align - bytes_overboard)

        return max_last_fp

    def _fill_line(self, str, width=70):
        """Fills a string to the given width with the '-'
        character."""

        extra = "-" * (width-1-len(str))
        return str + " " + extra
    
    def vFunctionDefn(self, node):
        """Output the assembly code for a function."""
        
        self.break_labels = []
        self.continue_labels = []
        self.curr_func_end_label = self.new_label() + "_function_end"

        # Calculate the base size of the stack frame (not including
        # space for the stack machine's temporary variables).
        stack_frame_size = self._calc_function_var_addrs(node.symtab, 0)

        line = self._fill_line("BEGIN FUNCTION: %s()" % node.name)
        self.c("%s\n"
               "#\n"
               "# Function type: %s" %
               (line, node.type.get_string()), 0)

        if not node.static:
            self.o("    .global %s" % node.compile_loc)
        self.o("%s:" % node.compile_loc)
        self.o("  pushl %ebp", "Save old frame pointer")
        self.o("  movl %esp, %ebp", "Set new frame pointer")

        # Create a new stack machine for this function.
        self.stack = x86Registers(self, stack_frame_size)

        # Generate assembly code for the function.  Here we
        # perform a little hack so that we can generate the
        # code for the function into a separate string, and then
        # insert it into our code later on.
        
        old_str = self.curr_str
        self.curr_str = ""

        node.body.accept(self)
        
        function_str = self.curr_str
        self.curr_str = old_str

        # Figure out the final size of the stack frame, taking into
        # account the stack machine's temporary variables, and
        # insert the code at the beginning of the function.
        if self.stack.get_max_fp() != 0:
            self.o("  subl $%d, %%esp" % (-self.stack.get_max_fp()),
                   "Allocate space for local+temp vars")

        # Save any callee-save registers that may have been used.
        self.stack.save_callee_saves()

        # Add the previously-generated assembly code for the function.
        self.curr_str += function_str
        
        self.o("%s:" % self.curr_func_end_label)

        # Restore any callee-save registers that may have been used.
        self.stack.load_callee_saves()
        self.o("  movl %ebp, %esp", "Deallocate stack frame")
        self.o("  popl %ebp", "Restore old stack frame")
        self.o("  ret\n")

        line = self._fill_line("END FUNCTION: %s()" % node.name)
        self.c(line, 0)
        
    def vCompoundStatement(self, node):
        node.statement_list.accept(self)

    def vIfStatement(self, node):
        done_label = self.new_label() + "_done"
        if not node.else_stmt.is_null():
            else_label = self.new_label() + "_else"
        else:
            else_label = done_label

        self.c("IF statment - begin")
        
        node.expr.accept(self)
        comparer = self.stack.pop()
        self.stack.done()
        self.o("  testl %s, %s" % (comparer, comparer), "Test the result")        
        self.o("  jz %s" % else_label,
               "If result is zero, jump to else clause")
        self.c("IF statment - THEN clause - begin")
        self._accept_and_empty_stack(node.then_stmt)
        self.c("IF statment - THEN clause - end")        
        self.o("  jmp %s" % done_label)
        if not node.else_stmt.is_null():
            self.c("IF statment - ELSE clause - begin")
            self.o("%s:" % else_label)
            self._accept_and_empty_stack(node.else_stmt)
            self.c("IF statment - ELSE clause - end")
        self.o("%s:" % done_label)

        self.c("IF statment - end")
        
    def _push_loop_labels(self, break_label, continue_label):
        """Pushes new values of labels to jump to for 'break' and
        'continue' statements."""
        
        self.break_labels.append(break_label)
        self.continue_labels.append(continue_label)
        
    def _pop_loop_labels(self):
        """Restores old values of labels to jump to for 'break' and
        'continue' statements."""
        
        self.break_labels.pop()
        self.continue_labels.pop()

    def vWhileLoop(self, node):
        test_label = self.new_label() + "_test"
        done_label = self.new_label() + "_done"

        self._push_loop_labels(break_label=done_label,
                               continue_label=test_label)

        self.c("WHILE loop - begin")
        
        self.o("%s:" % test_label)
        node.expr.accept(self)

        comparer = self.stack.pop()
        self.stack.done()
        self.o("  testl %s, %s" % (comparer, comparer), "Test the result")
        self.o("  jz %s" % done_label,
               "If result is zero, leave while loop")
        self._accept_and_empty_stack(node.stmt)
        self.o("  jmp %s" % test_label, "Jump to start of while loop")
        self.o("%s:" % done_label)

        self.c("WHILE loop - end")
        
        self._pop_loop_labels()

    def vForLoop(self, node):
        test_label = self.new_label() + "_test"
        done_label = self.new_label() + "_done"

        self._push_loop_labels(break_label=done_label,
                               continue_label=test_label)

        self.c("FOR loop - begin")
        
        self._accept_and_empty_stack(node.begin_stmt)
        
        self.o("%s:" % test_label)
        node.expr.accept(self)

        comparer = self.stack.pop()
        self.stack.done()
        self.o("  testl %s, %s" % (comparer, comparer), "Test the result")
        self.o("  jz %s" % done_label,
               "If result is zero, leave for loop")
        self._accept_and_empty_stack(node.stmt)
        self._accept_and_empty_stack(node.end_stmt)
        self.o("  jmp %s" % test_label, "Jump to start of for loop")
        self.o("%s:" % done_label)

        self.c("FOR loop - end")
        
        self._pop_loop_labels()

    def vBreakStatement(self, node):
        self.o("  jmp %s" % self.break_labels[-1],
               "Loop: break statement")

    def vContinueStatement(self, node):
        self.o("  jmp %s" % self.continue_labels[-1],
               "Loop: continue statement")

    def _get_new_str_literal_label(self, str):
        """Create a new string literal label for the given string,
        generate (but do not yet emit) the assembly for it, and return
        the name of the new label."""
        
        label_str = "LC%d" % self.__str_literal_label
        str = str.replace('\n', '\\12')
        self.__str_literal_str += """%s:\n  .ascii "%s\\0"\n""" % (label_str, str)
        self.__str_literal_label += 1
        return label_str

    def vStringLiteral(self, node):
        label_str = self._get_new_str_literal_label(node.get_str())

        # Make a little preview of the literal in the annotated
        # comments.
        COMMENT_CHARS = 7
        comment_label = node.get_sanitized_str()
        if len(comment_label) > COMMENT_CHARS:
            comment_label = "%s..." % comment_label[0:COMMENT_CHARS]

        self.o("  movl $%s, %s" % (label_str,
                                   self.stack.push(node.type)),
               "Get addr of string literal '%s'" % comment_label)

    def vConst(self, node):
        self.o("  movl $%d, %s" % (node.value,
                                   self.stack.push(node.type)),
               "Load numeric constant %d" % node.value)

    def vId(self, node):
        # If we're only supposed to push our address on the stack, not
        # our actual value, then do that and exit.
        if node.output_addr:
            self.o("  leal %s, %s" % (node.symbol.compile_loc,
                                      self.stack.push()),
                   "Get address of %s" % node.symbol.name)
            return
        type_str = node.type.get_outer_string()
        if type_str in ['pointer', 'int']:
            instr = 'movl'
        elif type_str == 'char':
            instr = 'movzbl'
        self.o("  %s %s, %s" % (instr, node.symbol.compile_loc,
                                self.stack.push(node.type)),
               "Get value of %s" % node.symbol.name)

    def vArrayExpression(self, node):
        node.expr.accept(self)
        node.index.accept(self)
        reg_index = self.stack.pop(node.index.type)
        reg_expr = self.stack.pop(node.expr.type)
        reg_to = self.stack.push(node.type)
        size = self._calc_var_size(node.type)
        addr_str = "(%s,%s,%d)" % (reg_expr, reg_index, size)
        self.stack.done()
        if node.output_addr:
            self.o("  leal %s, %s" % (addr_str, reg_to),
                   "Load addr of pointer array index")
        else:
            type_str = node.type.get_outer_string()            
            if type_str in ['int', 'pointer']:
                instr = 'movl'
            elif type_str == 'char':
                instr = 'movzbl'
            self.o("  %s %s, %s" % (instr, addr_str, reg_to),
                   "Pointer array index dereference")

    def vFunctionExpression(self, node):
        """Generates assembly for calling a function."""

        self.c("FUNCTION CALL to %s() - begin" %
               node.function.symbol.name)
        
        # If we're using any caller-save registers, free them up.
        self.stack.save_caller_saves()

        # We need to temporarily reverse the order of the function's
        # arguments because we need to push them onto the stack
        # in reverse order.
        node.arglist.nodes.reverse()
        argnum = len(node.arglist.nodes)
        for arg in node.arglist.nodes:
            arg_reg = self._accept_and_pop(arg)
            self.o("  pushl %s" % arg_reg, "Push arg %d" % argnum)
            self.stack.done()
            argnum -= 1
        node.arglist.nodes.reverse()

        self.o("  call %s" % node.function.symbol.compile_loc,
               "Call %s()" % node.function.symbol.name)

        # The function will place its return value in register %eax.
        # So, we'll push a register from the stack and ask it to
        # give us %eax.
        result = self.stack.push(node.function.symbol.type.get_return_type(), preferred_reg='%eax')

        # If we got %eax, don't do anything, because our return
        # value is already in there.  Otherwise, move it.
        #
        # (Note that in the current implementation of the stack
        # machine, we should always get %eax.)
        if result != '%eax':
            self.o("  movl %%eax, %s" % result, "Copy return value")

        arg_stack_size = (len(node.arglist.nodes)*WORD_SIZE)

        if arg_stack_size > 0:
            self.o("  addl $%d, %%esp" % arg_stack_size,
                   "Deallocate argument stack")

        self.c("FUNCTION CALL to %s() - end" %
        node.function.symbol.name)
        
    def vReturnStatement(self, node):
        return_reg = self._accept_and_pop(node.expr)
        self.o("  movl %s, %%eax" % return_reg, "Set return value")
        self.o("  jmp %s" % self.curr_func_end_label, "Exit function")
        self.stack.done()

    def _accept_and_pop(self, node):
        """Accept the given node and pop its value into a register and
        return the register.  Implicit type conversion is performed,
        if necessary, by the stack machine.

        Also, if the node is determined to be a numeric constant,
        the literal value of the constant (e.g., '$15') is returned,
        for purposes of optimization."""

        if node.is_const():
            return "$%d" % node.value
        else:
            node.accept(self)
            return self.stack.pop(node.coerce_to_type)

    def _binop_assign(self, node):
        """Performs an assignment operation (=, +=, etc) on the given
        Binop node."""
        
        node.left.accept(self)
        right_reg = self._accept_and_pop(node.right)
        left_reg = self.stack.pop()
        instr = self.binop_instructions[node.op[0]]
        instr += self._type_suffix(node.type)
        
        type_str = node.type.get_outer_string()
        if type_str == 'char':
            right_reg = self.stack.lo(right_reg)
            
        self.o("  %s %s, (%s)" % (instr, right_reg, left_reg),
               "Perform assignment '%s'" % node.op)

        # NOTE: Wow, this makes for insanely inefficient code, especially
        # when the result of the operation isn't being used.
        if type_str in ['int', 'pointer']:
            instr = 'movl'
        elif type_str == 'char':
            instr = 'movzbl'
            
        self.o("  %s (%s), %s" % (instr, left_reg,
                                  self.stack.push(node.type)),
               "Copy assignment result to register")
        self.stack.done()

    def _type_suffix(self, type):
        """Returns the assembly instruction suffix for the given type;
        'l' for 32-bit types, 'b' for 8-bit types, etc..."""
        
        type_str = type.get_outer_string()
        if type_str in ['int', 'pointer']:
            return 'l'
        elif type_str == 'char':
            return 'b'

    def _binop_arith(self, node):
        """Performs an arithmetic operation (+, -, etc) on the given
        Binop node."""
        
        node.left.accept(self)
        right_reg = self._accept_and_pop(node.right)
        left_reg = self.stack.pop(node.left.coerce_to_type)

        instr = self.binop_instructions[node.op] + \
                self._type_suffix(node.type)
        type_str = node.type.get_outer_string()

        if type_str == 'char':
            r_reg = self.stack.lo(right_reg)
            l_reg = self.stack.lo(left_reg)
        else:
            r_reg = right_reg
            l_reg = left_reg
            
        self.o("  %s %s, %s" % (instr, r_reg, l_reg),
               "Perform '%s'" % node.op)
        self.stack.done()

        # Here we are relying on the fact that left_reg is now free
        # from the last pop(), so we should be able to push it
        # back onto the stack machine.

        new_reg = self.stack.push(node.type, preferred_reg=left_reg)
        if new_reg != left_reg:
            raise Exception("PANIC! Binop push() isn't same as last pop()!")

    def _binop_compare(self, node):
        """Performs a comparison operation (>, ==, etc) on the given
        Binop node."""
        
        node.left.accept(self)
        right_reg = self._accept_and_pop(node.right)
        left_reg = self.stack.pop(node.left.coerce_to_type)
        self.stack.done()

        self.o("  cmpl %s, %s" % (right_reg, left_reg),
               "Compare %s to %s" % (left_reg, right_reg))

        # TODO: this could cause errors, if push() generates
        # mov instructions...  not sure if mov instructions
        # change the flags though, they probably shouldn't
        # since they're not arithmetic operations.
        byte_reg = self.stack.push(cparse.BaseType('char'))
        lo = self.stack.lo(byte_reg)
        self.o("  %s %s" % (self.binop_instructions[node.op],
                            lo),
               "Perform '%s'" % node.op)
        self.o("  movzbl %s, %s" % (lo, byte_reg),
               "Zero-extend the boolean result")

    def vBinop(self, node):
        if node.op in cparse.Binop.ASSIGN_OPS:
            self._binop_assign(node)
        elif node.op in ['+','-','*']:
            self._binop_arith(node)
        elif node.op in ['==', '!=', '<', '>', '<=', '>=']:
            self._binop_compare(node)

    def vNegative(self, node):
        node.expr.accept(self)
        self.o("  negl %s" % self.stack.peek(),
               "Perform unary negation")

    def vPointer(self, node):
        node.expr.accept(self)
        if node.output_addr:
            self.o("", "(Getting pointer target addr via '*')")
            return
        reg_from = self.stack.pop(node.expr.type)
        reg_to = self.stack.push(node.type)
        type_str = node.type.get_outer_string()
        if type_str in ['int', 'pointer']:
            instr = 'movl'
        elif type_str == 'char':
            instr = 'movzbl'
        self.o("  %s (%s), %s" % (instr, reg_from, reg_to),
               "Pointer dereference")
        self.stack.done()

    def vAddrOf(self, node):
        node.expr.accept(self)
        self.stack.force_type_change(node.type)
        self.o("", "(Address-of operator '&' used here)")

#  ---------------------------------------------------------------
#  End of cx86.py
#  ---------------------------------------------------------------

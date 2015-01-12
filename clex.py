#  ---------------------------------------------------------------
#  clex.py
#
#  Atul Varma
#  Python C Compiler - Lexical Analyzer
#  $Id: clex.py,v 1.2 2004/06/02 21:05:45 varmaa Exp $
#  ---------------------------------------------------------------

import lex
import re

#  ---------------------------------------------------------------
#  TOKEN LIST
#  ---------------------------------------------------------------

tokens = (
    # Reserved words
    'AUTO',
    'BREAK',
    'CASE',
    'CHAR',
    'CONST',
    'CONTINUE',
    'DEFAULT',    
    'DO',
    'DOUBLE',
    'ELSE',
    'ENUM',
    'EXTERN',
    'FLOAT',
    'FOR',
    'GOTO',
    'IF',
    'INT',
    'LONG',
    'REGISTER',
    'RETURN',
    'SHORT',
    'SIGNED',
    'SIZEOF',
    'STATIC',
    'STRUCT',
    'SWITCH',
    'TYPEDEF',
    'UNION',
    'UNSIGNED',
    'VOID',
    'VOLATILE',
    'WHILE',

    # Special characters
    'COMMA',
    'COLON',
    'SEMICOLON',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'LBRACE',
    'RBRACE',
    'ASSIGN',
    'GREATER',
    'LESS',
    'EQ',
    'NOT_EQ',
    'GREATER_EQ',
    'LESS_EQ',
    'DOUBLE_PLUS',
    'DOUBLE_MINUS',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIV',
    'MODULO',
    'DOUBLE_AMPERSAND',
    'DOUBLE_PIPE',
    'EXCLAMATION',
    'AMPERSAND',
    'PIPE',
    'CARET',
    'ASTERISK',
    'QUESTION',
    'TILDE',
    'POUND',
    'DOT',
    'ELLIPSIS',
    'ARROW',
    'SHIFT_LEFT',
    'SHIFT_RIGHT',
    'EQ_PLUS',
    'EQ_MINUS',
    'EQ_TIMES',
    'EQ_DIV',
    'EQ_MODULO',
    'EQ_PIPE',
    'EQ_AMPERSAND',
    'EQ_CARET',
    'EQ_SHIFT_LEFT',
    'EQ_SHIFT_RIGHT',

    # Complex tokens
    'ID',
    'FNUMBER',    
    'INUMBER',
    'STRING',
    'CHARACTER',
    )

#  ---------------------------------------------------------------
#  RESERVED WORDS
#  ---------------------------------------------------------------

reserved_words = {
    'auto' : 'AUTO',
    'break' : 'BREAK',
    'case' : 'CASE',
    'char' : 'CHAR',
    'const' : 'CONST',
    'continue' : 'CONTINUE',
    'default' : 'DEFAULT',    
    'do' : 'DO',
    'double' : 'DOUBLE',
    'else' : 'ELSE',
    'enum' : 'ENUM',
    'extern' : 'EXTERN',
    'float' : 'FLOAT',
    'for' : 'FOR',
    'goto' : 'GOTO',
    'if' : 'IF',
    'int' : 'INT',
    'long' : 'LONG',
    'register' : 'REGISTER',
    'return' : 'RETURN',
    'short' : 'SHORT',
    'signed' : 'SIGNED',
    'sizeof' : 'SIZEOF',
    'static' : 'STATIC',
    'struct' : 'STRUCT',
    'switch' : 'SWITCH',
    'typedef' : 'TYPEDEF',
    'union' : 'UNION',
    'unsigned' : 'UNSIGNED',
    'void' : 'VOID',
    'volatile' : 'VOLATILE',
    'while' : 'WHILE'
}

#  ---------------------------------------------------------------
#  SPECIAL CHARACTERS
#  ---------------------------------------------------------------

t_COMMA = r','
t_COLON = r':'
t_SEMICOLON = r';'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'{'
t_RBRACE = r'}'
t_ASSIGN = r'='
t_GREATER = r'>'
t_LESS = r'<'
t_EQ = r'=='
t_NOT_EQ = r'!='
t_GREATER_EQ = r'>='
t_LESS_EQ = r'<='
t_DOUBLE_PLUS = r'\+\+'
t_DOUBLE_MINUS = r'--'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIV = r'/(?!\*)'
t_MODULO = r'%'
t_DOUBLE_AMPERSAND = r'&&'
t_DOUBLE_PIPE = r'\|\|'
t_EXCLAMATION = r'!'
t_AMPERSAND = r'&'
t_PIPE = r'\|'
t_CARET = r'^'
t_ASTERISK = r'\*'
t_QUESTION = r'\?'
t_TILDE = r'~'
t_POUND = r'\#'
t_ELLIPSIS = r'\.\.\.'
t_DOT = r'\.'
t_ARROW = r'->'
t_SHIFT_LEFT = r'<<'
t_SHIFT_RIGHT = r'>>'
t_EQ_PLUS = r'\+='
t_EQ_MINUS = r'-='
t_EQ_TIMES = r'\*='
t_EQ_DIV = r'/='
t_EQ_MODULO = r'%='
t_EQ_PIPE = r'\|='
t_EQ_AMPERSAND = r'&='
t_EQ_CARET = r'\^='
t_EQ_SHIFT_LEFT = r'<<='
t_EQ_SHIFT_RIGHT = r'>>='

#  ---------------------------------------------------------------
#  COMPLEX TOKENS
#  ---------------------------------------------------------------

def t_ID(t):
    r'[A-Za-z_][\w]*'
    if reserved_words.has_key(t.value):
        t.type = reserved_words[t.value]
    return t

def t_FNUMBER(t):
    r'((0(?!\d))|([1-9]\d*))((\.\d+(e[+-]?\d+)?)|(e[+-]?\d+))'
    return t

def t_malformed_fnumber(t):
    r'(0\d+)((\.\d+(e[+-]?\d+)?)|(e[+-]?\d+))'
    print "Line %d. Malformed floating point number '%s'" % (t.lineno, t.value)

def t_INUMBER(t):
    r'0(?!\d)|([1-9]\d*)'
    return t

def t_malformed_inumber(t):
    r'0\d+'
    print "Line %d. Malformed integer '%s'" % (t.lineno, t.value)

def t_CHARACTER(t):
    r"'\w'"
    return t

def t_STRING(t):
    r'"[^\n]*?(?<!\\)"'
    temp_str = t.value.replace(r'\\', '')
    m = re.search(r'\\[^n"]', temp_str)
    if m != None:
        print "Line %d. Unsupported character escape %s in string literal." % (t.lineno, m.group(0))
        return
    return t

#  ---------------------------------------------------------------
#  IGNORED TOKENS
#  ---------------------------------------------------------------

def t_WHITESPACE(t):
    r'[ \t]+'
    pass

def t_NEWLINE(t):
    r'\n+'
    t.lineno += len(t.value)

def t_COMMENT(t):
    r'/\*[\w\W]*?\*/'
    t.lineno += t.value.count('\n')
    pass

#  ---------------------------------------------------------------
#  ERROR HANDLING
#  ---------------------------------------------------------------

def t_error(t):
    print "Line %d." % (t.lineno,) + "",
    if t.value[0] == '"':
        print "Unterminated string literal."
        if t.value.count('\n') > 0:
            t.skip(t.value.index('\n'))
    elif t.value[0:2] == '/*':
        print "Unterminated comment."
    else:
        print "Illegal character '%s'" % t.value[0]
        t.skip(1)

#  ---------------------------------------------------------------
#  MAIN LEXER FUNCTIONALITY
#  ---------------------------------------------------------------

def run_lexer():
    """This is just a debugging function that prints out a list of
    tokens, it's not actually called by the compiler or anything."""
    
    import sys
    file = open(sys.argv[1])
    lines = file.readlines()
    file.close()
    strings = ""
    for i in lines:
        strings += i
    lex.input(strings)
    while 1:
        token = lex.token()       # Get a token
        if not token: break        # No more tokens
        print "(%s,'%s',%d)" % (token.type, token.value, token.lineno)

lex.lex()

if __name__ == '__main__':
    run_lexer()

#  ---------------------------------------------------------------
#  End of clex.py
#  ---------------------------------------------------------------

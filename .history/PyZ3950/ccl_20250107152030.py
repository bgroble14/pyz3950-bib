"""
CCL (Common Command Language) Parser - ISO 8777 Implementation
Modernized for Python 3.12+ compatibility
"""

import string
from typing import Dict, List, Tuple, Union, Optional
from ply import lex, yacc

try:
    from PyZ3950 import z3950, oids, asn1
    _attrdict = {
        'bib1': oids.Z3950_ATTRS_BIB1_ov,
        'zthes1': oids.Z3950_ATTRS_ZTHES_ov,
        'xd1': oids.Z3950_ATTRS_XD1_ov,
        'utility': oids.Z3950_ATTRS_UTIL_ov,
        'exp1': oids.Z3950_ATTRS_EXP1_ov
    }
except ImportError as err:
    print(f"Error importing (OK during setup): {err}")
    _attrdict = {}

# Exception classes
class QuerySyntaxError(Exception): pass
class ParseError(QuerySyntaxError): pass
class LexError(QuerySyntaxError): pass
class UnimplError(QuerySyntaxError): pass

# Token definitions
tokens = (
    'LPAREN', 'RPAREN', 'COMMA', 'SET', 'ATTRSET',
    'QUAL', 'QUOTEDVALUE', 'RELOP', 'WORD', 'LOGOP', 'SLASH'
)

# Simple token patterns
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r','
t_SLASH = r'/'

# Case-insensitive patterns moved to function definitions with flags at start
def t_ATTRSET(t):
    r'(?i)ATTRSET'
    return t

def t_SET(t):
    r'(?i)SET'
    return t

def t_LOGOP(t):
    r'(?i)AND|OR|NOT'
    return t

# Relation operators mapping
relop_to_attrib = {
    '<': 1, '<=': 2, '=': 3, '>=': 4, '>': 5, '<>': 6
}

t_RELOP = '|'.join(f'({re.escape(r)})' for r in relop_to_attrib.keys())

# Qualifier dictionary for BIB-1 attributes
qual_dict = {
    'TI': (1, 4),
    'AU': (1, 1003),
    'ISBN': (1, 7),
    'LCCN': (1, 9),
    'ANY': (1, 1016),
    'FIF': (3, 1),
    'AIF': (3, 3),
    'RTRUNC': (5, 1),
    'NOTRUNC': (5, 100)
}

default_quals = ['ANY']
default_relop = '='

def t_QUAL(t):
    # Pattern built dynamically in mk_quals()
    return t

def mk_quals():
    """Build the qualifier pattern with case-insensitive flag at start"""
    quals = '|'.join(f'({x})' for x in qual_dict.keys())
    t_QUAL.__doc__ = f"(?i)({quals}|\\([0-9]+,[0-9]+\\))"

def t_QUOTEDVALUE(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # Remove quotes
    return t

# Word pattern components
word_init = "[a-zA-Z0-9&:]"
word_non_init = r"[,.'']"
t_WORD = f"({word_init})({word_init}|{word_non_init})*"

# Whitespace handling
t_ignore = " \t"

def t_error(t):
    raise LexError(f'Illegal character: {t.value[0]}')

# Build lexer
def build_lexer():
    mk_quals()
    return lex.lex()

lexer = build_lexer()

# Parser node class
class Node:
    def __init__(self, type_: str, children: Optional[List] = None, leaf: Optional[str] = None):
        self.type = type_
        self.children = children or []
        self.leaf = leaf
    
    def __str__(self):
        return self.str_depth(0)
    
    def str_depth(self, depth: int) -> str:
        indent = "    " * depth
        result = [f"{indent}{self.type} {self.leaf}"]
        for child in self.children:
            if isinstance(child, Node):
                result.append(child.str_depth(depth + 1))
            else:
                result.append(f"{indent}    {child}")
        return "\n".join(result)

# Parser rules
def p_top(p):
    'top : cclfind_or_attrset'
    p[0] = p[1]

def p_cclfind_or_attrset(p):
    '''cclfind_or_attrset : cclfind
                         | ATTRSET LPAREN WORD SLASH cclfind RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node('attrset', [p[5]], p[3])

def p_cclfind(p):
    '''cclfind : cclfind LOGOP elements
               | elements'''
    if len(p) == 4:
        p[0] = Node('op', [p[1], p[3]], p[2])
    else:
        p[0] = p[1]

def p_elements(p):
    '''elements : LPAREN cclfind RPAREN
                | SET RELOP WORD
                | val
                | quallist RELOP val'''
    if len(p) == 4:
        if p[1] == 'SET':
            if p[2] != '=':
                raise QuerySyntaxError(f"Invalid SET operator: {p[2]}")
            p[0] = Node('set', leaf=p[3])
        elif p[1] == '(':
            p[0] = p[2]
        else:
            p[0] = Node('relop', QuallistVal(list(map(xlate_qualifier, p[1])), p[3]), p[2])
    else:
        p[0] = Node('relop', QuallistVal(list(map(xlate_qualifier, default_quals)), p[1]), default_relop)

class QuallistVal:
    def __init__(self, quallist: List, val: str):
        self.quallist = quallist
        self.val = val
    
    def __str__(self):
        return f"QV: {self.quallist} {self.val}"
    
    def __getitem__(self, i: int):
        if i == 0: return self.quallist
        if i == 1: return self.val
        raise IndexError(f'QuallistVal index error: {i}')

def xlate_qualifier(x: str) -> Tuple[int, int]:
    if x[0] == '(' and x[-1] == ')':
        type_, value = map(int, x[1:-1].split(','))
        return (type_, value)
    return qual_dict[x.upper()]

# Additional parser rules
def p_quallist(p):
    '''quallist : QUAL
                | quallist COMMA QUAL'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_val(p):
    '''val : QUOTEDVALUE
           | val WORD
           | WORD'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = f"{p[1]} {p[2]}"

def p_error(p):
    raise ParseError(f'Syntax error at: {p}')

# Parser configuration
parser = yacc.yacc(debug=False, write_tables=False)

# Query conversion functions
def mk_rpn_query(query: str) -> Tuple[str, z3950.RPNQuery]:
    """Convert CCL query string to RPN query"""
    ast = parser.parse(query, lexer=build_lexer())
    return ast_to_rpn(ast)

def ast_to_rpn(ast: Node) -> Tuple[str, z3950.RPNQuery]:
    """Convert AST to RPN query"""
    if ast.type == 'attrset':
        attrset = attrset_to_oid(ast.leaf)
        ast = ast.children[0]
    else:
        attrset = oids.Z3950_ATTRS_BIB1_ov
    
    rpnq = z3950.RPNQuery(attributeSet=attrset)
    rpnq.rpn = tree_to_q(ast)
    return ('type_1', rpnq)

def attrset_to_oid(attrset: str) -> asn1.OidVal:
    """Convert attribute set name to OID"""
    l = attrset.lower()
    if l in _attrdict:
        return _attrdict[l]
    
    split_l = l.split('.')
    if split_l[0] == '':
        split_l = oids.Z3950_ATTRS + split_l[1:]
    try:
        intlist = list(map(int, split_l))
    except ValueError:
        raise ParseError(f'Invalid OID: {l}')
    return asn1.OidVal(intlist)

def tree_to_q(ast: Node) -> Tuple[str, Union[z3950.RpnRpnOp, z3950.AttributesPlusTerm]]:
    """Convert AST to RPN query components"""
    if ast.type == 'op':
        rpn_op = z3950.RpnRpnOp()
        rpn_op.rpn1 = tree_to_q(ast.children[0])
        rpn_op.rpn2 = tree_to_q(ast.children[1])
        op = ast.leaf.lower()
        if op == 'not':
            op = 'and-not'
        rpn_op.op = (op, None)
        return ('rpnRpnOp', rpn_op)
    
    elif ast.type == 'relop':
        try:
            relattr = relop_to_attrib[ast.leaf]
        except KeyError:
            raise UnimplError(f'Unsupported relation operator: {ast.leaf}')
        
        apt = z3950.AttributesPlusTerm()
        quallist = ast.children.quallist
        
        if ast.leaf != '=':
            quallist.append((2, relattr))
        
        apt.attributes = [
            z3950.AttributeElement(
                attributeType=qual[0],
                attributeValue=('numeric', qual[1])
            )
            for qual in quallist
        ]
        apt.term = ('general', ast.children.val)
        return ('op', ('attrTerm', apt))
    
    elif ast.type == 'set':
        return ('op', ('resultSet', ast.leaf))
    
    raise UnimplError(f"Invalid AST node type: {ast.type}")
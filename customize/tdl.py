### $Id: tdl.py,v 1.17 2008-07-24 11:16:41 sfd Exp $

######################################################################
# imports

import sys
import os
import copy

###########################################################################
# TDL Tokenization
#
# isid(s)
#   return true iff s is a valid TDL identifier (or '...')
#
# TDLtokenize(s)
#  convert s list of TDL tokens and return that list
###########################################################################

def isid(s):
  if s == '...':
    return True
  for c in s:
    if not (c.isalnum() or ord(c) > 127 or c in ['-', '+', '_', '*']):
      return False
  return True
  
def TDLtokenize(s):
  tok = ""
  val = []

  while len(s) > 0:
    s = s.strip()
    if isid(s[0]) or s[0] == '#' or s[0] == '\'':
      i = 1
      while isid(s[i:i + 1]):
        i += 1
      val.append(s[0:i])
      s = s[i:]
    elif s[0] == '"':
      i = 1
      while i < len(s) and s[i] != '"':
        i += 1
      i += 1
      val.append(s[0:i]) # include the quotes
      s = s[i:]
    elif s[0] == '<':
      if s[1:2] == '!':
        val.append(s[0:2])
        s = s[2:]
      else:
        val.append(s[0])
        s = s[1:]
    elif s[0] == '!':
      if s[1:2] == '>':
        val.append(s[0:2])
        s = s[2:]
      else:
        val.append('Unrecognized token "' + s[0] + '"')
        s = ""
    elif s[0] == ':':
      if s[1:2] == '=' or s[1:2] == '<' or s[1:2] == '+':
        val.append(s[0:2])
        s = s[2:]
      else:
        val.append('Unrecognized token "' + s[0] + '"')
        s = ""
    elif s[0:3] == '...':
      val.append(s[0:3])
      s = s[3:]
    elif s[0] == '[' or s[0] == ']' or \
         s[0] == '&' or s[0] == ',' or \
         s[0] == '.' or s[0] == '>':
      val.append(s[0])
      s = s[1:]
    else:
      val.append('Unrecognized token "' + s[0] + '"')
      s = ""

  return val


###########################################################################
# TDL Parsing classes and functions
###########################################################################

###########################################################################
# output

debug_write = False

tdl_file = sys.stdout
tdl_indent = 0

def TDLset_file(f):
  global tdl_file
  tdl_file = f

def TDLget_indent():
  global tdl_indent
  return tdl_indent

def TDLset_indent(indent):
  global tdl_indent
  tdl_indent = indent

def TDLwrite(s):
  global tdl_indent
  global tdl_file
  tdl_file.write(s)
  i = s.rfind('\n')
  if i != -1:
    tdl_indent = len(s) - (i + 1)
  else:
    tdl_indent += len(s)


###########################################################################
# A TDLelem is a node in a TDL parse tree.  This is an abstract class; the
# specific classes below derive from it.

class TDLelem:
  def add(self, ch):
    self.child.append(ch)

  def ordered(self):
    return False

  def set_comment(self, comment):
    pass

  def get_comment(self):
    return ''

  def sort(self):
    new_child = []
    # corefs first
    for c in self.child:
      if isinstance(c, TDLelem_coref):
        new_child.append(c)
    # ...then type names
    for c in self.child:
      if isinstance(c, TDLelem_type):
        new_child.append(c)
    # ...and finally everything else
    for c in self.child:
      if not isinstance(c, TDLelem_coref) and not isinstance(c, TDLelem_type):
        new_child.append(c)
    self.child = new_child
        

###########################################################################
# A TDLelem_literal is an unprocessed string.  It's used for things like
# inflectional rules that must be formatted in a particular way, and
# which aren't expected to merge:
#
# cs1n-bottom :=
#   %prefix (* foo)
#   cs1n-bottom-coord-rule.

class TDLelem_literal:
  def __init__(self, literal):
    self.child = []
    self.comment = ""
    self.literal = literal
    self.one_line = False

  def write(self):
    if self.comment:
      for l in self.comment.split('\n'):
        TDLwrite('; ' + l + '\n')
      TDLwrite('\n')

    if debug_write:
      TDLwrite('literal\n')

    tdl_file.write(self.literal)

  def set_comment(self, comment):
    self.comment = comment

  def get_comment(self):
    return self.comment


###########################################################################
# A TDLelem_typedef corresponds to a statement like:
#
# subj-head-phrase := basic-head-subj-phrase & head-final &
#  [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].
#
# ...as well as a comment preceding the statement

class TDLelem_typedef(TDLelem):
  def __init__(self, type, op):
    self.child = []
    self.comment = ""
    self.one_line = False
    self.type = type
    self.op = op

  def write(self):
    if self.comment and not self.one_line:
      for l in self.comment.split('\n'):
        TDLwrite('; ' + l + '\n')
      TDLwrite('\n')

    if debug_write:
      TDLwrite('typedef\n')
      
    TDLwrite(self.type + " " + self.op + " ")
    TDLset_indent(2)
    for ch in self.child:
      ch.write()

    TDLwrite('.')
    if self.one_line and self.comment:
      TDLwrite('  ; ' + self.comment)

  def set_comment(self, comment):
    self.comment = comment

  def get_comment(self):
    return self.comment

  def set_one_line(self, one_line):
    self.one_line = one_line

  def get_one_line(self):
    return self.one_line


###########################################################################
# A TDLelem_type corresponds to an identifier (e.g. basic-verb-lex)

class TDLelem_type(TDLelem):
  def __init__(self, type):
    self.child = []
    self.type = type

  def write(self):
    if debug_write:
      TDLwrite('type\n')

    TDLwrite(self.type)


###########################################################################
# A TDLelem_coref corresponds to a coreference (e.g. #comps)

class TDLelem_coref(TDLelem):
  def __init__(self, coref):
    self.child = []
    self.coref = coref

  def write(self):
    if debug_write:
      TDLwrite('coref\n')

    TDLwrite(self.coref)


###########################################################################
# A TDLelem_conj corresponds to a list of TDL statements conjoined using
# the & operator.

class TDLelem_conj(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    if debug_write:
      TDLwrite('conj\n')

    old_i = TDLget_indent()
    for ch in self.child[0:1]:
      if ch:
        ch.write()
        last_was_feat = (isinstance(ch, TDLelem_feat));
    for ch in self.child[1:]:
      cur_is_feat = (isinstance(ch, TDLelem_feat));
      if cur_is_feat or last_was_feat:
        TDLwrite(' &\n')
        for i in range(old_i):
          TDLwrite(' ')
      else:
        TDLwrite(' & ')
      ch.write()
      last_was_feat = cur_is_feat


###########################################################################
# A TDLelem_feat corresponds to an attribute-value matrix
# (e.g. [ HEAD noun, VAL.SPR < > ])

class TDLelem_feat(TDLelem):
  def __init__(self):
    self.child = []
    self.empty_list = False

  # Does self contain only a FIRST and a REST?  If so, self can be
  # printed with '<' and '>' (and maybe as a list).
  def is_cons(self):
    if self.empty_list:
      return True
    c0 = None
    c1 = None
    if len(self.child) == 2:
      c0 = self.child[0]
      c1 = self.child[1]
    return c0 and c1 and \
           isinstance(c0, TDLelem_av) and \
           isinstance(c1, TDLelem_av) and \
           ((c0.attr == 'FIRST' and c1.attr == 'REST') or \
            (c0.attr == 'REST' and c1.attr == 'FIRST'))

  # Does self contain only a FIRST and a REST, and is the value of REST
  # either the type 'null' or also a list?  If so, self can be printed
  # as a list.
  def is_list(self):
    if self.is_cons():
      c0 = self.child[0]
      c1 = self.child[1]

      if c0.attr == 'REST' and len(c0.child) == 1:
        r = c0.child[0]
      elif c1.attr == 'REST' and len(c1.child) == 1:
        r = c1.child[0]

      if r and \
         isinstance(r, TDLelem_conj) and \
         len(r.child) == 1:
        c = r.child[0]
        return (isinstance(c, TDLelem_type) and c.type == 'null') or \
               (isinstance(c, TDLelem_feat) and c.is_list())

    return False

  def write_cons(self):
    if self.empty_list:
      TDLwrite('< >')
      return
    islist = self.is_list() # just call once and store the result
    cur = self
    first_elem = True
    while cur:
      if first_elem:
        TDLwrite('< ')
        old_i = TDLget_indent()
        first_elem = False
      else:
        # pairs will never get this far
        TDLwrite(',\n')
        for i in range(old_i):
          TDLwrite(' ')

      if cur.child[0].attr == 'REST':
        f = cur.child[1].child[0]
        r = cur.child[0].child[0].child[0]
      else:
        f = cur.child[0].child[0]
        r = cur.child[1].child[0].child[0]

      f.write() # write value of FIRST

      if islist and isinstance(r, TDLelem_type) and r.type == 'null':
        break
      elif not islist:
        TDLwrite(' . ')
        r.write()
        break
      else:
        cur = r
    TDLwrite(' >')

  def write(self):
    if debug_write:
      TDLwrite('feat\n')

    if self.is_cons():
      self.write_cons()
    else:
      TDLwrite('[ ')
      old_i = TDLget_indent()
      for ch in self.child[0:1]:
        ch.write()
      for ch in self.child[1:]:
        TDLwrite(',\n')
        for i in range(old_i):
          TDLwrite(' ')
        ch.write()
      TDLwrite(' ]')


###########################################################################
# A TDLelem_av corresponds to a single attribute-value pair
# (e.g. HEAD noun)

class TDLelem_av(TDLelem):
  def __init__(self, attr):
    self.child = []
    self.attr = attr

  def write_dotted(self):
    c = self
    if len(c.child) == 1 and isinstance(c.child[0], TDLelem_conj):
      c = c.child[0]
      if len(c.child) == 1 and isinstance(c.child[0], TDLelem_feat):
        c = c.child[0]
        if len(c.child) == 1 and isinstance(c.child[0], TDLelem_av):
          c = c.child[0]
          TDLwrite(self.attr)
          TDLwrite('.')
          c.write()
          return True
    return False

  def write(self):
    if debug_write:
      TDLwrite('av\n')

    if not self.write_dotted():
      TDLwrite(self.attr)
      TDLwrite(' ')
      for ch in self.child:
        ch.write()


###########################################################################
# A TDLelem_dlist corresponds to a diff-list (e.g. <! [ PRED "_q_rel" ] !>)

class TDLelem_dlist(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    if debug_write:
      TDLwrite('dlist\n')

    TDLwrite('<! ')
    for ch in self.child[0:1]:
      ch.write()
    for ch in self.child[1:]:
      TDLwrite(', ')
      ch.write()
    TDLwrite(' !>')

  def ordered(self):
    return True


###########################################################################
# TDL Parsing functions
# These functions, one for each of the types of TDL element, all consume
# tokens from the global list "tok".

tok = []

def TDLparse_type():
  global tok
  return TDLelem_type(tok.pop(0))

def TDLparse_coref():
  global tok
  return TDLelem_coref(tok.pop(0))

def TDLparse_av():
  global tok
  attr = tok.pop(0)
  elem = TDLelem_av(attr)
  if tok[0] == '.':
    tok.pop(0) # '.'
    cur = elem
    temp = TDLelem_conj()
    cur.add(temp)
    cur = temp
    temp = TDLelem_feat()
    cur.add(temp)
    cur = temp
    cur.add(TDLparse_av())
  else:
    elem.add(TDLparse_conj())
  return elem

def TDLparse_feat():
  global tok
  tok.pop(0) # '['
  elem = TDLelem_feat()
  while tok[0] != ']':
    elem.add(TDLparse_av())
    if tok[0] == ',':
      tok.pop(0)
  tok.pop(0) # ']'
  return elem

def TDLparse_list():
  global tok
  tok.pop(0) # '<'

  term = True       # is the list terminated (i.e. doesn't end in ...)?
  seen_dot = False  # were the items separated by a .?
  child = []
  while tok[0] != '>':
    if tok[0] == '...':
      term = False
    child.append(TDLparse_conj())
    if tok[0] == ',':
      tok.pop(0)
    elif tok[0] == '.':
      seen_dot = True
      tok.pop(0)
  tok.pop(0) # '>'

  # We've got the list elements, and the variable seen_dot tells us
  # if it ends with a null (False) or not (True).  Loop through them
  # backwards, constructing feature structures with FIRST and REST
  # as appropriate

  # first, short-circuit empty lists
  if len(child) == 0:
    elem = TDLelem_feat()
    elem.empty_list = True
    return elem

  # otherwise, deal with the contents of the child list
  elem = None
  
  # loop through all but the last child, constructing a list as we go
  for i in range(len(child) - 1):
    if elem == None:
      temp = TDLelem_feat()
      elem = temp
      cur = temp
    else:
      temp = TDLelem_conj()
      cur.add(temp)
      cur = temp
      temp = TDLelem_feat()
      cur.add(temp)
      cur = temp
    temp = TDLelem_av('FIRST')
    cur.add(temp)
    temp.add(child[i])

    # Unless we're at the end of an unterminated list...
    if term or i < len(child) - 2:
      # ...set up for the next iteration (or the finish)
      temp = TDLelem_av('REST')
      cur.add(temp)
      cur = temp # leave cur pointing to an av

  # Unless the list is unterminated...
  if term:
    # ...add the last child to the list as indicated by seen_dot
    if seen_dot:
      cur.add(child[-1])
    else:
      if elem == None:
        temp = TDLelem_feat()
        elem = temp
        cur = temp
      else:
        temp = TDLelem_conj()
        cur.add(temp)
        cur = temp
        temp = TDLelem_feat()
        cur.add(temp)
        cur = temp
      temp = TDLelem_av('FIRST')
      cur.add(temp)
      temp.add(child[-1])
      temp = TDLelem_av('REST')
      cur.add(temp)
      cur = temp
      temp = TDLelem_conj()
      cur.add(temp)
      cur = temp
      temp = TDLelem_type('null')
      cur.add(temp)
      cur = temp
  
  return elem

def TDLparse_dlist():
  global tok
  tok.pop(0) # '<!'
  elem = TDLelem_dlist()
  while tok[0] != '!>':
    elem.add(TDLparse_conj())
    if tok[0] == ',':
      tok.pop(0)
  tok.pop(0) # '!>'
  return elem

def TDLparse_term():
  global tok
  if isid(tok[0]) or tok[0][0] == '"':
    return TDLparse_type()
  elif tok[0][0] == '#':
    return TDLparse_coref()
  elif tok[0] == '[':
    return TDLparse_feat()
  elif tok[0] == '<':
    return TDLparse_list()
  elif tok[0] == '<!':
    return TDLparse_dlist()

def TDLparse_conj():
  global tok
  elem = TDLelem_conj()
  elem.add(TDLparse_term())
  while tok[0] == '&':
    tok.pop(0)
    elem.add(TDLparse_term())
  return elem

def TDLparse_typedef():
  global tok
  type = tok.pop(0) # the type name
  op = tok.pop(0)
  while not op in (':=', ':<', ':+'):
    type += '_' + op  # turn spaces in the type name to _'s
    op = tok.pop(0)
  elem = TDLelem_typedef(type, op)
  elem.add(TDLparse_conj())
  tok.pop(0) # '.'
  return elem
  
def TDLparse(s):
  global tok
  tok = TDLtokenize(s)
  return TDLparse_typedef()


###########################################################################
# TDLmergeable
#   type:  e1.type == e2.type
#   coref: e1.coref == e2.coref
#   av:    same attr names
#   conj:  always true
#   feat:  always true
#   list:  always true
#   dlist: always true

def TDLmergeable(e1, e2):
  if isinstance(e1, TDLelem_typedef) and isinstance(e2, TDLelem_typedef):
    return e1.type == e2.type and e1.op == e2.op
  if isinstance(e1, TDLelem_type) and isinstance(e2, TDLelem_type):
    return e1.type == e2.type
  if isinstance(e1, TDLelem_coref) and isinstance(e2, TDLelem_coref):
    return e1.coref == e2.coref
  if isinstance(e1, TDLelem_av) and isinstance(e2, TDLelem_av):
    return e1.attr == e2.attr
  if (isinstance(e1, TDLelem_conj) and isinstance(e2, TDLelem_conj)) or \
     (isinstance(e1, TDLelem_feat) and isinstance(e2, TDLelem_feat)) or \
     (isinstance(e1, TDLelem_dlist) and isinstance(e2, TDLelem_dlist)):
    return True


###########################################################################
# TDLmerge takes two TDLelem_typedef, merges them together as high up in
# the tree as possible, and returns the merged TDLelem_typedef.

def TDLmerge(e1, e2):
  if TDLmergeable(e1, e2):
    e0 = copy.copy(e1)
    e0.child = []

    c1 = e1.get_comment()
    c2 = e2.get_comment()
    c0 = c1
    if c1 != c2:  # if the comments are the same, don't duplicate
      if len(c1) and len(c2):
        c0 += '\n\n'
      c0 += c2
    e0.set_comment(c0)

    # if the elements are ordered (list or dlist), merge the list
    # items in order.  That is, <a,b,c> + <A,B,C> = <a&A,b&B,c&C>.
    if e1.ordered():
      for i in range(max(len(e1.child), len(e2.child))):
        if i < len(e1.child) and i < len(e2.child) and \
           TDLmergeable(e1.child[i], e2.child[i]):
          e0.add(TDLmerge(e1.child[i], e2.child[i]))
        else:
          if i < len(e1.child):
            e0.add(copy.deepcopy(e1.child[i]))
          if i < len(e2.child):
            e0.add(copy.deepcopy(e2.child[i]))
    else:
      for c in e1.child + e2.child:
        handled = False
        for c0 in e0.child:
          if TDLmergeable(c0, c):
            e0.child.remove(c0)
            e0.add(TDLmerge(c0, c))
            handled = True
            break
        if not handled:
          e0.add(copy.deepcopy(c))
  else:
    e0 = TDLelem_conj()
    e0.add(copy.deepcopy(e1))
    e0.add(copy.deepcopy(e2))

  e0.sort()

  return e0

###########################################################################
# A TDLfile contains the contents of a single .tdl file.  It is initialized
# with a file name, to which it will be eventually saved.  Statements can
# be added to the TDLfile using the add() method.

class TDLfile:
  def __init__(self, file_name):
    self.file_name = file_name  # we'll eventually save to this file
    self.typedefs = []

  def save(self):
    f = open(self.file_name, 'w')
    TDLset_file(f)
    l = len(self.typedefs)
    for i in range(0, l):
      self.typedefs[i].write()
      TDLwrite('\n')  # always at least one line break...
      # ...and then another if it's not two one_line elements in a row
      if i < l - 1:
        if not (self.typedefs[i].one_line and self.typedefs[i+1].one_line):
          TDLwrite('\n')
    f.close()

  def dump(self):
    TDLset_file(sys.stdout)
    for t in self.typedefs:
      t.write()
    print '\n'

  ###########################################################################
  # Add a type definition to this file, merging with an existing definition
  # if possible.
  def add(self, tdl_type, comment = '', one_line = False):
    typedef = TDLparse(tdl_type)
    typedef.set_comment(comment)
    typedef.set_one_line(one_line)
    handled = 0
    for i in range(len(self.typedefs)):
      if TDLmergeable(self.typedefs[i], typedef):
        self.typedefs[i] = TDLmerge(self.typedefs[i], typedef)
        handled = 1
        break
        
    if not handled:
      self.typedefs.append(typedef)

  ###########################################################################
  # ERB 2006-09-22 Add just a comment to an existing type in the file.
  def comment(self, tdl_type, comment):
    self.add(tdl_type + ':= [].', comment)

  ###########################################################################
  # Add a literal to this file, which doesn't merge
  def add_literal(self, literal, comment = ''):
    l = TDLelem_literal(literal)
    l.set_comment(comment)
    self.typedefs.append(l)

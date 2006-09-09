#!/usr/local/bin/python

######################################################################
# imports
###########################################################################

import sys
import os
import copy

###########################################################################
# TDL Tokenization
#
# isid(c)
#   return true iff c can appear in TDL identifiers
#
# TDLtokenize(s)
#  convert s list of TDL tokens and return that list
###########################################################################

def isid(s):
  for c in s:
    if not c.isalnum() and c != '-' and c != '+' and c != '_':
      return 0

  return 1
  
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
      val.append(s[1:i]) # omit the open quote
      s = s[i + 1:] # ...and the close quote
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

tdl_file = sys.stdout
def TDLset_file(f):
  global tdl_file
  tdl_file = f

tdl_indent = 0
def TDLwrite(s):
  global tdl_indent
  global tdl_file
  tdl_file.write(s)
  i = s.find('\n')
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
    return 0

  def set_comment(self, comment):
    pass

  def get_comment(self):
    return ''


###########################################################################
# A TDLtypedef corresponds to a statement like:
#
# subj-head-phrase := basic-head-subj-phrase & head-final &
#  [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].
#
# ...as well as a comment preceding the statement

class TDLelem_typedef(TDLelem):
  def __init__(self, type, op):
    self.child = []
    self.comment = ""
    self.type = type
    self.op = op

  def write(self):
    for l in self.comment.split('\n'):
      TDLwrite('; ' + l + '\n')
    TDLwrite('\n')
    
    TDLwrite(self.type + " " + self.op + " ")
    for ch in self.child:
      ch.write()
    TDLwrite(' .\n\n')

  def set_comment(self, comment):
    self.comment = comment

  def get_comment(self):
    return self.comment


###########################################################################
# A TDLelem_type corresponds to an identifier (e.g. basic-verb-lex)

class TDLelem_type(TDLelem):
  def __init__(self, type):
    self.child = []
    self.type = type

  def write(self):
    TDLwrite(self.type)


###########################################################################
# A TDLelem_coref corresponds to a coreference (e.g. #comps)

class TDLelem_coref(TDLelem):
  def __init__(self, coref):
    self.child = []
    self.coref = coref

  def write(self):
    TDLwrite(self.coref)


###########################################################################
# A TDLelem_conj corresponds to a list of TDL statements conjoined using
# the & operator.

class TDLelem_conj(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    for ch in self.child[0:1]:
      ch.write()
    for ch in self.child[1:]:
      TDLwrite(' & ')
      ch.write()


###########################################################################
# A TDLelem_feat corresponds to an attribute-value matrix
# (e.g. [ HEAD noun, VAL.SPR < > ])

class TDLelem_feat(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    TDLwrite('[ ')
    old_i = tdl_indent
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

  def write(self):
    TDLwrite(self.attr)
    TDLwrite(' ')
    for ch in self.child:
      ch.write()


###########################################################################
# A TDLelem_list corresponds to a list (e.g. < LOCAL.CAT.HEAD det, OPT - >)

class TDLelem_list(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    TDLwrite('< ')
    for ch in self.child[0:1]:
      ch.write()
    for ch in self.child[1:]:
      TDLwrite(', ')
      ch.write()
    TDLwrite(' >')

  def ordered(self):
    return 1
  

###########################################################################
# A TDLelem_dlist corresponds to a diff-list (e.g. <! [ PRED "_q_rel" ] !>)

class TDLelem_dlist(TDLelem):
  def __init__(self):
    self.child = []

  def write(self):
    TDLwrite('<! ')
    for ch in self.child[0:1]:
      ch.write()
    for ch in self.child[1:]:
      TDLwrite(', ')
      ch.write()
    TDLwrite(' !>')

  def ordered(self):
    return 1


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
  while tok[0] == '.':
    attr += tok.pop(0) # '.'
    attr += tok.pop(0) # attr
  elem = TDLelem_av(attr)
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
  elem = TDLelem_list()
  while tok[0] != '>':
    elem.add(TDLparse_conj())
    if tok[0] == ',':
      tok.pop(0)
  tok.pop(0) # '>'
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
  if isid(tok[0]):
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
  op = tok.pop(0) # ':=', ':<', or ':+'
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
     (isinstance(e1, TDLelem_list) and isinstance(e2, TDLelem_list)) or \
     (isinstance(e1, TDLelem_dlist) and isinstance(e2, TDLelem_dlist)):
    return 1


###########################################################################
# TDLmerge takes two TDLelem_typedef, merges them together as high up in
# the tree as possible, and returns the merged TDLelem_typedef.

def TDLmerge(e1, e2):
  if TDLmergeable(e1, e2):
    e0 = copy.copy(e1)
    e0.child = []
    e0.set_comment(e1.get_comment() + '\n\n' + e2.get_comment())
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
        handled = 0
        for c0 in e0.child:
          if TDLmergeable(c0, c):
            e0.child.remove(c0)
            e0.add(TDLmerge(c0, c))
            handled = 1
            break
        if not handled:
          e0.add(copy.deepcopy(c))
    return e0
  else:
    e0 = TDLelem_conj()
    e0.add(copy.deepcopy(e1))
    e0.add(copy.deepcopy(e2))
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
    for t in self.typedefs:
      t.write()
    f.close()

  ###########################################################################
  # Add a type definition to this file, merging with an existing definition
  # if possible.
  def add(self, tdl_type, comment = ''):
    typedef = TDLparse(tdl_type)
    typedef.set_comment(comment)
    handled = 0
    for i in range(len(self.typedefs)):
      if TDLmergeable(self.typedefs[i], typedef):
        self.typedefs[i] = TDLmerge(self.typedefs[i], typedef)
        handled = 1
        break
        
    if not handled:
      self.typedefs.append(typedef)


#e1 = TDLparse('shp := [ H.S.L.C.H noun ].')
#e2 = TDLparse('shp := [ H.S.L.C.H verb ].')

#e1 = TDLparse('shp := bhsp.')
#e2 = TDLparse('shp := [ H.S.L.C.H verb ].')

#e1 = TDLparse('shp := bhsp & hf & [ H.S.L.C.H noun ].')
#e2 = TDLparse('shp := bhsp & hf & [ H.S.L.C.H verb ].')

#e1 = TDLparse('shp := bhsp & hf & [ H.S.L.C.H noun ].')
#e2 = TDLparse('shp := [ H.S.L.C.H verb ] & hf & bhsp.')

#e1 = TDLparse('shp := [ H.S.L.C.V.C < > ].')
#e2 = TDLparse('shp := [ H.S.L.C.V.C < n > ].')

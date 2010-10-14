from gmcs.lib import Hierarchy, HierarchyNode

# Base class for lexical types (lexicon.py) and lexical rules (morphotatics.py)

################################
### CONSTANTS (in principle) ###
################################

ALL_LEX_TYPES = ['noun', 'verb', 'det', 'aux', 'adj']


LEXICAL_CATEGORIES = ['noun', 'verb', 'det', 'adj']

# lexical_supertypes is a dictionary mapping the choices file
# encodings to the actual lex-type identifiers.
LEXICAL_SUPERTYPES = {'noun':'noun-lex',
                      'verb':'verb-lex',
                      'iverb':'intransitive-verb-lex',
                      'tverb':'transitive-verb-lex',
                      'mverb':'main-verb-lex',
                      'det':'determiner-lex',
                      'aux':'aux-lex',
                      'adj':'adjective-lex'}
###############
### CLASSES ###
###############

class MorphotacticNode(HierarchyNode):
  def __init__(self, key, name=None, pc=None, parents=None, supertypes=None):
    HierarchyNode.__init__(self, key, parents=parents)
    self.name = name or ''
    self.pc = pc
    self.constraints = {'req-fwd':set(), 'req-bkwd':set(), 'forbid':set()}
    self.disjunctive_flag_sets = set()
    self.flags = {'in':{},'out':{}}
    self.supertypes = supertypes or set()
    self.identifier_suffix = ''
    #self.input_lrt = None

  def relatives(self, relation):
    # 'input' is a fake relation. It's just the parents of the position
    # class (useful when the morphotactic node is an LRT)
    if relation == 'input':
      return self.pc.parents()
    else:
      return HierarchyNode.relatives(self, relation)

  def inputs(self):
    return self.relatives('input')

  def identifier(self):
    return '-'.join([self.name, self.identifier_suffix])

  def input_span(self):
    return PositionClass.input_span(self.pc)

  def precedes(self, other):
    """
    Return True if self occurs before other. This is True if self's
    position class is among the position classes of other's input span.
    """
    return self.pc in [o.pc for o in other.input_span()]

  #def extended_input_span(self):
  #  e_i_s = self.input_span()
  #  e_i_s.update(dict(x for i in e_i_s
  #                      for x in self.descendants().items()))
  #  return e_i_s

  #def generalized_input_span(self):
  #  g_i_s = self.extended_input_span()
  #  g_i_s.update(dict(x for i in self.input_span()
  #                      for x in self.ancestors().items()))
  #  return g_i_s

class PositionClass(MorphotacticNode):
  """
  """
  def __init__(self, key, name, parents=None, order=None,
               identifier_suffix=None):
    MorphotacticNode.__init__(self, key, name, parents=parents)
    self.l_hierarchy = Hierarchy()
    self.nodes = self.l_hierarchy.nodes # for convenience
    self.order = order
    self.pc = self
    self.identifier_suffix = identifier_suffix or 'lex-rule-super'

  def __repr__(self):
    return 'PositionClass(' + self.identifier() + ')'

  def add_node(self, node):
    node.pc = self
    return self.l_hierarchy.add_node(node)

  def relate_parent_child(self, parent, child):
    return self.l_hierarchy.relate_parent_child(parent, child)

  def input_span(self):
    return self.hierarchy.get_lineage(key=self.key, relation='input')

  def percolate_supertypes(self):
    # roots are those nodes without parents
    roots = [n for n in self.nodes.values() if len(n.parents()) == 0]
    root_sts = [r.percolate_supertypes() for r in roots]
    if len(root_sts) == 0:
      return
    common_sts = set.intersection(*root_sts)
    # update the supertype sets
    self.supertypes.update(common_sts)
    for r in roots:
      r.supertypes.difference_update(common_sts)

class LexicalType(MorphotacticNode):
  def __init__(self, key, name, parents=None):
    MorphotacticNode.__init__(self, key, name=name, parents=parents)
    self.identifier_suffix = 'lex'

class LexicalRuleType(MorphotacticNode):
  """
  A simple class for managing the properties of lexical rule types.
  """
  def __init__(self, key, name, pc=None, parents=None, supertypes=None):
    MorphotacticNode.__init__(self, key, name, pc, parents, supertypes)
    self.supertypes = supertypes or set()
    self.features = {}
    self.lris = {}
    self.identifier_suffix = 'lex-rule'

  def inputs(self):
    return self.pc.inputs

  def percolate_supertypes(self):
    # Only supertypes existing in all LRIs and subtypes can be percolated
    lri_sts = [lri.supertypes for lri in self.lris.values()]
    # Remember to do this recursively
    lrt_sts = [c.percolate_supertypes() for c in self.children().values()]
    # func(*[list]) is the Pythonic way of making a list into function
    # parameters. so func(*[1,2]) => func(1, 2)
    common_sts = set.intersection(*(lri_sts + lrt_sts))
    # now update the supertype sets
    self.supertypes.update(common_sts)
    for lri in self.lris.values():
      lri.supertypes.difference_update(common_sts)
    for c in self.children().values():
      c.supertypes.difference_update(common_sts)
    # for the recursive part, remember to return the updated set of supertypes
    return self.supertypes

class LexicalRuleInstance(object):
  def __init__(self, orthography, lrt, supertypes):
    self.orthography = orthography
    self.lrt = lrt
    self.supertypes = supertypes


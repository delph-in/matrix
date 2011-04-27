from gmcs.lib import Hierarchy, HierarchyNode

# Base class for lexical types (lexicon.py) and lexical rules (morphotatics.py)

################################
### CONSTANTS (in principle) ###
################################

# all types of lexical items (on lexicon page)
ALL_LEX_TYPES = ['noun', 'verb', 'det', 'aux', 'adj']

# types used for lexical rules (verb and aux are merged)
LEXICAL_CATEGORIES = ['noun', 'verb', 'det', 'adj']

# lexical_supertypes is a dictionary mapping the choices file
# encodings to the actual lex-type identifiers.
LEXICAL_SUPERTYPES = {'noun':'noun-lex',
                      'verb':'verb-lex',
                      'iverb':'intransitive-verb-lex',
                      'tverb':'transitive-verb-lex',
                      'dverb':'ditransitive-verb-lex',
                      'mverb':'main-verb-lex',
                      'det':'determiner-lex',
                      'aux':'aux-lex',
                      'adj':'adjective-lex'}
###############
### CLASSES ###
###############

class MorphotacticNode(HierarchyNode):
  def __init__(self, key, name=None, pc=None, parents=None, supertypes=None, instance=False):
    HierarchyNode.__init__(self, key, parents=parents)
    self.name = name or ''
    self.pc = pc
    self.constraints = {'req-fwd':{}, 'req-bkwd':{}, 'forbid':{}}
    self.disjunctive_flag_sets = {}
    self.flags = {'in':{},'out':{}}
    self.supertypes = supertypes or set()
    self.identifier_suffix = ''
    # tdl order is used for sorting the rules as the occur in the tdl
    # PCs are integers (1, 2, 3), LRTs are floats (1.1, 2,1, 2.2, etc)
    self.tdl_order = 0
    #self.input_lrt = None
    # Track for each mn whether it is an instance (lex entry, lex rule) or type.
    self.instance = instance

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
    return self.pc in [o.pc for o in other.input_span().values()]

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

  def roots(self):
    return [n for n in self.nodes.values() if len(n.parents()) == 0]

  def input_span(self):
    return self.hierarchy.get_lineage(key=self.key, relation='input')

  def valid_inputs(self):
    all_inps = self.input_span().values()
    # there are two conditions preventing an ancestor from being an input:
    # 1. A node, or all its ancestors, req-fwd an intervening node
    # 2. This pc, or all its followers, req-bkwd an intervening node

    # implement these later.. try to be efficient
    return all_inps

  def percolate_supertypes(self):
    # roots are those nodes without parents
    roots = self.roots()
    root_sts = [r.percolate_supertypes() for r in roots]
    if len(root_sts) == 0:
      return
    common_sts = reduce(set.intersection, root_sts)
    # update the supertype sets
    self.supertypes.update(common_sts)
    for r in roots:
      r.supertypes.difference_update(common_sts)
    # if, after all that, the PC has no supertypes, make it generic
    if len(self.supertypes) == 0 \
       and self.identifier_suffix == 'lex-rule-super':
      self.supertypes.add('lex-rule')

class LexicalType(MorphotacticNode):
  def __init__(self, key, name, parents=None, entry=False):
    MorphotacticNode.__init__(self, key, name=name, parents=parents, instance=entry)
    self.identifier_suffix = 'lex'

  def __repr__(self):
    return 'LexicalType(' + self.key + ')'

class LexicalRuleType(MorphotacticNode):
  """
  A simple class for managing the properties of lexical rule types.
  """
  def __init__(self, key, name, pc=None, parents=None, supertypes=None):
    MorphotacticNode.__init__(self, key, name, pc, parents, supertypes)
    self.supertypes = supertypes or set()
    self.features = {}
    self.lris = []
    self.identifier_suffix = 'lex-rule'

  def __repr__(self):
    return 'LexicalRuleType(' + self.key + ')'

  def inputs(self):
    return self.pc.inputs()

  def percolate_supertypes(self):
    # base condition: we're on a leaf type
    if len(self.children()) == 0:
      return self.supertypes
    # Remember to do this recursively
    lrt_sts = [c.percolate_supertypes() for c in self.children().values()]
    # func(*[list]) is the Pythonic way of making a list into function
    # parameters. so func(*[1,2]) => func(1, 2)
    common_sts = reduce(set.intersection, lrt_sts)
    # now update the supertype sets
    self.supertypes.update(common_sts)
    for c in self.children().values():
      c.supertypes.difference_update(common_sts)
    # for the recursive part, remember to return the updated set of supertypes
    return self.supertypes

  def all_supertypes(self):
    """
    Return the LRT's supertypes and parents or, if there are no
    parents, the LRT's PC.
    """
    parents = [lrt.identifier() for lrt in self.parents().values()]
    if len(parents) == 0:
      parents = [self.pc.identifier()]
    return set(parents).union(self.supertypes)

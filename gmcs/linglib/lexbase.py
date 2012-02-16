from gmcs.lib import Hierarchy, HierarchyNode

# Base class for lexical types (lexicon.py) and lexical rules (morphotatics.py)

################################
### CONSTANTS (in principle) ###
################################

# all types of lexical items (on lexicon page)
ALL_LEX_TYPES = ['noun', 'verb', 'det', 'aux', 'adj', 'adv', 'comp', 'adp','cop']

# types used for lexical rules (verb and aux are merged)
LEXICAL_CATEGORIES = ['noun', 'verb', 'det', 'adj', 'adv', 'comp', 'adp']

# lexical_supertypes is a dictionary mapping the choices file
# encodings to the actual lex-type identifiers.
LEXICAL_SUPERTYPES = {'noun':'noun-lex',
                      'verb':'verb-lex',
                      'iverb':'intransitive-verb-lex',
                      'tverb':'transitive-verb-lex',
                      'dverb':'ditransitive-verb-lex',
                      'sc2verb':'s-comp-2nd-arg-verb-lex',
                      'scontrverb':'subj-contr-transitive-verb-lex',
                      'oraisverb':'obj-raising-verb-lex',
                      'reflverb':'refl-verb-lex',
                      'partverb':'particle-trans-verb-lex',
                      'mverb':'main-verb-lex',
                      'det':'determiner-lex',
                      'aux':'aux-lex',
                      'adj':'adjective-lex',
                      'adv':'adverb-lex',
                      'comp':'complementizer-lex',
                      'adp':'adpositional-lex',
                      'cop':'copula-lex'}
###############
### CLASSES ###
###############

class MorphotacticNode(HierarchyNode):
  def __init__(self, key, name=None, pc=None, parents=None, supertypes=None,
               instance=False):
    HierarchyNode.__init__(self, key, parents=parents)
    self.name = name or ''
    self.pc = pc
    self.constraints = {'req-fwd':{}, 'req-bkwd':{}, 'forbid':{}}
    self.disjunctive_flag_sets = {}
    self.flags = {'in':{},'out':{}}
    self.supertypes = supertypes or set()
    self.comparative = ''
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
    if self.identifier_suffix:
      return '-'.join([self.name, self.identifier_suffix])
    else:
      return self.name

  def input_span(self):
    return PositionClass.input_span(self.pc)

  def precedes(self, other):
    """
    Return True if self occurs before other. This is True if self's
    position class is among the position classes of other's input span.
    """
    return self.pc in [o.pc for o in other.input_span().values()]

  def percolate_down(self, items, validate):
    """
    Copy the relevant items on the current node down to all child nodes.
    If no valid item exists on a leaf node, give it a default item.
    """
    if len(self.children()) > 0:
      # The following assumes incompatible types won't be merged onto
      # common descendants (this should be validated)
      for c in self.children().values():
        items(c).update(items(self))
        c.percolate_down(items, validate)
    # if it is a leaf node, make sure it is valid
    else:
      validate(self)

  def percolate_up(self, items, redundancies):
    # base condition: we're on a leaf type
    if len(self.children()) == 0:
      vals = items(self)
      return set(vals.items()) if type(vals) is dict else set(vals)
    # Can't do set.intersection(*list) until Python2.6, so using reduce
    common_items = reduce(set.intersection,
                          [c.percolate_up(items, redundancies)
                           for c in self.children().values()])
    if common_items:
      # now update the current node's values
      items(self).update(common_items)
      # and schedule the common values from descendants to be removed
      for c in self.children().values():
        redundancies[c].update(common_items)
    return common_items

class PositionClass(MorphotacticNode):
  """
  """
  def __init__(self, key, name, parents=None, order=None,
               identifier_suffix=None, lex_rule=True):
    MorphotacticNode.__init__(self, key, name, parents=parents)
    self.l_hierarchy = Hierarchy()
    self.nodes = self.l_hierarchy.nodes # for convenience
    self.order = order
    self.pc = self
    self.identifier_suffix = identifier_suffix or 'lex-rule-super'
    self.is_lex_rule = lex_rule

  def __repr__(self):
    return 'PositionClass(' + self.identifier() + ')'

  def add_node(self, node):
    node.pc = self
    return self.l_hierarchy.add_node(node)

  def relate_parent_child(self, parent, child):
    return self.l_hierarchy.relate_parent_child(parent, child)

  def roots(self):
    """
    Return the list of nodes at the top of the lexical rule type
    hierarchy for this position class. Note that there may be more than
    one root for any given position class.
    """
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

class LexicalType(MorphotacticNode):
  def __init__(self, key, name, parents=None, entry=False):
    MorphotacticNode.__init__(self, key, name=name, parents=parents,
                              instance=entry)
    # lexical entries don't have identifier suffixes
    if entry:
      self.identifier_suffix = ''
    else:
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

  def all_supertypes(self):
    """
    Return the LRT's supertypes and parents or, if there are no
    parents, the LRT's PC.
    """
    parents = [lrt.identifier() for lrt in self.parents().values()]
    if len(parents) == 0:
      parents = [self.pc.identifier()]
    return set(parents).union(self.supertypes)

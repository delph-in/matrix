
class HierarchyNode:
  def __init__(self, key, parents=None, children=None):
    self.key = key
    self.relations = {}
    self.relations['parent'] = parents or set()
    self.relations['child'] = children or set()

  def relatives(self, relationship):
    return self.relations.get(relationship, set())

  def parents(self):
    return self.relations['parent']

  def children(self):
    return self.relations['child']

class Hierarchy:
  def __init__(self):
    self.nodes = {}
    self.__cache = {}

  def add_node(self, node):
    if node.key in self.nodes:
      return None
    self.nodes[node.key] = node
    for parent in node.parents():
      self.nodes[parent].children().add(node.key)
    for child in node.children:
      self.nodes[child].parents().add(node.key)
    self.__cache = {}
    return node

  def get_lineage(self, key=None, node=None, relationship=None):
    if key is not None and node is None:
      node = self.nodes[key]
    return self.find_lineage(node, relationship)

  def find_lineage(self, node, relationship):
    if relationship in self.__cache.setdefault(node.key,{}):
      return self.__cache[node.key][relationship]
    relatives = node.relatives(relationship)
    self.__cache[node.key][relationship] = \
      relatives.union(x for r in relatives
                        for x in self.find_lineage(r, relationship))
    return self.__cache[node.key][relationship]

  def get_ancestors(self, key=None, node=None):
    return self.find_lineage(key, node, 'parent')

  def get_descendants(self, key=None, node=None):
    return self.find_lineage(key, node, 'child')

########################
### HELPER FUNCTIONS ###
########################

# these functions should eventually be folded into a revamped
# Hierarchy class, using hierarchies instead of dictionaries

#def subsumes(subsumers, subsumees, hierarchy,
#             partial=True, allow_outliers=False):
#  if len(subsumees) == 0 or len(subsumers) == 0:
#    return False
#  op = any if partial else all
#  all_subsumed = subsumed(subsumers, hierarchy)
#  return_value = op(x in subsumees for x in all_subsumed)
#  if not allow_outliers:
#
#  return 
#
#
#def supersumes(supersumers, supersumees, hierarchy,
#               partial=True, allow_outliers=False):
#  pass
#
#def subsumed(subsumers, hierarchy):
#  all_subsumed = set()
#  for x in subsumers:
#    all_subsumed.update(hierarchy[x])
#  return all_subsumed
#
#def supersumed(supersumer, hierarchy):
#  pass

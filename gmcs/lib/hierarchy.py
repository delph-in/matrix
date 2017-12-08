
class HierarchyNode(object):
    def __init__(self, key, parents=None, children=None):
        self.key = key
        self.relations = {}
        self.relations['parent'] = parents or {}
        self.relations['child'] = children or {}
        self.hierarchy = None

    def relate(self, other, relation):
        self.relations[relation][other.key] = other

    def relatives(self, relation):
        return self.relations.get(relation, {})

    def parents(self):
        return self.relations['parent']

    def ancestors(self):
        if self.hierarchy == None:
            return None
        return self.hierarchy.get_ancestors(node=self)

    def children(self):
        return self.relations['child']

    def descendants(self):
        if self.hierarchy == None:
            return None
        return self.hierarchy.get_descendants(node=self)

class Hierarchy(object):
    def __init__(self):
        self.nodes = {}
        self.__cache = {}

    def __str__(self):
        return str(self.nodes)

    def add_node(self, node):
        if node.key in self.nodes:
            return None
        # register it with and add it to the hierarchy
        node.hierarchy = self
        self.nodes[node.key] = node
        # add corollary relationships
        for parent in node.parents().values():
            parent.children()[node.key] = node
        for child in node.children().values():
            child.parents()[node.key] = node
        # adding nodes invalidates the cache
        self.__cache = {}
        return node

    def relate_parent_child(self, parent, child):
        parent.relate(child, 'child')
        child.relate(parent, 'parent')

    def get_lineage(self, key=None, node=None, relation=None):
        if key is not None and node is None:
            node = self.nodes[key]
        return self.find_lineage(node, relation)

    def find_lineage(self, node, relation):
        if relation in self.__cache.get(node.key,{}):
            return self.__cache[node.key][relation]
        self.__cache.setdefault(node.key, {})
        self.__cache[node.key][relation] = node.relatives(relation)
        self.__cache[node.key][relation].update(
            dict(i for n in self.__cache[node.key][relation].values()
                 for i in self.find_lineage(n, relation).items()))
        return self.__cache[node.key][relation]

    def get_ancestors(self, key=None, node=None):
        return self.get_lineage(key, node, 'parent')

    def get_descendants(self, key=None, node=None):
        return self.get_lineage(key, node, 'child')

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

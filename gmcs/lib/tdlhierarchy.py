######################################################################
# TDLHierarchy class

# TDLHierarchy:
# A class for storing, operating on, and saving to TDL a type
# hierarchy.  The hierarchy is stored as an array, each element of
# which is itself an array of three strings: a type, a supertype, and
# a comment.
class TDLHierarchy:
    # Initialize
    def __init__(self, name, type = ''):
        self.name = name
        self.type = type
        self.hierarchy = []

        self.supertypes = {}
        self.subtypes = {}
        self.leaves = set()
        self.coverage = {}


    def is_empty(self):
        return len(self.hierarchy) == 0


    # Add a type to the hierarchy
    def add(self, type, supertype, comment = ''):
        self.hierarchy += [ [ type, supertype, comment ] ]

    # Save the hierarchy to the passed TDLfile object.  The resulting
    # TDL will look like this:
    #
    # type1 := supertype1  ; comment1
    # type2 := supertype2  ; comment2
    # type3 := supertype3  ; comment3
    # ...
    def save(self, tdl_file, define = True):
        tdl_file.set_section('features')

        tdl_file.add_literal(';;; ' + self.name[0:1].upper() + self.name[1:])

        if define:
            tdl_file.add(self.name + ' := *top*.', '', True)

        for h in self.hierarchy:
            tdl_file.add(h[0] + ' := ' + h[1] + '.', h[2], True)

    # For each type in the hierarchy, calculate which types it is
    # the immediate supertype of, and save this information for later.
    def __calc_supertypes(self):
        self.supertypes = {}
        for h in self.hierarchy:
            if not h[0] in self.supertypes:
                self.supertypes[h[0]] = set()
            if not h[1] in self.supertypes:
                self.supertypes[h[1]] = set()

            self.supertypes[h[0]].add(h[1])


    # For each type in the hierarchy, calculate which types it is
    # the immediate subtype of, and save this information for later.
    def __calc_subtypes(self):
        self.subtypes = {}
        for h in self.hierarchy:
            if not h[0] in self.subtypes:
                self.subtypes[h[0]] = set()
            if not h[1] in self.subtypes:
                self.subtypes[h[1]] = set()

            self.subtypes[h[1]].add(h[0])


    # Calculate the leaf types (i.e. types with no subtypes) and save
    # this information for later.
    def __calc_leaves(self):
        self.__calc_subtypes()

        self.leaves = set()
        for st in self.subtypes:
            if len(self.subtypes[st]) == 0:
                self.leaves.add(st)


    # For each type in the hierarchy, calculate which leaf types it
    # covers, and save this information for later.
    def __calc_coverage(self):
        self.__calc_leaves()
        self.__calc_supertypes()

        self.coverage = {}
        for l in self.leaves:
            working = [ l ]
            while working:
                w = working[0]
                del working[0]
                if w != '*top*':
                    if not w in self.coverage:
                        self.coverage[w] = set()
                    self.coverage[w].add(l)
                    for st in self.supertypes[w]:
                        working += [ st ]


    # Search the hierarchy for a type and return its comment, if any
    def get_comment(self, type):
        for h in self.hierarchy:
            if h[0] == type:
                return h[2]

        return ''


    # Search the hierarchy for a type covering all the types in type_set
    # and return it.  Type hierarchies as described in the questionnaire
    # may be insufficient for some purposes.  For example, implementing
    # the scale hierarchy of a direct-inverse language may require the
    # existence of a grouping of leaf types that requires that does not
    # exist.  This method will add such types to the hierarchy as
    # necessary.
    def get_type_covering(self, type_set):

        type_list = list(type_set)
        if len(type_list) == 1:
            return type_list[0]

        if type(type_set) == 'list':
            type_set = set(type_set)

        self.__calc_coverage()
        cov = self.coverage

        # type_set may contain non-leaves, so construct a new all-leaf set
        new_set = set()
        for e in type_set:
            for l in cov[e]:
                new_set.add(l)

        # check for an existing type covering the right set of leaves
        for k in cov:
            if cov[k] == new_set:
                return k

        # Need to create a new type in the hierarchy:
        # If there are types in the hierarchy that have the same coverage,
        # then the approach where we distinguish nodes by their coverage
        # won't work.  In that case, simply create a new supertype under
        # the root.
        bad_hierarchy = False
        for k in cov:
            for l in cov:
                if k != l and cov[k] == cov[l]:
                    bad_hierarchy = True
                    break

        supers = []
        subs = []
        if bad_hierarchy:
            supers = [self.name]
            subs = type_set
        else:
            # Find types in the hierarchy that are supersets and subsets of
            for k in cov:
                if cov[k].issuperset(new_set):
                    supers += [ k ]
                elif cov[k].issubset(new_set):
                    subs += [ k ]

            # prune supers and subs
            toremove = set()
            for i in range(len(supers) - 1, -1, -1):
                for j in range(len(supers) -1, -1, -1):
                    if i != j and cov[supers[i]].issuperset(cov[supers[j]]):
                        toremove.add(i)
            remove_array = [e for e in toremove]
            remove_array.sort(reverse=True)
            for i in remove_array:
                del(supers[i])

            toremove = set()
            for i in range(len(subs) - 1, -1, -1):
                for j in range(len(subs) -1, -1, -1):
                    if i != j and cov[subs[i]].issubset(cov[subs[j]]):
                        toremove.add(i)
            remove_array = [e for e in toremove]
            remove_array.sort(reverse=True)
            for i in remove_array:
                del(subs[i])

        # figure out the name of the new type
        new_type = ''
        for h in self.hierarchy:
            covh = cov[h[0]]
            if len(covh.intersection(new_set)) == 0 and \
                            len(covh.union(new_set)) == len(self.leaves):
                new_type = 'non-' + h[0]
                break
        if not new_type:
            new_type = '+'.join(subs)

        # now insert the new type between supers and subs, making sure to
        # remove any direct inheritance of the subs by the supers
        for i in range(len(self.hierarchy) - 1, -1, -1):
            if self.hierarchy[i][0] in subs and self.hierarchy[i][1] in supers:
                del(self.hierarchy[i])
        for s in supers:
            self.hierarchy += [ [new_type, s, ''] ]
        for s in subs:
            self.hierarchy += [ [s, new_type, ''] ]

        return new_type


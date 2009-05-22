#!/usr/local/bin/python2.5

import re

######################################################################
# This file contains code implementing post-permutes.  These are
# like stringmods, except that they are applied to strings after
# permutation.  They are intended to be used for phenomena that
# require recursive filtering.  For example, in order to validate
# coordination, it is necessary first to validate that a sentence
# has the right basic word order, and then that a version of that
# sentence with coordination also matches the current coordination
# strategy.

class PostPermute:
    def __init__(self, mrs_id, new_mrs_id):
        self.mrs_id = mrs_id
        self.new_mrs_id = new_mrs_id

    # This method takes a single sentence and returns a list of
    # sentences, each with the semantics of the new MRS id
    def apply(self, s):
        pass


#######################################################################
# all_coord_strats(pat, sent)
#   Replace all occurrences of pat in sent with all possible
#   coordinated versions, and return a list of all the resulting
#   sentences (or the empty list, if pat doesn't appear)

def all_coord_strats(pat, sent):
    if not re.search(pat, sent):
        return []
    
    # c[123] are the three coordinands
    c1 = pat
    c2 = re.sub('1', '2', pat)
    c3 = re.sub('1', '3', pat)

    # create a list of substitutions for pat
    subs = []
    # lexical mark
    subs.append('%s %s %s co' % (c1, c2, c3))
    subs.append('%s %s co %s' % (c1, c2, c3))
    subs.append('%s co %s co %s' % (c1, c2, c3))
    subs.append('%s co %s co %s co' % (c1, c2, c3))
    subs.append('co %s co %s co %s' % (c1, c2, c3))
    # affix mark
    subs.append('%s %s %s-co' % (c1, c2, c3))
    subs.append('%s %s co-%s' % (c1, c2, c3))
    subs.append('%s co-%s co-%s' % (c1, c2, c3))
    subs.append('%s-co %s-co %s-co' % (c1, c2, c3))
    subs.append('co-%s co-%s co-%s' % (c1, c2, c3))

    # now make the list of sentences by replaces pat with all
    # the possible substitutions, and return it
    sents = []
    for sub in subs:
        sents.append(re.sub(pat, sub, sent))

    return sents


class NCoordPostPermute(PostPermute):
    def __init__(self):
        self.mrs_id = 'wo2'
        self.new_mrs_id = 'wo2-coord-n'

    def apply(self, s):
        return all_coord_strats('n1', s)


class NPCoordPostPermute(PostPermute):
    def __init__(self):
        self.mrs_id = 'wo2'
        self.new_mrs_id = 'wo2-coord-np'

    def apply(self, s):
        return all_coord_strats('det1 n1', s)


class VPCoordPostPermute(PostPermute):
    def __init__(self):
        self.mrs_id = 'wo2'
        self.new_mrs_id = 'wo2-coord-vp'

    def apply(self, s):
        return all_coord_strats('iv1', s)


class SCoordPostPermute(PostPermute):
    def __init__(self):
        self.mrs_id = 'wo2'
        self.new_mrs_id = 'wo2-coord-s'

    def apply(self, s):
        return all_coord_strats('det1 n1 iv1', s)


#######################################################################
# The actual list of post-permutes

post_permutes = [
    NCoordPostPermute(),
    NPCoordPostPermute(),
    VPCoordPostPermute(),
    SCoordPostPermute()
]

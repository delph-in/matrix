#!/usr/local/bin/python2.5
"""
File: post_permutes.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents:
    PostPermute - abstract class for a group of classes that creates coordination structures for
                          sentences
    all_coord_strats - function that creates a list of possible ways that pat could be coordinated in
                              sent if pat is in sent
    NCoordPostPermute - subclass of PostPermute that creates coordination structures for
                                     sentences with nouns of sem class wo2
    NPCoordPostPermute - subclass of PostPermute that creates coordination structures for
                                      sentences with noun phrases of sem class wo2
    VPCoordPostPermute - subclass of PostPermute that creates coordination structures for
                                      sentences with verb phrases of sem class wo2
    SCoordPostPermute - subclass of PostPermute that creates coordination structures for
                                     sentences with fully-specified verb phrases of sem class wo2
    post_permutes - list of instantiated post_permutes that are applied to string in
                            add_permutes.main
Tables accessed: none
Tables modified: none
Note: The filters are not completely set up to deal with coordination yet, so the code in
        add_permutes has been remmed out for the time being
"""
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
    """
    Class: PostPermute
    Members:
        mrs_id - a semantic class of sentences this PostPermute would apply to
        new_mrs_id - the semantic class of sentences 
    Functionality: an abstract class for a group of classes that creates coordination structures
                         for sentences
    """        

    def __init__(self, mrs_id, new_mrs_id):
        """
        Method: __init__
        Input:
            self - this PostPermute
            mrs_id - the semantic class of sentences this PostPermute applies to
            new_mrs_id - the semantic class of sentences this PostPermute produces
        Output: a new PostPermute
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        self.mrs_id = mrs_id                    # set self.mrs_id to mrs_id
        self.new_mrs_id = new_mrs_id     # set self.new_mrs_id to new_mrs_id

        return

    # This method takes a single sentence and returns a list of
    # sentences, each with the semantics of the new MRS id
    def applyMe(self, s):
        """
        Method: applyMe
        Input:
            self - this PostPermute
            s - a sentence
        Output: none
        Functionality: nothing
        """
        pass


#######################################################################
# all_coord_strats(pat, sent)
#   Replace all occurrences of pat in sent with all possible
#   coordinated versions, and return a list of all the resulting
#   sentences (or the empty list, if pat doesn't appear)

def all_coord_strats(pat, sent):
    """
    Function: all_coord_strings
    Input:
        pat - a pattern
        sent - a sentence
    Output: sents - a list of sentences where pat has been replaced by a coordination of pat-like
                           words
    Functionality: Creates a list of possible ways that pat could be coordinated in sent if pat
                         is in sent
    Tables accessed: none
    Tables modified: none
    """
    sents = []                              # initialize output

    if re.search(pat, sent):            # only add to sents if pat is in sent
        # c[123] are the three coordinands
        c1 = pat                            # set c1 to be pat
        c2 = re.sub('1', '2', pat)        # replace 1 with 2 and make that c2
        c3 = re.sub('1', '3', pat)        # replace 1 with 3 and make that c3

        # create a list of substitutions for pat...a variety of ways in which things might be
        # coordinated
        subs = []                           # initialize substitutions list

        # lexical mark
        # append some ways the coordinate word(s) can be distributed among the coordinands
        subs.append('%s %s %s co' % (c1, c2, c3))
        subs.append('%s %s co %s' % (c1, c2, c3))
        subs.append('%s co %s co %s' % (c1, c2, c3))
        subs.append('%s co %s co %s co' % (c1, c2, c3))
        subs.append('co %s co %s co %s' % (c1, c2, c3))

        # affix mark
        # append some ways the coordinate affix(es) can be distributed among the coordinands        
        subs.append('%s %s %s-co' % (c1, c2, c3))
        subs.append('%s %s co-%s' % (c1, c2, c3))
        subs.append('%s co-%s co-%s' % (c1, c2, c3))
        subs.append('%s-co %s-co %s-co' % (c1, c2, c3))
        subs.append('co-%s co-%s co-%s' % (c1, c2, c3))

        # now make the list of sentences by replacing pat with all
        # the possible substitutions, and return it
        for sub in subs:                                        # for every possible substitution...
            # ...replace pat with the substitution pattern of coordination and append it to output list
            sents.append(re.sub(pat, sub, sent))

    return sents            # return output


class NCoordPostPermute(PostPermute):
    """
    Class: NCoordPostPermute
    Superclass: PostPermute
    Members:
        mrs_id - a semantic class of sentences this PostPermute would apply to
        new_mrs_id - the semantic class of sentences 
    Functionality: Creates coordination structures for sentences with nouns of sem class wo2
    """
    def __init__(self):
        """
        Function: __init__
        Input: self - this NCoordPostPermute
        Output: a new NCoordPostPermute that applies to wo2 sem class sentences
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        self.mrs_id = 'wo2'                     # set sem class of sentences this PostPermute applies to
        self.new_mrs_id = 'wo2-coord-n'  # set sem class of sentences this PostPermute produces

    def applyMe(self, s):
        """
        Method: applyMe
        Input:
            self - this PostPermute
            s - a sentence
        Output: a list of sentences like s with every occurence of n1 replaced with one of a variety
                    of coordination structures
        Functionality: Creates a list of strings coordinating nouns
        Tables accessed: none
        Tables modified: none
        """        
        return all_coord_strats('n1', s)    # coordinate occurences of 'n1' in s


class NPCoordPostPermute(PostPermute):
    """
    Class: NPCoordPostPermute
    Superclass: PostPermute
    Members:
        mrs_id - a semantic class of sentences this PostPermute would apply to
        new_mrs_id - the semantic class of sentences 
    Functionality: Creates coordination structures for sentences with noun phrases of sem class
                         wo2
    """
    def __init__(self):
        """
        Function: __init__
        Input: self - this NPCoordPostPermute
        Output: a new NPCoordPostPermute that applies to wo2 sem class sentences
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        self.mrs_id = 'wo2'                     # set sem class of sentences this PostPermute applies to
        self.new_mrs_id = 'wo2-coord-n'  # set sem class of sentences this PostPermute produces

    def applyMe(self, s):
        """
        Method: applyMe
        Input:
            self - this PostPermute
            s - a sentence
        Output: a list of sentences like s with every occurence of det1 n1 replaced with one of a
                    variety of coordination structures
        Functionality: Creates a list of strings coordinating noun phrases
        Tables accessed: none
        Tables modified: none
        """
        # TODO: not all determiners have numbers...evaluate whether line below will work
        return all_coord_strats('det1 n1', s)           # coordinate occurences of 'det 1n1' in s


class VPCoordPostPermute(PostPermute):
    """
    Class: VPCoordPostPermute
    Superclass: PostPermute
    Members:
        mrs_id - a semantic class of sentences this PostPermute would apply to
        new_mrs_id - the semantic class of sentences 
    Functionality: Creates coordination structures for sentences of sem class 'wo2' and with
                         intransitive verbs in them
    """
    def __init__(self):
        """
        Function: __init__
        Input: self - this VPCoordPostPermute
        Output: a new VPCoordPostPermute that applies to wo2 sem class sentences
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """        
        self.mrs_id = 'wo2'                         # set old mrs_id to wo2
        self.new_mrs_id = 'wo2-coord-vp'     # set new mrs id to wo2 coordinated np

    def applyMe(self, s):
        """
        Method: applyMe
        Input:
            self - this PostPermute
            s - a sentence
        Output: a list of sentences like s with every occurence of iv1 replaced with one of a
                    variety of coordination structures
        Functionality: Creates a list of strings coordinating verb phrases
        Tables accessed: none
        Tables modified: none
        """
        # TODO: is this going to work with iv1 in there intsead of iv?
        return all_coord_strats('iv1', s)    # coordinate occurences of 'iv1' in s


class SCoordPostPermute(PostPermute):
    """
    Class: SCoordPostPermute
    Superclass: PostPermute
    Members:
        mrs_id - a semantic class of sentences this PostPermute would apply to
        new_mrs_id - the semantic class of sentences 
    Functionality: Creates coordination structures for sentences of sem class 'wo2' and with
                         fully specified verb phrases in them.
    """
    def __init__(self):
        """
        Function: __init__
        Input: self - this SCoordPostPermute
        Output: a new SCoordPostPermute that applies to wo2 sem class sentences
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """        
        self.mrs_id = 'wo2'                         # set old mrs_id to wo2
        self.new_mrs_id = 'wo2-coord-s'     # set new mrs id to wo2 coordinated s

    def applyMe(self, s):
        """
        Method: applyMe
        Input:
            self - this PostPermute
            s - a sentence
        Output: a list of sentences like s with every occurence of det1 n1 iv1 replaced with one of a
                    variety of coordination structures
        Functionality: Creates a list of strings coordinating fully specified verb phrases
        Tables accessed: none
        Tables modified: none
        """
        # TODO: is this going to work with iv1 in there intsead of iv?
        return all_coord_strats('det1 n1 iv1', s)   # coordinate occurences of 'iv1' in s


#######################################################################
# The actual list of post-permutes
# This gets imported by add_permutes and used in add_permutes.main, but it has been remmed
# out there for the time being.
post_permutes = [
    NCoordPostPermute(),
    NPCoordPostPermute(),
    VPCoordPostPermute(),
    SCoordPostPermute()
]

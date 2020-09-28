from gmcs.lib import Hierarchy, HierarchyNode
import sys
from functools import reduce
# Base class for lexical types (lexicon.py) and lexical rules (morphotatics.py)

################################
### CONSTANTS (in principle) ###
################################

# all types of lexical items (on lexicon page)
# TJT 2014-08-15: adding "cop"
# TJT 2014-08-15: changing to tuple for speed
# OZ 2018-01-25 comps is not really a lexical type;
# it is a complementation strategy that is defined by a complementizer type.
# Ideally we would have complementizers as actual lexical items some day.
ALL_LEX_TYPES = ('noun', 'verb', 'det', 'aux', 'adj',
                 'cop', 'comps', 'adv', 'normadp', 'qverb')

# types used for lexical rules (verb and aux are merged)
# TJT 2014-08-15: adding "cop"
# TJT 2014-08-15: changing to tuple for speed
LEXICAL_CATEGORIES = ('noun', 'verb', 'det', 'adj', 'cop', 'adv', 'qverb')

# TJT 2014-09-03: Types not automatically added to mylanguage.tdl
NON_ESSENTIAL_LEX_CATEGORIES = (
    'det', 'adj', 'cop', 'comps', 'normadp', 'adv', 'qverb')

# lexical_supertypes is a dictionary mapping the choices file
# encodings to the actual lex-type identifiers of the supertypes.
LEXICAL_SUPERTYPES = {'noun': 'noun-lex',
                      'verb': 'verb-lex',
                      'iverb': 'intransitive-verb-lex',
                      'tverb': 'transitive-verb-lex',
                      'mverb': 'main-verb-lex',
                      'cop': 'cop-lex',
                      'det': 'determiner-lex',
                      'aux': 'aux-lex',
                      'adj': 'adj-lex',
                      'comp': 'comp-lex',
                      'adv': 'adverb-lex',
                      'normadp': 'norm-adposition-lex',
                      'qverb': 'interrogative-verb-lex'}

# TYPE DEFINITIONS (that can be shared with other libraries)
COMPLEMENTIZER = '''
      complementizer-lex-item := raise-sem-lex-item & basic-one-arg & basic-icons-lex-item & 
         [ SYNSEM [ LOCAL.CAT [ HEAD comp &
                                   [ MOD < > ],
                              VAL [ SPR < >, SPEC < >,
                                    SUBJ < >,
                                    COMPS < #comp > ] ] ],
           ARG-ST < #comp & [ LOCAL.CAT [ HEAD verb,
                                          VAL [ SUBJ < >,
                                                COMPS < > ] ] ] > ].'''


QUES_CLITIC = '''
ques-clitic-lex := no-hcons-lex-item & zero-arg-ynq &
 [ SYNSEM [ LOCAL [ CAT [ VAL [ SPR < >, COMPS < >, SUBJ < >, SPEC < >],
                                        HEAD adv &
                                                [ MOD < [ LIGHT +,
                                                          LOCAL intersective-mod & 
                                                          [ CAT [ HEAD +nvrpd ] ],
                                                          L-PERIPH +,
                                                          L-QUE - ] > ] ],
                             CONT.RELS.LIST < > ],
            NON-LOCAL [  YNQ.LIST < *top* > ] ] ].
'''

WH_PRONOUN = '''wh-pronoun-noun-lex := basic-wh-word-lex & norm-hook-lex-item & basic-icons-lex-item & non-mod-lex-item & zero-arg-que &
  [ SYNSEM [ LOCAL [ CAT [ HEAD noun,
                           VAL [ SPR < >,
				 SUBJ < >,
				 COMPS < >,
				 SPEC < > ] ],
		     CONT [ RELS.LIST < [ LBL #larg,
				       ARG0 ref-ind & #arg0 ],
				  quant-relation & [ PRED "which_q_rel",
				    ARG0 #arg0,
				    RSTR #harg ] >,
			    HCONS.LIST < [ HARG #harg,
				        LARG #larg ] > ] ],
	     NON-LOCAL.QUE.LIST < #arg0 > ] ].'''

ADV_ITEM = '''adverb-lex-item := intersective-adverb-lex & 
  [ SYNSEM [ LOCAL [ CAT [ VAL [ SUBJ < >, SPEC < >,
                             SPR < >,
                             COMPS < > ],
                       HEAD adv &
                            [ MOD < [ LOCAL [ CAT.HEAD verb,
                                              CONT.HOOK [ CLAUSE-KEY #clause, LTOP #ltop ] ] ] > ] ],
                   CONT [ RELS.LIST < [ LBL #ltop, ARG0 event,
                                    ARG1 #clause, ARG2 #ind ],
                                  [ PRED #pred, ARG0 #ind, LBL #larg ],[ ARG0 #ind, RSTR #harg ] >,
                          HOOK.LTOP #ltop,
                          HCONS.LIST < qeq & [ HARG #harg,
                                            LARG #larg ] > ] ],
              LKEYS.KEYREL [ PRED #pred, ARG0 ref-ind & #ind, LBL #ltop ] ] ].'''


LOC_ADV_ITEM = '''loc-adverb-lex-item := adverb-lex-item & 
  [ SYNSEM.LOCAL.CONT.RELS.LIST.FIRST.PRED "loc_nonsp_rel" ].'''

MANNER_ADV_ITEM = '''manner-adverb-lex-item := adverb-lex-item & 
  [ SYNSEM.LOCAL.CONT.RELS.LIST.FIRST.PRED "manner_nonsp_rel" ].'''

ADV = '''adverb-lex := basic-non-wh-word-lex & adverb-lex-item & norm-zero-arg & 
[ SYNSEM.LOCAL.CONT.RELS.LIST < [ ], [ ], [ PRED "exist_q_rel" ] > ].'''

WH_ADV = '''wh-adverb-lex := basic-wh-word-lex & adverb-lex-item & zero-arg-que &
[ SYNSEM [ LOCAL.CONT.RELS.LIST < [ ], [ ARG0 #arg0 ], quant-relation & [ PRED "which_q_rel" ] >,
           NON-LOCAL.QUE.LIST < #arg0 > ] ].'''


WH_DET = '''wh-determiner-lex := basic-wh-word-lex & basic-determiner-lex & non-mod-lex-item  & zero-arg-nonslash &
  [ SYNSEM [ LOCAL [ CAT [ VAL [ SPR < >,
                           SPEC.FIRST.LOCAL [ CONT.HOOK.INDEX #arg0 ],
                           COMPS < >,
                           SUBJ < > ] ] ],
             NON-LOCAL.QUE.LIST < #arg0 > ] ].'''

ADP_LEX = '''norm-adposition-lex := norm-sem-lex-item & no-hcons-lex-item & basic-intersective-mod-lex & basic-one-arg &
  [ SYNSEM [ LOCAL [ CAT [ HEAD adp & [ MOD < [ LOCAL.CAT [ VAL.SPR cons, 
                                                            WH.BOOL - ] ] > ],
                           WH.BOOL -,
                           VAL [ SPR < >,
                                 SPEC < >,
                                 SUBJ < >,
                                 COMPS < #comp & [ L-QUE #lque,
                                                 LOCAL [ CAT [ HEAD noun, VAL.SPR < > ],
                                                 CONT.HOOK.INDEX #ind ],
                                         NON-LOCAL #nonloc ] > ] ],
                     CONT.RELS.LIST < [ PRED #pred, ARG0 event, ARG1 event-or-ref-index ] > ],
             LKEYS.KEYREL arg12-ev-relation & [ PRED #pred, ARG2 #ind ],
             NON-LOCAL #nonloc,
             L-QUE #lque ],
    ARG-ST < #comp > ].'''


ITRG_VB = '''interrogative-verb-lex := basic-wh-word-lex & basic-verb-lex-super & non-mod-lex-item &
[ SYNSEM [ LOCAL [ CONT.HOOK [ INDEX.SF ques, XARG #xarg ],
                   CAT [ VAL [ SPR < >,
                               SPEC < >,
                               COMPS < >,
                               SUBJ < #subj > ] ] ] ],
    ARG-ST.FIRST #subj &
                 [ LOCAL [ CAT cat-sat &
                               [ VAL [ SPR < >,
                                       COMPS < >, SUBJ < >, SPEC < > ] ],
                           CONT.HOOK.INDEX #xarg ] ] ].
'''

ITRG_THREE_REL = ''' := interrogative-verb-lex & basic-icons-lex-item & norm-hook-lex-item & 
[ ARG-ST < [ LOCAL [ CAT cat-sat & [ HEAD noun ],
                      CONT.HOOK [ INDEX  ref-ind & #ind,
                      ICONS-KEY.IARG1 #clause ] ] ] >,
 SYNSEM [ LKEYS.KEYREL event-relation & [ ARG1 #ind, ARG2 ref-ind & #ind2],
             LOCAL.CONT [ HOOK.CLAUSE-KEY #clause,
                          RELS.LIST < [ ARG2 #ind2 ], 
                                      quant-relation & [ PRED "which_q_rel",
                                                        ARG0 #ind2,
                                                        RSTR #harg ], 
                                       noun-relation & [ LBL #larg,
                                                         ARG0 #ind2 ] >,
               HCONS.LIST < [ HARG #harg,
                LARG #larg ] > ] ] ].'''

ITRG_FOUR_REL = ''' := interrogative-verb-lex & basic-icons-lex-item & norm-hook-lex-item & 
[ ARG-ST < [ LOCAL [ CAT cat-sat & [ HEAD noun ],
                      CONT.HOOK [ INDEX  ref-ind & #ind,
                      ICONS-KEY.IARG1 #clause ] ] ] >,
 SYNSEM [ LKEYS.KEYREL event-relation & [ ARG1 #ind ],
             LOCAL.CONT [ HOOK.CLAUSE-KEY #clause,
                          RELS.LIST < [ LBL #ltop, ARG0 #clause ], 
                                      [ LBL #ltop, ARG0 event,
                                    ARG1 #clause, ARG2 #ind2 ],
                                  [ ARG0 #ind2, LBL #larg ],[ PRED "which_q_rel", ARG0 #ind2, RSTR #harg ] >,
               HCONS.LIST < [ HARG #harg,
                LARG #larg ] > ] ] ].'''

###############
### CLASSES ###
###############


class MorphotacticNode(HierarchyNode):
    def __init__(self, key, name=None, pc=None, parents=None, supertypes=None,
                 instance=False):
        HierarchyNode.__init__(self, key, parents=parents)
        self.name = name or ''
        self.pc = pc
        self.constraints = {'req-fwd': {}, 'req-bkwd': {}, 'forbid': {}}
        self.disjunctive_flag_sets = {}
        self.flags = {'in': {}, 'out': {}}
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
        return self.pc in [o.pc for o in list(other.input_span().values())]

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
        self.nodes = self.l_hierarchy.nodes  # for convenience
        self.order = order
        self.pc = self
        self.identifier_suffix = identifier_suffix or 'lex-rule-super'
        self.is_lex_rule = lex_rule
        # TJT 2014-08-21: Keep track of whether position
        # class has incorporated stems
        self._has_is = None
        # CMC 2017-02-20: Keep track of whether position
        # class has valence-changing operations
        self._has_vcops = None

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
        all_inps = list(self.input_span().values())
        # there are two conditions preventing an ancestor from being an input:
        # 1. A node, or all its ancestors, req-fwd an intervening node
        # 2. This pc, or all its followers, req-bkwd an intervening node

        # TODO: implement these later.. try to be efficient
        return all_inps

    def has_valchg_ops(self):
        # 2017-02-20 CMC: Keep track of whether a position class has
        # valence-changing operations in its rules
        if self._has_vcops == None:
            for lrt in self.nodes.values():
                if len(lrt.valchgops) > 0:
                    self._has_vcops = True
                    return self._has_vcops
            self._has_vcops = False
        return self._has_vcops

    def has_evidential(self):
        for lrt in self.nodes.values():
            if lrt.evidential != None:
                return True
        return False

    def has_infostr(self):
        for lrt in self.nodes.values():
            for f in lrt.features:
                if f.startswith('information-structure'):
                    return True
        return False

    def has_possessive(self):
        for lrt in self.nodes.values():
            poss_pron = False
            for feature in lrt.features:
                if 'poss-pron' in feature:
                    poss_pron = True
            if lrt.possessive != None or poss_pron:
                return True
        return False

    def has_incorporated_stems(self):
        # 2014-08-21 TJT: Keep track of whether a PositionClass has
        # Incorporated Stem Lexical Rule Instances
        if self._has_is == None:  # Only do this once
            for lrt in self.nodes.values():
                for lri in lrt.lris:
                    if lri.pred:
                        self._has_is = True  # Keep track of result
                        return self._has_is
            self._has_is = False
        return self._has_is


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
        # CMC 2017-03-24 Valence change operations are new property of LRT
        self.valchgops = []
        self.evidential = None
        # EKN 2017-12-15 Possessor rule pseudofeatures are properties of LRT
        self.possessive = None
        self.poss_strat_num = None

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
        # LLD 1-3-2016 Now returns only supertypes that are not shared by the PC (to
        # prevent vacuous inheritance errors)
        # return set(parents).union(self.supertypes)
        return set(parents).union(self.supertypes).difference(self.pc.supertypes)

# TJT 2014-08-21: Class for keeping Lexical Rule Instances and their
# predicates together


class LexicalRuleInstance:
    """
    Store LRI name and pred together
    """

    def __init__(self, name, pred=None):
        self.name = name
        self.pred = pred or None

    def __repr__(self):
        return 'LexicalRuleInstance(' + self.name + ')'

    def __str__(self):
        return self.name

    def __len__(self):
        return len(self.name)

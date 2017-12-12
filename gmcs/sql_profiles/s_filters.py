"""
File: s_filters.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: 7/30/09 - taken over on this date
Project: MatrixTDB RA, summer 2009
Project Owner: Emily M. Bender
Contents:
    - filter_list - the list of the 178 specific filters
Tables accessed: none
Tables modified: none
"""

#################################################################
# TODO:

# Things are going to get a lot messier when we have more inflection,
# because this is all string based.  Consider replacing "n1" etc in
# the regular expressions with variables that expand out to all
# of the different inflected possibilities, especially for the filters
# that are not concerned with inflection per se.

# A lot of these have things in common.  Consider defining classes
# for the shared properties and then instances for the differences.
# ... depends on how flexible the class system is.  Multiple inheritance?...

# Some (many?) of these assume that the lts are fully specified for the
# things they care about (e.g., neg-aff-only-1 and neg-adv-only-2), which
# will give different behavior from the grammar starts for underspecified
# lts...

#################################################################
# Initial set of language-type specific word order filters.

from filters import FalseFilter
from filters import MatchFilter
from filters import NotMatchFilter
from filters import IfFilter
from filters import IfNotFilter
from filters import OrFilter
import g

#################################################################
# Language-type specific filters for word order.  Note that these
# apply to all sentences, not just the ones with 'wo' mrs_tags,
# because all sentences have word order!
#
# Each filter provides a list of feature-value pairs from language
# type definitions that it cares about.
#
# When the constraint is an 'or' (e.g., 'word-order' is ''sov'' or ''osv'')
# we put both 'word-order':'sov' and 'word-order':'osv' on the list.  The
# query which generates the gold standard test suites looks for all
# filters which match the particulare feature-value pairs for the language
# type in question.

filter_list = [

    ####################################################################
    # Word order filters

    MatchFilter(name = "s-o-order1",
                mrs_id_list = g.n1_subj_n2_obj,
                re1 = 'n1.*n2',
                comment = "If the word order for the language type has subjects before objects, " + \
                          "and n1 is the subject and n2 is the object, check that n1 precedes " + \
                          "n2.   (Worry some about what to do with inversion constructions ... " + \
                          "do they affect this?)",
                fv = ['or', 'word-order:svo','word-order:sov','word-order:vso']),

    MatchFilter(name = "s-o-order2",
                mrs_id_list = g.n2_subj_n1_obj,
                re1 = 'n2.*n1',
                comment = "If the word order for the language type has subjects before objects, " + \
                          "and n2 is the subject and n1 is the object, check that n2 precedes " + \
                          "n1.   (Worry some about what to do with inversion constructions ... " + \
                          "do they affect this?)",
                fv = ['or', 'word-order:svo','word-order:sov','word-order:vso']),

    MatchFilter(name = "s-o-order3",
                mrs_id_list = g.n1_subj_n2_obj,
                re1 = 'n2.*n1',
                comment = "If the word order for the language type has objects before subjects, " + \
                          "and n1 is the subject and n2 is the object, check that n2 precedes " + \
                          "n1.   (Worry some about what to do with inversion constructions ... " + \
                          "do they affect this?)",
                fv = ['or', 'word-order:ovs','word-order:osv','word-order:vos']),

    MatchFilter(name = "s-o-order4",
                mrs_id_list = g.n2_subj_n1_obj,
                re1 = 'n1.*n2',
                comment = "If the word order for the language type has objects before subjects, " + \
                          "and n2 is the subject and n1 is the object, check that n1 precedes " + \
                          "n2.   (Worry some about what to do with inversion constructions ... " + \
                          "do they affect this?)",
                fv = ['or', 'word-order:ovs','word-order:osv','word-order:vos']),

    OrFilter(name = "s-v-order1",
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = 'n1.*[ti]v(?:[1-9])?',
             comment = "If the word order for the language has subjects before verbs, and n1 is " + \
                       "the subject, check that n1 precedes the verb.  This doesn't apply to " + \
                       "ques1 and ques2.  For those we need to check if qpart (or, eventually, " + \
                       "question inflection) is present.  Also doesn't apply if there's an " + \
                       "auxiliary in the sentence.  Then we need to check separately.",
             fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    IfNotFilter(name = "s-v-order2",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'qpart',
                re2 = '[ti]v(?:[1-9])?.*n1',
                comment = "If the word order for the language has subjects before verbs, and n1 " + \
                          "is the subject, check that n1 precedes the verb.  Special case for " + \
                          "ques1 and ques2 where we want to check that we're not in the " + \
                          "inversion case, which needs to be worried about separately.",
                fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    OrFilter(name = "s-v-order3",
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])?.*n1',
             comment = "If the word order for the language has verbs before subjects, and n1 " + \
                       "is the subject, check that n1 follows the verb.  This doesn't apply " + \
                       "to ques1 and ques2.  For those we need to check if qpart (or, " + \
                       "eventually, question inflection) is present.  Also doesn't apply if " + \
                       "there's an auxiliary in the sentence.  Then we need to check separately.",
             fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    IfNotFilter(name = "s-v-order4",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'qpart',
                re2 = 'n1.*[ti]v(?:[1-9])?',
                comment = "If the word order for the language has verbs before subjects, and " + \
                          "n1 is the subject, check that n1 follows the verb.  Special case for " + \
                          "ques1 and ques2 where we want to check that we're not in the " + \
                          "inversion case, which needs to be worried about separately.",
                fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    OrFilter(name = "s-v-order5",
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = 'n2.*[ti]v(?:[1-9])?',
             comment = "If the word order for the language has subjects before verbs, and n2 " + \
                       "is the subject, check that n2 precedes the verb.  This doesn't apply " + \
                       "to ques3 and ques4.  For those we need to check if qpart (or, " + \
                       "eventually, question inflection) is present.  Also doesn't apply if " + \
                       "there's an auxiliary in the sentence.  Then we need to check " + \
                       "separately.",
             fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    IfNotFilter(name = "s-v-order6",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'qpart',
                re2 = '[ti]v(?:[1-9])?.*n2',
                comment = "If the word order for the language has subjects before verbs, and n2 " + \
                          "is the subject, check that n2 precedes the verb.  Special case for " + \
                          "ques1 and ques2 where we want to check that we're not in the " + \
                          "inversion case, which needs to be worried about separately.",
                fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    OrFilter(name = "s-v-order7",
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])?.*n2',
             comment = "If the word order for the language has verbs before subjects, and n2 is " + \
                       "the subject, check that n2 follows the verb.  This doesn't apply to " + \
                       "ques1 and ques2.  For those we need to check if qpart (or, " + \
                       "eventually, question inflection) is present. Also doesn't apply if " + \
                       "there's an auxiliary in the sentence.  Then we need to check " + \
                       "separately.",
             fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    IfNotFilter(name = "s-v-order8",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'qpart',
                re2 = 'n2.*[ti]v(?:[1-9])?',
                comment = "If the word order for the language has verbs before subjects, and n2 " + \
                          "is the subject, check that n2 follows the verb.  Special case for ques1 " + \
                          "and ques2 where we want to check that we're not in the inversion " + \
                          "case, which needs to be worried about separately.",
                fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    # NB: s-aux-order[1-4] would be more efficient as NotMatchFilters were the current re1 was
    # eliminated, the current re2 was moved to re1 and the orders of the two elements surrounding
    # '.*' were reversed
    IfFilter(name = 's-aux-order1',
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = 'n1.*aux',
             comment = "If the subject precedes the verb, and there is an auxiliary, and n1 is " + \
                       "the subject, check that n1 precedes the aux.",
             fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:sov','word-order:svo',
                                                            'word-order:osv','word-order:v-final']]),

    IfFilter(name = 's-aux-order2',
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = 'n2.*aux',
             comment = "If the subject precedes the verb, and there is an auxiliary, and n2 is " + \
                       "the subject, check that n2 precedes the aux.",
             fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:sov','word-order:svo',
                                                            'word-order:osv','word-order:v-final']]),

    IfFilter(name = 's-aux-order3',
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = 'aux.*n1',
             comment = "If the subject follows the verb, and there is an auxiliary, and n1 is the " + \
                       "subject, check that n1 follows the aux.",
             fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:vos','word-order:ovs',
                                                            'word-order:vso','word-order:v-initial']]),


    IfFilter(name = 's-aux-order4',
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = 'aux.*n2',
             comment = "If the subject follow the verb, and there is an auxiliary, and n2 is the " + \
                       "subject, check that n2 follows the aux.",
             fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:vos','word-order:ovs',
                                                            'word-order:vso','word-order:v-initial']]),


    IfFilter(name = 's-aux-order-5',
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = 'n1 .*[ti]v(?:[1-9])?',
             comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the " + \
                       "subject is in the right place wrt the main verb.  In this case the " + \
                       "subject should precede the main verb.",
             fv = ['and','aux-comp:s',['or','word-order:sov','word-order:svo','word-order:osv',
                                       'word-order:v-final']]),

    IfFilter(name = 's-aux-order-6',
             mrs_id_list = g.n1_subj_not_ques,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])?.* n1',
             comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the " + \
                       "subject is in the right place wrt the main verb.  In this case the " + \
                       "subject should follow the main verb.",
             fv = ['and','aux-comp:s',['or','word-order:vos','word-order:ovs','word-order:vso',
                                       'word-order:v-initial']]),

    IfFilter(name = 's-aux-order-7',
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = 'n2 .*[ti]v(?:[1-9])?',
             comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the " + \
                       "subject is in the right place wrt the main verb.  In this case the " + \
                       "subject should precede the main verb.",
             fv = ['and','aux-comp:s',['or','word-order:sov','word-order:svo','word-order:osv',
                                       'word-order:v-final']]),

    IfFilter(name = 's-aux-order-8',
             mrs_id_list = g.n2_subj_n1_obj_not_ques,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])?.* n2',
             comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that " + \
                       "the subject is in the right place wrt the main verb.  In this case the " + \
                       "subject should follow the main verb.",
             fv = ['and','aux-comp:s',['or','word-order:vos','word-order:ovs','word-order:vso',
                                       'word-order:v-initial']]),
    # 8/27/09 KEN added
    NotMatchFilter(name = 's-aux-order-ques1',
                   mrs_id_list = g.n1_subj_ques,
                   re1 = 'aux.*n1',
                   comment = "If the subject precedes the verb, if n1 is the subject, if there is "+ \
                             "either no inversion on questions or inversion happens only with the "+ \
                             "main verb, then ensure that aux does not precede n1.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'], ['or', 'q-inv:', 'q-inv-verb:main'],
                         ['or','word-order:sov','word-order:svo', 'word-order:osv','word-order:v-final']]),

    # 8/27/09 KEN added
    NotMatchFilter(name = 's-aux-order-ques2',
                   mrs_id_list = g.n2_subj_ques,
                   re1 = 'aux.*n2',
                   comment = "If the subject precedes the verb, if n2 is the subject, if there is "+ \
                             "either no inversion on questions or inversion happens only with the "+ \
                             "main verb, then ensure that aux does not precede n2.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'], ['or', 'q-inv:', 'q-inv-verb:main'],
                         ['or','word-order:sov','word-order:svo', 'word-order:osv','word-order:v-final']]),

    # 8/27/09 KEN added
    NotMatchFilter(name = 's-aux-order-ques3',
                   mrs_id_list = g.n1_subj_ques,
                   re1 = 'n1.*aux',
                   comment = "If the subject follws the verb, if n1 is the subject, if there is "+ \
                             "either no inversion on questions or inversion happens only with the "+ \
                             "main verb, then ensure that aux does not follow n1.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'], ['or', 'q-inv:', 'q-inv-verb:main'],
                         ['or','word-order:vos','word-order:ovs', 'word-order:vso','word-order:v-initial']]),

    # 8/27/09 KEN added
    NotMatchFilter(name = 's-aux-order-ques4',
                   mrs_id_list = g.n2_subj_ques,
                   re1 = 'n2.*aux',
                   comment = "If the subject follows the verb, if n2 is the subject, if there is "+ \
                             "either no inversion on questions or inversion happens only with the "+ \
                             "main verb, then ensure that aux does not follow n2.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'], ['or', 'q-inv:', 'q-inv-verb:main'],
                         ['or','word-order:vos','word-order:ovs', 'word-order:vso','word-order:v-initial']]),

    NotMatchFilter(name = "o-v-order1",
                   mrs_id_list = g.n1_subj_n2_obj,
                   re1 = 'tv(?:[1-9])?.*n2',
                   comment = "If the word order for the language has objects before verbs, and " + \
                             "n2 is the object, check that it precedes the verb.",
                   fv = ['or', 'word-order:sov','word-order:osv','word-order:ovs','word-order:v-final']),

    NotMatchFilter(name = "o-v-order2",
                   mrs_id_list = g.n1_subj_n2_obj,
                   re1 = 'n2.*tv(?:[1-9])?',
                   comment = "If the word order for the language has objects after verbs, and n2 " + \
                             "the object, check that it follows the verb.",
                   fv = ['or', 'word-order:vos','word-order:vso','word-order:svo','word-order:v-initial']),

    NotMatchFilter(name = "o-v-order3",
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'tv(?:[1-9])?.*n1',
                   comment = "If the word order for the language has objects before verbs, and " + \
                             "n1 is the object, check that it precedes the verb.",
                   fv = ['or', 'word-order:sov','word-order:osv','word-order:ovs','word-order:v-final']),

    NotMatchFilter(name = "o-v-order4",
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'n1.*tv(?:[1-9])?',
                   comment = "If the word order for the language has objects after verbs, and n1 " + \
                             "is the object, check that it follows the verb.",
                   fv = ['or', 'word-order:vos','word-order:vso','word-order:svo','word-order:v-initial']),


    FalseFilter(name = "no-dets",
                mrs_id_list = g.with_dets,
                comment = "If the language has no determiners, reject all strings with determiners.",
                fv = ['has-dets:no']),

    MatchFilter(name = "det-n-order1",
                mrs_id_list = g.one_det_on_n1,
                re1 = 'det n1',
                comment = "If the language has determiners preceding nouns, and the " + \
                          "sentence has a determiner for n1, the det should immediately " + \
                          "precede n1.  Revise when we add nominal modifiers.",
                fv = ['noun-det-order:det-noun']),

    MatchFilter(name = "det-n-order2",
                mrs_id_list = g.one_det_on_n1,
                re1 = 'n1 det',
                comment = "If the language has determiners following nouns, and the sentence " + \
                          "has a determiner for n1, the det should immediately follow n1.  " + \
                          "Revise when we add nominal modifiers.",
                fv = ['noun-det-order:noun-det']),

    MatchFilter(name = "det-n-order3",
                mrs_id_list = g.one_det_on_n2,
                re1 = 'det n2',
                comment = "If the language has determiners preceding nouns, and the sentence " + \
                          "has a determiner for n2, the det should immediately precede n2.  " + \
                          "Revise when we add nominal modifiers.",
                fv = ['noun-det-order:det-noun']),

    MatchFilter(name = "det-n-order4",
                mrs_id_list = g.one_det_on_n2,
                re1 = 'n2 det',
                comment = "If the language has determiners following nouns, and the sentence " + \
                          "has a determiner for n2, the det should immediately follow n2.  " + \
                          "Revise when we add nominal modifiers.",
                fv = ['noun-det-order:noun-det']),

    MatchFilter(name = "det-n-order5",
                mrs_id_list = g.two_dets_n1_n2,
                re1 = 'n1 det .*n2 det|n2 det .*n1 det',
                comment = "If the language has determiners following nouns, and n1 and n2 both " + \
                          "have determiners attached, the nouns and determiners should occur " + \
                          "in the proper order.",
                fv = ['noun-det-order:noun-det']),

    MatchFilter(name = "det-n-order6",
                mrs_id_list = g.two_dets_n1_n2,
                re1 = 'det n1 .*det n2|det n2 .*det n1',
                comment = "If the language has determiners preceding nouns, and n1 and n2 " + \
                          "both have determiners attached, the nouns and determiners should " + \
                          "occur in the proper order.",
                fv = ['noun-det-order:det-noun']),

    ####################################################################
    # Negation filters

    NotMatchFilter(name = "neg-infl",
                   mrs_id_list = g.all_neg,
                   re1 = '-neg|neg-',
                   comment = "If we haven't selected inflectional negation, we shouldn't see the " + \
                             "affix. NB: There is no value 'off' for the feature infl-neg in " + \
                             "matrixdef.  But the code for figuring out which groups are relevant " + \
                             "to which language types will do the right thing if we use a non-value " + \
                             "for the feature here.",
                   fv = ['infl-neg:']),

    NotMatchFilter(name = "neg-adv",
                   mrs_id_list = g.all_neg,
                   re1 = '(\s|^)neg(\s|$)',
                   comment = "If we haven't selected adverbial negation, we shouldn't see the " + \
                             "adverbs.",
                   fv = ['adv-neg:']),

    MatchFilter(name = "neg-affix-only-1",
                mrs_id_list = g.all_neg,
                re1 = '-neg|neg-',
                comment = "If we only selected inflectional negation, we should see one in every " + \
                          "negated sentence",
                fv = ['and', 'infl-neg:on','adv-neg:']),

    # this was a duplicate filter with neg-adv-only-2, but based on filter pattern, comment, and fv
    # KEN renamed to neg-adv-only-2
    MatchFilter(name = "neg-adv-only-1",
                mrs_id_list = g.all_neg,
                re1 = '(\s|^)neg(\s|$)',
                comment = "If we only selected adverbial negation, we should see one in every " + \
                          "negated sentence.",
                fv = ['and', 'infl-neg:','adv-neg:on']),

    # this was a duplicate filter with neg-affix-only-1, but based on filter pattern, comment, and fv
    # KEN renamed to neg-affix-only-1
    NotMatchFilter(name = "neg-affix-only-2",
                   mrs_id_list = g.all_neg,
                   re1 = '(\s|^)neg(\s|$)',
                   comment = "If we only selected inflectional negation, we shouldn't see the adverb",
                   fv = ['and', 'infl-neg:on','adv-neg:']),

    NotMatchFilter(name = "neg-adv-only-2",
                   mrs_id_list = g.all_neg,
                   re1 = '-neg|neg-',
                   comment = "If we only selected adverbial negation, we shouldn't see the affix",
                   fv = ['and', 'infl-neg:','adv-neg:on']),

    # 8/19/09 KEN broke AndFilter neg-both-req into two MatchFilters: neg-both-req1 and
    # neg-both-req2 due to logical equivalence and giving us fewer Filter subclasses
    MatchFilter(name = "neg-both-req1",
                mrs_id_list = g.all_neg,
                re1 = '-neg|neg-',
                comment = "If both adv and sel are required for negation, we should see always " + \
                          "see both affix & adv.  This filter works in conjunction with " + \
                          "neg-both-req2 to ensure they are both there.",
                fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:both-obl']),

    MatchFilter(name = "neg-both-req2",
                mrs_id_list = g.all_neg,
                re1 = '(\s|^)neg(\s|$)',
                comment = "If both adv and sel are required for negation, we should see always " + \
                          "see both affix & adv.  This filter works in conjunction with " + \
                          "neg-both-req1 to ensure they are both there.",
                fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:both-obl']),

    # 8/19/09 KEN made IfNot instead of Nand becuase those two types of filters are logically
    # equivalent
    IfNotFilter(name = "neg-comp-dist",
                mrs_id_list = g.all_neg,
                re1 = '-neg|neg-',
                re2 = '(\s|^)neg(\s|$)',
                comment = "If adverbial and inflectional negation are in complementary " + \
                          "distribution, we should see never see both affix & adv.",
                fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:comp']),

    MatchFilter(name = "neg-adv-obl",
                mrs_id_list = g.all_neg,
                re1 = '(\s|^)neg(\s|$)',
                comment = "If both adverb and affix are possible, but the adverb is obligatory, we " + \
                          "should see the adverb in every negated sentence.",
                fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:adv-obl']),

    MatchFilter(name = "neg-infl-obl",
                mrs_id_list = g.all_neg,
                re1 = '-neg|neg-',
                comment = "If both adverb and affix are possible, but the affix is obligatory, we " + \
                          "should see the affix in every negated sentence.",
                fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:infl-obl']),

    IfFilter(name = "neg-infl-aux",
             mrs_id_list = g.all_neg,
             re1 = '-neg|neg-',
             re2 = 'aux-neg|neg-aux',
             comment = "If the affix only attaches to aux and an affix is present, it should only " + \
                       "attach to aux.",
             fv = ['and', 'infl-neg:on', 'neg-infl-type:aux']),

    IfFilter(name = "neg-infl-main",
             mrs_id_list = g.all_neg,
             re1 = '-neg|neg-',
             re2 = '[ti]v(?:[1-9])?-neg|neg-[ti]v(?:[1-9])?',
             comment = "If the affix only attaches to main verbs and an affix is present, it should " + \
                       "only attach to a verb.",
             fv = ['and', 'infl-neg:on', 'neg-infl-type:main']),

    NotMatchFilter(name = "neg-adv-left",
                   mrs_id_list = g.all_neg,
                   re1 = '[ti]v(?:[1-9])?.* neg',
                   comment = "If there is an independent modifier adverb, it should not show up " + \
                             "on the wrong side of the verb.",
                   fv = ['and','adv-neg:on',
                         # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                         # file anymore
                         'neg-order:before']),

    NotMatchFilter(name = "neg-adv-right",
                   mrs_id_list = g.all_neg,
                   re1 = 'neg .*[ti]v(?:[1-9])?',
                   comment = "If there is an independent modifier adverb, it should not show up " + \
                             "on the wrong side of the verb.",
                   fv = ['and','adv-neg:on',
                         # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                         # file anymore
                         'neg-order:after']),

    # 8/24/09 KEN added to get miniaffixes grammar working
    NotMatchFilter(name = 'neg-infl-right',
                   mrs_id_list = g.all_neg,
                   re1 = 'neg-\S*(?:[ti]v[1-9]?|aux)',
                   comment = 'If inflectional negation happens after the verb, then "neg-" cannot ' + \
                             'appear before the verb (aux or iv or tv followed by an optional digit) ' + \
                             'in the same word as the verb (that is the \S* part)',
                   # This is the first filter to use groups and backreferences to ensure that the same
                   # slot that is negation is the one that specifies it comes after the verb
                   fv = ['and', 'verb-slot([1-9])_morph[1-9]_feat[1-9]_name:negation',
                         r'verb-slot\1_order:after']),

    # 8/24/09 KEN added as converse to neg-infl-right
    NotMatchFilter(name = 'neg-infl-left',
                   mrs_id_list = g.all_neg,
                   re1 = '(?:[ti]v[1-9]?|aux)\S*-neg',
                   comment = 'If inflectional negation happens before the verb, then "neg-" cannot ' + \
                             'appear after the verb (aux or iv or tv followed by an optional digit) ' + \
                             'in the same word as the verb (that is the \S* part)',
                   fv = ['and', 'verb-slot([1-9])_morph[1-9]_feat[1-9]_name:negation',
                         'verb-slot\\1_order:before']),

    NotMatchFilter(name = "neg-adv-s",
                   mrs_id_list = g.all_neg,
                   re1 = '(n(?:[1-9])?|[ti]v(?:[1-9])?|aux).* neg .*(n(?:[1-9])?|[ti]v(?:[1-9])?|aux)',
                   comment = "If the negative adverb is an independent modifier of S, it should " + \
                             "not appear between the verb and any arguments.  This filter will " + \
                             "obviously not work for any multiclausal cases.",
                   fv = ['and','adv-neg:on',
                         # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                         # file anymore
                         'neg-mod:s']),

    MatchFilter(name = "neg-adv-v",
                mrs_id_list = g.all_neg,
                re1 = 'neg \S*(tv(?:[1-9])?|iv(?:[1-9])?|aux)|(tv(?:[1-9])?|iv(?:[1-9])?|aux)\S* neg',
                comment = "If the negative adverb is an independent modifier of V, it should be " + \
                          "adjacent to the verb.  The verb can have any prefixes or suffies, " + \
                          "though (those are the \S* parts). As we get a more refined theory " + \
                          "of auxiliaries, we may wish to distinguish auxiliaries from main " + \
                          "verbs here.",
                fv = ['and','adv-neg:on',
                      # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                      # file anymore
                      'neg-mod:v']),

    NotMatchFilter(name = 'neg-adv-vp-1',
                   mrs_id_list = g.n1_subj_neg,
                   # TODO: double check these REs are using parens the right way.
                   # TODO: do the same throughout this file and u_filters as well.
                   re1 = '(n2|tv(?:[1-9])?).* neg .*(n2|tv(?:[1-9])?)',
                   comment = "If the negative adverb is an independent modifier of VP, it cannot " + \
                             "intervene between the object (n2) and the verb).",
                   fv = ['and','adv-neg:on',
                         # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                         # file anymore
                         'neg-mod:vp']),

    NotMatchFilter(name = 'neg-adv-vp-2',
                   mrs_id_list = g.n2_subj_neg,
                   re1 = '(n1|tv(?:[1-9])?).* neg .*(n1|tv(?:[1-9])?)',
                   comment = "If the negative adverb is an independent modifier of VP, it cannot " + \
                             "intervene between the object (n1) and the verb).",
                   fv = ['and','adv-neg:on',
                         # 'neg-adv:ind-adv', # KEN remmed 8/14/09 b/c it's not a feature in choices
                         # file anymore
                         'neg-mod:vp']),

    # Selected modifiers:
    # If the choices file specifies the adverb as a selected complement of
    # V/Aux, then it should show up in a plausible complement position: on
    # the correct side of the verb, and either inside or outside the
    # subject, as required.  If both the affix and the adverb are present,
    # likewise, the adverb should show up in a plausible complement
    # position.  In both cases, we have to track where the subject and
    # object are.

    NotMatchFilter(name = 'sel-adv-v-init-1',
                   mrs_id_list = g.all_neg,
                   re1 = 'neg .*tv(?:[1-9])?',
                   comment = "If the word order is v-initial and neg is a selected adverb, it has to " + \
                             "appear after the verb.",
                   fv = ['and','adv-neg:on',
                         'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                         # in choices file anymore, remming it would make it
                         # apply to inflectional negation and we don't want that
                         'word-order:v-initial']),

    # 8/20/09 KEN changed from an AndNotFilter.  The old re2 is now re1 and the old re1 is
    # commented below.  the old re1 should be covered by other filters
    NotMatchFilter(name = 'sel-adv-v-init-2',
                   mrs_id_list = g.all_neg,
                   # re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re1 = 'neg .*tv(?:[1-9])?',
                   comment = "If the word order is v-initial and neg adverb is a selected adverb by " + \
                             "virtue of co-occurring with the affix, it has to appear after the verb.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:v-initial']),

    NotMatchFilter(name = 'sel-adv-v-final-1',
                   mrs_id_list = g.all_neg,
                   re1 = 'tv(?:[1-9])?.* neg',
                   comment = "If the word order is v-final and neg is a selected adverb, it has to " + \
                             "appear before the verb.",
                   fv = ['and','adv-neg:on',
                         'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                         # in choices file anymore, remming it would make it
                         # apply to inflectional negation and we don't want that
                         'word-order:v-final']),


    # this was a duplicate with the filter two above.  based on pattern, comment, and fv, KEN
    # renamed from sel-adv-v-init-2 to sel-adv-v-final-2
    # 8/20/09 KEN changed from an AndNotFilter.  The old re2 is now re1 and the old re1 is
    # commented below.  the old re1 should be covered by other filters
    NotMatchFilter(name = 'sel-adv-v-final-2',
                   mrs_id_list = g.all_neg,
                   # re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re1 = 'tv(?:[1-9])?.* neg',
                   comment = "If the word order is v-final and neg adverb is a selected adverb by " + \
                             "virtue of co-occurring with the affix, it has to appear before the verb.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:v-final']),

    MatchFilter(name = 'sel-adv-vo-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'tv(?:[1-9])?( n2| aux){,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg " + \
                          "has to appear after the verb with at most an aux and the object " + \
                          "(here, n2) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      ['or','word-order:svo','word-order:vos']]),

    MatchFilter(name = 'sel-adv-vo-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'tv(?:[1-9])?( n1| aux){,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg " + \
                          "has to appear after the verb with at most an aux and the object " + \
                          "(here, n1) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      ['or','word-order:svo','word-order:vos']]),

    IfFilter(name = 'sel-adv-vo-3',
             mrs_id_list = g.n1_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'tv(?:[1-9])?(-neg){,1}( n2| aux){,2} neg',
             comment = "If the word order is SVO or VOS and neg is a selected adverb by " + \
                       "virtue of co-occuring with the negative affix, neg has to appear after " + \
                       "the verb with at most an aux and the object (here, n2) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:svo','word-order:vos']]),

    IfFilter(name = 'sel-adv-vo-4',
             mrs_id_list = g.n2_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'tv(?:[1-9])?(-neg){,1}( n1| aux){,2} neg',
             comment = "If the word order is SVO or VOS and neg is a selected adverb by " + \
                       "virtue of co-occuring with the negative affix, neg has to appear after " + \
                       "the verb with at most an aux and the object (here, n1) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:svo','word-order:vos']]),

    MatchFilter(name = 'sel-adv-ov-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'neg( n2| aux){,2} (neg-){,1}tv(?:[1-9])?',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg " + \
                          "has to appear before the verb with at most an aux and the object " + \
                          "(here, n2) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      ['or','word-order:svo','word-order:ovs']]),

    MatchFilter(name = 'sel-adv-ov-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'neg( n1| aux){,2} (neg-){,1}tv(?:[1-9])?',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg " + \
                          "has to appear before the verb with at most an aux and the object " + \
                          "(here, n1) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      ['or','word-order:svo','word-order:ovs']]),

    IfFilter(name = 'sel-adv-ov-3',
             mrs_id_list = g.n1_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'neg( n2| aux){,2} (neg-){,1}tv(?:[1-9])?',
             comment = "If the word order is SOV or OVS and neg is a selected adverb by " + \
                       "virtue of co-occuring with the negative affix, neg has to appear before " + \
                       "the verb with at most an aux and the object (here, n2) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:sov','word-order:ovs']]),

    IfFilter(name = 'sel-adv-ov-4',
             mrs_id_list = g.n2_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'neg( n1| aux){,2} (neg-){,1}tv(?:[1-9])?',
             comment = "If the word order is SOV or OVS and neg is a selected adverb by " + \
                       "virtue of co-occuring with the negative affix, neg has to appear before " + \
                       "the verb with at most an aux and the object (here, n1) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:sov','word-order:ovs']]),

    MatchFilter(name = 'sel-adv-vso-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'n1( n2){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to " + \
                          "appear after the subject with at most the object (here, n2) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      'word-order:vso']),

    MatchFilter(name = 'sel-adv-vso-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'n2( n1){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to " + \
                          "appear after the subject with at most the object (here, n1) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      'word-order:vso']),

    IfFilter(name = 'sel-adv-vso-3',
             mrs_id_list = g.n1_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'n1( n2){,1} neg',
             comment = "If the word order is VSO and neg is a selected adverb by virtue of " + \
                       "co-occuring with the negative affix, neg has to appear after the " + \
                       "subject with at most the object (here, n2) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on','word-order:vso']),

    IfFilter(name = 'sel-adv-vso-4',
             mrs_id_list = g.n2_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'n2( n1){,1} neg',
             comment = "If the word order is VSO and neg is a selected adverb by virtue of " + \
                       "co-occuring with the negative affix, neg has to appear before the verb " + \
                       "with at most the object (here, n1) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on','word-order:vso']),

    MatchFilter(name = 'sel-adv-osv-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'neg( n2){,1} n1',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to " + \
                          "appear before the subject with at most the object (here, n2) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      'word-order:osv']),

    MatchFilter(name = 'sel-adv-osv-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'neg( n1){,1} n2',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to " + \
                          "appear before the subject with at most the object (here, n1) intervening.",
                fv = ['and','adv-neg:on',
                      'neg-adv:sel-adv', # KEN left in 8/14/09 b/c even though it's not a feature
                      # in choices file anymore, remming it would make it
                      # apply to inflectional negation and we don't want that
                      'word-order:osv']),


    IfFilter(name = 'sel-adv-osv-3',
             mrs_id_list = g.n1_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'neg( n2){,1} n1',
             comment = "If the word order is VSO and neg is a selected adverb by virtue of " + \
                       "co-occuring with the negative affix, neg has to appear before the " + \
                       "subject with at most the object (here, n2) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on','word-order:osv']),

    IfFilter(name = 'sel-adv-osv-4',
             mrs_id_list = g.n2_subj_neg,
             re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
             re2 = 'neg( n1){,1} n2',
             comment = "If the word order is VSO and neg is a selected adverb by virtue of " + \
                       "co-occuring with the negative affix, neg has to appear before the " + \
                       "subject with at most the object (here, n1) intervening.",
             fv = ['and','adv-neg:on','infl-neg:on','word-order:osv']),

    ######################################################################
    # Filters for yes-no questions

    # 8/14/09 KEN added by starting from neg-infl
    NotMatchFilter(name = "ques-infl",
                   mrs_id_list = g.all_ques,
                   re1 = '-ques|ques-',
                   comment = "If we haven't selected verbal inflection on yes-no questions, we " + \
                             "shouldn't see the affix.",
                   fv = ['q-infl:']),

    FalseFilter(name = 'no-overt-q',
                mrs_id_list = g.all_ques,
                comment = "If a language has no over mark of yes-no questions, reject all " + \
                          "strings associated with the ques[1-4] mrs_ids.",
                fv = ['and','q-part:','q-infl:','q-inv:']),

    MatchFilter(name = 'always-qpart',
                mrs_id_list = g.all_ques,
                re1 = 'qpart',
                comment = "Currently, we don't allow question particles to co-exist with other " + \
                          "overt question marking, so if this strategy is selected, all question " + \
                          "mrs_ids must be expressed with a qpart in the sentence.",
                fv = ['q-part:on']),

    MatchFilter(name = 'qpart-init',
                mrs_id_list = g.all_ques,
                re1 = '^qpart',
                comment = "If the language type definition says that qpart is sentence-initial, " + \
                          "make sure it's at the beginning of the string.",
                fv = ['q-part-order:before']),

    MatchFilter(name = 'qpart-final',
                mrs_id_list = g.all_ques,
                re1 = 'qpart$',
                comment = "If the language type definition says that qpart is sentence-final, " + \
                          "make sure it's at the end of the string.",
                fv = ['q-part-order:after']),


    # Now for the tricky part.  If the language uses inversion, then
    # we should only allow the ques MRSs when we get the appropriate word order.
    # The inversion lexical rule (as written) makes the subject be the first
    # thing on the COMPS list, of either an auxiliary or a main verb.
    # I'd actally be really surprised to find this strategy outside SVO languages
    # anyway...

    # Again, these are written assuming that only one strategy at a time is
    # allowed for questions.  If that's not the case, we'll need to rule out
    # the possibility that we're looking at strings with another strategy first,
    # e.g., by checking for the presence of qpart.

    # S V O -> V S O
    # V O S -> V S O
    # S V O -> Aux S V O (for either VP or V comp auxiliaries.  S comp auxes just
    #                     shouldn't allow this option).
    # V O S -> Aux S V O
    # S V O -> V O S Aux (if the aux follows)
    # V O S -> V O S Aux (like this would be described as subj-aux inversion!)

    # word-order:svo or word-order:vos <- same outcomes, so can be collapsed
    # q-inv-verb:main or q-inv-verb:aux or q-inv-verb:main-aux <- need separate filters
    # aux-order:before or aux-order:after <- need separate filteres
    # Assuming that validate.py is ruling out subj-aux inversion with s-comp auxes.

    MatchFilter(name = "svo-vos-inv-1",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'tv(?:[1-9])?.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is no auxiliary. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "svo-vos-inv-2",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv(?:[1-9])?.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is no auxiliary. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "svo-vos-inv-3",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* tv(?:[1-9])?.* n2',
                comment = "This filter checks for the right word order in questions when the " + \
                          "word order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux',
                      'aux-order:before']),

    MatchFilter(name = "svo-vos-inv-4",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* tv(?:[1-9])?.* n1',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux',
                      'aux-order:before']),

    MatchFilter(name = "svo-vos-inv-5",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'tv(?:[1-9])?.* n2.* n1.* aux',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux',
                      'aux-order:after']),

    MatchFilter(name = "svo-vos-inv-6",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv(?:[1-9])?.* n1.* n2.* aux',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "svo-vos-inv-7",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1 .*tv(?:[1-9])?.* n2|tv(?:[1-9])?.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which precedes V/VP. " + \
                          "Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:before']),

    MatchFilter(name = "svo-vos-inv-8",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2 .*tv(?:[1-9])?.* n1|tv(?:[1-9])?.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which precedes V/VP. " + \
                          "Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:before']),

    MatchFilter(name = "svo-vos-inv-9",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'tv(?:[1-9])?.* n2.* n1.* aux|tv(?:[1-9])?.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which follows V/VP. " + \
                          "Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:after']),

    MatchFilter(name = "svo-vos-inv-10",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv(?:[1-9])?.* n1.* n2.* aux|tv(?:[1-9])?.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is SVO or VOS, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which follows V/VP. " + \
                          "Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:after']),

    # O V S -> O S V
    # S O V -> O S V
    # O V S -> O V S Aux
    # S O V -> O V S Aux
    # O V S -> Aux S O V
    # S O V -> Aux S O V

    MatchFilter(name = "ovs-sov-inv-1",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2.* n1 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is no auxiliary. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "ovs-sov-inv-2",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1.* n2 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is no auxiliary. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "ovs-sov-inv-3",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* n2 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux',
                      'aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-4",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* n1 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux',
                      'aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-5",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2 .*tv(?:[1-9])?.* n1.* aux',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-6",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1 .*tv(?:[1-9])?.* n2.* aux',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-7",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* n2 .*tv(?:[1-9])?|n2.* n1 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which precedes V/VP. " + \
                          "Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux','aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-8",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* n1 .*tv(?:[1-9])?|n1.* n2 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which precedes V/VP. " + \
                          "Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-9",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2 .*tv(?:[1-9])?.* n1.* aux|n2.* n1 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which follows V/VP. " + \
                          "Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-10",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1 .*tv(?:[1-9])?.* n2.* aux|n1.* n2 .*tv(?:[1-9])?',
                comment = "This filter checks for the right word order in questions when the word " + \
                          "order is OVS or SOV, questions are marked by subject-verb " + \
                          "inversion, and there is potentially an auxiliary which follows V/VP. " + \
                          "Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux',
                      'aux-order:after']),

    # V S O -> V S O
    # V S O -> Aux S V O (VP auxes won't work in VSO languages)
    # V S O -> V O S Aux
    # O S V -> O S V
    # O S V -> O V S Aux (VP auxes won't work in OSV languages)
    # O S V -> Aux S O V

    # As for V-final or V-initial languages, the predictions are weird.
    # That is, we get homophony with declaratives.  I think we should just
    # constrain validate.py to refuse to implement this strategy in V-final
    # or V-initial languages.  ... In fact, I'm going to do that.
    # Likewise for free word order.  How could you use word order to
    # mark questions, if word order is free in general?

    ######################################################################
    # Filters for lexical properties

    # Non-finite forms of verbs.  Note that the requirement that all -nf forms
    # be in the context of an auxiliary is handled as a universal filter.
    # _FIX_ME_: Be sure that string list has some -nf in sentences without aux.

    # 8/25/09 KEN created to replace old non-finite-verb-1 and non-finite-verb-2 below.  There
    # were two main changes.  First the fv list was made regexy to deal with iterative features in
    # choices file.  Second, the two old filters were merged because there aren't separate features
    # for transitive and intransitive anymore.  Note other possible values to
    # aux1_compfeature1_value feature are 'nonfinite', and 'finite, nonfinite' if both are allowed.
    # NB: one problem with this is if we have more than one aux and one of them does not take
    # nonfinites, this filter doesn't make sure that the -nf is on the appropriate verb...just that it's
    # somewhere in the sentence if an aux is, too.

    IfFilter(name = "non-finite-verb-1",
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = '-nf|nf-',
             comment = "If any aux in the langauge type only takes non-finite " + \
                       "complements, make sure that if nf- or -nf is in the sentence that " + \
                       "aux is in as well",
             fv = ['aux[1-9]_compfeature[1-9]_value:nonfinite']),

    ################################################################
    # 8/25/09 KEN remmed these two filters out and combined them into filter above
    #IfFilter(name = "non-finite-verb-1",
    #         mrs_id_list = g.all,
    #         re1 = 'aux.* iv(?:[1-9])?|iv(?:[1-9])? .*aux',
    #         re2 = 'iv(?:[1-9])?-nf',
    #          comment = "If auxiliaries take complements in non-finite form, check whether the " + \
    #                           "other verb is non-finite, if an auxiliary is present.  Note that this " + \
    #                           "assumes at most one non-aux verb, and will need to be updated. " + \
    #                           "Because the non-finite forms are handled separately for each verb, " + \
    #                           "need to do filters separately for iv and tv.",
    #         fv = ['iverb-non-finite:iv-nf']),
    #IfFilter(name = "non-finite-verb-2",
    #         mrs_id_list = g.all,
    #         re1 = 'aux.* tv(?:[1-9])?|tv(?:[1-9])? .*aux',
    #         re2 = 'tv(?:[1-9])?-nf',
    #         comment = "If auxiliaries take complements in non-finite form, check whether the " + \
    #                           "other verb is non-finite, if an auxiliary is present.  Note that this " + \
    #                            "assumes at most one non-aux verb, and will need to be updated. " + \
    #                            "Because the non-finite forms are handled separately for each verb, " + \
    #                             "need to do filters separately for iv and tv.",
    #         fv = ['tverb-non-finite:tv-nf']),
    ################################################################


    # 8/21/09 KEN modifed fv list to deal with changes to choices file.  Instead of looking for
    # absence of iverb-non-finite feature, it now checks to see if any aux takes only finite as a
    # complement and if so it eliminates any intransitive verb followed by -nf.  Note other
    # possible values to aux1_compfeature1_value feature are 'nonfinite', and 'finite, nonfinite' if
    # both are allowed.  NB: one problem with this is if we have more than one aux and one of
    # them does allow nonfinites, this filter will disallow -nf on every intransitive verb
    # 8/21/09 KEN also modified re1 to deal with both intransitive and transitive verbs to merge
    # with old non-finite-verb-2 and also modified re1 to deal with nf as an affix not as suffix always
    # adjacent to verb
    NotMatchFilter(name = "non-finite-verb-3",
                   mrs_id_list = g.all,
                   re1 = '(?:[it]v[1-9]?\S*-nf)|(?:nf-\S*[it]v[1-9])',
                   comment = "If any aux in the langauge type only takes finite complements, we " + \
                             "should disallow an nf affix on all verbs",
                   fv = ['aux[1-9]_compfeature[1-9]_value:finite']),

    # 8/21/09 KEN merged with non-finite-verb-3 since we now don't have separate features for
    # trans and intrans verbs taking non-finites and instead deal with it on the aux complement,
    # and the customization system assumes that what the aux from takes as a complement it
    # takes the same for intransitive and transitive verbs
    #    NotMatchFilter(name = "non-finite-verb-4",
    #                   mrs_id_list = g.all,
    #                   re1 = 'tv(?:[1-9])?-nf',
    #                   comment = "If the transitive verb doesn't take a special form after the auxiliary, " + \
    #                                     "then we shouldn't see tv-nf.",
    #                   fv = ['tverb-non-finite:']),

    # 9/5/09 KEN added
    # NB: If some language allows nf as both a suffix and a prefix, then there is no way yet for
    # MatrixTDB to handle this due to a lack of logic (no 'not's) in the fv lists
    NotMatchFilter(name = 'nf-infl-right',
                   mrs_id_list = g.all,
                   re1 = 'nf-',
                   comment = 'If non-finite inflection happens after the verb, then the "nf-" prefix ' + \
                             'cannot appear in the sentence ',
                   # This is the first filter to use groups and backreferences to ensure that the same
                   # slot that is negation is the one that specifies it comes after the verb
                   fv = ['and', 'verb-slot([1-9])_morph[1-9]_feat[1-9]_name:nonfinite',
                         r'verb-slot\1_order:after']),

    # 9/5/09 KEN added
    # NB: If some language allows nf as both a suffix and a prefix, then there is no way yet for
    # MatrixTDB to handle this due to a lack of logic (no 'not's) in the fv lists
    NotMatchFilter(name = 'nf-infl-left',
                   mrs_id_list = g.all,
                   re1 = '-nf',
                   comment = 'If non-finite inflection happens before the verb, then the "-nf" suffix ' + \
                             'cannot appear in the sentence ',
                   fv = ['and', 'verb-slot([1-9])_morph[1-9]_feat[1-9]_name:nonfinite',
                         r'verb-slot\1_order:before']),

    # Obligatory specifiers.  Since overt specifiers change the MRS, consider keying this off the
    # mrs_id instead of the string.

    MatchFilter(name = "n1-obl-spr-1",
                mrs_id_list = g.all,
                re1 = 'det n1',
                comment = "If n1 is defined as having an obligatory specifier, and determiners " + \
                          'come before nouns, the string det n1 must be in all sentences since ' + \
                          'they all have n1.',
                fv = ['and', 'noun1_det:obl', 'noun-det-order:det-noun']),

    MatchFilter(name = "n1-obl-spr-2",
                mrs_id_list = g.all,
                re1 = 'n1 det',
                comment = "If n1 is defined as having an obligatory specifier, and determiners " + \
                          'come after nouns, the string n1 det must be in all sentences since they' + \
                          'all have n1.',
                fv = ['and', 'noun1_det:obl', 'noun-det-order:noun-det']),

    MatchFilter(name = "n2-obl-spr-1",
                mrs_id_list = g.trans,
                re1 = 'det n2',
                comment = "If n2 is defined as having an obligatory specifier, and determiners " + \
                          'come before nouns, the string det n2 must be in all transitive ' + \
                          'sentences since those all have n2.',
                fv = ['and', 'noun2_det:obl', 'noun-det-order:det-noun']),

    MatchFilter(name = "n2-obl-spr-2",
                mrs_id_list = g.all,
                re1 = 'n2 det',
                comment = "If n2 is defined as having an obligatory specifier, and determiners " + \
                          'come after nouns, the string det n2 must be in all transitive sentences' + \
                          'since those all have n2.',
                fv = ['and', 'noun2_det:obl', 'noun-det-order:noun-det']),

    # Impossible specifiers.  Since overt specifiers change the MRS, keying off the mrs_id instead of the string.  That is,
    # the regexes are dummies here.
    # ERB 2007-06-05 Recasting as AlwaysFilter
    # SFD 2007-10-31 AlwaysFilter -> FalseFilter


    FalseFilter(name = "n1-no-spr",
                mrs_id_list = g.n1_with_det,
                comment = "If n1 is defined as having no specifier possible, then no string with " + \
                          "an mrs_id corresponding to an overt det on n1 should be grammatical.",
                fv = ['noun1_det:imp']),

    FalseFilter(name = "n2-no-spr",
                mrs_id_list = g.n2_with_det,
                comment = "If n2 is defined as having no specifier possible, then no string with " + \
                          "an mrs_id corresponding to an overt det on n2 should be grammatical.",
                fv = ['noun2_det:imp']),

    # Check whether subject is NP or PP, as appropriate.  Allow for auxiliaries to control choice of subject form.

    IfFilter(name = "tv-np-subj-no-aux",
             mrs_id_list = g.trans,
             re1 = 'p-nom',
             re2 = 'aux',
             comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the " + \
                       "string at all.  In order to handle the cases with and without axuiliaries " + \
                       "separately, this one only allows strings with p-nom through if they " + \
                       "also have an auxiliary.  Only applies to mrs_ids corresponding to tv.",
             fv = ['tverb-subj:np']),

    IfFilter(name = "iv-np-subj-no-aux",
             mrs_id_list = g.intrans,
             re1 = 'p-nom',
             re2 = 'aux',
             comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the " + \
                       "string at all.  In order to handle the cases with and without axuiliaries " + \
                       "separately, this one only allows strings with p-nom through if they " + \
                       "also have an auxiliary.  Only applies to mrs_ids corresponding to iv.",
             fv = ['iverb-subj:np']),

    # 8/20/09 KEN changed from AndNotFilter based on what comment said it should do and
    # e-mail exchange with Emily
    IfNotFilter(name = "tv-np-subj-scomp-aux",
                mrs_id_list = g.trans,
                re1 = 'aux',
                re2 = 'p-nom',
                comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in " + \
                          "the string at all.  In order to handle the cases with and without " + \
                          "axuiliaries separately, this one only looks at strings with an " + \
                          "auxiliary and only in language types with s-comp auxes..  Only " + \
                          "applies to mrs_ids corresponding to tv.",
                fv = ['and','tverb-subj:np','aux-comp:s']),

    # 8/20/09 KEN changed from AndNotFilter based on what comment said it should do and
    # e-mail exchange with Emily
    IfNotFilter(name = "iv-np-subj-scomp-aux",
                mrs_id_list = g.intrans,
                re1 = 'aux',
                re2 = 'p-nom',
                comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in " + \
                          "the string at all.  In order to handle the cases with and without " + \
                          "axuiliaries separately, this one only looks at strings with an " + \
                          "auxiliary and only in language types with s-comp auxes..  Only " + \
                          "applies to mrs_ids corresponding to iv.",
                fv = ['and','iverb-subj:np','aux-comp:s']),

    # 8/20/09 KEN changed from AndNotFilter based on what comment said it should do and
    # e-mail exchange with Emily
    IfNotFilter(name = "v*comp-aux-np-subj",
                mrs_id_list = g.all,
                re1 = 'aux',
                re2 = 'p-nom',
                comment = "If there is a V or VP-complement auxiliary present and that " + \
                          "auxiliary says subj is NP, we shouldn't see p-nom at all.",
                fv = ['and',['or','aux-comp:v','aux-comp:vp'],'aux-subj:np']),

    OrFilter(name = "tv-pp-subj-no-aux",
             mrs_id_list = g.trans,
             re1 = 'aux', #only if there's no aux
             re2 = 'p-nom', #require p-nom
             comment = "If the subject is supposed to be a PP, we should see p-nom.  In order " + \
                       "to handle the cases with and without axuiliaries separately, this " + \
                       "one only checks for p-nom if it doesn't see an auxiliary.  Only " + \
                       "applies to mrs_ids corresponding to tv.",
             fv = ['tverb-subj:adp']),

    OrFilter(name = "iv-pp-subj-no-aux",
             mrs_id_list = g.intrans,
             re1 = 'aux', #only if there's no aux
             re2 = 'p-nom', #require p-nom
             comment = "If the subject is supposed to be a PP, we should see p-nom.  In order " + \
                       "to handle the cases with and without axuiliaries separately, this " + \
                       "one only checks for p-nom if it doesn't see an auxiliary.  Only " + \
                       "applies to mrs_ids corresponding to iv.",
             fv = ['iverb-subj:adp']),

    IfFilter(name = "tv-pp-subj-scomp-aux",
             mrs_id_list = g.trans,
             re1 = 'aux',
             re2 = 'p-nom',
             comment = "If the subject is supposed to be a PP, we should see p-nom.  In order " + \
                       "to handle the cases with and without axuiliaries separately, this " + \
                       "one only looks at strings with an auxiliary and only in language " + \
                       "types with s-comp auxes.  Only applies to mrs_ids corresponding " + \
                       "to tv.",
             fv = ['and','tverb-subj:adp','aux-comp:s']),

    IfFilter(name = "iv-pp-subj-scomp-aux",
             mrs_id_list = g.intrans,
             re1 = 'aux',
             re2 = 'p-nom',
             comment = "If the subject is supposed to be a PP, we should see p-nom.  In order " + \
                       "to handle the cases with and without axuiliaries separately, this " + \
                       "one only looks at strings with an auxiliary and only in language " + \
                       "types with s-comp auxes.  Only applies to mrs_ids corresponding " + \
                       "to iv.",
             fv = ['and','iverb-subj:adp','aux-comp:s']),

    IfFilter(name = "v*comp-aux-pp-subj",
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = 'p-nom',
             comment = "If there is a V or VP-complement auxiliary present and that auxiliary " + \
                       "says subj is PP, p-nom should be present.",
             fv = ['and',['or','aux-comp:v','aux-comp:vp'],'aux-subj:adp']),


    # Check for NP v. PP on object of tverb.

    NotMatchFilter(name = 'tv-np-comp',
                   mrs_id_list = g.trans,
                   re1 = 'p-acc',
                   comment = "If tv is defined to take an NP complement, we shouldn't see p-acc " + \
                             "at all.",
                   fv = ['tverb-obj:np']),

    MatchFilter(name = 'tv-pp-comp',
                mrs_id_list = g.trans,
                re1 = 'p-acc',
                comment = "If tv is defined to take a PP complement, the string should contain " + \
                          "p-acc.",
                fv = ['tverb-obj:adp']),

    # Check for order of adpositions with respect to object and subject.

    IfFilter(name = 'pnom-prep-1',
             mrs_id_list = g.n1_subj,
             re1 = 'p-nom',
             re2 = 'p-nom.* n1',
             comment = "If p-nom is a preposition, then when it is present and n1 is the " + \
                       "subject, p-nom should precede n1.",
             fv = ['subj-adp-order:before']),

    IfFilter(name = 'pnom-prep-2',
             mrs_id_list = g.n2_subj_n1_obj,
             re1 = 'p-nom',
             re2 = 'p-nom.* n2',
             comment = "If p-nom is a preposition, then when it is present and n2 is the " + \
                       "subject, p-nom should precede n2.",
             fv = ['subj-adp-order:before']),

    IfFilter(name = 'pnom-post-1',
             mrs_id_list = g.n1_subj,
             re1 = 'p-nom',
             re2 = 'n1.* p-nom',
             comment = "If p-nom is a postposition, then when it is present and n1 is the " + \
                       "subject, p-nom should follow n1.",
             fv = ['subj-adp-order:after']),

    IfFilter(name = 'pnom-post-2',
             mrs_id_list = g.n2_subj_n1_obj,
             re1 = 'p-nom',
             re2 = 'n2.* p-nom',
             comment = "If p-nom is a postposition, then when it is present and n2 is the " + \
                       "subject, p-nom should follow n2.",
             fv = ['subj-adp-order:after']),

    ##
    IfFilter(name = 'pacc-prep-1',
             mrs_id_list = g.n2_subj_n1_obj,
             re1 = 'p-acc',
             re2 = 'p-acc.* n1',
             comment = "If p-acc is a preposition, then when it is present and n1 is the object, " + \
                       "p-acc should precede n1.",
             fv = ['obj-adp-order:before']),

    IfFilter(name = 'pacc-prep-2',
             mrs_id_list = g.n1_subj_n2_obj,
             re1 = 'p-acc',
             re2 = 'p-acc.* n2',
             comment = "If p-acc is a preposition, then when it is present and n2 is the object, " + \
                       "p-acc should precede n2.",
             fv = ['obj-adp-order:before']),

    IfFilter(name = 'pacc-post-1',
             mrs_id_list = g.n2_subj_n1_obj,
             re1 = 'p-acc',
             re2 = 'n1.* p-acc',
             comment = "If p-acc is a postposition, then when it is present and n1 is the object, " + \
                       "p-acc should follow n1.",
             fv = ['obj-adp-order:after']),

    IfFilter(name = 'pacc-post-2',
             mrs_id_list = g.n1_subj_n2_obj,
             re1 = 'p-acc',
             re2 = 'n2.* p-acc',
             comment = "If p-acc is a postposition, then when it is present and n2 is the object, " + \
                       "p-acc should follow n2.",
             fv = ['obj-adp-order:after']),


    # Placement of auxiliaries with respect to the rest of the sentence.

    # 1. No auxiliaries.

    NotMatchFilter(name = 'no-aux',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   comment = "If the language doesn't have an auxiliary, we should never see " + \
                             "aux in the string.",
                   # 8/20/09 KEN changed fv list from 'aux-verb:', which looks like it's a deprecated
                   # feature.  has-aux seems to always be present and be either yes or no
                   fv = ['has-aux:no']),


    # 2. V-comp auxiliaries.

    # TODO: I think this might need to be adjusted to allow for prefixes on the verb
    IfFilter(name = 'vcomp-aux-auxleft',
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = 'aux [ti]v(?:[1-9])?',
             comment = "If the auxiliary takes a V complement and appears before the verb, it " + \
                       "should be to the left of the V, in any wo except free.",
             fv = ['and','aux-comp:v','aux-order:before',['or','word-order:sov','word-order:svo',
                                                          'word-order:osv','word-order:vos',
                                                          'word-order:ovs','word-order:vso',
                                                          'word-order:v-final','word-order:v-initial']]),

    # TODO: I think this might need to be adjusted to allow for more suffixes on the verb
    IfFilter(name = 'vcomp-aux-auxright',
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])? aux|[ti]v(?:[1-9])?-nf aux',
             comment = "If the auxiliary takes a V complement and appears after the verb, it " + \
                       "should be to the right of the V, in any wo except free.  Allow for -nf " + \
                       "forms of the verb.",
             fv = ['and','aux-comp:v','aux-order:after',['or','word-order:sov','word-order:svo',
                                                         'word-order:osv','word-order:vos',
                                                         'word-order:ovs','word-order:vso',
                                                         'word-order:v-final','word-order:v-initial']]),

    IfFilter(name = 'vcomp-aux-either-order',
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])? aux|[ti]v(?:[1-9])?-nf aux|aux [ti]v(?:[1-9])?',
             comment = "If the auxiliary takes a V complement and can appear on either side of " + \
                       "the V, it should be adjacent to the V, in any wo except free.  Allow for " + \
                       "-nf forms of the verb.",
             fv = ['and','aux-comp:v','aux-order:after',['or','word-order:sov','word-order:svo',
                                                         'word-order:osv','word-order:vos',
                                                         'word-order:ovs','word-order:vso',
                                                         'word-order:v-final','word-order:v-initial']]),

    # 3. VP comp auxiliaries

    # 8/19/09 KEN changed from AndNotFilter based on discussion with Emily
    # TODO: These next two filters don't need to be IfFilters, they can be NotMatchFilters    
    IfNotFilter(name = 'vpcomp-n1-subj',
                mrs_id_list = g.n1_subj_n2_obj_not_ques,
                re1 = 'aux',
                re2 = 'aux.* n1 .*tv(?:[1-9])?|tv(?:[1-9])?.* n1.* aux',
                comment = "If the auxiliary takes a VP complement, and n1 is the subject, " + \
                          "then n1 shouldn't appear between the auxiliary and the verb in " + \
                          "non-questions. Exception is free word order.",
                fv = ['and','aux-comp:vp',['or','word-order:sov','word-order:svo','word-order:osv',
                                           'word-order:vos','word-order:ovs','word-order:vso',
                                           'word-order:v-final','word-order:v-initial']]),


    # 8/19/09 KEN changed from AndNotFilter based on discussion with Emily
    IfNotFilter(name = 'vpcomp-n2-subj',
                mrs_id_list = g.n2_subj_n1_obj_not_ques,
                re1 = 'aux',
                re2 = 'aux.* n2 .*tv(?:[1-9])?|tv(?:[1-9])?.* n2.* aux',
                comment = "If the auxiliary takes a VP complement, and n2 is the subject, then " + \
                          "n2 shouldn't appear between the auxiliary and the verb in " + \
                          "non-questions. Exception is free word order.",
                fv = ['and','aux-comp:vp',['or','word-order:sov','word-order:svo','word-order:osv',
                                           'word-order:vos','word-order:ovs','word-order:vso',
                                           'word-order:v-final','word-order:v-initial']]),

    # TODO: Is re2 written right?  Based on comment it seems the things in parens should each
    # be followed by '?'
    IfFilter(name = 'vpcomp-aux-free-1',
             mrs_id_list = g.n1_subj_n2_obj,
             re1 = 'aux',
             re2 = 'tv(?:[1-9])?(-nf) (neg )(p-acc )(det )n2|n2 (det )(p-acc )(neg )tv(?:[1-9])?',
             comment = "If the languge has free word order but VP-comp auxiliaries, then the " + \
                       "only things that can intervene between tv and its object (for now) are " + \
                       "det, p-acc, and neg.  Here object = n2.",
             fv = ['and','aux-comp:vp','word-order:free']),

    # TODO: Is re2 written right?  Based on comment it seems the things in parens should each
    # be followed by '?'
    IfFilter(name = 'vpcomp-aux-free-2',
             mrs_id_list = g.n2_subj_n1_obj,
             re1 = 'aux',
             re2 = 'tv(?:[1-9])?(-nf) (neg )(p-acc )( )n1|n1 (det )(p-acc )(neg )tv(?:[1-9])?',
             comment = "If the languge has free word order but VP-comp auxiliaries, then the " + \
                       "only things that can intervene between tv and its object (for now) are " + \
                       "det, p-acc, and neg.  Here object = n1.",
             fv = ['and','aux-comp:vp','word-order:free']),

    # 8/25/09 KEN added the next ## filters to ensure the verb and object are on the right side
    # of the aux in the case of vp comp auxes
    IfFilter(name = 'vpcomp-aux-vp-order-1',
             mrs_id_list = g.n1_subj_n2_obj, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = 'aux.*(?:(?:tv[1-9]?.*n2)|(?:n2.*tv[1-9]?))',
             comment = "If the language has VP-comp auxiliaries and the aux comes before its " + \
                       "complement and the sentence has n2 as its object, then make sure " + \
                       "both the verb and the object occur after the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:before']),

    IfFilter(name = 'vpcomp-aux-vp-order-2',
             mrs_id_list = g.n2_subj_n1_obj, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = 'aux.*(?:(?:tv[1-9]?.*n1)|(?:n1.*tv[1-9]?))',
             comment = "If the language has VP-comp auxiliaries and the aux comes before its " + \
                       "complement and the sentence has n1 as its object, then make sure " + \
                       "both the verb and the object occur after the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:before']),

    IfFilter(name = 'vpcomp-aux-vp-order-3',
             mrs_id_list = g.n1_subj_n2_obj, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = '(?:(?:tv[1-9]?.*n2)|(?:n2.*tv[1-9]?)).*aux',
             comment = "If the language has VP-comp auxiliaries and the aux comes after its " + \
                       "complement and the sentence has n2 as its object, then make sure " + \
                       "both the verb and the object occur before the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:after']),

    IfFilter(name = 'vpcomp-aux-vp-order-4',
             mrs_id_list = g.n2_subj_n1_obj, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = '(?:(?:tv[1-9]?.*n1)|(?:n1.*tv[1-9]?)).*aux',
             comment = "If the language has VP-comp auxiliaries and the aux comes after its " + \
                       "complement and the sentence has n1 as its object, then make sure " + \
                       "both the verb and the object occur before the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:after']),

    IfFilter(name = 'vpcomp-aux-vp-order-5',
             mrs_id_list = g.intrans, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = 'aux.*iv[1-9]?',
             comment = "If the language has VP-comp auxiliaries and the aux comes before its " + \
                       "complement and the sentence is intransitive, then make sure " + \
                       "the verb occurs after the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:before']),

    IfFilter(name = 'vpcomp-aux-vp-order-6',
             mrs_id_list = g.intrans, # TODO: should this be different for negs or questions?
             re1 = 'aux',
             re2 = 'iv[1-9]?.*iv',
             comment = "If the language has VP-comp auxiliaries and the aux comes after its " + \
                       "complement and the sentence is intransitive, then make sure " + \
                       "the verb occurs before the aux",
             fv = ['and','aux-comp:vp','aux-comp-order:after']),

    # 4. S comp

    # 8/20/09 KEN changed from AndNotFilter based on what comment said it should do and
    # e-mail exchange with Emily
    IfNotFilter(name = 's-comp-aux',
                mrs_id_list = g.all,
                re1 = 'aux',
                re2 = 'n[12].* aux .*[ti]v(?:[1-9])?|[ti]v(?:[1-9])?.* aux.* n[12]',
                comment = "If the auxiliary takes s complements, it shouldn't intervene between " + \
                          "the verb and either noun, in any word order.",
                fv = ['aux-comp:s']),

    # Order for VP and S cases.

    IfFilter(name = 's-or-vp-comp-aux-left',
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = 'aux .*[ti]v(?:[1-9])?',
             comment = "Because the general VP/S comp filters doesn't check order, need a " + \
                       "set of second filters to check aux-v order in the VP/S comp case.  " + \
                       "This one checks for aux before v.",
             fv = ['and',['or','aux-comp:vp','aux-comp:s'],'aux-order:before']),


    IfFilter(name = 's-or-vp-comp-aux-right',
             mrs_id_list = g.all,
             re1 = 'aux',
             re2 = '[ti]v(?:[1-9])?.* aux',
             comment = "Because the general VP/S comp filters doesn't check order, need a " + \
                       "set of second filters to check aux-v order in the VP/S comp case.  " + \
                       "This one checks for aux after v.",
             fv = ['and',['or','aux-comp:s','aux-comp:vp'],'aux-order:after']),


    # Coordination filters

    # Lexical or morphological marking

    NotMatchFilter(name = 'coord-lexical-mark',
                   mrs_id_list = g.coord,
                   re1 = '-co|co-',
                   comment = "If coordination is marked lexically, then the affix must not occur",
                   fv = ['cs1_mark:word']),

    NotMatchFilter(name = 'coord-affix-mark',
                   mrs_id_list = g.coord,
                   re1 = '(^| )co( |$)',
                   comment = "If coordination is marked by an affix, then the word must not occur",
                   fv = ['cs1_mark:affix']),

    NotMatchFilter(name = 'coord-no-mark',
                   mrs_id_list = g.coord,
                   re1 = '(^| |-)co( |-|$)',
                   comment = "If coordination is unmarked then neither coordination mark can occur",
                   fv = ['cs1_pat:a']),

    # N monosyndetic

    MatchFilter(name = 'coord-n-mono-before',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1 n2 co[ -]n3',
                comment = "If N coordination is monosyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:mono','cs1_order:before']),

    MatchFilter(name = 'coord-n-mono-after',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1 n2 n3[ -]co',
                comment = "If N coordination is monosyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:mono','cs1_order:after']),

    # N polysyndetic

    MatchFilter(name = 'coord-n-poly-before',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1 co[ -]n2 co[ -]n3',
                comment = "If N coordination is polysyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:poly','cs1_order:before']),

    MatchFilter(name = 'coord-n-poly-after',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1 n2[ -]co n3[ -]co',
                comment = "If N coordination is polysyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:poly','cs1_order:after']),

    # N omnisyndetic

    MatchFilter(name = 'coord-n-omni-before',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'co[ -]n1 co[ -]n2 co[ -]n3',
                comment = "If N coordination is omnisyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:omni','cs1_order:before']),

    MatchFilter(name = 'coord-n-omni-after',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1[ -]co n2[ -]co n3[ -]co',
                comment = "If N coordination is omnisyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated N",
                fv = ['and','cs1_pat:omni','cs1_order:after']),

    # N asyndetic

    MatchFilter(name = 'coord-n-a',
                mrs_id_list = ['wo2-coord-n'],
                re1 = 'n1 n2 n3',
                comment = "If N coordination is asyndetic, there must be a properly-marked " + \
                          "coordinated N",
                fv = ['cs1_pat:a']),

    # NP monosyndetic

    MatchFilter(name = 'coord-np-mono-before',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 det n2 co det n3',
                comment = "If NP coordination is monosyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:mono','cs1_order:before']),

    MatchFilter(name = 'coord-np-mono-after',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 det n2 det n3 co',
                comment = "If NP coordination is monosyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:mono','cs1_order:after']),

    # NP polysyndetic

    MatchFilter(name = 'coord-np-poly-before',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 co det n2 co det n3',
                comment = "If NP coordination is polysyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:poly','cs1_order:before']),

    MatchFilter(name = 'coord-np-poly-after',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 det n2 co det n3 co',
                comment = "If NP coordination is polysyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:poly','cs1_order:after']),

    # NP omnisyndetic

    MatchFilter(name = 'coord-np-omni-before',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'co det n1 co det n2 co det n3',
                comment = "If NP coordination is omnisyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:omni','cs1_order:before']),

    MatchFilter(name = 'coord-np-omni-after',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 co det n2 co det n3 co',
                comment = "If NP coordination is omnisyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated NP",
                fv = ['and','cs1_pat:omni','cs1_order:after']),

    # NP asyndetic

    MatchFilter(name = 'coord-np-a',
                mrs_id_list = ['wo2-coord-np'],
                re1 = 'det n1 det n2 det n3',
                comment = "If NP coordination is asyndetic, there must be a properly-marked " + \
                          "coordinated NP",
                fv = ['cs1_pat:a']),

    # VP monosyndetic

    MatchFilter(name = 'coord-vp-mono-before',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? iv(?:[1-9])? co iv(?:[1-9])?',
                comment = "If NP coordination is monosyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:mono','cs1_order:before']),

    MatchFilter(name = 'coord-vp-mono-after',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? iv(?:[1-9])? iv(?:[1-9])? co',
                comment = "If NP coordination is monosyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:mono','cs1_order:after']),

    # VP polysyndetic

    MatchFilter(name = 'coord-vp-poly-before',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? co iv(?:[1-9])? co iv(?:[1-9])?',
                comment = "If VP coordination is polysyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:poly','cs1_order:before']),

    MatchFilter(name = 'coord-vp-poly-after',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? iv(?:[1-9])? co iv(?:[1-9])? co',
                comment = "If VP coordination is polysyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:poly','cs1_order:after']),

    # VP omnisyndetic

    MatchFilter(name = 'coord-vp-omni-before',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'co iv(?:[1-9])? co iv(?:[1-9])? co iv(?:[1-9])?',
                comment = "If VP coordination is omnisyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:omni','cs1_order:before']),

    MatchFilter(name = 'coord-vp-omni-after',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? co iv(?:[1-9])? co iv(?:[1-9])? co',
                comment = "If VP coordination is omnisyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated VP",
                fv = ['and','cs1_pat:omni','cs1_order:after']),

    # VP asyndetic

    MatchFilter(name = 'coord-vp-a',
                mrs_id_list = ['wo2-coord-vp'],
                re1 = 'iv(?:[1-9])? iv(?:[1-9])? iv(?:[1-9])?',
                comment = "If VP coordination is asyndetic, there must be a properly-marked " + \
                          "coordinated VP",
                fv = ['cs1_pat:a']),

    # S monosyndetic

    MatchFilter(name = 'coord-s-mono-before',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'det n1 iv(?:[1-9])? det n2 iv(?:[1-9])? co det n3 iv(?:[1-9])?',
                comment = "If S coordination is monosyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:mono','cs1_order:before']),

    MatchFilter(name = 'coord-s-mono-after',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'det n1 iv(?:[1-9])? det n2 iv(?:[1-9])? det n3 iv(?:[1-9])? co',
                comment = "If S coordination is monosyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:mono','cs1_order:after']),

    # S polysyndetic

    MatchFilter(name = 'coord-s-poly-before',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'det n1 iv(?:[1-9])? co det n2 iv(?:[1-9])? co det n3 iv(?:[1-9])?',
                comment = "If S coordination is polysyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:poly','cs1_order:before']),

    MatchFilter(name = 'coord-s-poly-after',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'det n1 iv(?:[1-9])? det n2 iv(?:[1-9])? co det n3 iv(?:[1-9])? co',
                comment = "If S coordination is polysyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:poly','cs1_order:after']),

    # S omnisyndetic

    MatchFilter(name = 'coord-s-omni-before',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'co det n1 iv(?:[1-9])? co det n2 iv(?:[1-9])? co det n3 iv(?:[1-9])?',
                comment = "If S coordination is omnisyndetic and the mark comes before the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:omni','cs1_order:before']),

    MatchFilter(name = 'coord-s-omni-after',
                mrs_id_list = ['wo2-coord-s'],
                re1 = 'det n1 iv(?:[1-9])? co det n2 iv(?:[1-9])? co det n3 iv(?:[1-9])? co',
                comment = "If S coordination is omnisyndetic and the mark comes after the " + \
                          "coordinand, there must be a properly-marked coordinated S",
                fv = ['and','cs1_pat:omni','cs1_order:after']),

    # S asyndetic

    MatchFilter(name = 'coord-s-a',
                mrs_id_list = ['wo2-coord-s'],
                # TODO: verify sentences with multilpe verbs don't really call for verbs with
                # specific number (e.g., v1 v2 instead of v[1-9]?.
                re1 = 'det n1 iv(?:[1-9])? det n2 iv(?:[1-9])? det n3 iv(?:[1-9])?',
                comment = "If S coordination is asyndetic, there must be a properly-marked " + \
                          "coordinated S",
                fv = ['cs1_pat:a']),

    # No case-marking.
    # added by KEN 7/21/09

    NotMatchFilter(name = 'no-case',
                   # TODO: does it need to be all?  probably, but double-check it can't be smaller
                   # it doesn't hurt that it's all, just would take longer if unnecessary.
                   mrs_id_list = g.all,
                   re1 = 'p-',
                   comment = "If the language doesn't have case-marking, we should never see " + \
                             "case-marking adpositions.",
                   fv = ['case-marking:none']),
]

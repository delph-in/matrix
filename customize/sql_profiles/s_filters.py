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

from filters import NotFilter
from filters import AndNotFilter
from filters import MatchFilter
from filters import NotCopresentFilter
from filters import AlwaysFilter
from filters import MatchAndFilter
from filters import AndMatchFilter
from filters import NegTrigMatchFilter
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
              comment = "If the word order for the language type has subjects before objects, and n1 is the subject and n2 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'word-order:svo','word-order:sov','word-order:vso']),

    MatchFilter(name = "s-o-order2",
              mrs_id_list = g.n2_subj_n1_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has subjects before objects, and n2 is the subject and n1 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'word-order:svo','word-order:sov','word-order:vso']),

    MatchFilter(name = "s-o-order3",
              mrs_id_list = g.n1_subj_n2_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has objects before subjects, and n1 is the subject and n2 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
           fv = ['or', 'word-order:ovs','word-order:osv','word-order:vos']),

    MatchFilter(name = "s-o-order4",
              mrs_id_list = g.n2_subj_n1_obj,
              re1 = 'n1.*n2',
              comment = "If the word order for the language type has objects before subjects, and n2 is the subject and n1 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'word-order:ovs','word-order:osv','word-order:vos']),

    NegTrigMatchFilter(name = "s-v-order1",
                          mrs_id_list = g.n1_subj_not_ques,
                          re1 = 'aux',
                          re2 = 'n1.* (tv|iv)',
                          comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present.  Also doesn't apply if there's an auxiliary in the sentence.  Then we need to check separately.",
                          fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    AndNotFilter(name = "s-v-order2",
                 mrs_id_list = g.n1_subj_ques,
                 re1 = 'qpart',
                 re2 = '(tv|iv).*n1',
                 comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    NegTrigMatchFilter(name = "s-v-order3",
                       mrs_id_list = g.n1_subj_not_ques,
                       re1 = 'aux',
                       re2 = '(tv|iv).* n1',
                       comment = "If the word order for the language has verbs before subjects, and n1 is the subject, check that n1 follows the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present.",
                       fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    AndNotFilter(name = "s-v-order4",
                 mrs_id_list = g.n1_subj_ques,
                 re1 = 'qpart',
                 re2 = 'n1.*(tv|iv)',
                 comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    NegTrigMatchFilter(name = "s-v-order5",
                       mrs_id_list = g.n2_subj_n1_obj_not_ques,
                       re1 = 'aux',
                       re2 = 'n2.* (tv|iv)',
                       comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  This doesn't apply to ques3 and ques4.  For those we need to check if qpart (or, eventually, question inflection) is present.  Also doesn't aply if there's an auxiliary in the sentence.  Then we need to check separately.",
                       fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    AndNotFilter(name = "s-v-order6",
                 mrs_id_list = g.n2_subj_ques,
                 re1 = 'qpart',
                 re2 = '(tv|iv).*n2',
                 comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'word-order:sov','word-order:svo','word-order:osv','word-order:v-final']),

    NegTrigMatchFilter(name = "s-v-order7",
                       mrs_id_list = g.n2_subj_n1_obj_not_ques,
                       re1 = 'aux',
                       re2 = '(tv|iv).* n2',
                       comment = "If the word order for the language has verbs before subjects, and n2 is the subject, check that n2 follows the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present. Also doesn't apply if there's an auxiliary in the sentence.  Then we need to check separately.",
                       fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    AndNotFilter(name = "s-v-order8",
                 mrs_id_list = g.n2_subj_ques,
                 re1 = 'qpart',
                 re2 = 'n2.*(tv|iv)',
                 comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']),

    AndMatchFilter(name = 's-aux-order1',
                   mrs_id_list = g.n1_subj_not_ques,
                   re1 = 'aux',
                   re2 = 'n1.* aux',
                   comment = "If the subject precedes the verb, and there is an auxiliary, and n1 is the subject, check that n1 precedes the aux.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:sov','word-order:svo','word-order:osv','word-order:v-final']]),


    AndMatchFilter(name = 's-aux-order2',
                   mrs_id_list = g.n2_subj_n1_obj_not_ques,
                   re1 = 'aux',
                   re2 = 'n2.* aux',
                   comment = "If the subject precedes the verb, and there is an auxiliary, and n2 is the subject, check that n2 precedes the aux.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:sov','word-order:svo','word-order:osv','word-order:v-final']]),

    AndMatchFilter(name = 's-aux-order3',
                   mrs_id_list = g.n1_subj_not_ques,
                   re1 = 'aux',
                   re2 = 'aux.* n1',
                   comment = "If the subject follows the verb, and there is an auxiliary, and n1 is the subject, check that n1 follows the aux.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']]),


    AndMatchFilter(name = 's-aux-order4',
                   mrs_id_list = g.n2_subj_n1_obj_not_ques,
                   re1 = 'aux',
                   re2 = 'aux.* n2',
                   comment = "If the subject follow the verb, and there is an auxiliary, and n2 is the subject, check that n2 follows the aux.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:v'],['or','word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']]),


    AndMatchFilter(name = 's-aux-order-5',
                   mrs_id_list = g.n1_subj_not_ques,
                   re1 = 'aux',
                   re2 = 'n1.* [ti]v',
                   comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the subject is in the right place wrt the main verb.  In this case the subject should precede the main verb.",
                   fv = ['and','aux-comp:s',['or','word-order:sov','word-order:svo','word-order:osv','word-order:v-final']]),

    AndMatchFilter(name = 's-aux-order-6',
                   mrs_id_list = g.n1_subj_not_ques,
                   re1 = 'aux',
                   re2 = '[ti]v.* n1',
                   comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the subject is in the right place wrt the main verb.  In this case the subject should follow the main verb.",
                   fv = ['and','aux-comp:s',['or','word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']]),

    
    AndMatchFilter(name = 's-aux-order-7',
                   mrs_id_list = g.n2_subj_n1_obj_not_ques,
                   re1 = 'aux',
                   re2 = 'n2.* [ti]v',
                   comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the subject is in the right place wrt the main verb.  In this case the subject should precede the main verb.",
                   fv = ['and','aux-comp:s',['or','word-order:sov','word-order:svo','word-order:osv','word-order:v-final']]),

    AndMatchFilter(name = 's-aux-order-8',
                   mrs_id_list = g.n2_subj_n1_obj_not_ques,
                   re1 = 'aux',
                   re2 = '[ti]v.* n2',
                   comment = "If there's an auxiliary, but it's an s-comp auxiliary, then check that the subject is in the right place wrt the main verb.  In this case the subject should follow the main verb.",
                   fv = ['and','aux-comp:s',['or','word-order:vos','word-order:ovs','word-order:vso','word-order:v-initial']]),


    NotFilter(name = "o-v-order1",
              mrs_id_list = g.n1_subj_n2_obj,
              re1 = 'tv.*n2',
              comment = "If the word order for the language has objects before verbs, and n2 is the object, check that it precedes the verb.",
              fv = ['or', 'word-order:sov','word-order:osv','word-order:ovs','word-order:v-final']),
             
    NotFilter(name = "o-v-order2",
              mrs_id_list = g.n1_subj_n2_obj,
              re1 = 'n2.*tv',
              comment = "If the word order for the language has objects after verbs, and n2 is the object, check that it follows the verb.",
              fv = ['or', 'word-order:vos','word-order:vso','word-order:svo','word-order:v-initial']),

    NotFilter(name = "o-v-order3",
              mrs_id_list = g.n2_subj_n1_obj,
              re1 = 'tv.*n1',
              comment = "If the word order for the language has objects before verbs, and n1 is the object, check that it precedes the verb.",
              fv = ['or', 'word-order:sov','word-order:osv','word-order:ovs','word-order:v-final']),
             
    NotFilter(name = "o-v-order4",
              mrs_id_list = g.n2_subj_n1_obj,
              re1 = 'n1.*tv',
              comment = "If the word order for the language has objects after verbs, and n1 is the object, check that it follows the verb.",
              fv = ['or', 'word-order:vos','word-order:vso','word-order:svo','word-order:v-initial']),


    AlwaysFilter(name = "no-dets",
                 mrs_id_list = g.with_dets,
                 comment = "If the language has no determiners, reject all strings with determiners.",
                 fv = ['has-dets:no']),

    MatchFilter(name = "det-n-order1",
              mrs_id_list = g.one_det_on_n1,
              re1 = 'det n1',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n1, the det should immediately precede n1.  Revise when we add nominal modifiers.",
              fv = ['noun-det-order:det-noun']),

    MatchFilter(name = "det-n-order2",
              mrs_id_list = g.one_det_on_n1,
              re1 = 'n1 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n1, the det should immediately follow n1.  Revise when we add nominal modifiers.",
              fv = ['noun-det-order:noun-det']),

    MatchFilter(name = "det-n-order3",
              mrs_id_list = g.one_det_on_n2,
              re1 = 'det n2',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n2, the det should immediately precede n2.  Revise when we add nominal modifiers.",
              fv = ['noun-det-order:det-noun']),

    MatchFilter(name = "det-n-order4",
              mrs_id_list = g.one_det_on_n2,
              re1 = 'n2 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n2, the det should immediately follow n2.  Revise when we add nominal modifiers.",
              fv = ['noun-det-order:noun-det']),

    NotFilter(name = "det-n-order5",
                mrs_id_list = g.two_dets_n1_n2,
                re1 = '(det .*det (n1|n2))|(det (n1|n2) .*det (n1|n2))|(det (n1|n2) .*(n1|n2) det)',
                comment = "If the language has determiners following nouns, and n1 and n2 both have determiners attached, neither should be preceded by a determiner.  This is perhaps more baroque than it needs to be?",
                fv = ['noun-det-order:noun-det']),

    NotFilter(name = "det-n-order6",
                mrs_id_list = g.two_dets_n1_n2,
                re1 = '((n1|n2) det .*det)|((n1|n2) det .*(n1|n2) det)|(det (n1|n2) .*(n1|n2) det)',
                comment = "If the language has determiners precedings nouns, and n1 and n2 both have determiners attached, neither should be followed by a determiner.  This is perhaps more baroque than it needs to be?",
                fv = ['noun-det-order:det-noun']),

####################################################################
# Negation filters

    NotFilter(name = "neg-infl",
                mrs_id_list = g.all_neg,
                re1 = '-neg|neg-',
                comment = "If we haven't selected inflectional negation, we shouldn't see the affix. NB: There is no value 'off' for the feature infl-neg in matrixdef.  But the code for figuring out which groups are relevant to which language types will do the right thing if we use a non-value for the feature here.",
                fv = ['infl-neg:']),

    NotFilter(name = "neg-adv",
                mrs_id_list = g.all_neg,
                re1 = '(\s|^)neg(\s|$)',
                comment = "If we haven't selected adverbial negation, we shouldn't see the adverbs. NB: There is no value 'off' for the feature infl-neg in matrixdef.  But the code for figuring out which groups are relevant to which language types will do the right thing if we use a non-value for the feature here.",
                fv = ['adv-neg:']),

    MatchFilter(name = "neg-affix-only-1",
              mrs_id_list = g.all_neg,
              re1 = '-neg|neg-',
              comment = "If we only selected inflectional negation, we should see one in every negated sentence",
              fv = ['and', 'infl-neg:on','adv-neg:']),

    MatchFilter(name = "neg-adv-only-2",
              mrs_id_list = g.all_neg,
              re1 = '(\s|^)neg(\s|$)',
              comment = "If we only selected adverbial negation, we should see one in every negated sentence.",
              fv = ['and', 'infl-neg:','adv-neg:on']),

    NotFilter(name = "neg-affix-only-1",
              mrs_id_list = g.all_neg,
              re1 = '(\s|^)neg(\s|$)',
              comment = "If we only selected inflectional negation, we shouldn't see the adverb",
              fv = ['and', 'infl-neg:on','adv-neg:']),

    NotFilter(name = "neg-adv-only-2",
              mrs_id_list = g.all_neg,
              re1 = '-neg|neg-',
              comment = "If we only selected adverbial negation, we shouldn't see the affix",
              fv = ['and', 'infl-neg:','adv-neg:on']),

    MatchAndFilter(name = "neg-both-req",
              mrs_id_list = g.all_neg,
              re1 = '-neg|neg-',
              re2 = '(\s|^)neg(\s|$)',
              comment = "If both are required for negation, we should see always see both affix & adv.",
              fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:both-obl']),

    NotCopresentFilter(name = "neg-comp-dist",
                       mrs_id_list = g.all_neg,
                       re1 = '-neg|neg-',
                       re2 = '(\s|^)neg(\s|$)',
                       comment = "If adverbial and inflectional negation are in complementary distribution, we should see never see both affix & adv.",
                       fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:comp']),

    MatchFilter(name = "neg-adv-obl",
              mrs_id_list = g.all_neg,
              re1 = '(\s|^)neg(\s|$)',
              comment = "If both adverb and affix are possible, but the adverb is obligatory, we should see the adverb in every negated sentence.",
              fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:adv-obl']),

    MatchFilter(name = "neg-infl-obl",
              mrs_id_list = g.all_neg,
              re1 = '-neg|neg-',
              comment = "If both adverb and affix are possible, but the affix is obligatory, we should see the affix in every negated sentence.",
              fv = ['and', 'infl-neg:on','adv-neg:on','multi-neg:infl-obl']),

    NotFilter(name = "neg-infl-aux",
           mrs_id_list = g.all_neg,
           re1 = '[^(aux)]-neg|neg-[^(aux)]',
           comment = "If the affix only attaches to aux, it shouldn't attach to anything else.",
           fv = ['and', 'infl-neg:on', 'neg-infl-type:aux']),
                 
    NotFilter(name = "neg-infl-main",
           mrs_id_list = g.all_neg,
           re1 = '[^(tv)(iv)]-neg|neg-[^(tv)(iv)]',
           comment = "If the affix only attaches to main verbs, it shouldn't attach to anything else.",
           fv = ['and', 'infl-neg:on', 'neg-infl-type:main']),

    AndNotFilter(name = "neg-adv-left",
           mrs_id_list = g.all_neg,
           re1 = '-neg|neg-',
           re2 = '(tv|iv).* neg',
           comment = "If there is an independent modifier adverb, it should show up on the correct side of the verb.  Not applicable to strings which also have the negative affix, since in this case the adverb has to be selected.",
           fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-order:before']),

    AndNotFilter(name = "neg-adv-right",
           mrs_id_list = g.all_neg,
           re1 = '-neg|neg-',
           re2 = 'neg.* (tv|iv)',
           comment = "If there is an independent modifier adverb, it should show up on the correct side of the verb.  Not applicable to strings which also have the negative affix, since in this case the adverb has to be selected.",
           fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-order:after']),

    AndNotFilter(name = "neg-adv-s",
           mrs_id_list = g.all_neg,
           re1 = '-neg|neg-',
           re2 = '(n[12]|tv|iv|aux).* neg.* (n[12]|tv|iv|aux)',
           comment = "If the negative adverb is an independent modifier of S, it should not appear between the verb and any arguments.  This filter will obviously not work for any multiclausal cases.  This only applies to sentences where there is no negative affix as well, since in those cases, the adverb has to be selected.",
           fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-mod:s']),

    AndMatchFilter(name = "neg-adv-v",
                   mrs_id_list = g.all_neg,
                   re1 = '-neg|neg-',
                   re2 = 'neg (tv|iv|aux)|(tv|iv|aux) neg',
                   comment = "If the negative adverb is an independent modifier of V, it should be adjacent to the verb. This only applies to sentences where there is no negative affix as well, since in those cases, the adverb has to be selected.  As we get a more refined theory of auxiliaries, we may wish to distinguish auxiliaries from main verbs here.",
                   fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-mod:v']),

    AndNotFilter(name = 'neg-adv-vp-1',
                 mrs_id_list = g.n1_subj_neg,
                 re1 = '-neg|neg-',
                 re2 = '(n2|tv).* neg.* (n2|tv)',
                 comment = "If the negative adverb is an independent modifier of VP, it cannot intervene between the object (n2) and the verb).",
                 fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-mod:vp']),

    AndNotFilter(name = 'neg-adv-vp-2',
                 mrs_id_list = g.n2_subj_neg,
                 re1 = '-neg|neg-',
                 re2 = '(n1|tv).* neg.* (n1|tv)',
                 comment = "If the negative adverb is an independent modifier of VP, it cannot intervene between the object (n1) and the verb).",
                 fv = ['and','adv-neg:on','neg-adv:ind-adv','neg-mod:vp']),

# Selected modifiers:
# If the choices file specifies the adverb as a selected complement of
# V/Aux, then it should show up in a plausible complement position: on
# the correct side of the verb, and either inside or outside the
# subject, as required.  If both the affix and the adverb are present,
# likewise, the adverb should show up in a plausible complement
# position.  In both cases, we have to track where the subject and
# object are.

    NotFilter(name = 'sel-adv-v-init-1',
              mrs_id_list = g.all_neg,
              re1 = 'neg .*tv',
              comment = "If the word order is v-initial and neg is a selected adverb, it has to appear after the verb.",
              fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:v-initial']),

    AndNotFilter(name = 'sel-adv-v-init-2',
                 mrs_id_list = g.all_neg,
                 re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                 re2 = 'neg .*tv',
                 comment = "If the word order is v-initial and neg adverb is a selected adverb by virtue of co-occurring with the affix, it has to appear after the verb.",
                 fv = ['and','adv-neg:on','infl-neg:on','word-order:v-initial']),

    NotFilter(name = 'sel-adv-v-final-1',
              mrs_id_list = g.all_neg,
              re1 = 'tv .*neg',
              comment = "If the word order is v-final and neg is a selected adverb, it has to appear before the verb.",
              fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:v-final']),

    AndNotFilter(name = 'sel-adv-v-init-2',
                 mrs_id_list = g.all_neg,
                 re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                 re2 = 'tv .*neg',
                 comment = "If the word order is v-final and neg adverb is a selected adverb by virtue of co-occurring with the affix, it has to appear before the verb.",
                 fv = ['and','adv-neg:on','infl-neg:on','word-order:v-final']),

    MatchFilter(name = 'sel-adv-vo-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'tv[( n2)( aux)]{,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg has to appear after the verb with at most an aux and the object (here, n2) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv',['or','word-order:svo','word-order:vos']]),

    MatchFilter(name = 'sel-adv-vo-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'tv[( n1)( aux)]{,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg has to appear after the verb with at most an aux and the object (here, n1) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv',['or','word-order:svo','word-order:vos']]),

    AndMatchFilter(name = 'sel-adv-vo-3',
                   mrs_id_list = g.n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'tv(-neg){,1}[( n2)( aux)]{,2} neg',
                   comment = "If the word order is SVO or VOS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the verb with at most an aux and the object (here, n2) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:svo','word-order:vos']]),

    AndMatchFilter(name = 'sel-adv-vo-4',
                   mrs_id_list = g.n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'tv(-neg){,1}[( n1)( aux)]{,2} neg',
                   comment = "If the word order is SVO or VOS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the verb with at most an aux and the object (here, n1) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:svo','word-order:vos']]),

    MatchFilter(name = 'sel-adv-ov-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'neg[( n2)( aux)]{,2} (neg-){,1}tv',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg has to appear before the verb with at most an aux and the object (here, n2) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv',['or','word-order:sov','word-order:ovs']]),

    MatchFilter(name = 'sel-adv-ov-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'neg[( n1)( aux)]{,2} (neg-){,1}tv',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg has to appear before the verb with at most an aux and the object (here, n1) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv',['or','word-order:sov','word-order:ovs']]),

    AndMatchFilter(name = 'sel-adv-ov-3',
                   mrs_id_list = g.n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg[( n2)( aux)]{,2} (neg-){,1}tv',
                   comment = "If the word order is SOV or OVS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most an aux and the object (here, n2) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:sov','word-order:ovs']]),

    AndMatchFilter(name = 'sel-adv-ov-4',
                   mrs_id_list = g.n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg[( n1)( aux)]{,2} (neg-){,1}tv',
                   comment = "If the word order is SOV or OVS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most an aux and the object (here, n1) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on',['or','word-order:sov','word-order:ovs']]),

    MatchFilter(name = 'sel-adv-vso-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'n1( n2){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear after the subject with at most the object (here, n2) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:vso']),

    MatchFilter(name = 'sel-adv-vso-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'n2( n1){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear after the subject with at most the object (here, n1) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:vso']),

    AndMatchFilter(name = 'sel-adv-vso-3',
                   mrs_id_list = g.n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'n1( n2){,1} neg',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the subject with at most the object (here, n2) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:vso']),

    AndMatchFilter(name = 'sel-adv-vso-4',
                   mrs_id_list = g.n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'n2( n1){,1} neg',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most the object (here, n1) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:vso']),

    MatchFilter(name = 'sel-adv-osv-1',
                mrs_id_list = g.n1_subj_neg,
                re1 = 'neg( n2){,1} n1',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear before the subject with at most the object (here, n2) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:osv']),

    MatchFilter(name = 'sel-adv-osv-2',
                mrs_id_list = g.n2_subj_neg,
                re1 = 'neg( n1){,1} n2',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear before the subject with at most the object (here, n1) intervening.",
                fv = ['and','adv-neg:on','neg-adv:sel-adv','word-order:osv']),

    AndMatchFilter(name = 'sel-adv-osv-3',
                   mrs_id_list = g.n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg( n2){,1} n1',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the subject with at most the object (here, n2) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:osv']),

    AndMatchFilter(name = 'sel-adv-osv-4',
                   mrs_id_list = g.n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg( n1){,1} n2',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the subject with at most the object (here, n1) intervening.",
                   fv = ['and','adv-neg:on','infl-neg:on','word-order:osv']),

######################################################################
# Filters for yes-no questions

    AlwaysFilter(name = 'no-overt-q',
                 mrs_id_list = g.all_ques,
                 comment = "If a language has no over mark of yes-no questions, reject all strings associated with the ques[1-4] mrs_ids.",
                 fv = ['and','q-part:','q-infl:','q-inv:']),

    MatchFilter(name = 'always-qpart',
                mrs_id_list = g.all_ques,
                re1 = 'qpart',
                comment = "Currently, we don't allow question particles to co-exist with other overt question marking, so if this strategy is selected, all question mrs_ids must be expressed with a qpart in the sentence.",
                fv = ['q-part:on']),

    MatchFilter(name = 'qpart-init',
                mrs_id_list = g.all_ques,
                re1 = '^qpart',
                comment = "If the language type definition says that qpart is sentence-initial, make sure it's at the beginning of the string.",
                fv = ['q-part-order:before']),

    MatchFilter(name = 'qpart-final',
                mrs_id_list = g.all_ques,
                re1 = 'qpart$',
                comment = "If the language type definition says that qpart is sentence-final, make sure it's at the end of the string.",
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
                re1 = 'tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "svo-vos-inv-2",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "svo-vos-inv-3",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* tv.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux','aux-order:before']),

    MatchFilter(name = "svo-vos-inv-4",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* tv.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux','aux-order:before']),

    MatchFilter(name = "svo-vos-inv-5",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'tv.* n2.* n1.* aux',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "svo-vos-inv-6",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv.* n1.* n2.* aux',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "svo-vos-inv-7",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* tv.* n2|tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux','aux-order:before']),

    MatchFilter(name = "svo-vos-inv-8",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* tv.* n1|tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux','aux-order:before']),

    MatchFilter(name = "svo-vos-inv-9",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'tv.* n2.* n1.* aux|tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux','aux-order:after']),

    MatchFilter(name = "svo-vos-inv-10",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'tv.* n1.* n2.* aux|tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:svo','word-order:vos'],'q-inv:on','q-inv-verb:main-aux','aux-order:after']),


    # O V S -> O S V
    # S O V -> O S V
    # O V S -> O V S Aux
    # S O V -> O V S Aux
    # O V S -> Aux S O V
    # S O V -> Aux S O V

    MatchFilter(name = "ovs-sov-inv-1",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2.* n1.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "ovs-sov-inv-2",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1.* n2.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main']),

    MatchFilter(name = "ovs-sov-inv-3",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* n2.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-4",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* n1.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-5",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2.* tv.* n1.* aux',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-6",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1.* tv.* n2.* aux',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:aux','aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-7",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'aux.* n1.* n2.* tv|n2.* n1.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux','aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-8",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'aux.* n2.* n1.* tv|n1.* n2.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux','aux-order:before']),

    MatchFilter(name = "ovs-sov-inv-9",
                mrs_id_list = g.n1_subj_ques,
                re1 = 'n2.* tv.* n1.* aux|n2.* n1.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux','aux-order:after']),

    MatchFilter(name = "ovs-sov-inv-10",
                mrs_id_list = g.n2_subj_ques,
                re1 = 'n1.* tv.* n2.* aux|n1.* n2.* tv',
                comment = "This filter checks for the right word order in questions when the word order is OVS or SOV, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','word-order:ovs','word-order:sov'],'q-inv:on','q-inv-verb:main-aux','aux-order:after']),

  
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

    AndMatchFilter(name = "non-finite-verb-1",
                   mrs_id_list = g.all,
                   re1 = 'aux.* iv|iv .*aux',
                   re2 = 'iv-nf',
                   comment = "If auxiliaries take complements in non-finite form, check whether the other verb is non-finite, if an auxiliary is present.  Note that this assumes at most one non-aux verb, and will need to be updated. Because the non-finite forms are handled separately for each verb, need to do filters separately for iv and tv.",
                   fv = ['iverb-non-finite:iv-nf']),
    AndMatchFilter(name = "non-finite-verb-2",
                   mrs_id_list = g.all,
                   re1 = 'aux.* tv|tv .*aux',
                   re2 = 'tv-nf',
                   comment = "If auxiliaries take complements in non-finite form, check whether the other verb is non-finite, if an auxiliary is present.  Note that this assumes at most one non-aux verb, and will need to be updated. Because the non-finite forms are handled separately for each verb, need to do filters separately for iv and tv.",
                   fv = ['tverb-non-finite:tv-nf']),
    NotFilter(name = "non-finite-verb-3",
              mrs_id_list = g.all,
              re1 = 'iv-nf',
              comment = "If the intransitive verb doesn't take a special form after the auxiliary, then we shouldn't see iv-nf.",
              fv = ['iverb-non-finite:iv']),
    NotFilter(name = "non-finite-verb-4",
              mrs_id_list = g.all,
              re1 = 'tv-nf',
              comment = "If the transitive verb doesn't take a special form after the auxiliary, then we shouldn't see tv-nf.",
              fv = ['tverb-non-finite:tv']),
    

    # Obligatory specifiers.  Since overt specifiers change the MRS, consider keying this off the mrs_id instead of the string.

    AndMatchFilter(name = "n1-obl-spr",
                   mrs_id_list = g.all,
                   re1 = 'n1',
                   re2 = 'n1 det|det n1',
                   comment = "If n1 is defined as having an obligatory specifier, there should be a det next to it.  The word order filters will ensure that the det is in the right place.  Since n-det order is always consistent within the current system, we can't have a string like /n1 det n2/ going through with the det counting for both of the nouns.",
                   fv = ['noun1_det:obl']),
    AndMatchFilter(name = "n2-obl-spr",
                   mrs_id_list = g.all,
                   re1 = 'n2',
                   re2 = 'n2 det|det n2',
                   comment = "If n2 is defined as having an obligatory specifier, there should be a det next to it.  The word order filters will ensure that the det is in the right place.  Since n-det order is always consistent within the current system, we can't have a string like /n1 det n2/ going through with the det counting for both of the nouns.",
                   fv = ['noun2_det:obl']),
    
    # Impossible specifiers.  Since overt specifiers change the MRS, keying off the mrs_id instead of the string.  That is,
    # the regexes are dummies here.
    # ERB 2007-06-05 Recasting as AlwaysFilter


    AlwaysFilter(name = "n1-no-spr",
              mrs_id_list = g.n1_with_det,
              comment = "If n1 is defined as having no specifier possible, then no string with an mrs_id corresponding to an overt det on n1 should be grammatical.",
              fv = ['noun1_det:imp']),
    AlwaysFilter(name = "n2-no-spr",
              mrs_id_list = g.n2_with_det,
              comment = "If n2 is defined as having no specifier possible, then no string with an mrs_id corresponding to an overt det on n2 should be grammatical.",
              fv = ['noun2_det:imp']),
    
    # Check whether subject is NP or PP, as appropriate.  Allow for auxiliaries to control choice of subject form.

    AndMatchFilter(name = "tv-np-subj-no-aux",
                   mrs_id_list = g.trans,
                   re1 = 'p-nom',
                   re2 = 'aux',
                   comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the string at all.  In order to handle the cases with and without axuiliaries separately, this one only allows strings with p-nom through if they also have an auxiliary.  Only applies to mrs_ids corresponding to tv.",
                   fv = ['tverb-subj:np']),

    AndMatchFilter(name = "iv-np-subj-no-aux",
                   mrs_id_list = g.intrans,
                   re1 = 'p-nom',
                   re2 = 'aux',
                   comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the string at all.  In order to handle the cases with and without axuiliaries separately, this one only allows strings with p-nom through if they also have an auxiliary.  Only applies to mrs_ids corresponding to iv.",
                   fv = ['iverb-subj:np']),

    AndNotFilter(name = "tv-np-subj-scomp-aux",
                 mrs_id_list = g.trans,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the string at all.  In order to handle the cases with and without axuiliaries separately, this one only looks at strings with an auxiliary and only in language types with s-comp auxes..  Only applies to mrs_ids corresponding to tv.",
                 fv = ['and','tverb-subj:np','aux-comp:s']),

    AndNotFilter(name = "iv-np-subj-scomp-aux",
                 mrs_id_list = g.intrans,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If the subject is supposed to be an NP, we shouldn't see p-nom in the string at all.  In order to handle the cases with and without axuiliaries separately, this one only looks at strings with an auxiliary and only in language types with s-comp auxes..  Only applies to mrs_ids corresponding to iv.",
                 fv = ['and','iverb-subj:np','aux-comp:s']),

    AndNotFilter(name = "v*comp-aux-np-subj",
                 mrs_id_list = g.all,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If there is a V or VP-complement auxiliary present and that auxiliary says subj is NP, we shouldn't see p-nom at all.",
                 fv = ['and',['or','aux-comp:v','aux-comp:vp'],'aux-subj:np']),

    NegTrigMatchFilter(name = "tv-pp-subj-no-aux",
                   mrs_id_list = g.trans,
                   re1 = 'aux', #only if there's no aux
                   re2 = 'p-nom', #require p-nom
                   comment = "If the subject is supposed to be a PP, we should see p-nom.  In order to handle the cases with and without axuiliaries separately, this one only checks for p-nom if it doesn't see an auxiliary.  Only applies to mrs_ids corresponding to tv.",
                   fv = ['tverb-subj:adp']),

    NegTrigMatchFilter(name = "iv-pp-subj-no-aux",
                   mrs_id_list = g.intrans,
                   re1 = 'aux', #only if there's no aux
                   re2 = 'p-nom', #require p-nom
                   comment = "If the subject is supposed to be a PP, we should see p-nom.  In order to handle the cases with and without axuiliaries separately, this one only checks for p-nom if it doesn't see an auxiliary.  Only applies to mrs_ids corresponding to iv.",
                   fv = ['iverb-subj:adp']),

    AndMatchFilter(name = "tv-pp-subj-scomp-aux",
                 mrs_id_list = g.trans,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If the subject is supposed to be a PP, we should see p-nom.  In order to handle the cases with and without axuiliaries separately, this one only looks at strings with an auxiliary and only in language types with s-comp auxes.  Only applies to mrs_ids corresponding to tv.",
                 fv = ['and','tverb-subj:adp','aux-comp:s']),

    AndMatchFilter(name = "iv-pp-subj-scomp-aux",
                 mrs_id_list = g.intrans,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If the subject is supposed to be a PP, we should see p-nom.  In order to handle the cases with and without axuiliaries separately, this one only looks at strings with an auxiliary and only in language types with s-comp auxes.  Only applies to mrs_ids corresponding to iv.",
                 fv = ['and','iverb-subj:adp','aux-comp:s']),

    AndMatchFilter(name = "v*comp-aux-pp-subj",
                 mrs_id_list = g.all,
                 re1 = 'aux',
                 re2 = 'p-nom',
                 comment = "If there is a V or VP-complement auxiliary present and that auxiliary says subj is PP, p-nom should be present.",
                 fv = ['and',['or','aux-comp:v','aux-comp:vp'],'aux-subj:adp']),


    # Check for NP v. PP on object of tverb.

    NotFilter(name = 'tv-np-comp',
              mrs_id_list = g.trans,
              re1 = 'p-acc',
              comment = "If tv is defined to take an NP complement, we shouldn't see p-acc at all.",
              fv = ['tverb-obj:np']),

    MatchFilter(name = 'tv-pp-comp',
                mrs_id_list = g.trans,
                re1 = 'p-acc',
                comment = "If tv is defined to take a PP complement, the string should contain p-acc.",
                fv = ['tverb-obj:adp']),

    # Check for order of adpositions with respect to object and subject.

    AndMatchFilter(name = 'pnom-prep-1',
                   mrs_id_list = g.n1_subj,
                   re1 = 'p-nom',
                   re2 = 'p-nom.* n1',
                   comment = "If p-nom is a preposition, then when it is present and n1 is the subject, p-nom should precede n1.",
                   fv = ['subj-adp-order:before']),

    AndMatchFilter(name = 'pnom-prep-2',
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'p-nom',
                   re2 = 'p-nom.* n2',
                   comment = "If p-nom is a preposition, then when it is present and n2 is the subject, p-nom should precede n2.",
                   fv = ['subj-adp-order:before']),

    AndMatchFilter(name = 'pnom-post-1',
                   mrs_id_list = g.n1_subj,
                   re1 = 'p-nom',
                   re2 = 'n1.* p-nom',
                   comment = "If p-nom is a postposition, then when it is present and n1 is the subject, p-nom should follow n1.",
                   fv = ['subj-adp-order:after']),

    AndMatchFilter(name = 'pnom-post-2',
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'p-nom',
                   re2 = 'n2.* p-nom',
                   comment = "If p-nom is a postposition, then when it is present and n2 is the subject, p-nom should follow n2.",
                   fv = ['subj-adp-order:after']),

##
    AndMatchFilter(name = 'pacc-prep-1',
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'p-acc',
                   re2 = 'p-acc.* n1',
                   comment = "If p-acc is a preposition, then when it is present and n1 is the object, p-acc should precede n1.",
                   fv = ['obj-adp-order:before']),

    AndMatchFilter(name = 'pacc-prep-2',
                   mrs_id_list = g.n1_subj_n2_obj,
                   re1 = 'p-acc',
                   re2 = 'p-acc.* n2',
                   comment = "If p-acc is a preposition, then when it is present and n2 is the object, p-acc should precede n2.",
                   fv = ['obj-adp-order:before']),

    AndMatchFilter(name = 'pacc-post-1',
                   mrs_id_list = g.n2_subj_n1_obj,
                   re1 = 'p-acc',
                   re2 = 'n1.* p-acc',
                   comment = "If p-acc is a postposition, then when it is present and n1 is the object, p-acc should follow n1.",
                   fv = ['obj-adp-order:after']),

    AndMatchFilter(name = 'pacc-post-2',
                   mrs_id_list = g.n1_subj_n2_obj,
                   re1 = 'p-acc',
                   re2 = 'n2.* p-acc',
                   comment = "If p-acc is a postposition, then when it is present and n2 is the object, p-acc should follow n2.",
                   fv = ['obj-adp-order:after']),


    # Placement of auxiliaries with respect to the rest of the sentence.

    # 1. No auxiliaries.

    NotFilter(name = 'no-aux',
              mrs_id_list = g.all,
              re1 = 'aux',
              comment = "If the language doesn't have an auxiliary, we should never see aux in the string.",
              fv = ['aux-verb:']),
              

    # 2. V-comp auxiliaries.

    AndMatchFilter(name = 'vcomp-aux-auxleft',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   re2 = 'aux [ti]v',
                   comment = "If the auxiliary takes a V complement and appears before the verb, it should be to the left of the V, in any wo except free.",
                   fv = ['and','aux-comp:v','aux-order:before',['or','word-order:sov','word-order:svo','word-order:osv','word-order:vos','word-order:ovs','word-order:vso','word-order:v-final','word-order:v-initial']]),

    AndMatchFilter(name = 'vcomp-aux-auxright',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   re2 = '[ti]v aux|[ti]v-nf aux',
                   comment = "If the auxiliary takes a V complement and appears after the verb, it should be to the right of the V, in any wo except free.  Allow for -nf forms of the verb.",
                   fv = ['and','aux-comp:v','aux-order:after',['or','word-order:sov','word-order:svo','word-order:osv','word-order:vos','word-order:ovs','word-order:vso','word-order:v-final','word-order:v-initial']]),

    AndMatchFilter(name = 'vcomp-aux-either-order',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   re2 = '[ti]v aux|[ti]v-nf aux|aux [ti]v',
                   comment = "If the auxiliary takes a V complement and can appear on either side of the V, it should be adjacent to the V, in any wo except free.  Allow for -nf forms of the verb.",
                   fv = ['and','aux-comp:v','aux-order:after',['or','word-order:sov','word-order:svo','word-order:osv','word-order:vos','word-order:ovs','word-order:vso','word-order:v-final','word-order:v-initial']]),

    # 3. VP comp auxiliaries

    AndNotFilter(name = 'vpcomp-n1-subj',
                 mrs_id_list = g.n1_subj_n2_obj_not_ques,
                 re1 = 'aux',
                 re2 = 'aux.* n1.* tv|tv.* n1.* aux',
                 comment = "If the auxiliary takes a VP complement, and n1 is the subject, then n1 shouldn't appear between the auxiliary and the verb in non-questions. Exception is free word order.",
                 fv = ['and','aux-comp:vp',['or','word-order:sov','word-order:svo','word-order:osv','word-order:vos','word-order:ovs','word-order:vso','word-order:v-final','word-order:v-initial']]),


    AndNotFilter(name = 'vpcomp-n2-subj',
                 mrs_id_list = g.n2_subj_n1_obj_not_ques,
                 re1 = 'aux',
                 re2 = 'aux.* n2.* tv|tv.* n2.* aux',
                 comment = "If the auxiliary takes a VP complement, and n2 is the subject, then n2 shouldn't appear between the auxiliary and the verb in non-questions. Exception is free word order.",
                 fv = ['and','aux-comp:vp',['or','word-order:sov','word-order:svo','word-order:osv','word-order:vos','word-order:ovs','word-order:vso','word-order:v-final','word-order:v-initial']]),


    AndMatchFilter(name = 'vpcomp-aux-free-1',
           mrs_id_list = g.n1_subj_n2_obj,
           re1 = 'aux',
           re2 = 'tv(-nf) (neg )(p-acc )(det )n2|n2 (det )(p-acc )(neg )tv',
           comment = "If the languge has free word order but VP-comp auxiliaries, then the only things that can intervene between tv and its object (for now) are det, p-acc, and neg.  Here object = n2.",
           fv = ['and','aux-comp:vp','word-order:free']),

    AndMatchFilter(name = 'vpcomp-aux-free-2',
           mrs_id_list = g.n2_subj_n1_obj,
           re1 = 'aux',
           re2 = 'tv(-nf) (neg )(p-acc )(det )n1|n1 (det )(p-acc )(neg )tv',
           comment = "If the languge has free word order but VP-comp auxiliaries, then the only things that can intervene between tv and its object (for now) are det, p-acc, and neg.  Here object = n1.",
           fv = ['and','aux-comp:vp','word-order:free']),


    # 4. S comp

    AndNotFilter(name = 's-comp-aux',
                 mrs_id_list = g.all,
                 re1 = 'aux',
                 re2 = 'n[12].* aux.* [ti]v|[ti]v.* aux.* n[12]',
                 comment = "If the auxiliary takes s complements, it shouldn't intervene between the verb and either noun, in any word order.",
                 fv = ['aux-comp:s']),

    # Order for VP and S cases.

    AndMatchFilter(name = 's-or-vp-comp-aux-left',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   re2 = 'aux.* [ti]v',
                   comment = "Because the general VP/S comp filters doesn't check order, need a set of second filters to check aux-v order in the VP/S comp case.  This one checks for aux before v.",
                   fv = ['and',['or','aux-comp:vp','aux-comp:s'],'aux-order:before']),


    AndMatchFilter(name = 's-or-vp-comp-aux-right',
                   mrs_id_list = g.all,
                   re1 = 'aux',
                   re2 = '[ti]v.* aux',
                   comment = "Because the general VP/S comp filters doesn't check order, need a set of second filters to check aux-v order in the VP/S comp case.  This one checks for aux after v.",
                   fv = ['and',['or','aux-comp:s','aux-comp:vp'],'aux-order:after']),


]

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

#################################################################
# Initial set of language-type specific word order filters.

from filters import NotFilter
from filters import AndNotFilter
from filters import MatchFilter
from filters import NotCopresentFilter
from filters import AlwaysFilter
from filters import MatchAndFilter
from filters import AndMatchFilter

#################################################################
# Language-type specific filters for word order.  Note that these
# apply to all sentences, not just the ones with 'wo' mrs_tags,
# because all sentences have word order!
#
# Each filter provides a list of feature-value pairs from language
# type definitions that it cares about.
#
# When the constraint is an 'or' (e.g., 'wordorder' is ''sov'' or ''osv'')
# we put both 'wordorder':'sov' and 'wordorder':'osv' on the list.  The
# query which generates the gold standard test suites looks for all
# filters which match the particulare feature-value pairs for the language
# type in question.

# Variables storing sets of MRSs ids.  We can add to these and update multiple
# filters at once as we add harvester strings.

# NOTE: When adding new mrs_ids to these lists, one should always examine
# the filters that call the lists being modified.  In some cases, we may need
# to create new lists in order to maintain the right behavior.

n1_subj = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2']
n1_subj_n2_obj = ['wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2']
n2_subj_n1_obj = ['wo7','wo8','wo9','wo10','neg3','neg4','ques3','ques4']
n1_subj_not_ques = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2']
n2_subj_n1_obj_not_ques = ['wo7','wo8','wo9','wo10','neg3','neg4'],
with_dets = ['wo2','wo4','wo5','wo6','wo8','wo9','wo10','neg2','neg4','ques2','ques4']
one_det_on_n1 = ['wo2','wo5','wo10'],
one_det_on_n2 = ['wo6','wo9'],
two_dets_n1_n2 =['wo4','wo8','neg2','neg4','ques2','ques4']
all_neg = ['neg1','neg2','neg3','neg4']
n1_subj_neg = ['neg1','neg2']
n2_subj_neg = ['neg1','neg2']
all_ques = ['ques1','ques2','ques3','ques4']
n1_subj_ques = ['ques1','ques2']
n2_subj_ques = ['ques3','ques4']
             
filter_list = [

####################################################################
# Word order filters    

    MatchFilter(name = "s-o-order1",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'n1.*n2',
              comment = "If the word order for the language type has subjects before objects, and n1 is the subject and n2 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:svo','wordorder:sov','wordorder:vso']),

    MatchFilter(name = "s-o-order2",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has subjects before objects, and n2 is the subject and n1 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:svo','wordorder:sov','wordorder:vso']),

    MatchFilter(name = "s-o-order3",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has objects before subjects, and n1 is the subject and n2 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
           fv = ['or', 'wordorder:ovs','wordorder:osv','wordorder:vos']),

    MatchFilter(name = "s-o-order4",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'n1.*n2',
              comment = "If the word order for the language type has objects before subjects, and n2 is the subject and n1 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:ovs','wordorder:osv','wordorder:vos']),

    MatchFilter(name = "s-v-order1",
              mrs_id_list = n1_subj_not_ques,
              re1 = '(tv|iv).*n1',
              comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present.",
              fv = ['or', 'wordorder:sov','wordorder:svo','wordorder:osv','wordorder:v-final']),

    AndNotFilter(name = "s-v-order2",
                 mrs_id_list = ['ques1','ques2'],
                 re1 = 'qpart',
                 re2 = '(tv|iv).*n1',
                 comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'wordorder:sov','wordorder:svo','wordorder:osv','wordorder:v-final']),

    MatchFilter(name = "s-v-order3",
              mrs_id_list = n1_subj_not_ques,
              re1 = 'n1.*(tv|iv)',
              comment = "If the word order for the language has verbs before subjects, and n1 is the subject, check that n1 precedes the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present.",
              fv = ['or', 'wordorder:vos','wordorder:ovs','wordorder:vso','wordorder:v-initial']),

    AndNotFilter(name = "s-v-order4",
                 mrs_id_list = ['ques1','ques2'],
                 re1 = 'qpart',
                 re2 = 'n1.*(tv|iv)',
                 comment = "If the word order for the language has subjects before verbs, and n1 is the subject, check that n1 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'wordorder:vos','wordorder:ovs','wordorder:vso','wordorder:v-initial']),

    NotFilter(name = "s-v-order5",
              mrs_id_list = n2_subj_n1_obj_not_ques,
              re1 = '(tv|iv).*n2',
              comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  This doesn't apply to ques3 and ques4.  For those we need to check if qpart (or, eventually, question inflection) is present.",
              fv = ['or', 'wordorder:sov','wordorder:svo','wordorder:osv','wordorder:v-final']),

    AndNotFilter(name = "s-v-order6",
                 mrs_id_list = ['ques3','ques4'],
                 re1 = 'qpart',
                 re2 = '(tv|iv).*n2',
                 comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'wordorder:sov','wordorder:svo','wordorder:osv','wordorder:v-final']),

    NotFilter(name = "s-v-order7",
              mrs_id_list = n2_subj_n1_obj_not_ques,
              re1 = 'n2.*(tv|iv)',
              comment = "If the word order for the language has verbs before subjects, and n2 is the subject, check that n2 precedes the verb.  This doesn't apply to ques1 and ques2.  For those we need to check if qpart (or, eventually, question inflection) is present.",
              fv = ['or', 'wordorder:vos','wordorder:ovs','wordorder:vso','wordorder:v-initial']),

    AndNotFilter(name = "s-v-order8",
                 mrs_id_list = ['ques3','ques4'],
                 re1 = 'qpart',
                 re2 = 'n2.*(tv|iv)',
                 comment = "If the word order for the language has subjects before verbs, and n2 is the subject, check that n2 precedes the verb.  Special case for ques1 and ques2 where we want to check that we're not in the inversion case, which needs to be worried about separately.",
                 fv = ['or', 'wordorder:vos','wordorder:ovs','wordorder:vso','wordorder:v-initial']),

    NotFilter(name = "o-v-order1",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'tv.*n2',
              comment = "If the word order for the language has objects before verbs, and n2 is the object, check that it precedes the verb.",
              fv = ['or', 'wordorder:sov','wordorder:osv','wordorder:ovs','wordorder:v-final']),
             
    NotFilter(name = "o-v-order2",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'n2.*tv',
              comment = "If the word order for the language has objects after verbs, and n2 is the object, check that it follows the verb.",
              fv = ['or', 'wordorder:vos','wordorder:vso','wordorder:svo','wordorder:v-initial']),

    NotFilter(name = "o-v-order3",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'tv.*n1',
              comment = "If the word order for the language has objects before verbs, and n1 is the object, check that it precedes the verb.",
              fv = ['or', 'wordorder:sov','wordorder:osv','wordorder:ovs','wordorder:v-final']),
             
    NotFilter(name = "o-v-order4",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'n1.*tv',
              comment = "If the word order for the language has objects after verbs, and n1 is the object, check that it follows the verb.",
              fv = ['or', 'wordorder:vos','wordorder:vso','wordorder:svo','wordorder:v-initial']),


    AlwaysFilter(name = "no-dets",
                 mrs_id_list = with_dets,
                 comment = "If the language has no determiners, reject all strings with determiners.",
                 fv = ['hasDets:nil']),

    MatchFilter(name = "det-n-order1",
              mrs_id_list = one_det_on_n1,
              re1 = 'det n1',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n1, the det should immediately precede n1.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:SpecHead']),

    MatchFilter(name = "det-n-order2",
              mrs_id_list = one_det_on_n1,
              re1 = 'n1 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n1, the det should immediately follow n1.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:HeadSpec']),

    MatchFilter(name = "det-n-order3",
              mrs_id_list = one_det_on_n2,
              re1 = 'det n2',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n2, the det should immediately precede n2.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:SpecHead']),

    MatchFilter(name = "det-n-order4",
              mrs_id_list = one_det_on_n2,
              re1 = 'n2 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n2, the det should immediately follow n2.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:HeadSpec']),

    NotFilter(name = "det-n-order5",
                mrs_id_list = two_dets_n1_n2,
                re1 = '(det .*det (n1|n2))|(det (n1|n2) .*det (n1|n2))|(det (n1|n2) .*(n1|n2) det)',
                comment = "If the language has determiners following nouns, and n1 and n2 both have determiners attached, neither should be preceded by a determiner.  This is perhaps more baroque than it needs to be?",
                fv = ['NounDetOrder:HeadSpec']),

    NotFilter(name = "det-n-order6",
                mrs_id_list = two_dets_n1_n2,
                re1 = '((n1|n2) det .*det)|((n1|n2) det .*(n1|n2) det)|(det (n1|n2) .*(n1|n2) det)',
                comment = "If the language has determiners precedings nouns, and n1 and n2 both have determiners attached, neither should be followed by a determiner.  This is perhaps more baroque than it needs to be?",
                fv = ['NounDetOrder:SpecHead']),

####################################################################
# Negation filters

    NotFilter(name = "neg-infl",
                mrs_id_list = all_neg,
                re1 = '-neg|neg-',
                comment = "If we haven't selected inflectional negation, we shouldn't see the affix. NB: There is no value 'off' for the feature infl_neg in matrixdef.  But the code for figuring out which groups are relevant to which language types will do the right thing if we use a non-value for the feature here.",
                fv = ['infl_neg:off']),

    NotFilter(name = "neg-adv",
                mrs_id_list = all_neg,
                re1 = '(\s|^)neg(\s|$)',
                comment = "If we haven't selected adverbial negation, we shouldn't see the adverbs. NB: There is no value 'off' for the feature infl_neg in matrixdef.  But the code for figuring out which groups are relevant to which language types will do the right thing if we use a non-value for the feature here.",
                fv = ['adv_neg:off']),

    MatchFilter(name = "neg-affix-only",
              mrs_id_list = all_neg,
              re1 = '-neg|neg-',
              comment = "If we only selected inflectional negation, we should see one in every negated sentence",
              fv = ['and', 'infl_neg:on','adv_neg:off']),

    MatchFilter(name = "neg-adv-only",
              mrs_id_list = all_neg,
              re1 = '(\s|^)neg(\s|$)',
              comment = "If we only selected adverbial negation, we should see one in every negated sentence.",
              fv = ['and', 'infl_neg:off','adv_neg:on']),

    MatchAndFilter(name = "neg-both-req",
              mrs_id_list = all_neg,
              re1 = '-neg|neg-',
              re2 = '(\s|^)neg(\s|$)',
              comment = "If both are required for negation, we should see always see both affix & adv.",
              fv = ['and', 'infl_neg:on','adv_neg:on','multineg:bothobl']),

    NotCopresentFilter(name = "neg-comp-dist",
                       mrs_id_list = all_neg,
                       re1 = '-neg|neg-',
                       re2 = '(\s|^)neg(\s|$)',
                       comment = "If adverbial and inflectional negation are in complementary distribution, we should see never see both affix & adv.",
                       fv = ['and', 'infl_neg:on','adv_neg:on','multineg:comp']),

    MatchFilter(name = "neg-adv-obl",
              mrs_id_list = all_neg,
              re1 = '(\s|^)neg(\s|$)',
              comment = "If both adverb and affix are possible, but the adverb is obligatory, we should see the adverb in every negated sentence.",
              fv = ['and', 'infl_neg:on','adv_neg:on','multineg:advobl']),

    MatchFilter(name = "neg-infl-obl",
              mrs_id_list = all_neg,
              re1 = '-neg|neg-',
              comment = "If both adverb and affix are possible, but the affix is obligatory, we should see the affix in every negated sentence.",
              fv = ['and', 'infl_neg:on','adv_neg:on','multineg:inflobl']),

    NotFilter(name = "neg-infl-aux",
           mrs_id_list = all_neg,
           re1 = '[^(aux)]-neg|neg-[^(aux)]',
           comment = "If the affix only attaches to aux, it shouldn't attach to anything else.",
           fv = ['and', 'infl_neg:on', 'neg-infl-type:aux']),
                 
    NotFilter(name = "neg-infl-main",
           mrs_id_list = all_neg,
           re1 = '[^(tv)(iv)]-neg|neg-[^(tv)(iv)]',
           comment = "If the affix only attaches to main verbs, it shouldn't attach to anything else.",
           fv = ['and', 'infl_neg:on', 'neg-infl-type:main']),

    AndNotFilter(name = "neg-adv-left",
           mrs_id_list = all_neg,
           re1 = '-neg|neg-',
           re2 = '(tv|iv).* neg',
           comment = "If there is an independent modifier adverb, it should show up on the correct side of the verb.  Not applicable to strings which also have the negative affix, since in this case the adverb has to be selected.",
           fv = ['and','adv_neg:on','neg-adv:ind-adv','negprepostmod:left']),

    AndNotFilter(name = "neg-adv-right",
           mrs_id_list = all_neg,
           re1 = '-neg|neg-',
           re2 = 'neg.* (tv|iv)',
           comment = "If there is an independent modifier adverb, it should show up on the correct side of the verb.  Not applicable to strings which also have the negative affix, since in this case the adverb has to be selected.",
           fv = ['and','adv_neg:on','neg-adv:ind-adv','negprepostmod:right']),

    AndNotFilter(name = "neg-adv-s",
           mrs_id_list = all_neg,
           re1 = '-neg|neg-',
           re2 = '(n[12]|tv|iv|aux).* neg.* (n[12]|tv|iv|aux)',
           comment = "If the negative adverb is an independent modifier of S, it should not appear between the verb and any arguments.  This filter will obviously not work for any multiclausal cases.  This only applies to sentences where there is no negative affix as well, since in those cases, the adverb has to be selected.",
           fv = ['and','adv_neg:on','neg-adv:ind-adv','negmod:s']),

    AndMatchFilter(name = "neg-adv-v",
                   mrs_id_list = all_neg,
                   re1 = '-neg|neg-',
                   re2 = 'neg (tv|iv|aux)|(tv|iv|aux) neg',
                   comment = "If the negative adverb is an independent modifier of V, it should be adjacent to the verb. This only applies to sentences where there is no negative affix as well, since in those cases, the adverb has to be selected.  As we get a more refined theory of auxiliaries, we may wish to distinguish auxiliaries from main verbs here.",
                   fv = ['and','adv_neg:on','neg-adv:ind-adv','negmod:v']),

    AndNotFilter(name = 'neg-adv-vp-1',
                 mrs_id_list = n1_subj_neg,
                 re1 = '-neg|neg-',
                 re2 = '(n2|tv).* neg.* (n2|tv)',
                 comment = "If the negative adverb is an independent modifier of VP, it cannot intervene between the object (n2) and the verb).",
                 fv = ['and','adv_neg:on','neg-adv:ind-adv','negmod:vp']),

    AndNotFilter(name = 'neg-adv-vp-2',
                 mrs_id_list = n2_subj_neg,
                 re1 = '-neg|neg-',
                 re2 = '(n1|tv).* neg.* (n1|tv)',
                 comment = "If the negative adverb is an independent modifier of VP, it cannot intervene between the object (n1) and the verb).",
                 fv = ['and','adv_neg:on','neg-adv:ind-adv','negmod:vp']),

# Selected modifiers:
# If the choices file specifies the adverb as a selected complement of
# V/Aux, then it should show up in a plausible complement position: on
# the correct side of the verb, and either inside or outside the
# subject, as required.  If both the affix and the adverb are present,
# likewise, the adverb should show up in a plausible complement
# position.  In both cases, we have to track where the subject and
# object are.

    NotFilter(name = 'sel-adv-v-init-1',
              mrs_id_list = all_neg,
              re1 = 'neg .*tv',
              comment = "If the word order is v-initial and neg is a selected adverb, it has to appear after the verb.",
              fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:v-initial']),

    AndNotFilter(name = 'sel-adv-v-init-2',
                 mrs_id_list = all_neg,
                 re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                 re2 = 'neg .*tv',
                 comment = "If the word order is v-initial and neg adverb is a selected adverb by virtue of co-occurring with the affix, it has to appear after the verb.",
                 fv = ['and','adv_neg:on','infl_neg:on','wordorder:v-initial']),

    NotFilter(name = 'sel-adv-v-final-1',
              mrs_id_list = all_neg,
              re1 = 'tv .*neg',
              comment = "If the word order is v-final and neg is a selected adverb, it has to appear before the verb.",
              fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:v-final']),

    AndNotFilter(name = 'sel-adv-v-init-2',
                 mrs_id_list = all_neg,
                 re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                 re2 = 'tv .*neg',
                 comment = "If the word order is v-final and neg adverb is a selected adverb by virtue of co-occurring with the affix, it has to appear before the verb.",
                 fv = ['and','adv_neg:on','infl_neg:on','wordorder:v-final']),

    MatchFilter(name = 'sel-adv-vo-1',
                mrs_id_list = n1_subj_neg,
                re1 = 'tv[( n2)( aux)]{,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg has to appear after the verb with at most an aux and the object (here, n2) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv',['or','wordorder:svo','wordorder:vos']]),

    MatchFilter(name = 'sel-adv-vo-2',
                mrs_id_list = n2_subj_neg,
                re1 = 'tv[( n1)( aux)]{,2} neg',
                comment = "If the word order is SVO or VOS and neg is a selected adverb, neg has to appear after the verb with at most an aux and the object (here, n1) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv',['or','wordorder:svo','wordorder:vos']]),

    AndMatchFilter(name = 'sel-adv-vo-3',
                   mrs_id_list = n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'tv(-neg){,1}[( n2)( aux)]{,2} neg',
                   comment = "If the word order is SVO or VOS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the verb with at most an aux and the object (here, n2) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on',['or','wordorder:svo','wordorder:vos']]),

    AndMatchFilter(name = 'sel-adv-vo-4',
                   mrs_id_list = n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'tv(-neg){,1}[( n1)( aux)]{,2} neg',
                   comment = "If the word order is SVO or VOS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the verb with at most an aux and the object (here, n1) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on',['or','wordorder:svo','wordorder:vos']]),

    MatchFilter(name = 'sel-adv-ov-1',
                mrs_id_list = n1_subj_neg,
                re1 = 'neg[( n2)( aux)]{,2} (neg-){,1}tv',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg has to appear before the verb with at most an aux and the object (here, n2) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv',['or','wordorder:sov','wordorder:ovs']]),

    MatchFilter(name = 'sel-adv-ov-2',
                mrs_id_list = n2_subj_neg,
                re1 = 'neg[( n1)( aux)]{,2} (neg-){,1}tv',
                comment = "If the word order is SOV or OVS and neg is a selected adverb, neg has to appear before the verb with at most an aux and the object (here, n1) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv',['or','wordorder:sov','wordorder:ovs']]),

    AndMatchFilter(name = 'sel-adv-ov-3',
                   mrs_id_list = n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg[( n2)( aux)]{,2} (neg-){,1}tv',
                   comment = "If the word order is SOV or OVS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most an aux and the object (here, n2) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on',['or','wordorder:sov','wordorder:ovs']]),

    AndMatchFilter(name = 'sel-adv-ov-4',
                   mrs_id_list = n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg[( n1)( aux)]{,2} (neg-){,1}tv',
                   comment = "If the word order is SOV or OVS and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most an aux and the object (here, n1) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on',['or','wordorder:sov','wordorder:ovs']]),

    MatchFilter(name = 'sel-adv-vso-1',
                mrs_id_list = n1_subj_neg,
                re1 = 'n1( n2){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear after the subject with at most the object (here, n2) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:vso']),

    MatchFilter(name = 'sel-adv-vso-2',
                mrs_id_list = n2_subj_neg,
                re1 = 'n2( n1){,1} neg',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear after the subject with at most the object (here, n1) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:vso']),

    AndMatchFilter(name = 'sel-adv-vso-3',
                   mrs_id_list = n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'n1( n2){,1} neg',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear after the subject with at most the object (here, n2) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on','wordorder:vso']),

    AndMatchFilter(name = 'sel-adv-vso-4',
                   mrs_id_list = n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'n2( n1){,1} neg',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the verb with at most the object (here, n1) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on','wordorder:vso']),

    MatchFilter(name = 'sel-adv-osv-1',
                mrs_id_list = n1_subj_neg,
                re1 = 'neg( n2){,1} n1',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear before the subject with at most the object (here, n2) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:osv']),

    MatchFilter(name = 'sel-adv-osv-2',
                mrs_id_list = n2_subj_neg,
                re1 = 'neg( n1){,1} n2',
                comment = "If the word order is VSO and neg is a selected adverb, neg has to appear before the subject with at most the object (here, n1) intervening."
                fv = ['and','adv_neg:on','neg-adv:sel-adv','wordorder:osv']),

    AndMatchFilter(name = 'sel-adv-osv-3',
                   mrs_id_list = n1_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg( n2){,1} n1',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the subject with at most the object (here, n2) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on','wordorder:osv']),

    AndMatchFilter(name = 'sel-adv-osv-4',
                   mrs_id_list = n2_subj_neg,
                   re1 = '(-neg|neg-.* neg(\s|$))|((\s|^)neg .*-neg|neg-)',
                   re2 = 'neg( n1){,1} n2',
                   comment = "If the word order is VSO and neg is a selected adverb by virtue of co-occuring with the negative affix, neg has to appear before the subject with at most the object (here, n1) intervening."
                   fv = ['and','adv_neg:on','infl_neg:on','wordorder:osv']),

######################################################################
# Filters for yes-no questions

    AlwaysFilter(name = 'no-overt-q',
                 mrs_id_list = all_ques,
                 comment = "If a language has no over mark of yes-no questions, reject all strings associated with the ques[1-4] mrs_ids.",
                 fv = ['ques:int']),

    MatchFilter(name = 'always-qpart',
                mrs_id_list = all_ques,
                re1 = 'qpart',
                comment = "Currently, we don't allow question particles to co-exist with other overt question marking, so if this strategy is selected, all question mrs_ids must be expressed with a qpart in the sentence.",
                fv = ['ques:qpart']),

    MatchFilter(name = 'qpart-init',
                mrs_id_list = all_ques,
                re1 = '^qpart',
                comment = "If the language type definition says that qpart is sentence-initial, make sure it's at the beginning of the string.",
                fv = ['qpartposthead:-']),

    MatchFilter(name = 'qpart-final',
                mrs_id_list = all_ques,
                re1 = 'qpart$',
                comment = "If the language type definition says that qpart is sentence-final, make sure it's at the end of the string.",
                fv = ['qpartposthead:+']),


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

    # wordorder:svo or wordorder:vos <- same outcomes, so can be collapsed
    # qinvverb:main or qinvverb:aux or qinvverb:main-aux <- need separate filters
    # auxorder:left or auxorder:right <- need separate filteres
    # Assuming that validate.py is ruling out subj-aux inversion with s-comp auxes.

    MatchFilter(name = "svo-vos-inv-1",
                mrs_id_list = n1_subj_ques,
                re1 = 'tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n1",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main']),

    MatchFilter(name = "svo-vos-inv-2",
                mrs_id_list = n2_subj_ques,
                re1 = 'tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is no auxiliary. Subj = n2",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main']),

    MatchFilter(name = "svo-vos-inv-3",
                mrs_id_list = n1_subj_ques,
                re1 = 'aux.* n1.* tv.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:aux','auxorder:left']),

    MatchFilter(name = "svo-vos-inv-4",
                mrs_id_list = n2_subj_ques,
                re1 = 'aux.* n2.* tv.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:aux','auxorder:left']),

    MatchFilter(name = "svo-vos-inv-5",
                mrs_id_list = n1_subj_ques,
                re1 = 'tv.* n2.* n1.* aux',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:aux','auxorder:right']),

    MatchFilter(name = "svo-vos-inv-6",
                mrs_id_list = n2_subj_ques,
                re1 = 'tv.* n1.* n2.* aux',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:aux','auxorder:right']),

    MatchFilter(name = "svo-vos-inv-7",
                mrs_id_list = n1_subj_ques,
                re1 = 'aux.* n1.* tv.* n2|tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n1",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main-aux','auxorder:left']),

    MatchFilter(name = "svo-vos-inv-8",
                mrs_id_list = n2_subj_ques,
                re1 = 'aux.* n2.* tv.* n1|tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which precedes V/VP. Subj = n2",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main-aux','auxorder:left']),

    MatchFilter(name = "svo-vos-inv-9",
                mrs_id_list = n1_subj_ques,
                re1 = 'tv.* n2.* n1.* aux|tv.* n1.* n2',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n1",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main-aux','auxorder:right']),

    MatchFilter(name = "svo-vos-inv-10",
                mrs_id_list = n2_subj_ques,
                re1 = 'tv.* n1.* n2.* aux|tv.* n2.* n1',
                comment = "This filter checks for the right word order in questions when the word order is SVO or VOS, questions are marked by subject-verb inversion, and there is potentially an auxiliary which follows V/VP. Subj = n2",
                fv = ['and',['or','wordorder:svo','wordorder:vos'],'ques:inv','qinvverb:main-aux','auxorder:right']),


*** TO HERE ***

    # O V S -> O S V
    # S O V -> O S V
    # O V S -> O V S Aux
    # S O V -> O V S Aux
    # O V S -> Aux S O V
    # S O V -> Aux S O V

    if ch('wordorder') == 'ovs' or ch('wordorder') == 'sov':
      if re.search('ques[12]',mrs_id):
        if not ((re.search('n2.* n1.* tv',sent) and not re.search('aux',sent)) or \
                re.search('n2.* tv.* n1.* aux',sent) or \
                re.search('aux.* n2.* n1.* tv',sent)):
          return True
      if re.search('ques[34]',mrs_id):
        if not ((re.search('n1.* n2.* tv',sent) and not re.search('aux',sent)) or \
                re.search('n1.* tv.* n2.* aux',sent) or \
                re.search('aux.* n1.* n2.* tv',sent)):
          return True
  
    # V S O -> V S O
    # V S O -> Aux S V O (VP auxes won't work in VSO languages)
    # V S O -> V O S Aux
    # O S V -> O S V
    # O S V -> O V S Aux (VP auxes won't work in OSV languages)
    # O S V -> Aux S O V

    if ch('wordorder') == 'vso' or ch('wordorder') == 'osv':
      if re.search('ques[12]',mrs_id):
        if not ((re.search('tv.* n1.* n2',sent) and not re.search('aux',sent)) or \
                re.search('aux.* n1.* tv.* n2',sent) or \
                re.search('tv.* n2.* n1.* aux',sent)):
          return True
      if re.search('ques[34]',mrs_id):
        if not ((re.search('tv.* n2.* n1',sent) and not re.search('aux',sent)) or \
                re.search('aux.* n2.* tv.* n1',sent) or \
                re.search('tv.* n1.* n2.* aux',sent)):
          return True
    
    # As for V-final or V-initial languages, the predictions are weird.
    # That is, we get homophony with declaratives.  I think we should just
    # constrain validate.py to refuse to implement this strategy in V-final
    # or V-initial languages.  ... In fact, I'm going to do that.
    # Likewise for free word order.  How could you use word order to
    # mark questions, if word order is free in general?


######################################################################
# filter_lexicon(sent)

# ERB 2006-10-12 The general filters related to this are all in
# filter_word_order.

def filter_lexicon(sent, mrs_id):

  # Figure out if we're in a case where the auxiliary is controlling
  # the pos of the subj.

  auxsubj = False
  if re.search('aux',sent) and \
     (ch('auxcomp') == 'VP' or ch('auxcomp') == 'V'):
    auxsubj = True
     
  

  # Checking for -nf forms, if appropriate.  The only seed strings
  # with the -nf forms (so far) are those with auxiliaries.  Should
  # eventually test some strings with -nf forms and no aux.... _FIX_ME_

  if re.search('wo|neg|ques',mrs_id):
    if ch('iverb-nonfinite') == 'iv-nf':
      if re.search('aux',sent) and re.search('iv',sent) \
         and not re.search('iv-nf',sent):
        return True

    if ch('tverb-nonfinite') == 'tv-nf':
      if re.search('aux',sent) and re.search('tv',sent) \
         and not re.search('tv-nf',sent):
        return True

    if not ch('iverb-nonfinite') and re.search('iv-nf',sent):
      return True

    if not ch('tverb-nonfinite') and re.search('tv-nf',sent):
      return True

  # This assumes that noun1 has the form n1, noun2 has the form n2, etc.
  if re.search('wo|neg|ques',mrs_id):
    if ch('noun1spr') == 'obl' and re.search('n1', sent):
      if not re.search('n1 det|det n1',sent):
        return True
    if ch('noun2spr') == 'obl' and re.search('n2', sent):
      if not re.search('n2 det|det n2',sent):
        return True
    #This one assumes that all nouns are n+digit(s), and that we only
    #see n1 once/string.
    if ch('noun1spr') == 'nil':
      if re.search('n1 det', sent):
        if re.search('n1 det n2', sent):
          if re.search('n1 det n2 det',sent):
            return True
          elif ch('noun2spr') == 'nil':
            return True
        else:
          return True
      if re.search('det n1', sent):
        if re.search('n2 det n1', sent):
          if re.search('det n2 det n1',sent):
            return True
          elif ch('noun2spr') == 'nil':
            return True
        else:
          return True

    #This one assumes that all nouns are n+digit(s), and that we only
    #see n2 once/string.
    if ch('noun2spr') == 'nil':
      if re.search('n2 det', sent):
        if re.search('n2 det n1', sent):
          if re.search('n2 det n1 det', sent):
            return True
          elif ch('noun1spr') == 'nil':
            return True
        else:
          return True
      if re.search('det n2', sent):
        if re.search('n1 det n2', sent):
          if re.search('det n1 det n2', sent):
            return True
          elif ch('noun1spr') == 'nil':
            return True
        else:
          return True

    if ch('iverbSubj') == 'pp':
      # We're talking about intransitive verbs here.  n1 is always
      # the subject.

      # BUT: The auxiliary might want an np subject, so if there's an
      # aux, we have to consider it separately.
      if re.search('wo[12]$',mrs_id):
        if not auxsubj:
          if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
            return True

    if ch('iverbSubj') == 'np':

      # Likewise here, the aux might want a pp subject.
      
      if re.search('wo[12]$',mrs_id) and not auxsubj
        if re.search('p-nom',sent):
          return True

    #I think we don't have to worry about picking up a det or a p-nom from the other np,
    #because the general filters should have taken out those cases. ??

    if ch('tverbSubj') == 'pp' and not auxsubj
      if re.search('wo[3-6]|neg[12]|ques[12]',mrs_id):
        if not re.search(r'p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
          return True
      # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
      if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
        if not re.search(r'p-nom n2|n2 p-nom|p-nom det n2|p-nom n2 det|n2 det p-nom|det n2 p-nom',sent):
          return True

    if ch('tverbSubj') == 'np' and re.search('p-nom',sent) and not auxsubj
        return True


    if ch('tverbObj') == 'pp':
      if re.search('wo[3-6]|neg[12]|ques[12]',mrs_id):
        if not re.search(r'p-acc n2|n2 p-acc|p-acc det n2|p-acc n2 det|n2 det p-acc|det n2 p-acc',sent):
          return True
      # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
      if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
        if not re.search(r'p-acc n1|n1 p-acc|p-acc det n1|p-acc n1 det|n1 det p-acc|det n1 p-acc',sent):
          return True

    if ch('tverbObj') == 'np' and re.search('p-acc',sent):
        return True

    if ch('objAdp') == 'pre':
      if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
        if re.search('n2 p-acc|n2 det p-acc',sent):
          return True
    if ch('objAdp') == 'post':
      if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
        if re.search('p-acc n2|p-acc det n2',sent):
          return True
    if ch('subjAdp') == 'pre':
      if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
        if re.search('n1 p-nom|n1 det p-nom',sent):
          return True
    if ch('subjAdp') == 'post':
      if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
        if re.search('p-nom n1|p-nom det n1',sent):
          return True

    # Aux cases:
    # _FIX_ME_ worry about questions here
    # 1. No auxiliaries

    if ch('language') != '' and ch('auxverb') == '' and re.search('aux',sent):
        return True
      
    # 2. Arg composition => adjacent to v, let's say for now even in free word order
    #        may be further specified as left only or right only.

    if re.search('wo|neg',mrs_id):
      if ch('auxverb') and ch('auxcomp')=='V':
        if ch('auxorder') == 'right':
          if re.search('aux',sent) and not re.search('(tv|iv)(-nf)* (neg )*aux',sent):
            return True
        elif ch('auxorder') == 'left':
          if re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)',sent):
            return True
        elif re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)|(tv|iv)(-nf)* (neg )*aux',sent):
          return True
      
      # 3. VP comp, so S attaches outside: aux n1,n2 tv or aux n1 tv
      #        may be further specified as left only or right only

      if ch('auxverb') and ch('auxcomp')=='VP':
        if re.search('aux .*n1 .*n2 .*tv|aux .*n2 .*n1 .*tv|tv(-nf)* .*n2 .*n1 .*aux|tv(-nf)* .*n1 .*n2 aux',sent):
          return True
        if re.search('aux .*n1 .*iv|iv(-nf)* .*n1 .*aux',sent):
          return True
    
    # 4. S comp, attaches outside S and O.
    #        may be further specified as left only or right only

      if ch('auxverb') and ch('auxcomp') == 'S':
        if re.search('(n1|n2) .*aux .*(tv|iv)',sent) or \
           re.search('(tv|iv)(-nf)* .*aux .*(n1|n2)',sent):
          return True

    # This left or right only seems to work across all complementation cases.
    
      if ch('auxorder') == 'right' and re.search('aux .*(tv|iv)',sent):
        return True
      if ch('auxorder') == 'left' and re.search('(tv|iv)(-nf)* .*aux',sent):
        return True

    # Now worry about the pos of the subject in sentences with auxiliaries:

    if auxsubj:
      
      if ch('auxsubj') == 'noun':
        if re.search('wo|neg|ques',mrs_id):
          if re.search('p-nom',sent):
            return True

      if ch('auxsubj') == 'adp':
        if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
          if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
            return True
        if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
          if not re.search('p-nom n2|n2 p-nom|p-nom det n2|p-nom n2 det|n2 det p-nom|det n2 p-nom',sent):
            return True

  return False


    ]
               
                

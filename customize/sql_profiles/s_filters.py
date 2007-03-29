#################################################################
# Initial set of language-type specific word order filters.

from filters import NotFilter
from filters import AndNotFilter
from filters import MatchFilter
from filters import NotCopresentFilter
from filters import AlwaysFilter
from filters import MatchAndFilter

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
             
filter_list = [

####################################################################
# Word order filters    

    NotFilter(name = "s-o-order1",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'n1.*n2',
              comment = "If the word order for the language type has subjects before objects, and n1 is the subject and n2 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:svo','wordorder:sov','wordorder:vso']),

    NotFilter(name = "s-o-order2",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has subjects before objects, and n2 is the subject and n1 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:svo','wordorder:sov','wordorder:vso']),

    NotFilter(name = "s-o-order3",
              mrs_id_list = n1_subj_n2_obj,
              re1 = 'n2.*n1',
              comment = "If the word order for the language type has objects before subjects, and n1 is the subject and n2 is the object, check that n2 precedes n1.   (Worry some about what to do with inversion constructions ... do they affect this?)",
           fv = ['or', 'wordorder:ovs','wordorder:osv','wordorder:vos']),

    NotFilter(name = "s-o-order4",
              mrs_id_list = n2_subj_n1_obj,
              re1 = 'n1.*n2',
              comment = "If the word order for the language type has objects before subjects, and n2 is the subject and n1 is the object, check that n1 precedes n2.   (Worry some about what to do with inversion constructions ... do they affect this?)",
              fv = ['or', 'wordorder:ovs','wordorder:osv','wordorder:vos']),

    NotFilter(name = "s-v-order1",
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

    NotFilter(name = "s-v-order3",
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

    NotFilter(name = "det-n-order1",
              mrs_id_list = one_det_on_n1,
              re1 = 'det n1',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n1, the det should immediately precede n1.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:SpecHead']),

    NotFilter(name = "det-n-order2",
              mrs_id_list = one_det_on_n1,
              re1 = 'n1 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n1, the det should immediately follow n1.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:HeadSpec']),

    NotFilter(name = "det-n-order3",
              mrs_id_list = one_det_on_n2,
              re1 = 'det n2',
              comment = "If the language has determiners preceding nouns, and the sentence has a determiner for n2, the det should immediately precede n2.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:SpecHead']),

    NotFilter(name = "det-n-order4",
              mrs_id_list = one_det_on_n2,
              re1 = 'n2 det',
              comment = "If the language has determiners following nouns, and the sentence has a determiner for n2, the det should immediately follow n2.  Revise when we add nominal modifiers.",
              fv = ['NounDetOrder:HeadSpec']),

    MatchFilter(name = "det-n-order5",
                mrs_id_list = two_dets_n1_n2,
                re1 = '(det .*det (n1|n2))|(det (n1|n2) .*det (n1|n2))|(det (n1|n2) .*(n1|n2) det)',
                comment = "If the language has determiners following nouns, and n1 and n2 both have determiners attached, neither should be preceded by a determiner.  This is perhaps more baroque than it needs to be?",
                fv = ['NounDetOrder:HeadSpec']),

    MatchFilter(name = "det-n-order6",
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
              comment = "If we only selected adverbial negation, we should see one in every negated sentence",
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
                       fv = ['and', 'infl_neg:on','adv_neg:on','multineg:comp'])
    ]
               
                

#!/usr/local/bin/python2.4

# FIXME:  NotFilter should probably be MatchFilter throughout.

########################################################################
# Universal filters.  This file just defines the filter_list of universal
# filtesr.  See run_u_filters.py for actually running them, and
# test_u_filters.py for testing them.

from filters import NotFilter
from filters import AndNotFilter
from filters import MatchFilter
import g

########################################################################
# Filters

filter_list = [AndNotFilter(name = "uf1", 
                  mrs_id_list = g.no_det_n1_subj,
                  re1 = 'p-nom',
                  re2 = 'p-nom n1|n1 p-nom',
                  comment = "If n1 is the subject, and there is no determiner for it, any p-nom in the sentence needs to be adjacent to n1."),
               
               AndNotFilter(name = "uf2", 
                            mrs_id_list = g.no_det_n2_subj,
                            re1 = 'p-nom',
                            re2 = 'p-nom n2|n2 p-nom',
                            comment = "If n2 is the subject, and there is no determiner for it, any p-nom in the sentence needs to be adjacent to n2."),
               
               AndNotFilter(name = "uf3", 
                            mrs_id_list = g.no_det_n2_subj,
                            re1 = 'p-acc',
                            re2 = 'p-acc n1|n1 p-acc',
                            comment = "If n1 is the object, and there is no determiner for it, any p-acc in the sentence needs to be adjacent to n1."),
               
               AndNotFilter(name = "uf4",
                            mrs_id_list = g.no_det_n2_obj,
                            re1 = 'p-acc',
                            re2 = 'p-acc n2|n2 p-acc',
                            comment = "If n2 is the object, and there is no determiner for it, any p-acc in the sentence needs to be adjacent to n2."),
               
               NotFilter(name = "uf5",
                         mrs_id_list = g.one_det_on_n1,
                         re1 = 'det n1|n1 det',
                         comment = "If there's only one det, and it's attached to n1, it must be adjacent to n1."),
               
               NotFilter(name = "uf6",
                         mrs_id_list = g.one_det_on_n2,
                         re1 = 'det n2|n2 det',
                         comment = "If there's only one det, and it's attached to n2, it must be adjacent to n2."),
               
               NotFilter(name = "uf7",
                         mrs_id_list = g.two_dets_n1_n2,
                         re1 = '(det (n1|n2).*det (n1|n2))|(det (n1|n2).*(n1|n2) det)|((n1|n2) det.*det (n1|n2))|((n1|n2) det.*(n1|n2) det)',
                         comment = "If there are two dets, each one has to be next to a noun, and no fair putting them both next to the same noun."),

               AndNotFilter(name = "uf8",
                            mrs_id_list = g.n1_subj_with_det,
                            re1='p-nom',
                            re2='(p-nom det n1)|(p-nom n1 det)|(det n1 p-nom)|(n1 det p-nom)',
                            comment = "If n1 is the subject and it does have a determiner attached to, it, if there's a p-nom, it has to form a coherent NP with n1 and det.  Not worrying for now about that det actually belonging to n2, since then probably something else will be wrong with the string."),
                            
               AndNotFilter(name = "uf9",
                            mrs_id_list = g.n2_subj_with_det,
                            re1='p-nom',
                            re2='(p-nom det n2)|(p-nom n2 det)|(det n2 p-nom)|(n2 det p-nom)',
                            comment = "If n2 is the subject and it does have a determiner attached to it, if there's a p-nom, it has to form a coherent NP with n2 and det."),

               AndNotFilter(name = "uf10",
                            mrs_id_list = g.n1_obj_with_det,
                            re1='p-acc',
                            re2='(p-acc det n1)|(p-acc n1 det)|(det n1 p-acc)|(n1 det p-acc)',
                            comment = "If n1 is the object and it does have a determiner attached to it, if there's a p-acc, it has to form a coherent NP with n1 and det. Not worrying for now about that det actually belonging to n2, since then probably something else will be wrong with the string."),

               AndNotFilter(name = "uf11",
                            mrs_id_list = g.n2_obj_with_det,
                            re1 = 'p-acc',
                            re2 = '(p-acc det n2)|(p-acc n2 det)|(det n2 p-acc)|(n2 det p-acc)',
                            comment = "If n2 is the object and it does have a determiner attached to it, if there's a p-acc, it has to form a coherent NP with n1 and det."),

               AndNotFilter(name = "uf12",
                            mrs_id_list = g.neg,
                            re1 = '(neg-)|(-neg)',
                            re2 = '(neg-[a-z-]*(tv|iv|aux))|((tv|iv|aux)[a-z-]*-neg)',
                            comment = "If there's a negative affix in the sentence, it has to be attached to tv, iv, or aux.  '[a-z-]*' is meant to allow other affixes in between. Not tested as of 3/24/07"),

               MatchFilter(name = "uf13",
                           mrs_id_list = g.ques,
                           re1 = '^.*[a-z]+.*qpart.*[a-z]+.*$',
                           comment = "If the question particle appears in the middle of the string, then this is clearly no good. --- Applies to examples with matrix questions.  Different filter will be needed once we do embedded questions."),

               AndNotFilter(name = "uf14",
                            mrs_id_list = g.all,
                            re1 = '-nf',
                            re2 = 'aux',
                            comment = "So far, the non-finite forms should only appear in the context of an auxiliary."),

               AndNotFilter(name = "uf15",
                            mrs_id_list = g.ques,
                            re1 = "-ques|ques-",
                            re2 = '(ques-[a-z-]*(tv|iv|aux))|((tv|iv|aux)[a-z-]*-ques)',
                            comment = "If there's a question affix in the sentence, it has to be attached to tv, iv, or aux.  '[a-z-]*' is meant to allow other affixes in between. Not tested as of 3/24/07"),

               MatchFilter(name = "uf16",
                         mrs_id_list = g.neg,
                         re1 = "neg",
                         comment = "If the semantics indicates negation, there should be a negative morpheme somewhere.")]



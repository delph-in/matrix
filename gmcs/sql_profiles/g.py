"""
File: g.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: 7/30/09 - taken over on this date
Project: MatrixTDB RA, summer 2009
Project Owner: Emily M. Bender
Contents:
    - a series of lists that define groupings of semantic tags or classes.  These lists are used
      when creating filters to say that a filter applies to these groups of semantic classes.
Note: g as in global
"""

# Variables storing sets of MRSs ids.  We can add to these and update multiple
# filters at once as we add harvester strings.

# These variables are referenced in s_filters, u_filters, and add_permutes and stringmod.py

# NOTE: When adding new mrs_ids to these lists, one should always examine
# the filters that call the lists being modified.  In some cases, we may need
# to create new lists in order to maintain the right behavior.

# _FIX_ME_ This should probably be stored in the DB, and g.py should read
# in definitions of these variables.

n1_subj = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2'] # n1 as subject

# semantic classes with n1 as subject, n2 as object
n1_subj_n2_obj = ['wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2']

# semantic classes with n2 as subject, n1 as object
n2_subj_n1_obj = ['wo7','wo8','wo9','wo10','neg3','neg4','ques3','ques4']

# semantic classes of non-questions with n1 as subject.  used mostly for aux-verb order since
# questions can have them in a different order if they have inversion
n1_subj_not_ques = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2']

# semantic classes of non-questions with n1 as subject and n2 as object.  used mostly for
# aux-verb order since questions can have them in a different order if they have inversion
n1_subj_n2_obj_not_ques = ['wo3','wo4','wo5','wo6','neg1','neg2']
n1_subj_ques = ['ques1','ques2']                                            # questions with n1 as subject

# semantic classes of non-questions with n2 as subject and n1 as object.  used mostly for
# aux-verb order since questions can have them in a different order if they have inversion
n2_subj_n1_obj_not_ques = ['wo7','wo8','wo9','wo10','neg3','neg4']
n2_subj_ques = ['ques3','ques4']                                            # questions with n2 as subject

# sentences with determiners
with_dets = ['wo2','wo4','wo5','wo6','wo8','wo9','wo10','neg2','neg4','ques2','ques4']
one_det_on_n1 = ['wo2','wo5','wo10']    # sentences with a determiner just for n1
one_det_on_n2 = ['wo6','wo9']             # sentences with a determiner just for n2

# sentences with determiners on both nouns
two_dets_n1_n2 =['wo4','wo8','neg2','neg4','ques2','ques4']
n1_with_det = one_det_on_n1 + two_dets_n1_n2    # sentences with a determiner for n1
n2_with_det = one_det_on_n2 + two_dets_n1_n2    # sentences with a determiner for n2

# semantic class with n1 as subj and no determiner on n1
no_det_n1_subj = ['wo1','wo3','wo6','neg1','ques1']

# semantic class with n2 as subj and no determiner on n2
no_det_n2_subj = ['wo7','wo10','neg3','ques3']

# semantic class with n2 as object and no determiner on n2
no_det_n2_obj = ['wo3','wo6','neg1','ques1']

# semantic class with n1 as subj and a determiner on n1
n1_subj_with_det = ['wo2','wo4','wo5','neg2','ques2']

# semantic class with n2 as subject and a determiner on n2
n2_subj_with_det = ['wo8','wo9','neg4','ques4']

# semantic class with n1 as object and a determiner on n1
n1_obj_with_det = ['wo8','wo10','neg4','ques4']

# semantic class with n2 as object and a determiner on n2
n2_obj_with_det = ['wo4','wo6','neg2','ques2']
all_neg = ['neg1','neg2','neg3','neg4']             # negative semantic class
n1_subj_neg = ['neg1','neg2']                       # negative semantic class with n1 as subject

# 8/17/09 KEN changed from neg1, neg2 to mirror n1_subj_ques and n2_subj_ques and because
# neg3 and neg4 are the neg sem classes where n2 is the subject
n2_subj_neg = ['neg3','neg4']                       # negative semantic class with n2 as subject
all_ques = ['ques1','ques2','ques3','ques4']     # question semantic class
n1_subj_ques = ['ques1','ques2']                  # question semantic class with n1 as subject
n2_subj_ques = ['ques3','ques4']                  # question semantic class with n2 as subject

# all harvester semantic tags
# TODO: don't assign to built-in function all
all = ['wo1', 'wo2', 'wo3', 'wo4', 'wo5', 'wo6', 'wo7', 'wo8', 'wo9', 'wo10', 'neg1', 'neg2', 'neg3',
       'neg4', 'ques1', 'ques2', 'ques3', 'ques4']

#semantic class of transitive sentences
trans = ['wo3', 'wo4', 'wo5', 'wo6', 'wo7', 'wo8', 'wo9', 'wo10', 'neg1', 'neg2', 'neg3', 'neg4',
         'ques1', 'ques2', 'ques3', 'ques4']
intrans = ['wo1','wo2']                 # semantic class of intransitive sentences

# negative semantic class.  TODO: fix this duplication with all_neg
neg = ['neg1','neg2','neg3','neg4']

# question semantic class.  TODO: fix this duplication with all_ques
ques = ['ques1','ques2','ques3','ques4']
coord = ['wo2-coord-n','wo2-coord-np','wo2-coord-vp','wo2-coord-s'] # coordination semantic class

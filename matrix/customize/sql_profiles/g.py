# Variables storing sets of MRSs ids.  We can add to these and update multiple
# filters at once as we add harvester strings.

# These variables are referenced in s_filters, u_filters, and add_permutes.

# NOTE: When adding new mrs_ids to these lists, one should always examine
# the filters that call the lists being modified.  In some cases, we may need
# to create new lists in order to maintain the right behavior.

# _FIX_ME_ This should probably be stored in the DB, and g.py should read
# in definitions of these variables.

n1_subj = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2']
n1_subj_n2_obj = ['wo3','wo4','wo5','wo6','neg1','neg2','ques1','ques2']
n2_subj_n1_obj = ['wo7','wo8','wo9','wo10','neg3','neg4','ques3','ques4']
n1_subj_not_ques = ['wo1','wo2','wo3','wo4','wo5','wo6','neg1','neg2']
n1_subj_n2_obj_not_ques = ['wo3','wo4','wo5','wo6','neg1','neg2']
n1_subj_ques = ['ques1','ques2']
n2_subj_n1_obj_not_ques = ['wo7','wo8','wo9','wo10','neg3','neg4']
n2_subj_ques = ['ques3','ques4']
with_dets = ['wo2','wo4','wo5','wo6','wo8','wo9','wo10','neg2','neg4','ques2','ques4']
one_det_on_n1 = ['wo2','wo5','wo10']
one_det_on_n2 = ['wo6','wo9']
two_dets_n1_n2 =['wo4','wo8','neg2','neg4','ques2','ques4']
n1_with_det = one_det_on_n1 + two_dets_n1_n2
n2_with_det = one_det_on_n2 + two_dets_n1_n2
no_det_n1_subj = ['wo1','wo3','wo6','neg1','ques1']
no_det_n2_subj = ['wo7','wo10','neg3','ques3']
no_det_n2_obj = ['wo3','wo6','neg1','ques1']
n1_subj_with_det = ['wo2','wo4','wo5','neg2','ques2']
n2_subj_with_det = ['wo8','wo9','neg4','ques4']
n1_obj_with_det = ['wo8','wo10','neg4','ques4']
n2_obj_with_det = ['wo4','wo6','neg2','ques2']
all_neg = ['neg1','neg2','neg3','neg4']
n1_subj_neg = ['neg1','neg2']
n2_subj_neg = ['neg1','neg2']
all_ques = ['ques1','ques2','ques3','ques4']
n1_subj_ques = ['ques1','ques2']
n2_subj_ques = ['ques3','ques4']
all = ['wo1', 'wo2', 'wo3', 'wo4', 'wo5', 'wo6', 'wo7', 'wo8', 'wo9', 'wo10', 'neg1', 'neg2', 'neg3', 'neg4', 'ques1', 'ques2', 'ques3', 'ques4']
trans = ['wo3', 'wo4', 'wo5', 'wo6', 'wo7', 'wo8', 'wo9', 'wo10', 'neg1', 'neg2', 'neg3', 'neg4', 'ques1', 'ques2', 'ques3', 'ques4']
intrans = ['wo1','wo2']
neg = ['neg1','neg2','neg3','neg4']
ques = ['ques1','ques2','ques3','ques4']
coord = ['wo2-coord-n','wo2-coord-np','wo2-coord-vp','wo2-coord-s']

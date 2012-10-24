from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy
from gmcs.utils import get_name
from gmcs.linglib import case



# Returns the verb type for lexical/main verbs.
def main_or_verb(ch):
  if ch.get('has-aux') == 'yes':
    return 'main-verb-lex'
  else:
    return 'verb-lex'


################################################################################
# New cross-classification set-up for verbal types
#
#
#
################################################################################

def create_subcategorization_values(ch, mylang):

  mylang.add('synsem-sat := synsem & \
               [ LOCAL.CAT.VAL [ SUBJ < >, \
                                 COMPS < >, \
                                 SPR < > ] ].')
  #HEADS, FORMS, CASES: CREATE SYNSEM FOR EACH
  
  # if full tiger lexicon is used, no arg should be optional for verbs
  if not ch.get('subcat-regroup') == 'on':
    mylang.add('synsem-sat := [ OPT - ].')  
  else:
    create_optional_argument_frames(mylang)
    

  synsem_ts = []
  for head in ch.get('subcatheads').split(','):
    if head:
      name = head + '-synsem'
      mylang.add(name + ' := synsem & \
                [ LOCAL.CAT.HEAD ' + head + ' ].')
      synsem_ts.append(name)
      if head == 'comp':
        mylang.add(name + ' := [ LOCAL.CONT.HOOK.INDEX.SF prop-or-ques ].')
        mylang.add('prop-' + name + ' := ' + name + ' & \
                                  [ LOCAL.CONT.HOOK.INDEX.SF prop ].') 
        mylang.add('wh-synsem := ' + name + ' & \
                                  [ LOCAL.CONT.HOOK.INDEX.SF ques ].')
        mylang.add('phr-wh-synsem := wh-synsem & phr-synsem.')
        mylang.add('gap-wh-synsem := wh-synsem & gap.')
        mylang.add('phr-prop-synsem := prop-comp-synsem & phr-synsem.')
        mylang.add('gap-prop-synsem := prop-comp-synsem & gap.')
      elif head == 'adj':
        mylang.add(name + ' := [ LOCAL.CAT.HEAD.PRD + ].')
  for form in ch.get('subcatforms').split(','):
    if form:
      name = form + '-synsem'
      mylang.add(name + ' := verb-synsem & \
                [ LOCAL.CAT.HEAD.FORM ' + form + ' ].')
      synsem_ts.append(name)
  for case in ch.get('subcatcases').split(','):
    if case:
      name = case + '-synsem'
      mylang.add(name + ' := \
                [ LOCAL.CAT.HEAD.CASE ' + case + ' ].')
      synsem_ts.append(name)

#CROSS-CLASS: if not in exceptions: synsem-sat, final name based on
#FORM or CASE or (if irrelevant, HEAD adds synsem-sat)

def create_synsem_cross_classifications(ch, mylang):

  combs = ch.get('subcatcombs').split(',')
  val_excepts = ch.get('subcat-valexcept')
####simplification: only subject can remain open
  exs = []
  for ve in val_excepts.split(','):
    exs.append(ve)
  if len(exs) > 0:
    mylang.add('sbj-open-synsem := synsem & \
           [ LOCAL.CAT.VAL [ SUBJ <[ ]>, \
                             COMPS < >, \
                             SPR < > ] ].')
  # if full tiger lexicon is used, no arg should be optional for verbs
    if not ch.get('subcat-regroup') == 'on':
      mylang.add('synsem-sat := [ OPT - ].')
    else:
      create_optional_argument_frames(mylang)
  for comb in combs:
    parts = comb.split('-')
    if comb == 'verb-finite':
      name = 'sent-synsem'
    elif comb == 'verb-infinitive':
      name = 'inf-synsem'
    else:
      name = comb + '-synsem'
#currently: form and case reserved to verb/noun respectively adding supert 
      name = parts[1] + '-synsem'      

    mylang.add(name + ' := ' + parts[0] + '-synsem.')
# should be able to occur as phrase (as argument) or be gapped
# subtypes allowing for both unifications
    mylang.add('phr-' + name + ' := ' + name + ' & phr-synsem.')
    mylang.add('gap-' + name + ' := ' + name + ' & gap.')
    if not comb in exs:
      mylang.add(name + ' := synsem-sat.')
    else:
      mylang.add(name + ' := sbj-open-synsem.')

####redo: for each position define possibles
####sum of these consitutues list from above

#2. Create types of val that subcat for these synsems
#2. a) only those that occur in this position (i.e. no acc subj)
#   b) adapt comps code
#3. Cross-classification, based on list: only those occurring are created
#   a) add list of existing types (other approach is fall-back)
#4. Create version that merges cp or nom-acc can both merge
#5. Make Haugereid set-up: one lex entry for all words
# a). retrieve groups of subcategorization frames
# b). create list of groups
# c). create classification groups (use underspecification code as found for
# case, gender, etc.)
#6. Run replacement algorithm on Bart's lexicon: create ready to integrate lexicon
#7. Collect irules + irreg tabs
#8. Make sure all are created
#9. Run replacement algorithm on irules/irreg tabs


def create_optional_argument_frames(mylang):

  # subject already done in create_basic_subject_values
  # optional objects

  mylang.add('1st-comp-opt-lex-item := lex-item & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT + ].')

  mylang.add('2nd-comp-opt-lex-item := lex-item & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.FIRST.OPT + ].') 

  mylang.add('3rd-comp-opt-lex-item := lex-item & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.REST.FIRST.OPT + ].')

  # 1comp
 
  mylang.add('comp1-lex-item := lex-item & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')

  # 2comps
#  mylang.add('opt-comp1-opt-comp2-lex-item := 1st-comp-opt-lex-item & 2nd-comp-opt-lex-item.')
#  mylang.add('opt-comp1-comp2-lex-item := 1st-comp-opt-lex-item & \
  mylang.add('comp2-lex-item := lex-item & \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.FIRST.OPT - ].')

#  mylang.add('comp1-opt-comp2-lex-item := 2nd-comp-opt-lex-item & \
#                 [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')
#  mylang.add('comp1-comp2-lex-item := lex-item & \
#            [ SYNSEM.LOCAL.CAT.VAL.COMPS [ FIRST.OPT -,
#                                           REST.FIRST.OPT - ] ].')


  # 3comps
#  mylang.add('opt-comp1-opt-comp2-opt-comp3-lex-item := opt-comp1-opt-comp2-lex-item & 3rd-comp-opt-lex-item.')

  mylang.add('comp3-lex-item := lex-item & \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.REST.FIRST.OPT - ].')

#  mylang.add('opt-comp1-comp2-opt-comp3-lex-item := opt-comp1-comp2-lex-item & \
#     3rd-comp-opt-lex-item.')

#  mylang.add('opt-comp1-comp2-comp3-lex-item := opt-comp1-comp2-lex-item & \
#          [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.REST.FIRST.OPT - ].') 
  
#  mylang.add('comp1-opt-comp2-opt-comp3-lex-item := comp1-opt-comp2-lex-item & \
#             3rd-comp-opt-lex-item.')
 
#  mylang.add('comp1-opt-comp2-comp3-lex-item := comp1-opt-comp2-lex-item & \
#       [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.REST.FIRST.OPT - ].') 

#  mylang.add('comp1-comp2-opt-comp3-lex-item := comp1-comp2-lex-item & \
#       3rd-comp-opt-lex-item.') 
 
#  mylang.add('comp1-comp2-comp3-lex-item := comp1-comp2-lex-item & \
#       [ SYNSEM.LOCAL.CAT.VAL.COMPS.REST.REST.FIRST.OPT - ].')


def create_basic_subject_values(ch, mylang, stype):
# 
####list of those elements that can occur as subject
  subj_list = []
  my_subs = ch.get('subj_prop')
  subs = my_subs.split(',')

  opt = ch.get('opt_subj',[])

  if len(opt) > 0:
    mylang.add('opt-sbj-verb-lex := ' + stype + ' & \
       [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ OPT + ] >].')
    mylang.add('obl-sbj-verb-lex := ' + stype + ' & \
       [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ OPT - ] >].')

    opt_kds = []  
    for k in opt:
      kn = k.get('name')
      opt_kds.append(kn)
    
  if ch.get('expl') == 'yes':
    eh = ch.get('expl_head')
    ec = ch.get('expl_case')
    if ec:
      pref = ec
    else:
      pref = eh
    mylang.add('expl-synsem := ' + pref + '-synsem & \
                    [ LOCAL.CONT.HOOK.INDEX expl-ind ].')
      


  for s in subs:     
###Subjects in German (non-spoken language) can only be dropped if they are
###explitives  
    ps = s.split('-')
    if len(ps) > 1:
      tn = p[1]
    else:
      tn = s
    mys = stype

    mylang.add(tn + '-sbj-verb-lex := ' + mys + ' & \
             [ SYNSEM.LOCAL.CAT.VAL.SUBJ < ' + s + '-synsem > ].')
    
    subj_list.append(tn)

  return subj_list

def create_some_compl(ch, mylang, stype, prefix):
  myc = prefix + '_comp'
  comps = ch.get(myc + '-cmps-sub').split(',')
  refl = ch.get(myc + '_refl')
  opt_refl = ch.get(myc + 'opt-refl')
  part = ch.get(myc + '_part')
  opts = ch.get(myc + '_opt',[])

  arg_ana = ch.get('adp-arg-analysis')

  comps_list = [] 

###############
# NEXT: COLLECT IRULES FROM TABS AND CREATE IRULES
################

  opt_kds = []
  if opts:
    for opt in opts:
      opt_kds = opt.get('kind')
    constraint = 'OPT +'
    cur_stype = prefix + '-comp-verb-lex'
    name = 'opt-' + cur_stype
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    add_comp_feature(mylang, cur_stype, name, constraint)
    name = 'obl-' + cur_stype
    constraint = 'OPT -'
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    add_comp_feature(mylang, cur_stype, name, constraint)
        
  for c in comps:
    if 'opt_' in c:
      cn = c.replace('opt_','')
    else:
      cn = c
    constraint = cn + '-synsem'
    if prefix != '1st':
      constraint = 'synsem, ' + constraint
      if prefix != '2nd':
        constraint = 'synsem, ' + constraint
        if prefix != '3rd':
          constraint = 'synsem, ' + constraint
    basic_stype = 'main-verb-lex'
    if opts:
      if c in opt_kds:
        stype = basic_stype
        mylang.add('opt-' + c + '-' + prefix + '-comp-verb-lex := \
                                 opt-' + stype + ' & \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS <' + constraint + ', ... > ].')
        mylang.add('obl-' + c + '-' + prefix + '-comp-verb-lex := \
                                 obl-' + stype + ' & \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS <' + constraint + ', ... > ].')
      else:
        stype = 'obl-' + basic_stype  
    else:
      stype = basic_stype
    mylang.add(c + '-' + prefix + '-comp-verb-lex := ' + stype + ' & \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS <' + constraint + ', ... > ].')
    comps_list.append(c) 
  
####REFL
  if refl:
    mylang.add('refl-1st-comp-verb-lex := acc-1st-comp-verb-lex & comp1-lex-item & \
         [ SYNSEM.LOCAL.CAT.VAL [ COMPS.FIRST.LOCAL refl-local & \
                                     [ CONT.HOOK.INDEX #ind ], \
                             SUBJ < [ LOCAL.CONT.HOOK.INDEX #ind ] > ] ].')
    comps_list.append('refl')

  if opt_refl:
    mylang.add('opt-refl-1st-comp-verb-lex := acc-1st-comp-verb-lex & 1st-comp-opt-lex-item & \
         [ SYNSEM.LOCAL.CAT.VAL [ COMPS.FIRST.LOCAL refl-local & \
                                     [ CONT.HOOK.INDEX #ind ], \
                             SUBJ < [ LOCAL.CONT.HOOK.INDEX #ind ] > ] ].')
    comps_list.append('refl')
####PART
  if part:
#particle is never followed by another argument
    mylang.add('lexkeys :+ [ KEY-PART list ].', section='addenda')
    mylang.add('verb :+ [ PART-FORM list ].', section='addenda')
    mylang.add('main-verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.PART-FORM < "nopart" > ].')
    mylang.add('aux-lex := [ SYNSEM.LOCAL.CAT.HEAD.PART-FORM < "nopart" > ].')
    constraint = 'OPT -, \
              LOCAL.CAT.HEAD verb & [ PART-FORM #prt ] ] >,'
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    mylang.add('part-' + prefix + '-comp-verb-lex := ' + stype + ' & \
     [ SYNSEM [ LOCAL.CAT.VAL.COMPS < [ ' + constraint + \
                'LKEYS.KEY-PART #prt ] ].')
    comps_list.append('part')

###to facilitate incorporation of Cheetah's lexicon

  if 'adp' in comps:
    mylang.add('lexkeys :+ [ KEY-ADP pform ].', section='addenda')
    constraint = 'OPT -, \
              LOCAL.CAT.HEAD.FORM #pform ], ...>,'
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    mylang.add('adp-' + prefix + '-comp-verb-lex := \
     [ SYNSEM [ LOCAL.CAT.VAL.COMPS < [ ' + constraint + \
                'LKEYS.KEY-ADP #pform ] ].')

  my_lists = []
  my_lists.append(comps_list)
#  my_lists.append(adp_forms)
  return my_lists



def create_basic_verb_types(ch, mylang):
  create_subcategorization_values(ch, mylang)
  create_synsem_cross_classifications(ch, mylang)
  stype = main_or_verb(ch)
  subjs = create_basic_subject_values(ch, mylang, stype)
  fcomps = create_some_compl(ch, mylang, stype, '1st')
  scomps = create_some_compl(ch, mylang, stype, '2nd')
  tcomps = create_some_compl(ch, mylang, stype, '3rd')

  if ch.get('subcat-regroup') == 'on':
    pats = create_subcat_with_opt_list(ch) 
  else:
    pats = create_subcat_total_list(ch)

# pats create_subcat_with_opt_list(ch)

  add_pforms_imported_lex(mylang)
 # fourcomps = [] #create_some_compl(ch, mylang, stype, '4th')

  cross_classify_verb_types(mylang, subjs, fcomps, scomps, tcomps, pats)


def combine_opt_features(mylang, bname, npart, constraint, opt_kds, prefix):
  if npart in opt_kds:
    name = 'opt-' + npart + bname
    sname = 'opt-' + bname
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    add_comp_feature(mylang, sname, name, constraint)      
    name = prefix + '-plus-'+ npart + bname
    sname = prefix + '-plus' + bname
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    add_comp_feature(mylang, sname, name, constraint)
  else:
    name = npart + '-' + prefix + bname
    sname = prefix + '-plus' + bname
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    add_comp_feature(mylang, sname, name, constraint)

def add_comp_feature(mylang, stype, name, constraint):
  mylang.add(name + ' := ' + stype + ' & \
    [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + constraint + ' ], ... > ].')


def cross_classify(mylang, prop1, prop2, suffix, arg_list):

  for p1 in prop1:
    for p2 in prop2:
      mylang.add(p1 + '-' + p2 + '-' + suffix + ' := ' + p1 + '-' + suffix + ' & ' + p2 + '-' + suffix + '.')
      arg_list.append(p1 + '-' + p2)
  return arg_list

def cross_classify_verb_types(mylang, slist, fcomps, scomps, tcomps, pats):
  subjs = cross_class_intransitives(mylang, slist)
  s_c1 = cross_class_comps(mylang, subjs, fcomps, pats)
  s_c2 = cross_class_comps(mylang, s_c1, scomps, pats)
  s_c3 = cross_class_comps(mylang, s_c2, tcomps, pats)
# cross_class_comps(mylang, s_c3, lcomps)



###name shortening done above now..., see if can be simplified
def cross_class_intransitives(mylang, slist):
  sbs = []
  mylang.add('basic-intransitive-verb := basic-verb-lex & \
              [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
                                       COMPS < > ], \
                ARG-ST < #subj > ].')
  for s in slist:
#    if '-' in s:
#      sn_ps = s.split('-')
#      sn = sn_ps[0]
#    else:
#      sn = s
#    sbs.append(sn)
#    name = sn + '-intrans-verb-lex'
##### expl
####opt in name sollte auch mitzaehlen
    
    mylang.add(s + '-intrans-verb-lex := ' + s + '-sbj-verb-lex & \
                                             basic-intransitive-verb.') 
    if 'opt' in s:
      mylang.add(s + '-intrans-verb-lex := opt-sbj-verb-lex.')
    else:
      mylang.add(s + '-intrans-verb-lex := obl-sbj-verb-lex.')
 #   if 'expl' in s:
 #     if not 'opt' in s:
 #       mylang.add(s + '-intrans-verb-lex := ' + s + '-sbj-verb-lex & \
 #                                            basic-intransitive-verb.')
 #   else:
 #   #  mylang.add(sn + '-verb-lex := ' + s + '-sbj-verb-lex.')
 #     mylang.add(s + '-intrans-verb-lex := ' + s + '-sbj-verb-lex & \
 #                                            basic-intransitive-verb.') 
    if not 'expl' in s and not 'comp' in s:
      mylang.add(s + '-intrans-verb-lex := intransitive-lex-item.')
  return slist

def cross_class_comps(mylang, fargs, ncomps_nadps, pats):
  ncomps = ncomps_nadps[0]
#  nadps = ncomps_nadps[1]
  mylang.add('basic-transitive-verb := basic-verb-lex & basic-two-arg & \
    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
                             COMPS < #comp > ], \
      ARG-ST < #subj & [ ], #comp & [ ] > ].')

####adding a type for double clausal arguments (comp-comp)

  mylang.add('clausal-1st-2nd-arg-trans-lex-item := basic-two-arg & \
      [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg1 ], \
                 [ LOCAL.CONT.HOOK.LTOP #larg2 ] >, \
        SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg1, LARG #larg1 ], \
                                     qeq & [ HARG #harg2, LARG #larg2 ] !>, \
                 LKEYS.KEYREL [ ARG1 #harg1, \
                                ARG2 #harg2 ] ] ].')

  mylang.add('clausal-first-arg-refl-lex-item := basic-two-arg & \
   [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ], \
	      [ LOCAL refl-local ] >, \
     SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
					  LARG #larg ] !>, \
	      LKEYS.KEYREL [ ARG1 #harg ] ] ].')
  
  mylang.add('expl-refl-arg-lex-item := basic-two-arg-no-hcons & \
  [ ARG-ST < [ LOCAL.CONT.HOOK.INDEX expl-ind ], \
             [ LOCAL refl-local ] > ].')

  mylang.add('expl-refl-arg-verb-lex := expl-refl-arg-lex-item & main-verb-lex.')

  mylang.add('basic-ditransitive-verb := basic-verb-lex & basic-three-arg & \
    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
                             COMPS < #comp1, #comp2 > ], \
      ARG-ST < #subj & [ ], #comp1 & [ ], #comp2 & [ ] > ].')

  mylang.add('clausal-1st-3rd-arg-lex-item := basic-three-arg & \
   [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg1 ], \
	      [ LOCAL.CONT.HOOK.INDEX ref-ind & #ind ], \
	      [ LOCAL.CONT.HOOK.LTOP #larg2 ] >, \
     SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg1, \
                                          LARG #larg1 ], \
                                  qeq & [ HARG #harg2, \
					  LARG #larg2 ] !>, \
	      LKEYS.KEYREL [ ARG1 #harg1, \
                             ARG2 #ind, \
                             ARG3 #harg2 ] ] ].')
  mylang.add('clausal-1st-refl-3rd-arg-lex-item := basic-three-arg & \
   [ ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg1 ], \
	      [ LOCAL refl-local ], \
	      [ LOCAL.CONT.HOOK.LTOP #larg2 ] >, \
     SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg1, \
                                          LARG #larg1 ], \
                                  qeq & [ HARG #harg2, \
					  LARG #larg2 ] !>, \
	      LKEYS.KEYREL [ ARG1 #harg1, \
                             ARG2 #harg2 ] ] ].')

  mylang.add('clausal-3rd-expl-arg-lex-item := basic-three-arg & \
   [ ARG-ST < [ LOCAL.CONT.HOOK.INDEX expl-ind ], \
	      [ LOCAL.CONT.HOOK.INDEX ref-ind & #ind ], \
	      [ LOCAL.CONT.HOOK.LTOP #larg ] >, \
     SYNSEM [ LOCAL.CONT.HCONS <! qeq & [ HARG #harg, \
					  LARG #larg ] !>, \
	      LKEYS.KEYREL [ ARG1 #ind, \
                             ARG2 #harg ] ] ].')

  mylang.add('basic-fourarg-verb := basic-verb-lex & basic-four-arg & \
    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
                             COMPS < #comp1, #comp2, #comp3 > ], \
      ARG-ST < #subj & [ ], #comp1 & [ ], #comp2 & [ ], #comp3 & [ ] > ].')

  mylang.add('basic-fivearg-verb := basic-verb-lex & basic-four-arg & \
    [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < #subj >, \
                             COMPS < #comp1, #comp2, #comp3, [ ] > ], \
      ARG-ST < #subj & [ ], #comp1 & [ ], #comp2 & [ ], #comp3 & [ ] > ].')
  sbj_cps = []

  if len(pats) < 1:
    only_followed_by_part = ["zuinf","comp","verb"]
    for fa in fargs:
      if not 'part' in fa:
        for c in ncomps:
          if '-' in c:
            cn_ps = c.split('-')
            cn = cn_ps[0]
          else:
            cn = c
          parts = fa.split('-') 
          latest = parts[len(parts) - 1]
          if latest in only_followed_by_part:
            if cn == 'part':
              sbj_cps = create_relevant_types(mylang, sbj_cps, c, fa, cn)
 #       elif latest in nadps:
 #         if cn == 'part' or cn in only_followed_by_part:
 #           sbj_cps = create_relevant_types(mylang, sbj_cps, c, fa, cn)
          else:
            sbj_cps = create_relevant_types(mylang, sbj_cps, c, fa, cn)
  else:
    for fa in fargs:
      for c in ncomps:
        if fa + '-' + c in pats:
          sbj_cps = create_relevant_types(mylang, sbj_cps, c, fa, c)

  return sbj_cps


def create_relevant_types(mylang, sbj_cps, c, fa, cn):
  name = fa + '-' + cn
  sbj_cps.append(name)
  if '-' not in fa:
    c += '-1st'
  # default for expl: not optional
  #  if fa == 'expl':
  #    fan = fa + '-obl'
  #  else:
    fan = fa
    mylang.add(name + '-verb-lex := ' + fan + '-sbj-verb-lex & ' + c + '-comp-verb-lex.')
    mylang.add(name + '-trans-verb-lex := ' + name + '-verb-lex & basic-transitive-verb.')
    if 'opt_' in cn:
      mylang.add(name + '-verb-lex := 1st-comp-opt-lex-item.')
    else:
      mylang.add(name + '-verb-lex := comp1-lex-item.')
    if '-comp' in name or '-sent' in name or '-wh' in name or '_comp' in name or '_sent' in name or '_wh' in name:
      if 'comp-' in name:
        mylang.add(name + '-trans-verb-lex := clausal-1st-2nd-arg-trans-lex-item.')
      elif 'expl-' in name:
        mylang.add(name + '-trans-verb-lex := clausal-expl-arg-lex-item.')
      else:
        mylang.add(name + '-trans-verb-lex := clausal-second-arg-trans-lex-item.')
    elif 'expl-' in name:
      if '-refl' in name:
        mylang.add(name + '-trans-verb-lex := expl-refl-arg-verb-lex.')
      else:
        mylang.add(name + '-trans-verb-lex := expl-two-arg-verb-lex.')
    elif 'comp-' in name:
      if '-refl' in name:
        mylang.add(name + '-trans-verb-lex := clausal-first-arg-refl-lex-item.')
      else:
        mylang.add(name + '-trans-verb-lex := clausal-first-arg-trans-lex-item.')
####to-do: all these should get correct supertype
    elif 'part' not in c and 'refl' not in c:
      mylang.add(name + '-trans-verb-lex := transitive-verb-lex.')
  else:
    fas = fa.split('-')
    if len(fas) == 2:
      c += '-2nd'
      mylang.add(name + '-verb-lex := ' + fa + '-verb-lex & ' + c + '-comp-verb-lex.')
      mylang.add(name + '-ditrans-verb-lex := ' + name + '-verb-lex & basic-ditransitive-verb.')
      if 'opt_' in cn:
        mylang.add(name + '-verb-lex := 2nd-comp-opt-lex-item.')
      else:
        mylang.add(name + '-verb-lex := comp2-lex-item.')
      if ('-comp' in name and not 'comp-' in name) or ('-wh' in name and not 'wh-' in name) or ('_comp' in name and not 'comp-' in name) or ('_wh' in name and not 'wh-' in name):
        if 'expl-' in name:  
          mylang.add(name + '-ditrans-verb-lex := clausal-3rd-expl-arg-lex-item.')
#no comp-X-comp in lexicon, but comp-X-wh is found
        elif 'comp-' in name:
          if '-refl-' in name:
            mylang.add(name + '-ditrans-verb-lex := clausal-1st-refl-3rd-arg-lex-item.')
          else:
            mylang.add(name + '-ditrans-verb-lex := clausal-1st-3rd-arg-lex-item.')
        else:
          mylang.add(name + '-ditrans-verb-lex := clausal-third-arg-ditrans-lex-item.')
      elif 'part' not in c and 'refl' not in c and 'expl' not in fa and 'comp' not in fa and 'wh' not in fa:
        mylang.add(name + '-ditrans-verb-lex := ditransitive-lex-item.')
    elif len(fas) == 3:
      c += '-3rd'
      mylang.add(name + '-verb-lex := ' + fa + '-verb-lex & ' + c + '-comp-verb-lex.')
      mylang.add(name + '-4arg-verb-lex := ' + name + '-verb-lex & basic-fourarg-verb.')
      if 'opt_' in cn:
        mylang.add(name + '-verb-lex := 3rd-comp-opt-lex-item.')
      else:
        mylang.add(name + '-verb-lex := comp3-lex-item.')
      if 'part' not in c and 'refl' not in c and 'expl' not in fa and 'comp' not in fa:
        mylang.add(name + '-4arg-verb-lex := basic-fourarg-verb.')
    elif len(fas) == 4:
      c += '-4th'
      mylang.add(name + '-verb-lex := ' + fa + '-verb-lex & ' + c + '-comp-verb-lex.')
      mylang.add(name + '-5arg-verb-lex := ' + name + '-verb-lex & basic-fivearg-verb.')

  return sbj_cps


######################################################################
#
#  List of potential subcatframes
#
######################################################################

def create_subcat_total_list(ch):
  sc_list = []
  if ch.get('subcat_list') == 'tiger-complete':
    sc_list = [ 'comp-adp-zuinf', 'expl-dat-zuinf', 'comp-adp-zuinf-part', 'nom-acc-adp-comp', 'comp-dat-zuinf' , 'expl-acc' , 'comp-dat-comp', 'nom' , 'nom-acc' , 'nom-acc-dat' , 'nom-acc-inf', 'nom-adj' , 'nom-nom' , 'nom-refl-adp' , 'nom-acc-part' , 'nom-comp' , 'nom-sent' , 'nom-wh' , 'nom-zuinf' , 'nom-dat-zuinf' , 'nom-refl' , 'nom-part' , 'nom-refl-part' , 'nom-acc-dat-part' , 'nom-dat-part' , 'nom-refl-dat-part' , 'nom-refl-acc' , 'nom-dat' , 'comp-adp-part' , 'comp-acc-part' , 'comp-part' , 'nom-zuinf-part' , 'nom-adp', 'nom-refl-acc-part' , 'nom-comp-part' , 'expl-part' , 'nom-acc-adp' , 'nom-refl-dat' , 'nom-dat-zuinf-part' , 'nom-sent-part' , 'comp-comp-part' , 'nom-acc-zuinf' , 'nom-refl-sent-part' , 'nom-acc-zuinf-part' , 'nom-acc-gen-part' , 'nom-gen-part' , 'expl-adp-part' , 'expl-dat-part' , 'comp-acc-dat-part' , 'comp-dat-part' , 'comp-refl-acc-part' , 'comp-sent-part' , 'comp-refl-gen-part' , 'nom-refl-gen-part' , 'comp-acc' , 'nom-acc-sent-part' , 'nom-dat-comp-part' , 'nom-adp-sent' , 'nom-adp-zuinf' , 'nom-dat-sent-part' , 'expl-acc-part' , 'expl-acc-adp-part' , 'comp' , 'comp-zuinf-part' , 'comp-acc-adp-part' , 'comp-refl-adp-part' , 'comp-refl-part' , 'nom-acc-comp' , 'comp-comp' , 'comp-zuinf' , 'comp-acc-dat' , 'comp-dat-sent' , 'comp-sent' , 'nom-dat-sent' , 'nom-acc-gen' , 'nom-refl-gen' , 'expl-dat' , 'expl-gen' , 'nom-gen' , 'nom-acc-sent' , 'comp-acc-adp' , 'comp-acc-sent' , 'comp-refl-adp' , 'comp-refl' , 'expl' , 'expl-comp' , 'expl-zuinf' , 'expl-refl' , 'expl-sent' , 'comp-adp-zuinf-part' , 'nom-adp-zuinf-part' , 'comp-dat' , 'nom-refl-comp' , 'nom-refl-sent' , 'nom-acc-adp-sent' , 'nom-acc-dat-adp' , 'nom-dat-adp' , 'comp-adp' , 'nom-dat-comp' , 'expl-acc-adp' , 'expl-acc-dat' , 'expl-acc-sent' , 'expl-adp' , 'expl-dat-adp' , 'nom-refl-comp-part' , 'comp-dat-zuinf-part' , 'comp-refl-comp' , 'comp-refl-sent' , 'nom-adp-comp' , 'comp-adp-comp' , 'dat-comp' , 'comp-refl-acc' , 'dat-zuinf' , 'expl-adp-comp' , 'expl-adp-sent' , 'expl-refl-acc' , 'expl-refl-acc-adp' , 'nom-refl-acc-adp' , 'comp-refl-comp-part' , 'comp-refl-sent-part' , 'expl-acc-gen' , 'dat-zuinf' , 'expl-refl-adp' , 'comp-dat-comp-part' , 'comp-dat-adp' , 'comp-acc-dat-zuinf' , 'comp-refl-acc-sent' , 'expl-acc-dat-zuinf' , 'expl-dat-comp' , 'expl-refl-acc-sent' , 'nom-acc-dat-zuinf' , 'nom-refl-acc-sent' , 'comp-dat-sent-part' , 'comp-refl-dat' , 'comp-acc-comp' , 'comp-adp-sent' , 'comp-acc-zuinf' , 'expl-acc-zuinf' , 'comp-gen' , 'comp-refl-gen' , 'expl-acc-dat-sent' , 'nom-acc-dat-sent' , 'nom-refl-adp-sent' , 'nom-acc-adp-part' , 'nom-adp-part' , 'nom-refl-adp-part' , 'nom-wh-part' , 'nom-refl-wh-part' , 'nom-dat-wh-part' , 'nom-acc-wh', 'comp-wh' , 'nom-adp-wh' , 'comp-acc-wh' , 'comp-dat-wh' , 'nom-dat-wh' , 'comp-refl-wh-part' , 'comp-wh-part' , 'nom-refl-wh' , 'expl-wh' , 'expl-acc-wh' , 'comp-dat-wh-part' , 'comp-refl-wh' , 'comp-adj' , 'comp-adj-acc' , 'comp-nom' , 'comp-nom-acc' , 'expl-adj' , 'expl-adj-acc' , 'expl-nom' , 'expl-nom-acc' , 'nom-adj-acc' , 'nom-nom-acc' , 'nom-adj-part' , 'nom-nom-part' , 'comp-refl-adj-acc' , 'comp-refl-nom-acc' , 'expl-refl-adj-acc' , 'expl-refl-nom-acc' , 'nom-refl-adj-acc' , 'nom-refl-nom-acc' , 'comp-adj-comp' , 'comp-adj-dat' , 'comp-adj-sent' , 'comp-nom-comp' , 'comp-nom-dat' , 'comp-nom-sent' , 'comp-refl-adj' , 'comp-refl-nom' , 'expl-adj-comp' , 'expl-adj-dat' , 'expl-adj-sent' , 'expl-nom-comp' , 'expl-nom-dat' , 'expl-nom-sent' , 'expl-refl-adj' , 'expl-refl-nom' , 'nom-adj-comp' , 'nom-adj-dat' , 'nom-adj-sent' , 'nom-nom-comp' , 'nom-nom-dat' , 'nom-nom-sent' , 'nom-refl-adj' , 'nom-refl-nom']

  return sc_list


def create_subcat_with_opt_list(ch):
  sc_list = []
  if ch.get('subcat_list') == 'tiger-complete':
    sc_list = ['expl-acc','nom-acc-inf','nom-acc','nom-opt_adp', 'nom-opt_acc' , 'nom-opt_refl-opt_gen', 'nom-refl-opt_gen', 'nom-opt_refl', 'nom-acc-opt_dat', 'nom-opt_refl-acc', 'nom-opt_acc-adp', 'nom-opt_acc-opt_adp', 'nom-opt_acc-opt_sent', 'nom-opt_refl-adp', 'nom-opt_refl-opt_adp', 'nom-refl-opt_adp', 'nom-opt_refl-opt_sent', 'nom-refl-opt_sent', 'expl-opt_adp', 'nom-opt_acc-part', 'nom-opt_adp-part', 'nom-opt_refl-adp-part', 'nom-opt_refl-opt_adp-part', 'nom-refl-opt_adp-part', 'nom-opt_refl-part', 'nom-opt_zuinf', 'expl-opt_acc', 'expl-opt_dat', 'nom-opt_dat', 'nom-opt_sent', 'nom-acc-opt_adp', 'nom-adp-opt_comp', 'nom-opt_adp-comp', 'nom-opt_adp-opt_comp', 'nom-adp-opt_wh', 'nom-opt_adp-opt_wh', 'nom-opt_adp-wh', 'nom-opt_comp', 'nom-opt_wh', 'expl-acc-opt_dat', 'expl-opt_acc-dat', 'expl-opt_acc-opt_dat', 'expl-acc-opt_sent', 'expl-opt_acc-opt_sent', 'nom-opt_acc-dat', 'nom-opt_acc-opt_dat', 'nom-acc-opt_sent', 'expl-acc-opt_adp', 'expl-opt_acc-adp', 'expl-opt_acc-opt_adp', 'expl-adp-opt_comp', 'expl-opt_adp-comp', 'expl-opt_adp-opt_comp', 'expl-adp-opt_sent', 'expl-opt_adp-opt_sent', 'expl-opt_adp-sent', 'expl-opt_comp', 'expl-opt_refl-acc-adp', 'expl-opt_refl-acc-opt_adp', 'expl-opt_refl-opt_acc-adp', 'expl-opt_refl-opt_acc-opt_adp', 'expl-refl-acc-opt_adp', 'expl-refl-opt_acc-opt_adp', 'expl-opt_refl-acc', 'expl-opt_refl-opt_acc','expl-refl-opt_acc', 'expl-opt_refl', 'expl-opt_sent','expl-opt_zuinf','nom-opt_refl-acc-adp','nom-opt_refl-acc-opt_adp', 'nom-opt_refl-opt_acc-adp', 'nom-opt_refl-opt_acc-opt_adp', 'nom-refl-acc-opt_adp','nom-refl-opt_acc-opt_adp','nom-opt_refl-opt_acc', 'nom-refl-opt_acc','nom-opt_dat-part','nom-dat-opt_sent', 'nom-opt_dat-opt_sent', 'comp-comp-part','comp-opt_refl-opt_gen-part','comp-opt_sent-part','opt_nom-comp-part', 'nom-opt_comp-part', 'nom-opt_refl-opt_gen-part','nom-opt_sent-part', 'comp-opt_acc', 'comp-opt_dat', 'comp-acc-opt_dat', 'comp-opt_acc-opt_dat', 'comp-comp','comp-opt_refl','comp-opt_sent','opt_nom-comp', 'nom-acc-opt_dat-part', 'nom-opt_acc-opt_dat-part', 'nom-dat-opt_zuinf', 'nom-opt_dat-opt_zuinf', 'nom-opt_dat-zuinf', 'comp-opt_wh', 'nom-opt_acc-dat-part', 'nom-dat-opt_comp-part', 'nom-opt_adj', 'nom-nom', 'nom-refl-opt_acc-part','nom-refl-opt_dat','nom-acc-opt_zuinf','nom-opt_acc-opt_zuinf','nom-opt_acc-zuinf','nom-adp-opt_zuinf','nom-opt_adp-opt_zuinf','nom-opt_adp-zuinf','nom-opt_refl-dat-part', 'comp-acc-opt_sent', 'comp-opt_acc-opt_sent', 'comp-opt_zuinf', 'opt_expl-comp', 'expl-opt_refl-adp', 'expl-opt_refl-opt_adp', 'expl-refl-opt_adp', 'nom-acc-opt_sent-part', 'nom-opt_acc-sent-part', 'nom-acc-opt_gen', 'nom-opt_adj-acc', 'nom-opt_adj-opt_acc', 'nom-nom-acc', 'nom-nom-opt_acc', 'comp-acc-opt_adp','comp-opt_acc-adp','comp-opt_acc-opt_adp', 'comp-opt_adp', 'nom-acc-opt_adp-part', 'nom-opt_acc-adp-part', 'nom-opt_acc-opt_adp-part', 'comp-adj-opt_dat', 'comp-opt_adj-dat', 'comp-opt_adj-opt_dat', 'comp-opt_adj', 'comp-opt_nom-dat', 'comp-opt_nom-opt_dat', 'comp-nom-opt_dat', 'opt_comp-nom-dat', 'opt_comp-nom-opt_dat', 'comp-opt_nom', 'opt_comp-nom','comp-opt_refl-adj', 'comp-opt_refl-opt_adj', 'comp-opt_refl-opt_nom', 'comp-opt_refl-nom', 'opt_comp-opt_refl-nom', 'expl-adj-opt_dat', 'expl-opt_adj-dat', 'expl-opt_adj-opt_dat', 'expl-opt_adj', 'expl-opt_nom-dat', 'expl-opt_nom-opt_dat', 'expl-nom-opt_dat', 'opt_expl-nom-dat','opt_expl-nom-opt_dat','expl-opt_nom','opt_expl-nom','expl-opt_refl-adj','expl-opt_refl-opt_adj','expl-opt_refl-opt_nom','expl-opt_refl-nom','opt_expl-opt_refl-nom','nom-adj-opt_dat','nom-opt_adj-dat','nom-opt_adj-opt_dat', 'nom-nom-dat','nom-nom-opt_dat','nom-opt_refl-adj','nom-opt_refl-opt_adj', 'nom-opt_refl-nom','comp-acc-opt_zuinf','comp-opt_acc-opt_zuinf','comp-opt_acc-zuinf','comp-opt_dat-comp','comp-dat-comp','comp-opt_dat-opt_zuinf','comp-dat-opt_zuinf','comp-opt_dat-zuinf','expl-acc-opt_zuinf','expl-opt_acc-opt_zuinf','expl-opt_acc-zuinf', 'opt_expl-opt_dat-comp','opt_expl-dat-comp','expl-opt_dat-comp','expl-dat-opt_comp','expl-opt_dat-opt_comp','expl-dat-opt_zuinf','expl-opt_dat-opt_zuinf','expl-opt_dat-zuinf','opt_nom-opt_dat-comp','opt_nom-dat-comp','nom-opt_dat-comp','nom-dat-opt_comp','nom-opt_dat-opt_comp','nom-opt_dat-adp','nom-opt_dat-opt_adp','nom-opt_dat-sent','nom-opt_dat-opt_wh','nom-opt_dat-wh','comp-opt_acc-dat','comp-adp-opt_sent','comp-opt_adp-opt_sent','comp-opt_adp-sent','opt_nom-adp-comp','opt_nom-opt_adp-comp','nom-opt_acc-sent','nom-opt_refl-acc-part', 'nom-opt_zuinf-part','comp-opt_adp-comp','comp-opt_refl-opt_sent','comp-refl-opt_sent','comp-opt_refl-sent','nom-opt_refl-sent','nom-opt_adj-part','nom-nom-part','nom-opt_dat-opt_wh','nom-opt_dat-wh-part','nom-opt_wh-part','nom-acc-opt_comp', 'nom-opt_acc-opt_comp','expl-opt_dat-adp','expl-dat-opt_adp','expl-opt_dat-opt_adp','nom-dat-opt_adp','nom-acc-opt_zuinf-part','nom-opt_acc-opt_zuinf-part','nom-opt_acc-zuinf-part','nom-refl-opt_comp','comp-acc-opt_dat-part', 'comp-opt_acc-opt_dat-part','comp-opt_dat-comp-part','comp-opt_dat-opt_sent-part','comp-opt_dat-sent-part','opt_nom-opt_dat-comp-part','nom-opt_dat-comp-part','nom-opt_dat-opt_comp-part','nom-opt_dat-opt_sent-part', 'nom-opt_dat-sent-part', 'nom-dat-opt_sent-part', 'nom-dat-opt_zuinf-part', 'comp-opt_acc-dat-part', 'comp-opt_dat-part', 'comp-opt_dat-opt_zuinf-part', 'comp-dat-opt_zuinf-part', 'comp-opt_dat-zuinf-part', 'comp-opt_zuinf-part', 'nom-opt_dat-opt_zuinf-part', 'nom-opt_dat-zuinf-part','comp-refl-comp-part', 'opt_nom-refl-comp-part', 'nom-refl-opt_comp-part', 'comp-refl-opt_adp-part', 'expl-opt_acc-sent', 'expl-opt_wh', 'comp-opt_adj-acc', 'comp-adj-opt_acc', 'comp-opt_adj-opt_acc', 'comp-opt_nom-acc','comp-opt_nom-opt_acc','comp-nom-opt_acc','opt_comp-nom-acc','opt_comp-nom-opt_acc','expl-opt_adj-acc','expl-adj-opt_acc','expl-opt_adj-opt_acc','expl-opt_nom-acc','expl-opt_nom-opt_acc','expl-nom-opt_acc','opt_expl-nom-acc','opt_expl-nom-opt_acc','nom-adj-opt_acc','comp-opt_acc-sent','nom-opt_refl-opt_dat','expl-opt_adp-part','expl-opt_dat-part','nom-opt_refl-opt_acc-part','nom-opt_refl-comp','nom-opt_refl-opt_comp','nom-acc-opt_dat-adp','nom-acc-opt_dat-opt_adp','nom-opt_acc-opt_dat-adp','nom-opt_acc-dat-adp','nom-opt_acc-dat-opt_adp','nom-opt_acc-opt_dat-opt_adp','comp-opt_refl-dat', 'nom-opt_refl-dat','comp-opt_adp-part','expl-opt_acc-adp-part','expl-opt_acc-opt_adp-part','nom-opt_gen','comp-opt_refl-opt_adp','comp-refl-opt_adp','nom-opt_refl-comp-part','nom-opt_refl-opt_comp-part','nom-refl-adp-opt_sent', 'nom-refl-opt_adp-opt_sent','nom-refl-opt_dat-part', 'comp-acc-comp', 'comp-opt_acc-comp', 'comp-opt_refl-adp','comp-opt_refl-comp', 'opt_nom-acc-comp', 'opt_nom-opt_acc-comp', 'nom-opt_acc-comp','opt_nom-opt_refl-comp', 'nom-acc-opt_wh','nom-opt_acc-opt_wh','nom-opt_acc-wh','nom-opt_refl-opt_wh', 'nom-refl-opt_wh', 'nom-opt_refl-wh', 'comp-opt_refl-acc-part', 'comp-opt_refl-opt_acc-part', 'comp-opt_dat-adp', 'comp-dat-opt_adp', 'comp-opt_dat-opt_adp', 'expl-acc-dat-opt_sent', 'expl-acc-opt_dat-opt_sent', 'nom-acc-dat-opt_sent', 'nom-acc-opt_dat-opt_sent','comp-opt_refl-part','nom-acc-opt_adp-opt_comp','nom-opt_acc-opt_adp-opt_comp', 'comp-opt_dat-opt_wh', 'comp-opt_dat-wh','comp-opt_refl-acc','comp-opt_refl-opt_acc','comp-refl-opt_acc','expl-acc-opt_gen','expl-opt_acc-opt_gen','nom-opt_acc-opt_gen', 'comp-adj-comp', 'comp-opt_adj-comp', 'comp-adj-opt_sent', 'comp-opt_adj-opt_sent', 'comp-opt_adj-sent','comp-opt_gen', 'comp-opt_nom-comp','comp-nom-comp','opt_comp-nom-opt_comp','comp-opt_nom-opt_sent','comp-nom-opt_sent', 'comp-opt_nom-sent', 'opt_comp-nom-opt_sent','opt_comp-nom-sent','comp-refl-opt_adj','comp-refl-opt_nom','opt_comp-refl-nom','opt_expl-adj-comp','opt_expl-opt_adj-comp','expl-adj-opt_comp','expl-opt_adj-comp','expl-opt_adj-opt_comp', 'expl-adj-opt_sent', 'expl-opt_adj-opt_sent','expl-opt_adj-sent', 'expl-opt_gen', 'opt_expl-opt_nom-comp', 'opt_expl-nom-comp','expl-opt_nom-comp', 'expl-opt_nom-opt_comp','expl-nom-opt_comp', 'opt_expl-nom-opt_comp','expl-opt_nom-opt_sent','expl-nom-opt_sent','expl-opt_nom-sent','opt_expl-nom-opt_sent','opt_expl-nom-sent','expl-refl-opt_adj','expl-refl-opt_nom','opt_expl-refl-nom','opt_nom-adj-comp','opt_nom-opt_adj-comp','nom-opt_adj-comp','nom-adj-opt_comp','nom-opt_adj-opt_comp','nom-adj-opt_sent','nom-opt_adj-opt_sent','nom-opt_adj-sent','opt_nom-opt_nom-comp','nom-nom-comp','nom-nom-opt_comp','nom-nom-opt_sent','nom-nom-sent','nom-refl-opt_adj','nom-refl-nom','comp-acc-opt_adp-part','comp-opt_acc-adp-part','comp-opt_acc-opt_adp-part','comp-opt_refl-adp-part','comp-opt_refl-opt_adp-part','comp-adp-opt_zuinf-part','comp-opt_adp-opt_zuinf','nom-adp-opt_zuinf-part','nom-opt_adp-opt_zuinf-part','nom-opt_refl-opt_dat-part','nom-opt_acc-gen','nom-opt_refl-opt_sent-part','nom-refl-opt_sent-part', 'nom-opt_refl-opt_wh-part', 'nom-refl-opt_wh-part','comp-opt_refl-opt_dat','expl-opt_acc-gen','nom-acc-opt_gen-part','nom-opt_acc-gen-part','nom-opt_acc-opt_gen-part','nom-opt_gen-part','expl-opt_acc-part','comp-opt_refl-comp-part','comp-opt_refl-opt_sent-part','comp-refl-opt_sent-part','comp-opt_refl-opt_wh-part','comp-refl-opt_wh-part','comp-opt_refl-wh-part','comp-opt_wh-part','opt_nom-opt_refl-comp-part','nom-opt_refl-wh-part','expl-acc-opt_wh','expl-opt_acc-opt_wh','nom-opt_acc-opt_sent-part','nom-opt_refl-gen','comp-dat-opt_sent-part','comp-acc-opt_wh','comp-opt_acc-opt_wh','comp-opt_acc-wh','comp-refl-comp','opt_nom-refl-comp', 'comp-acc-dat-opt_zuinf', 'comp-acc-opt_dat-opt_zuinf','comp-opt_acc-opt_dat-opt_zuinf', 'comp-opt_acc-dat-opt_zuinf', 'comp-opt_acc-opt_dat-zuinf', 'comp-opt_refl-acc-opt_sent', 'comp-opt_refl-opt_acc-opt_sent', 'comp-refl-acc-opt_sent', 'comp-refl-opt_acc-opt_sent', 'comp-opt_refl-opt_acc-sent', 'comp-opt_refl-opt_adj-acc','comp-opt_refl-adj-acc','comp-opt_refl-opt_adj-opt_acc','comp-refl-opt_adj-acc','comp-refl-opt_adj-opt_acc','comp-opt_refl-opt_nom-acc','comp-opt_refl-opt_nom-opt_acc','comp-opt_refl-nom-acc','comp-refl-opt_nom-acc','comp-refl-opt_nom-opt_acc','opt_comp-opt_refl-nom-acc','comp-opt_refl-nom-opt_acc','opt_comp-opt_refl-nom-opt_acc', 'opt_comp-refl-nom-acc', 'opt_comp-refl-nom-opt_acc', 'expl-acc-dat-opt_zuinf', 'expl-acc-opt_dat-opt_zuinf', 'expl-opt_acc-dat-opt_zuinf','expl-opt_acc-opt_dat-opt_zuinf', 'expl-opt_acc-opt_dat-zuinf', 'expl-opt_refl-acc-opt_sent', 'expl-opt_refl-opt_acc-opt_sent', 'expl-refl-acc-opt_sent','expl-refl-opt_acc-opt_sent','expl-opt_refl-opt_acc-sent', 'expl-opt_refl-opt_adj-acc','expl-opt_refl-adj-acc','expl-opt_refl-opt_adj-opt_acc','expl-refl-opt_adj-acc','expl-refl-opt_adj-opt_acc','expl-opt_refl-opt_nom-acc','expl-opt_refl-opt_nom-opt_acc','expl-opt_refl-nom-acc','expl-refl-opt_nom-acc', 'expl-refl-opt_nom-opt_acc', 'opt_expl-opt_refl-nom-acc', 'opt_expl-opt_refl-nom-opt_acc', 'opt_expl-refl-nom-acc', 'opt_expl-refl-nom-opt_acc', 'nom-acc-dat-opt_zuinf', 'nom-acc-opt_dat-opt_zuinf', 'nom-opt_acc-dat-opt_zuinf', 'nom-opt_acc-opt_dat-opt_zuinf', 'nom-opt_acc-opt_dat-zuinf', 'nom-opt_refl-acc-opt_sent', 'nom-opt_refl-opt_acc-opt_sent','nom-refl-acc-opt_sent', 'nom-refl-opt_acc-opt_sent', 'nom-opt_refl-opt_acc-sent', 'nom-opt_refl-opt_adj-acc', 'nom-opt_refl-adj-acc', 'nom-opt_refl-opt_adj-opt_acc', 'nom-refl-opt_adj-acc', 'nom-refl-opt_adj-opt_acc', 'nom-opt_refl-nom-acc', 'nom-opt_refl-nom-opt_acc', 'nom-refl-nom-acc', 'nom-refl-nom-opt_acc', 'comp-dat-opt_wh', 'nom-dat-opt_wh']

  return sc_list

def add_pforms_imported_lex(mylang):

  mylang.add('auf-ueber := pform.',section='features')
  mylang.add('auf := auf-ueber.',section='features')
  mylang.add('ueber := auf-ueber.',section='features')
  mylang.add('von := pform.',section='features')
  mylang.add('um := pform.',section='features')
  mylang.add('aus := pform.',section='features')
  mylang.add('an := pform.',section='features')
  mylang.add('mit := pform.',section='features')
  mylang.add('fuer := pform.',section='features')
  mylang.add('zu := pform.',section='features')
  mylang.add('unter := pform.',section='features')
  mylang.add('nach := pform.',section='features')
  mylang.add('bis := pform.',section='features')
  mylang.add('vor := pform.',section='features')
  mylang.add('bei := pform.',section='features')
  mylang.add('in := pform.',section='features')
  mylang.add('gegen := pform.',section='features')
  mylang.add('ohne := pform.',section='features')
  mylang.add('andie := pform.',section='features')
  mylang.add('trotz := pform.',section='features')
  mylang.add('durch := pform.',section='features')
  mylang.add('als := pform.',section='features')


def interpret_verb_valence(valence):
  '''
  Return the canonical valence name (e.g. iverb, tverb) given the
  valence for a verb as defined in a choices file.
  '''
  if '-' in valence:
    v = valence.split('-')
    if 'refl' in v:
      return 'reflverb'
    elif len(v) == 2:
      if v[1] == 'scomp':
        return 'sc2verb'
      elif v[1] == 'inf' or v[1] == 'zuinf':
        return 'scontrverb'
      else:
        return 'tverb'
    elif len(v) == 3:
      if v[2] == 'inf':
        return 'oraisverb'
      else:
        return 'dverb'
  elif valence == 'trans':
    return 'tverb'
  elif valence == 'ditrans':
    return 'dverb'
  elif valence == 'scomp':
    return 'sc2verb'
  elif valence == 'scontr':
    return 'scontrverb'
  elif valence == 'orais':
    return 'oraisverb'
  else:
    return 'iverb'


#################################
#
# Predicative verbs:
#
# pred(x), if semantically empty verb (i.e. "to be")
# verb(x), pred(x), if semantic content verb
# create category 'predicative' with accordingly corrected semantics
# 


############################################################
#
# previous functions for subcategorization requirements
#
#






def customize_verb_case(mylang, ch):
  cm = ch.get('case-marking')
  cases = case.case_names(ch)

  # Pass through the list of case-marking patterns.  If a pattern is a
  # lexical pattern (i.e. the third item in the list is False), then
  # create and contrain the appropriate lexical type.  This type is a
  # subtype of either transitive-verb-lex or intransitive-verb-lex.
  #
  # Note: I specify ARG-ST.FIRST... below instead of ARG-ST < [], ...>
  # because TDLFile has trouble with merges and open-ended lists.
  # Which should get fixed...  - sfd

  for p in ch.patterns():
    rule_pattern = p[2]

    p = p[0].split(',')  # split off ',dirinv', if present
    dir_inv = ''
    if len(p) > 1 and p[1] == 'dirinv':
      dir_inv = 'dir-inv-'
    
    subjcontr = False
    if p[0] == 'scontr':
      subjcontr = True

    if not rule_pattern:
      c = p[0].split('-')  # split 'agentcase-patientcase'

      if len(c) == 2:
        if (c[1] == 'inf' or c[1] == 'zuinf'):
          subjcontr = True

      if p[0] == 'scomp' or (len(c) == 2 and c[1] == 'scomp'):
        if p[0] == 'scomp':
          a_case = ''
          a_head = ch.case_head()
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          a_head = ch.case_head(c[0])
                               
        if a_case:
          t_type = a_case + '-scomp-transitive-verb-lex'
        else:
          t_type = 'scomp-transitive-verb-lex'                      
        mylang.add(t_type + ' := s-comp-2nd-arg-verb-lex.')

        # constrain the head of the agent/subject
        typedef = \
          t_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)

####subj control verbs: TODO make better interface and restrictions
  
      elif subjcontr:
#p[0] == 'scontr' or (len(c) == 2 and (c[1] == 'inf' or c[1] == 'zuinf')):
        if p[0] == 'scontr':
          a_case = ''
          a_head = ch.case_head()
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          a_head = ch.case_head(c[0])
          b_res = c[1]                        
        
        if a_case:
          t_type = a_case + '-' + b_res + '-subj-contr-transitive-verb-lex'
          if b_res == 'inf':
            b_res += 'initive'
#HACK to get old choices files to work with addition of zuinf
          elif b_res == 'zuinf':
            mylang.add('zuinf := nonfinite.')
        else:
          t_type = 'subj-contr-transitive-verb-lex'                      
        mylang.add(t_type + ' := subj-contr-transitive-verb-lex.')

        # constrain the head of the agent/subject
        if ch.get('vc-analysis') != 'aux-rule':
          typedef = \
            t_type + ' := \
            [ ARG-ST < [ LOCAL.CAT.HEAD ' + a_head + ' ], \
                       [ LOCAL.CAT.HEAD verb  & [ FORM ' + b_res + ' ] ] > ].'
        else:
          typedef = \
            t_type + ' := \
            [ ARG-ST < [ LOCAL.CAT.HEAD verb & [ FORM ' + b_res + ' ] ] > ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case and not ch.get('vc-analysis') == 'aux-rule':
          typedef = \
            t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)

      elif p[0] == 'expl' or (len(c) == 2 and c[0] == 'expl'):      
###skipping just 'expl' scenario for now  
        if len(c) == 2:
          a_case = case.canon_to_abbr(c[0], cases)
          o_case = case.canon_to_abbr(c[1], cases)
          a_head = ch.case_head(c[0])
          o_head = ch.case_head(c[1])
        if a_case and o_case:
          t_type = dir_inv + a_case + '-' + o_case + '-transitive-verb-lex'
        else:
          t_type = dir_inv + 'transitive-verb-lex'
        mylang.add(t_type + ' := expl-two-arg-verb-lex & \
                      [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comp >, \
                        ARG-ST < [ ], #comp & [ ] > ].')
###a_case value is not expl, but nom for Germanic
        a_case = 'nom'  
 # constrain the head of the agent/subject
        typedef = \
          t_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)
# constrain the head of the patient/object
        typedef = \
          t_type + ' := \
          [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
        mylang.add(typedef)

        # constrain the case of the patient/object
        if o_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ] > ].'
          mylang.add(typedef)

      elif p[0] == 'trans' or len(c) == 2:  # transitive
        if p[0] == 'trans':
          a_case = ''
          o_case = ''
          a_head = ch.case_head()
          o_head = ch.case_head()
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          o_case = case.canon_to_abbr(c[1], cases)
          a_head = ch.case_head(c[0])
          o_head = ch.case_head(c[1])

        if a_case and o_case:
          t_type = dir_inv + a_case + '-' + o_case + '-transitive-verb-lex'
        else:
          t_type = dir_inv + 'transitive-verb-lex'

        if t_type != 'transitive-verb-lex':
          mylang.add(t_type + ' := transitive-verb-lex.')

        # constrain the head of the agent/subject
        typedef = \
          t_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)

        # constrain CASE-MARKING of the agent/subject, if appropriate
        if a_case and ch.has_mixed_case() and not ch.has_optadp_case(a_case):
          typedef = \
            t_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
          mylang.add(typedef)

        # constrain the head of the patient/object
        typedef = \
          t_type + ' := \
          [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
        mylang.add(typedef)

        # constrain the case of the patient/object
        if o_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ] > ].'
          mylang.add(typedef)

        # constrain CASE-MARKING of the patient/object, if appropriate
        if o_case and ch.has_mixed_case() and not ch.has_optadp_case(o_case):
          typedef = \
            t_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
          mylang.add(typedef)

      elif p[0] == 'oraisverb' or (len(c) == 3 and c[2] == 'inf'):
        if p[0] == 'oraisverb':
          a_case = ''
          a_head = ch.case_head()
          o_case = ''
          o_head = ch.case_head()
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          a_head = ch.case_head(c[0])
          o_case = case.canon_to_abbr(c[1], cases)
          o_head = ch.case_head(c[1])
          b_res = c[2]                        
        
        if a_case and o_case:
          t_type = a_case + '-' + o_case + '-' + b_res + '-obj-raising-ditransitive-verb-lex'
          b_res += 'initive'
        else:
          t_type = 'obj-rais-ditrans-verb-lex'                      
        mylang.add(t_type + ' := obj-raising-verb-lex.')

        # constrain the head of the agent/subject
        
        typedef = \
          t_type + ' := \
          [ ARG-ST < [ LOCAL.CAT.HEAD ' + a_head + ' ], \
                     [ LOCAL.CAT.HEAD ' + o_head + ' ], \
                     [ LOCAL.CAT.HEAD verb  & [ FORM ' + b_res + ' ] ] > ].'

      ##no distinction in basic arg-str between auxrule and basic
      ##subject is not raised, object is restricted enough 
      # else:
      #    typedef = \
      #      t_type + ' := \
      #      [ ARG-ST < [ LOCAL.CAT.HEAD ' + o_head + ' ], \
      #                 [ LOCAL.CAT.HEAD verb  & [ FORM ' + b_res + ' ] ] > ].'

        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
            t_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)
        if o_case:
          typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ], [ ] > ].'
          mylang.add(typedef)

      elif p[0] == 'refl' or (len(c) == 3 and c[1] == 'refl'):   #reflexive
        if p[0] == 'refl':
          a_case = ''
          b_case = ''
          o_case = ''
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          b_case = case.canon_to_abbr(c[1], cases)
          o_case = case.canon_to_abbr(c[2], cases)

        a_head = 'noun'
        b_head = 'noun'
        o_head = 'noun'
        if o_case == 'pp':
          o_head = 'adp'

        if a_case and b_case and o_case:
          t_type = dir_inv + a_case + '-' + b_case + '-' + o_case + '-transitive-verb-lex'
        else:
          t_type = dir_inv + 'refl-trans-verb-lex'
  # constrain the head of the agent/subject
        typedef = \
            t_type + ' := \
             [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
             t_type + ' := \
               [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)

        # second argument get correct properties from refl-verb-lex
        mylang.add(t_type + ' := refl-verb-lex.')

        # constrain the head of the patient/object
        typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
        mylang.add(typedef)

        # constrain the case of the patient/object
        if not o_case == 'pp':
          typedef = \
            t_type + ' := \
                [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ] > ].'
          mylang.add(typedef)

       ####adding some properties to get the semantics right
        typedef = \
          t_type + ' := trans-first-arg-raising-lex-item & \
               [ SYNSEM [ LOCAL.CAT.VAL.COMPS < [ ], #comp2 & \
                                      [ LOCAL.CONT.HOOK.INDEX #arg2 ] >, \
                          LKEYS.KEYREL [ ARG1 #arg1, \
                                         ARG2 #arg2 ] ], \
                 ARG-ST < [ LOCAL.CONT.HOOK.INDEX #arg1 ], \
                              #comp2 > ].'
        mylang.add(typedef)

      elif p[0] == 'ditrans' or p[0] == 'partv' or len(c) == 3: #ditrans 

        if p[0] == 'ditrans':
          a_case = ''
          b_case = ''
          o_case = ''
        elif p[0] == 'partv':
          a_case = ''
          b_case = ''
          o_case = ''
        else:
          a_case = case.canon_to_abbr(c[0], cases)
          b_case = case.canon_to_abbr(c[1], cases)
          o_case = case.canon_to_abbr(c[2], cases)
        
        a_head = 'noun'
        b_head = 'noun'
###should allow to create list of particles and check if is in list
###rather than specific form
        if p[0] == 'partv' or o_case == 'vor' or o_case == 'voor':
          o_head = 'verb'
        else:
          o_head = 'noun'
        if o_head == 'noun':
          if a_case and b_case and o_case:
            t_type = dir_inv + a_case + '-' + b_case + '-' + o_case + '-ditransitive-verb-lex'
          else:
            t_type = dir_inv + 'ditransitive-verb-lex'
          if t_type != 'ditransitive-verb-lex':
            mylang.add(t_type + ' := ditransitive-verb-lex.')
        else:
          if a_case and b_case and o_case:
            t_type = dir_inv + a_case + '-' + b_case + '-' + o_case + '-part-transitive-verb-lex'
          else:
            t_type = dir_inv + 'part-transitive-verb-lex'
          if t_type != 'part-transitive-verb-lex':
            mylang.add(t_type + ' := part-transitive-verb-lex.')

        typedef = \
            t_type + ' := \
             [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + a_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the agent/subject
        if a_case:
          typedef = \
             t_type + ' := \
               [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + a_case + ' ].'
          mylang.add(typedef)
       
        if o_head == 'noun':   # constrain the head of the agent/subject
         
        # constrain the head of the beneficiary
          typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + b_head + ' ], [ ] > ].'
          mylang.add(typedef)

        # constrain the case of the patient/object
          if b_case:
            typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + b_case + ' ], [ ] > ].'
            mylang.add(typedef)

        # constrain the head of the patient/object
          typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ ], [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
          mylang.add(typedef)

        # constrain the case of the patient/object
          if o_case:
            typedef = \
              t_type + ' := \
               [ ARG-ST < [ ], [ ], [ LOCAL.CAT.HEAD.CASE ' + o_case + ' ] > ].'
            mylang.add(typedef)
        else:  

        # constrain the head of the beneficiary
          typedef = \
            t_type + ' := \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD ' + b_head + ' ] > ].'
          mylang.add(typedef)

        # constrain the case of the patient/object
          if b_case:
            typedef = \
              t_type + ' := \
                [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.CASE ' + b_case + ' ] > ].'
            mylang.add(typedef)

# constrain the head of the patient/object
          typedef = \
            t_type + ' := \
              [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], \
                                             [ LOCAL.CAT.HEAD ' + o_head + ' ] > ].'
          mylang.add(typedef)
          if o_case:
            o_case += '-part'
            typedef = \
              t_type + ' := \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], \
                                     [ LOCAL.CAT.HEAD.FORM ' + o_case + ' ] > ].'
            mylang.add(o_case + ' := form.')
 
          mylang.add(typedef)


      else:     # intransitive
        if c[0] == 'intrans':
          s_case = ''
          s_head = ch.case_head()
        else:
          s_case = case.canon_to_abbr(c[0], cases)
          s_head = ch.case_head(c[0])

        if s_case:
          i_type = dir_inv + s_case + '-intransitive-verb-lex'
        else:
          i_type = dir_inv + 'intransitive-verb-lex'

        if i_type != 'intransitive-verb-lex':
          mylang.add(i_type + ' := intransitive-verb-lex.')

        # constrain the head of the subject
        typedef = \
          i_type + ' := \
          [ ARG-ST.FIRST.LOCAL.CAT.HEAD ' + s_head + ' ].'
        mylang.add(typedef)

        # constrain the case of the subject
        if s_case:
          typedef = \
            i_type + ' := \
            [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + s_case + ' ].'
          mylang.add(typedef)

        # constrain CASE-MARKING of the subject, if appropriate
        if s_case and ch.has_mixed_case() and not ch.has_optadp_case(s_case):
          typedef = \
            i_type + ' := \
            [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE-MARKED + ] > ].'
          mylang.add(typedef)



def create_some_compl_old(ch, mylang, stype, prefix):
  myc = prefix + '_comp_'
  heads = ch.get(myc + 'head',[])
  cases = ch.get(myc + 'case',[])  
  forms = ch.get(myc + 'form',[])
  refl = ch.get(myc + 'refl')
  part = ch.get(myc + 'part')
  opts = ch.get(myc + 'opt',[])
# for now only one val variation, change if needed
  vals = ch.get(myc + 'val',[])
  arg_ana = ch.get('adp-arg-analysis')

  comps_list = [] 
  sd_val = '[ SUBJ < >, \
              COMPS < >, \
              SPR < >, \
              SPEC < > ]'
  val_constr = 'LOCAL.CAT.VAL ' + sd_val
  if prefix != '1st':
    val_constr = '], [ ' + val_constr
    if prefix != '2nd':
      val_constr = '], [ ' + val_constr
      if prefix != '3rd':
        val_constr = '], [ ' + val_constr
  
#  mylang.add('std-val-' + prefix + '-comp' + stype + ' := ' + stype + ' & \
#            [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + val_constr + ' ], ... > ].')
  mylang.add(prefix + '-comp-verb-lex := ' + stype + ' & \
            [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + val_constr + ' ], ... > ].')
  opt_kds = []
  if opts:
    for opt in opts:
      opt_kds = opt.get('kind')
      name = 'opt-' + prefix + '-comp-verb-lex'
      constraint = 'OPT +'
      if prefix != '1st':
        constraint = '], [ ' + constraint
        if prefix != '2nd':
          constraint = '], [ ' + constraint
          if prefix != '3rd':
            constraint = '], [ ' + constraint
      cur_stype = prefix + '-comp-verb-lex'
      add_comp_feature(mylang, cur_stype, name, constraint)
      name = prefix + '-plus-comp-verb-lex'
      constraint = 'OPT -'
      if prefix != '1st':
        constraint = '], [ ' + constraint
        if prefix != '2nd':
          constraint = '], [ ' + constraint
          if prefix != '3rd':
            constraint = '], [ ' + constraint
      add_comp_feature(mylang, cur_stype, name, constraint)
        
  if vals:
    for val in vals:
      kind = val.get('kind')
      if kind == 'subj':
        myv = 'LOCAL.CAT.VAL [ SUBJ < [ ] >, \
                             COMPS < >, \
                             SPEC < >, \
                             SPR < > ], \
               OPT -'
        my_loc = ''
        if prefix != '1st':
          my_loc = '], [ '
          if prefix != '2nd':
            my_loc = '], [ ' + my_loc
            if prefix != '3rd':
              my_loc = '], [ ' + my_loc
        myv = my_loc + myv
      mylang.add(kind + '-searching-' + prefix + '-' + stype + ' := ' + \
        stype + ' & [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + myv + ' ], ... > ].')
      hd = val.get('head')
      form = val.get('form')
      cstr = my_loc + 'LOCAL.CAT.HEAD ' + hd + ' & [ FORM ' + form + ' ]'
      mylang.add(form + '-' + prefix + '-comp-verb-lex := ' + kind + '-searching-' + prefix + '-' + stype + ' & \
        [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ' + cstr + '], ... > ].')  
      comps_list.append(form)

  cross_h = []
  for hd in heads:

    bname = '-comp-verb-lex'
    hdn = hd.get('name')
    if hdn == 'noun' or hdn == '+nc':
      cross_h.append(hdn)
    constraint = 'LOCAL.CAT.HEAD ' + hdn
    combine_opt_features(mylang, bname, hdn, constraint, opt_kds, prefix)
    if hdn == 'comp':
      constr = 'LOCAL.CONT.HOOK.INDEX.SF prop-or-ques'
      combine_opt_features(mylang, bname, hdn, constr, opt_kds, prefix)     
      constr1 = 'LOCAL.CONT.HOOK.INDEX.SF prop'
      combine_opt_features(mylang, bname, 'prop-'+hdn, constr1, opt_kds, prefix) 
      constr2 = 'LOCAL.CONT.HOOK.INDEX.SF ques'
      combine_opt_features(mylang, bname, 'ques-'+hdn, constr2, opt_kds, prefix)
 
  cns = []
  
  for cs in cases:
    bname = '-comp-verb-lex'
    csn = cs.get('name')
    cns.append(csn)
    constraint = 'LOCAL.CAT.HEAD.CASE ' + csn 
    combine_opt_features(mylang, bname, csn, constraint, opt_kds, prefix)
  suffix = prefix + '-comp-verb-lex'
  comps_list = cross_classify(mylang, cns, cross_h, suffix, comps_list)
  

  adp_forms = []
  for form in forms:
    bname = '-comp-verb-lex'
    fn = form.get('name')
    myh = form.get('head')
    hs = myh.split(',')
    suffix = prefix + '-comp-verb-lex'  
  
    fns = fn.split(',')
    for f in fns:
      constraint = 'LOCAL.CAT.HEAD.FORM ' + f 
      combine_opt_features(mylang, bname, f, constraint, opt_kds, prefix)
      if myh == 'adp':
        mylang.add(f + ' := pform.', section='features')
        adp_forms.append(f)
    comps_list = cross_classify(mylang, fns, hs, suffix, comps_list)

  if refl:
    mylang.add('refl-1st-comp-verb-lex := 1st-comp-verb-lex & \
         [ SYNSEM.LOCAL.CAT.VAL [ COMPS.FIRST.LOCAL refl-local & \
                                     [ CONT.HOOK.INDEX #ind ], \
                             SUBJ < [ LOCAL.CONT.HOOK.INDEX #ind ] > ] ].')
    comps_list.append('refl')

  if part:
#particle is never followed by another argument
    mylang.add('lexkeys :+ [ KEY-PART list ].', section = 'addenda')
    mylang.add('verb :+ [ PART-FORM list ].', section = 'addenda')
    constraint = 'OPT -, \
              LOCAL.CAT.HEAD verb & [ PART-FORM #prt ] ] >,'
    if prefix != '1st':
      constraint = '], [ ' + constraint
      if prefix != '2nd':
        constraint = '], [ ' + constraint
        if prefix != '3rd':
          constraint = '], [ ' + constraint
    mylang.add('part-' + prefix + '-comp-verb-lex := ' + stype + ' & \
     [ SYNSEM [ LOCAL.CAT.VAL.COMPS < [ ' + constraint + \
                'LKEYS.KEY-PART #prt ] ].')
    comps_list.append('part')

  my_lists = []
  my_lists.append(comps_list)
  my_lists.append(adp_forms)
  return my_lists

def create_basic_subject_values_old(ch, mylang, stype):
# 
# 1 create possible subjects
# case for subjects: nominative
# head for subjects: noun
# subjects subkinds: 
# explicit or not
# optional or not
# define defaults
  heads = ch.get('subj_prop_head',[])
  cases = ch.get('subj_prop_case',[])
  expl = ch.get('subj_prop_expl',[])
  opt = ch.get('subj_prop_opt_kind',[])
####list of those elements that can occur as subject
  subj_list = []

# making sure that subjects have taken all their complements
  val = '[ SUBJ < >, \
           COMPS < >, \
           SPR < >, \
           SPEC < > ]'
  mylang.add(stype + ' := \
             [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.VAL ' + val + ' ] > ].')

  opt_kds = []
  if len(opt) > 0:
    mylang.add('opt-sbj-verb-lex := ' + stype + ' & \
       [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ OPT + ] >].')
    mylang.add('obl-sbj-verb-lex := ' + stype + ' & \
       [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ OPT - ] >].')
  
    for k in opt:
      kn = k.get('name')
      opt_kds.append(kn)
      if not kn == 'head' and not kn == 'case':
        mylang.add(kn + '-opt-sbj-verb-lex := ' + kn + '-verb-lex & opt-sbj-verb-lex.')
        mylang.add(kn + '-obl-sbj-verb-lex := ' + kn + '-verb-lex & obl-sbj-verb-lex.')
        subj_list.append(kn + '-opt')
        subj_list.append(kn + '-obl')
      
###Subjects in German (non-spoken language) can only be dropped if they are
###explitives  
  hdns = []
  for hd in heads:
    if not hd in opt_kds:
      hstype = 'obl-sbj-verb-lex'
    else:
      hstype = stype
    hdn = hd.get('name')
    hdns.append(hdn)
    mylang.add(hdn + '-sbj-verb-lex := ' + hstype + ' & \
            [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + hdn + ' ] > ].')
    
  cns = []
  for cs in cases:

    if len(heads) == 1:
      cstype = 'obl-sbj-verb-lex'
      if 'head' in opt_kds and not 'case' in opt_kds:
        cstype = cstype + ' & basic-sbj-verb-lex'
    elif not 'case' in opt_kds:
      cstype = 'basic-' + stype
    else:
      cstype = stype
    csn = cs.get('name')
    cns.append(csn)
    mylang.add(csn + '-sbj-verb-lex := ' + cstype + ' & \
      [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE ' + csn + ' ] > ].' )
    if len(heads) > 0:  
      subj_list = cross_classify(mylang, cns, hdns, 'sbj-verb-lex', subj_list)
    else:
      subj_list.append(csn)
 
  for e in expl:
    ex_h = e.get('head')
    ex_c = e.get('case')
    if 'expl' in opt_kds: 
      mylang.add('expl-verb-lex := ' + stype + ' & \
             [ ARG-ST < [ LOCAL.CONT.HOOK.INDEX expl-ind ], ... > ].')

    else:
      mylang.add('expl-verb-lex := ' + ex_c + '-' + ex_h + '-sbj-verb-lex & \
             [ ARG-ST < [ LOCAL.CONT.HOOK.INDEX expl-ind ], ... > ].')
   # subj_list.append('expl')
  return subj_list


from gmcs.linglib import long_distance_dependencies

########################################################
# COMMON COMPONENTS V2+CLUSTERS ACROSS ANALYSES CHOICES
# GENERAL COMPONENTS

def v2_and_verbal_clusters(ch, mylang, lrules, rules, climb_files):
#create nexus phrases if cluster is present  
  climb_gwo = climb_files.get('ger-wo')

  add_nexus_constraints_v2_with_cluster(ch, mylang, climb_gwo)

#creating basic phrases and rules

  add_basic_phrases_v2_with_cluster(ch, mylang, rules, climb_gwo)
  add_v2_with_cluster_rules(ch, rules, climb_gwo)

#specialized rules and interactions for Germanic

  specialized_word_order_v2_with_cluster(ch, mylang, lrules, rules, climb_gwo)

##word-order revised for object-raising (mainly argument-composition)
##calling relevant functions to add constraints accordingly 2011-11-04

###CHECK: SHOULD THIS STILL BE CALLED?
  if ch.get('old-analysis') == 'yes':
    add_old_analysis_no_obj_raising_constraints(ch, mylang, rules, lrules, climb_gwo)
  else:
    add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules, climb_gwo)

  if ch.get('rel-clause') == 'yes':
    create_rel_clause_phrases(mylang, rules, climb_gwo, ch)
    if not ch.get('wh-questions') == 'yes':
      mylang.add('bare-np-phrase := head-nexus-phrase.')

  if ch.get('nachfeld') == 'yes':
    create_nachfeld_phrases(ch, mylang, rules, climb_gwo)

def add_nexus_constraints_v2_with_cluster(ch, mylang, climb_gwo):

###ADDING CONSTRAINTS ON BASIC RULES CREATING SECOND POSITION
###creating those rules to let elements follow the verb in second
###position: for the Vorfeld position, phrases depend on choice of analysis
###(MC or filler-gap)

###making sure language properties of having pre- or postpositions are respected
###calling head_rest function to see if general head-comp to build clauses
###should be restricted and, if so, to what
  head_rest = get_head_restr_for_non_clausal_head_comp(ch)

  if not ch.get('verb-morph') == 'off':
    mylang.add('finite-lex-rule := [ SYNSEM.LOCAL.CAT.MC na-or-- ].')
    climb_gwo.add('finite-lex-rule := [ SYNSEM.LOCAL.CAT.MC na-or-- ].')

###[ INV - ] for yes-no questions
  if ch.get('aux-comp-order') == 'both' or ch.get('vc-analysis') == 'aux-rule':
    mylang.add('cat :+ [ HEADFINAL bool ].', section='addenda')
    climb_gwo.add('cat :+ [ HEADFINAL bool ].', comment='section=addenda')

  if ch.get('vc-analysis') == 'basic':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    climb_gwo.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
  
  elif ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    climb_gwo.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
  
####we need to use special rules for adp-comp, for same reason as compl-comp
###introducing separate rules for sub-parts of sentence and main-clausal 
###structure (needed if not using head-filler for v2-ness

  if head_rest:
    mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')
    climb_gwo.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')    

  
  if ch.get('second-dependent-morph') == 'yes':
    mylang.add('cat :+ [ SUB-POS bool ].',comment='SUB-POS used to check whether subject follows or precedes the verb (morphology on verb changes for 2nd person sg in Dutch).',section='features')
    mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.SUB-POS #subpos, \
                                     DTR.SYNSEM.LOCAL.CAT.SUB-POS #subpos ].')
    mylang.add('int-cl := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                            HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
    mylang.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS + ].')
    mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
    climb_gwo.add('cat :+ [ SUB-POS bool ].',comment='SUB-POS used to check whether subject follows or precedes the verb (morphology on verb changes for 2nd person sg in Dutch).\n' + 'section=features')
    climb_gwo.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.SUB-POS #subpos, \
                                     DTR.SYNSEM.LOCAL.CAT.SUB-POS #subpos ].')
    climb_gwo.add('int-cl := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                            HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
    climb_gwo.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS + ].')
    climb_gwo.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')

    if ch.get('v2-analysis') == 'filler-gap':
      mylang.add('extracted-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS -].')
      climb_gwo.add('extracted-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS -].')
    else:
      mylang.add('subj-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS - ].')
      mylang.add('comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
      mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
      climb_gwo.add('subj-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS - ].')
      climb_gwo.add('comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')
      climb_gwo.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SUB-POS #sub-pos, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.SUB-POS #sub-pos ].')

def add_basic_phrases_v2_with_cluster(ch, mylang, rules, climb_gwo):

  mylang.add('subj-head-vc-phrase := decl-head-subj-phrase & head-final-invc & nonverbal-comp-phrase.')
  climb_gwo.add('subj-head-vc-phrase := decl-head-subj-phrase & head-final-invc & nonverbal-comp-phrase.')
  if ch.get('wh-questions') == 'yes':
    mylang.add('subj-head-vc-phrase := [ SYNSEM.NON-LOCAL.QUE 0-dlist ].')
    climb_gwo.add('subj-head-vc-phrase := [ SYNSEM.NON-LOCAL.QUE 0-dlist ].')
 
###2011-10-23 in-vc phrases are are per definition verbal
  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'

  mylang.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  climb_gwo.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  if ch.get('q-inv'):  
    mylang.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
 
###Analysis independent rules

  if ch.get('vc-placement') == 'pre':
    general_pre_objectival_cluster_phrases(ch, mylang, climb_gwo)
  else:
    general_post_objectival_cluster_phrases(ch, mylang, climb_gwo)

###2011-11-02 Removing condition that head-comp-phrase-2 is only needed
###if the argument order is free.
###head-comp-phrase-2 also needed for fixed order languages with new 
###word order (capturing SUBJ AUX OBJ TV, where OBJ is not first complement
###of the auxiliary)

  mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  climb_gwo.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  if ch.get('argument-order') == 'fixed':
    if ch.get('vc-placement') == 'post':
      add_fixed_argument_order_constraints_post_obj(mylang, climb_gwo)
    else:
      add_fixed_argument_order_constraints_pre_obj(mylang, climb_gwo)
    path = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD'
    mylang.add('head-comp-phrase-2 := [ ' + path + ' verb ].')
    mylang.add('head-comp-phrase-2 := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')
    climb_gwo.add('head-comp-phrase-2 := [ ' + path + ' verb ].')
    climb_gwo.add('head-comp-phrase-2 := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')
 
  if not (ch.get('argument-order') == 'fixed' and ch.get('v2-analysis') == 'filler-gap'):
    mylang.add('basic-extracted-subj-phrase :+ [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', section='addenda')
    climb_gwo.add('basic-extracted-subj-phrase :+ [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', comment='section=addenda')

###Additional trick to help efficiency: conj cannot be complement or subjects
###2013-09-26 changed so that it can be turned off.

  if not ch.get('forbid-constraints-nhd') == 'on':
    comment = 'Conjunction markers and determiners cannot be complement or subject markers. Adding the appropriate restrictions helps against spurious analyses'
    mylang.add('basic-head-comp-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvjrpcm ].', section='addenda')
    climb_gwo.add('basic-head-comp-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvjrpcm ].', comment='section=addenda')
###See comment
    comment = 'Germanic languages (except for Icelandic) only have nominative NPs as subject'
    mylang.add('basic-head-subj-phrase :+ \
              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD noun & [ CASE nom ], \
                                                VAL.SPR < > ] ].', comment, section='addenda')
    comment += '\n section=addenda'
    climb_gwo.add('basic-head-subj-phrase :+ \
              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD noun & [ CASE nom ], \
                                                VAL.SPR < > ] ].', comment)

  #this constraint was only added to argument-comp, not aux-rule  
  mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
  climb_gwo.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

  if ch.get('vc-analysis') == 'basic':
    create_argument_composition_phrases(ch, mylang, rules, climb_gwo)
  elif ch.get('vc-analysis') == 'aux-rule':
    create_aux_plus_verb_phrases(ch, mylang, climb_gwo)

  if ch.get('v2-analysis') == 'filler-gap':
    long_distance_dependencies.add_basic_ldd_phrases(ch, mylang, rules, climb_gwo)
    filler_gap_word_order(ch, mylang, climb_gwo)
    filler_gap_rules(ch, rules, climb_gwo)
  else:
    mc_v2_word_order(ch, mylang, rules, climb_gwo)

def general_pre_objectival_cluster_phrases(ch, mylang, climb_gwo):
  mylang.add('general-head-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc.')
  mylang.add('head-comp-vc-phrase := general-head-comp-vc-phrase & nonverbal-comp-phrase.')
  mylang.add('head-comp-2-vc-phrase := basic-head-2nd-comp-phrase & head-initial-invc & nonverbal-comp-phrase.')
  climb_gwo.add('general-head-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc.')
  climb_gwo.add('head-comp-vc-phrase := general-head-comp-vc-phrase & nonverbal-comp-phrase.')
  climb_gwo.add('head-comp-2-vc-phrase := basic-head-2nd-comp-phrase & head-initial-invc & nonverbal-comp-phrase.')
  if ch.get('aux-comp-order') == 'before':
    mylang.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT +, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].') 
    climb_gwo.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT +, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].') 


def general_post_objectival_cluster_phrases(ch, mylang, climb_gwo):

  mylang.add('general-comp-head-vc-phrase:= basic-head-1st-comp-phrase & head-final-invc.')
  mylang.add('comp-head-vc-phrase := general-comp-head-vc-phrase & nonverbal-comp-phrase.')
  mylang.add('comp-2-head-vc-phrase := basic-head-2nd-comp-phrase & head-final-invc & nonverbal-comp-phrase.')
  climb_gwo.add('general-comp-head-vc-phrase:= basic-head-1st-comp-phrase & head-final-invc.')
  climb_gwo.add('comp-head-vc-phrase := general-comp-head-vc-phrase & nonverbal-comp-phrase.')
  climb_gwo.add('comp-2-head-vc-phrase := basic-head-2nd-comp-phrase & head-final-invc & nonverbal-comp-phrase.')
  if ch.get('aux-comp-order') == 'after': 
    mylang.add('comp-aux-vc-phrase := [ HEAD-DTR.SYNSEM [ LIGHT +, \
                                                          LOCAL.CAT.MC -], \
                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
    climb_gwo.add('comp-aux-vc-phrase := [ HEAD-DTR.SYNSEM [ LIGHT +, \
                                                          LOCAL.CAT.MC -], \
                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
    if ch.get('q-inv'):
      mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
  elif ch.get('aux-comp-order') == 'before':
    mylang.add('general-head-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc.')
    mylang.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT [ VC +, \
                                                                    MC - ], \
                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC - ]].')
    climb_gwo.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT [ VC +, \
                                                                    MC - ], \
                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC - ]].')
    climb_gwo.add('general-head-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc.')
    mylang.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT + ].') 
    climb_gwo.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT + ].') 

    if ch.get('q-inv'):
      mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')

  elif ch.get('aux-comp-order') == 'both':
    mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT [ HEADFINAL #hf ], \
                                            HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL +, \
                                                                        MC - ], \
                                            NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
                                                                            HEADFINAL #hf ] ].')
    mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL -, \
				                       VC + ], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
				                           HEADFINAL #hf ]].')
    climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT [ HEADFINAL #hf ], \
                                            HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL +, \
                                                                        MC - ], \
                                            NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
                                                                            HEADFINAL #hf ] ].')
    climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL -, \
				                       VC + ], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
				                           HEADFINAL #hf ]].')
    if ch.get('q-inv'):
      mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    if ch.get('edge-related-res') == 'yes':
      dtr = ch.get('edge-dtr')
      add_edge_constraint(mylang, climb_gwo, dtr)

def add_v2_with_cluster_rules(ch, rules, climb_gwo):
  climb_gwo.set_section('rules')
  
  if ch.get('vc-placement') == 'post':
    rules.add('comp-2-head-vc := comp-2-head-vc-phrase.')
    climb_gwo.add('comp-2-head-vc := comp-2-head-vc-phrase.')
  else:
    rules.add('head-comp-2-vc := head-comp-2-vc-phrase.')
    climb_gwo.add('head-comp-2-vc := head-comp-2-vc-phrase.')
  rules.add('subj-head-vc := subj-head-vc-phrase.')
  rules.add('aux-2nd-comp := aux-2nd-comp-phrase.')
  climb_gwo.add('subj-head-vc := subj-head-vc-phrase.')
  climb_gwo.add('aux-2nd-comp := aux-2nd-comp-phrase.')

  if not ch.get('v2-analysis') == 'filler-gap':
    rules.add('comp-aux-2nd := comp-aux-2nd-phrase.')
    climb_gwo.add('comp-aux-2nd := comp-aux-2nd-phrase.')

# Additional rules for if object-raising is covered
  if ch.get('vc-analysis') == 'basic' and not ch.get('v2-analysis') == 'filler-gap':
    rules.add('comp-aux-2nd-2 := comp-aux-2nd-phrase-2.')
    climb_gwo.add('comp-aux-2nd-2 := comp-aux-2nd-phrase-2.')
  elif ch.get('vc-analysis') == 'aux-rule':
    rules.add('vcomp-mverb-2nd := vcomp-mverb-2nd-phrase.')
    rules.add('mverb-2nd-vcomp := mverb-2nd-vcomp-phrase.')
    climb_gwo.add('vcomp-mverb-2nd := vcomp-mverb-2nd-phrase.')
    climb_gwo.add('mverb-2nd-vcomp := mverb-2nd-vcomp-phrase.')
    if ch.get('obj-raising') == 'yes':
      if ch.get('aux-comp-order') != 'before':
        rules.add('comp-mverb-vc := comp-mverb-vc-phrase.')
        climb_gwo.add('comp-mverb-vc := comp-mverb-vc-phrase.', section='rules')
      if ch.get('aux-comp-order') != 'after':
        rules.add('mverb-comp-vc := mverb-comp-vc-phrase.')
        climb_gwo.add('mverb-comp-vc := mverb-comp-vc-phrase.', section='rules')
  # rule for yes-no question inversion
  # for now only when analysis is aux-rule (apparently not used for 
  # arg-comp, or old error...)
  if ch.get('q-inv') and ch.get('vc-analysis') == 'aux-rule':
    rules.add('aux-1st-comp := aux-1st-comp-phrase.')
    climb_gwo.add('aux-1st-comp := aux-1st-comp-phrase.')

###2011-11-02 removing condition of non-fixed arg order (see above)
###2012-12-17 trying to put it back in Dutch grammar is overgenerating...
#  if ch.get('argument-order') != 'fixed':
  rules.add('head-comp-2 := head-comp-phrase-2.')
  climb_gwo.add('head-comp-2 := head-comp-phrase-2.')

  if ch.get('vc-analysis') == 'aux-rule':
    auxRule = True
  else:
    auxRule = False

  if ch.get('vc-placement') == 'pre':
    add_preobjectival_verbcluster_rules(ch, rules, climb_gwo, auxRule)
  else:
    add_postobjectival_verbcluster_rules(ch, rules, climb_gwo, auxRule)

  if ch.get('split-cluster') == 'yes':
    add_split_cluster_rules(rules, climb_gwo, auxRule)

  climb_gwo.set_section('mylang')


def add_preobjectival_verbcluster_rules(ch, rules, climb_gwo, auxRule):

  rules.add('head-comp-vc := head-comp-vc-phrase.')
  if not auxRule:
    rules.add('head-comp-2-vc := head-comp-2-vc-phrase.')
  if ch.get('aux-comp-order') == 'before':
    rules.add('aux-comp-vc := aux-comp-vc-phrase.')

  climb_gwo.add('head-comp-vc := head-comp-vc-phrase.')
  if not auxRule:
    climb_gwo.add('head-comp-2-vc := head-comp-2-vc-phrase.')
  if ch.get('aux-comp-order') == 'before':
    climb_gwo.add('aux-comp-vc := aux-comp-vc-phrase.')


def add_postobjectival_verbcluster_rules(ch, rules, climb_gwo, auxRule):

  rules.add('comp-head-vc := comp-head-vc-phrase.')
  if not auxRule:
    rules.add('comp-2-head-vc := comp-2-head-vc-phrase.')
  if ch.get('aux-comp-order') == 'after' or ch.get('aux-comp-order') == 'both':
    rules.add('comp-aux-vc := comp-aux-vc-phrase.')
  if ch.get('aux-comp-order') == 'before' or ch.get('aux-comp-order') == 'both':
    rules.add('aux-comp-vc := aux-comp-vc-phrase.') 

  climb_gwo.add('comp-head-vc := comp-head-vc-phrase.')
  if not auxRule:
    climb_gwo.add('comp-2-head-vc := comp-2-head-vc-phrase.')
  if ch.get('aux-comp-order') == 'after' or ch.get('aux-comp-order') == 'both':
    climb_gwo.add('comp-aux-vc := comp-aux-vc-phrase.')
  if ch.get('aux-comp-order') == 'before' or ch.get('aux-comp-order') == 'both':
    climb_gwo.add('aux-comp-vc := aux-comp-vc-phrase.') 


def add_split_cluster_rules(rules, climb_gwo, auxRule):
  if auxRule:
    rules.add('noncomp-aux-2nd := noncomp-aux-2nd-phrase.')  
    rules.add('insert-auxiliary := special-insert-aux-phrase.')
    climb_gwo.add('noncomp-aux-2nd := noncomp-aux-2nd-phrase.')  
    climb_gwo.add('insert-auxiliary := special-insert-aux-phrase.')

####################
# More specialized word order
#

###function that registers whether a specific verb-form is placed
###at the edge of the verbal cluster

def add_edge_constraint(mylang, climb_gwo, dtr):
  if dtr == 'head-dtr':
    DTR = 'HEAD-DTR'
  else:
    DTR = 'NON-HEAD-DTR'
  mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & na-or--, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & bool, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & na-or--, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & bool, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')

###function that adds necessary constraints to make complements that are not
###in sentence initial position appear in fixed order

def add_fixed_argument_order_constraints_post_obj(mylang, climb_gwo):

####TO DO: MOVE NON-HEAD-FILLER-PHRASE CONSTRAINTS TO WORD ORDER ANALYSES.

  mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')  
  mylang.add('subj-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-vc-phrase := \
                      [ SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap & bool, \
                                           ARG-ORDER - ], \
                        HEAD-DTR.SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap, \
                                                    ARG-ORDER + ] ].')

  climb_gwo.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')  
  climb_gwo.add('subj-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  climb_gwo.add('comp-head-vc-phrase := \
                      [ SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap & bool, \
                                           ARG-ORDER - ], \
                        HEAD-DTR.SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap, \
                                                    ARG-ORDER + ] ].')
###test: sharing removed or staying?
  mylang.add('comp-2-head-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER  + ].')
  climb_gwo.add('comp-2-head-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER  + ].')


def add_fixed_argument_order_constraints_pre_obj(mylang, climb_gwo):

####TO DO: MOVE NON-HEAD-FILLER-PHRASE CONSTRAINTS TO WORD ORDER ANALYSES.

  mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')  
  mylang.add('subj-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('head-comp-vc-phrase := \
                      [ SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap & bool, \
                                           ARG-ORDER - ], \
                        HEAD-DTR.SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap, \
                                                    ARG-ORDER + ] ].')

  climb_gwo.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')  
  climb_gwo.add('subj-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  climb_gwo.add('head-comp-vc-phrase := \
                      [ SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap & bool, \
                                           ARG-ORDER - ], \
                        HEAD-DTR.SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap, \
                                                    ARG-ORDER + ] ].')
###test: sharing removed or staying?
  mylang.add('head-comp-2-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER  + ].')
  climb_gwo.add('head-comp-2-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER  + ].')


def specialized_word_order_v2_with_cluster(ch, mylang, lrules, rules, climb_gwo):
 
  auxorder = ch.get('aux-comp-order')

##VC used to distinguish elements that can or cannot be part of verbal cluster
##nouns break verbal cluster
# 2011-11-01 Generalizing: some categories belong in the verbal cluster, 
# some do not. Moved to 'old-analysis' functions 2011-11-04
# mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.VC - ].')
 

##phrase to combine head with complement while head is in final position
##taking Germanic languages: either needs to become complement of conjugated
##verb in second position, or is a subordinate clause. (hence MC -)
##TO CHECK: DANISH
  
  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'
  mylang.add(hf_invc + ' := head-final & \
                    [ SYNSEM.LOCAL.CAT.MC #mc & -, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
  climb_gwo.add(hf_invc + ' := head-final & \
                    [ SYNSEM.LOCAL.CAT.MC #mc & -, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')

##nonverbal-comp-phrase: states that phrase cannot be used to combine
##head with verbal complement
###2013-09-21 changing so that this only restricts to non-verbs when basic non-head-dtr constraints are not used
  if not ch.get('forbid-constraints-nhd') == 'on':
    hres = '+njrpcm'
  else:
    hres = '+njrpcdmo'

  mylang.add('nonverbal-comp-phrase := basic-binary-headed-phrase & \
                   [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD ' + hres + ', \
                                                     VAL [ SPR < >, \
                                                           SUBJ < >, \
                                                           COMPS < > ] ] ].')
  climb_gwo.add('nonverbal-comp-phrase := basic-binary-headed-phrase & \
                   [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD ' + hres + ', \
                                                     VAL [ SPR < >, \
                                                           SUBJ < >, \
                                                           COMPS < > ] ] ].')

  if ch.get('v2-analysis') == 'filler-gap' or ch.get('nvc-restr') == 'on':
    mylang.add('head-subj-phrase := nonverbal-comp-phrase.')
    mylang.add('head-wh-subj-phrase := nonverbal-comp-phrase.') 
    mylang.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
    climb_gwo.add('head-subj-phrase := nonverbal-comp-phrase.')
    climb_gwo.add('head-wh-subj-phrase := nonverbal-comp-phrase.') 
    climb_gwo.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
  mylang.add('head-comp-phrase := nonverbal-comp-phrase.')
  mylang.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
  climb_gwo.add('head-comp-phrase := nonverbal-comp-phrase.')
  climb_gwo.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
##edge related restrictions to account for Dutch:
##*hij zal de bal kunnen gekregen hebben.
##hij zal de bal gekregen kunnen hebben.
##hij zal de bal kunnen hebben gekregen.
##he shall the ball can-inf have-inf got-ptc
##
##the participle can appear on either side of the cluster,
##but not in the middle (this is however allowed in Flemish)
##
##feature EDGE can register if an item is at the edge of a phrase or not

  if ch.get('edge-related-res') == 'yes':
    mylang.add('cat :+ [ EDGE luk ].', 'EDGE is used to prevent certain word forms from occurring in the middle of the cluster', section='addenda')
    climb_gwo.add('cat :+ [ EDGE luk ].', 'EDGE is used to prevent certain word forms from occurring in the middle of the cluster\n' + 'section=addenda')
##
##verb-second languages can have a fixed or (relatively) free order of arguments
##that are not in first position.
##Dutch and Danish have fixed order, German allows more freedom.
##Fixed order is reflected as well in which partial VPs are allowed in the 
##Vorfeld

  if ch.get('argument-order') == 'fixed':
    mylang.add('cat :+ [ ARG-ORDER bool, \
                         ALLOWED-PART luk ].', 'ARG-ORD keeps track of ordering of arguments. ALLOWED-PART makes sure no disallowed partial VP-phrases occur in the Vorfeld.', section='addenda')
    mylang.add('lex-rule :+ [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                              DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].', section='addenda')
    climb_gwo.add('cat :+ [ ARG-ORDER bool, \
                         ALLOWED-PART luk ].', 'ARG-ORD keeps track of ordering of arguments. ALLOWED-PART makes sure no disallowed partial VP-phrases occur in the Vorfeld.\n section=addenda')
    climb_gwo.add('lex-rule :+ [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                              DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].',
               comment='section=addenda')
###not allowed partial vps can occur with ditransitive verbs 
    if ch.get('ditransitives') == 'yes':
      mylang.add('ditransitive-verb-lex := [ SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-- ].')
      climb_gwo.add('ditransitive-verb-lex := [ SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-- ].')


##verb-initial in cluster counter part, only when necessary which may be:
## 1. verbs precede their verbal complement (Dutch, German auxiliary flip
## 2. verbs precede their objects (Danish)
## [MC - ] as for head-final-invc (above)

  if auxorder == 'before' or auxorder == 'both' or ch.get('vc-placement') == 'pre':
    mylang.add('head-initial-invc := head-initial & \
                  [ SYNSEM.LOCAL.CAT [ MC #mc & -, \
                                       HEAD verb & [ INV - ] ], \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    climb_gwo.add('head-initial-invc := head-initial & \
                  [ SYNSEM.LOCAL.CAT [ MC #mc & -, \
                                       HEAD verb & [ INV - ] ], \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
##
##calling specific rules depending on chosen analysis

  if ch.get('vc-analysis') == 'aux-rule':
    spec_word_order_phrases_aux_plus_verb(ch, mylang, climb_gwo)
  else:
    spec_word_order_phrases_argument_composition(ch, mylang, lrules, rules, climb_gwo)

###can be done by head-sub-comp, but leads to odd ambiguities for nouns
###leaving this for nouns for now
  if ch.get('nonverb-zuinf-comp') == 'yes':
    wh = False
    if ch.get('wh-questions') == 'yes':
      wh = True
    create_nonverb_zuinf_structure(mylang, rules, climb_gwo, wh)

##############################
#
# Phrases and rules for wh-containing sentences
#

def create_wh_wo_phrases(ch, mylang, climb_wh):

### RESTRICTING ALL THAT IS VC OR 2ND TO VERBAL HEADS 2013-06-02
### TODO: SEE IF THIS IS NECESSARY


  mylang.add('head-wh-subj-phrase := basic-head-wh-subj-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  mylang.add('wh-subj-head-vc-phrase := basic-head-wh-subj-phrase & head-final-invc & nonverbal-comp-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  
  mylang.add('wh-adjunct-head-phrase := basic-head-wh-mod-phrase-simple & \
            [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +rp ].')

  mylang.add('head-2nd-wh-adj-int-phrase := head-wh-adj-int-phrase & \
               head-initial-head-nexus & wh-adjunct-head-phrase & \
              [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  climb_wh.add('head-wh-subj-phrase := basic-head-wh-subj-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  climb_wh.add('wh-subj-head-vc-phrase := basic-head-wh-subj-phrase & head-final-invc & nonverbal-comp-phrase & \
              [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  
  climb_wh.add('wh-adjunct-head-phrase := basic-head-wh-mod-phrase-simple & \
            [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +rp ].')

  climb_wh.add('head-2nd-wh-adj-int-phrase := head-wh-adj-int-phrase & \
               head-initial-head-nexus & wh-adjunct-head-phrase & \
             [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'
  mylang.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' 
             + hf_invc + ' & adjunct-head-phrase  \
              [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  mylang.add('wh-adj-head-int-vc-phrase := wh-adj-head-int-phrase & \
                head-final-invc & wh-adjunct-head-phrase  & \
              [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  mylang.add('comp-aux-vc-phrase := head-non-wh-or-rel.')
  if ch.get('vc-placement') == 'post':
    mylang.add('comp-head-vc-phrase := head-non-wh-or-rel & share-que-non-head-phrase  & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
    mylang.add('wh-comp-head-vc-phrase := general-comp-head-vc-phrase & \
               nonverbal-comp-phrase & head-wh  & \
              [ SYNSEM.LOCAL.CAT.HEAD verb ].') 
    climb_wh.add('comp-head-vc-phrase := head-non-wh-or-rel & share-que-non-head-phrase  & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
    climb_wh.add('wh-comp-head-vc-phrase := general-comp-head-vc-phrase & \
               nonverbal-comp-phrase & head-wh & \
                [ SYNSEM.LOCAL.CAT.HEAD verb ].') 
  else:
    mylang.add('head-comp-vc-phrase := head-non-wh-or-rel & share-que-non-head-phrase  & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
    mylang.add('head-wh-comp-vc-phrase := general-head-comp-vc-phrase & \
               nonverbal-comp-phrase & head-wh & [ SYNSEM.LOCAL.CAT.HEAD verb ].') 
    climb_wh.add('head-comp-vc-phrase := head-non-wh-or-rel & share-que-non-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
    climb_wh.add('head-wh-comp-vc-phrase := general-head-comp-vc-phrase & \
               nonverbal-comp-phrase & head-wh & [ SYNSEM.LOCAL.CAT.HEAD verb ].') 
    
  mylang.add('head-comp-phrase := head-non-wh-or-rel.')
  mylang.add('adjunct-head-phrase := head-non-wh-or-rel.') 

  climb_wh.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' 
             + hf_invc + ' & adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  climb_wh.add('wh-adj-head-int-vc-phrase := wh-adj-head-int-phrase & \
                head-final-invc & wh-adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  climb_wh.add('comp-aux-vc-phrase := head-non-wh-or-rel.')
  climb_wh.add('head-comp-phrase := head-non-wh-or-rel.')
  climb_wh.add('adjunct-head-phrase := head-non-wh-or-rel.') 

  if ch.get('comp-for-np-adj') == 'yes':
    mylang.add('adjunct-head-phrase := normal-val-mod.')
    climb_wh.add('adjunct-head-phrase := normal-val-mod.')

  mylang.add('head-wh-comp-phrase := head-wh & basic-head-1st-comp-phrase & \
                          head-initial-head-nexus & nonverbal-comp-phrase.')
  climb_wh.add('head-wh-comp-phrase := head-wh & basic-head-1st-comp-phrase & \
                          head-initial-head-nexus & nonverbal-comp-phrase.')

  if ch.get('v2-analysis') == 'mc' and ch.get('vc-analysis') == 'basic':
    if ch.get('vc-placement') == 'post':
      mylang.add('comp-2-head-vc-phrase := head-non-wh-or-rel.')
      climb_wh.add('comp-2-head-vc-phrase := head-non-wh-or-rel.')
    else:
      mylang.add('head-comp-2-vc-phrase := head-non-wh-or-rel.')
      climb_wh.add('head-comp-2-vc-phrase := head-non-wh-or-rel.')
####if 100% correct, should check for presence of determiners
###but Germanic languages have them
  mylang.add('wh-spec-head-phrase := basic-head-wh-spec-phrase & head-final & \
       [ SYNSEM.LOCAL.CAT.VC #vc, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc, \
         NON-HEAD-DTR.SYNSEM.OPT - ].')
  climb_wh.add('wh-spec-head-phrase := basic-head-wh-spec-phrase & head-final & \
       [ SYNSEM.LOCAL.CAT.VC #vc, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc, \
         NON-HEAD-DTR.SYNSEM.OPT - ].')

  mylang.add('bare-np-phrase :=[ HEAD-DTR.SYNSEM.NON-LOCAL.QUE 0-dlist ].')
  climb_wh.add('bare-np-phrase :=[ HEAD-DTR.SYNSEM.NON-LOCAL.QUE 0-dlist ].')
  
  #no wh when definite marker has been added
  if ch.get('def-morph-mark') == 'yes': 
    mylang.add('bare-def-np-phrase := [ SYNSEM.NON-LOCAL.QUE 0-dlist ].')

def create_wh_rules(ch, rules, climb_wh):
  rules.add('head-wh-subj := head-wh-subj-phrase.')
  rules.add('wh-subj-head-vc := wh-subj-head-vc-phrase.')
  if ch.get('vc-placement') == 'post':
    rules.add('wh-comp-head-vc := wh-comp-head-vc-phrase.')
  else:
    rules.add('head-wh-comp-vc := head-wh-comp-vc-phrase.')
  rules.add('head-wh-comp := head-wh-comp-phrase.')
 # if not ch.get('v2-analysis') == 'filler-gap':
  rules.add('wh-ques := create-wh-ques-vcomp-phrase.')
  rules.add('head-2nd-wh-adj-int := head-2nd-wh-adj-int-phrase.')
  rules.add('wh-adj-head-int-vc := wh-adj-head-int-vc-phrase.')
  rules.add('wh-spec-head := wh-spec-head-phrase.')
  climb_wh.set_section('rules')
  climb_wh.add('head-wh-subj := head-wh-subj-phrase.')
  climb_wh.add('wh-subj-head-vc := wh-subj-head-vc-phrase.')
  if ch.get('vc-placement') == 'post':
    climb_wh.add('wh-comp-head-vc := wh-comp-head-vc-phrase.')
  else:
    climb_wh.add('head-wh-comp-vc := head-wh-comp-vc-phrase.')
    
  climb_wh.add('head-wh-comp := head-wh-comp-phrase.')
 # if not ch.get('v2-analysis') == 'filler-gap':
  climb_wh.add('wh-ques := create-wh-ques-vcomp-phrase.')
  climb_wh.add('head-2nd-wh-adj-int := head-2nd-wh-adj-int-phrase.')
  climb_wh.add('wh-adj-head-int-vc := wh-adj-head-int-vc-phrase.')
  climb_wh.add('wh-spec-head := wh-spec-head-phrase.')
  climb_wh.set_section('mylang')

def create_nonverb_zuinf_structure(mylang, rules, climb_gwo, wh):
  nom_head_zu_comp = 'nonverb-head-zu-comp-phrase := \
                         basic-head-1st-comp-phrase & head-initial & \
                                                  share-que-non-head-phrase & \
                        [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD noun, \
                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ FORM zuinf ], \
				                          VAL [ COMPS < > ], \
			                                  MC - ] ].'
  mylang.add(nom_head_zu_comp)
  climb_gwo.add(nom_head_zu_comp)

  rules.add('nonverb-head-zu-comp := nonverb-head-zu-comp-phrase.')
  climb_gwo.add('nonverb-head-zu-comp := nonverb-head-zu-comp-phrase.',section='rules')

  mylang.add('head-spec-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                                    COMPS < > ] ].')
 
  if wh:
    mylang.add('wh-spec-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                                    COMPS < > ] ].')
    climb_gwo.add('wh-spec-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                                    COMPS < > ] ].')


def create_nachfeld_phrases(ch, mylang, rules, climb_gwo):
  mylang.add('nf-form := form.')
  mylang.add('verb-lex := [ SYNSEM.LIGHT + ].')
  climb_gwo.add('nf-form := form.')
  climb_gwo.add('verb-lex := [ SYNSEM.LIGHT + ].')
  forms = ch.get('nf-forms')  
  my_forms = forms.split(',')
  for f in my_forms:
    mylang.add(f + ' := nf-form.', section='features')
    climb_gwo.add(f + ' := nf-form.', comment='section=features')
  nf_type = '''nachfeld-phrase := head-initial &
 [ SYNSEM.LOCAL.CAT [ VC na-or--,
		      MC #mc ],
   HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
			       VC bool,
			       MC #mc ],
   NON-HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD +vpc & [ FORM nf-form ] ],
			 NON-LOCAL [ SLASH 0-dlist,
                                     REL 0-dlist,
				     QUE 0-dlist ],
			 LIGHT - ] ].'''
 
  mylang.add(nf_type)
  climb_gwo.add(nf_type)

  if ch.get('extraposition'):
    mylang.add('nachfeld-phrase := collect-anchor-phrase.')
    climb_gwo.add('nachfeld-phrase := collect-anchor-phrase.')

 # introduce EDGE if not already present
  if not ch.get('edge-related-res') == 'yes':
    mylang.add('cat :+ [ EDGE luk ].')
    climb_gwo.add('cat :+ [ EDGE luk ].')

  typedef = '''extracted-comp-phrase-nachfeld := 
         basic-extracted-comp-phrase & share-anchor-unary-phrase & 
    [ HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc & -,
                                VC na-or-+,
				HEAD +nvj,
				EDGE +,
                                VFRONT #vf & - ],
      SYNSEM [ LOCAL.CAT [ VC na-or--,
		           VFRONT #vf,
		           MC #mc,
                           EDGE - ] ] ].'''

  mylang.add(typedef)
  climb_gwo.add(typedef)


  if ch.get('split-cluster') == 'yes':
    mylang.add('nachfeld-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vfront, \
                                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vfront ].')
    climb_gwo.add('nachfeld-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vfront,\
                                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vfront ].')
    mylang.add('extracted-comp-phrase-nachfeld := [ SYNSEM.LOCAL.CAT.VFRONT #vfront & -, \
                                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vfront ].')
    climb_gwo.add('extracted-comp-phrase-nachfeld := [ SYNSEM.LOCAL.CAT.VFRONT #vfront & -, \
                                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vfront ].')

  if ch.get('ldd') == 'yes':
    mylang.add('cat :+ [ HEADFINAL bool ].')
    mylang.add('nachfeld-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].') 
    mylang.add('extracted-comp-phrase-nachfeld := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEADFINAL - ].')
    mylang.add('general-filler-head-phrase := \
      [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL + ].')

    climb_gwo.add('cat :+ [ HEADFINAL bool ].')
    climb_gwo.add('nachfeld-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].') 
    climb_gwo.add('extracted-comp-phrase-nachfeld := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEADFINAL - ].')
    climb_gwo.add('general-filler-head-phrase := \
      [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL + ].')

  rules.add('extracted-comp-nachfeld := extracted-comp-phrase-nachfeld.')
  climb_gwo.add('extracted-comp-nachfeld := extracted-comp-phrase-nachfeld.', section='rules')
  mylang.add('nachfeld-head-filler-phrase := nachfeld-phrase.') 
  mylang.add('nachfeld-head-filler-phrase := basic-head-filler-phrase & \
    [ SYNSEM.LOCAL.CAT.VAL #val, \
      HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].')
  climb_gwo.add('nachfeld-head-filler-phrase := nachfeld-phrase.') 
  climb_gwo.add('nachfeld-head-filler-phrase := basic-head-filler-phrase & \
    [ SYNSEM.LOCAL.CAT.VAL #val, \
      HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].')
  rules.add('nachfeld-head-filler := nachfeld-head-filler-phrase.')
  climb_gwo.add('nachfeld-head-filler := nachfeld-head-filler-phrase.', section='rules')

  if ch.get('vc-analysis') == 'basic':
   # mylang.add('nachfeld-head-filler-phrase := basic-head-1st-comp-phrase.')
    mylang.add('nachfeld-phrase := \
       [ SYNSEM.LOCAL.CAT.SECOND #scd, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd, \
         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    mylang.add('extracted-comp-phrase-nachfeld := \
               [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                 HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
    climb_gwo.add('nachfeld-phrase := \
       [ SYNSEM.LOCAL.CAT.SECOND #scd, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd, \
         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    climb_gwo.add('extracted-comp-phrase-nachfeld := \
               [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                 HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
    if ch.get('v2-analysis') == 'mc':
      mylang.add('nachfeld-head-filler-phrase := \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
      climb_gwo.add('nachfeld-head-filler-phrase := \
               [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
  else:
    mylang.add('nachfeld-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
    mylang.add('nachfeld-head-filler-phrase := \
         [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
    mylang.add('nachfeld-head-verbal-filler-phrase := nachfeld-phrase & \
                           basic-head-filler-phrase & \
    [ SYNSEM.LOCAL.CAT [ VAL #val, \
                         HEAD.AUX + ], \
      HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < >, \
      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL #val, \
                                      HEAD verb ] ].')
    climb_gwo.add('nachfeld-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
    climb_gwo.add('nachfeld-head-filler-phrase := \
         [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
    climb_gwo.add('nachfeld-head-verbal-filler-phrase := nachfeld-phrase & \
                           basic-head-filler-phrase & \
    [ SYNSEM.LOCAL.CAT [ VAL #val, \
                         HEAD.AUX + ], \
      HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < >, \
      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL #val, \
                                      HEAD verb ] ].')
    rules.add('nf-head-verbal-filler := nachfeld-head-verbal-filler-phrase.')
    climb_gwo.add('nf-head-verbal-filler := nachfeld-head-verbal-filler-phrase.',section='rules')
  #  mylang.add('nachfeld-head-comp-phrase := basic-verbal-comp-rule-1 & \
  #             [ SYNSEM.LOCAL.CAT.VAL #val, \
  #               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].')


def create_special_mod_valence_phrases(mylang, climb_gwo):
  mylang.add('normal-val-mod := head-mod-phrase-simple & \
 [ SYNSEM.LOCAL.CAT.VAL #val, \
   HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
  climb_gwo.add('normal-val-mod := head-mod-phrase-simple & \
 [ SYNSEM.LOCAL.CAT.VAL #val, \
   HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

##########################################################################
#                                                                        #
# ALTERNATIVE ANALYSES                                                   #
#                                                                        #
##########################################################################

#########################################################################
#
# A. vs B. Argument-composition vs auxiliary-rule + construction
# I. vs II. MC analysis vs Filler-gap
#
# Structure: 
# A. is common features of arg-comp analysis
# B. auxiliary-rule + construction
# I. MC-analysis common features
# II. Filler-gap common features
# A.I. Properties arg-comp and MC
# A.II. Properties arg-comp and Filler-gap
#
# B.II does not exist at present, so B corresponds to B and B.I.

#####################################################################
# A. Argument-composition analysis (standard HPSG)
#  

####DISCLAIMER: NOT ALL LOGICAL COMBINATIONS ARE COVERED BY CODE FOR NOW

def create_argument_composition_phrases(ch, mylang, rules, climb_gwo):
  if ch.get('has-dets') == 'yes':
    if ch.get('noun-det-order') == 'det-noun':
      mylang.add('head-spec-phrase := [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

      climb_gwo.add('head-spec-phrase := [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

  if ch.get('vc-placement') == 'pre':

    if ch.get('aux-comp-order') == 'before':
      currenttype = 'aux-comp-vc-phrase'
      mylang.add(currenttype + ' :=  general-head-comp-vc-phrase & \
                                 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')
      climb_gwo.add(currenttype + ' :=  general-head-comp-vc-phrase & \
                                 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

  else:
  # adding constraint that head must be [ AUX + ], leading to spurious analyses
  # without
    if ch.get('aux-comp-order') == 'after':
      mylang.add('comp-aux-vc-phrase := general-comp-head-vc-phrase & \
                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].') 
      climb_gwo.add('comp-aux-vc-phrase := general-comp-head-vc-phrase & \
                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].') 

    elif ch.get('aux-comp-order') == 'before':
      mylang.add('aux-comp-vc-phrase := general-head-comp-vc-phrase & \
                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].') 
      climb_gwo.add('aux-comp-vc-phrase := general-head-comp-vc-phrase & \
                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].') 
      if ch.get('argument-order') == 'fixed':
        mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
        climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
   

    elif ch.get('aux-comp-order') == 'both':
      mylang.add('comp-aux-vc-phrase := general-comp-head-vc-phrase & \
                         [ SYNSEM.LOCAL.CAT.SECOND na,\
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ VC +, \
                                                       SECOND +, \
                                                       HEAD verb & [ AUX +  ]], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
      mylang.add('aux-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc & \
                         [ SYNSEM.LOCAL.CAT.SECOND +,\
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ], \
                                                       SECOND na ], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
      climb_gwo.add('comp-aux-vc-phrase := general-comp-head-vc-phrase & \
                         [ SYNSEM.LOCAL.CAT.SECOND na,\
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ VC +, \
                                                       SECOND +, \
                                                       HEAD verb & [ AUX +  ]], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
      climb_gwo.add('aux-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc & \
                         [ SYNSEM.LOCAL.CAT.SECOND +,\
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ], \
                                                       SECOND na ], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')

      if ch.get('argument-order') == 'fixed':
        mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')

        mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
        climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')

        climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
   
  if not ch.get('v2-analysis') == 'filler-gap':
    mc_argcomp_word_order_phrases(ch, mylang, rules, climb_gwo)
    if not ch.get('old-analysis') == 'on':
      mc_argcomp_revised_wo(ch, mylang, climb_gwo)
################################################################
# specialized phrases for argument composition plus clusters
#

def spec_word_order_phrases_argument_composition(ch, mylang, lrules, rules, climb_gwo):

###SECOND registers whether element is currently in 2nd position or not
###and whether it is allowed to be in second position (for Germanic only 
###finite verb forms)
  mylang.add('cat :+ [ SECOND luk ].')
  climb_gwo.add('cat :+ [ SECOND luk ].')

 
### 2013-09-26: changing this to an option that can be turned on or off

  if not ch.get('forbid-constraints-nhd') == 'on':
    mylang.add('basic-head-2nd-comp-phrase :+ \
 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvrpcdm ].','As coverage grows (nachfeld, comparatives), the unspecified COMPS list of auxiliaries leads to problems.', section='addenda')

    climb_gwo.add('basic-head-2nd-comp-phrase :+ \
 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvrpcdm ].','As coverage grows (nachfeld, comparatives), the unspecified COMPS list of auxiliaries leads to problems.\nsection=addenda')

###verbal items are [ VC + ]
###basic phrases need to pass on value of VC feature

  mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
  mylang.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
  climb_gwo.add('verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
  climb_gwo.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
###[POTENTIALLY MORE GENERAL]
###head-comp constraint on LIGHT extend to head-subj-phrase
  mylang.add('basic-head-subj-phrase :+ [ SYNSEM [ LOCAL.CAT [ VC #vc, \
                                                               HC-LIGHT #light ], \
                                          LIGHT #light ], \
                          HEAD-DTR.SYNSEM.LOCAL.CAT.HC-LIGHT #light, \
                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
  climb_gwo.add('basic-head-subj-phrase :+ [ SYNSEM [ LOCAL.CAT [ VC #vc, \
                                                               HC-LIGHT #light ], \
                                          LIGHT #light ], \
                          HEAD-DTR.SYNSEM.LOCAL.CAT.HC-LIGHT #light, \
                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

###first analysis: to model some efficiency properties of aux+verb
###auxiliaries may only pick up verbal complements to their right
###this avoids spurious phrases where any form is taken as a potential subject
###or complement (the auxiliaries are more or less forced to find their verbal complements first.
###if auxiliary takes its verbal complement to its right, it still must pick up
###an element to its left to become verb second (SECOND registers this)   
###
###2011-11-03 removed [AUX +] obj-raising verbs are not auxiliaries and use
###this rule.
#
# Filler-gap has both aux-2nd-comp-phrase and aux-2nd-comp-2-phrase
#
  aux2nd = ''
  if ch.get('v2-analysis') == 'filler-gap':
    aux2nd = 'general-aux-2nd-comp'
  else:
    aux2nd = 'aux-2nd-comp-phrase'

  mylang.add(aux2nd + ' := head-initial & \
                    [ SYNSEM.LOCAL.CAT.MC #mc, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT[ MC #mc, \
			                         SECOND +, \
                                                 HEAD verb ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb, \
		                        	      MC - ]].') 
  mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.SECOND - ].')

  climb_gwo.add(aux2nd + ' := head-initial & \
                    [ SYNSEM.LOCAL.CAT.MC #mc, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT[ MC #mc, \
			                         SECOND +, \
                                                 HEAD verb ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb, \
		                        	      MC - ]].') 
  climb_gwo.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.SECOND - ].')

  if ch.get('wh-questions') == 'yes':
    add_wh_additions_for_arg_comp(ch, mylang, rules, climb_gwo)


##additional constraints to account for split clusters, if occuring
##(explanation see below)
##TRYING OUT: MAY NO LONGER BE NEEDED WITH REVISED WORD ORDER ANALYSIS
##2011-11-04 (worked)
#  if ch.get('split-cluster') == 'yes':
#    split_cluster_phrases_argument_composition(ch, mylang, rules, lrules)


def split_cluster_phrases_argument_composition(ch, mylang, rules, lrules):

  mylang.add('split-cl-comp-aux-2nd-phrase := gen-comp-aux-2nd-phrase & \
                       [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND -, \
                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
  rules.add('split-cluster-comp-aux-2nd := split-cl-comp-aux-2nd-phrase.')



  if ch.get('split-analysis') == 'lex-rule':
    split_cluster_arg_comp_lex_rule(ch, mylang, lrules)

  elif ch.get('split-analysis') == '3head-comp':
    mylang.add('comp-head-3-vc-phrase := basic-head-3rd-comp-phrase & \
                         nonverbal-comp-phrase & head-final-invc.')
    rules.add('comp-head-3-vc := comp-head-3-vc-phrase.')


def split_cluster_arg_comp_lex_rule(ch, mylang, lrules):

  headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  nhddtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'

  mylang.add('head-initial-head-nexus := ' + headdtrval + ' ].')
  mylang.add('comp-head-vc-phrase := ' + headdtrval + ' & na-or-- ].')
  mylang.add('comp-2-head-vc-phrase:= [ HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT - ].')
  mylang.add('comp-aux-2nd-phrase := ' + nhddtrval + ' ].')
  if not ch.get('v2-analysis') == 'filler-gap': 
    mylang.add('comp-aux-2nd-phrase-2 := ' + nhddtrval + ' ].')

  mylang.add('comp-aux-vc-phrase := ' + nhddtrval + ' ].')
  mylang.add('basic-head-subj-phrase :+ '+ headdtrval + ' ].')
  mylang.add('aux-2nd-comp-phrase := ' + nhddtrval + ' ].')
  vfrontval = '+'
  mylang.add('split-cl-comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.VFRONT -, \
                                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT ' + vfrontval + ' ].')
  mylang.add('basic-head-mod-phrase-simple :+ ' +headdtrval + ' ].')

  mylang.add('cat :+ [ VFRONT luk ].', 'VFRONT checks whether ditransitive verb has undergone needed modification to occur in the Vorfeld', section='addenda')
  mylang.add('infl-lex-rule :+ [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                       DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  if ch.get('argument-order') != 'fixed':
    mylang.add('change-arg-order-rule := const-val-change-only-lex-rule & \
 [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj, \
			    COMPS < #comp2, #comp1 >,\
			    SPR #spr,\
			    SPEC #spec ],\
		      VC #vc,\
		      SECOND #sd ], \
   DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,\
				COMPS < #comp1, #comp2 >,\
				SPR #spr,\
				SPEC #spec ],\
			  HEAD verb & [ AUX - ],\
			  VC #vc,\
			  SECOND #sd   ] ].')
    mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.VFRONT +, \
                    DTR.SYNSEM.LOCAL.CAT [ VFRONT -, \
                                           HEAD.FORM nonfinite ] ].' )
    lrules.add('change-arg-order := change-arg-order-rule.')
    if ch.get('argument-order') == 'fixed':
      mylang.add('change-arg-order-rule := \
                        [ SYNSEM.LOCAL.CAT [ ARG-ORDER #ao, \
                                             ALLOWED-PART #ap ], \
                          DTR.SYNSEM.LOCAL.CAT [ ARG-ORDER #ao, \
                                                 ALLOWED-PART #ap ]].')

    if ch.get('edge-related-res') == 'yes':
      mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.EDGE #ed, \
                                          DTR.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  if ch.get('aux-comp-order') == 'both':
    mylang.add('aux-comp-vc-phrase := ' + nhddtrval + ' ].')
    mylang.add('head-initial-invc := ' + headdtrval + ' ].')

##not compatible with new word order analysis that was introduced for object
##raising. Removing for now, see what must be done for Dutch later
#    if ch.get('ditransitives') == 'yes':
#      mylang.add('ditransitive-verb-lex := [ SYNSEM.LOCAL.CAT.VFRONT - ].')
#  else:
#    mylang.add('split-cl-comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.VFRONT -, \
#                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')


###2011-11-04 Revisions were made for object raising.
###pulling decision points marked by 'old-analysis' out of the grammar
###and placing them in two functions, covering pre-and post-obj-raising, resp.


def add_old_analysis_no_obj_raising_constraints(ch, mylang, rules, lrules, climb_gwo):
 
  mylang.add('no-cluster-lex-item := lex-item & [ SYNSEM.LOCAL.CAT.VC - ].') 
  climb_gwo.add('no-cluster-lex-item := lex-item & [ SYNSEM.LOCAL.CAT.VC - ].')  

  if ch.get('argument-order') == 'fixed':
    if ch.get('vc-placement') == 'post':
      mylang.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                       HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
      climb_gwo.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                       HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')

  if ch.get('vc-analysis') == 'basic':
    mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
    mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC na ].')
    climb_gwo.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
    climb_gwo.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC na ].')

    headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'

    mylang.add('head-initial-invc := ' + headdtrval + ' ].')
    climb_gwo.add('head-initial-invc := ' + headdtrval + ' ].')

    if ch.get('split-clusters'):
      split_cluster_phrases_argument_composition(ch, mylang, rules, lrules, climb_gwo)
    

def add_wh_additions_for_arg_comp(ch, mylang, rules, climb_gwo):
  if ch.get('old-analysis') != 'yes':
    mylang.add('head-wh-comp-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('head-wh-comp-phrase := basic-head-comp-share-vc.')

  if ch.get('vc-placement') == 'post':
    mylang.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
    climb_gwo.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('head-comp-phrase-2 := head-non-wh-or-rel.')
  mylang.add('head-wh-comp-phrase-2 := basic-head-2nd-comp-phrase & \
               head-initial-head-nexus & basic-head-comp-share-vc & head-wh.')

  climb_gwo.add('head-comp-phrase-2 := head-non-wh-or-rel.')
  climb_gwo.add('head-wh-comp-phrase-2 := basic-head-2nd-comp-phrase & \
               head-initial-head-nexus & basic-head-comp-share-vc & head-wh.')
  rules.add('head-wh-comp-2 := head-wh-comp-phrase-2.')
  climb_gwo.add('head-wh-comp-2 := head-wh-comp-phrase-2.',section='rules')

def add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules, climb_gwo):

  mylang.add('no-cluster-lex-item := lex-item & \
                                 [ SYNSEM.LOCAL.CAT.VC na-or-- ].') 
  climb_gwo.add('no-cluster-lex-item := lex-item & \
                                 [ SYNSEM.LOCAL.CAT.VC na-or-- ].')  

  if ch.get('vc-analysis') == 'basic':

    mylang.add('basic-head-comp-share-vc := basic-head-comp-phrase & \
                                 [ SYNSEM.LOCAL.CAT.VC #vc, \
                                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

    if ch.get('vc-placement') == 'post':
      mylang.add('comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
      mylang.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
      climb_gwo.add('comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
      climb_gwo.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')

    climb_gwo.add('basic-head-comp-share-vc := basic-head-comp-phrase & \
                                 [ SYNSEM.LOCAL.CAT.VC #vc, \
                                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')


###moved constraint [ MC + ] to mc specific (filler-gap [ MC na ])
###for dutch aux-2nd-comp-phrase must have VC na-or-+
    if ch.get('argument-order') == 'fixed':
      val = 'na-or-+'
    else:
      val = 'bool'
    mylang.add('aux-2nd-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC ' + val + ' ].')
    climb_gwo.add('aux-2nd-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC ' + val + ' ].')  
  
    argument_composition_revised_additional_constraints(ch, mylang, lrules, climb_gwo)

    if ch.get('argument-order') == 'fixed' and ch.get('aux-comp-order') == 'both':    
      add_additional_arg_order_constraints(ch, mylang, climb_gwo)


def argument_composition_revised_additional_constraints(ch, mylang, lrules, climb_gwo):
  mylang.add('cat :+ [ VFRONT luk ].')
  climb_gwo.add('cat :+ [ VFRONT luk ].')
  if ch.get('q-inv'):
    mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VFRONT na ].')
    climb_gwo.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VFRONT na ].')
  mylang.add('head-initial-head-nexus := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')
  mylang.add('head-comp-phrase := basic-head-comp-share-vc.')
  mylang.add('head-comp-phrase-2 := basic-head-comp-share-vc.')
  if ch.get('vc-placement') == 'post':
    mylang.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
    mylang.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')
    if ch.get('aux-comp-order') == 'before':
      mylang.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
      mylang.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')
      climb_gwo.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
      climb_gwo.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')
  else:
    mylang.add('general-head-comp-vc-phrase := basic-head-comp-share-vc.')
    mylang.add('head-comp-2-vc-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('general-head-comp-vc-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('head-comp-2-vc-phrase := basic-head-comp-share-vc.')

  climb_gwo.add('head-initial-head-nexus := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')
  climb_gwo.add('head-comp-phrase := basic-head-comp-share-vc.')
  climb_gwo.add('head-comp-phrase-2 := basic-head-comp-share-vc.')

  aux2nd = ''
  if ch.get('v2-analysis') == 'filler-gap':
    aux2nd = 'general-aux-2nd-comp'
  else:
    aux2nd = 'aux-2nd-comp-phrase'
  mylang.add(aux2nd + ' := basic-head-comp-share-vc.')
  mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC na-or-+ ].')

  climb_gwo.add(aux2nd + ' := basic-head-comp-share-vc.')
  climb_gwo.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC na-or-+ ].')

#with revised rules for object generation, languages with fixed argument order
#shouldn't use the change-arg-order-rule anymore

  if ch.get('argument-order') != 'fixed': 
    mylang.add('change-arg-order-rule := const-val-change-only-lex-rule & \
 [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj, \
			    COMPS < #comp2, #comp1 >,\
			    SPR #spr,\
			    SPEC #spec ],\
		      VC #vc,\
                      VFRONT -, \
		      SECOND #sd & - ], \
   DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,\
				COMPS < #comp1, #comp2 >,\
				SPR #spr,\
				SPEC #spec ],\
			  HEAD verb & [ AUX - ],\
			  VC #vc,\
                          VFRONT +, \
			  SECOND #sd   ] ].')

    climb_gwo.add('change-arg-order-rule := const-val-change-only-lex-rule & \
 [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj, \
			    COMPS < #comp2, #comp1 >,\
			    SPR #spr,\
			    SPEC #spec ],\
		      VC #vc,\
                      VFRONT -, \
		      SECOND #sd & - ], \
   DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,\
				COMPS < #comp1, #comp2 >,\
				SPR #spr,\
				SPEC #spec ],\
			  HEAD verb & [ AUX - ],\
			  VC #vc,\
                          VFRONT +, \
			  SECOND #sd   ] ].')

    lrules.add('change-arg-order := change-arg-order-rule.')
    climb_gwo.add('change-arg-order := change-arg-order-rule.',section='lrules')
    if ch.get('edge-related-res') == 'yes':
      mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.EDGE #ed, \
                                          DTR.SYNSEM.LOCAL.CAT.EDGE #ed ].')
      climb_gwo.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.EDGE #ed, \
                                          DTR.SYNSEM.LOCAL.CAT.EDGE #ed ].')

  headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  nhddtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                 NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  
  mylang.add('head-initial-head-nexus := ' + headdtrval + ' ].')
  climb_gwo.add('head-initial-head-nexus := ' + headdtrval + ' ].')

  if ch.get('vc-placement') == 'post':
    mylang.add('comp-head-vc-phrase := ' + headdtrval + ' ].')
    mylang.add('comp-2-head-vc-phrase := ' + headdtrval + ' ].')
  else:
    climb_gwo.add('comp-head-vc-phrase := ' + headdtrval + ' ].')
    climb_gwo.add('comp-2-head-vc-phrase := ' + headdtrval + ' ].')
   
  mylang.add('basic-head-subj-phrase :+ '+ headdtrval + ' ].')
  mylang.add(aux2nd + ' := ' + nhddtrval + ' ].')
  mylang.add('basic-head-mod-phrase-simple :+ ' +headdtrval + ' ].')

  climb_gwo.add('basic-head-subj-phrase :+ '+ headdtrval + ' ].')
  climb_gwo.add(aux2nd + ' := ' + nhddtrval + ' ].')
  climb_gwo.add('basic-head-mod-phrase-simple :+ ' +headdtrval + ' ].')


  mylang.add('infl-lex-rule :+ [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                       DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  climb_gwo.add('infl-lex-rule :+ [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                       DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')

 # if ch.get('argument-order') == 'fixed':
 #   mylang.add('change-arg-order-rule := \
 #                       [ SYNSEM.LOCAL.CAT [ ARG-ORDER #ao, \
 #                                            ALLOWED-PART #ap ], \
 #                         DTR.SYNSEM.LOCAL.CAT [ ARG-ORDER #ao, \
 #                                                ALLOWED-PART #ap ]].')

  if ch.get('aux-comp-order') == 'both' or ch.get('aux-comp-order') == 'before':
    mylang.add('aux-comp-vc-phrase := ' + nhddtrval + ' ].')
    climb_gwo.add('aux-comp-vc-phrase := ' + nhddtrval + ' ].')
  if ch.get('aux-comp-order') == 'both' or ch.get('aux-comp-order') == 'after':
    mylang.add('comp-aux-vc-phrase := ' + nhddtrval + ' ].')
    climb_gwo.add('comp-aux-vc-phrase := ' + nhddtrval + ' ].')

####
# Additions needed to account for split clusters if arg-comp
# analysis is used.
##split clusters are for instance
#
#1) geslapen zou hij kunnen hebben.
#sleep-ptc would he can-inf have-inf.
#'he could have been sleeping'
#where the main verb is in the Vorfeld, but there are still verbs in 
#the right bracket.

def add_additional_arg_order_constraints(ch, mylang, climb_gwo):

#function adds some additional constraints for Dutch
#temporal solution to be improved 2011-11-03
  auxorder = ch.get('aux-comp-order')
  if auxorder != 'before':
    mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
    climb_gwo.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  if auxorder != 'after': 
    mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
    climb_gwo.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
##################TODOTODOTODO  
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
    climb_gwo.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER - ].')

### turning this off: this really does not make sense: this function is only called if argument-order is fuxed...
#  if not ch.get('argument-order') == 'fixed':
#    mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')
#    climb_gwo.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')

  mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  climb_gwo.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  aux2nd = ''
  if ch.get('v2-analysis') == 'filler-gap':
    aux2nd = 'general-aux-2nd-comp'
  else:
    aux2nd = 'aux-2nd-comp-phrase'

  mylang.add(aux2nd + ' := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  climb_gwo.add(aux2nd + ' := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
#######################TODOTODOTO
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('subj-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
    mylang.add('comp-head-phrase-2 := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')
    climb_gwo.add('subj-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
    climb_gwo.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
    climb_gwo.add('comp-head-phrase-2 := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')


#######################################################################
# B) Aux+verb rule analysis, proposed by Dan Flickinger,
# first described in Bender (2008)
# 


def create_aux_plus_verb_phrases(ch, mylang, climb_gwo):

  if ch.get('vc-placement') == 'pre':
    if ch.get('aux-comp-order') == 'before':
      currenttype = 'aux-comp-vc-phrase'
      mylang.add(currenttype + ' := head-initial & basic-aux-verb-rule & \
                            [ SYNSEM [ LOCAL.CAT [ MC -, \
                                                   VC na ], \
                                       LIGHT - ], \
                              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-+ ].')
      climb_gwo.add(currenttype + ' := head-initial & basic-aux-verb-rule & \
                            [ SYNSEM [ LOCAL.CAT [ MC -, \
                                                   VC na ], \
                                       LIGHT - ], \
                              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-+ ].')
  else:
    if ch.get('aux-comp-order') == 'after': 
      mylang.add('comp-aux-vc-phrase := basic-aux-verb-rule & head-final & \
                         [ SYNSEM [ LOCAL.CAT [ MC -, \
                                                VC + ], \
                                    LIGHT - ] ].')
      climb_gwo.add('comp-aux-vc-phrase := basic-aux-verb-rule & head-final & \
                         [ SYNSEM [ LOCAL.CAT [ MC -, \
                                                VC + ], \
                                    LIGHT - ] ].')

    elif ch.get('aux-comp-order') == 'both':
      mylang.add('comp-aux-vc-phrase := basic-aux-verb-rule & head-final & \
                                    [ SYNSEM.LOCAL.CAT [ MC -, \
                                                         VC +, \
                                                         NOMINAL #nl ], \
                                      HEAD-DTR.SYNSEM.LOCAL.CAT.VC na, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL #nl ].')
      mylang.add('aux-comp-vc-phrase := basic-aux-verb-rule & head-initial & \
                                  [ SYNSEM.LOCAL.CAT [ MC -, \
                                                       VC na ], \
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VC na-or-+, \
                                                                      NOMINAL - ] ].')
      climb_gwo.add('comp-aux-vc-phrase := basic-aux-verb-rule & head-final & \
                                    [ SYNSEM.LOCAL.CAT [ MC -, \
                                                         VC +, \
                                                         NOMINAL #nl ], \
                                      HEAD-DTR.SYNSEM.LOCAL.CAT.VC na, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL #nl ].')
      climb_gwo.add('aux-comp-vc-phrase := basic-aux-verb-rule & head-initial & \
                                  [ SYNSEM.LOCAL.CAT [ MC -, \
                                                       VC na ], \
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VC na-or-+, \
                                                                      NOMINAL - ] ].')

####ADD BEFORE AS AN OPTION

    elif ch.get('aux-comp-order') == 'before':
      mylang.add('aux-comp-vc-phrase := basic-aux-verb-rule & head-initial & \
                                  [ SYNSEM.LOCAL.CAT [ MC -, \
                                                       VC na ], \
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VC na-or-+ ] ].')
      climb_gwo.add('aux-comp-vc-phrase := basic-aux-verb-rule & head-initial & \
                                  [ SYNSEM.LOCAL.CAT [ MC -, \
                                                       VC na ], \
                                    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ VC na-or-+ ] ].')

######next constraint is shared between both alternative analyses
####WILL STAY UP THERE....
#      mylang.add(currenttype + ' := ' + currentsupertype + ' & \
#                           [ HEAD-DTR.SYNSEM.LIGHT +, \
#                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].') 



########################################################################
# specialized rules
#

def spec_word_order_phrases_aux_plus_verb(ch, mylang, climb_gwo):


  if ch.get('extraposition') == 'yes':
    stype = 'collect-anchor-phrase'
  else:
    stype = 'basic-binary-headed-phrase'

### sharing xarg with non-head-dtr else this goes wrong when more than
### one auxiliary is in the sentence (coindexing between xarg and subj is lost)
### 2013-09-05 this constraint should not be part of the basic-verbal-comp-rule
### it should not apply to object-raising verbs. 
### Moving it to appropriate subtype basic-aux-verb-rule

  mylang.add('basic-verbal-comp-rule := head-compositional & ' + stype + ' & head-valence-phrase & \
                [ SYNSEM.LOCAL [ CAT.VAL [ SPR #spr, \
                                           SPEC #spec ], \
		                 CONT.HOOK #hook ], \
                  C-CONT [ RELS <! !>, \
	                   HCONS <! !>, \
	                   HOOK #hook ], \
                  HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD verb, \
			                  CONT.HOOK #hook ], \
                  NON-HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD verb, \
					          VAL [ SPR #spr, \
                                                        SPEC #spec ] ] ] ].')
  climb_gwo.add('basic-verbal-comp-rule := head-compositional & ' + stype + ' & head-valence-phrase & \
                [ SYNSEM.LOCAL [ CAT.VAL [ SPR #spr, \
                                           SPEC #spec ], \
		                 CONT.HOOK #hook ], \
                  C-CONT [ RELS <! !>, \
	                   HCONS <! !>, \
	                   HOOK #hook ], \
                  HEAD-DTR.SYNSEM.LOCAL [ CAT.HEAD verb, \
			                  CONT.HOOK #hook ], \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb, \
					          VAL [ SPR #spr, \
                                                        SPEC #spec ] ] ].')
  bvcr1 = \
    'basic-verbal-comp-rule-1 := basic-verbal-comp-rule & \
     [ SYNSEM.LOCAL.CAT.VAL.COMPS #comps, \
       HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < #comp >, \
       NON-HEAD-DTR.SYNSEM #comp & \
                             [ LOCAL.CAT.VAL.COMPS #comps ] ].'
  bvcr2 = \
    'basic-verbal-comp-rule-2 := basic-verbal-comp-rule & \
     [ SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                              COMPS < #obj . #comps > ], \
       HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < #obj , #comp >, \
     				       SUBJ #subj ], \
       NON-HEAD-DTR.SYNSEM #comp & \
                            [ LOCAL.CAT.VAL.COMPS #comps ] ].'
  mylang.add(bvcr1)
  mylang.add(bvcr2)
  climb_gwo.add(bvcr1)
  climb_gwo.add(bvcr2)
   
  bar = \
    'basic-aux-verb-rule := basic-verbal-comp-rule-1 & \
        [ SYNSEM.LOCAL [ CAT [ VAL #val, \
	      	               HEAD.AUX + ], \
                         CONT.HOOK.XARG #xarg ], \
          NON-HEAD-DTR.SYNSEM.LOCAL [ CAT.VAL #val, \
                                      CONT.HOOK.XARG #xarg ] ].'
  mylang.add(bar)
  climb_gwo.add(bar)

  mv2vcp = \
   'mverb-2nd-vcomp-phrase := aux-comp-non-vc-phrase & \
      [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.AUX -, \
                                    VAL.SUBJ #subj ], \
        SYNSEM.LOCAL.CAT.VAL.SUBJ #subj ].'
  mylang.add(mv2vcp) 
  climb_gwo.add(mv2vcp) 

  ###for object raising, we also need rules for these verbs in the verbal cluster
  if ch.get('obj-raising') == 'yes':
    vmvvc = \
    '''comp-mverb-vc-phrase := basic-verbal-comp-rule-1 & head-final &
  [ SYNSEM.LOCAL.CAT [ HEADFINAL #hf,
                       HEAD.INV -,
                       VAL.SUBJ #subj,
                       EDGE #ed & na-or--,
                       MC -,
                       VC + ],
    HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL +,
                                VAL.SUBJ #subj,
                                MC -,
                                VC na ],
    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -,
                                    HEADFINAL #hf,
                                    EDGE #ed ] ].'''

    mvvvc = \
    '''mverb-comp-vc-phrase := basic-verbal-comp-rule-1 & head-initial &
  [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL -,
                                VC +,
                                VAL.SUBJ #subj ],
    SYNSEM.LOCAL.CAT [ HEADFINAL #hf,
                       HEAD.INV -,
                       VAL.SUBJ #subj,
                       EDGE #ed & bool,
                       MC -,
                       VC na ],
    NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -,
                                    HEADFINAL #hf,
                                    EDGE #ed,
                                    VC na-or-+ ] ].'''


    if ch.get('aux-comp-order') != 'before':
      mylang.add(vmvvc)
      climb_gwo.add(vmvvc)
    if ch.get('aux-comp-order') != 'after':
      mylang.add(mvvvc)
      climb_gwo.add(mvvvc)
    if ch.get('aux-comp-order') == 'both':
      mylang.add('comp-mverb-vc-phrase := [ SYNSEM.LOCAL.CAT.NOMINAL #nl, \
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL #nl ].')
      climb_gwo.add('comp-mverb-vc-phrase := [ SYNSEM.LOCAL.CAT.NOMINAL #nl, \
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL #nl ].')
      mylang.add('mverb-comp-vc-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL - ].')
      climb_gwo.add('mverb-comp-vc-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NOMINAL - ].')


  mytype = 'aux-comp-non-vc-phrase'
###to be adapted with future version (1 addition)
  mylang.add(mytype + ' := basic-verbal-comp-rule-1 & head-initial & \
                             [ SYNSEM [ LOCAL.CAT [ HEADFINAL +, \
			                            MC #mc, \
			                            HEAD.FORM finite ], \
	                                            LIGHT - ], \
                               HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc, \
                               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
  mylang.add('aux-2nd-comp-phrase := ' + mytype + ' & basic-aux-verb-rule.')
  mylang.add('aux-2nd-comp-phrase := [ HEAD-DTR.SYNSEM [ LOCAL.CAT.MC na, \
                                                         LIGHT + ] ].')

  climb_gwo.add(mytype + ' := basic-verbal-comp-rule-1 & head-initial & \
                             [ SYNSEM [ LOCAL.CAT [ HEADFINAL +, \
			                            MC #mc, \
			                            HEAD.FORM finite ], \
	                                            LIGHT - ], \
                               HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc, \
                               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
  climb_gwo.add('aux-2nd-comp-phrase := ' + mytype + ' & basic-aux-verb-rule.')
  climb_gwo.add('aux-2nd-comp-phrase := [ HEAD-DTR.SYNSEM [ LOCAL.CAT.MC na, \
                                                         LIGHT + ] ].')
  if ch.get('q-inv'):
##to be changed in future version (1 addition)
    mylang.add('mverb-2nd-vcomp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    mylang.add('aux-2nd-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    mylang.add('aux-1st-comp-phrase := ' + mytype + ' & basic-aux-verb-rule:.')
    mylang.add('aux-1st-comp-phrase := [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD.INV +, \
                                                                       MC na ], \
                                                           LIGHT + ], \
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
				                           COMPS < > ] ].')

    climb_gwo.add('mverb-2nd-vcomp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('aux-2nd-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('aux-1st-comp-phrase := ' + mytype + ' & basic-aux-verb-rule:.')
    climb_gwo.add('aux-1st-comp-phrase := [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD.INV +, \
                                                                       MC na ], \
                                                           LIGHT + ], \
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
				                           COMPS < > ] ].')
      

    comment = \
      'The sequence aux-1st-comp-inv-cl-head-adj leads to sentences like\n' + \
      '*Wird der mann schlafen nicht?, *Hat der mann geschlafen bestimmt.\n' + \
      'Using MODIFIED to prevent this from happening. ' + \
      'Replace with other (especially introduced) feature if necessary.'
    mylang.add('aux-1st-comp-phrase := [ SYNSEM.MODIFIED notmod-or-rmod ].', comment)
    climb_gwo.add('aux-1st-comp-phrase := [ SYNSEM.MODIFIED notmod-or-rmod ].', comment)
 

  mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                         HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')
  climb_gwo.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                         HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')
  if ch.get('vc-placement') == 'pre':
    mylang.add('head-initial-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                       HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')
    climb_gwo.add('head-initial-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                       HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')

  if ch.get('argument-order') == 'fixed':
    mylang.add('gen-vcomp-verb-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
    mylang.add('basic-verbal-comp-rule := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
    climb_gwo.add('gen-vcomp-verb-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
    climb_gwo.add('basic-verbal-comp-rule := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')

  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
    climb_gwo.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].') 

  if ch.get('aux-comp-order') == 'both':
    mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.NOMINAL + ].')
    mylang.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster', section='addenda')
    climb_gwo.add('head-final-invc := [ SYNSEM.LOCAL.CAT.NOMINAL + ].')
    climb_gwo.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster\nsection=addenda')

  mylang.add('gen-vcomp-verb-2nd-phrase := head-final & \
                              [ SYNSEM.LOCAL.CAT [ MC +, \
		                                   HEADFINAL #hf, \
		                                   VAL.SUBJ < [] >  ], \
                                HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
			       	                            HEADFINAL #hf ], \
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')

  climb_gwo.add('gen-vcomp-verb-2nd-phrase := head-final & \
                              [ SYNSEM.LOCAL.CAT [ MC +, \
		                                   HEADFINAL #hf, \
		                                   VAL.SUBJ < [] >  ], \
                                HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
			       	                            HEADFINAL #hf ], \
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
  if ch.get('q-inv'):
    mylang.add('gen-vcomp-verb-2nd-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('gen-vcomp-verb-2nd-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

  mylang.add('comp-aux-2nd-phrase := basic-aux-verb-rule & gen-vcomp-verb-2nd-phrase.')    
  mylang.add('vcomp-mverb-2nd-phrase := gen-vcomp-verb-2nd-phrase & \
                                      basic-verbal-comp-rule-2 & \
                                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
  climb_gwo.add('comp-aux-2nd-phrase := basic-aux-verb-rule & gen-vcomp-verb-2nd-phrase.')    
  climb_gwo.add('vcomp-mverb-2nd-phrase := gen-vcomp-verb-2nd-phrase & \
                                      basic-verbal-comp-rule-2 & \
                                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].') 
  
  if ch.get('split-cluster') == 'yes':
    split_cluster_phrases_aux_plus_verb(ch, mylang, climb_gwo)

   
def split_cluster_phrases_aux_plus_verb(ch, mylang, climb_gwo):


  mylang.add('cat :+ [ VFRONT bool ].', 'VFRONT checks whether the vorfeld contains a partial verbal cluster', section='addenda')
  mylang.add('head :+ [ DTR-FORM form ].',section='addenda')

  climb_gwo.add('cat :+ [ VFRONT bool ].', 'VFRONT checks whether the vorfeld contains a partial verbal cluster\nsection=addenda')
  climb_gwo.add('head :+ [ DTR-FORM form ].',comment='section=addenda')
  
  if ch.get('wh-questions') == 'yes':
    mylang.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
    climb_gwo.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')

###flexible aux-comp-order always uses NOMINAL with aux+verb analysis
###only add for fixed aux-comp-order + split cluster

  if ch.get('aux-comp-order') != 'both': 
    mylang.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster', section='addenda')
    climb_gwo.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster\n section=addenda')

  mylang.add('special-basic-aux-verb-rule :=  head-compositional & \
                           basic-binary-headed-phrase & head-valence-phrase & \
           [ SYNSEM.LOCAL [ CAT.VAL #val, \
                            CONT.HOOK #hook ], \
             C-CONT [ RELS <! !>, \
                      HCONS <! !>, \
                      HOOK #hook ], \
             HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD verb & [ AUX + ], \
                                           VAL.COMPS.FIRST.LOCAL.CONT #cont ], \
                                     CONT.HOOK #hook ], \
             NON-HEAD-DTR.SYNSEM  [ LOCAL [ CAT [ HEAD verb, \
                                                  VAL #val ], \
                                                  CONT #cont ] ] ].')

  climb_gwo.add('special-basic-aux-verb-rule :=  head-compositional & \
                           basic-binary-headed-phrase & head-valence-phrase & \
           [ SYNSEM.LOCAL [ CAT.VAL #val, \
                            CONT.HOOK #hook ], \
             C-CONT [ RELS <! !>, \
                      HCONS <! !>, \
                      HOOK #hook ], \
             HEAD-DTR.SYNSEM.LOCAL [ CAT [ HEAD verb & [ AUX + ], \
                                           VAL.COMPS.FIRST.LOCAL.CONT #cont ], \
                                     CONT.HOOK #hook ], \
             NON-HEAD-DTR.SYNSEM  [ LOCAL [ CAT [ HEAD verb, \
                                                  VAL #val ], \
                                                  CONT #cont ] ] ].')

  mylang.add('gen-vcomp-verb-2nd-phrase := \
                       [ SYNSEM.LOCAL.CAT.HEAD [ DTR-FORM #dform \
                                                 FORM finite ], \
                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT. HEAD.FORM #dform ].')
  climb_gwo.add('gen-vcomp-verb-2nd-phrase := \
                       [ SYNSEM.LOCAL.CAT.HEAD [ DTR-FORM #dform \
                                                 FORM finite ], \
                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT. HEAD.FORM #dform ].')
  if ch.get('q-inv'):
    mylang.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
  mylang.add('comp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT - ].')
  mylang.add('noncomp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & special-basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT +, \
                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
  climb_gwo.add('comp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT - ].')
  climb_gwo.add('noncomp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & special-basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT +, \
                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
  
  mylang.add('special-insert-aux-phrase := headed-phrase & \
         [ SYNSEM.LOCAL [ CONT [ HOOK #hook, \
		                 RELS [ LIST #first,\
				 LAST #last ], \
			 HCONS [ LIST [ FIRST [ HARG #harg1,\
				        	LARG #larg1 ],\
					REST #scfirst ],\
				 LAST #sclast ] ],\
		          CAT [ VAL #val,\
			        MC #mc,\
			        VFRONT - ] ],\
            HEAD-DTR #firstarg & head-initial & \
              [ SYNSEM [ LOCAL [ CAT [ HEAD verb & [ AUX +,\
						     DTR-FORM #dform ],\
				       VAL #val & [ SUBJ < >,\
						    COMPS < > ],\
				       MC #mc,\
				       VFRONT + ],\
				 CONT [ HOOK #hook,\
				 HCONS [ LIST.FIRST [ HARG #harg1, \
                                                      LARG #larg2 ] ] ] ] ] ], \
           INSERT-DTR #secarg & [ SYNSEM [ LOCAL [ CAT [ HEAD verb & [ AUX + ],\
						         VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM #dform ],\
COORD -, \
					            CONT [ HOOK.LTOP #larg1,\
						    HCONS [ LIST.FIRST [ LARG #larg2 ] ] ] ] ],\
			                   INFLECTED infl-satisfied ], \
                                  C-CONT [ RELS [ LIST #middle2,\
		                                  LAST #last ],\
	                          HCONS [ LIST #scmiddle2,\
		                          LAST #sclast ] ],\
           ARGS < #firstarg & [ SYNSEM.LOCAL local & \
                                     [ CONT [ RELS [ LIST #first,\
						     LAST #middle1 ],\
				       HCONS [ LIST [ FIRST [ ],\
						      REST #scfirst ],\
					       LAST #scmiddle1 ] ] ] ], \
	          #secarg  & [ SYNSEM.LOCAL local & \
                                     [ CONT [ RELS [ LIST #middle1,\
						     LAST #middle2 ],\
				       HCONS [ LIST #scmiddle1,\
					       LAST #scmiddle2 ] ] ] ] > ].')
  climb_gwo.add('special-insert-aux-phrase := headed-phrase & \
         [ SYNSEM.LOCAL [ CONT [ HOOK #hook, \
		                 RELS [ LIST #first,\
				 LAST #last ], \
			 HCONS [ LIST [ FIRST [ HARG #harg1,\
				        	LARG #larg1 ],\
					REST #scfirst ],\
				 LAST #sclast ] ],\
		          CAT [ VAL #val,\
			        MC #mc,\
			        VFRONT - ] ],\
            HEAD-DTR #firstarg & head-initial & \
              [ SYNSEM [ LOCAL [ CAT [ HEAD verb & [ AUX +,\
						     DTR-FORM #dform ],\
				       VAL #val & [ SUBJ < >,\
						    COMPS < > ],\
				       MC #mc,\
				       VFRONT + ],\
				 CONT [ HOOK #hook,\
				 HCONS [ LIST.FIRST [ HARG #harg1, \
                                                      LARG #larg2 ] ] ] ] ] ], \
           INSERT-DTR #secarg & [ SYNSEM [ LOCAL [ CAT [ HEAD verb & [ AUX + ],\
						         VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM #dform ],\
COORD -, \
					            CONT [ HOOK.LTOP #larg1,\
						    HCONS [ LIST.FIRST [ LARG #larg2 ] ] ] ] ],\
			                   INFLECTED infl-satisfied ], \
                                  C-CONT [ RELS [ LIST #middle2,\
		                                  LAST #last ],\
	                          HCONS [ LIST #scmiddle2,\
		                          LAST #sclast ] ],\
           ARGS < #firstarg & [ SYNSEM.LOCAL local & \
                                     [ CONT [ RELS [ LIST #first,\
						     LAST #middle1 ],\
				       HCONS [ LIST [ FIRST [ ],\
						      REST #scfirst ],\
					       LAST #scmiddle1 ] ] ] ], \
	          #secarg  & [ SYNSEM.LOCAL local & \
                                     [ CONT [ RELS [ LIST #middle1,\
						     LAST #middle2 ],\
				       HCONS [ LIST #scmiddle1,\
					       LAST #scmiddle2 ] ] ] ] > ].')
  if ch.get('q-inv'):
    mylang.add('special-insert-aux-phrase := \
                  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV -, \
                    INSERT-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('special-insert-aux-phrase := \
                  [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV -, \
                    INSERT-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

  mylang.add('decl-head-subj-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('basic-head-comp-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('basic-head-mod-phrase-simple :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  climb_gwo.add('decl-head-subj-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  climb_gwo.add('basic-head-comp-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  climb_gwo.add('basic-head-mod-phrase-simple :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')


  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-vcomp-verb-2nd-rule := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
    climb_gwo.add('gen-vcomp-verb-2nd-rule := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

  if ch.get('argument-order') == 'fixed':
    mylang.add('noncomp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
    climb_gwo.add('noncomp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')


def create_germanic_adjunct_phrases(ch, mylang, rules, climb_gwo):
###need to separate adjunctial from adjectival phrases, due to different
###wo interactions
  head_res = '+'  
  if ch.get('mod-noun') == 'yes':
    head_res += 'n'
  if ch.get('rel-clause') == 'yes':
      head_res += 'v'
  head_res += 'r'
  head_res += 'p'

  mylang.add('adjunct-head-phrase := basic-head-mod-phrase-simple & \
                  [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + head_res + ' ].')
  climb_gwo.add('adjunct-head-phrase := basic-head-mod-phrase-simple & \
                  [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + head_res + ' ].')
 
###if no auxiliary, direct attachment v2
###to do: introduce EDGE if not already present
  if not ch.get('edge-related-res') == 'yes':
    mylang.add('cat :+ [ EDGE luk ].')
    climb_gwo.add('cat :+ [ EDGE luk ].')

  mylang.add('head-2nd-adj-phrase := head-initial-head-nexus & adjunct-head-phrase & \
 [ SYNSEM.LOCAL.CAT [ EDGE #edge, \
                      HEAD +nv ], \
   HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
  climb_gwo.add('head-2nd-adj-phrase := head-initial-head-nexus & adjunct-head-phrase & \
 [ SYNSEM.LOCAL.CAT [ EDGE #edge, \
                      HEAD +nv ], \
   HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
####only verbs can be modified by post-modifying adverbs
  if ch.get('q-inv') and ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-2nd-adj-phrase := [ HEAD-DTR.SYNSEM.MODIFIED hasmod ].')
    climb_gwo.add('head-2nd-adj-phrase := [ HEAD-DTR.SYNSEM.MODIFIED hasmod ].')

  mylang.add('head-2nd-adj-int-phrase := head-adj-int-phrase & head-2nd-adj-phrase.')
  mylang.add('head-2nd-adj-scop-phrase := head-adj-scop-phrase & head-2nd-adj-phrase.')
  climb_gwo.add('head-2nd-adj-int-phrase := head-adj-int-phrase & head-2nd-adj-phrase.')
  climb_gwo.add('head-2nd-adj-scop-phrase := head-adj-scop-phrase & head-2nd-adj-phrase.')

  rules.add('head-2nd-adj-int := head-2nd-adj-int-phrase.')
  rules.add('head-2nd-adj-scop := head-2nd-adj-scop-phrase.')
  climb_gwo.add('head-2nd-adj-int := head-2nd-adj-int-phrase.',section='rules')
  climb_gwo.add('head-2nd-adj-scop := head-2nd-adj-scop-phrase.',section='rules')

 
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('adj-head-2nd-int-phrase := adj-head-int-phrase & head-final-head-nexus & adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD +nv ].')
 
    mylang.add('adj-head-2nd-scop-phrase := adj-head-scop-phrase & head-final-head-nexus & adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD +nv ].')
    climb_gwo.add('adj-head-2nd-int-phrase := adj-head-int-phrase & head-final-head-nexus & adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD +nv ].')
 
    climb_gwo.add('adj-head-2nd-scop-phrase := adj-head-scop-phrase & head-final-head-nexus & adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD +nv ].')

    rules.add('adj-head-2nd-int := adj-head-2nd-int-phrase.')
    rules.add('adj-head-2nd-scop := adj-head-2nd-scop-phrase.')
    climb_gwo.add('adj-head-2nd-int := adj-head-2nd-int-phrase.',section='rules')
    climb_gwo.add('adj-head-2nd-scop := adj-head-2nd-scop-phrase.',section='rules')
    #constraint to exclude scopal single word adverbs
    if ch.get('fronted-scopal') == 'no':
      mylang.add('adj-head-2nd-scop-phrase := [ NON-HEAD-DTR.SYNSEM.LIGHT - ].')
      climb_gwo.add('adj-head-2nd-scop-phrase := [ NON-HEAD-DTR.SYNSEM.LIGHT - ].')


### TO DO: FIND OUT ABOUT DANISH
### 2013-11-17 removing requirement, Danish has adjuncts before cluster
 # if ch.get('vc-placement') == 'post':

  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'
  mylang.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' + hf_invc +' & adjunct-head-phrase.')
  mylang.add('adj-head-scop-vc-phrase := adj-head-scop-phrase & ' + hf_invc +  ' & adjunct-head-phrase.')    
  rules.add('adj-head-int-vc := adj-head-int-vc-phrase.')
  rules.add('adj-head-scop-vc := adj-head-scop-vc-phrase.')
  climb_gwo.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' + hf_invc +' & adjunct-head-phrase.')
  climb_gwo.add('adj-head-scop-vc-phrase := adj-head-scop-phrase & ' + hf_invc +  ' & adjunct-head-phrase.')    
  climb_gwo.add('adj-head-int-vc := adj-head-int-vc-phrase.',section='rules')
  climb_gwo.add('adj-head-scop-vc := adj-head-scop-vc-phrase.',section='rules')


def create_rel_clause_phrases(mylang, rules, climb_gwo, ch):

  basic_rel = \
   '''basic-rel-phrase := basic-binary-marker-phrase &
  [ SYNSEM [ LOCAL.CAT.HEAD.MOD < [ LOCAL intersective-mod & 
                                   [ CONT.HOOK [ LTOP #hand,
                                                 INDEX #ind,
						 XARG #xarg ] ] ] >,	
             NON-LOCAL.SLASH #slash ],	
    NON-MARKER-DTR.SYNSEM [ NON-LOCAL [ QUE 0-dlist,
				        REL 1-dlist & 
                                               [ LIST < [ INDEX #ind ] > ] ],
			  LOCAL.CONT.HOOK [ LTOP #hand,
					    XARG #xarg ] ],
    MARKER-DTR.SYNSEM [ LOCAL.CONT.HOOK [ LTOP #ltop,
			  	          INDEX #index ],
                        NON-LOCAL.SLASH #slash ],
    C-CONT [ HOOK [ LTOP #ltop,
		    INDEX #index ] ] ].'''
  
  mylang.add(basic_rel)
  climb_gwo.add(basic_rel)

  rel_ph = \
   '''rel-phrase := basic-rel-phrase & marker-final-phrase &
     [ SYNSEM.LOCAL.CAT [ HEAD verb & [ FORM #form & finite,
   				        AUX #aux,
                                        MOD < [ LOCAL.CAT.VAL.SPR < [ ] > ] > ],
		          VAL [ SUBJ < >,
			        COMPS < >,
			        SPR < >,
			        SPEC < > ],
		          MC #mc & - ],
       MARKER-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ FORM #form,
			 			   AUX #aux ],
				       MC #mc ] ].'''
  mylang.add(rel_ph)
  climb_gwo.add(rel_ph)
#  mylang.add('rel-arg0-phrase := rel-phrase & \
#   [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CONT.HOOK.INDEX #index ]>, \
#     NON-MARKER-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX #index ].')
#  mylang.add('rel-arg2-phrase := rel-phrase & \
#   [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CONT.HOOK.INDEX #index ]>, \
#     NON-MARKER-DTR.SYNSEM.LOCAL.CONT.RELS.LIST.FIRST.ARG2 #index ].')

  mylang.add('rel-comp-phrase := rel-phrase & basic-marker-comp-phrase.')
  climb_gwo.add('rel-comp-phrase := rel-phrase & basic-marker-comp-phrase.')
#  mylang.add('rel-arg2-comp-phrase := rel-arg2-phrase & \
#                                          basic-marker-comp-phrase & \
#              [ NON-MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD adp ].')
  mylang.add('rel-subj-phrase := rel-phrase & basic-marker-subj-phrase.')
  climb_gwo.add('rel-subj-phrase := rel-phrase & basic-marker-subj-phrase.')
  
  mylang.add('rel-mod-phrase := rel-phrase & marker-mod-phrase-simple & \
       [ SYNSEM.LOCAL.CAT.HEAD #head, \
         NON-MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD adp & \
                                      [ MOD < [ LOCAL.CAT.HEAD #head ] > ] ].')
  climb_gwo.add('rel-mod-phrase := rel-phrase & marker-mod-phrase-simple & \
       [ SYNSEM.LOCAL.CAT.HEAD #head, \
         NON-MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD adp & \
                                      [ MOD < [ LOCAL.CAT.HEAD #head ] > ] ].')

  rules.add('rel-comp := rel-comp-phrase.')
  rules.add('rel-subj := rel-subj-phrase.')
  rules.add('rel-mod := rel-mod-phrase.')
  climb_gwo.add('rel-comp := rel-comp-phrase.',section='rules')
  climb_gwo.add('rel-subj := rel-subj-phrase.',section='rules')
  climb_gwo.add('rel-mod := rel-mod-phrase.',section='rules')
####TO DO: ADD REL-RESTRICTION TO HEAD-COMP ETC IF NO WH-QUESTIONS IN THE
####GRAMMAR

  mylang.add('basic-head-comp-phrase :+ \
                   [ SYNSEM.NON-LOCAL.REL #rel, \
                     NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')
  mylang.add('head-spec-phrase := [ SYNSEM.NON-LOCAL.REL #rel, \
                  NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')
  climb_gwo.add('basic-head-comp-phrase :+ \
                   [ SYNSEM.NON-LOCAL.REL #rel, \
                     NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')
  climb_gwo.add('head-spec-phrase := [ SYNSEM.NON-LOCAL.REL #rel, \
                  NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')

  if ch.get('wh-rel') or ch.get('non-wh-rel'):
    mylang.add('infl-satisfied :+ [ WH-FLAG luk ].')
    mylang.add('infl-wh := infl-satisfied & [ WH-FLAG + ].')
    mylang.add('infl-non-wh := infl-satisfied & [ WH-FLAG na ].')
    climb_gwo.add('infl-satisfied :+ [ WH-FLAG luk ].')
    climb_gwo.add('infl-wh := infl-satisfied & [ WH-FLAG + ].')
    climb_gwo.add('infl-non-wh := infl-satisfied & [ WH-FLAG na ].')

####assuming for now that other way around does not occur in Germanic
####we know from English that having one of the two in both positions (wh)
####occurs

    if ch.get('non-wh-rel') != 'embedded' and ch.get('non-wh-rel') != 'both':
      mylang.add('head-comp-sub-phrase := [ NON-HEAD-DTR.INFLECTED infl-wh ].')
      climb_gwo.add('head-comp-sub-phrase := [ NON-HEAD-DTR.INFLECTED infl-wh ].')
    if ch.get('wh-rel') != 'non-embedded' and ch.get('wh-rel') != 'both':
      mylang.add('rel-phrase := [ NON-MARKER-DTR.INFLECTED infl-non-wh ].')
      climb_gwo.add('rel-phrase := [ NON-MARKER-DTR.INFLECTED infl-non-wh ].')
########################################################
# I) MC analysis (which follows from the original v-2 analysis
# from the Grammar Matrix
#

def mc_v2_word_order(ch, mylang, rules, climb_gwo):
#
# head-final phrases with head in second position
#

###making sure language properties of having pre- or postpositions are respected
###calling head_rest function to see if general head-comp to build clauses
###should be restricted and, if so, to what
  head_rest = get_head_restr_for_non_clausal_head_comp(ch)

  mylang.add('head-final-head-nexus := nonverbal-comp-phrase & \
                                       [ SYNSEM.LOCAL.CAT.MC +,\
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')

####head-initial-head-nexus must have HEAD-DTR [ MC + ] for MC analysis,
#### [ MC na ] for filler-gap  
  mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC + ].')
  mylang.add('head-comp-phrase-2 := nonverbal-comp-phrase.')

  climb_gwo.add('head-final-head-nexus := nonverbal-comp-phrase & \
                                       [ SYNSEM.LOCAL.CAT.MC +,\
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')

####head-initial-head-nexus must have HEAD-DTR [ MC + ] for MC analysis,
#### [ MC na ] for filler-gap  
  climb_gwo.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC + ].')
  climb_gwo.add('head-comp-phrase-2 := nonverbal-comp-phrase.')
  if ch.get('wh-questions') == 'yes':
    mylang.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
    mylang.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
    climb_gwo.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
    climb_gwo.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
  
  if ch.get('q-inv'):  
    mylang.add('head-final-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('head-final-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

####TO BE MOVED
  if ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL #hf ].')
    climb_gwo.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL #hf ].')
  
  if head_rest:
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')
    climb_gwo.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')


  if ch.get('wh-questions') == 'yes':
    wh_mc_word_order_phrases(mylang, climb_gwo)
    wh_mc_word_order_rules(rules, climb_gwo)


def wh_mc_word_order_phrases(mylang, climb_gwo):
  
  mylang.add('wh-subj-head-phrase := basic-head-wh-subj-phrase & head-final-head-nexus.')
  mylang.add('wh-adj-head-2nd-int-phrase := wh-adj-head-int-phrase & \
               head-final-head-nexus & wh-adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  mylang.add('comp-head-phrase := head-non-wh-or-rel.')
  mylang.add('wh-comp-head-phrase := head-wh & basic-head-1st-comp-phrase & \
                        head-final-head-nexus.')

  climb_gwo.add('wh-subj-head-phrase := basic-head-wh-subj-phrase & head-final-head-nexus.')
  climb_gwo.add('wh-adj-head-2nd-int-phrase := wh-adj-head-int-phrase & \
               head-final-head-nexus & wh-adjunct-head-phrase & [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  climb_gwo.add('comp-head-phrase := head-non-wh-or-rel.')
  climb_gwo.add('wh-comp-head-phrase := head-wh & basic-head-1st-comp-phrase & \
                        head-final-head-nexus.')


def wh_mc_word_order_rules(rules, climb_gwo):
  climb_gwo.set_section('rules')
  rules.add('wh-subj-head := wh-subj-head-phrase.')
  rules.add('wh-comp-head := wh-comp-head-phrase.')
  rules.add('wh-adj-head-2nd-int := wh-adj-head-2nd-int-phrase.')
  climb_gwo.add('wh-subj-head := wh-subj-head-phrase.')
  climb_gwo.add('wh-comp-head := wh-comp-head-phrase.')
  climb_gwo.add('wh-adj-head-2nd-int := wh-adj-head-2nd-int-phrase.')
  climb_gwo.set_section('mylang')

#########################################################################
#
# II. Filler-gap analysis: standard HPSG for German v-2
#

def filler_gap_word_order(ch, mylang, climb_gwo):
####head-initial-head-nexus must have HEAD-DTR [ MC + ] for MC analysis,
#### [ MC na ] for filler-gap  
  mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].')
  mylang.add('general-aux-2nd-comp := [ SYNSEM.LOCAL.CAT.MC na ].')
  mylang.add('aux-2nd-comp-phrase := general-aux-2nd-comp & basic-head-1st-comp-phrase.')
  mylang.add('aux-2nd-comp-phrase-2 := general-aux-2nd-comp & \
                                            basic-head-2nd-comp-phrase & \
              [ SYNSEM.LOCAL.CAT [ SECOND na, \
                                   HEAD.INV - ] ].')

  mylang.add('head-final-invc := general-head-final-invc.')


  climb_gwo.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].')
  climb_gwo.add('general-aux-2nd-comp := [ SYNSEM.LOCAL.CAT.MC na ].')
  climb_gwo.add('aux-2nd-comp-phrase := general-aux-2nd-comp & basic-head-1st-comp-phrase.')
  climb_gwo.add('aux-2nd-comp-phrase-2 := general-aux-2nd-comp & \
                                            basic-head-2nd-comp-phrase & \
              [ SYNSEM.LOCAL.CAT [ SECOND na, \
                                   HEAD.INV - ] ].')

  climb_gwo.add('head-final-invc := general-head-final-invc.')

# additional constraints for fixed argument order:
#turning this off: no longer works for filler-gap 2013-10-16 (TO DO: FIND OUT
#HOW TO SOLVE THIS ISSUE
 # if ch.get('argument-order') == 'fixed':
#    mylang.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 1-dlist].')
 #   mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
 #                                                        COMPS < > ] ].')
 #   climb_gwo.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
 #                                                        COMPS < > ] ].')
#                                  NON-HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 1-dlist ].') 

 # should not be necessary anymore with revised slash constraints
 #
 # hd_slash_share = '[ SYNSEM.NON-LOCAL.SLASH #slash, \
 #                     HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash ].'
 # mylang.add('basic-head-mod-phrase-simple :+ ' + hd_slash_share)
  
  if ch.get('clz-optionality'):
    mylang.add('create-informal-vcomp-phrase := \
               [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 0-dlist ] >, \
                 SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
    climb_gwo.add('create-informal-vcomp-phrase := \
               [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 0-dlist ] >, \
                 SYNSEM.NON-LOCAL.SLASH 0-dlist ].')

  mylang.add('subj-v-inv-lrule := [ SYNSEM [ NON-LOCAL.SLASH #slash, \
                                             LOCAL [ CAT.HEAD.AUX #aux ] ], \
                                    DTR.SYNSEM [ NON-LOCAL.SLASH #slash, \
                                                 LOCAL [ CAT.HEAD.AUX #aux ] ] ].')
  mylang.add('int-cl := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-dlist \
                                                           & [ LIST < > ] ].')
  climb_gwo.add('subj-v-inv-lrule := [ SYNSEM [ NON-LOCAL.SLASH #slash, \
                                             LOCAL [ CAT.HEAD.AUX #aux ] ], \
                                    DTR.SYNSEM [ NON-LOCAL.SLASH #slash, \
                                                 LOCAL [ CAT.HEAD.AUX #aux ] ] ].')
  climb_gwo.add('int-cl := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-dlist \
                                                           & [ LIST < > ] ].')
  
  comment = ''';;;;;;;;;;;;;extraction phrases \
                                              \
               ;;; declarative sentences: result of extraction \
               ;;; not allowing inverted head-dtr '''

###CHANGE: CHECK IF WH- TURNED ON
#  mylang.add('general-filler-head-phrase := basic-head-filler-phrase & \
#                                                     head-final & \
#                [ SYNSEM.LOCAL [ CAT [ MC +, \#
#		                       VFRONT #vf ], \
#		                 CONT [ HOOK.INDEX.SF prop-or-ques ] ], \
#                  HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ FORM finite, \
# 					                    INV - ], \
#                                              MC na, \
#				              VFRONT #vf & -, \
#				              VAL [ SUBJ < >, \
#				                    COMPS < >, \
#				                    SPR < >, \
#				                    SPEC < > ] ] ].', comment)
#
 # mylang.add('filler-head-phrase := general-filler-head-phrase & \
#                [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE 0-dlist ].')

#  mylang.add('wh-filler-head-phrase := general-filler-head-phrase & \
#                [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques, \
#                  NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE 1-dlist ].')
#  comment = ''';;; extracted phrases \
#               ;;; in German, only verbs allow extraction (except for wh)'''

#  mylang.add('basic-extracted-arg-phrase :+ \
#                [ HEAD-DTR.SYNSEM [ MODIFIED notmod, \
#				    LOCAL.CAT.HEAD verb & [ INV - ] ] ].', comment)
            
#  mylang.add('extracted-comp-phrase := basic-extracted-comp-phrase & \
#                [ SYNSEM.LOCAL.CAT [ SECOND #sec, \
#                                     VAL.COMPS < > ], \
#                  HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.SUBJ < >, \
#			                      SECOND #sec ] ].')

 # comment = ''';;;no non-verbal-comp extraction allowed after aux-2nd-comp \
 #              ;;;aux-2nd-comp-2 does however allow for this \
 #              ;;;Dich sah der Mann schlafen.''' 

  mylang.add('extracted-non-verbal-comp-phrase := extracted-comp-phrase & \
 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.COMPS.FIRST.LOCAL.CAT.HEAD +njrpcdmo ] ].', comment)
  climb_gwo.add('extracted-non-verbal-comp-phrase := extracted-comp-phrase & \
 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.COMPS.FIRST.LOCAL.CAT.HEAD +njrpcdmo ] ].', comment)

  mylang.add('extracted-verbal-comp-phrase := extracted-comp-phrase & \
 [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT [ HEAD verb, \
							 VAL.SUBJ <[ ]>, \
							 MC - ] ].')
  climb_gwo.add('extracted-verbal-comp-phrase := extracted-comp-phrase & \
 [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT [ HEAD verb, \
							 VAL.SUBJ <[ ]>, \
							 MC - ] ].')
  mylang.add('ger-extracted-adj-phrase := extracted-adj-phrase & \
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb & [ FORM finite ], \
			       VAL [ SUBJ < >, \
				     COMPS < >, \
				     SPR < > ], \
			       SECOND #scd ], \
                      NON-LOCAL.QUE #que ], \
   SYNSEM [ NON-LOCAL [ SLASH <! [ CAT [ HEAD +nrp & [ PRD - ], \
				     VAL [ SUBJ < >, \
					   COMPS < >, \
					   SPR < > ] ] ]!>, \
                        QUE #que ], \
	    LOCAL.CAT.SECOND #scd ] ].')

  climb_gwo.add('ger-extracted-adj-phrase := extracted-adj-phrase & \
 [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb & [ FORM finite ], \
			       VAL [ SUBJ < >, \
				     COMPS < >, \
				     SPR < > ], \
			       SECOND #scd ], \
                      NON-LOCAL.QUE #que ], \
   SYNSEM [ NON-LOCAL [ SLASH <! [ CAT [ HEAD +nrp & [ PRD - ], \
				     VAL [ SUBJ < >, \
					   COMPS < >, \
					   SPR < > ] ] ]!>, \
                        QUE #que ], \
	    LOCAL.CAT.SECOND #scd ] ].')

  mylang.add('extracted-subj-phrase := basic-extracted-subj-phrase & \
                 [ SYNSEM.LOCAL.CAT [ MC #mc, \
		                      SECOND #sec ], \
                   HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc, \
			                       SECOND #sec ] ].')
  climb_gwo.add('extracted-subj-phrase := basic-extracted-subj-phrase & \
                 [ SYNSEM.LOCAL.CAT [ MC #mc, \
		                      SECOND #sec ], \
                   HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc, \
			                       SECOND #sec ] ].')

def filler_gap_rules(ch, rules, climb_gwo):
  climb_gwo.set_section('rules')
  rules.add('aux-2nd-comp-2 := aux-2nd-comp-phrase-2.')
  rules.add('extracted-subj := extracted-subj-phrase.')
  rules.add('extracted-vcomp := extracted-verbal-comp-phrase.')
  rules.add('extracted-comp := extracted-non-verbal-comp-phrase.')
  rules.add('extracted-adj := ger-extracted-adj-phrase.')
  rules.add('filler-head := filler-head-phrase.')

  climb_gwo.add('aux-2nd-comp-2 := aux-2nd-comp-phrase-2.')
  climb_gwo.add('extracted-subj := extracted-subj-phrase.')
  climb_gwo.add('extracted-vcomp := extracted-verbal-comp-phrase.')
  climb_gwo.add('extracted-comp := extracted-non-verbal-comp-phrase.')
  climb_gwo.add('extracted-adj := ger-extracted-adj-phrase.')
  climb_gwo.add('filler-head := filler-head-phrase.')
  if ch.get('wh-questions') == 'yes':
    rules.add('wh-filler-head := wh-filler-head-phrase.')
    climb_gwo.add('wh-filler-head := wh-filler-head-phrase.')

  climb_gwo.set_section('mylang')
  
#########################################################################
#
# A.I. phrases and rules required to combine MC analysis and arg-comp
#

def mc_argcomp_word_order_phrases(ch, mylang, rules, climb_gwo):

  mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
  mylang.add('aux-2nd-comp-phrase := basic-head-1st-comp-phrase.')
  climb_gwo.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
  climb_gwo.add('aux-2nd-comp-phrase := basic-head-1st-comp-phrase.')
###Assuming no [subj + verb] in Vorfeld, despite occasional exceptions
###(NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ <[ ]>)
  mylang.add('gen-comp-aux-2nd-phrase := head-final &  \
                     [ SYNSEM.LOCAL.CAT [ MC +, \
		                          SECOND #scd ], \
                       HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
                                                   HEAD verb,\
		             	                   SECOND #scd ], \
                       NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
			                               HEAD verb, \
				                       VAL.SUBJ < [ ] >]].')
  climb_gwo.add('gen-comp-aux-2nd-phrase := head-final &  \
                     [ SYNSEM.LOCAL.CAT [ MC +, \
		                          SECOND #scd ], \
                       HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
                                                   HEAD verb,\
		             	                   SECOND #scd ], \
                       NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
			                               HEAD verb, \
				                       VAL.SUBJ < [ ] >]].')

###if partial vp fronting is not allowed (it is not in Danish),
###verbs must be accompanied by all their complements if placed in the
###Vorfeld
  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
    climb_gwo.add('gen-comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
  mylang.add('comp-aux-2nd-phrase := gen-comp-aux-2nd-phrase & \
                                 basic-head-1st-comp-phrase & \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].') 
  climb_gwo.add('comp-aux-2nd-phrase := gen-comp-aux-2nd-phrase & \
                                 basic-head-1st-comp-phrase & \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].') 
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('comp-aux-2nd-phrase-2 := gen-comp-aux-2nd-phrase & \
                                 basic-head-2nd-comp-phrase & \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND +, \
                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX - ] ].')
    climb_gwo.add('comp-aux-2nd-phrase-2 := gen-comp-aux-2nd-phrase & \
                                 basic-head-2nd-comp-phrase & \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND +, \
                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX - ] ].')

###languages that have fixed argument-order (Dutch) cannot have the verb
###be placed in the vorfeld with an argument that is non-adjacent in
###canonical position: i.e.
###ik heb de man het boek gegeven.
#I have-1st-sg the man the book give-ptc.
#can be realized as (though pretty marked):
#a)het boek gegeven heb ik de man. 
#but absolutely not as:
#b) de man gegeven heb ik het boek.

  if ch.get('argument-order') == 'fixed':
    mylang.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].') 
    climb_gwo.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].') 
    if not ch.get('v2-analysis') == 'filler-gap':
      mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
      climb_gwo.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
  if ch.get('q-inv'):
    mylang.add('gen-comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    climb_gwo.add('gen-comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')

  if ch.get('wh-questions') == 'yes':
    wh_mc_argcomp_word_order(ch, mylang, rules, climb_gwo)

def mc_argcomp_revised_wo(ch, mylang, climb_gwo):
  mylang.add('comp-head-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-head-phrase-2 := basic-head-comp-share-vc.')
  mylang.add('gen-comp-aux-2nd-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT + ].')
  climb_gwo.add('comp-head-phrase := basic-head-comp-share-vc.')
  climb_gwo.add('comp-head-phrase-2 := basic-head-comp-share-vc.')
  climb_gwo.add('gen-comp-aux-2nd-phrase := basic-head-comp-share-vc.')
  climb_gwo.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT + ].')
  vfrontval = 'bool'
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT ' + vfrontval + ' ].')
    climb_gwo.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT ' + vfrontval + ' ].')

### [ MC + ] is mc specific (filler-gap [ MC na ])

  mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC + ].')
  climb_gwo.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC + ].')  
  


def wh_mc_argcomp_word_order(ch, mylang, rules, climb_gwo):
  if ch.get('old-analysis') != 'yes':
    mylang.add('wh-comp-head-phrase := basic-head-comp-share-vc.')
    climb_gwo.add('wh-comp-head-phrase := basic-head-comp-share-vc.')

  climb_gwo.add('comp-head-phrase-2 := head-non-wh-or-rel.')
  climb_gwo.add('wh-comp-head-phrase-2 := basic-head-2nd-comp-phrase & \
               head-final-head-nexus & basic-head-comp-share-vc & head-wh.')
  mylang.add('comp-head-phrase-2 := head-non-wh-or-rel.')
  mylang.add('wh-comp-head-phrase-2 := basic-head-2nd-comp-phrase & \
               head-final-head-nexus & basic-head-comp-share-vc & head-wh.')
  rules.add('wh-comp-head-2 := wh-comp-head-phrase-2.')
  climb_gwo.add('wh-comp-head-2 := wh-comp-head-phrase-2.',section='rules')

###########################################################################
#
# functions to find a combination of properties
#

def get_head_restr_for_non_clausal_head_comp(ch):
  head_rest = ''
  
  if ch.get('adp-order'): 
    head_rest = '+nvjrcdmo'
    if ch.get('clz-order'):
      head_rest = '+nvjrdmo'
  elif ch.get('clz-order'):
    head_rest = '+nvjrpdmo'  

  return head_rest

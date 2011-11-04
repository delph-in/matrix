

########################################################
# COMMON COMPONENTS V2+CLUSTERS ACROSS ANALYSES CHOICES
# GENERAL COMPONENTS

def v2_and_verbal_clusters(ch, mylang, lrules, rules):

#create nexus phrases if cluster is present  
  add_nexus_constraints_v2_with_cluster(ch, mylang)
#creating basic phrases and rules
  add_basic_phrases_v2_with_cluster(ch, mylang)
  add_v2_with_cluster_rules(ch, rules)
#specialized rules and interactions for Germanic
  specialized_word_order_v2_with_cluster(ch, mylang, lrules, rules)

##word-order revised for object-raising (mainly argument-composition)
##calling relevant functions to add constraints accordingly 2011-11-04

  if ch.get('old-analysis') == 'yes':
    add_old_analysis_no_obj_raising_constraints(ch, mylang, rules, lrules)
  else:
    add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules)


def add_nexus_constraints_v2_with_cluster(ch, mylang):

###ADDING CONSTRAINTS ON BASIC RULES CREATING SECOND POSITION

###making sure language properties of having pre- or postpositions are respected

  
  head_rest = ''
  if ch.get('adp-order'): 
    head_rest = '+nvjrcdmo'
    if ch.get('clz-order'):
      head_rest = '+nvjrdmo'
  elif ch.get('clz-order'):
    head_rest = '+nvjrpdmo'




  mylang.add('head-initial-head-nexus := nonverbal-comp-phrase & \
                  [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC + ].')
  mylang.add('finite-lex-rule := [ SYNSEM.LOCAL.CAT.MC na-or-- ].')
  mylang.add('head-final-head-nexus := nonverbal-comp-phrase & \
                                       [ SYNSEM.LOCAL.CAT.MC +,\
                                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
###[ INV - ] for yes-no questions
  if ch.get('q-inv'):  
    mylang.add('head-final-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
  if ch.get('aux-comp-order') == 'both' or ch.get('vc-analysis') == 'aux-rule':
    mylang.add('cat :+ [ HEADFINAL bool ].', section='addenda')

  if ch.get('vc-analysis') == 'basic':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
  elif ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL #hf ].')
  
  
####we need to use special rules for adp-comp, for same reason as compl-comp
###introducing separate rules for sub-parts of sentence and main-clausal 
###structure (needed if not using head-filler for v2-ness

  if head_rest:
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')
    mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')
    


def add_basic_phrases_v2_with_cluster(ch, mylang):

  mylang.add('subj-head-vc-phrase := decl-head-subj-phrase & head-final-invc & nonverbal-comp-phrase.') 
 
###2011-10-23 in-vc phrases are are per definition verbal

  mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.HEAD verb ].')

###Analysis independent rules

  if ch.get('vc-placement') == 'pre':
    general_pre_objectival_cluster_phrases(ch, mylang)
  else:
    general_post_objectival_cluster_phrases(ch, mylang)

###2011-11-02 Removing condition that head-comp-phrase-2 is only needed
###if the argument order is free.
###head-comp-phrase-2 also needed for fixed order languages with new 
###word order (capturing SUBJ AUX OBJ TV, where OBJ is not first complement
###of the auxiliary)

  mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus.')
  if ch.get('argument-order') == 'fixed':
    add_fixed_argument_order_constraints(mylang)
    path = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD'
    mylang.add('head-comp-phrase-2 := [ ' + path + ' verb ].')
    mylang.add('head-comp-phrase-2 := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')
 
###Additional trick to help efficiency: conj cannot be complement or subjects
  comment = 'Conjunction markers cannot be complement or subject markers. Adding the appropriate restrictions helps against spurious analyses'
  mylang.add('basic-head-subj-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvjrpcdm ].', comment, section='addenda')
  mylang.add('basic-head-comp-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvjrpcdm ].', section='addenda')


  if ch.get('vc-analysis') == 'basic':
    create_argument_composition_phrases(ch, mylang)
  elif ch.get('vc-analysis') == 'aux-rule':
    create_aux_plus_verb_phrases(ch, mylang)




def general_pre_objectival_cluster_phrases(ch, mylang):
  mylang.add('general-head-comp-vc-phrase := basic-head-1st-comp-phrase & head-initial-invc.')
  mylang.add('head-comp-vc-phrase := general-head-comp-vc-phrase & nonverbal-comp-phrase.')
  mylang.add('head-comp-2-vc-phrase := basic-head-2nd-comp-phrase & head-initial-invc & nonverbal-comp-phrase.')
  if ch.get('aux-comp-order') == 'before':
    mylang.add('aux-comp-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT +, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].') 


def general_post_objectival_cluster_phrases(ch, mylang):

  mylang.add('general-comp-head-vc-phrase:= basic-head-1st-comp-phrase & head-final-invc.')
  mylang.add('comp-head-vc-phrase := general-comp-head-vc-phrase & nonverbal-comp-phrase.')
  mylang.add('comp-2-head-vc-phrase := basic-head-2nd-comp-phrase & head-final-invc & nonverbal-comp-phrase.')
  if ch.get('aux-comp-order') == 'after': 
    mylang.add('comp-aux-vc-phrase := [ HEAD-DTR.SYNSEM.LIGHT +, \
                                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')

  elif ch.get('aux-comp-order') == 'both':
    mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT [ HEADFINAL #hf ], \
                                            HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL +, \
                                            NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
                                                                            HEADFINAL #hf ] ].')
    mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT [ HEADFINAL -, \
				                       VC + ], \
                           NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ MC -, \
				                           HEADFINAL #hf ]].')
    if ch.get('edge-related-res') == 'yes':
      add_edge_constraint(mylang)

def add_v2_with_cluster_rules(ch, rules):

  rules.add('comp-2-head-vc := comp-2-head-vc-phrase.')
  rules.add('subj-head-vc := subj-head-vc-phrase.')
  rules.add('aux-2nd-comp := aux-2nd-comp-phrase.')
  rules.add('comp-aux-2nd := comp-aux-2nd-phrase.')
# Additional rules for if object-raising is covered
  if ch.get('vc-analysis') == 'basic':
    rules.add('comp-aux-2nd-2 := comp-aux-2nd-phrase-2.')
  else:
    rules.add('mverb-2nd-vcomp := mverb-2nd-vcomp-phrase.')
    rules.add('vcomp-mverb-2nd := vcomp-mverb-2nd-phrase.')
  # rule for yes-no question inversion
  # for now only when analysis is aux-rule (apparently not used for 
  # arg-comp, or old error...)
  if ch.get('q-inv') and ch.get('vc-analysis') == 'aux-rule':
    rules.add('aux-1st-comp := aux-1st-comp-phrase.')

###2011-11-02 removing condition of non-fixed arg order (see above)
# if ch.get('argument-order') != 'fixed':
  rules.add('head-comp-2 := head-comp-phrase-2.')

  if ch.get('vc-analysis') == 'aux-rule':
    auxRule = True
  else:
    auxRule = False

  if ch.get('vc-placement') == 'pre':
    add_preverbal_verbcluster_rules(rules, auxRule)
  else:
    add_postobjectival_verbcluster_rules(ch, rules, auxRule)

  if ch.get('split-cluster') == 'yes':
    add_split_cluster_rules(rules, auxRule)


def add_preobjectival_verbcluster_rules(rules, auxRule):

  rules.add('head-comp-vc := head-comp-vc-phrase.')
  if not auxRule:
    rules.add('head-comp-2-vc := head-comp-2-vc-phrase.')
  if ch.get('aux-comp-order') == 'before':
    rules.add('aux-comp-vc := aux-comp-vc-phrase.')


def add_postobjectival_verbcluster_rules(ch, rules, auxRule):

  rules.add('comp-head-vc := comp-head-vc-phrase.')
  if not auxRule:
    rules.add('comp-2-head-vc := comp-2-head-vc-phrase.')
  if ch.get('aux-comp-order') == 'after' or ch.get('aux-comp-order') == 'both':
    rules.add('comp-aux-vc := comp-aux-vc-phrase.')
  if ch.get('aux-comp-order') == 'before' or ch.get('aux-comp-order') == 'both':
    rules.add('aux-comp-vc := aux-comp-vc-phrase.') 


def add_split_cluster_rules(rules, auxRule):
  if auxRule:
    rules.add('noncomp-aux-2nd := noncomp-aux-2nd-phrase.')  
    rules.add('insert-auxiliary := special-insert-aux-phrase.')

####################
# More specialized word order
#

###function that registers whether a specific verb-form is placed
###at the edge of the verbal cluster

def add_edge_constraint(mylang):
  mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & na-or--, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & bool , \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #ed ].')

###function that adds necessary constraints to make complements that are not
###in sentence initial position appear in fixed order

def add_fixed_argument_order_constraints(mylang):
  mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')  
  mylang.add('subj-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-vc-phrase := \
                      [ SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap & bool, \
                                           ARG-ORDER - ], \
                        HEAD-DTR.SYNSEM.LOCAL.CAT [ ALLOWED-PART #ap, \
                                                    ARG-ORDER + ] ].')
###test: sharing removed or staying?
  mylang.add('comp-2-head-vc-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER  + ].')


def specialized_word_order_v2_with_cluster(ch, mylang, lrules, rules):
 
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

  mylang.add('head-final-invc := head-final & \
                    [ SYNSEM.LOCAL.CAT.MC #mc & -, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')

##nonverbal-comp-phrase: states that phrase cannot be used to combine
##head with verbal complement
  mylang.add('nonverbal-comp-phrase := basic-binary-headed-phrase & \
                   [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +njrpcdmo ].')

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
    mylang.add('cat :+ [ EDGE luk ].', 'EDGE is used to prevent participles from occurring in the middle of the cluster', section='addenda')

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
                              DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].',
               section='addenda')
###not allowed partial vps can occur with ditransitive verbs 
    if ch.get('ditransitives') == 'yes':
      mylang.add('ditransitive-verb-lex := [ SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-- ].')


##verb-initial in cluster counter part, only when necessary which may be:
## 1. verbs precede their verbal complement (Dutch, German auxiliary flip
## 2. verbs precede their objects (Danish)
## [MC - ] as for head-final-invc (above)

  if auxorder == 'before' or auxorder == 'both' or ch.get('vc-placement') == 'pre':
    mylang.add('head-initial-invc := head-initial & \
                  [ SYNSEM.LOCAL.CAT.MC #mc & -, \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
##
##calling specific rules depending on chosen analysis

  if ch.get('vc-analysis') == 'aux-rule':
    spec_word_order_phrases_aux_plus_verb(ch, mylang)
  else:
    spec_word_order_phrases_argument_composition(ch, mylang, lrules, rules)

##########################################################################
#                                                                        #
# ALTERNATIVE ANALYSES                                                   #
#                                                                        #
##########################################################################

#####
# A. Argument-composition analysis (standard HPSG)
#  

####DISCLAIMER: NOT ALL LOGICAL COMBINATIONS ARE COVERED BY CODE FOR NOW

def create_argument_composition_phrases(ch, mylang):

  if ch.get('has-dets') == 'yes':
    if ch.get('noun-det-order') == 'det-noun':
      mylang.add('head-spec-phrase := [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

  if ch.get('vc-placement') == 'pre':

    if ch.get('aux-comp-order') == 'before':
      currenttype = 'aux-comp-vc-phrase'
      mylang.add(currenttype + ' :=  general-head-comp-vc-phrase & \
                                 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

  else:
  # adding constraint that head must be [ AUX + ], leading to spurious analyses
  # without
    if ch.get('aux-comp-order') == 'after':
      mylang.add('comp-aux-vc-phrase := general-comp-head-vc-phrase & \
                         [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC +, \
                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].') 

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

      if ch.get('argument-order') == 'fixed':
        mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')

        mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')
   


################
# specialized phrases for argument composition plus clusters
#

def spec_word_order_phrases_argument_composition(ch, mylang, lrules, rules):

###SECOND registers whether element is currently in 2nd position or not
###and whether it is allowed to be in second position (for Germanic only 
###finite verb forms)
  mylang.add('cat :+ [ SECOND luk ].')

###verbal items are [ VC + ]
###basic phrases need to pass on value of VC feature

  mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.VC + ].')
  mylang.add('basic-bare-np-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                        HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
###[POTENTIALLY MORE GENERAL]
###head-comp constraint on LIGHT extend to head-subj-phrase
  mylang.add('basic-head-subj-phrase :+ [ SYNSEM [ LOCAL.CAT [ VC #vc, \
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

  mylang.add('aux-2nd-comp-phrase := basic-head-1st-comp-phrase & head-initial & \
                    [ SYNSEM.LOCAL.CAT [ MC #mc, \
		                         SECOND - ], \
                      HEAD-DTR.SYNSEM.LOCAL.CAT[ MC #mc, \
			                         SECOND +, \
                                                 HEAD verb ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb, \
		                        	      MC - ]].') 

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
###if partial vp fronting is not allowed (it is not in Danish),
###verbs must be accompanied by all their complements if placed in the
###Vorfeld
  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')
  mylang.add('comp-aux-2nd-phrase := gen-comp-aux-2nd-phrase & \
                                 basic-head-1st-comp-phrase & \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
  mylang.add('comp-aux-2nd-phrase-2 := gen-comp-aux-2nd-phrase & \
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
    mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
  if ch.get('q-inv'):
    mylang.add('gen-comp-aux-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')

##additional constraints to account for split clusters, if occuring
##(explanation see below)
##TRYING OUT: MAY NO LONGER BE NEEDED WITH REVISED WORD ORDER ANALYSIS
##2011-11-04
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
#'old-analysis' keep until tested
  headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  nhddtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'

  mylang.add('head-initial-head-nexus := ' + headdtrval + ' ].')
  mylang.add('comp-head-vc-phrase := ' + headdtrval + ' & na-or-- ].')
  mylang.add('comp-2-head-vc-phrase:= [ HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT - ].')
  mylang.add('comp-aux-2nd-phrase := ' + nhddtrval + ' ].')
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
			  HEAD [ AUX - ],\
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


def add_old_analysis_no_obj_raising_constraints(ch, mylang, rules, lrules):
 
  mylang.add('no-cluster-lex-item := lex-item & [ SYNSEM.LOCAL.CAT.VC - ].')  

  if ch.get('argument-order') == 'fixed':
    mylang.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                       HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')

  if ch.get('vc-analysis') == 'basic':
    mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
    mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC na ].')

    headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                     HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
    mylang.add('head-initial-invc := ' + headdtrval + ' ].')

    if ch.get('split-clusters'):
      split_cluster_phrases_argument_composition(ch, mylang, rules, lrules)
    

def add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules):

  mylang.add('no-cluster-lex-item := lex-item & \
                                 [ SYNSEM.LOCAL.CAT.VC na-or-- ].')  

  if ch.get('vc-analysis') == 'basic':

    mylang.add('basic-head-comp-share-vc := basic-head-comp-phrase & \
                                 [ SYNSEM.LOCAL.CAT.VC #vc, \
                                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
    mylang.add('comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
    mylang.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')

    mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC +, \
                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')  
  
    argument_composition_revised_additional_constraints(ch, mylang, lrules)

    if ch.get('argument-order') == 'fixed' and ch.get('aux-comp-order') == 'both':    
      add_additional_arg_order_constraints(mylang)


def argument_composition_revised_additional_constraints(ch, mylang, lrules):
  mylang.add('cat :+ [ VFRONT luk ].')
  if ch.get('q-inv'):
    mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VFRONT na ].')
  mylang.add('head-initial-head-nexus := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')
  mylang.add('head-comp-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-head-phrase := basic-head-comp-share-vc.')
  mylang.add('head-comp-phrase-2 := basic-head-comp-share-vc.')
  mylang.add('comp-head-phrase-2 := basic-head-comp-share-vc.')
  mylang.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')
  mylang.add('aux-2nd-comp-phrase := basic-head-comp-share-vc.')
  mylang.add('gen-comp-aux-2nd-phrase := basic-head-comp-share-vc.')
  mylang.add('basic-head-mod-phrase-simple :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
  mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC na-or-+ ].')

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
			  HEAD [ AUX - ],\
			  VC #vc,\
                          VFRONT +, \
			  SECOND #sd   ] ].')

  lrules.add('change-arg-order := change-arg-order-rule.')

  headdtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  nhddtrval = '[ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                 NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf'
  
  mylang.add('head-initial-head-nexus := ' + headdtrval + ' ].')

  mylang.add('comp-head-vc-phrase := ' + headdtrval + ' ].')
  mylang.add('comp-2-head-vc-phrase := ' + headdtrval + ' ].')
  mylang.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT + ].')
  vfrontval = 'bool'
  mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT ' + vfrontval + ' ].')
  mylang.add('comp-aux-vc-phrase := ' + nhddtrval + ' ].')
  mylang.add('basic-head-subj-phrase :+ '+ headdtrval + ' ].')
  mylang.add('aux-2nd-comp-phrase := ' + nhddtrval + ' ].')
  mylang.add('basic-head-mod-phrase-simple :+ ' +headdtrval + ' ].')

  mylang.add('infl-lex-rule :+ [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                       DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')

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

def add_additional_arg_order_constraints(mylang):

#function adds some additional constraints for Dutch
#temporal solution to be improved 2011-11-03
  mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')

  mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')

  mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')

  mylang.add('subj-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-phrase-2 := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')



###########
# B) Aux+verb rule analysis, proposed by Dan Flickinger,
# first described in Bender (2008)
# 


def create_aux_plus_verb_phrases(ch, mylang):

  if ch.get('vc-placement') == 'pre':
    if ch.get('aux-comp-order') == 'before':
      currenttype = 'aux-comp-vc-phrase'
      mylang.add(currenttype + ' := head-initial & basic-aux-verb-rule & \
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
######next constraint is shared between both alternative analyses
####WILL STAY UP THERE....
#      mylang.add(currenttype + ' := ' + currentsupertype + ' & \
#                           [ HEAD-DTR.SYNSEM.LIGHT +, \
#                             NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].') 



########
# specialized rules
#

def spec_word_order_phrases_aux_plus_verb(ch, mylang):
  mylang.add('basic-verbal-comp-rule := head-compositional & basic-binary-headed-phrase & head-valence-phrase & \
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
   
  bar = \
    'basic-aux-verb-rule := basic-verbal-comp-rule-1 & \
        [ SYNSEM.LOCAL.CAT [ VAL #val, \
	      	             HEAD.AUX + ], \
          NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].'
  mylang.add(bar)

  mv2vcp = \
   'mverb-2nd-vcomp-phrase := aux-comp-non-vc-phrase & \
      [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD [ AUX - ], \
                                    VAL.SUBJ #subj ], \
        SYNSEM.LOCAL.CAT.VAL.SUBJ #subj ].'
  mylang.add(mv2vcp) 


  mytype = 'aux-comp-non-vc-phrase'
###to be adapted with future version (1 addition)
  mylang.add(mytype + ' := basic-verbal-comp-rule-1 & head-initial & \
                             [ SYNSEM [ LOCAL.CAT [ HEADFINAL +, \
			                            MC #mc, \
			                            HEAD.FORM finite ], \
	                                            LIGHT - ], \
                               HEAD-DTR.SYNSEM [ LOCAL.CAT.MC #mc ], \
                               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
  mylang.add('aux-2nd-comp-phrase := ' + mytype + ' & basic-aux-verb-rule.')
  mylang.add('aux-2nd-comp-phrase := [ HEAD-DTR.SYNSEM [ LOCAL.CAT.MC na, \
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
      

    comment = \
      'The sequence aux-1st-comp-inv-cl-head-adj leads to sentences like\n' + \
      '*Wird der mann schlafen nicht?, *Hat der mann geschlafen bestimmt.\n' + \
      'Using MODIFIED to prevent this from happening. ' + \
      'Replace with other (especially introduced) feature if necessary.'
    mylang.add('aux-1st-comp-phrase := [ SYNSEM.MODIFIED notmod-or-rmod ].', comment)

  mylang.add(mytype + ' := basic-verbal-comp-rule-1 & head-initial & \
                             [ SYNSEM [ LOCAL.CAT [ HEADFINAL +, \
			                            MC #mc, \
			                            HEAD.FORM finite ], \
	                                            LIGHT - ], \
                               HEAD-DTR.SYNSEM [ LOCAL.CAT.MC #mc ], \
                               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')  

  mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                         HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')
  if ch.get('vc-placement') == 'pre':
    mylang.add('head-initial-invc := [ SYNSEM.LOCAL.CAT.VC -, \
                                       HEAD-DTR.SYNSEM.LOCAL.CAT.VC na-or-- ].')

  if ch.get('argument-order') == 'fixed':
    mylang.add('gen-vcomp-verb-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')
    mylang.add('basic-verbal-comp-rule := [ SYNSEM.LOCAL.CAT.ALLOWED-PART #ap, \
NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART #ap ].')

  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].') 

  if ch.get('aux-comp-order') == 'both':
    mylang.add('head-final-invc := [ SYNSEM.LOCAL.CAT.NOMINAL + ].')
    mylang.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster', section='addenda')

  mylang.add('gen-vcomp-verb-2nd-phrase := head-final & \
                              [ SYNSEM.LOCAL.CAT [ MC +, \
		                                   HEADFINAL #hf, \
		                                   VAL.SUBJ < [] >  ], \
                                HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
			       	                            HEADFINAL #hf ], \
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
  if ch.get('q-inv'):
    mylang.add('gen-vcomp-verb-2nd-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

  mylang.add('comp-aux-2nd-phrase := basic-aux-verb-rule & gen-vcomp-verb-2nd-phrase.')    
  mylang.add('vcomp-mverb-2nd-phrase := gen-vcomp-verb-2nd-phrase & \
                                      basic-verbal-comp-rule-2 & \
                                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].') 
  
  if ch.get('split-cluster') == 'yes':
    split_cluster_phrases_aux_plus_verb(ch, mylang)




def split_cluster_phrases_aux_plus_verb(ch, mylang):


  mylang.add('cat :+ [ VFRONT bool ].', 'VFRONT checks whether the vorfeld contains a partial verbal cluster', section='addenda')
  mylang.add('head :+ [ DTR-FORM form ].')

###flexible aux-comp-order always uses NOMINAL with aux+verb analysis
###only add for fixed aux-comp-order + split cluster

  if ch.get('aux-comp-order') != 'both': 
    mylang.add('cat :+ [ NOMINAL bool ].', 'NOMINAL prevents nominal forms from occurring in the verbal cluster', section='addenda')

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

  mylang.add('gen-vcomp-verb-2nd-phrase := \
                       [ SYNSEM.LOCAL.CAT.HEAD [ DTR-FORM #dform \
                                                 FORM finite ], \
                         NON-HEAD-DTR.SYNSEM.LOCAL.CAT. HEAD.FORM #dform ].')
  if ch.get('q-inv'):
    mylang.add('gen-vcomp-verb-2nd-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
  mylang.add('comp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT - ].')
  mylang.add('noncomp-aux-2nd-phrase := gen-vcomp-verb-2nd-phrase & special-basic-aux-verb-rule & [ SYNSEM.LOCAL.CAT.VFRONT +, \
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
  mylang.add('decl-head-subj-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('basic-head-comp-phrase :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('basic-head-mod-phrase-simple :+ \
                      [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                        HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')


  if ch.get('part-vp-front') == 'no':
    mylang.add('gen-vcomp-verb-2nd-rule := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

  if ch.get('argument-order') == 'fixed':
    mylang.add('noncomp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ALLOWED-PART na-or-+ ].')


def create_germanic_adjunct_phrases(ch, mylang, rules):
###need to separate adjunctial from adjectival phrases, due to different
###wo interactions
 
  mylang.add('adjunct-head-phrase := basic-head-mod-phrase-simple & \
                  [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +rp ].')
 
###if no auxiliary, direct attachment v2

  mylang.add('head-2nd-adj-phrase := head-initial-head-nexus & adjunct-head-phrase.')
  if ch.get('q-inv') and ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-2nd-adj-phrase := [ HEAD-DTR.SYNSEM.MODIFIED hasmod ].')

  mylang.add('head-2nd-adj-int-phrase := head-adj-int-phrase & head-2nd-adj-phrase.')
  mylang.add('head-2nd-adj-scop-phrase := head-adj-scop-phrase & head-2nd-adj-phrase.')
 
  mylang.add('adj-head-2nd-int-phrase := adj-head-int-phrase & head-final-head-nexus & adjunct-head-phrase.')
 
  mylang.add('adj-head-2nd-scop-phrase := adj-head-scop-phrase & head-final-head-nexus & adjunct-head-phrase.')

  rules.add('adj-head-2nd-int := adj-head-2nd-int-phrase.')
  rules.add('adj-head-2nd-scop := adj-head-2nd-scop-phrase.')
  rules.add('head-2nd-adj-int := head-2nd-adj-int-phrase.')
  rules.add('head-2nd-adj-scop := head-2nd-adj-scop-phrase.')


### TO DO: FIND OUT ABOUT DANISH
  if ch.get('vc-placement') == 'post':
    mylang.add('adj-head-int-vc-phrase := adj-head-int-phrase & head-final-invc & adjunct-head-phrase.')
    mylang.add('adj-head-scop-vc-phrase := adj-head-scop-phrase & head-final-invc & adjunct-head-phrase.')    
    rules.add('adj-head-int-vc := adj-head-int-vc-phrase.')
    rules.add('adj-head-scop-vc := adj-head-scop-vc-phrase.')


from gmcs.linglib import long_distance_dependencies

########################################################
# COMMON COMPONENTS V2+CLUSTERS ACROSS ANALYSES CHOICES
# GENERAL COMPONENTS

def v2_and_verbal_clusters(ch, mylang, lrules, rules):
#create nexus phrases if cluster is present  
  add_nexus_constraints_v2_with_cluster(ch, mylang)

#creating basic phrases and rules

  add_basic_phrases_v2_with_cluster(ch, mylang, rules)
  add_v2_with_cluster_rules(ch, rules)

#specialized rules and interactions for Germanic

  specialized_word_order_v2_with_cluster(ch, mylang, lrules, rules)

##word-order revised for object-raising (mainly argument-composition)
##calling relevant functions to add constraints accordingly 2011-11-04

###CHECK: SHOULD THIS STILL BE CALLED?
  if ch.get('old-analysis') == 'yes':
    add_old_analysis_no_obj_raising_constraints(ch, mylang, rules, lrules)
  else:
    add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules)

  if ch.get('rel-clause') == 'yes':
    create_rel_clause_phrases(mylang, rules)
    if not ch.get('wh-questions') == 'yes':
      mylang.add('bare-np-phrase := head-nexus-phrase.')

  if ch.get('nachfeld') == 'yes':
    create_nachfeld_phrases(ch, mylang, rules)

def add_nexus_constraints_v2_with_cluster(ch, mylang):

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

###[ INV - ] for yes-no questions
  if ch.get('aux-comp-order') == 'both' or ch.get('vc-analysis') == 'aux-rule':
    mylang.add('cat :+ [ HEADFINAL bool ].', section='addenda')

  if ch.get('vc-analysis') == 'basic':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
  
  elif ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-initial-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
  
####we need to use special rules for adp-comp, for same reason as compl-comp
###introducing separate rules for sub-parts of sentence and main-clausal 
###structure (needed if not using head-filler for v2-ness

  if head_rest:
    mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')    


def add_basic_phrases_v2_with_cluster(ch, mylang, rules):

  mylang.add('subj-head-vc-phrase := decl-head-subj-phrase & head-final-invc & nonverbal-comp-phrase.')
  if ch.get('wh-questions') == 'yes':
    mylang.add('subj-head-vc-phrase := [ SYNSEM.NON-LOCAL.QUE 0-dlist ].')
 
###2011-10-23 in-vc phrases are are per definition verbal
  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'

  mylang.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD verb ].')
  if ch.get('q-inv'):  
    mylang.add(hf_invc + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
 
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

  mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  if ch.get('argument-order') == 'fixed':
    add_fixed_argument_order_constraints(mylang)
    path = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD'
    mylang.add('head-comp-phrase-2 := [ ' + path + ' verb ].')
    mylang.add('head-comp-phrase-2 := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')
 
###Additional trick to help efficiency: conj cannot be complement or subjects
  comment = 'Conjunction markers and determiners cannot be complement or subject markers. Adding the appropriate restrictions helps against spurious analyses'
  mylang.add('basic-head-comp-phrase :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvjrpcm ].', section='addenda')
###See comment
  comment = 'Germanic languages (except for Icelandic) only have nominative NPs as subject'
  mylang.add('basic-head-subj-phrase :+ \
              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD noun & [ CASE nom ], \
                                                VAL.SPR < > ] ].', comment, section='addenda')

  if ch.get('vc-analysis') == 'basic':
    create_argument_composition_phrases(ch, mylang, rules)
  elif ch.get('vc-analysis') == 'aux-rule':
    create_aux_plus_verb_phrases(ch, mylang)

  if ch.get('v2-analysis') == 'filler-gap':
    long_distance_dependencies.add_basic_ldd_phrases(ch, mylang, rules)
    filler_gap_word_order(mylang)
    filler_gap_rules(rules)
  else:
    mc_v2_word_order(ch, mylang, rules)

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
    mylang.add('comp-aux-vc-phrase := [ HEAD-DTR.SYNSEM [ LIGHT +, \
                                                          LOCAL.CAT.MC -], \
                                        NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC - ].')
    if ch.get('q-inv'):
      mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
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
    if ch.get('q-inv'):
      mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
      mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    if ch.get('edge-related-res') == 'yes':
      dtr = ch.get('edge-dtr')
      add_edge_constraint(mylang, dtr)

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

def add_edge_constraint(mylang, dtr):
  if dtr == 'head-dtr':
    DTR = 'HEAD-DTR'
  else:
    DTR = 'NON-HEAD-DTR'
  mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & na-or--, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')
  mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.EDGE #ed & bool, \
                                      ' + DTR + '.SYNSEM.LOCAL.CAT.EDGE #ed ].')

###function that adds necessary constraints to make complements that are not
###in sentence initial position appear in fixed order

def add_fixed_argument_order_constraints(mylang):

####TO DO: MOVE NON-HEAD-FILLER-PHRASE CONSTRAINTS TO WORD ORDER ANALYSES.

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
  
  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'
  mylang.add(hf_invc + ' := head-final & \
                    [ SYNSEM.LOCAL.CAT.MC #mc & -, \
                      HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')

##nonverbal-comp-phrase: states that phrase cannot be used to combine
##head with verbal complement
  mylang.add('nonverbal-comp-phrase := basic-binary-headed-phrase & \
                   [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD +njrpcm, \
                                                     VAL [ SPR < >, \
                                                           SUBJ < >, \
                                                           COMPS < > ] ] ].')

  if ch.get('v2-analysis') == 'filler-gap' or ch.get('nvc-restr') == 'on':
    mylang.add('head-subj-phrase := nonverbal-comp-phrase.')
    mylang.add('head-wh-subj-phrase := nonverbal-comp-phrase.') 
    mylang.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
  mylang.add('head-comp-phrase := nonverbal-comp-phrase.')
  mylang.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
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

###can be done by head-sub-comp, but leads to odd ambiguities for nouns
###leaving this for nouns for now
  if ch.get('nonverb-zuinf-comp') == 'yes':
    wh = False
    if ch.get('wh-questions') == 'yes':
      wh = True
    create_nonverb_zuinf_structure(mylang, rules, wh)

##############################
#
# Phrases and rules for wh-containing sentences
#

def create_wh_wo_phrases(ch, mylang):

  mylang.add('head-wh-subj-phrase := basic-head-wh-subj-phrase & head-initial-head-nexus & nonverbal-comp-phrase.')
  mylang.add('wh-subj-head-vc-phrase := basic-head-wh-subj-phrase & head-final-invc & nonverbal-comp-phrase.')
  
  mylang.add('wh-adjunct-head-phrase := basic-head-wh-mod-phrase-simple & \
            [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +rp ].')

  mylang.add('head-2nd-wh-adj-int-phrase := head-wh-adj-int-phrase & \
               head-initial-head-nexus & wh-adjunct-head-phrase.')

  hf_invc = ''
  if ch.get('v2-analysis') == 'filler-gap':
    hf_invc = 'general-head-final-invc'
  else:
    hf_invc = 'head-final-invc'
  mylang.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' 
             + hf_invc + ' & adjunct-head-phrase.')

  mylang.add('wh-adj-head-int-vc-phrase := wh-adj-head-int-phrase & \
                head-final-invc & wh-adjunct-head-phrase.')

  mylang.add('comp-aux-vc-phrase := head-non-wh-or-rel.')
  mylang.add('comp-head-vc-phrase := head-non-wh-or-rel & share-que-non-head-phrase.')
  mylang.add('wh-comp-head-vc-phrase := general-comp-head-vc-phrase & \
               nonverbal-comp-phrase & head-wh.') 
  mylang.add('head-comp-phrase := head-non-wh-or-rel.')
  mylang.add('adjunct-head-phrase := head-non-wh-or-rel.') 
  if ch.get('comp-for-np-adj') == 'yes':
    mylang.add('adjunct-head-phrase := normal-val-mod.')
  mylang.add('head-wh-comp-phrase := head-wh & basic-head-1st-comp-phrase & \
                          head-initial-head-nexus & nonverbal-comp-phrase.')

  if ch.get('v2-analysis') == 'mc' and ch.get('vc-analysis') == 'basic':
    mylang.add('comp-2-head-vc-phrase := head-non-wh-or-rel.')
####if 100% correct, should check for presence of determiners
###but Germanic languages have them
  mylang.add('wh-spec-head-phrase := basic-head-wh-spec-phrase & head-final & \
       [ SYNSEM.LOCAL.CAT.VC #vc, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc, \
         NON-HEAD-DTR.SYNSEM.OPT - ].')

  mylang.add('bare-np-phrase :=[ HEAD-DTR.SYNSEM.NON-LOCAL.QUE 0-dlist ].')
  

def create_wh_rules(ch, rules):
  rules.add('head-wh-subj := head-wh-subj-phrase.')
  rules.add('wh-subj-head-vc := wh-subj-head-vc-phrase.')
  rules.add('wh-comp-head-vc := wh-comp-head-vc-phrase.')
  rules.add('head-wh-comp := head-wh-comp-phrase.')
 # if not ch.get('v2-analysis') == 'filler-gap':
  rules.add('wh-ques := create-wh-ques-vcomp-phrase.')
  rules.add('head-2nd-wh-adj-int := head-2nd-wh-adj-int-phrase.')
  rules.add('wh-adj-head-int-vc := wh-adj-head-int-vc-phrase.')
  rules.add('wh-spec-head := wh-spec-head-phrase.')


def create_nonverb_zuinf_structure(mylang, rules, wh):
  nom_head_zu_comp = 'nonverb-head-zu-comp-phrase := \
                         basic-head-1st-comp-phrase & head-initial & \
                                                  share-que-non-head-phrase & \
                        [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD noun, \
                          NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb & [ FORM zuinf ], \
				                          VAL [ COMPS < > ], \
			                                  MC - ] ].'
  mylang.add(nom_head_zu_comp)
  rules.add('nonverb-head-zu-comp := nonverb-head-zu-comp-phrase.')
  mylang.add('head-spec-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                                    COMPS < > ] ].')
 
  if wh:
    mylang.add('wh-spec-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                                    COMPS < > ] ].')


def create_nachfeld_phrases(ch, mylang, rules):
  mylang.add('nf-form := form.')
  mylang.add('verb-lex := [ SYNSEM.LIGHT + ].')
  forms = ch.get('nf-forms')  
  my_forms = forms.split(',')
  for f in my_forms:
    mylang.add(f + ' := nf-form.', section='features')
  nf_type = '''nachfeld-phrase := head-initial &
 [ SYNSEM.LOCAL.CAT [ VC na-or--,
		      VFRONT #vf,
		      MC #mc ],
   HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
			       VC bool,
			       VFRONT #vf,
			       MC #mc ],
   NON-HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD +vpc & [ FORM nf-form ] ],
			 NON-LOCAL [ SLASH 0-dlist,
                                     REL 0-dlist,
				     QUE 0-dlist ],
			 LIGHT - ] ].''' 
  mylang.add(nf_type)
  if ch.get('extraposition'):
    mylang.add('nachfeld-phrase := collect-anchor-phrase.')

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
  rules.add('extracted-comp-nachfeld := extracted-comp-phrase-nachfeld.')
  mylang.add('nachfeld-head-filler-phrase := nachfeld-phrase.') 
  mylang.add('nachfeld-head-filler-phrase := basic-head-filler-phrase & \
    [ SYNSEM.LOCAL.CAT.VAL #val, \
      HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].')
  rules.add('nachfeld-head-filler := nachfeld-head-filler-phrase.')
  if ch.get('vc-analysis') == 'basic':
   # mylang.add('nachfeld-head-filler-phrase := basic-head-1st-comp-phrase.')
    mylang.add('nachfeld-phrase := \
       [ SYNSEM.LOCAL.CAT.SECOND #scd, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd, \
         NON-HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    mylang.add('extracted-comp-phrase-nachfeld := \
               [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                 HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
    if ch.get('v2-analysis') == 'mc':
      mylang.add('nachfeld-head-filler-phrase := \
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
    rules.add('nf-head-verbal-filler := nachfeld-head-verbal-filler-phrase.')
  #  mylang.add('nachfeld-head-comp-phrase := basic-verbal-comp-rule-1 & \
  #             [ SYNSEM.LOCAL.CAT.VAL #val, \
  #               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val ].')


def create_special_mod_valence_phrases(mylang):
  mylang.add('normal-val-mod := head-mod-phrase-simple & \
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

def create_argument_composition_phrases(ch, mylang, rules):

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
   
  if not ch.get('v2-analysis') == 'filler-gap':
    mc_argcomp_word_order_phrases(ch, mylang, rules)
    if not ch.get('old-analysis') == 'on':
      mc_argcomp_revised_wo(mylang)
################################################################
# specialized phrases for argument composition plus clusters
#

def spec_word_order_phrases_argument_composition(ch, mylang, lrules, rules):

###SECOND registers whether element is currently in 2nd position or not
###and whether it is allowed to be in second position (for Germanic only 
###finite verb forms)
  mylang.add('cat :+ [ SECOND luk ].')

  mylang.add('basic-head-2nd-comp-phrase :+ \
 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +nvrpcdm ].','As coverage grows (nachfeld, comparatives), the unspecified COMPS list of auxiliaries leads to problems.', section='addenda')

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

  if ch.get('wh-questions') == 'yes':
    add_wh_additions_for_arg_comp(ch, mylang, rules)


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
    

def add_wh_additions_for_arg_comp(ch, mylang, rules):
  if ch.get('old-analysis') != 'yes':
    mylang.add('head-wh-comp-phrase := basic-head-comp-share-vc.')

  mylang.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
  mylang.add('head-comp-phrase-2 := head-non-wh-or-rel.')
  mylang.add('head-wh-comp-phrase-2 := basic-head-2nd-comp-phrase & \
               head-initial-head-nexus & basic-head-comp-share-vc & head-wh.')
  rules.add('head-wh-comp-2 := head-wh-comp-phrase-2.')

def add_revised_analysis_incl_obj_raising_constraints(ch, mylang, lrules):

  mylang.add('no-cluster-lex-item := lex-item & \
                                 [ SYNSEM.LOCAL.CAT.VC na-or-- ].')  

  if ch.get('vc-analysis') == 'basic':

    mylang.add('basic-head-comp-share-vc := basic-head-comp-phrase & \
                                 [ SYNSEM.LOCAL.CAT.VC #vc, \
                                   NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

    mylang.add('comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')
    mylang.add('comp-2-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT na-or-+ ].')

###moved constraint [ MC + ] to mc specific (filler-gap [ MC na ])
    mylang.add('aux-2nd-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')  
  
    argument_composition_revised_additional_constraints(ch, mylang, lrules)

    if ch.get('argument-order') == 'fixed' and ch.get('aux-comp-order') == 'both':    
      add_additional_arg_order_constraints(ch, mylang)


def argument_composition_revised_additional_constraints(ch, mylang, lrules):
  mylang.add('cat :+ [ VFRONT luk ].')
  if ch.get('q-inv'):
    mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VFRONT na ].')
  mylang.add('head-initial-head-nexus := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC bool ].')
  mylang.add('head-comp-phrase := basic-head-comp-share-vc.')
  mylang.add('head-comp-phrase-2 := basic-head-comp-share-vc.')
  mylang.add('general-comp-head-vc-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-2-head-vc-phrase := basic-head-comp-share-vc.')

  aux2nd = ''
  if ch.get('v2-analysis') == 'filler-gap':
    aux2nd = 'general-aux-2nd-comp'
  else:
    aux2nd = 'aux-2nd-comp-phrase'
  mylang.add(aux2nd + ' := basic-head-comp-share-vc.')
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
			  HEAD verb & [ AUX - ],\
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
  mylang.add('comp-aux-vc-phrase := ' + nhddtrval + ' ].')
  mylang.add('basic-head-subj-phrase :+ '+ headdtrval + ' ].')
  mylang.add(aux2nd + ' := ' + nhddtrval + ' ].')
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

def add_additional_arg_order_constraints(ch, mylang):

#function adds some additional constraints for Dutch
#temporal solution to be improved 2011-11-03
  mylang.add('comp-aux-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  mylang.add('aux-comp-vc-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
##################TODOTODOTODO 
  mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('change-arg-order-rule := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')

  mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
               HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
  aux2nd = ''
  if ch.get('v2-analysis') == 'filler-gap':
    aux2nd = 'general-aux-2nd-comp'
  else:
    aux2nd = 'aux-2nd-comp-phrase'
  mylang.add(aux2nd + ' := [ SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.ARG-ORDER #ao ].')
#######################TODOTODOTO
  mylang.add('subj-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.ARG-ORDER - ].')
  mylang.add('comp-head-phrase-2 := [ SYNSEM.LOCAL.CAT.ARG-ORDER + ].')


#######################################################################
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



########################################################################
# specialized rules
#

def spec_word_order_phrases_aux_plus_verb(ch, mylang):

  if ch.get('extraposition') == 'yes':
    stype = 'collect-anchor-phrase'
  else:
    stype = 'basic-binary-headed-phrase'

  mylang.add('basic-verbal-comp-rule := head-compositional & ' + stype + ' & head-valence-phrase & \
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
      [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.AUX -, \
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
                               HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc, \
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
  
  if ch.get('wh-questions') == 'yes':
    mylang.add('wh-comp-head-vc-phrase := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                   HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')

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
  head_res = '+'  
  if ch.get('mod-noun') == 'yes':
    head_res += 'n'
  if ch.get('rel-clause') == 'yes':
      head_res += 'v'
  head_res += 'r'
  head_res += 'p'

  mylang.add('adjunct-head-phrase := basic-head-mod-phrase-simple & \
                  [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + head_res + ' ].')
 
###if no auxiliary, direct attachment v2
###to do: introduce EDGE if not already present
  if not ch.get('edge-related-res') == 'yes':
    mylang.add('cat :+ [ EDGE luk ].')
  mylang.add('head-2nd-adj-phrase := head-initial-head-nexus & adjunct-head-phrase & \
 [ SYNSEM.LOCAL.CAT.EDGE #edge, \
   HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
####only verbs can be modified by post-modifying adverbs
  if ch.get('q-inv') and ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-2nd-adj-phrase := [ HEAD-DTR.SYNSEM.MODIFIED hasmod ].')

  mylang.add('head-2nd-adj-int-phrase := head-adj-int-phrase & head-2nd-adj-phrase.')
  mylang.add('head-2nd-adj-scop-phrase := head-adj-scop-phrase & head-2nd-adj-phrase.')

  rules.add('head-2nd-adj-int := head-2nd-adj-int-phrase.')
  rules.add('head-2nd-adj-scop := head-2nd-adj-scop-phrase.')

 
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('adj-head-2nd-int-phrase := adj-head-int-phrase & head-final-head-nexus & adjunct-head-phrase.')
 
    mylang.add('adj-head-2nd-scop-phrase := adj-head-scop-phrase & head-final-head-nexus & adjunct-head-phrase.')

    rules.add('adj-head-2nd-int := adj-head-2nd-int-phrase.')
    rules.add('adj-head-2nd-scop := adj-head-2nd-scop-phrase.')



### TO DO: FIND OUT ABOUT DANISH
  if ch.get('vc-placement') == 'post':

    hf_invc = ''
    if ch.get('v2-analysis') == 'filler-gap':
      hf_invc = 'general-head-final-invc'
    else:
      hf_invc = 'head-final-invc'
    mylang.add('adj-head-int-vc-phrase := adj-head-int-phrase & ' + hf_invc +' & adjunct-head-phrase.')
    mylang.add('adj-head-scop-vc-phrase := adj-head-scop-phrase & ' + hf_invc +  ' & adjunct-head-phrase.')    
    rules.add('adj-head-int-vc := adj-head-int-vc-phrase.')
    rules.add('adj-head-scop-vc := adj-head-scop-vc-phrase.')


def create_rel_clause_phrases(mylang, rules):

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
#  mylang.add('rel-arg0-phrase := rel-phrase & \
#   [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CONT.HOOK.INDEX #index ]>, \
#     NON-MARKER-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX #index ].')
#  mylang.add('rel-arg2-phrase := rel-phrase & \
#   [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CONT.HOOK.INDEX #index ]>, \
#     NON-MARKER-DTR.SYNSEM.LOCAL.CONT.RELS.LIST.FIRST.ARG2 #index ].')

  mylang.add('rel-comp-phrase := rel-phrase & basic-marker-comp-phrase.')
#  mylang.add('rel-arg2-comp-phrase := rel-arg2-phrase & \
#                                          basic-marker-comp-phrase & \
#              [ NON-MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD adp ].')
  mylang.add('rel-subj-phrase := rel-phrase & basic-marker-subj-phrase.')
  
  mylang.add('rel-mod-phrase := rel-phrase & marker-mod-phrase-simple & \
       [ SYNSEM.LOCAL.CAT.HEAD #head, \
         NON-MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD adp & \
                                      [ MOD < [ LOCAL.CAT.HEAD #head ] > ] ].')

  rules.add('rel-comp := rel-comp-phrase.')
  rules.add('rel-subj := rel-subj-phrase.')
  rules.add('rel-mod := rel-mod-phrase.')
####TO DO: ADD REL-RESTRICTION TO HEAD-COMP ETC IF NO WH-QUESTIONS IN THE
####GRAMMAR

  mylang.add('basic-head-comp-phrase :+ \
                   [ SYNSEM.NON-LOCAL.REL #rel, \
                     NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')
  mylang.add('head-spec-phrase := [ SYNSEM.NON-LOCAL.REL #rel, \
                  NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')

########################################################
# I) MC analysis (which follows from the original v-2 analysis
# from the Grammar Matrix
#

def mc_v2_word_order(ch, mylang, rules):
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
  if ch.get('wh-questions') == 'yes':
    mylang.add('head-wh-comp-phrase := nonverbal-comp-phrase.')
    mylang.add('head-wh-comp-phrase-2 := nonverbal-comp-phrase.')
  
  if ch.get('q-inv'):  
    mylang.add('head-final-head-nexus := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

####TO BE MOVED
  if ch.get('vc-analysis') == 'aux-rule':
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEADFINAL #hf, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL #hf ].')
  
  if head_rest:
    mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.HEAD ' + head_rest + ' ].')


  if ch.get('wh-questions') == 'yes':
    wh_mc_word_order_phrases(mylang)
    wh_mc_word_order_rules(rules)


def wh_mc_word_order_phrases(mylang):
  
  mylang.add('wh-subj-head-phrase := basic-head-wh-subj-phrase & head-final-head-nexus.')
  mylang.add('wh-adj-head-2nd-int-phrase := wh-adj-head-int-phrase & \
               head-final-head-nexus & wh-adjunct-head-phrase.')
  mylang.add('comp-head-phrase := head-non-wh-or-rel.')
  mylang.add('wh-comp-head-phrase := head-wh & basic-head-1st-comp-phrase & \
                        head-final-head-nexus.')


def wh_mc_word_order_rules(rules):
  rules.add('wh-subj-head := wh-subj-head-phrase.')
  rules.add('wh-comp-head := wh-comp-head-phrase.')
  rules.add('wh-adj-head-2nd-int := wh-adj-head-2nd-int-phrase.')

#########################################################################
#
# II. Filler-gap analysis: standard HPSG for German v-2
#

def filler_gap_word_order(mylang):
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
 
 # should not be necessary anymore with revised slash constraints
 #
 # hd_slash_share = '[ SYNSEM.NON-LOCAL.SLASH #slash, \
 #                     HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash ].'
 # mylang.add('basic-head-mod-phrase-simple :+ ' + hd_slash_share)
  
  mylang.add('create-informal-vcomp-phrase := \
               [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 0-dlist ] >, \
                 SYNSEM.NON-LOCAL.SLASH 0-dlist ].')

  mylang.add('subj-v-inv-lrule := [ SYNSEM [ NON-LOCAL.SLASH #slash, \
                                             LOCAL [ CAT.HEAD.AUX #aux ] ], \
                                    DTR.SYNSEM [ NON-LOCAL.SLASH #slash, \
                                                 LOCAL [ CAT.HEAD.AUX #aux ] ] ].')
  mylang.add('int-cl := [ HEAD-DTR.SYNSEM.NON-LOCAL.SLASH 0-dlist \
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
 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ VAL.COMPS.FIRST.LOCAL.CAT.HEAD +njrpcdmo, \
			       SECOND na-or-+ ] ].', comment)

  mylang.add('extracted-verbal-comp-phrase := extracted-comp-phrase & \
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

  mylang.add('extracted-subj-phrase := basic-extracted-subj-phrase & \
                 [ SYNSEM.LOCAL.CAT [ MC #mc, \
		                      SECOND #sec ], \
                   HEAD-DTR.SYNSEM.LOCAL.CAT [ MC #mc, \
			                       SECOND #sec ] ].')

def filler_gap_rules(rules):
  rules.add('aux-2nd-comp-2 := aux-2nd-comp-phrase-2.')
  rules.add('extracted-subj := extracted-subj-phrase.')
  rules.add('extracted-vcomp := extracted-verbal-comp-phrase.')
  rules.add('extracted-comp := extracted-non-verbal-comp-phrase.')
  rules.add('extracted-adj := ger-extracted-adj-phrase.')
  rules.add('filler-head := filler-head-phrase.')
  rules.add('wh-filler-head := wh-filler-head-phrase.')

#########################################################################
#
# A.I. phrases and rules required to combine MC analysis and arg-comp
#

def mc_argcomp_word_order_phrases(ch, mylang, rules):

  mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.SECOND #scd, \
                                           HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].')
  mylang.add('aux-2nd-comp-phrase := basic-head-1st-comp-phrase.')
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

  if ch.get('wh-questions') == 'yes':
    wh_mc_argcomp_word_order(ch, mylang, rules)

def mc_argcomp_revised_wo(mylang):
  mylang.add('comp-head-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-head-phrase-2 := basic-head-comp-share-vc.')
  mylang.add('gen-comp-aux-2nd-phrase := basic-head-comp-share-vc.')
  mylang.add('comp-aux-2nd-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT + ].')
  vfrontval = 'bool'
  mylang.add('comp-aux-2nd-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VFRONT ' + vfrontval + ' ].')

### [ MC + ] is mc specific (filler-gap [ MC na ])

  mylang.add('aux-2nd-comp-phrase := [ SYNSEM.LOCAL.CAT.MC + ].')  
  


def wh_mc_argcomp_word_order(ch, mylang, rules):
  if ch.get('old-analysis') != 'yes':
    mylang.add('wh-comp-head-phrase := basic-head-comp-share-vc.')

  mylang.add('comp-head-phrase-2 := head-non-wh-or-rel.')
  mylang.add('wh-comp-head-phrase-2 := basic-head-2nd-comp-phrase & \
               head-final-head-nexus & basic-head-comp-share-vc & head-wh.')
  rules.add('wh-comp-head-2 := wh-comp-head-phrase-2.')

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

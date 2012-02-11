from gmcs.linglib.parameters import determine_vcluster
from gmcs.linglib import  word_order_v2_vcluster
######################################################################
# customize_word_order()
#   Create the type definitions associated with the user's choices
#   about basic word order, including information about adpositions
#   and auxiliaries.

def customize_word_order(mylang, ch, rules):

  wo = ch.get('word-order')

  mylang.set_section('phrases')

# Add type definitions.

# Handle major constituent order first.  This function returns the hs and hc
# values that other parts of this code will want.

  linear_precedence = customize_major_constituent_order(wo, mylang, ch, rules)
  hs = linear_precedence['hs']
  hc = linear_precedence['hc']


# Head specifier rules

  customize_np_word_order(mylang, ch, rules)

# adverb rules (only Germanic)
  if ch.get('has-adv') == 'yes' and ch.get('verb-cluster') == 'yes':
    if ch.get('adv-order') == 'free':
      word_order_v2_vcluster.create_germanic_adjunct_phrases(ch, mylang, rules)

# ERB 2006-09-14 Then add information as necessary to handle adpositions,
# free auxiliaries, etc.

#In general, we might also find word order sensitive to
#clause type (matrix v. subordinate) and dependent type.

  orders = determine_consistent_order(wo,hc,ch)
  specialize_word_order(hc,orders,mylang,ch,rules)




def customize_major_constituent_order(wo, mylang, ch, rules):

# ERB 2006-09-14 The most elegant factoring of just the major
# constituent information uses the same type names for head-comp and
# comp-head phrases.  This doesn't scale when we have to worry about
# adpositions and auxiliaries (and other twists on word order) where
# we might want both comp-head and head-comp in a single language.
# To try to keep the elegant factoring, I'm going to store the
# type names in variables.

  hc = ''
  hs = ''

# This part of the code handles the following basic word orders:
# all six strict orders, V-final and V-initial.  These 8 possible
# orders can be grouped according to head-comp order, head-subj order,
# and whether or not complements must attach before subjects, or subjects before
# complements.  I'm treating SVO and OVS as having complements attaching
# lower.

# (Are there languages which allow all orders in which S attaches
# ouside O as basic word orders?  Likewise all cases where O attaches
# outside S?)

# ERB 2007-01-21 Moving to message-free universe, and now hypothesizing
# that all rules which attach subjects attend to clausal semantics.
# Thus, the head-subj rules that are defined here inherit from
# decl-head-subj-phrase (imp-head-subj-phrase is also available).

# ASF 2008-11-03 v2 analysis requires MC feature is not passed up to mother in
# head - comp and not from mod to mother, putting it back for other wo options
  if not ch.get('comp-for-np-adj') == 'yes':
    mylang.add('basic-head-mod-phrase-simple :+\
                [ SYNSEM.LOCAL.CAT.VAL #val, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].',comment='Adding the old standard valence constraints to basic-head-mod-phrase-simple. Pulled out of the matrix, because modifiers can introduce arguments for the entire phrase sometimes, e.g. This is better wine than yours.')
  else:
    word_order_v2_vcluster.create_special_mod_valence_phrases(mylang)


  if not wo == 'v2':
    mylang.add_literal(';Constraint on MC used to be part of matrix.tdl\n;' +
               ';it applies to all wo implementations, except for v2')
    mylang.add('basic-head-comp-phrase :+\
                [ SYNSEM.LOCAL.CAT.MC #mc,\
                  HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].',
               section='addenda')
    mylang.add('basic-head-mod-phrase-simple :+\
                [ SYNSEM.LOCAL.CAT.MC #mc, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].',
               section='addenda')


# ASF 2011-10-23 adpositions can occur without their (obligatory) complements
# for now only in Germanic branch: making satisfaction of subcategorization
# requirements obligatory for modifiers

  mylang.add('basic-head-mod-phrase-simple :+\
              [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < >,\
                                                    SPR < >,\
                                                    SPEC < > ] ].')



# Head-comp order

  if wo == 'sov' or wo == 'osv' or wo == 'ovs' or wo == 'v-final':
    hc = 'comp-head'
    mylang.add(hc + '-phrase := basic-head-1st-comp-phrase & head-final.')

  if wo == 'svo' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
    hc = 'head-comp'
    mylang.add(hc + '-phrase := basic-head-1st-comp-phrase & head-initial.')

# Head-subj order

  if wo == 'osv' or wo == 'sov' or wo == 'svo' or wo == 'v-final':
    hs = 'subj-head'
    mylang.add(hs + '-phrase := decl-head-subj-phrase & head-final.')

  if wo == 'ovs' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
    hs = 'head-subj'
    mylang.add(hs + '-phrase := decl-head-subj-phrase & head-initial.')

# Complements attach before subjects

  if wo == 'ovs' or wo == 'vos' or wo == 'sov' or wo == 'svo':
    mylang.add(hs + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

# Subjects attach before complements
# ASF 2008-11-20 in order to allow for aux with vp-comp for VSO and OSV
# languages, the standard analysis needs to be adapted to a LIGHT +
# constraint on the hs-rule.

  auxcomp = ch.get('aux-comp')
  if wo == 'vso' or wo == 'osv':
    if ch.get('has-aux') == 'yes' and auxcomp == 'vp':
      mylang.add(hs + '-phrase := [ HEAD-DTR.SYNSEM.LIGHT + ].')
    else:
      mylang.add(hc + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')

# ERB 2006-09-14 Free word order is a big fat special case:

# Module for free word order, i.e., all possible orders of head, subj,
# comps (thinking initially about V, S, and O).  Use this with
# free-order-rules.tdl, which is partially redunant to
# V-initial-rules.tdl and V-final-rules.tdl.  The difference is that
# those currently don't have the head-2nd-comp rules.

# Just using the V-final and V-initial modules together is
# unsatisfactory, since that system gives multiple parses for V-medial
# sentences.  On the other hand, the SVO and OVS modules together will
# generate SOV and VOS words, but not VSO and OSV.  Any other
# combinations would (I believe) lead to respecifications of types
# (head-subj-phrase, etc) rather than multiple parallel types.

# The root of the problem seems to be that we need the subject to be
# able to attach inside the object(s) for VSO and OSV, but at the same
# time, we don't want complete flexibility on order of attachment when
# the verb is in the middle -- that would give spurious ambiguity.

# This solution adopts the xmod hierarchy to enforce right-first
# attachment.  That is, all arguments appears to the right of the verb
# must attach before all arguments appearing to the left.  The
# linguistic prediction of this analysis is that free word order
# languages do not have a consistent VP consituent, even when the verb
# and object are adjacent (OV order).

# Using a separate feature for tracking argument attachment (as
# opposed to modifier attachment).  We might be able to collapse these
# one day, but that's not obvious.

# ERB 2006-09-14 This looks like a case for :+, even in this code,
# since we're adding to something defined in matrix.tdl.


  if wo == 'free':
    mylang.add('synsem :+ [ ATTACH xmod ].',
               'We can\'t just use the V-final and V-initial word\n' +
               'order modules together to get a good free word order\n' +
               'module. The root of the problem seems to be that we\n' +
               'need the subject to be able to attach inside the\n' +
               'object(s) for VSO and OSV, but at the same time, we\n' +
               'don\'t want complete flexibility on order of attachment\n' +
               'when the verb is in the middle -- that would give\n' +
               'spurious ambiguity.  This solution adopts the xmod\n' +
               'hierarchy to enforce right-first attachment.  That is,\n' +
               'all arguments appears to the right of the verb must\n' +
               'attach before all arguments appearing to the left.  The\n' +
               'linguistic prediction of this analysis is that free\n' +
               'word order languages do not have a consistent VP\n' +
               'consituent, even when the verb and object are adjacent\n' +
               '(OV order).  Using a separate feature for tracking\n' +
               'argument attachment (as opposed to modifier\n' +
               'attachment).  We might be able to collapse these one\n' +
               'day, but that\'s not obvious.',
               section='addenda')

# ASF 2008-11-18, if free wo lgge has aux and aux precedes verb,
# the enforced attachment must apply in the other direction.

    if ch.get('has-aux') == 'yes' and  ch.get('aux-comp-order') == 'before':
      mylang.add('head-final-head-nexus := head-final & \
                [ SYNSEM.ATTACH lmod,\
                  HEAD-DTR.SYNSEM.ATTACH notmod-or-lmod ].')
      mylang.add('head-initial-head-nexus := head-initial &\
                [ SYNSEM.ATTACH rmod ].')
    else:
      mylang.add('head-initial-head-nexus := head-initial & \
                [ SYNSEM.ATTACH lmod,\
                  HEAD-DTR.SYNSEM.ATTACH notmod-or-lmod ].')

      mylang.add('head-final-head-nexus := head-final &\
                [ SYNSEM.ATTACH rmod ].')

    mylang.add('head-mod-phrase :+\
                [ SYNSEM.ATTACH #attach,\
                  HEAD-DTR.SYNSEM.ATTACH #attach ].',
               'We\'ll need to add identification of ATTACH between\n\
mother and head-daughter for all other kinds of phrases\n\
if we do this.  Just for illustration, I\'m putting it\n\
in for head-adjunct phrases here:',
               section='addenda')


# ASF (2008-11-03) Another big special case: v2
#
# This implementation does Autronesian-type v2, but without discontinuous nps.
# The only word order constraint is that the (verbal) head of the phrase must
# be in second position.
# It can be preceded by a noun phrase, verb or verbal cluster.
# Interaction with auxiliaries is not implemented for now, because it is not
# clear what may occur (so auxiliaries can occur anywhere for now, as long as
# the v2 constraint is respected)
# Also note that the implementation may need to be revised when more complex
# phenomena (such as clause final verbal cluster and vp-fronting) are
# implemented
#

  if wo == 'v2':
    mylang.add('verbal-head-nexus := headed-phrase & \
                [ SYNSEM.LOCAL.CAT.HEAD verb ].')
# Change introduced by Germanic: [ MC na ] only if no verbal cluster
# removed [ na ] value from rule below
    mylang.add('head-initial-head-nexus := head-initial & \
                [ SYNSEM.LOCAL.CAT.MC #mc, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    if not ch.get('v2-analysis') == 'filler-gap':
      mylang.add('head-final-head-nexus := head-final & \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].')

#####value "na" on head-initial-head-nexus only for no cluster
#####value "bool" on head-final-head-nexus only for no cluster
#####if cluster, specialized rule is called (see word_order_v2_vcluster.py)

    if not ch.get('verb-cluster') == 'yes':
      mylang.add('head-initial-head-nexus := [ SYNSEM.LOCAL.CAT.MC na ].')
      if not ch.get('v2-analysis') == 'filler-gap':
        mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.MC bool ].')
   

#rules shared among free and v2

  if wo == 'free' or wo == 'v2':
    mylang.add('head-subj-phrase := decl-head-subj-phrase & head-initial-head-nexus.')
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus.')
    if not ch.get('v2-analysis') == 'filler-gap':
      mylang.add('subj-head-phrase := decl-head-subj-phrase & head-final-head-nexus.')
      mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus.')
# Change introduced by Germanic: head-comp-phrase-2 not needed 
# for all v2 analyses when verbal cluster is formed
    if not ch.get('verb-cluster') == 'yes':
      mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus.')
    if not ch.get('v2-analysis') == 'filler-gap':
      mylang.add('comp-head-phrase-2 := basic-head-2nd-comp-phrase & head-final-head-nexus.')



# Add rule definitions for major constituent order.

  if wo == 'free' or wo == 'v2':
    rules.add('head-comp := head-comp-phrase.')
    rules.add('head-subj := head-subj-phrase.')
    if not ch.get('v2-analysis') == 'filler-gap':
      rules.add('comp-head := comp-head-phrase.')
      rules.add('subj-head := subj-head-phrase.')
      rules.add('comp-head-2 := comp-head-phrase-2.')
# Change introduced by Germanic: see above
    if wo == 'free'  or (not ch.get('verb-cluster') == 'yes'):
      rules.add('head-comp-2 := head-comp-phrase-2.')
  # Assume at this point that there's a good value of wo.
  # Rule names are stored in hs and hc, since they're the same as type names
  # without the -phrase suffix.
  else:
    rules.add(hc + ' :=  ' + hc + '-phrase.')
    rules.add(hs + ' :=  ' + hs + '-phrase.')

  return {'hs': hs, 'hc': hc}




# ERB 2006-09-15 Subroutine for emitting additional information about
# head-complement and head-subject rules as required for adpositions and
# auxiliaries.

# aux		 adp		head-comp	comp-head	#rules    add to rules.tdl
# ---		 ---		---------	---------	------    ----------------
# ov-auxv 	 ov-prep	v:AUX + | adp	~adp:AUX -	2         head-comp: both
# vo-vaux	 vo-post	~adp:AUX - 	v:AUX + | adp	2         comp-head: both
# free-auxv	 free-prep	unrestricted	~adp:AUX - 	2         --
# free-vaux      free-post	~adp:AUX -	unrestricted	2         --
# free-auxv	 free-post	~adp		AUX -		2         --
# free-vaux      free-prep	AUX -		~adp		2         --
# easy		 ov-prep	adp		~adp		2         head-comp: adp
# easy		 vo-post	~adp		adp		2         comp-head: adp
# easy		 free-prep	unrestricted	~adp		2         --
# easy		 free-post	~adp		unrestricted	2         --
# ov-auxv	 easy		v:AUX +		AUX -		2         head-comp: aux
# vo-vaux	 easy		AUX -		v:AUX +		2         comp-head: aux
#
# Not bothering with the case where adpositions aren't fixed in their order,
# since I don't believe it exists.

# ERB 2006-09-15 Trying to make each tdl snippet appear only once in this code.
# The individual constraints from the table above are:

# v:AUX + | adp    HEAD +vp & [ AUX + ]
# ~adp             HEAD +nvjrcdmo
# adp              HEAD adp
# AUX -            [ AUX - ]
# v:AUX +          HEAD verb & [ AUX + ]

# These can apply to either head-comp or comp-head, depending.

# ERB 2006-10-05 But: We need to worry about question particles now,
# too.  As I add to this and the space gets more complex, I wish that
# there was some what I could take advantage of the head-types hiearchy
# better.

# In particular, the ~adp constraint right now adds +nvjrcdmo.
# But, the same phrase type might need to be compatible with comp,
# so I have to weaken that do +nvjrdmo, and then further constrain it
# to +nvjrcdmo.  Likewise, ~comp boils down to +nvjrdmo, possibly
# further constrained to +nvjrpdmo.  But I can't just add both
# of those constraints, since the tdl won't compile (you can't add
# a type to a conjunction if its supertype or subtype is mentioned).

# My first pass at a solution is to first collect two kinds of information
# about each of head-comp and comp-head:

# head-comp-is is a list which stores positive statements about
# what can be the head of a head-comp phrase.  If the word order is
# free, we shouldn't see any statements here, because anything can be
# a head. (Similarly for comp-head-is.)

# head-comp-is-not is a list which stores negative statements about
# possible heads of head-comp phrase, i.e., what can't be the head.
# We will have interesting values here even in the free word order
# case unless all of the orders values are 'easy'.  (Similarly for
# comp-head-is-not)

# It's possible that I could do this all in one step, but somehow it's
# easier to keep my undestanding of it if I break it out like this.



def specialize_word_order(hc,orders, mylang, ch, rules):

  adp = orders['adp']
  aux = orders['aux']
  qpart_order = orders['qpart_order'] #TODO: verify _-delimited key
  auxcomp = ch.get('aux-comp')
  wo = ch.get('word-order')
  auxorder = ch.get('aux-comp-order')

  if ch.get('has-aux') == 'yes':
    vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)
  else:
    vcluster = False

  # ASF 2008-12-07 If verbal cluster is present, introduce relevant feature
  # and pass-up in lex-rule.
  # Also add relevant constraint to basic-head-comp-phrase

  if vcluster:
    mylang.add('cat :+ [ VC luk ].',
               'Introducing VC keeps track whether main-verb is present in cluster',
               section='addenda')
    mylang.add('lex-rule :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                              DTR.SYNSEM.LOCAL.CAT.VC #vc ].',
               section='addenda')
###Germanic introduced change:
    if not wo == 'v2':
      mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                       NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].',
               section='addenda')
  # ERB 2006-09-15 First add head-comp or comp-head if they aren't
  # already there.  I don't think we have to worry about constraining
  # SUBJ or COMPS on these additional rules because they'll only be for
  # adp or auxv or qpart, so far

  if hc == 'comp-head' and (adp == 'ov-prep' or aux == 'ov-auxv' or qpart_order == 'ov-qs'):
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial.')

  if hc == 'head-comp' and (adp == 'vo-post' or aux == 'vo-vaux' or qpart_order == 'vo-sq'):
    mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final.')

  # ASF 2008-11-18, special auxiliary rule that allows for auxiliaries
  # to combine with v's when aux-comp order is not harmonic
  # the vcluster + constraint is added to head-comp-phrase, since this
  # aux-rule is always combined with the verbal cluster analysis.

  if aux == 'auxv-rule':
    mylang.add('''aux-comp-phrase := basic-marker-comp-phrase & marker-initial-phrase &
                                   [ SYNSEM.LOCAL.CAT [ HEAD.FORM #vform,
                                                        VC #vc ],
                                     MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX +,
                                                                               FORM #vform ],
                                     NON-MARKER-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
                                                             VC #vc ] ].''')
    mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
  if aux == 'vaux-rule':
    mylang.add('''comp-aux-phrase := basic-marker-comp-phrase & marker-final-phrase &
                                   [ SYNSEM.LOCAL.CAT [ HEAD.FORM #vform,
                                                        VC #vc ],
                                     MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX +,
                                                                               FORM #vform ],
                                     NON-MARKER-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
                                                             VC #vc ] ].''')
    mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')

  # add necessary restrictions to assure verb clusters
  # and special auxiliary rules for vso/osv and free word order.

  if vcluster:
    if wo == 'vso' or wo == 'free' or wo == 'v-initial':
      mylang.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
    if wo == 'osv' or wo == 'free' or wo == 'v-final':
      mylang.add('subj-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
    if (aux == 'vini-vc' and aux == 'vo-auxv' ) or wo == 'free':
      mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
    if (aux == 'vfin-vc' and aux == 'ov-vaux') or wo == 'free':
      mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
    if wo == 'free' or wo == 'vso' or wo == 'osv':
      if auxorder == 'before' and aux != 'ov-auxv':
        mylang.add('aux-comp-phrase := basic-head-1st-comp-phrase & head-initial & \
                    [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb ].')
        aux = 'auxc'
      elif auxorder == 'after' and aux != 'vo-vaux':
        mylang.add('comp-aux-phrase := basic-head-1st-comp-phrase & head-final & \
                    [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb ].')
        aux = 'caux'
      if wo == 'free':
        mylang.add('head-comp-phrase-2 := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
        mylang.add('comp-head-phrase-2 := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')

  # Add rules to rules.tdl when necessary

  if aux == 'ov-auxv' or adp == 'ov-prep' or qpart_order == 'ov-qs':
    rules.add('head-comp := head-comp-phrase.')

  if aux == 'vo-vaux' or adp == 'vo-post' or qpart_order == 'vo-sq':
    rules.add('comp-head := comp-head-phrase.')

  if aux == 'auxv-rule' or aux == 'auxc':
    rules.add('aux-comp := aux-comp-phrase.')

  if aux == 'vaux-rule' or aux == 'caux':
    rules.add('comp-aux := comp-aux-phrase.')

  # ERB 2006-09-15 AUX if we're going to mention it, so the tdl compiles.

  if aux != 'easy':
    mylang.add('head :+ [AUX bool].', section='addenda')

  # ERB 2006-10-05 Collect positive statements about head-comp/comp-head
  # We only need to do this is if the word order is not free, and we only
  # need to do it for one of head-comp or comp-head, depending on the order
  # of o and v.


  head_comp_is = []
  comp_head_is = []

  # VO order
  if aux == 'vo-vaux':
     comp_head_is.append('aux')
  if adp == 'vo-post':
    comp_head_is.append('adp')
  if qpart_order == 'vo-sq':
    comp_head_is.append('comp')

  # OV order

  if aux == 'ov-auxv':
    head_comp_is.append('aux')
  if adp == 'ov-prep':
    head_comp_is.append('adp')
  if qpart_order == 'ov-qs':
    head_comp_is.append('comp')

  # ERB 2006-10-05 Collect negative statements about head-comp/comp-head.
  # This needs to be done even if the word order is free.  It might need
  # to be done for both head-comp and comp-head in a free word order language
  # with inconsistent constraints on comp, adp, and aux.


  head_comp_is_not = []
  comp_head_is_not = []

  # when free word order has cluster, restriction on hc does not apply

  if vcluster and (aux == 'free-auxv' or aux == 'free-vaux'):
    aux = 'free-auxcl'

  if aux == 'free-auxv' or aux == 'ov-auxv' or aux == 'auxv-rule':
    comp_head_is_not.append('aux')
  if aux == 'free-vaux' or aux == 'vo-vaux' or aux == 'vaux-rule':
    head_comp_is_not.append('aux')
  if adp == 'free-prep' or adp == 'ov-prep':
    comp_head_is_not.append('adp')
  if adp == 'free-post' or adp == 'vo-post':
    head_comp_is_not.append('adp')
  if qpart_order == 'free-qs' or qpart_order == 'ov-qs':
    comp_head_is_not.append('comp')
  if qpart_order == 'free-sq'or qpart_order == 'vo-sq':
    head_comp_is_not.append('comp')


  # ERB 2006-10-05 Add constraints to head-comp/comp-head.

  # First the positive constraints.  We only have positive constraints if
  # the word order is not free. We should only have positive constraints for
  # one of head-comp and comp-head and negative constraints for the other.
  # In the free word order case, we might have negative constraints for each
  # one.

  # If only one part of speech type is allowed by the positive constraints,
  # we don't have a disjunctive type.

  # ASF for aux-comp and comp-aux, restriction should be AUX +
  # ASF for v-final and v-initial, added SUBJ < [] > constraint to form
  # a verbal cluster at the beginning or end of the sentence.
  # ASF 2009-04-21: should also be restricted to being a verb, if only one
  # non-standard order is present.


  if len(head_comp_is) == 1:
    head = head_comp_is[0]
    if head == 'aux':
      mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ] ].',
               'head-comp-phrase requires auxiliary heads.')
      if wo == 'v-final' and auxcomp == 'vp':
        mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ ] > ].')
    else:
      mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'head-comp-phrase requires things that are [ HEAD ' + head + ' ].')

  if len(comp_head_is) == 1:
    head = comp_head_is[0]
    if head == 'aux':
      mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ] ].',
               'comp-head-phrase requires auxiliary heads.')
      if wo == 'v-initial' and auxcomp == 'vp':
        mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ ] > ].')
    else:
      mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'comp-head-phrase requires things that are [ HEAD ' + head + ' ].')

  # Now the case where we do have disjunctive constraints.   NB: The order
  # here is important, since we're constructing the string which has to
  # correspond to the disjunctive head type.  While those are logically just
  # disjunctions, as far as the LKB is concerned, they're type names, so
  # we need an exact string match.

  # +nvjrcdmo.
  # +nvjrpdmo.


  if len(head_comp_is) > 1:
    head = '+'
    auxresthc = False
    if head_comp_is.count('aux'):
      head += 'v'
      auxresthc = True
    if head_comp_is.count('adp'):
      head += 'p'
    if head_comp_is.count('comp'):
      head += 'c'

    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'head-comp-phrase requires things that are one of: ' + str(head_comp_is))
    if auxresthc:
      mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

  if len(comp_head_is) > 1:
    head = '+'
    auxrestch = False
    if comp_head_is.count('aux'):
      head += 'v'
      auxrestch = True
    if comp_head_is.count('adp'):
      head += 'p'
    if comp_head_is.count('comp'):
      head += 'c'

    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'comp-head-phrase requires things that are one of: ' + str(head_comp_is))
    if auxrestch:
      mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

  # Now the negative constraints.  This is where we extracted the
  # information that head-comp or comp-head can't be certain things.
  # Of course, in tdl we only have positive constraints.  So, we have
  # to translate the information to positive constraints.  Currently,
  # these will always involve disjunctive types, since we have more
  # pos types than things we're collecting negative info about.  The
  # other case is that we have no negative constraints, in which case
  # there's nothing to do.  We can just leave the HEAD value
  # completely underspecified.

  if head_comp_is_not.count('aux'):
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
    head_comp_is_not.remove('aux')

  if len(head_comp_is_not) > 0:
    head = '+n'
    if head_comp_is_not.count('verb') == 0:
      head += 'v'
    head += 'jr'
    if head_comp_is_not.count('adp') == 0:
      head += 'p'
    if head_comp_is_not.count('comp') == 0:
      head += 'c'
    head += 'dmo'

    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'The head of head-comp-phrase can\'t be: ' + str(head_comp_is_not))

  if comp_head_is_not.count('aux'):
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
    comp_head_is_not.remove('aux')

  if len(comp_head_is_not) > 0:
    head = '+n'
    if comp_head_is_not.count('verb') == 0:
      head += 'v'
    head += 'jr'
    if comp_head_is_not.count('adp') == 0:
      head += 'p'
    if comp_head_is_not.count('comp') == 0:
      head += 'c'
    head += 'dmo'

    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'The head of comp-head-phrase can\'t be: ' + str(comp_head_is_not))

# ASF 2011-11-08 Not directly related to specialize_word_order
# adding additional phrases for wh-words (should probably be moved...)


  if ch.get('wh-questions') == 'yes':
    create_wh_phrases(ch, mylang, rules)


# ERB 2006-10-05 Below is what I had before I had to generalize because
# of addition of qpart_order.

# # The ~adp constraint and the ~comp constraint

#   if adp == 'ov-prep' or adp == 'free-prep':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
#                'comp-head-phrase is restricted from taking prepositions as its head.')

#   if adp == 'vo-post' or adp == 'free-post':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
#                'head-comp-phrase is restricted from taking adpositions as its head.')


# # And the opposite cases (adp, V:AUX + | adp)

#   if adp == 'ov-prep' and aux == 'easy':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
#                'head-comp-phrase is only for prepositions.')

#   if adp == 'ov-prep' and aux == 'ov-auxv':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
#                'head-comp-phrase is only for prepositions and auxiliaries.')

#   if adp == 'vo-post' and aux == 'easy':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
#                'comp-head-phrase is only for postpositions.')

#   if adp == 'vo-post' and aux == 'vo-vaux':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
#                'comp-head-phrase is only for postpositions and auxiliaries.')

# # The [AUX -] constraint

#   if aux == 'vo-vaux' or aux == 'free-vaux':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
#                'head-comp-phrase is restricted from taking auxiliaries as its head.')

#   if aux == 'ov-auxv' or aux == 'free-auxv':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
#                'comp-head-phrase is restricted from taking auxiliaries as its head.')

# # The v:AUX + constraint

#   if aux == 'ov-auxv' and adp == 'easy':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
#                'head-comp-phrase is only for auxiliaries.')

#   if aux == 'vo-vaux' and adp == 'easy':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
#                'comp-head-phrase is only for auxiliaries.')



# ERB 2006-09-15 Subroutine for handling NP rules.

def customize_np_word_order(mylang, ch, rules):

  if ch.get('has-dets') == 'yes':
    mylang.add(
      'head-spec-phrase := basic-head-spec-phrase.',
      'Rules for building NPs.  Note that the Matrix uses SPR for\n' +
      'the specifier of nouns and SUBJ for the subject (specifier) of verbs.')

    if ch.get('noun-det-order') == 'noun-det':
      mylang.add('head-spec-phrase := head-initial.')
    if ch.get('noun-det-order') == 'det-noun':
      mylang.add('head-spec-phrase := head-final.')
     
    rules.add('head-spec := head-spec-phrase.')

####
# Adjectives
# For now, assuming det-adj-noun interaction arranged in matrix.tdl
# relevant phrases are defined in matrix.tdl, needn't be included in
# language specific file
###

###
# Germanic reveals interaction with negation (adverb order is free)
# should be adapted for specific cases (i.e. adjective order differs from
# adverbs or from negation marker)
# 2011-10-23 adding constraint to make sure predicative adjectives do not
# show up as modifiers
  ll_adj = False
  if ch.get('2ndll') == 'on':
    for myll in ch.get('ll'):
      if myll.get('phen') == 'adj':
        ll_adj = True    

  if ch.get('has-cop') == 'yes' and not ll_adj:
    mylang.add('basic-head-mod-phrase-simple :+ [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.PRD - ].')
  if ch.get('has-adj') == 'yes':
    # adding adjective specific phrase
    # Germanic, same phrase for adverb modifiers when not modifying verbs
    adj_st = ''
    mylang.add('mod-non-verbal-head-phrase := basic-head-mod-phrase-simple & \
                 [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +jr, \
                   HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD +njrpd ].')
      
    if ll_adj:
      create_adjective_learning_phrases(ch, mylang, rules)
      adj_st += 'gram-'
    adj_st += 'mod-non-verbal-head-phrase'

    if ch.get('adj-noun-order') != 'adj-noun':
      mylang.add('head-nonv-mod-int-phrase := head-adj-int-phrase & ' + adj_st + '.')

      mylang.add('head-nonv-mod-scop-phrase := head-adj-scop-phrase & ' + adj_st + '.')
      rules.add('head-nonv-mod-int := head-nonv-mod-int-phrase.')
      rules.add('head-nonv-mod-scop := head-nonv-mod-scop-phrase.') 
    if ch.get('noun-adj-order') != 'noun-adj':
      mylang.add('mod-nonv-head-int-phrase := adj-head-int-phrase & ' + adj_st + '.')

      mylang.add('mod-nonv-head-scop-phrase := adj-head-scop-phrase & ' + adj_st + '.')
      rules.add('mod-nonv-head-int := mod-nonv-head-int-phrase.')
      rules.add('mod-head-scop := mod-nonv-head-scop-phrase.') 

   
    if not ch.get('comp-for-np-adj') == 'yes':
      mylang.add('mod-non-verbal-head-phrase := \
                 [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
                   SYNSEM.LOCAL.CAT.VAL #val ].')
    else:
      mylang.add('mod-nonv-head-int-phrase := \
             [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD adv, \
                                               VAL.COMPS < > ], \
               HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
               SYNSEM.LOCAL.CAT.VAL #val ].')
      mylang.add('mod-nonv-head-scop-phrase := \
             [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD adv, \
                                               VAL.COMPS < > ], \
               HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
               SYNSEM.LOCAL.CAT.VAL #val ].')
      adj_n_phr = '''adjective-noun-phrase := mod-non-verbal-head-phrase &
           [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD adj,
				             VAL.COMPS #comps ],
             HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD noun,
				         VAL [ SUBJ #subj,
				               SPR #spr,
				               SPEC #spec ] ],
             SYNSEM.LOCAL.CAT.VAL [ COMPS #comps,
			            SUBJ #subj,
			            SPR #spr,
			            SPEC #spec ] ].'''
      mylang.add(adj_n_phr)     
      mylang.add('adj-int-noun-phrase := adj-head-int-phrase & \
                                                     adjective-noun-phrase.') 
      mylang.add('adj-scop-noun-phrase := adj-head-scop-phrase & \
                                                     adjective-noun-phrase.') 
      rules.add('adj-int-noun := adj-int-noun-phrase.')
      rules.add('adj-scop-noun := adj-scop-noun-phrase.')
 

    # ERB 2006-09-14 I think that all languages have some form of
    # the Bare NP phrase.  Eventually there will be some choices about
    # this (given an appropriate module).  For now, use this stand in.

  mylang.add('bare-np-phrase := basic-bare-np-phrase &\
  [ C-CONT.RELS <! [ PRED \"exist_q_rel\" ] !> ].',
             'Bare NP phrase.  Consider modifying the PRED value of the quantifier relation\nintroduced to match the semantic effect of bare NPs in your language.')

  if ch.get('reflexives') == 'yes':
    mylang.add('bare-np-phrase := [ SYNSEM.LOCAL non-refl-local, \
                                    HEAD-DTR.SYNSEM.LOCAL non-refl-local ].')
    if ch.get('has-dets') == 'yes':
      mylang.add('head-spec-phrase := [ SYNSEM.LOCAL non-refl-local ].')

  if ch.get('compound-nouns') == 'yes':
    mylang.add('bare-np-phrase := [ SYNSEM.LIGHT - ].')
    typedef = \
      'compound-noun-phrase := head-initial & \
           [ SYNSEM [ NON-LOCAL #non-loc, \
             LOCAL non-refl-local & \
                    [ CAT #cat & [ HEAD noun & [ MOD <  > ] ] ] ], \
             HEAD-DTR.SYNSEM [ NON-LOCAL #non-loc, \
		               LOCAL compound-local & [ CAT #cat, \
		                               CONT.HOOK.INDEX #modind & \
                                                              [ png #png ] ], \
		               LIGHT + ], \
             NON-HEAD-DTR.SYNSEM [ LOCAL [ CAT [ HEAD noun, \
				                 VAL [ COMPS < >, \
					         SPR < [ OPT + ] > ] ], \
				           CONT.HOOK [ INDEX #hind & \
                                                              [ PNG #png ], \
					               LTOP #ltop ] ], \
			                   LIGHT + ], \
             C-CONT.RELS.LIST < relation & \
                                   [ LBL #ltop, \
                                     PRED "compound_rel", \
                                     ARG1 #hind, \
                                     ARG2 #modind ], ... > ].'
    mylang.add(typedef)
    rules.add('compound-noun := compound-noun-phrase.')

  rules.add('bare-np := bare-np-phrase.')

###################################################
#
#  Germanic specific! genitive modifiers (can be generalized to other
#  cases for other languages)
 
  if ch.get('genitive-modifiers') == 'yes':
    stype = ''
    if ch.get('extraposition') == 'yes':
      stype += 'share-anchor-'
    stype += 'unary-phrase'
    typedef = \
   ' genitive-mod-phrase := ' + stype + ' & \
      [ SYNSEM.LOCAL.CAT [ HEAD noun & [ CASE #case, \
				         MOD < [ LOCAL intersective-mod & \
                                                  [ CAT.HEAD noun, \
						    CONT.HOOK.INDEX #modarg ], \
					         LIGHT + ] > ], \
		              VAL #val ], \
        ARGS < head-spec-phrase & [ SYNSEM [ LOCAL [ CAT [ HEAD noun & [ CASE gen & #case, \
					            MOD < > ], \
				      VAL #val & [ SPR < > ] ], \
                                COORD -, \
			        CONT.HOOK.INDEX #argind ], \
                                        NON-LOCAL.REL 0-dlist ] ] >, \
        C-CONT [ HOOK [ LTOP #ltop, \
                        INDEX #modarg ], \
                 RELS <! [ LBL #ltop, \
                           PRED "_von_v_mod_rel", \
       		           ARG0 event, \
                           ARG1 #modarg, \
                           ARG2 #argind ] !>, \
                 HCONS <! !> ] ].'
    mylang.add(typedef)
    rules.add('genitive-mod := genitive-mod-phrase.')

###########################################################
# rules for vocatives
# (partially Germanic specific (case, preceeding complete sentence))

  if ch.get('vocatives') == 'yes':
    t1 = \
      'vocative-mod-phrase := scopal-mod-phrase & head-final & \
          [ NON-HEAD-DTR vocative-np-phrase, \
            HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH 0-dlist, \
                              LOCAL.CAT.MC #mc ], \
            SYNSEM.LOCAL.CAT.MC #mc  ].'
    
    stype = ''
    if ch.get('comp-for-np-adj') == 'yes':
      mylang.add('vocative-mod-phrase := normal-val-mod.')
    if ch.get('extraposition') == 'yes':
      stype += 'share-anchor-'
    stype += 'unary-phrase'

    t2 = \
      'vocative-np-phrase := ' + stype + ' & \
                [ SYNSEM [ LOCAL [ CAT [ HEAD adv & [ PRD -, \
                                         MOD < [ LOCAL scopal-mod & \
                                                   [ CAT [ HEAD verb & \
                                                             [ FORM finite ], \
                                                           VAL [ SUBJ < >, \
                                                                 COMPS < > ], \
						           MC + ], \
                                                 CONT.HOOK [ LTOP #ltop, \
                                                       INDEX #index ] ] ] > ], \
		                      VAL   [ SPR < >, \
                                              SUBJ < >, \
			                      COMPS < >, \
			                      SPEC < > ] ] ], \
                          NON-LOCAL #nonloc ], \
                  ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD noun & [ MOD < >, \
                                                                CASE nom ], \
                                                  VAL [ SPR < [ OPT +, \
                                                                NON-LOCAL.SLASH 0-dlist ] >, \
                                                        COMPS < >, \
                                                        SPEC <  >, \
                                                        SUBJ < > ] ], \
                                            CONT.HOOK.INDEX #argind, \
                                            COORD - ], \
                                    NON-LOCAL #nonloc, \
                                    LKEYS.KEYREL.PRED named_rel, \
		                    LIGHT + ] ] >, \
                 C-CONT [ HOOK [ LTOP #ltop, \
                                 INDEX #index ], \
                 RELS <! [ LBL #ltop, \
                           PRED "_addressee_rel", \
                           ARG1 #argind, \
                           ARG2 #index ] !>, \
                 HCONS <! !> ] ].'
    mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LIGHT - ].', section='addenda')
    mylang.add(t1)
    mylang.add(t2)
    rules.add('voc-mod := vocative-mod-phrase.')
    rules.add('voc-np := vocative-np-phrase.')

###
# addition of complementizers (not clear how general developed for Germanic)
# 
# customize_head_comp_non_main_phrase general rule that covers both 
# complementizers and prepositions.
# add_subclausal_rules replaces add_complementizer_rules: more general form
# to cover both compl-comp and prep-comp rules.

  if ch.get('has-compl') == 'yes' or ch.get('has-adp') == 'yes':
    customize_head_comp_non_main_phrase(ch, mylang)
    add_subclausal_rules(ch, rules)

# ERB 2006-09-14 Subroutine for figuring out the relationship of major
# constituent order to adpositions and auxiliaries.  Returns two values:
# for adp and aux.  It takes in the values of wo and hc determined in
# the course of creating the basic word order rules.

def create_adjective_learning_phrases(ch, mylang, rules):
######determine agreement features
#  caseagr = False
#  strengthagr = False
#  for agr in ch.get('adjagr'):
#    if agr.get('feat') == case
  mylang.add('case-sharing-adj-head := adjective-head-phrase & \
         [ SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')
  mylang.add('strength-sharing-adj-head := adjective-head-phrase & \
         [ SYNSEM.LOCAL.CAT.HEAD.STRONG #strength, \
           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.STRONG #strength ].')
  mylang.add('gend-sharing-adj-head := adjective-head-phrase & \
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
           NON-HEAD-DTR.SYNSEM.LOCAL.AGR.PNG.GEND #gend ].')
  mylang.add('number-sharing-adj-head := adjective-head-phrase & \
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM #number, \
           NON-HEAD-DTR.SYNSEM.LOCAL.AGR.PNG.NUM #number ].') 

  mylang.add('syn-agr-adj-head-phrase := case-sharing-adj-head & \
                                         strength-sharing-adj-head.')
  mylang.add('sem-agr-adj-head-phrase := gend-sharing-adj-head & \
                                           number-sharing-adj-head.')
  
  mylang.add('all-agr-adjective-head-phrase := syn-agr-adj-head-phrase & \
                                         sem-agr-adj-head-phrase.')
####predicative constraint
  mylang.add('no-prd-adj-head-phrase := adjective-head-phrase & \
                               [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.PRD - ].')
####phrase only allowing grammatical combinations
  mylang.add('gram-adjective-head-phrase := all-agr-adjective-head-phrase & \
                  no-prd-adj-head-phrase.')
#####robustness rules
  mylang.add('wrong-case-adj-head-phrase := strength-sharing-adj-head & \
                             sem-agr-adj-head-phrase & no-prd-adj-head-phrase.')
  mylang.add('wrong-strength-adj-head-phrase :=  case-sharing-adj-head & \
                             sem-agr-adj-head-phrase & no-prd-adj-head-phrase.')
  mylang.add('wrong-gend-adj-head-phrase :=  number-sharing-adj-head & \
                             syn-agr-adj-head-phrase & no-prd-adj-head-phrase.')
  mylang.add('wrong-number-adj-head-phrase :=  gend-sharing-adj-head & \
                             syn-agr-adj-head-phrase & no-prd-adj-head-phrase.')
  
######cross classification with scop and int (only adjective noun for now)
  mylang.add('wr-case-int-adj-head-phrase := wrong-case-adj-head-phrase & \
                                                        adj-head-int-phrase.')
  mylang.add('wr-case-scop-adj-head-phrase := wrong-case-adj-head-phrase & \
                                                        adj-head-scop-phrase.')
  mylang.add('wr-strength-int-adj-head-phrase := \
                        wrong-strength-adj-head-phrase & adj-head-int-phrase.')
  mylang.add('wr-strength-scop-adj-head-phrase := \
                      wrong-strength-adj-head-phrase & adj-head-scop-phrase.')
  mylang.add('wr-gend-int-adj-head-phrase := wrong-gend-adj-head-phrase & \
                                                        adj-head-int-phrase.')
  mylang.add('wr-gend-scop-adj-head-phrase := wrong-gend-adj-head-phrase & \
                                                        adj-head-scop-phrase.')
  mylang.add('wr-number-int-adj-head-phrase := wrong-number-adj-head-phrase & \
                                                        adj-head-int-phrase.')
  mylang.add('wr-number-scop-adj-head-phrase := wrong-number-adj-head-phrase & \
                                                        adj-head-scop-phrase.')

  mylang.add('missing-infl-int-adj-phrase := adj-head-int-phrase & \
                                              all-agr-adjective-head-phrase & \
                               [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.PRD + ].')
  mylang.add('missing-infl-scop-adj-phrase := adj-head-scop-phrase & \
                                              all-agr-adjective-head-phrase & \
                               [ NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.PRD + ].')
##########adding robustness rules.

  rules.add('int-adj-head-case-robust := wr-case-int-adj-head-phrase.')
  rules.add('int-adj-head-strength-robust := wr-strength-int-adj-head-phrase.')
  rules.add('int-adj-head-gend-robust := wr-gend-int-adj-head-phrase.')
  rules.add('int-adj-head-number-robust := wr-number-int-adj-head-phrase.')
  rules.add('scop-adj-head-case-robust := wr-case-scop-adj-head-phrase.')
  rules.add('scop-adj-head-strength-robust := \
                                           wr-strength-scop-adj-head-phrase.')
  rules.add('scop-adj-head-gend-robust := wr-gend-scop-adj-head-phrase.')
  rules.add('scop-adj-head-number-robust := wr-number-scop-adj-head-phrase.')
  rules.add('missing-infl-int-adj-robust := missing-infl-int-adj-phrase .')
  rules.add('missing-infl-scop-adj-robust := missing-infl-scop-adj-phrase .')

def determine_consistent_order(wo,hc,ch):

  adp = 'easy'
  aux = 'easy'
  qpart_order = 'easy'

  # Is the ordering of adpositions consistent with the ordering of O and V?
  # Assuming that adpositions are consistent within a language (i.e., you won't
  # find subject postpositions and object prepositions).

  adporder = ''
  for adp in ch.get('adp',[]):
    adporder = adp.get('order')

  # ERB 2006-10-05 Fixing bug in free word order case.

  if adporder:
    if wo == 'free':
      if adporder == 'before':
        adp = 'free-prep'
      elif adporder == 'after':
        adp = 'free-post'
    elif hc == 'comp-head' and adporder == 'before':
      adp = 'ov-prep'
    elif hc == 'head-comp' and adporder == 'after':
      adp = 'vo-post'

  # Now what about auxiliaries?
  # ASF 2008-12-07 for non-harmonic order and v (not vp) comps,
  # we need a different procedure (see if auxcomp...)

  if ch.get('has-aux') == 'yes':
    auxcomp = ch.get('aux-comp')
    if wo == 'free':
      if ch.get('aux-comp-order') == 'before':
        aux = 'free-auxv'
      elif ch.get('aux-comp-order') == 'after':
        aux = 'free-vaux'
    elif hc == 'comp-head' and ch.get('aux-comp-order') == 'before':
      if auxcomp == 'v':
        aux = 'auxv-rule'
      else:
        aux = 'ov-auxv'
    elif hc == 'head-comp' and ch.get('aux-comp-order') == 'after':
      if auxcomp == 'v':
        aux = 'vaux-rule'
      else:
        aux = 'vo-vaux'

  # ERB 2006-10-05 And what about the order of question particles wrt
  # to other kinds of head-comp?  I'm assuming for now that question particles
  # and other complementizers will behave the same way.  Are there languages
  # in which that is not true?

  if ch.get('q-part-order'):
    if wo == 'free':
      if ch.get('q-part-order') == 'after':
        qpart_order = 'free-sq'
      elif ch.get('q-part-order') == 'before':
        qpart_order = 'free-qs'
    elif hc == 'comp-head' and ch.get('q-part-order') == 'before':
      qpart_order = 'ov-qs'
    elif hc == 'head-comp' and ch.get('q-part-order') == 'after':
      qpart_order = 'vo-sq'

   # return what we learned

  return {'adp': adp, 'aux': aux, 'qpart_order': qpart_order} #TODO: verify key

###########
# ASF
# basic additions for complementizer phrases (assuming specific phrase is
# needed)
# 2011-10-23 generalized to include adpositions

def customize_head_comp_non_main_phrase(ch, mylang):
  c_ord = ''
# has-compl must also be used for arg-comp (previous working was based on
# an error)
  if ch.get('has-compl') == 'yes': 
    if ch.get('clz-comp-order') == 'clz-comp':
      c_ord = 'initial'
    elif ch.get('clz-comp-order') == 'comp-clz':
      c_ord = 'final'

  adp_order = ''  
  if ch.get('adp-order'): 
    if ch.get('adp-order') == 'adp-comp':
      adp_order = 'initial'
    elif ch.get('adp-order') == 'comp-adp':
      adp_order = 'final'  
    else:
      adp_order = 'both' 
  
  adv_order = ''
  if ch.get('adv-argst') == 'yes':
    if ch.get('adv-comp-order') == 'adv-comp':
      adv_order = 'initial'
    elif ch.get('adv-comp-order') == 'comp-adv':
      adv_order = 'final'
 
  fhead = ''
  ihead = ''
  my_iheads = [ ]
  my_fheads = [ ] 
  if not adp_order == 'final':
    my_iheads.append('adp')
  if not adp_order == 'initial':
    my_fheads.append('adp')
  if not c_ord == 'final':
    my_iheads.append('comp')     
  if not c_ord == 'initial':
    my_iheads.append('comp')
  if not adv_order == 'final':
    my_iheads.append('adv')
  if not adv_order == 'initial':
    my_fheads.append('adv')

  ihead = determine_head_value(my_iheads)
  fhead = determine_head_value(my_fheads)

  vc_share = ''
  if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2':
    vc_share = \
      '[ SYNSEM.LOCAL.CAT.VC #vc, \
         HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].'
    
  wh = ''
  if ch.get('wh-questions') == 'yes':
    wh = 'on'
    mylang.add('share-que-non-head-phrase := binary-headed-phrase & \
                  [ SYNSEM.NON-LOCAL.QUE #que, \
                    NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].')
  rel = ''
  if ch.get('rel-clause') == 'yes':
    rel = 'on'
    mylang.add('share-rel-non-head-phrase := binary-headed-phrase & \
                  [ SYNSEM.NON-LOCAL.REL #rel, \
                    NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL #rel ].')
  if ihead:
    mylang.add('head-comp-sub-phrase := basic-head-1st-comp-phrase & \
                    head-initial & \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + ihead +  ' ].')
    if vc_share:
      mylang.add('head-comp-sub-phrase := ' + vc_share) 
###assuming (probably wrong cross-linguistically) that if both orders occur
###at least one category allows both orders, and feature HEADFINAL
###was introduced somewhere
    if fhead:
      mylang.add('head-comp-sub-phrase := \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
    if wh:
      mylang.add('head-comp-sub-phrase := head-valence-head-nexus.')
    if rel:
      mylang.add('head-comp-sub-phrase := share-rel-non-head-phrase.')
  if fhead: 
    mylang.add('comp-head-sub-phrase := basic-head-1st-comp-phrase & \
                    head-final & \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD ' + fhead + ' ].')
    if vc_share:
      mylang.add('comp-head-sub-phrase := ' + vc_share) 
###see comment above
    if ihead:
      mylang.add('comp-head-sub-phrase := \
                      [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL + ].') 
    if wh:
      mylang.add('comp-head-sub-phrase := head-valence-head-nexus.') 
    if rel:
      mylang.add('comp-head-sub-phrase := share-rel-non-head-phrase.') 

  if ch.get('clz-optionality'):
    
    stype = ''
    if ch.get('extraposition') == 'yes':
      stype += 'share-anchor-'
    stype += 'unary-phrase'

    mylang.add('basic-informal-vcomp := ' + stype + ' &\
   [ ARGS < [ SYNSEM [ LOCAL [ COORD -, \
	         	       CAT [ HEAD verb & [ FORM finite ],\
				   VAL #val & [ SUBJ < >,\
					        COMPS < >,\
					        SPR < >,\
					        SPEC < > ] ] ], \
                        NON-LOCAL.SLASH #slash ] ] >,\
     SYNSEM [ LOCAL.CAT [ HEAD comp,\
	                  VAL #val,\
		          MC - ], \
              NON-LOCAL [ QUE 0-dlist, \
                          SLASH #slash ] ] ].')
    mylang.add('create-informal-vcomp-phrase := basic-informal-vcomp & \
                [ ARGS < [ SYNSEM.LOCAL [ CAT.MC +, \
                                          CONT.HOOK #hook & \
                                               [ INDEX.SF prop ] ] ] >, \
                  C-CONT.HOOK #hook ].') 

def determine_head_value(my_heads):
 
  head = ''
  if len(my_heads) == 1:
    head = my_heads[0]
  else:
    head = '+'
    if 'adv' in my_heads:
      head += 'r'
    if 'adp' in my_heads:
      head += 'p'
    if 'comp' in my_heads:
      head += 'c'
  return head

def add_subclausal_rules(ch, rules): 
  c_ord = ch.get('clz-comp-order')
  adp_ord = ch.get('adp-order')
###adv-comp order must be added for completeness
  if c_ord != 'clz-comp' or adp_ord != 'adp-comp':
    rules.add('comp-head-sub := comp-head-sub-phrase.')
  if c_ord != 'comp-clz' or adp_ord != 'comp-adp':
    rules.add('head-sub-comp := head-comp-sub-phrase.')
  if ch.get('clz-optionality'):
    rules.add('informal-vcomp := create-informal-vcomp-phrase.')

def create_wh_phrases(ch, mylang, rules):
  mylang.add('lex-rule :+ [ SYNSEM.NON-LOCAL.QUE #que, \
                            DTR.SYNSEM.NON-LOCAL.QUE #que ].',section='addenda')
  
  mylang.add('bare-np-phrase := head-nexus-phrase.')
  mylang.add('basic-head-wh-mod-phrase-simple :+ \
              [ SYNSEM.LOCAL.CAT.VAL #val, \
                HEAD-DTR.SYNSEM.LOCAL.CAT.VAL #val, \
                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', section='addenda') 

  if ch.get('has-compl') == 'yes':
    
    stype = ''
    if ch.get('extraposition') == 'yes':
      stype += 'share-anchor-'
    stype += 'unary-phrase'
    wh_sub_type = \
      'create-wh-ques-vcomp-phrase := ' + stype + ' & \
        [ ARGS < [ SYNSEM [ LOCAL [ CONT.HOOK #hook & [ INDEX.SF ques ], \
                              COORD -, \
                              CAT [ HEAD verb & \
                                       [ FORM finite, \
                                         MOD < >, \
                                         INV - ], \
                                    VAL #val & \
                                      [ SUBJ < >, \
                                        COMPS < >, \
                                        SPR < >, \
                                        SPEC < > ], \
                                    MC #mc ] ], \
                              NON-LOCAL [ QUE 1-dlist, \
                                          SLASH #slash, \
                                          REL 0-dlist & [ LIST < > ] ] ] ] >, \
      C-CONT.HOOK #hook, \
      SYNSEM [ LOCAL.CAT [ HEAD comp, \
                           VAL #val, \
                           MC #mc & - ], \
               NON-LOCAL [ QUE 0-dlist, \
                           SLASH #slash ] ] ].'
    mylang.add(wh_sub_type)
  
###word order rules in Germanic specific for now
  word_order_v2_vcluster.create_wh_wo_phrases(ch, mylang)
  word_order_v2_vcluster.create_wh_rules(ch, rules)

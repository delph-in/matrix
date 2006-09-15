#!/usr/local/bin/python

######################################################################
# imports

import os
import tdl

######################################################################
# globals

choices = {}

mylang = None
rules = None
irules = None
lrules = None
lexicon = None
roots = None


######################################################################
# customize_word_order()
#   Create the type definitions associated with the user's choices
#   about basic word order, including information from lexicon about
#   adpositions and auxiliaries. 

# ERB 2006-09-15 DOCUMENTATION

# 1. Statement of domain

# This module handles the basic word order facts for matrix declarative
# clauses, include permissible orders of major constituents (V, S, O),
# and (where applicable) determiner-noun order, adposition-NP order, and
# auxiliary-verb order.  It gathers information from multiple sections of
# the questionnaire, namely: The word order section and various parts of
# the lexicon section (entries for adpositions and  auxiliaries)

# 2. Seed strings/requirements of POS lexicon

# The phenomena covered by this module can be illustrated using the following
# lexical entries:

# det (determiner)
# s (subject)
# o (object)
# io (indirect object)
# tv (transitive verb)
# iv (intransitive verb)
# dtv (ditransitive verb)
# aux (auxiliary verb)
# p (adposition)

# In the absence of a module for case (and in languages without case)
# both 's tv o' and 'o tv s' will parse in an ostensibly svo language.  We
# want to distinguish s and o anyway because we would expect those to
# have different semantic representations in an svo language.  That is,
# you shouldn't be able to generate 'o tv s' from the semantics of 's tv o'
# in a strictly svo language.

# Seed strings, not all of which will have valid permutations in all languages.

# Note that his module does NOT handle argument optionality, so the seed
# strings given here don't illustrate it.

# These seed strings are written assuming sov order, prepositions, det-n,
# and auxiliaries which immediately follow the verbs they attach to.
# They allow for optional dets and optional adps.

# The module as written really only does anything interesting with io
# (indirect objects) in the case of free word order languages.  The lexicon
# does not currently elicit a ditransitive verb, however.

## Intransitive verb, transitive verb, det on s, o, both, neither
## p on s, o, both, neither, with and without det

# s iv
# det s iv
# s o tv
# det s det o tv
# det s o tv
# s det o tv
# p s iv
# p s o tv
# p s p o tv
# s p o tv
# p det s iv
# p det s o tv
# p det s p o tv
# det s p o tv
# p s det o tv
# p s p det o tv
# s p det o tv
# p det s det o tv
# p det s p det o tv
# det s p o det tv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three.

# s io o dtv
# det s io o dtv
# s det io o dtv
# s io det o dtv
# det s det io o dtv
# det s io det o dtv
# s det io det o dtv
# det s det io det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on s argument only.

# p s io o dtv
# p det s io o dtv
# p s det io o dtv
# p s io det o dtv
# p det s det io o dtv
# p det s io det o dtv
# p s det io det o dtv
# p det s det io det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on io argument only.

# s p io o dtv
# det s p io o dtv
# s det p io o dtv
# s p io det o dtv
# det s p det io o dtv
# det s p io det o dtv
# s p det io det o dtv
# det s p det io det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on o argument only.

# s io p o dtv
# det s io p o dtv
# s det io p o dtv
# s io p det o dtv
# det s det io p o dtv
# det s io p det o dtv
# s det io p det o dtv
# det s det io p det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on s and io arguments only.

# p s p io o dtv
# p det s p io o dtv
# p s p det io o dtv
# p s p io det o dtv
# p det s p det io o dtv
# p det s p io det o dtv
# p s p det io det o dtv
# p det s p det io det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on s and o arguments only.

# p s io p o dtv
# p det s io p o dtv
# p s det io p o dtv
# p s io p det o dtv
# p det s det io p o dtv
# p det s io p det o dtv
# p s det io p det o dtv
# p det s det io p det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on io and o arguments only.

# s p io p o dtv
# det s p io p o dtv
# s p det io p o dtv
# s p io p det o dtv
# det s p det io p o dtv
# det s p io p det o dtv
# s p det io p det o dtv
# det s p det io p det o dtv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three. P on all three arguments.

# p s p io p o dtv
# p det s p io p o dtv
# p s p det io p o dtv
# p s p io p det o dtv
# p det s p det io p o dtv
# p det s p io p det o dtv
# p s p det io p det o dtv
# p det s p det io p det o dtv

# ## All of the above, with aux added at the end.

# s iv aux
# det s iv aux
# s o tv aux
# det s det o tv aux
# det s o tv aux
# s det o tv aux
# p s iv aux
# p s o tv aux
# p s p o tv aux
# s p o tv aux
# p det s iv aux
# p det s o tv aux
# p det s p o tv aux
# det s p o tv aux
# p s det o tv aux
# p s p det o tv aux
# s p det o tv aux
# p det s det o tv aux
# p det s p det o tv aux
# det s p o det tv aux
# s io o dtv aux
# det s io o dtv aux
# s det io o dtv aux
# s io det o dtv aux
# det s det io o dtv aux
# det s io det o dtv aux
# s det io det o dtv aux
# det s det io det o dtv aux
# p s io o dtv aux
# p det s io o dtv aux
# p s det io o dtv aux
# p s io det o dtv aux
# p det s det io o dtv aux
# p det s io det o dtv aux
# p s det io det o dtv aux
# p det s det io det o dtv aux
# s p io o dtv aux
# det s p io o dtv aux
# s det p io o dtv aux
# s p io det o dtv aux
# det s p det io o dtv aux
# det s p io det o dtv aux
# s p det io det o dtv aux
# det s p det io det o dtv aux
# s io p o dtv aux
# det s io p o dtv aux
# s det io p o dtv aux
# s io p det o dtv aux
# det s det io p o dtv aux
# det s io p det o dtv aux
# s det io p det o dtv aux
# det s det io p det o dtv aux
# p s p io o dtv aux
# p det s p io o dtv aux
# p s p det io o dtv aux
# p s p io det o dtv aux
# p det s p det io o dtv aux
# p det s p io det o dtv aux
# p s p det io det o dtv aux
# p det s p det io det o dtv aux
# p s io p o dtv aux
# p det s io p o dtv aux
# p s det io p o dtv aux
# p s io p det o dtv aux
# p det s det io p o dtv aux
# p det s io p det o dtv aux
# p s det io p det o dtv aux
# p det s det io p det o dtv aux
# s p io p o dtv aux
# det s p io p o dtv aux
# s p det io p o dtv aux
# s p io p det o dtv aux
# det s p det io p o dtv aux
# det s p io p det o dtv aux
# s p det io p det o dtv aux
# det s p det io p det o dtv aux
# p s p io p o dtv aux
# p det s p io p o dtv aux
# p s p det io p o dtv aux
# p s p io p det o dtv aux
# p det s p det io p o dtv aux
# p det s p io p det o dtv aux
# p s p det io p det o dtv aux
# p det s p det io p det o dtv aux

# 3. TDL Samples

# For a v-final language, with prepositions, auxiliaries which precede the verb,
# and det-n order, we should get the following in mylanguage.tdl.  (Actually, this
# is a more nicely formatted version of what we *do* get, I haven't tested it
# as tdl yet.)

# ; comp-head-phrase is restricted from taking prepositions as its head.
# ; comp-head-phrase is restricted from taking auxiliaries as its head.

# comp-head-phrase := basic-head-1st-comp-phrase & head-final &
#    [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo & [ AUX - ]].

# ; head-comp-phrase is only for prepositions and auxiliaries.

# head-comp-phrase := basic-head-1st-comp-phrase & head-initial &
#    [ SYNSEM.LOCAL.CAT.HEAD +vp [ AUX + ]].

# subj-head-phrase := basic-head-subj-phrase & head-final.

# ; Rules for bulding NPs.  Note that the Matrix uses SPR for
# ; the specifier of nouns and SUBJ for the subject (specifier) of verbs.

# head-spec-phrase := basic-head-spec-phrase & head-final.

# ; Bare NP phrase.  Consider modifying the PRED value of the quantifier relation
# ; introduced to match the semantic effect of bare NPs in your language.

# bare-np-phrase := basic-bare-np-phrase &
#    [ C-CONT.RELS <! [ PRED "unspec_q_rel" ] !> ].

# And in rules.tdl, we get:

# comp-head := comp-head-phrase.
# head-comp := head-comp-phrase.
# subj-head := subj-head-phrase.
# head-spec := head-spec-phrase.
# bare-np := bare-np-phrase.

# 4. Description of cases handled

# For the major constituent orders, this module handles: all fixed
# orders of S,O, and V, V-final, V-initial and "free".  It allows for
# strict det-n and strict n-det, as well as strict P-NP and strict
# NP-P.  In addition, it allows auxiliaries to appear strictly left,
# strictly right or either to the left or to the right of the
# constituent they combine with.  The lexicon module allows for
# auxiliaries which combine with V (argument composition), VP
# (raising) or S (??), and contribute their own preds or not.

# It also creates a bare np phrase for every language, because
# a) all nouns need quantifiers and b) I suspect that all languages
# allow bare NPs in at least some cases.  

# The free word order case does some interesting work with ditransitives
# in order to illustrate all 24 possible orders.  This involves allowing
# the second complement to be realized by the first.

# 5. Known unhandled cases

# The module currently does not support word order which varies
# according to clause type (e.g., matrix v. subordinate), V2 order,
# free order of determiners or adpositions with respect to nouns.  It
# also doesn't handle "splattered NP"-type free word order where the
# major constituents are not cohesive.

# There is no specialization of the bare-np phrase (to e.g., pronouns
# only, etc).

# The present lexicon module does not elicit any ditransitive verbs,
# and this word order module does not elicit any information about the
# relative ordering of complements when there is more than one, aside
# from allowing free order of direct and indirect objects in the free
# word order case.  The basic-head-1st-comp-phrase
# v. basic-head-2nd-comp-phrase distinction provided in matrix.tdl is
# a start in this direction, however (and indeed is used by the free
# word order module).

# In addition to adpositions and auxiliaries, there are other types of
# words which will eventually want to use the head-comp (and maybe
# head-subj or head-spec) phrases.  Doing so will undoubtedly require
# some additional modifications to the (already quite complex)
# functions determine_consistent_order() and specialize_word_order().
# These include complementizers, question particles, degree specifiers,
# and complement-taking substantives.

# 6. Description of tdl

# The general strategy is to take the basic head-complement,
# head-subject and head-specifier phrases provided by matrix.tdl, and
# cross-classify them with the types that determine linear precedence
# (head-initial and head-final).  In addition, to allow only the
# correct orders (in VSO, VOS, SOV, OSV) and to eliminate spurious
# ambiguity (in SVO and OVS), head-subject rules are constrained to be
# COMPS < > or head-complement rules are constrained to be SUBJ < >.

# Some trickiness comes in when the orders for adpositions and/or
# auxiliaries aren't consistent with the order of V and O, either
# because they are more constrained (e.g., free order of V,O,S, but
# prepositions only) or constrained to be opposite (an SOV language
# with prepositions).  In this case, we have both head-comp and
# comp-head rules, but one or both constrains its HEAD value (and
# therefore what type of head daughter it can have).  These
# constraints are either on the type of the HEAD value (making use of
# the disjunctive head types provided in head-types.tdl) or on the AUX
# value.  To simplify things, AUX is declared as a feature of head, so
# that a rule that says [AUX -] is e.g., compatible with [HEAD adp].

# 7. Description of customization (python)

# I have attempted to keep each tdl statement (ascription of a particular
# constraint to a particular type) stated only once in this script,
# even if it gets used in multiple cases.  Those cases are handled with
# if statements which test either the input values directly (e.g.,
# the value chosen for word order) or derived properties of those input
# values (whether or not the order of adpositions and verbs with respect
# to their complements is 'consistent').

# customize_word_order() calls the following subroutines:

#  customize_major_constituent_order(), which emits types and
#  instances for head-subject and head-complement rules, without worrying
#  about adp or aux.  In order to make the tdl statements even more compact
#  while preserving a the head-comp v. comp-head naming convention,
#  this subroutine stores part of the appropriate type and rule names
#  in the variables hs (for head-subject) and hc (for head-complement).
#  Since this information is also useful for the other subroutines,
#  customize_major_constituent_order() returns it as a dictionary.

#  customize_np_order() emits the head-spec (or spec-head) rule
#  and a bare-np phrase.

#  determine_consistent_order() takes the word order (wo) and head-
#  complement order (hc) and determines whether the relative order
#  of aux and v and/or p and np requires special treatment given the
#  order of v and o.  It returns a dictionary storing the information
#  for aux and adp.

#  specialize_word_order() takes the head-complement order (hc) and
#  the dictionary returned by determine_consistent_order() and emits
#  additional tdl for types and instances as required.

# Further documentation is provided in comments within the subroutines.

# 8. Required changes to other modules

# None --- this module is likely to change as more get added though!

# ERB 2006-09-14
# First pass at translating from old perl script.

def customize_word_order():

  if choices.has_key('wordorder'):
    wo = choices['wordorder']

# Add type definitions.  

# Handle major constituent order first.  This function returns the hs and hc
# values that other parts of this code will want.

  linear_precedence = customize_major_constituent_order(wo)
  hs = linear_precedence['hs']
  hc = linear_precedence['hc']


# Head specifier rules

  customize_np_word_order()

# ERB 2006-09-14 Then add information as necessary to handle adpositions,
# free auxiliaries, etc. 

#In general, we might also find word order sensitive to
#clause type (matrix v. subordinate) and dependent type.

  orders = determine_consistent_order(wo,hc)
  specialize_word_order(hc,orders)


def customize_major_constituent_order(wo):

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
    mylang.add(hs + '-phrase := basic-head-subj-phrase & head-final.')

  if wo == 'ovs' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
    hs = 'head-subj'
    mylang.add(hs + '-phrase := basic-head-subj-phrase & head-initial.')

# Complements attach before subjects

  if wo == 'ovs' or wo == 'vos' or wo == 'sov' or wo == 'svo':
    mylang.add(hs + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

# Subjects attach before complements

  if wo == 'vso' or wo == 'osv':
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
               'We can\'t just use the V-final and V-initial word\n\
order modules together to get a good free word order\n\
module. The root of the problem seems to be that we\n\
need the subject to be able to attach inside the\n\
object(s) for VSO and OSV, but at the same time, we\n\
don\'t want complete flexibility on order of attachment\n\
when the verb is in the middle -- that would give\n\
spurious ambiguity.  This solution adopts the xmod\n\
hierarchy to enforce right-first attachment.  That is,\n\
all arguments appears to the right of the verb must\n\
attach before all arguments appearing to the left.  The\n\
linguistic prediction of this analysis is that free\n\
word order languages do not have a consistent VP\n\
consituent, even when the verb and object are adjacent\n\
(OV order).  Using a separate feature for tracking\n\
argument attachment (as opposed to modifier\n\
attachment).  We might be able to collapse these one\n\
day, but that\'s not obvious.')

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
in for head-adjunct phrases here:')

    mylang.add('head-subj-phrase := basic-head-subj-phrase & head-initial-head-nexus.')
    mylang.add('subj-head-phrase := basic-head-subj-phrase & head-final-head-nexus.')
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus.')
    mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus.')
    mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus.')
    mylang.add('comp-head-phrase-2 := basic-head-2nd-comp-phrase & head-final-head-nexus.')

# Add rule definitions for major constituent order.

  if wo == 'free':
    rules.add('head-comp := head-comp-phrase.')
    rules.add('head-subj := head-subj-phrase.')
    rules.add('comp-head := comp-head-phrase.')
    rules.add('subj-head := subj-head-phrase.')
    rules.add('head-comp-2 := head-comp-phrase-2.')
    rules.add('comp-head-2 := comp-head-phrase-2.')
  # Assume at this point that there's a good value of wo.
  # Rule names are stored in hs and hc, since they're the same as type names
  # without the -phrase suffix.
  else:
    rules.add(hc + ' :=  ' + hc + '-phrase.')
    rules.add(hs + ' :=  ' + hs + '-phrase.')

  return {'hs': hs, 'hc': hc}

# ERB 2006-09-15 Subroutine for handling NP rules.

def customize_np_word_order():

  if choices.has_key('hasDets'):
    if choices['hasDets'] == 't':
      mylang.add('head-spec-phrase := basic-head-spec-phrase.',
                 'Rules for bulding NPs.  Note that the Matrix uses SPR for\n\
the specifier of nouns and SUBJ for the subject (specifier) of verbs.')

      if choices.has_key('NounDetOrder'):
        if choices['NounDetOrder'] == 'HeadSpec':
          mylang.add('head-spec-phrase := head-initial.')
        if choices['NounDetOrder'] == 'SpecHead':
          mylang.add('head-spec-phrase := head-final.')

    rules.add('head-spec := head-spec-phrase.')

    
# ERB 2006-09-14 I think that all languages have some form of
# the Bare NP phrase.  Eventually there will be some choices about
# this (given an appropriate module).  For now, use this stand in.

  mylang.add('bare-np-phrase := basic-bare-np-phrase &\
              [ C-CONT.RELS <! [ PRED \"unspec_q_rel\" ] !> ].',
             'Bare NP phrase.  Consider modifying the PRED value of the quantifier relation\n\
introduced to match the semantic effect of bare NPs in your language.')

  rules.add('bare-np := bare-np-phrase.')


# ERB 2006-09-14 Subroutine for figuring out the relationship of major
# constituent order to adpositions and auxiliaries.  Returns two values:
# for adp and aux.  It takes in the values of wo and hc determined in
# the course of creating the basic word order rules.


def determine_consistent_order(wo,hc):

  adp = 'easy'
  aux = 'easy'

  # Is the ordering of adpositions consistent with the ordering of O and V?
  # Assuming that adpositions are consistent within a language (i.e., you won't
  # find subject postpositions and object prepositions).

  if choices['subjAdp']:
    adpOrder = choices['subjAdp']
  elif choices['objAdp']:
    adpOrder = choices['objAdp']

  if adpOrder:
    if wo == 'free':
      adp = 'free'
    elif hc == 'comp-head' and adpOrder == 'pre':
      adp = 'ov-prep'
    elif hc == 'head-comp' and adpOrder == 'post':
      adp = 'vo-postp'

  if choices['auxverb']:
    if wo == 'free':
      if choices['auxorder'] == 'left':
        aux = 'free-aux-left'
      elif choices['auxorder'] == 'right':
        aux = 'free-aux-right'
    elif hc == 'comp-head' and choices['auxorder'] == 'left':
      aux = 'ov-auxv'
    elif hc == 'head-comp' and choices['auxorder'] == 'right':
      aux = 'vo-vaux'

   # return what we learned

  return {'adp': adp, 'aux': aux}

# ERB 2006-09-15 Subroutine for emitting additional information about
# head-complement and head-subject rules as required for adpositions and
# auxiliaries.

# aux		 adp		head-comp	comp-head	#rules    add to rules.tdl
# ---		 ---		---------	---------	------    ----------------
# ov-auxv 	 ov-prep	v:AUX + | adp	~adp:AUX -	2         head-comp: both
# vo-vaux	 vo-post	~adp:AUX - 	v:AUX + | adp	2         comp-head: both
# free-aux-left	 free-prep	unrestricted	~adp:AUX - 	2         --
# free-aux-right free-post	~adp:AUX -	unrestricted	2         --
# free-aux-left	 free-post	~adp		AUX -		2         --
# free-aux-right free-prep	AUX -		~adp		2         --
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

def specialize_word_order(hc,orders):

  adp = orders['adp']
  aux = orders['aux']

# ERB 2006-09-15 First add head-comp or comp-head if they aren't
# already there.  I don't think we have to worry about constraining
# SUBJ or COMPS on these additional rules because they'll only be for
# adp or auxv.

  if hc == 'comp-head' and (adp == 'ov-prep' or aux == 'ov-auxv'):
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial.')

  if hc == 'head-comp' and (adp == 'vo-post' or aux == 'vo-vaux'):
    mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final.')

# ERB 2006-09-15 AUX if we're going to mention it, so the tdl compiles.

  if aux != 'easy':
    mylang.add('head :+ [AUX bool].')

# The ~adp constraint

  if adp == 'ov-prep' or adp == 'free-prep':
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
               'comp-head-phrase is restricted from taking prepositions as its head.')

  if adp == 'vo-post' or adp == 'free-post': 
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
               'head-comp-phrase is restricted from taking adpositions as its head.')


# And the opposite cases (adp, V:AUX + | adp)

  if adp == 'ov-prep' and aux == 'easy':
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
               'head-comp-phrase is only for prepositions.')

  if adp == 'ov-prep' and aux == 'ov-auxv':
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
               'head-comp-phrase is only for prepositions and auxiliaries.')

  if adp == 'vo-post' and aux == 'easy':
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
               'comp-head-phrase is only for postpositions.')

  if adp == 'vo-post' and aux == 'vo-vaux':
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
               'comp-head-phrase is only for postpositions and auxiliaries.')

# The [AUX -] constraint

  if aux == 'vo-vaux' or aux == 'free-aux-right':
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
               'head-comp-phrase is restricted from taking auxiliaries as its head.')

  if aux == 'ov-auxv' or aux == 'free-aux-left':
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
               'comp-head-phrase is restricted from taking auxiliaries as its head.')

# The v:AUX + constraint

  if aux == 'ov-auxv' and adp == 'easy':
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
               'head-comp-phrase is only for auxiliaries.')

  if aux == 'vo-vaux' and adp == 'easy':
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
               'comp-head-phrase is only for auxiliaries.')

# Add rules to rules.tdl when necessary

  if aux == 'ov-auxv' or adp == 'ov-prep':
    rules.add('head-comp := head-comp-phrase.')

  if aux == 'vo-vaux' or adp == 'vo-post':
    rules.add('comp-head := comp-head-phrase.')

######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

def customize_sentential_negation():
  pass


######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf):
  pn = pos + num
  if pos == 'n' or pos == 'np':
    headtype = 'noun'
  else:
    headtype = 'verb'

  # First define the rules in mylang.  Every strategy has a
  # top rule and a bottom rule, but only some have a mid rule, so if
  # the mid prefix argument $mid is empty, don't emit a rule.
  # Similarly, not all strategies have a left rule.

  mylang.add(pn + '-top-coord-rule :=\
               basic-' + pos + '-top-coord-rule &\
               ' + top + 'top-coord-rule &\
               [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].')
  if mid:
    mylang.add(pn + '-mid-coord-rule :=\
                 basic-' + pos + '-mid-coord-rule &\
                 ' + mid + 'mid-coord-rule &\
                 [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].')

  if pre or suf:
    # first the rule in mylang
    mylang.add(pn + '-bottom-coord-rule :=\
               ' + bot + 'bottom-coord-rule &\
               [ SYNSEM.LOCAL.COORD-STRAT "' + num + '",\
                 SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel",\
                 DTR.SYNSEM.LOCAL.CAT.HEAD ' + headtype + ' ].')

    # now the spelling change rule in irules.tdl
    rule = pn + '-bottom :=\n'
    if pre:
      rule += '  %prefix (* ' + pre + ')\n'
    else:
      rule += '  %suffix (* ' + suf + ')\n'
    rule += '  ' + pn + '-bottom-coord-rule.'
    irules.add_literal(rule)
  else:
    rule = pn + '-bottom-coord-rule :=\
           ' + bot + 'bottom-coord-rule &\
           ' + pos + '-bottom-coord-phrase &\
           [ SYNSEM.LOCAL.COORD-STRAT "' + num + '"'
    if bot == 'unary':
      rule += ', SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].'
    else:
      rule += ' ].'
    mylang.add(rule)

  if left:
    mylang.add(pn + '-left-coord-rule :=\
               ' + bot + 'left-coord-rule &\
               ' + pos + '-bottom-coord-phrase.')

  # Now define the rule instances into rules.tdl.  As above, the mid
  # or left rule may not be necessary.

  rules.add(pn + '-top-coord := ' + pn + '-top-coord-rule.')
  if mid:
    rules.add(pn + '-mid-coord := ' + pn + '-mid-coord-rule.')
  rules.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  if left:
    rules.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')
  

# customize_coordination(): the main coordination customization routine

def customize_coordination():
  for i in (1, 2):
    if choices.has_key('cs' + str(i)):
      mark = choices['cs' + str(i) + 'mark']
      pat = choices['cs' + str(i) + 'pat']
      orth = choices['cs' + str(i) + 'orth']
      order = choices['cs' + str(i) + 'order']

      pre = ''
      suf = ''

      print type(orth)
    
      if mark == 'word':
        lexicon.add(orth + '_1 := conj-lex &\
                    [ STEM < "' + orth + '" >,\
                      SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                      CFORM "1" ].')
        if pat == 'omni':
          lexicon.add(orth + '_ns += nosem-conj-lex &\
                        [ STEM < "' + orth + '" > ].')

      if pat == 'a':
        top = 'apoly-'
        mid = ''
        bot = 'unary-'
        left = ''
      else:
        if pat == 'mono':
          top = 'monopoly-'
          mid = 'monopoly-'
          bot = ''
          left = ''
        elif pat == 'omni':
          top = 'omni-'
          mid = 'omni-'
          bot = 'omni-'
          left = 'omni-'
        elif pat == 'poly':
          top = 'apoly-'
          mid = ''
          bot = ''
          left = ''

        if mark == 'affix':
          bot = 'infl-'
          if order == 'before':
            pre = orth
          else:
            suf = orth
        else:
          if order == 'before':
            bot += 'conj-first-'
            if left:
              left += 'conj-first-'
          else:
            bot += 'conj-last-'
            if left:
              left += 'conj-last-'

      for pos in ('n', 'np', 'vp', 's'):
        if choices['cs' + str(i) + pos]:
          define_coord_strat(str(i), pos, top, mid, bot, left, pre, suf)



######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.

def customize_yesno_questions():
  pass


######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.

def customize_lexicon():
  pass


######################################################################
# customize_test_sentences()
#   Create the script file entries for the user's test sentences.

def customize_test_sentences():
  pass


######################################################################
# customize_matrix(path)
#   Create and prepare for download a copy of the matrix based on
#   the choices file in the directory 'path'.  This function
#   assumes that validation of the choices has already occurred.

def customize_matrix(path):
  try:
    f = open(path + '/choices', 'r')
    lines = f.readlines()
    f.close()
    for l in lines:
      l = l.strip()
      w = l.split('=')
      choices[w[0]] = w[1]
  except:
    pass

  path += '/matrix/'

  # TODO: copy the matrix
  if not os.path.exists(path):
    os.mkdir(path)

  global mylang, rules, irules, lrules, lexicon, roots
  mylang =  tdl.TDLfile(path + choices['language'].lower() + '.tdl')
  rules =   tdl.TDLfile(path + 'rules.tdl')
  irules =  tdl.TDLfile(path + 'irules.tdl')
  lrules =  tdl.TDLfile(path + 'lrules.tdl')
  lexicon = tdl.TDLfile(path + 'lexicon.tdl')
  roots =   tdl.TDLfile(path + 'roots.tdl')

  customize_word_order()
  customize_sentential_negation()
  customize_coordination()
  customize_yesno_questions()
  customize_lexicon()
  customize_test_sentences()

  mylang.save()
  rules.save()
  irules.save()
  lrules.save()
  lexicon.save()
  roots.save()

  # TODO: zip it up

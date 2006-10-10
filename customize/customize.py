#!/usr/local/bin/python2.4

######################################################################
# imports

import os
import shutil
import tdl
import tarfile
if os.name == 'nt':
  import gzip
import zipfile

from utils import read_choices

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
# Utility functions
#
# ERB 2006-09-16 There are properties which are derived from the
# choices file as a whole which various modules will want to know about.
# The first example I have is the presence of auxiliaries.  Both the
# negation and yes-no questions modules have cases where they need to
# restrict lexical rules to applying to main verbs only, but only if
# there is in fact a distinction between main and auxiliary verbs (i.e.,
# they need to say [ AUX - ], but only if the feature AUX is defined).

def has_auxiliaries_p():

  return ch('neginfltype') == 'aux' or \
         ch('negseladv') == 'aux' or \
         ch('qinvverb') == 'aux'


# ERB 2006-09-21 This function assembles an inflectional rule out
# of the appropriate information and adds it to irules.tdl.
# Assumes we consistently use 'prefix' and 'suffix' as values in the
# html form.
# Should this actually be a method on TDLfile?

def add_irule(instance_name,type_name,affix_type,affix_form):

  rule = instance_name + ' :=\n'
  if affix_type == 'prefix':
    rule += '%prefix (* ' + affix_form + ')\n'
  elif affix_type == 'suffix':
    rule += '%suffix (* ' + affix_form + ')\n'

# TODO: generate error here.
#  else:
#    error 'probable script bug'

  rule += type_name + '.\n'

  irules.add_literal(rule)

######################################################################
# ch(s)
#   Return the value of choice s, or '' if it has none

def ch(s):
  if choices.has_key(s):
    return choices[s]
  else:
    return ''


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
# auxiliary-verb order.  Since the word order section and the basic vocabulary
# section are closely related, both are documented together here.

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

# In these seed strings, p and aux are assumed to be semantically
# empty.  Det does have semantic content which is distinct from the
# semantic relation introduced by the bare-np phrase.

# In the absence of a module for case (and in languages without case)
# both 's tv o' and 'o tv s' will parse in an ostensibly svo language.  We
# want to distinguish s and o anyway because we would expect those two strings to
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

## Semantically distinct seed strings from the above:

# s iv
# det s iv
# s o tv
# det s o tv
# det s det o tv

# ## Ditransitive verbs.  Det on no arguments, each one separately
# ## each pair, all three.  All of these are semantically distinct
# ## from each other, and from the strings above.  

# s io o dtv
# det s io o dtv
# s det io o dtv
# s io det o dtv
# det s det io o dtv
# det s io det o dtv
# s det io det o dtv
# det s det io det o dtv

# ## P is semantically empty, so the strings below all share semantics
# ## with something in the set above.

# ## Ditransitive verbs.  Det on no arguments, eachone separately
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
# ## aux is semantically empty, so no new semantics here.

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

# head :+ [ AUX bool ] .

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

# The lexical types look like this:

# ;;; Lexical types

# ;;; Nouns

# noun-lex := basic-noun-lex &
#   basic-one-arg &
#   [ SYNSEM.LOCAL [ CAT.VAL [ SPR < #spr &
#                                    [ LOCAL.CAT.HEAD det ] >,
#                              COMPS < >,
#                              SUBJ < >,
#                              SPEC < > ] ],
#     ARG-ST < #spr > ] .

# obl-spr-noun-lex := noun-lex &
#   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ] .

# ;;; Verbs

# verb-lex := basic-verb-lex &
#   [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,
#                                SPEC < >,
#                                SUBJ < #subj > ] ],
#                    CONT.HOOK.XARG #xarg ],
#     ARG-ST < #subj &
#              [ LOCAL [ CAT.VAL [ SPR < >,
#                                  COMPS < > ],
#                        CONT.HOOK.INDEX #xarg ] ], ... > ] .

# intransitive-verb-lex := verb-lex &
#   intransitive-lex-item &
#   [ SYNSEM.LOCAL.CAT.VAL.COMPS < >,
#     ARG-ST < [ LOCAL.CAT.HEAD adp ] > ] .

# transitive-verb-lex := verb-lex &
#   transitive-lex-item &
#   [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >,
#     ARG-ST < [ LOCAL.CAT.HEAD adp ], #comps &
#                                      [ LOCAL.CAT [ VAL [ SPR < >,
#                                                          COMPS < > ],
#                                                    HEAD adp ] ] > ] .

# ;;; Case-marking adpositions
# ;;; Case marking adpositions are constrained not to
# ;;; be modifiers.

# case-marker-p-lex := basic-one-arg &
#   raise-sem-lex-item &
#   [ SYNSEM.LOCAL.CAT [ HEAD adp &
#                             [ MOD < > ],
#                        VAL [ SPR < >,
#                              SUBJ < >,
#                              COMPS < #comps >,
#                              SPEC < > ] ],
#     ARG-ST < #comps &
#              [ LOCAL.CAT [ HEAD noun,
#                            VAL.SPR < > ] ] > ] .


# ;;; Determiners
# ;;; SPEC is non-empty, and already specified by basic-determiner-lex.

# determiner-lex := basic-determiner-lex &
#   basic-zero-arg &
#   [ SYNSEM.LOCAL.CAT.VAL [ SPR < >,
#                            COMPS < >,
#                            SUBJ < > ] ] .



# 4. Description of cases handled

# For the major constituent orders, this module handles: all fixed
# orders of S,O, and V, V-final, V-initial and "free".  It allows for
# strict det-n and strict n-det, as well as strict P-NP and strict
# NP-P.  In addition, it allows auxiliaries to appear strictly left,
# strictly right or either to the left or to the right of the
# constituent they combine with.  The lexicon module allows for
# auxiliaries which combine with V (argument composition), VP
# (raising) or S (??), and contribute their own preds or not.
# The lexicon module also allows for each verb to select for NP or
# PP arguments in each position (subject/object), and for nouns
# to have obligatory, optional, or no determiners.

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

# The tdl for the lexical types is pretty straightforward, in the
# case of nouns and verbs.  They inherit from the relevant supertypes
# in matrix.tdl, link ARG-ST elements to VAL elements, and constrain
# other VAL lists to be empty.  They also give the HEAD value for
# each argument.  Nouns might further provide a value for OPT on the
# specifier.  Verbs identify the XARG with the first ARG-ST element's
# INDEX.  The determiners just inherit from basic-zero-arg and make
# empty VAL lists (maybe that should be on basic-zero-arg anyway?).
# The case-marking adpositions inherit from basic-one-arg and raise-sem-lex-item.
# They link their sole argument, an NP, to a COMPS list. Further, they
# are MOD < >.

# The trickiest lexical types are the auxiliaries.  They are cross-classified
# along three dimensions: V, VP or S complement, pred or no pred, and
# (for V or VP complement) NP v. PP subject.  

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

  wo = ch('wordorder')

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

  if ch('hasDets') == 't':
    mylang.add('head-spec-phrase := basic-head-spec-phrase.',
               'Rules for bulding NPs.  Note that the Matrix uses SPR for\n\
               the specifier of nouns and SUBJ for the subject (specifier) of verbs.')

    if ch('NounDetOrder') == 'HeadSpec':
      mylang.add('head-spec-phrase := head-initial.')
    if ch('NounDetOrder') == 'SpecHead':
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
  qpart_order = 'easy'

  # Is the ordering of adpositions consistent with the ordering of O and V?
  # Assuming that adpositions are consistent within a language (i.e., you won't
  # find subject postpositions and object prepositions).

  adpOrder = ''
  if ch('subjAdp'):
    adpOrder = ch('subjAdp')
  elif ch('objAdp'):
    adpOrder = ch('objAdp')

  # ERB 2006-10-05 Fixing bug in free word order case.

  if adpOrder:
    if wo == 'free':
      if adpOrder == 'pre':
        adp = 'free-prep'
      elif adpOrder == 'post':
        adp = 'free-post'
    elif hc == 'comp-head' and adpOrder == 'pre':
      adp = 'ov-prep'
    elif hc == 'head-comp' and adpOrder == 'post':
      adp = 'vo-postp'

  # Now what about auxiliaries?

  if ch('auxverb'):
    if wo == 'free':
      if ch('auxorder') == 'left':
        aux = 'free-auxv'
      elif ch('auxorder') == 'right':
        aux = 'free-vaux'
    elif hc == 'comp-head' and ch('auxorder') == 'left':
      aux = 'ov-auxv'
    elif hc == 'head-comp' and ch('auxorder') == 'right':
      aux = 'vo-vaux'

  # ERB 2006-10-05 And what about the order of question particles wrt
  # to other kinds of head-comp?  I'm assuming for now that question particles
  # and other complementizers will behave the same way.  Are there languages
  # in which that is not true?

  if ch('qpartposthead'):
    if wo == 'free':
      if ch('qpartposthead') == '+':
        qpart_order = 'free-sq'
      elif ch('qpartposthead') == '-':
        qpart_order = 'free-qs'
    elif hc == 'comp-head' and ch('qpartposthead') == '-':
      qpart_order = 'ov-qs'
    elif hc == 'head-comp' and ch('qpartposthead') == '+':
      qpart_order = 'vo-sq'

   # return what we learned

  return {'adp': adp, 'aux': aux, 'qpart_order': qpart_order}

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

def specialize_word_order(hc,orders):

  adp = orders['adp']
  aux = orders['aux']
  qpart_order = orders['qpart_order']

  # ERB 2006-09-15 First add head-comp or comp-head if they aren't
  # already there.  I don't think we have to worry about constraining
  # SUBJ or COMPS on these additional rules because they'll only be for
  # adp or auxv or qpart, so far.

  if hc == 'comp-head' and (adp == 'ov-prep' or aux == 'ov-auxv' or qpart_order == 'ov-qs'):
    mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial.')

  if hc == 'head-comp' and (adp == 'vo-post' or aux == 'vo-vaux' or qpart_order == 'vo-sq'):
    mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final.')

  # Add rules to rules.tdl when necessary

  if aux == 'ov-auxv' or adp == 'ov-prep' or qpart_order == 'ov-qs':
    rules.add('head-comp := head-comp-phrase.')

  if aux == 'vo-vaux' or adp == 'vo-post' or qpart_order == 'vo-sq':
    rules.add('comp-head := comp-head-phrase.')

  # ERB 2006-09-15 AUX if we're going to mention it, so the tdl compiles.

  if aux != 'easy':
    mylang.add('head :+ [AUX bool].')

  # ERB 2006-10-05 Collect positive statements about head-comp/comp-head
  # We only need to do this is if the word order is not free, and we only
  # need to do it for one of head-comp or comp-head, depending on the order
  # of o and v.

  head_comp_is = []
  comp_head_is = []

  # VO order
  if aux == 'vo-vaux':
     comp_head_is.append('verb')
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

  #print 'Aux is: ' + aux + '\n'
  #print 'Adp is: ' + adp + '\n'
  #print 'Qpart_order is: ' + qpart_order + '\n'
  #print str(head_comp_is) + '\n'

  # ERB 2006-10-05 Collect negative statements about head-comp/comp-head.
  # This needs to be done even if the word order is free.  It might need
  # to be done for both head-comp and comp-head in a free word order language
  # with inconsistent constraints on comp, adp, and aux.

  head_comp_is_not = []
  comp_head_is_not = []

  if aux == 'free-auxv' or aux == 'ov-auxv':
    comp_head_is_not.append('aux')
  if aux == 'free-vaux' or aux == 'vo-vaux':
    head_comp_is_not.append('aux')
  if adp == 'free-prep' or adp == 'ov-prep':
    comp_head_is_not.append('adp')
  if adp == 'free-post' or adp == 'vo-prep':
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

  if len(head_comp_is) == 1:
    head = head_comp_is[0]
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'head-comp-phrase can requires things that are [ HEAD ' + head + ' ].')

  if len(comp_head_is) == 1:
    head = comp_head_is[0]
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'comp-head-phrase can requires things that are [ HEAD ' + head + ' ].')

  # Now the case where we do have disjunctive constraints.   NB: The order
  # here is important, since we're constructing the string which has to
  # correspond to the disjunctive head type.  While those are logically just
  # disjunctions, as far as the LKB is concerned, they're type names, so
  # we need an exact string match.

  # +nvjrcdmo. 
  # +nvjrpdmo.  


  if len(head_comp_is) > 1:
    head = '+'
    if head_comp_is.count('verb'):
      head += 'v'
    if head_comp_is.count('adp'):
      head += 'p'
    if head_comp_is.count('comp'):
      head += 'c'

    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'head-comp-phrase can requires things that are one of: ' + str(head_comp_is))

  if len(comp_head_is) > 1:
    head = '+'
    if comp_head_is.count('verb'):
      head += 'v'
    if comp_head_is.count('adp'):
      head += 'p'
    if comp_head_is.count('comp'):
      head += 'c'

    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
               'head-comp-phrase can requires things that are one of: ' + str(head_comp_is))

  # Now the negative constraints.  This is where we extracted the information
  # that head-comp or comp-head can't be certain things.  Of course, in tdl
  # we only have positive constraints.  So, we have to translate the information
  # to positive constraints.  Currently, these will always involve disjunctive
  # types, since we have more pos types than things we're collecting negative info
  # about.  The other case is that we have no negative constraints, in which case
  # there's nothing to do.  We can just leave the HEAD value completely underspecified.

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


  # ERB 2006-10-05 Auxiliaries are a bit special, because they involve a
  # feature as well.  The negative constraint (is not an auxiliary) is
  # just a constraint on the value of the feature: AUX -.  The positive
  # constraint (can be an auxiliary) is verb & [AUX +].  That confused
  # me for a moment, because "can be an auxiliary" seems consistent with
  # "can be a verb that is not an auxiliary", but in fact, the only cases
  # where we would want that are the free word order cases, and we're not
  # putting anything into the positive constraint lists when word
  # order is free (see above).  The only time we have an entry for verb in
  # one of these lists is if auxiliaries show up where non-auxes
  # can't.  I'm using "verb" as the entry instead of "aux" because that allows
  # me to use the values in the list directly to produce some of the positive
  # constraints above.

  if comp_head_is_not.count('verb'):
    mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
               'comp-head-phrase is restricted from taking auxiliaries as its head.')

  if head_comp_is_not.count('verb'):
    mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
               'head-comp-phrase is restricted from taking auxiliaries as its head.')


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


######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

# ERB 2006-10-05 DOCUMENTATION

# 1. Statement of domain

# This module handles the expression of sentential negation.  It
# provides two general strategy types (inflection v. adverb), several
# variations on each type, and what I believe to be an exhaustive listing
# of the possible ways of combining the two strategies.

# 2. Seed strings/requirements of POS lexicon

# In addition to the lexical entries given in the word order module,
# this module requires:

# neg (negative adverb)
# -neg (negative suffix)
# neg- (negative prefix)

# Note: at some point, the permutation machinery will need to do
# interesting things with affix order.  For now, there's not really
# enough affixes to worry about it.

# These seed strings are designed to work with languages that require
# dets and Ps as well as those that do not.  They are written assuming
# sov order, and use a transitive frame as the base to make the testing
# a little more interesting (e.g., when you have aux involved).  For
# the aux case, I'm assuming an auxiliary that follows the verb, and takes
# a VP complement (subject raising).

## These seed strings should each have one of two semantic representations,
## the only difference being the presence v. absence of det.  So,
## these two seed strings, parsed using a grammar that has negative
## adverbs as independent modifiers to the left o V, should give the
## two MRSs that we need:  (Once again, I'm assuming semantically empty aux.)

# s o neg tv
# det s det o neg tv

## Negative adverb --- the module allows for attachment at V, VP, and S,
## and to the left, right, or either.  Since we'll get all of these
## positions (and more!) by doing the permutation then, I'm just doing
## one set with a negative adverb, using the left of V placement:

# s o neg tv
# det s det o neg tv
# p s p o neg tv
# p det s p det o neg tv
# det s det o neg tv aux
# p det s p det o tv neg aux
# s o tv neg aux
# p s p o tv neg aux

# ## Prefix, on main verbs and aux:

# s o neg-tv
# det s det o neg-tv
# p s p o neg-tv
# p det s p det o neg-tv
# s o tv neg-aux
# det s det o tv neg-aux
# p s p o tv neg-aux
# p det s p det o tv neg-aux

# ## Suffix, on main verbs and aux:

# s o tv-neg
# det s det o tv-neg
# p s p o tv-neg
# p det s p det o tv-neg
# s o tv aux-neg
# det s det o tv aux-neg
# p s p o tv aux-neg
# p det s p det o tv aux-neg

# ## Adverb plus prefix, on main verbs and aux:

# s o neg neg-tv
# det s det o neg neg-tv
# p s p o neg neg-tv
# p det s p det o neg neg-tv
# s o tv neg neg-aux
# det s det o tv neg neg-aux
# p s p o tv neg neg-aux
# p det s p det o tv neg neg-aux

# ## Adverb plus suffix, on main verbs and aux:

# s o neg tv-neg
# det s det o neg tv-neg
# p s p o neg tv-neg
# p det s p det o neg tv-neg
# s o tv neg aux-neg
# det s det o tv neg aux-neg
# p s p o tv neg aux-neg
# p det s p det o tv neg aux-neg



# Unhandled case:  Main verbs take inflection + adverb, auxiliaries only
# inflection (or vice versa).

# ERB 2006-09-16 First pass at replicating functionality from
# perl script.

def customize_sentential_negation():

  # ERB 2006-09-16 Calculate a bunch of derived properties based on the
  # inputs they gave for negation.  The same thing (e.g., negation via
  # inflection on the main verb) gives a very different output depending
  # on whether there are other options (negation via selected adverb)
  # and how they combine.

  advAlone = ''
  multineg = ch('multineg')
  if ch('adv_neg') == 'on' or multineg == 'comp':
    advAlone = 'always'
  if multineg == 'bothopt' or multineg == 'advobl':
    advAlone = 'sometimes'
  if multineg == 'bothobl' or multineg == 'inflobl':
    advAlone = 'never'

  # ERB 2006-09-16 TODO: The perl script had an else on the above if
  # statment which generated a "probable script error" if we fell into
  # it.  It's probably good idea to put that in here, too.

  if ch('adv_neg') == 'on' and ch('neg-adv') == 'sel-adv':
    create_neg_add_lex_rule(advAlone)
    create_neg_adv_lex_item(advAlone)

  if ch('infl_neg') == 'on' and multineg != 'bothobl' and multineg != 'advobl':
    create_neg_infl_lex_rule()

  if ch('adv_neg') == 'on' and ch('neg-adv') == 'ind-adv':
    if advAlone == 'never':
      # Override user input: multineg as bothobl or inflobl means
      # we go with the selected adverb analysis.
      create_neg_add_lex_rule(advAlone)

    create_neg_adv_lex_item(advAlone)

# ERB 2006-09-21 neg-add-lex rule, for negation strategies that involve
# selected adverbs.

def create_neg_add_lex_rule(advAlone):

  # ERB 2006-09-17 The lexical rule conditions both need these
  # variables, so declare them here.

  pre = ''
  suf = ''
  orth = ''
  rule = ''

  # This first bit is shared by all the grammar types where we want
  # the neg-add-lex-rule.

  # ERB 2006-09-21 The value of COMPS on the mother is prettier
  # with the . notation rather than FIRST/REST, but for now tdl.py
  # isn't handling that case.
  #                                 COMPS < [ LOCAL [ CONT [ HOOK [ INDEX #negind,\
  #                                                                  LTOP #negltop ],\
  #                                                          HCONS <! [ LARG #larg ] !> #]],\
  #                                                   LKEYS.KEYREL.PRED \"_neg_r_rel\" ] . #comps > ],\

  mylang.add('''neg-add-lex-rule := local-change-only-lex-rule &
                       same-ctxt-lex-rule &
                       same-agr-lex-rule &
                       same-head-lex-rule &
                       same-hc-light-lex-rule &
                       same-posthead-lex-rule &
     [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
                                  SPR #spr,
                                  SPEC #spec ,
                                  COMPS [ FIRST [ LOCAL.CONT [ HOOK [ INDEX #negind,
                                                                      LTOP #negltop ],
                                                               HCONS <! [ LARG #larg ] !> ],
                                                  LKEYS.KEYREL.PRED "_neg_r_rel" ],
                                          REST #comps ]],
                        CONT [ HOOK [ INDEX #negind,
                                      LTOP #negltop,
                                      XARG #xarg ],
                               MSG #msg ]],
        DTR lex-item &  [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ #subj,
                                                     SPR #spr,
                                                     SPEC #spec,
                                                     COMPS #comps ],
                                               HEAD verb ] ],
                                         CONT [ HOOK [ LTOP #larg,
                                                       XARG #xarg ],
                                               MSG #msg ]]]].''',
                                               '''This lexical rule adds a selected negative\n
                                               adverb to the beginning of the COMPS list''')

  #Decide what to do with AUX value.

  if ch('neg-sel-adv') == 'aux':
    mylang.add('neg-add-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].'
               'This rule applies only to auxiliaries.')

  if ch('neg-sel-adv') == 'main' and has_auxiliaries_p():
    mylang.add('neg-add-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].'
               'This rule applies only to main verbs.')

    #Make subtypes and instances as appropriate, depending on advAlone condition.

  if advAlone == 'always':
    mylang.add('neg-add-lex-rule := constant-lex-rule.'
               'Thie type is instantiated in lrules.tdl.')
    
    lrules.add('neg-add-lr := neg-add-lex-rule.')

      
    # TODO: I really just want to add a comment to this type in this case.  Will
    # this syntax do it?  If not, is there some other syntax in tdl.py that will?
    
  if advAlone == 'sometimes':
    mylang.comment('neg-add-lex-rule',
               'This type has subtypes instantiated by instances in both\n\
               irules.tdl and lrules.tdl.')
    mylang.add('infl-neg-add-lex-rule := neg-add-lex-rule & inflecting-lex-rule.')
    mylang.add('const-neg-add-lex-rule := neg-add-lex-rule & constant-lex-rule.')

    lrules.add('neg-add-lr := const-neg-add-lex-rule.')

    add_irule('neg-add-lr','infl-neg-add-lex-rule',ch('neg-aff'),ch('neg-aff-form'))

  if advAlone == 'never':
    mylang.add('neg-add-lex-rule := inflecting-lex-rule.'
               'This type is instantiated in irules.tdl.')
      
    add_irule('neg-add-lr','neg-add-lex-rule',ch('neg-aff'),ch('neg-aff-form'))

# ERB 2006-09-21 Create negative inflection lexical rule
#Inflection without selected adverb
#This one adds the '_neg_r_rel, and as such is only used
#when inflection appears alone (infl strategy only, both
#strategies with multineg = comp, bothopt, inflobl).

#Spell _neg_r_rel with leading _ even though it is introduced
#by the lexical rule so that "the cat didn't sleep" and "the
#cat did not sleep" have the same representation.

#Copying up LKEYS here because I use the KEYREL.PRED to select
#the neg adv in the neg-add-lex-rule.  We don't want the output
#of this rule to be a possible first complement to a neg-add aux.
#If we find another way to select the neg adv, something will
#probably need to be changed here.

def create_neg_infl_lex_rule():

  mylang.add('neg-infl-lex-rule := cont-change-only-lex-rule &\
	                     inflecting-lex-rule &\
	   [ C-CONT [ MSG #msg,\
	              HOOK [ XARG #xarg,\
	                     LTOP #ltop,\
	                     INDEX #ind ],\
	              RELS <! event-relation &\
	                      [ PRED "_neg_r_rel",\
	                        LBL #ltop,\
	                        ARG0 #ind,\
	                        ARG1 #harg ] !>,\
	              HCONS <! qeq &\
	                       [ HARG #harg,\
	                         LARG #larg ] !> ],\
	     SYNSEM.LKEYS #lkeys,\
	     DTR lex-item & \
	         [ SYNSEM [ LKEYS #lkeys,\
	                    LOCAL [ CONT [ MSG #msg,\
	                                 HOOK [ XARG #xarg,\
	                                        LTOP #larg ]],\
	                          CAT.HEAD verb]]]].',
             'This lexical rule adds the neg_r_rel to the verb\'s\n\
	RELS list.  It is instantiated by a spelling-changing\n\
	rule as specified in irules.tdl.')

  # ERB 2006-10-05 Adding AUX here independently from adding it anywhere
  # else, in case we need it.
  if has_auxiliaries_p:
    mylang.add('head :+ [ AUX bool ].')

  if ch('neg-infl-type') == 'aux':
    mylang.add('neg-infl-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
               'This rule applies only to auxiliaries.')

  if ch('neg-infl-type') == 'main' and has_auxiliaries_p:
    mylang.add('neg-infl-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
               'This rule applies only to main verbs.')

  add_irule('neg-infl-lr','neg-infl-lex-rule',ch('neg-aff'),ch('neg-aff-form'))

# ERB 2006-09-22 Create lexical types and lexical entries for 

def create_neg_adv_lex_item(advAlone):

  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD.MOD < [ LOCAL.CAT.HEAD verb ] > ]].''',
             'Type for negative adverbs.')

  # ERB 2006-10-06 Below was advAlone == 'always', but that seems wrong.
  # changing it to advAlone == 'never' being the case where we don't want
  # the adverb to be a modifier.

  if advAlone == 'never':
    mylang.comment('neg-adv-lex','''Constrain the MOD value of this adverb to keep\n
    it from modifying the kind of verbs which can select it,\n
    To keep spurious parses down, as a starting point, we have\n
    assumed that it only modifies verbs (e.g., non-finite verbs).''')

  if ch('negprepostmod') == 'pre':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
  elif ch('negprepostmod') == 'post':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

  if ch('negmod') == 's':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
  elif ch('negmod') == 'vp':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
  elif ch('negmod') == 'v':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.LIGHT + ].''')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HG-LIGHT - to allow us to pick out\n
    lexical Vs for V-level attachment of negative adverbs.''')

  # ERB 2006-09-22 Validation should really make sure we have a value of
  # negadvform before we get here, but just in case, checking first, since
  # the script gets really unhappy if I try to write to an empty type.

  if(ch('negadvform')):
    lexicon.add(ch('negadvform') + ' := neg-adv-lex &\
                [ STEM < \"'+ ch('negadvform') +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"_neg_r_rel\" ].')
                                         

  # ERB 2006-10-06 And of course we need the head-modifier rules, if we're
  # going to have an independent modifier.  While we're at it, we need to
  # contrain the MOD value on the rest of the head types to keep them
  # from going nuts.

  if advAlone != 'never':
    rules.add('head-adj-int := head-adj-int-phrase.',
              'Rule instances for head-modifier structures. Corresponding types\n' +
              'are defined in matrix.tdl.  The matrix customization script did\n' +
              'not need to add any further constraints, so no corresponding tyes\n' +
              'appear in ' + choices['language'].lower() + '.tdl')
    rules.add('adj-head-int := adj-head-int-phrase.')
    rules.add('head-adj-scop := head-adj-scop-phrase.')
    rules.add('adj-head-scop := adj-head-scop-phrase.')

    mylang.add('+nvcdmo :+ [ MOD < > ].',
               'This grammar includes head-modifier rules.  To keep\n' +
               'out extraneous parses, constrain the value of MOD on\n' +
               'various subtypes of head.  This may need to be loosened later.\n' +
               'This constraint says that only adverbs, adjectives,\n' +
               'and adpositions can be modifiers.')
  
######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

# sfd 2006-09-24 DOCUMENTATION
#
# 1. Statement of domain
#
# This module handles coordination of various phrase types (currently
# N, NP, VP, and S).  It relies on choices in the Coordination section
# of the questionnaire.
#
# 2. Seed strings/requirements of POS lexicon
#
# The phenomena covered by this module can be illustrated using the following
# lexical entries:
#
# det (determiner)
# s1 (subject noun)
# s2 (subject noun)
# s3 (subject noun)
# iv1 (intransitive verb)
# iv2 (intransitive verb)
# iv3 (intransitive verb)
# co (conjunction)
# -co or co- (affix versions of the conjunction)
#
# Seed strings, not all of which will have valid permutations in all languages.
#
# These seed strings are written assuming sov order, det-n order, and
# mandatory determiners.  Each section below will show the coordination
# of one phrase type in with several different coordination strategies,
# namely:
#   monosyndeton word before
#   monosyndeton word after
#   monosyndeton affix before (for N only)
#   monosyndeton affix after  (for N only)
#   polysyndeton word before
#   polysyndeton word after
#   polysyndeton affix before (for N only)
#   polysyndeton affix after  (for N only)
#   omnisyndeton word before
#   omnisyndeton word after
#   omnisyndeton affix before (for N only)
#   omnisyndeton affix after  (for N only)
#   asyndeton
#
## Coordination of nouns (each group of sentences should have the
## same semantic representation, though with different grammars)
#
# det s1 s2 co s3 iv1
# det s1 s2 s3 co iv1
# det s1 s2 co-s3 iv1
# det s1 s2 s3-co iv1
#
# det s1 co s2 co s3 iv1
# det s1 s2 co s3 co iv1
# det s1 co-s2 co-s3 iv1
# det s1 s2-co s3-co iv1
#
# det co s1 co s2 co s3 iv1
# det s1 co s2 co s3 co iv1
# det co-s1 co-s2 co-s3 iv1
# det s1-co s2-co s3-co iv1
#
# det s1 s2 s3 iv1
#
## Coordination of noun phrases (each group of sentences should have the
## same semantic representation, though with different grammars)
#
# det s1 det s2 co det s3 iv1
# det s1 det s2 det s3 co iv1
#
# det s1 co det s2 co det s3 iv1
# det s1 det s2 co det s3 co iv1
#
# co det s1 co det s2 co det s3 iv1
# det s1 co det s2 co det s3 co iv1
#
# det s1 det s2 det s3 iv1
#
## Coordination of verb phrases (each group of sentences should have the
## same semantic representation, though with different grammars)
#
# det s1 iv1 iv2 co iv3
# det s1 iv1 iv2 iv3 co
#
# det s1 iv1 co iv2 co iv3
# det s1 iv1 iv2 co iv3 co
#
# det s1 co iv1 co iv2 co iv3
# det s1 iv1 co iv2 co iv3 co
#
# det s1 iv1 iv2 iv3
#
## Coordination of sentences
#
# det s1 iv1 det s2 iv2 co det s3 iv3
# det s1 iv1 det s2 iv2 det s3 iv3 co
#
# det s1 iv1 co det s2 iv2 co det s3 iv3
# det s1 iv1 det s2 iv2 co det s3 iv3 co
#
# co det s1 iv1 co det s2 iv2 co det s3 iv3
# det s1 iv1 co det s2 iv2 co det s3 iv3 co
#
# det s1 iv1 det s2 iv2 det s3 iv3
#
# 3. TDL Samples
#
# for an language with a single monosyndeton word-before strategy, we should
# get the following TDL.
#
## mylanguage.tdl:
#
# ;;; Coordination
#
# n1-top-coord-rule := basic-n-top-coord-rule & monopoly-top-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# n1-mid-coord-rule := basic-n-mid-coord-rule & monopoly-mid-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# n1-bottom-coord-rule := conj-first-bottom-coord-rule & n-bottom-coord-phrase &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
#
# np1-top-coord-rule := basic-np-top-coord-rule & monopoly-top-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# np1-mid-coord-rule := basic-np-mid-coord-rule & monopoly-mid-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# np1-bottom-coord-rule := conj-first-bottom-coord-rule & np-bottom-coord-phrase &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
#
# vp1-top-coord-rule := basic-vp-top-coord-rule & monopoly-top-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# vp1-mid-coord-rule := basic-vp-mid-coord-rule & monopoly-mid-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# vp1-bottom-coord-rule := conj-first-bottom-coord-rule & vp-bottom-coord-phrase &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
#
# s1-top-coord-rule := basic-s-top-coord-rule & monopoly-top-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# s1-mid-coord-rule := basic-s-mid-coord-rule & monopoly-mid-coord-rule &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
# s1-bottom-coord-rule := conj-first-bottom-coord-rule & s-bottom-coord-phrase &
#   [ SYNSEM.LOCAL.COORD-STRAT "1" ].
#
## lexicon.tdl:
#
# and_1 := conj-lex &
#   [ STEM < "and" >,
#     SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",
#     CFORM "1" ].
#
# 4. Description of cases handled
#
# For the four phrase types N, NP, VP, and S, this module handles every
# combination of the following three dimensions, with a few exceptions
# noted below:
#
# Marking pattern: monosyndeton, polysyndeton, omnisyndeton, asyndeton
# Type of mark: word or affix (with some spelling)
# Position of mark: before or after coordinand
#
# Exceptions: when the pattern is asyndeton, type and position of mark,
# as well as its spelling, must not be specified since they are
# meaningless.  Also, an affix mark can currently only be specified
# for lexical phrase types (i.e. just N).
#
# 5. Known unhandled cases
#
# There are a few coordination strategies that this module cannot encode:
#
# Classical Tibetan and Amharic(Haspelmath 2000):
#   In strategies usually marked A-co B-co C, all but the *first* co
#   can be optionally omitted, giving A-co B C (and so on for N>3).
# Inupiaq (ref?):
#   There is a strategy in this language that is mandatorily bisyndetic
#   when there are two coordinands, but *mandatorily* monosyndetic for
#   3 or more coordinands.  That is, we get A-lu B-lu for two, but
#   A B C-lu for 3 or more.  A-lu B-lu C-lu and A B-lu C-lu are not
#   allowed.
# Indonesian (ref?):
#   A strategy that uses different conjunctions based on position:
#     A B serta C
#     A dan B serta C
#   * A serta B dan C
#   * A serta B serta C
#
# In addition to these, there are some coordination strategies that can
# be implemented using the Matrix rules, but which cannot be described
# with the controls in the web questionnaire.  For example, Hebrew has
# a strategy (ref?) that, like quite a few other strategies including
# Latin -que (ref?), marks the first word of a coordinated phrase.  Such
# a strategy could be implemented by adding a new binary feature, say
# COORDMARKED, that is added by a morphological marking rule and identified
# between all phrases and their first daughters.  Another odd strategy
# is found in Kannada (ref?), where -uu is added to *every word* in a
# coordinated phrase.  The implementation of this would be similar to the
# first-word marking mentioned above, except that the new COORDMARKED
# feature would be identified between phrases and *all* of their daughters.
#
# 6. Description of tdl
#
# For every phrase type that has a coordination strategy, there will be
# a set of rules in mylanguage.tdl created with the same COORD-STRAT value.
# These rules derive from the coordination rules in matrix.tdl (currently
# near tbe bottom of the file), and specify the type of phrase being
# coordinated.  They will have names that begin with the part of speech
# (n, np, vp, s) and the number of the strategy.  For details about the
# implementation, see the comments in matrix.tdl or Drellishak & Bender
# 2005.
#
# 7. Description of customization (python)
#
# The entry point for this code is customize_coordination().  It loops
# through every coordination strategy (currently only 1 and 2).  For each,
# it looks at the user's choices and, from them, constructs a series of
# parameters containing the prefixes and suffixes of the rules that
# will be output (which also determines the prefixes and suffixes of the
# matrix.tdl rules they will derive form.
#
# These are the parameters:
#   num: the number of the strategy
#   pos: the part of speech
#   top: the prefix of the top rule
#   mid: the prefix of the mid rule (empty for poly- and asyndeton)
#   bot: the prefix of the bottom rule
#   left: the prefix of the left rule (only for omnisyndeton)
#   pre: the spelling of the prefix coordination mark (possibly empty)
#   suf: the spelling of the suffix coordination mark (possibly empty)
#
# These parameters are passed to define_coord_strat(), a utility function
# that outputs the actual rules.
#
# 8. Required changes to other modules
#
# None so far.


######################################################################
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
  

######################################################################
# customize_coordination(): the main coordination customization routine

def customize_coordination():
  for n in (1, 2):
    i = str(n)
    if choices.has_key('cs' + i):
      mark = ch('cs' + i + 'mark')
      pat = ch('cs' + i + 'pat')
      orth = ch('cs' + i + 'orth')
      order = ch('cs' + i + 'order')

      pre = ''
      suf = ''

      if mark == 'word':
        lexicon.add(orth + '_1 := conj-lex &\
                    [ STEM < "' + orth + '" >,\
                      SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                      CFORM "1" ].')
        if pat == 'omni':
          lexicon.add(orth + '_ns := nosem-conj-lex &\
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
        if ch('cs' + i + pos):
          define_coord_strat(i, pos, top, mid, bot, left, pre, suf)



######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.

# DOCUMENTATION

# 2. Seed strings, requirements of POS lexicon:

# The only addition to the POS lexicon is qpart, the question particle:

# qpart

# The seed strings are written assuming an SVO language with no
# adpositions (in order to illustrate inversion).  Again all the
# seed strings have the same semantics, modulo determiners.  For
# the purposes of harvesting the semantics, I recommend these two
# strings, parsed with an SOV grammar which expresses yes no questions
# with a sentence final question particle:

# s o tv qpart
# det s det o tv qpart

# Now the SVO seed strings:

# s tv o qpart
# tv s o 
# aux s tv o

# det s tv det o qpart
# tv det s det o 
# aux det s tv det o

# Since this module actually involves changes to word order, the
# seed strings/permutation interaction is actually different.  I
# think it will come down to being careful with the filters...

# Note also that the semantic contrast between questions and non-questions
# is that questions are marked as [ MESG ques ] while non-questions are
# [ MESG prop-or-ques ].  The type prop-or-ques subsumes ques, but
# in [incr tsdb()], we'll be looking at exact match semantics, not
# unification or other measures of compatibility.

# 8. Changes to other modules:

# Moving to the complementizer analysis of question particles impacted
# the word order module.  In particular, we now have to worry about
# the order of question particles with respect to heads when we do the
# head-comp and comp-head rules, just like we have to worry about the
# adpositions.  I also had to touch the root definition, to allow for
# CPs as roots. 

def customize_yesno_questions():
  ques = ch('ques')
  qinvverb = ch('qinvverb')
  qpartposthead = ch('qpartposthead')
  qpartform = ch('qpartform')

  if qinvverb == 'aux':
    comment = \
      'Your grammar has auxiliaries, so we are adding the features AUX\n' + \
      'and FORM to the type verb.  We are assuming that auxiliaries\n' + \
      'select non-finite verbal projections for their complements.\n' + \
      '\n' + \
      'To allow for a simpler statement of word order rules (in some\n' + \
      'grammars) we add the feature AUX to the type head, rather than verb.'
    mylang.add('head :+ [ AUX bool ].', comment)
    mylang.add('verb :+ [ FORM form ].')

  if ques == 'inv':
    comment = \
      'For the analysis of inverted yes-no questions, we add the feature INV.'
    mylang.add('verb :+ [ INV bool ].', comment)

    comment = \
      'Rule for inverted subject verb order in questions.\n' + \
      'The incompatible SUBJ values on SYNSEM and DTR are\n' + \
      'what keeps this one from spinning.'
    if qinvverb == 'aux':
      aux = ', AUX +'
    elif qinvverb == 'main':
      aux = ', AUX -'
    elif qinvverb == 'main-aux':
      aux = ''
      # ERB 2006-10-05 Adding in semantics here.  This rule constrains MESG to ques.  
    typedef = '''
    subj-v-inv-lrule := val-change-only-lex-rule &
      constant-lex-rule &
      [ SYNSEM [ LOCAL [ CAT [ HEAD verb & [ INV +''' + aux + ''' ],
                               VAL [ COMPS < #subj . #comps >,
                                     SUBJ < >,
                                     SPR #spr,
                                     SPEC #spec ]],
                         CONT.INDEX.MESG ques ],
                 LKEYS #lkeys ],
        DTR.SYNSEM [ LOCAL.CAT.VAL [ SUBJ < #subj >,
                                     COMPS #comps,
                                     SPR #spr,
                                     SPEC #spec ],
                     LKEYS #lkeys ]].'''
    mylang.add(typedef, comment)

    lrules.add('inv-lr := subj-v-inv-lrule.')

  # ERB 2006-10-05 Moving away from the modifier analysis of question particles
  # which I think doesn't handle the facts well.  These look more like complementizers
  # to me.

  if ques == 'qpart':
    comment = \
             'We treat question particles as complementizers.\n' + \
             'Here is the lexical type for complementizers.'
    typedef = '''
      complementizer-lex-item := raise-sem-lex-item & basic-one-arg &
         [ SYNSEM.LOCAL.CAT [ HEAD comp &
                                   [ MOD < > ],
                              VAL [ SPR < >,
                                    SUBJ < >,
                                    COMPS < #comp > ]],
           ARG-ST < #comp & [ LOCAL.CAT [ HEAD verb,
                                          VAL [ SUBJ < >,
                                                COMPS < > ]]] > ]
                                                .'''
    mylang.add(typedef,comment)

    comment = 'Subtype for question particles. Constrains MESG to ques.'
    typedef = '''
      qpart-lex-item := complementizer-lex-item &
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.MESG ques ].'''
    mylang.add(typedef,comment)

    #   if ques == 'qpart':
    #     comment = \
    #       'This grammar includes head-modifier rules.  To keep out\n' + \
    #       'extraneous parses, constrain the value of MOD on various subtypes\n' + \
    #       'of head.  This may need to be loosened later.  This constraint\n' + \
    #       'says that only adverbs, adjectives, and adpositions can be modifiers.'
    #     mylang.add('+nvcdmo :+ [ MOD < > ].', comment)
    
    #     typedef = '''
    #     qpart-le := basic-scopal-adverb-lex &
    #       [ SYNSEM [ LOCAL.CAT [ HEAD.MOD < [ LOCAL.CAT [ HEAD verb,
    #                                                       VAL [ SUBJ < >,
    #                                                             COMPS < > ]]]>,
    #                              VAL [ SUBJ < >,
    #                                    SPR < >,
    #                                    COMPS < > ],
    #                              POSTHEAD ''' + qpartposthead + '''],
    #                  LKEYS.KEYREL.PRED question_m_rel ]].'''
    #     mylang.add(typedef)

  if qpartform:
    typedef = qpartform + ' := qpart-lex-item & [ STEM < "' + qpartform + '" > ].'
    lexicon.add(typedef)


######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.

def customize_lexicon():
  noun1 = ch('noun1')
  noun1pred = ch('noun1pred')
  noun1spr = ch('noun1spr')
  noun2 = ch('noun2')
  noun2pred = ch('noun2pred')
  noun2spr = ch('noun2spr')

  iverb = ch('iverb')
  ivpred = ch('ivpred')
  iverbSubj = ch('iverbSubj')
  tverbnf = ch('iverbnf')

  tverb = ch('tverb')
  tvpred = ch('tvpred')
  tverbSubj = ch('tverbSubj')
  tverbObj = ch('tverbObj')
  tverbnf = ch('tverbnf')

  subjAdp = ch('subjAdp')
  subjAdpForm = ch('subjAdpForm')
  objAdp = ch('objAdp')
  objAdpForm = ch('objAdpForm')

  det1 = ch('det1')
  det1pred = ch('det1pred')
  det2 = ch('det2')
  det2pred = ch('det2pred')

  auxverb = ch('auxverb')
  auxsem = ch('auxsem')
  auxcomp = ch('auxcomp')
  auxorder = ch('auxorder')

  neg = ch('neg')
  negadv = ch('negadv')
  negmod = ch('negmod')

  qpart = ch('qpart')

  # Do the noun entries have the same behavior wrt to overt determiners?
  singlentype = (noun1spr and noun2spr and noun1spr != noun2spr)

  # Do the verbs take the same category (NP or PP) for their subjects?
  singlevtype = (iverbSubj and tverbSubj and iverbSubj != tverbSubj)

  # Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?
  hclight = ((negadv == 'ind-adv' and negmod == 'V') or auxcomp == 'V')
    
  # Add the lexical types to mylang
  mylang.add_literal(';;; Lexical types')

  # Lexical types for nouns

  mylang.add_literal(';;; Nouns')
    
  # Playing fast and loose with the meaning of OPT on SPR.  Using
  # OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
  # weird), and leaving OPT unspecified for truly optional.  Hoping
  # this will work at least for LSA111 lab.  
  # Funny leading white space is to make the tdl look good.

  if singlentype:
    typedef = \
      'noun-lex := basic-noun-lex & basic-one-arg & \
         [ SYNSEM.LOCAL [ CAT.VAL [ SPR < #spr & \
                                          [ LOCAL.CAT.HEAD det'
    if noun1spr == 'obl':
      typedef += ', OPT - ] >, '
    elif noun1spr == 'nil':
      typedef += ', OPT + ] >, '
    else:
      typedef += ' ] >, '
    typedef += 'COMPS < >, SUBJ < >, SPEC < > ] ], ARG-ST < #spr > ].'
    mylang.add(typedef)
  else:
    typedef = \
      'noun-lex := basic-noun-lex & basic-one-arg & \
         [ SYNSEM.LOCAL [ CAT.VAL [ SPR < #spr & [ LOCAL.CAT.HEAD det ] >, \
                                    COMPS < >, \
                                    SUBJ < >, \
                                    SPEC < > ] ], \
           ARG-ST < #spr > ].'
    mylang.add(typedef)

    if noun1spr == 'obl' or noun2spr == 'obl':
      typedef = \
        'obl-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)

    if noun1spr == 'nil' or noun2spr == 'nil':
      typedef = \
        'no-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)
    
  # Lexical types for verbs
  # I'm adding the constraint to associate XARG with the
  # first ARG-ST element here (so raising auxiliaries work),
  # but perhaps this belongs in matrix.tdl?  Or maybe this
  # is another module/parameter (like, the external argument
  # might not be the first one?

  mylang.add_literal(';;; Verbs')
    
  typedef = \
    'verb-lex := basic-verb-lex & \
       [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >, \
                                    SPEC < >, \
                                    SUBJ < #subj > ]'
  if has_auxiliaries_p():
    typedef += ', HEAD.AUX - ],'
  else:
    typedef += '],'
  typedef += 'CONT.HOOK.XARG #xarg ], ARG-ST < #subj & ';
  if singlevtype:
    if iverbSubj == 'np':
      typedef += '[ LOCAL [ CAT [ HEAD noun, '
    else:
      typedef += '[ LOCAL [ CAT [ HEAD adp, '
    typedef += 'VAL [ SPR < >, COMPS < > ]], \
                CONT.HOOK.INDEX #xarg ]], ... > ].'
  else:
    typedef += \
      '[ LOCAL [ CAT.VAL [ SPR < >, COMPS < > ], \
       CONT.HOOK.INDEX #xarg ]], ... > ].'
  mylang.add(typedef)

  if hclight:
    comment = \
      ';;; If there are aspects of the syntax which pick out\n' + \
      ';;; lexical Vs (transitive or intransitive) such as V-attachment\n' + \
      ';;; of adverbs or argument composition auxiliaries which take V\n' + \
      ';;; complements, we need to distinguish (intranstive) V and VP.\n' + \
      ';;; To do so, we make use of a feature LIGHT.  Phrases are\n' + \
      ';;; generally [LIGHT -] with the exception of head-complement\n' + \
      ';;; phrases, which take their value for LIGHT from the head\'s\n' + \
      ';;; HC-LIGHT feature.  To make this work for us here, constraint\n' + \
      ';;; HC-LIGHT on verbs to be -.'
    mylang.add_literal(comment)
    mylang.add('verb-lex :+ [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')

  typedef = \
    'intransitive-verb-lex := verb-lex & intransitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < >'
  if singlevtype:
    typedef += '].'
  else:
    typedef += ','
    if iverbSubj == 'np':
      typedef += 'ARG-ST < [ LOCAL.CAT.HEAD noun ] > ].'
    else:
      typedef += 'ARG-ST < [ LOCAL.CAT.HEAD adp ] > ].'
  mylang.add(typedef)

  typedef = \
    'transitive-verb-lex := verb-lex & transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >,'
  if singlevtype:
    typedef += 'ARG-ST < [ ],'
  else:
    if tverbSubj == 'np':
      typedef += 'ARG-ST < [ LOCAL.CAT.HEAD noun ],'
    else:
      typedef += 'ARG-ST < [ LOCAL.CAT.HEAD adp ],'
  typedef += '#comps & [ LOCAL.CAT [ VAL [ SPR < >, COMPS < > ],'
  if tverbObj == 'np':
    typedef += 'HEAD noun ] ] > ].'
  else:
    typedef += 'HEAD adp ] ] > ].'
  mylang.add(typedef)

  # If there are auxiliaries, define finite and non-finite verb types,
  # cross-classify with trans and intrans.
  if has_auxiliaries_p():
    comment = \
      ';;; Types for finite and non-finite verbs.  These will\n' + \
      ';;; most likely need to be replaced with lexical rules\n' + \
      ';;; deriving the finite and non-finite forms from verb stems.'
    mylang.add_literal(comment)

    typedef = \
      'finite-verb-lex := verb-lex & \
         [ SYNSEM.LOCAL.CAT.HEAD.FORM fin ].'
    mylang.add(typedef)

    typedef = \
      'non-finite-verb-lex := verb-lex & \
         [ SYNSEM.LOCAL.CAT.HEAD.FORM inf ].'
    mylang.add(typedef)

    typedef = \
      'finite-trans-verb-lex := finite-verb-lex & transitive-verb-lex.'
    mylang.add(typedef)
    typedef = \
      'non-finite-trans-verb-lex := non-finite-verb-lex & transitive-verb-lex.'
    mylang.add(typedef)
    typedef = \
      'finite-intrans-verb-lex := finite-verb-lex & intransitive-verb-lex.'
    mylang.add(typedef)
    typedef = \
      'non-finite-intrans-verb-lex := \
         non-finite-verb-lex & intransitive-verb-lex.'
    mylang.add(typedef)

  # Lexical types for adpositions (if present):
  if subjAdp or objAdp:
    comment = \
      ';;; Case-marking adpositions\n' + \
      ';;; Case marking adpositions are constrained not to\n' + \
      ';;; be modifiers.'
    mylang.add_literal(comment)

    typedef = \
      'case-marker-p-lex := basic-one-arg & raise-sem-lex-item & \
          [ SYNSEM.LOCAL.CAT [ HEAD adp & [ MOD < > ], \
                               VAL [ SPR < >, \
                                     SUBJ < >, \
                                     COMPS < #comps >, \
                                     SPEC < > ]], \
            ARG-ST < #comps & [ LOCAL.CAT [ HEAD noun, \
                                             VAL.SPR < > ]] > ].'

    mylang.add(typedef)
	
  # Lexical type for determiners, if the language has any:
  if ch('hasDets') == 't':
    comment = \
      ';;; Determiners\n' + \
      ';;; SPEC is non-empty, and already specified by basic-determiner-lex.'
    mylang.add_literal(comment)

    typedef = \
      'determiner-lex := basic-determiner-lex & basic-zero-arg & \
          [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                   COMPS < >, \
                                   SUBJ < > ]].'
    mylang.add(typedef)

  # Lexical type for auxiliaries.  There's probably more we can give them
  # here, if we ask more questions (are there auxiliaries with both independent
  # preds and those which just contribute tense/aspect information?)...

  # ERB 2006-10-03 Removing if statement in the middle of a string...

  #                                      HEAD '
  #       if auxsubj == 'noun':
  #         typedef += 'noun ]],'
  #       else:
  #         typedef += 'adp ]],'
  #       typedef += \

  if has_auxiliaries_p():
    mylang.add_literal(';;; Auxiliaries')
    if auxcomp == 'VP':
      typedef = 'subj-raise-aux := trans-first-arg-raising-lex-item & \
                 [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ < \#subj >, \
                                         COMPS < \#comps >, \
                                         SPR < >, \
                                         SPEC < > ], \
                                   HEAD verb & [ AUX + ]], \
                ARG-ST < \#subj & \
                       [ LOCAL.CAT [ VAL  [ SPR < >, \
                                            COMPS < > ]]],\
                      \#comps & \
                       [ LOCAL.CAT [ VAL [ SUBJ < [ ] >, \
                                           COMPS < > ], \
                                     HEAD verb & \
                                          [ FORM inf ]]] > ].'
      mylang.add(typedef)

      if auxsubj == 'noun':
        mylang.add('subj-raise-aux := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].')
      else:
        mylang.add('subj-raise-aux := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].')
        

      
      if auxsem == 'pred':
        typedef = \
          'subj-raise-aux-with-pred := subj-raise-aux & \
                                       trans-first-arg-raising-lex-item-1.'
        mylang.add(typedef)
      else:
        comment = \
          '; To keep the semantically empty ones from spinning on\n' + \
          '; generation, require complement to be [AUX -].  The\n' + \
          '; FORM feature might be enough in the starter grammars,\n' + \
          '; but I don\'t want to rely on this.  Then again, [ AUX - ]\n' + \
          '; might not be true.  Be sure to put in a comment.'
        mylang.add_literal(comment)

        typedef = \
          'subj-raise-aux-no-sem := subj-raise-aux & \
                                    trans-first-arg-raising-lex-item-2 & \
              [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
        mylang.add(typedef)
    elif auxcomp == 'V': 
      comment = \
        '; Somewhat surprisingly, this inherits from basic-two-arg, so\n' + \
        '; that the non-local features are amalgamated from subj, the\n' + \
        '; lexical verb complement, but not the other complements, if any.'
      mylang.add_literal(comment)

      # ERB Removing if statement in the middle of a string.
      #                                       HEAD 
      #       if auxsubj == 'noun':
      #         typedef += 'noun ]],'
      #       else:
      #         typedef += 'adp ]],'
      #       typedef += \

      typedef = \
        'arg-comp-aux := basic-two-arg & \
           [ SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ], \
                                VAL [ SUBJ < #subj >, \
                                      COMPS < #comps . #vcomps >, \
                                      SPR < >, \
                                      SPEC < > ]], \
             ARG-ST < #subj & \
                      [ LOCAL [ CAT [ VAL [ SPR < >, \
                                            COMPS < > ], \
                                CONT.HOOK.INDEX #xarg ]]], \
                    #comps & \
                    [ LIGHT +, \
                      LOCAL [ CAT [ VAL [ SUBJ <[ ]>, \
                                          COMPS #vcomps ], \
                                    HEAD verb & [ FORM inf ]], \
                              CONT.HOOK.XARG #xarg ]] > ].'
      mylang.add(typedef)

      if auxsubj == 'noun':
        mylang.add('arg-comp-aux := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].')
      else:
        mylang.add('arg-comp-aux := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].')

      if auxsem == 'pred':
        comment = \
          '; Not inheriting from basic-verb-lex, so need to put in' + \
          '; event-relation by hand here.'
        mylang.add_literal(comment)

        typedef = \
          'arg-comp-aux-with-pred := arg-comp-aux & hcons-lex-item & \
             [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq & \
                                              [ HARG #harg, \
                                                LARG #larg ] !> ], \
                        LKEYS.KEYREL event-relation & \
                                     [ ARG1 #harg ]], \
               ARG-ST < [ ], \
                        [ LOCAL.CONT.HOOK [ XARG #xarg, \
                        LTOP #larg ]] > ].'
        mylang.add(typedef)
      else:
        comment = \
          '; Note that raise-sem-lex-item assumes the first complement is' + \
          '; where the HOOK comes from.  It\'s not clear to me how you\'d' + \
          '; tell that you had an argument composition auxiliary if it' + \
          '; wasn\'t appearing adjacent to the verb.'
        mylang.add_literal(comment)

        typedef = \
          'arg-comp-aux-no-sem := arg-comp-aux  & raise-sem-lex-item & \
             [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
        mylang.add(typedef)
    elif auxcomp == 'S':
      typedef = \
        's-comp-aux := basic-one-arg & \
           [ SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ], \
                                VAL [ SUBJ < >, \
                                      COMPS < #comps >, \
                                      SPR < >, \
                                      SPEC < > ]], \
             ARG-ST < #comps & \
                      [ LOCAL.CAT [ VAL [ SUBJ < >, \
                                          COMPS < > ], \
                                    HEAD verb & [ FORM inf ]]] > ].'
      mylang.add(typedef)

      if auxsem == 'pred':
        mylang.add_literal('; S comp aux, with pred')

        typedef = \
          's-comp-aux-with-pred := s-comp-aux & hcons-lex-item & \
              [ SYNSEM [ LOCAL.CONT.HCONS <! qeq & \
                                             [ HARG #harg, \
                                               LARG #larg ] !>, \
                         LKEYS.KEYREL event-relation & \
                                      [ ARG1 #harg ]], \
                ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] > ].'
        mylang.add(typedef)
      else:
        mylang.add_literal('; S comp aux, no sem')

        comment = \
          '; Better say [ AUX - ] on complement here, or we\'ll spin' + \
          '; on generation.'
        mylang.add_literal(comment)

        typedef = \
          's-comp-aux-no-sem := s-comp-aux & raise-sem-lex-item & \
             [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].'
        mylang.add(typedef)

  # Add the lexical entries
  lexicon.add_literal(';;; Nouns')

  for i in (1, 2):
    noun = ch('noun' + str(i))
    if noun:
      pred = ch('noun' + str(i) + 'pred')
      spr = ch('noun' + str(i) + 'spr')
      typedef = noun + ' := '
      if singlentype or spr == 'opt':
        typedef += 'noun-lex & '
      elif spr == 'obl':
        typedef += 'obl-spr-noun-lex & '
      else:
        typedef += 'no-spr-noun-lex & '
      typedef += '[ STEM < "' + noun + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

  lexicon.add_literal(';;; Verbs')

  # Do intransitve and transitive verbs in the same loop, since they
  # share a lot of TDL in common  - sfd
  for i in ('i', 't'):
    verb = ch(i + 'verb')
    verbpred = ch(i + 'vpred')
    verbnf = ch(i + 'verbnf')
    tivity = 'trans'
    if i == 'i':
      tivity = 'intrans'
    if verb:
      if has_auxiliaries_p():
        typedef = \
          verb + ' := finite-' + tivity + '-verb-lex & \
                      [ STEM < "' + verb + '" >, \
                          SYNSEM.LKEYS.KEYREL.PRED "' + verbpred + '" ].'
        lexicon.add(typedef)

        if verbnf:
          typedef = verbnf
        else:
          typedef = verb + "2"
        typedef += ' := non-finite-' + tivity + '-verb-lex & [ STEM < "'
        if verbnf:
           typedef += verbnf
        else:
          typedef += verb
        typedef += '" >, SYNSEM.LKEYS.KEYREL.PRED "' + verbpred + '" ].'
        lexicon.add(typedef)
      else: # not has_auxiliaries_p()
        typedef = \
          verb + ' := ' + tivity + 'itive-verb-lex & \
                      [ STEM < "' + verb + '" >, \
                        SYNSEM.LKEYS.KEYREL.PRED "' + verbpred + '" ].'
        lexicon.add(typedef)

  if has_auxiliaries_p():
    lexicon.add_literal(';;; Auxiliaries')
    typedef = \
      auxverb + ' := ' + auxtypename + ' & \
                     [ STEM < "' + auxverb + '" >, \
                       SYNSEM.LKEYS.KEYREL.PRED '
    if auxsem == 'pred':
      typedef += '"' + auxpred + '"'
    else:
      typedef += 'no-pred'
    typedef += ' ].'
    lexicon.add(typedef)

  lexicon.add_literal(';;; Other')

  # sfd ToDo? Some of these could be further parameterized.  Probably
  # not necessary unless they become iterators, though.

  # Case-marking adpositions
  if subjAdpForm or objAdpForm:
    lexicon.add_literal(';;; Case-marking adpositions')

  if subjAdpForm:
    typedef = \
      'subj-marker := case-marker-p-lex & \
                        [ STEM < "' + subjAdpForm + '" > ].'
    lexicon.add(typedef)

  if objAdpForm:
    typedef = \
      'obj-marker := case-marker-p-lex & \
                     [ STEM < "' + objAdpForm + '" > ].'
    lexicon.add(typedef)

  # Determiners
  if det1 or det2:
    lexicon.add_literal(';;; Determiners')

  if det1:
    typedef = \
      det1 + ' := determiner-lex & \
                  [ STEM < "' + det1 + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + det1pred + '" ].'
    lexicon.add(typedef)

  if det2:
    typedef = \
      det2 + ' := determiner-lex & \
                  [ STEM < "' + det2 + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + det2pred + '" ].'
    lexicon.add(typedef)

  # Negative adverb
  if negadv:
    typedef = \
      negadv + ' := neg-adv-lex & \
                    [ STEM < "' + negadv + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "_neg_r_rel" ].'
    lexicon.add(typedef)

  # Question particle
  if qpart:
    typedef = \
      qpart + ' := qpart-le & \
                   [ STEM < "' + qpart + '" > ].'
    lexicon.add(typedef)


######################################################################
# customize_test_sentences(matrix_path)
#   Create the script file entries for the user's test sentences.

def customize_test_sentences(matrix_path):
  try:
    b = open('matrix-core/basic_script', 'r')
    s = open(matrix_path + 'lkb/script', 'w')
    lines = b.readlines()
    b.close()
    for l in lines:
      l = l.strip()
      if l == ';;; Modules: LOAD my_language.tdl':
        myl = choices['language'].lower() + '.tdl'
        s.write('   (lkb-pathname (parent-directory) "' + myl + '")\n')
      elif l == ';;; Modules: Default sentences':
        s1 = ch('sentence1')
        s2 = ch('sentence2')
        s.write('(if (eq (length *last-parses*) 1)\n')
        s.write('   (setf *last-parses* \'("' + s1 + '" "' + s2 + '")))\n')
      else:
        s.write(l + '\n')
    s.close()
  except:
    pass


######################################################################
# customize_roots()
#   Create the file roots.tdl

def customize_roots():
  comment = \
    'A sample start symbol: Accept fully-saturated verbal\n' + \
    'projections only; if a grammar makes use of the head-subject and\n' + \
    'head-complement types as provided by the Matrix, this should be a\n' + \
    'good starting point.  Note that it is legal to have multiple start\n' + \
    'symbols, but they all need to be listed as the value of\n' + \
    '`*start-symbol\' (see `lkb/user-fns.lsp\').'
# ERB 2006-10-05 Removing if statement from within string

#  verb_addendum = ''
#  if has_auxiliaries_p():
#    verb_addendum = ' & [ FORM fin ]'
#[ HEAD verb' + verb_addendum + ', \

  typedef = \
    'root := phrase & \
       [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < >, \
                              COMPS < > ], \
                        COORD - ] ].'
  roots.add(typedef, comment)

  if has_auxiliaries_p():
    roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD.FORM fin ].')

  # ERB 2006-10-05 I predict a bug here:  If we a language with auxiliaries
  # and question particles, we're going to need to make sure that FORM is
  # compatible with comp.

  if ch('ques') == 'qpart':
    roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD +vc ].')
  else:
    roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD verb ].')

  comment = \
    'This start symbol allows you to parse single words as stand-alone\n' + \
    'utterances.  This can be useful for grammar debugging purposes.'
  typedef = \
    'lex-root := word-or-lexrule.'
  roots.add(typedef, comment)
  

######################################################################
# Archive helper functions
#   make_tgz(dir) and make_zip(dir) create an archive called
#   dir.(tar.gz|zip) that contains the contents of dir

def make_tgz(dir):
  archive = dir + '.tar'
  t = tarfile.open(archive, 'w')
  t.add(dir)
  t.close()

  if os.name == 'nt':
    g = gzip.open(archive + '.gz', 'wb')
    f = open(archive, 'rb')
    g.write(f.read())
    f.close()
    g.close()
  else:
    os.system('gzip ' + archive)


def add_zip_files(z, dir):
  files = os.listdir(dir)
  for f in files:
    cur = dir + '/' + f
    if os.path.isdir(cur):
      add_zip_files(z, cur)
    else:
      z.write(cur, cur)


def make_zip(dir):
  z = zipfile.ZipFile(dir + '.zip', 'w')
  add_zip_files(z, dir)
  z.close()


######################################################################
# customize_matrix(path)
#   Create and prepare for download a copy of the matrix based on
#   the choices file in the directory 'path'.  This function
#   assumes that validation of the choices has already occurred.

def customize_matrix(path, arch_type):
  global choices
  choices = {}
  
  choices_file = path + '/choices'
  choices = read_choices(choices_file)

  matrix_path = path + '/matrix/'

  # Copy from matrix-core
  if os.path.exists(matrix_path):
    shutil.rmtree(matrix_path)
  shutil.copytree('matrix-core', matrix_path)
  shutil.copy(choices_file, matrix_path) # include a copy of choices

  # Create TDL object for each output file
  global mylang, rules, irules, lrules, lexicon, roots
  mylang =  tdl.TDLfile(matrix_path + choices['language'].lower() + '.tdl')
  rules =   tdl.TDLfile(matrix_path + 'rules.tdl')
  irules =  tdl.TDLfile(matrix_path + 'irules.tdl')
  lrules =  tdl.TDLfile(matrix_path + 'lrules.tdl')
  lexicon = tdl.TDLfile(matrix_path + 'lexicon.tdl')
  roots =   tdl.TDLfile(matrix_path + 'roots.tdl')

  # ERB 2006-10-05 Customizing Version.lsp, too, to make it easier
  # to keep track of different grammars.  It's probably better to
  # use some other object type than tdl (since this is just lisp), but
  # doing it quick and dirty for now.  Also, we should store the
  # Matrix version info somewhere that this can pull it from.

  global version_lsp
  version_lsp = tdl.TDLfile(matrix_path + 'Version.lsp')

  version_lsp.add_literal('(in-package :common-lisp-user)\n\n')
  version_lsp.add_literal('(defparameter *grammar-version* \"' + choices['language'] + ' (Matrix-10-2006)\")\n')

  # Call the various customization functions
  customize_word_order()
  customize_sentential_negation()
  customize_coordination()
  customize_yesno_questions()
  customize_lexicon()
  customize_test_sentences(matrix_path)
  customize_roots()

  # Save the output files
  mylang.save()
  rules.save()
  irules.save()
  lrules.save()
  lexicon.save()
  roots.save()
  version_lsp.save()

  # Either tar or zip up the results
  old_dir = os.getcwd()
  os.chdir(path)
  if arch_type == 'tgz':
    make_tgz('matrix')
  else:
    make_zip('matrix')
  os.chdir(old_dir)

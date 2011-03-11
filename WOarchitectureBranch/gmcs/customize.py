### $Id: customize.py,v 1.71 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import os
import datetime
import shutil
import tdl
import tarfile
import gzip
import zipfile
import sys
import re
from subprocess import call

from gmcs.choices import ChoicesFile
from gmcs.utils import TDLencode
from gmcs.utils import get_name
from gmcs.utils import format_comment_block

from gmcs.lib import TDLHierarchy

from gmcs.linglib import morphotactics
from gmcs.linglib import argument_optionality
from gmcs.linglib import direct_inverse
from gmcs.linglib import case
from gmcs.linglib import word_order
from gmcs.linglib import auxiliaries

######################################################################
# globals

ch = {}

hierarchies = {}

mylang = None
rules = None
irules = None
lrules = None
lexicon = None
roots = None

# ERB 2006-09-16 There are properties which are derived from the
# choices file as a whole which various modules will want to know about.
# The first example I have is the presence of auxiliaries.  Both the
# negation and yes-no questions modules have cases where they need to
# restrict lexical rules to applying to main verbs only, but only if
# there is in fact a distinction between main and auxiliary verbs (i.e.,
# they need to say [ AUX - ], but only if the feature AUX is defined).

# ERB 2006-10-15 I want this function to return true if an auxiliary is
# defined, even if it's not needed for negation or questions.

def has_auxiliaries_p():

  return ch.get('has-aux') == 'yes'

# Returns the verb type for lexical/main verbs.
def main_or_verb():
  if has_auxiliaries_p():
    return 'main-verb-lex'
  else:
    return 'verb-lex'


def irule_name(type_name):
  return re.sub('\s+', '_', type_name)

# ERB 2006-09-21 This function assembles an inflectional rule out
# of the appropriate information and adds it to irules.tdl.
# Assumes we consistently use either 'prefix' and 'suffix' or 'before'
# and 'after' as values in the html form.
# Should this actually be a method on TDLfile?

def add_irule(instance_name,type_name,affix_type,affix_form):

  rule = irule_name(instance_name) + ' :=\n'
  if affix_type == 'prefix' or affix_type == 'before':
    rule += '%prefix (* ' + affix_form + ')\n'
  elif affix_type == 'suffix' or affix_type == 'after':
    rule += '%suffix (* ' + affix_form + ')\n'

# TODO: generate error here.
#  else:
#    error 'probable script bug'

  rule += irule_name(type_name) + '.\n'

  irules.add_literal(rule)


######################################################################
# customize_feature_values(ch_dict, type_name, pos, features, cases)
#   In the passed-in choices dictionary, go through the 'feat'
#   iterator and specify the feature/value pairs found to the
#   passed-in type.

def process_cfv_list(to_cfv, tdlfile=None):
  for (ch_key, type_id, pos) in to_cfv:
    customize_feature_values(ch[ch_key], type_id, pos,
                             tdlfile=tdlfile or mylang)

def customize_feature_values(ch_dict, type_name, pos, features=None, cases=None, tdlfile=None):

  if not features:
    features = ch.features()
  if not cases:
    cases = case.case_names(ch)
  if not tdlfile:
    tdlfile = mylang

  pos_geom_prefix = ''

  if pos == 'det':
    pos_geom_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.'
  elif pos == 'con':
    pos_geom_prefix = 'HEAD-DTR.SYNSEM.'
  else:
    pos_geom_prefix = 'SYNSEM.'

  if pos == 'auxcomplement':
    iter_feat = 'compfeature'
  else:
    iter_feat = 'feat'

  for feat in ch_dict.get(iter_feat,[]):
    n = feat.get('name','')
    v = feat.get('value','').split(', ')

    if n == 'case':
      v = [case.canon_to_abbr(c, cases) for c in v]

    geom_prefix = pos_geom_prefix

    # The 'head' choice only appears on verb pcs, and allows the user
    # to specify features on the subject and object as well
    h = feat.get('head','')
    if h == 'subj':
      geom_prefix += 'LOCAL.CAT.VAL.SUBJ.FIRST.'
    elif h == 'obj':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'
    elif h == 'higher':
      geom_prefix = 'SC-ARGS.FIRST.'
    elif h == 'lower':
      geom_prefix = 'SC-ARGS.REST.FIRST.'

    if pos == 'auxcomplement':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'

    geom = ''
    for f in features:
      if f[0] == n:
        geom = f[2]

    if geom:
      geom = geom_prefix + geom

    # If the feature has a geometry, just specify its value;
    # otherwise, handle it specially.
    if geom:
      if n in hierarchies:
        value = hierarchies[n].get_type_covering(v)
        tdlfile.add(type_name +
                    ' := [ ' + geom + ' ' + value + ' ].',
                    merge=True)
        if n == 'case' and ch.has_mixed_case():
          val = '-' if '-synth-' + value in type_name else '+'
          tdlfile.add(type_name +
                      ' := [ ' + geom + '-MARKED ' + val + ' ].',
                      merge=True)
      else:
        for value in v:
          tdlfile.add(type_name +
                      ' := [ ' + geom + ' ' + value + ' ].',
                      merge=True)
    elif n == 'argument structure':
      # constrain the ARG-ST to be passed up
      tdlfile.add(type_name + ' := [ ARG-ST #arg-st, DTR.ARG-ST #arg-st ].',
                  merge=True)

      # get the feature geometry of CASE
      for f in features:
        if f[0] == 'case':
          geom = f[2]

      for argst in v:
        # specify the subj/comps CASE values
        c = argst.split('-')
        if case.interpret_verb_valence(argst) == 'tverb':
          # if no case marking specified AVMS are blank
          a_case = o_case = ''
          # otherwise use the geometry and case name
          if len(c) > 1:
            a_case = geom + ' ' + case.canon_to_abbr(c[0], cases)
            o_case = geom + ' ' + case.canon_to_abbr(c[1], cases)
          tdlfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + a_case + '], ' +\
                                     '[ ' + o_case + '] > ].',
                      merge=True)
        else:
          c = c[0]
          s_case = '' if c == 'intrans' \
                      else (geom + ' ' + case.canon_to_abbr(c, cases))
          tdlfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + s_case + '] > ].',
                      merge=True)

    elif (n == 'negation' and v[0] == 'plus'):
      # ERB 2009-01-22 This is where we deal with the
      # negative affixes.
      tdlfile.add(type_name + ':= \
                     [ C-CONT [ HOOK [ XARG #xarg,\
	                     LTOP #ltop,\
	                     INDEX #ind ],\
	              RELS <! event-relation &\
	                      [ PRED "_neg_r_rel",\
	                        LBL #ltop,\
	                        ARG1 #harg ] !>,\
	              HCONS <! qeq &\
	                       [ HARG #harg,\
	                         LARG #larg ] !> ],\
	              SYNSEM.LKEYS #lkeys,\
	            DTR [ SYNSEM [ LKEYS #lkeys,\
	                    LOCAL [ CONT.HOOK [ XARG #xarg,\
                                                INDEX #ind,\
	                                        LTOP #larg ],\
	                          CAT.HEAD verb]]]].',
                 'This lexical rule adds the neg_r_rel to the verb\'s\n\
	          RELS list.  It is instantiated by a spelling-changing\n\
	          rule as specified in irules.tdl.',
                  merge=True)


    elif (n == 'question' and v[0] == 'plus'):
      # ERB 2009-07-01 Adding in semantics for question affixes
      tdlfile.add(type_name + ':= \
                     [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques ].',
                  merge=True)

    ## Specifiying OPT- on each user defined type instead of creating a supertype because
    #It the supertype would need to inherit from transitive-verb-lex and the code already puts
    #transitive-verb-lex as a supertype of user-defined typ thus causing an inheritance issue.
    #elif(n=='OPT' and v[0] == 'plus'):
      # SS 2009-05-26 argument optionality is added to user defined types here
      #if h == 'subj':
      #  tdlfile.add(type_name + ':= subj-drop-verb-lex.', merge = True)
      #if h == 'obj':
      #  tdlfile.add(type_name + ':= obj-drop-verb-lex.', merge = True)

    elif n == 'OPT':
      bool_val = {'plus': '+', 'minus': '-'}[v[0].lower()]
      val_geom = {'subj': 'SUBJ', 'obj': 'COMPS'}[h.lower()]
      tdlfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.OPT %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)

    elif n == 'direction':
      if v[0] == 'dir':
        tdlfile.add(type_name + ' := dir-lex-rule.')
      else:
        tdlfile.add(type_name + ' := inv-lex-rule.')
    elif n == 'dirinv-type':
      d = v[0]
      if h == 'subj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < '+d+' > ].')
      elif h == 'obj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.COMPS < '+d+' > ].')

# Note: customize case code is now in gmcs/linglib/case.py

# Note: direct inverse code is now in gmcs/linglib/direct_inverse.py

######################################################################
# customize_person_and_number()
#   Create the type definitions associated with the user's choices
#   about person and number.

def init_person_hierarchy():
  hier = TDLHierarchy('person')

  for p in ch.persons():
    for st in p[1].split(';'):
      hier.add(p[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def init_number_hierarchy():
  hier = TDLHierarchy('number')

  for n in ch.numbers():
    for st in n[1].split(';'):
      hier.add(n[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def init_pernum_hierarchy():
  hier = TDLHierarchy('pernum')

  for pn in ch.pernums():
    for st in pn[1].split(';'):
      hier.add(pn[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_person_and_number():
  if 'pernum' in hierarchies:
    mylang.add('png :+ [ PERNUM pernum ].', section='addenda')
    hierarchies['pernum'].save(mylang)
  else:
    if 'person' in hierarchies:
      mylang.add('png :+ [ PER person ].', section='addenda')
      hierarchies['person'].save(mylang)

    if 'number' in hierarchies:
      mylang.add('png :+ [ NUM number ].', section='addenda')
      hierarchies['number'].save(mylang)


######################################################################
# customize_gender()
#   Create the type definitions associated with the user's choices
#   about gender.

def init_gender_hierarchy():
  hier = TDLHierarchy('gender')

  for g in ch.genders():
    for st in g[1].split(';'):
      hier.add(g[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_gender():
  if 'gender' in hierarchies:
    mylang.add('png :+ [ GEND gender ].', section='addenda')
    hierarchies['gender'].save(mylang)


######################################################################
# customize_other_features()
#   Create the type definitions associated with the user's choices
#   about other features.

def init_other_hierarchies():
  for feature in ch.get('feature',[]):
    feat = feature.get('name','')
    type = feature.get('type','')
    hier = TDLHierarchy(feat, type)

    for value in feature.get('value', []):
      val = value.get('name')
      for supertype in value.get('supertype', []):
        stype = supertype.get('name')
        hier.add(val, stype)

    if not hier.is_empty():
      hierarchies[hier.name] = hier


def customize_other_features():
  for name in hierarchies:
    h = hierarchies[name]
    feat = h.name
    type = h.type
    # if this hierarchy isn't handled elsewhere, handle it here
    if feat not in ['case', 'person', 'number', 'pernum', 'gender',
                    'form', 'tense', 'aspect', 'situation', 'mood']:
      if type == 'head':
        mylang.add('head :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   section='addenda')
      else:
        mylang.add('png :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   section='addenda')

      # sfd: If it's an 'index' feature, we should make sure to strip it
      # out in the VPM

      h.save(mylang)


######################################################################
# customize_tense()
# Create tense feature value hierarchies per the user's choices

def init_tense_hierarchy():
  hier = TDLHierarchy('tense')

  tdefn = ch.get('tense-definition')
  if tdefn:
    if tdefn == 'choose':
      ppflist = []
      for ten in ('nonfuture', 'nonpast', 'past', 'present', 'future' ):

        if ten in ch:
          if ten not in ppflist:
            hier.add(ten, 'tense')

          for subtype in ch.get(ten + '-subtype',[]):
            st = subtype.get('name','')
            hier.add(st, ten)

          if ten == 'nonfuture':
            for moreten in ('past', 'present'):
              if moreten in ch:
                hier.add(moreten, ten)
                ppflist.append(moreten)

          if ten == 'nonpast':
            for moreten in ('present', 'future'):
              if moreten in ch:
                hier.add(moreten, ten)
                ppflist.append(moreten)

    elif tdefn == 'build':

      for tense in ch.get('tense',[]):
        name = tense.get('name')

        for supertype in tense.get('supertype',[]):
          supername = supertype.get('name')
          hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_tense():
  if 'tense' in hierarchies:
    hierarchies['tense'].save(mylang, False)


######################################################################
# customize_aspect()
# Create viewpoint aspect feature value definitions per the user's choices

def init_aspect_hierarchy():
  hier = TDLHierarchy('aspect')

  for aspect in ch.get('aspect',[]):
    name = aspect.get('name')
    for supertype in aspect.get('supertype', []):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier
  elif ch.get('perimper'):
    for asp in ('perfective', 'imperfective'):
      name = asp
      supername = 'aspect'
      hier.add(name, supername)
      hierarchies[hier.name] = hier

def customize_aspect():
  if 'aspect' in hierarchies:
    hierarchies['aspect'].save(mylang, False)

# customize_situation()
# Create situation aspect feature value definitions per the user's choices

def init_situation_hierarchy():
  hier = TDLHierarchy('situation')

  for situation in ch.get('situation',[]):
    name = situation.get('name')
    for supertype in situation.get('supertype',[]):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier

def customize_situation():
  if 'situation' in hierarchies:
    mylang.set_section('features')
    mylang.add('situation := sort.')
    mylang.add('tam :+ [SITUATION situation].', section='addenda')
    hierarchies['situation'].save(mylang, False)

######################################################################
# customize_mood()
# Create mood feature value definitions per the user's choices

def init_mood_hierarchy():
  hier = TDLHierarchy('mood')

  for mood in ch.get('mood',[]):
    name = mood.get('name')
    for supertype in mood.get('supertype',[]):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier
  elif ch.get('subjind'):
    for md in ('subjunctive', 'indicative'):
      name = md
      supername = 'mood'
      hier.add(name, supername)
      hierarchies[hier.name] = hier

def customize_mood():
  if 'mood' in hierarchies:
    hierarchies['mood'].save(mylang, False)


# find out whether verbal clusters

def determine_vcluster(auxcomp, auxorder, wo):

  vcluster = False

  if auxcomp == 'vp':
    if (wo == 'v-initial' and auxorder == 'before') or (wo == 'v-final' and auxorder == 'after'):
      vcluster = True
  elif auxcomp == 'v':
    if wo == 'v-initial' or wo == 'v-final' or wo == 'osv' or wo == 'vso':
      vcluster = True
    if wo == 'sov' or wo == 'ovs':
      if auxorder == 'before':
        vcluster = True
      elif wo == 'sov' or wo == 'ovs': 
        vcluster = False
    if wo == 'vos' or wo == 'svo':
      if auxorder == 'after':
        vcluster = True
      elif wo == 'vos' or wo == 'svo':
        vcluster = False
    if wo == 'free' and ch.get('multiple-aux') == 'yes':
      vcluster = True
  if not has_auxiliaries_p():
    vcluster = False
  return vcluster




######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

def customize_sentential_negation():

  # ERB 2006-09-16 Calculate a bunch of derived properties based on the
  # inputs they gave for negation.  The same thing (e.g., negation via
  # inflection on the main verb) gives a very different output depending
  # on whether there are other options (negation via selected adverb)
  # and how they combine.

  # ERB 2009-01-23 This is all moot right now since the interim system
  # doesn't do the interaction between the two, but it probably won't
  # break anything to leave it in.

  # ERB 2009-07-01 It was adding defunct lex rules in at least some
  # cases, so taking it out for now.  This much still seems to be
  # required:

  advAlone = ''
  multineg = ch.get('multi-neg')
  if ch.get('adv-neg') == 'on' or multineg == 'comp':
    advAlone = 'always'

  # ERB 2009-01-23 Migrating negation to modern customization system.
  # This intermediate version only does independent adverbs, and so
  # I'm removing ch.get('neg-adv') == 'ind-adv' as a second part of
  # the test below.

  if ch.get('adv-neg') == 'on': # and ch.get('neg-adv') == 'ind-adv':
    create_neg_adv_lex_item(advAlone)


def create_neg_adv_lex_item(advAlone):

  mylang.set_section('otherlex')

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
    mylang.add_comment('neg-adv-lex',
    '''Constrain the MOD value of this adverb to keep\n
    it from modifying the kind of verbs which can select it,\n
    To keep spurious parses down, as a starting point, we have\n
    assumed that it only modifies verbs (e.g., non-finite verbs).''')

  if ch.get('neg-order') == 'before':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
  elif ch.get('neg-order') == 'after':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

  if ch.get('neg-mod') == 's':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
                                                                                   COMPS null ]].''')
  elif ch.get('neg-mod') == 'vp':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
                                                                                   COMPS null ]].''')
  elif ch.get('neg-mod') == 'v':
    mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
    lexical Vs for V-level attachment of negative adverbs.''')

  # ERB 2006-09-22 Validation should really make sure we have a value of
  # neg-adv-orth before we get here, but just in case, checking first, since
  # the script gets really unhappy if I try to write to an empty type.

  if(ch.get('neg-adv-orth')):
    orth = ch.get('neg-adv-orth')
    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orth +'\" >,\
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
              'appear in ' + ch.get('language').lower() + '.tdl')
    rules.add('adj-head-int := adj-head-int-phrase.')
    rules.add('head-adj-scop := head-adj-scop-phrase.')
    rules.add('adj-head-scop := adj-head-scop-phrase.')

    mylang.add('+nvcdmo :+ [ MOD < > ].',
               'This grammar includes head-modifier rules.  To keep\n' +
               'out extraneous parses, constrain the value of MOD on\n' +
               'various subtypes of head.  This may need to be loosened later.\n' +
               'This constraint says that only adverbs, adjectives,\n' +
               'and adpositions can be modifiers.',
               section='addenda')

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf):
  mylang.add_literal(';;; Coordination Strategy ' + num)

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
           [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].'
    mylang.add(rule)
    if bot == 'unary-':
      rule = pn + '-bottom-coord-rule :=\
             [ SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].'
      mylang.add(rule)

  if left:
    # first the rule in mylang
    rule = pn + '-left-coord-rule :=\
           ' + bot + 'left-coord-rule &\
           ' + pos + '-bottom-coord-phrase &\
           [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].'
    mylang.add(rule)

    if pre or suf:
      # constrain the predicate
      mylang.add(pn + '-left-coord-rule :=\
                 [ SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].')

      # now the spelling change rule in irules.tdl
      rule = pn + '-left :=\n'
      if pre:
        rule += '  %prefix (* ' + pre + ')\n'
      else:
        rule += '  %suffix (* ' + suf + ')\n'
      rule += '  ' + pn + '-left-coord-rule.'
      irules.add_literal(rule)

  # Now define the rule instances into rules.tdl.  As above, the mid
  # or left rule may not be necessary.

  rules.add(pn + '-top-coord := ' + pn + '-top-coord-rule.')
  if mid:
    rules.add(pn + '-mid-coord := ' + pn + '-mid-coord-rule.')
  rules.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  if left:
    rules.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')


def customize_coordination():
  """
  The main coordination customization routine
  """
  mylang.set_section('coord')

  for cs in ch.get('cs'):
    csnum = str(cs.iter_num())

    mark = cs.get('mark')
    pat = cs.get('pat')
    orth = cs.get('orth')
    order = cs.get('order')

    pre = ''
    suf = ''

    if mark == 'word':
      lexicon.add(TDLencode(orth) + ' := conj-lex &\
                  [ STEM < "' + orth + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                    CFORM "' + csnum + '" ].')
      if pat == 'omni':
        lexicon.add(TDLencode(orth) + '_nosem := nosem-conj-lex &\
                      [ STEM < "' + orth + '" >,\
                        CFORM "' + csnum + '" ].')

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
      if cs.get(pos):
        define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf)


######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.

def customize_yesno_questions():

  qinvverb = ch.get('q-inv-verb')
  qpartposthead = ch.get('q-part-order')
  qpartform = ch.get('q-part-orth')

  if ch.get('q-inv'):
    comment = \
      'For the analysis of inverted yes-no questions, we add the feature INV.'
    mylang.add('verb :+ [ INV bool ].', comment, section='addenda')

    comment = \
      'All verbs start off as not inverted.'
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.INV - ].',
               comment, section='verblex')


    comment = \
      'Rule for inverted subject verb order in questions.\n' + \
      'The incompatible SUBJ values on SYNSEM and DTR are\n' + \
      'what keeps this one from spinning.'

      # ERB 2006-10-05 Adding in semantics here.  This rule constrains MESG to ques.
      # ERB 2007-01-21 Removing semantics here: Need to allow inversion to not express questions.  Instead, the result of this is MC na, and there is a separate non-branching rule which introduces question semantics.  Following the ERG in this.
      # ERB 2010-04-15 Adding [AUX +] on DTR, too.
    typedef = '''
    subj-v-inv-lrule := cat-change-only-lex-rule &
			same-hc-light-lex-rule &
			same-posthead-lex-rule &
                        constant-lex-rule &
      [ SYNSEM [ LOCAL.CAT [ HEAD verb & [ INV + ],
                             VAL [ COMPS < #subj . #comps >,
                                     SUBJ < >,
                                     SPR #spr,
                                     SPEC #spec ],
                             MC na ],
                 LKEYS #lkeys ],
        DTR.SYNSEM [ LOCAL.CAT [ HEAD verb,
                                 VAL [ SUBJ < #subj >,
                                       COMPS #comps,
                                       SPR #spr,
                                       SPEC #spec ]],
                     LKEYS #lkeys ]].'''
    mylang.add(typedef, comment, section='lexrules')

    lrules.add('inv-lr := subj-v-inv-lrule.')

    # ERB 2010-04-15 Cleaning up treatent of constraints on AUX, 
    # which were still very old-school. Need to both constrain DTR
    # and copy the value up.  Only checking qinvverb if we know
    # we have auxiliaries.

    if has_auxiliaries_p():
      mylang.add('''
                 subj-v-inv-lrule :=
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM #form,
                      DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].''')

      if qinvverb == 'aux':
        mylang.add('subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

      if qinvverb == 'main':
        mylang.add('subj-v-inv-lrule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].')



    # ERB 2010-04-15 If object drop is enabled (i.e., if the 
    # head-opt-comp rule is instantiated) then we need to prevent
    # the inverted subject from being dropped.  This is true even if
    # subject drop is generally allowed, since subj-verb inversion
    # is not apparent if the subject is dropped.  Assuming for now
    # that this rule would not be used to model inflection that requires
    # subj-v inversion but allows subject drop.

    if ch.get('obj-drop'):
      mylang.add('subj-v-inv-lrule := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT - ].')


    # ERB 2010-04-15 If we have a finite/non-finite disctintion,
    # the FORM value needs to be copied up.  FIXME: More generally,
    # any verby head features should be copied by this rule.
    # See notes on FORM and qpart below.

    if 'form' in hierarchies:
      mylang.add('''
                 subj-v-inv-lrule :=
                    [ SYNSEM.LOCAL.CAT.HEAD.FORM #form,
                      DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].''')



    # ERB 2007-01-21 Then we need the non-branching construction which
    # corrects to MC + and adds SF ques.

    comment = \
           'This rule takes [MC na] inverted phrases and licenses' + \
           'them as main clauses with question semantics.\n'

    typedef = '''
    int-cl := interrogative-clause & head-only &
    [ SYNSEM.LOCAL.CAT [ HEAD.INV +,
                         VAL #val,
                         MC + ],
      HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na,
                                  VAL #val &
                                       [SUBJ < >,
                                       COMPS < >]],
      C-CONT.HOOK.INDEX.SF ques ].'''
    mylang.add(typedef, comment, section='phrases')

    rules.add('int := int-cl.')

  # ERB 2006-10-05 Moving away from the modifier analysis of question particles
  # which I think doesn't handle the facts well.  These look more like complementizers
  # to me.

  if ch.get('q-part'):
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
           ARG-ST < #comp & [ LOCAL.CAT [ MC +,
                                          HEAD verb,
                                          VAL [ SUBJ < >,
                                                COMPS < > ]]] > ]
                                                .'''
    mylang.add(typedef, comment, section='otherlex')

    comment = 'Subtype for question particles. Constrains SF to ques.'
    typedef = '''
      qpart-lex-item := complementizer-lex-item &
         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques ].'''
    mylang.add(typedef, comment, section='otherlex')

    # ERB 2010-04-15 If we have a finite/non-finite distinction in the
    # language, the qpart should insist on attaching to finite clauses
    # only.  An alternative would be to have it raise the FORM value, but
    # I don't see any evidence for that just now. Using presence of 'form'
    # in hierarchies to detect this situation. This could break if someone
    # named their own feature "form", but the name-space validation should
    # catch that.  This works because customize_form() is called before
    # customize_yesno_questions. This is an example of cross-library
    # interaction.

    if 'form' in hierarchies:
      mylang.add('qpart-lex-item := [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM finite ].')


# ERB 2009-07-01 To remove:
#   if ch.get('q-infl'):

#     mylang.add('ques-infl-lex-rule := add-only-no-ccont-rule & inflecting-lex-rule &\
#     [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques,\
#     DTR lex-item & [ SYNSEM.LOCAL.CAT.HEAD verb ]].',
#                'Constrains SF to ques. Instantiated by a verbal affix.')

#     if ch.get('q-infl-type') == 'aux':
#       mylang.add('ques-infl-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
#                  'This rule applies only to auxiliaries.')

#     if ch.get('q-infl-type') == 'main' and has_auxiliaries_p():
#       mylang.add('ques-infl-lex-rule := [ DTR.SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
#                  'This rule applies only to main verbs.')


#     add_irule('ques-infl-lr','ques-infl-lex-rule',ch.get('ques-aff'),ch.get('ques-aff-orth'))


######################################################################
# customize_arg_op()
#   Create phrase-structure and lexical rules associated with user's
#   choices on argument optionality page.

def customize_arg_op():
  """ Create the lexical types, lexical, rules and phrase structure
      rules to allow argument dropping"""

  if 'scale' in ch and (ch.get('subj-drop')or ch.get('obj-drop')):
     mylang.add('dir-inv-scale := unexpressed-reg')

  mylang.set_section('verblex')
  ##Adding potential fix for integrating argument optionality and direct-inverse
  
  #Figure out the constraints on subject dropping and write the
  #appropriate types to mylang.tdl or rules.tdl

  if ch.get('subj-drop') == 'subj-drop-all' and not (ch.get('subj-con') == 'subj-con-some'):
    rules.add('decl-head-opt-subj := decl-head-opt-subj-phrase.')
  if ch.get('subj-drop') == 'subj-drop-lex' and not (ch.get('subj-con') == 'subj-con-some'):
    rules.add('decl-head-opt-subj := decl-head-opt-subj-phrase.')
    mylang.add('no-subj-drop-verb-lex := verb-lex &\
                         [SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].')
    mylang.add('subj-drop-verb-lex := verb-lex.')


  #Figure out the constraints on object dropping and write the
  #appropriate types to mylang.tdl or rules.tdl
  if ch.get('obj-drop')=='obj-drop-all':
    rules.add('basic-head-opt-comp := basic-head-opt-comp-phrase.')

  if ch.get('obj-drop') == 'obj-drop-lex':
    rules.add('basic-head-opt-comp := basic-head-opt-comp-phrase.')
    mylang.add('no-obj-drop-verb-lex := transitive-verb-lex &\
                        [SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].')
    mylang.add('obj-drop-verb-lex := transitive-verb-lex.')

  if ch.get('subj-drop') == 'subj-drop-lex' and ch.get('obj-drop') == 'obj-drop-lex':
    mylang.add('subj-drop-only-verb-lex := subj-drop-verb-lex & no-obj-drop-verb-lex.')
    mylang.add('obj-drop-only-verb-lex := obj-drop-verb-lex & no-subj-drop-verb-lex.')
    mylang.add('subj-obj-drop-verb-lex := subj-drop-verb-lex & obj-drop-verb-lex.')
    mylang.add('no-drop-verb-lex := no-subj-drop-verb-lex & no-obj-drop-verb-lex.')

  mylang.set_section('phrases')

  #Create phrase-structure rules for each context
  for context in ch.get('context'):
    name = 'context' + str(context.iter_num())
    ptype = name + '-decl-head-opt-subj-phrase'
    customize_feature_values(context, ptype, 'con')
    mylang.add(ptype + ':= decl-head-opt-subj-phrase.')
    rules.add(name + '-decl-head-opt-subj := '+ name + '-decl-head-opt-subj-phrase.')

  #Trying to get co-occurrence of marker dropping to work

  if (ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and (ch.get('subj-mark-drop')== 'subj-mark-drop-opt'or ch.get('subj-mark-drop')=='subj-mark-drop-req')):
    mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

  if ((ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and ch.get('obj-mark-drop') == 'obj-mark-drop-req') or ((ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and ch.get('obj-mark-drop') == 'obj-mark-drop-req'))):
    mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and ch.get('obj-mark-drop') == 'obj-mark-drop-opt' :
    mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-req' and ch.get('obj-mark-drop') == 'obj-mark-drop-not' :
    mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and ch.get('obj-mark-drop') == 'obj-mark-drop-not' :
    mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('obj-mark-drop')== 'obj-mark-drop-opt' and ch.get('obj-mark-no-drop') == 'obj-mark-no-drop-req':
    mylang.add( 'basic-head-comp-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('subj-mark-drop')== 'subj-mark-drop-opt' and ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-req':
    mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and ch.get('subj-mark-drop') == 'subj-mark-drop-opt' :
    mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-req' and ch.get('subj-mark-drop') == 'subj-mark-drop-not' :
    mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

  if ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and ch.get('subj-mark-drop') == 'subj-mark-drop-not' :
    mylang.add( 'basic-head-subj-phrase :+ [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT -].', merge = True, section='addenda')

#def customize_subj_phrase(phrase)
  #Trying to get the subject/object marker co-occurrence to work out
  #if (ch.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and (ch.get('subj-mark-drop')== 'subj-mark-drop-opt'or ch.get('subj-mark-drop')=='subj-mark-drop-req')):
   # mylang.add(phrase + ':= [HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.OPT +].', merge = True)

######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.

def customize_nouns():
  # Figure out which kinds of determiner-marking are in the language
  seen = {'obl':False, 'opt':False, 'imp':False}
  seenCount = 0

  for noun in ch.get('noun',[]):
    det = noun.get('det')
    if not seen[det]:
      seen[det] = True
      seenCount += 1

  singlentype = (seenCount == 1)

  # Playing fast and loose with the meaning of OPT on SPR.  Using
  # OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
  # weird), and leaving OPT unspecified for truly optional.  Hoping
  # this will work at least for LSA111 lab.

  # ERB 2006-11-28 Update: To make that weird use of OPT work, the
  # head-spec rule has to require [OPT -] on its non-head daughter.
  # Adding that just in case we add the no-spr-noun-lex type.

  typedef = \
    'noun-lex := basic-noun-lex & basic-one-arg & no-hcons-lex-item & \
       [ SYNSEM.LOCAL [ CAT.VAL [ SPR < #spr & [ LOCAL.CAT.HEAD det ] >, \
                                  COMPS < >, \
                                  SUBJ < >, \
                                  SPEC < > ] ], \
         ARG-ST < #spr > ].'
  mylang.add(typedef)

  if singlentype:
    if seen['obl']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)
    elif seen['imp']:
      typedef = 'noun-lex := [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)
  else:
    if seen['obl']:
      typedef = \
        'obl-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].'
      mylang.add(typedef)

    if seen['imp']:
      typedef = \
        'no-spr-noun-lex := noun-lex & \
           [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].'
      mylang.add(typedef)

  if seen['imp'] and ch.get('has-dets') == 'yes':
    mylang.add(
      'head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.OPT - ].',
      'Nouns which cannot take specifiers mark their SPR requirement\n' +
      'as OPT +.  Making the non-head daughter OPT - in this rule\n' +
      'keeps such nouns out.')

  if ch.get('case-marking') != 'none':
    if not ch.has_adp_case():
      mylang.add('noun :+ [ CASE case ].', section='addenda')

  # Add the lexical entries
  lexicon.add_literal(';;; Nouns')

  for noun in ch.get('noun',[]):
    name = get_name(noun)
    det = noun.get('det')

    if singlentype or det == 'opt':
      stype = 'noun-lex'
    elif det == 'obl':
      stype = 'obl-spr-noun-lex'
    else:
      stype = 'no-spr-noun-lex'

    ntype = name + '-noun-lex'

    mylang.add(ntype + ' := ' + stype + '.')

    customize_feature_values(noun, ntype, 'noun')

    for stem in noun.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = TDLencode(orth) + ' := ' + ntype + ' & \
                  [ STEM < "' + orth + '" >, \
                    SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)


def customize_verb_case():
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

    if not rule_pattern:
      c = p[0].split('-')  # split 'agentcase-patientcase'
      if p[0] == 'trans' or len(c) > 1:  # transitive
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


###############################################################
# customize_form()

def init_form_hierarchy():
  """
  Create the FORM hierarchies associated with the user's choices
  about verb forms
  Adds FORM finite and nonfinte values if there are auxiliaries
  or if user specified
  """
  hier = TDLHierarchy('form')

  if has_auxiliaries_p() or 'noaux-fin-nf' in ch:

    hier.add('nonfinite', 'form')
    hier.add('finite', 'form')

    for p in ('nf', 'fin'):

      for subform in ch.get(p + '-subform',[]):
        if p == 'nf':
          sup = 'nonfinite'
        elif p == 'fin':
          sup = 'finite'

        sub = subform.get('name')
        hier.add(sub, sup)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_form():
  if 'form' in hierarchies:
    mylang.add('head :+ [FORM form].', section='addenda')
    hierarchies['form'].save(mylang)


##########################################################
# customize_verbs()

def customize_verbs():
  negmod = ch.get('neg-mod')
  negadv = ch.get('neg-adv')
  wo = ch.get('word-order')
  auxcomp = ch.get('aux-comp')
  auxorder = ch.get('aux-comp-order')
  # Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?
  hclight = (negadv == 'ind-adv' and negmod == 'v')
  hclightallverbs = False

  if has_auxiliaries_p():
    vc = determine_vcluster(auxcomp, auxorder, wo)
    if wo == 'vso' or wo == 'osv':
      wo = 'req-hcl-vp'
    if auxcomp == 'v' and hclight != True:
      hclight = True
      if wo != 'free' or vc == True:
        hclightallverbs = True
    if auxcomp == 'vp' and wo == 'req-hcl-vp':
      hclightallverbs = True
  else:
    vc = False

  if wo == 'req-hcl-vp':
    wo = ch.get('word-order')

  # Lexical types for verbs
  # I'm adding the constraint to associate XARG with the
  # first ARG-ST element here (so raising auxiliaries work),
  # but perhaps this belongs in matrix.tdl?  Or maybe this
  # is another module/parameter (like, the external argument
  # might not be the first one?

  mainorverbtype = main_or_verb()
# The variable mainorverbtype is a type name for lexical/main (non-aux) verbs.
# Note that the use of 'main' instead of 'lexical' is strictly for
# coding clarity
# If there are auxiliaries, non-aux verbs are 'main-verb-lex', and 'verb-lex'
# includes both aux and lexical/main verbs.
# If there are no auxiliaries then 'verb-lex' covers all verbs

  if has_auxiliaries_p():
    mylang.add('head :+ [ AUX bool ].', section='addenda')
    #mainorverbtype = 'main-verb-lex'

# we need to know whether the auxiliaries form a vcluster

    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')
    vcluster = determine_vcluster(auxcomp, auxorder, wo)

    typedef = \
      'verb-lex := lex-item & \
                 [ SYNSEM.LOCAL.CAT.HEAD verb ].'
    mylang.add(typedef)
    typedef = \
      'main-verb-lex := verb-lex & basic-verb-lex & \
                      [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].'
    mylang.add(typedef)
    typedef = \
      'aux-lex := verb-lex & \
                [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].'
    mylang.add(typedef)
    if vcluster:
      mylang.add('main-verb-lex := [ SYNSEM.VC + ].')
      mylang.add('aux-lex := [ SYNSEM.VC - ].')
  else:
    #mainorverbtype = 'verb-lex'
    vcluster = False
    mylang.add('verb-lex := basic-verb-lex.')

  typedef = mainorverbtype + ' :=  \
       [ SYNSEM.LOCAL [ CAT.VAL [ SPR < >, \
                                  SPEC < >, \
                                  SUBJ < #subj > ], \
                        CONT.HOOK.XARG #xarg ], \
         ARG-ST < #subj & \
                  [ LOCAL [ CAT.VAL [ SPR < >, \
                                      COMPS < > ], \
                            CONT.HOOK.INDEX #xarg ] ], ... > ].'
  mylang.add(typedef)

  if hclightallverbs:
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')
  elif hclight:
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
#    mylang.add_literal(comment)
    mylang.add(mainorverbtype + ' := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].')

  # intransitive verb lexical type
  typedef = \
    'intransitive-verb-lex := ' + mainorverbtype + ' & intransitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < > ].'
  mylang.add(typedef)

  # transitive verb lexical type
  typedef = \
    'transitive-verb-lex := ' + mainorverbtype + ' & transitive-lex-item & \
       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
         ARG-ST < [ ], \
                  #comps & \
                  [ LOCAL.CAT [ VAL [ SPR < >, \
                                      COMPS < > ] ] ] > ].'
  mylang.add(typedef)

  customize_verb_case()

  # Lexical entries
  lexicon.add_literal(';;; Verbs')

  # Now create the lexical entries for all the defined verb types
  cases = case.case_names(ch)
  for verb in ch.get('verb',[]):
    name = get_name(verb)
    val = verb.get('valence')

    i = val.find(',')
    dir_inv = ''
    if i != -1:
      val = val[:i]
      dir_inv = 'dir-inv-'

    if val == 'trans':
      tivity = 'trans'
    elif val == 'intrans':
      tivity = 'intrans'
    elif val.find('-') != -1:
      c = val.split('-')
      a_case = case.canon_to_abbr(c[0], cases)
      o_case = case.canon_to_abbr(c[1], cases)
      tivity = a_case + '-' + o_case + '-trans'
    else:
      s_case = case.canon_to_abbr(val, cases)
      tivity = s_case + '-intrans'

    stype = dir_inv + tivity + 'itive-verb-lex'
    vtype = name + '-verb-lex'

    mylang.add(vtype + ' := ' + stype + '.')

    customize_feature_values(verb, vtype, 'verb', None, cases)

    for stem in verb.get('stem', []):
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = \
        TDLencode(orth) + ' := ' + vtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

#######################################################
def customize_users_auxtype(aux, userstype, supertype):
  """
  A utility that declares the userstype as subtype of supertype and
  calls the functions that specify feature values on the type.
  Called by customize_auxiliaries.
  userstype = userstypename in customize_auxiliaries
  supertype = userstypename in customize_auxiliaries
  """

  customize_feature_values(aux, userstype, 'aux')
  customize_feature_values(aux, userstype, 'auxcomplement')
  mylang.add(userstype + ' := ' + supertype + '.')



def customize_determiners():

  # Lexical type for determiners, if the language has any:
  if ch.get('has-dets') == 'yes':
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

  # Determiners
  if 'det' in ch:
    lexicon.add_literal(';;; Determiners')

  for det in ch.get('det',[]):
    name = get_name(det)

    stype = 'determiner-lex'
    dtype = name + '-determiner-lex'

    mylang.add(dtype + ' := ' + stype + '.')

    customize_feature_values(det, dtype, 'det')

    for stem in det.get('stem',[]):
      orth = stem.get('orth')
      pred = stem.get('pred')
      typedef = \
        TDLencode(orth) + ' := ' + dtype + ' & \
                    [ STEM < "' + orth + '" >, \
                      SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef)

def customize_misc_lex():

  #lexicon.add_literal(';;; Other')

  # Question particle
  if ch.get('q-part'):
    orth = ch.get('q-part-orth')
    typedef = \
      TDLencode(orth) + ' := qpart-lex-item & \
                   [ STEM < "' + orth + '" > ].'
    lexicon.add(typedef)


def customize_lexicon():

  mylang.set_section('nounlex')
  customize_nouns()

  mylang.set_section('otherlex')
  to_cfv = case.customize_case_adpositions(mylang, lexicon, ch)
  process_cfv_list(to_cfv, tdlfile=lexicon)

  mylang.set_section('verblex')
  customize_verbs()

  if has_auxiliaries_p():
    mylang.set_section('auxlex')
    users_types = auxiliaries.customize_auxiliaries(mylang, ch, lexicon)
    if len(users_types) > 2:
      customize_users_auxtype(users_types[0], users_types[1], users_types[2])

  mylang.set_section('otherlex')
  customize_determiners()
  customize_misc_lex()

######################################################################
# customize_test_sentences(grammar_path)
#   Create the script file entries for the user's test sentences.

def customize_test_sentences(grammar_path):
  try:
    b = open(os.path.join(grammar_path, 'lkb/script'), 'r')
    lines = b.readlines()
    b.close()
    s = open(os.path.join(grammar_path, 'lkb/script'), 'w')
    ts = open(os.path.join(grammar_path, 'test_sentences'), 'w')
    for l in lines:
      l = l.strip()
      if l == ';;; Modules: Default sentences':
        s.write('(if (eq (length *last-parses*) 1)\n')
        s.write('   (setf *last-parses* \'(')
        if 'sentence' not in ch:
          s.write('""')
        for sentence in ch.get('sentence',[]):
          s.write('"' + sentence.get('orth','') + '" ')
          ts.write(sentence.get('orth','') + '\n')
        s.write(')))\n')
      else:
        s.write(l + '\n')
    s.close()
    ts.close()
  except:
    pass

def customize_script(grammar_path):
  try:
    b = open(os.path.join(grammar_path, 'lkb/script'), 'r')
    lines = b.readlines()
    b.close()
    s = open(os.path.join(grammar_path, 'lkb/script'), 'w')
    for l in lines:
      l = l.strip()
      if l == ';;; Modules: LOAD my_language.tdl':
        myl = ch.get('language').lower() + '.tdl'
        s.write('   (lkb-pathname (parent-directory) "' + myl + '")\n')
      else:
        s.write(l + '\n')
    s.close()
  except:
    pass

######################################################################
# customize_pettdl()
#

def customize_pettdl(grammar_path):
  try:
    p_in = open('matrix-core/pet.tdl', 'r')
    lines = p_in.readlines()
    p_in.close()
    myl = ch.get('language').lower()
    p_out = open(os.path.join(grammar_path, myl + '-pet.tdl'), 'w')
    for l in lines:
      l = l.strip()
      p_out.write(l + '\n')
      if l == ':include "matrix".':
        p_out.write(':include "' + myl + '".\n')
    p_out.close()
    set_out = open(os.path.join(grammar_path, 'pet/' + myl + '-pet.set'), 'w')
    set_out.write(';;;; settings for CHEAP -*- Mode: TDL; Coding: utf-8 -*-\n')
    set_out.write('include "global".\n')
    set_out.write('include "flop".\n')
    set_out.write('include "pet".\n')
    set_out.close()
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

  # ERB 2007-01-21 Need to add [MC +] for inversion strategy for
  # questions, but it's hard to see how this could hurt in general,
  # so let's just put it in.

  typedef = \
    'root := phrase & \
       [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < >, \
                                    COMPS < > ], \
                              MC + ],\
                        COORD - ] ].'
  roots.add(typedef, comment)

  if has_auxiliaries_p() or 'noaux-fin-nf' in ch:
    roots.add('root := [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')

  # ERB 2006-10-05 I predict a bug here:  If we a language with auxiliaries
  # and question particles, we're going to need to make sure that FORM is
  # compatible with comp.

  if ch.get('q-part'):
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
# Version Control
#   Use shell commands to setup Mercurial or Bazaar, if the user
#   has specified that they want one or the other.

def setup_vcs(ch, grammar_path):
  if 'vcs' in ch:
    from subprocess import call
    IGNORE = open(os.devnull,'w')
    cwd = os.getcwd()
    os.chdir(grammar_path)
    if ch['vcs'] == 'git':
      call(['git', 'init'], stdout=IGNORE, stderr=IGNORE)
      call(['git', 'add', '.'], stdout=IGNORE, stderr=IGNORE)
      call(['git', 'commit',
            '--author="Grammar Matrix <matrix-dev@u.washington.edu>"',
            '-m "Initial commit."'], stdout=IGNORE, stderr=IGNORE)
    elif ch['vcs'] == 'hg':
      call(['hg', 'init'], stdout=IGNORE, stderr=IGNORE)
      call(['hg', 'add'], stdout=IGNORE, stderr=IGNORE)
      call(['hg', 'commit',
            '-u Grammar Matrix <matrix-dev@u.washington.edu>',
            '-m "Initial commit."'], stdout=IGNORE, stderr=IGNORE)
    #elif ch['vcs'] == 'bzr':
    #  call(['bzr', 'init', grammar_path])
    #  call(['bzr', 'add', grammar_path])
    #  call(['bzr', 'commit', '-m "Initial commit."'],
    #       stdout=IGNORE, stderr=IGNORE)
    os.chdir(cwd)
    IGNORE.close()

######################################################################
# customize_matrix(path)
#   Create and prepare for download a copy of the matrix based on
#   the choices file in the directory 'path'.  This function
#   assumes that validation of the choices has already occurred.

def customize_matrix(path, arch_type, destination=None):
  if os.path.isdir(path):
    path = os.path.join(path, 'choices')
  # if no destination dir is specified, just use the choices file's dir
  destination = destination or os.path.dirname(path)

  global ch
  ch = ChoicesFile(path)

  language = ch['language']

  grammar_path = get_grammar_path(ch.get('iso-code', language).lower(),
                                  language.lower(), destination)

  # Copy from matrix-core
  if os.path.exists(grammar_path):
    shutil.rmtree(grammar_path)
  core_path = os.path.join(os.environ.get('CUSTOMIZATIONROOT',''),
                           'matrix-core')
  # Use the following command when python2.6 is available
  #shutil.copytree('matrix-core', grammar_path,
  #                ignore=shutil.ignore_patterns('.svn'))
  shutil.copytree(core_path, grammar_path)
  # Since we cannot use shutil.ignore_patterns until 2.6, remove .svn dirs
  shutil.rmtree(os.path.join(grammar_path, '.svn'), ignore_errors=True)
  shutil.rmtree(os.path.join(grammar_path, 'lkb/.svn'), ignore_errors=True)
  shutil.rmtree(os.path.join(grammar_path, 'pet/.svn'), ignore_errors=True)
  # include a copy of choices (named 'choices' to avoid collisions)
  shutil.copy(path, os.path.join(grammar_path, 'choices'))

  # Create TDL object for each output file
  global mylang, rules, irules, lrules, lexicon, roots
  mylang =  tdl.TDLfile(os.path.join(grammar_path, language.lower() + '.tdl'))
  mylang.define_sections([['addenda', 'Matrix Type Addenda', True, False],
                          ['features', 'Features', True, False],
                          ['dirinv', 'Direct-Inverse', True, False],
                          ['lextypes', 'Lexical Types', True, True],
                          ['nounlex', 'Nouns', False, False],
                          ['verblex', 'Verbs', False, False],
                          ['auxlex', 'Auxiliaries', False, False],
                          ['otherlex', 'Others', False, False],
                          ['lexrules', 'Lexical Rules', True, False],
                          ['phrases', 'Phrasal Types', True, False],
                          ['coord', 'Coordination', True, False]])
  rules =   tdl.TDLfile(os.path.join(grammar_path, 'rules.tdl'))
  irules =  tdl.TDLfile(os.path.join(grammar_path, 'irules.tdl'))
  lrules =  tdl.TDLfile(os.path.join(grammar_path, 'lrules.tdl'))
  lexicon = tdl.TDLfile(os.path.join(grammar_path, 'lexicon.tdl'), False)
  roots =   tdl.TDLfile(os.path.join(grammar_path, 'roots.tdl'))

  # date/time
  try:
    f = open('datestamp', 'r')
    matrix_dt = f.readlines()[0].strip()
    f.close()
  except:
    matrix_dt= 'unknown time'

  current_dt = datetime.datetime.utcnow()
  tdl_dt = current_dt.strftime('%a %b %d %H:%M:%S UTC %Y')
  lisp_dt = current_dt.strftime('%Y-%m-%d_%H:%M:%S_UTC')

  # Put the current date/time in my_language.tdl...
  mylang.add_literal(
    ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n' +
    ';;; Grammar of ' + ch.get('language') + '\n' +
    ';;; created at:\n' +
    ';;;     ' + tdl_dt + '\n' +
    ';;; based on Matrix customization system version of:\n' +
    ';;;     ' + matrix_dt + '\n' +
    ';;;\n' + format_comment_block(ch.get('comment')) + '\n' +
    ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;')

  # BUT, put the date/time of the Matrix version in Version.lsp (along
  # with the name of the language.
  global version_lsp
  version_lsp = tdl.TDLfile(os.path.join(grammar_path, 'Version.lsp'))

  version_lsp.add_literal('(in-package :common-lisp-user)\n\n' +
                          '(defparameter *grammar-version* \"' +
                          ch.get('language') + ' (' + lisp_dt + ')\")')

  # Initialize various type hierarchies
  case.init_case_hierarchy(ch, hierarchies)
  init_person_hierarchy()
  init_number_hierarchy()
  init_pernum_hierarchy()
  init_gender_hierarchy()
  init_tense_hierarchy()
  init_aspect_hierarchy()
  init_situation_hierarchy()
  init_mood_hierarchy()
  init_form_hierarchy()
  init_other_hierarchies()

  # The following might modify hierarchies in some way, so it's best
  # to customize those components and only have them contribute their
  # information to lexical rules when we customize inflection.
  customize_lexicon()
  customize_arg_op()
  direct_inverse.customize_direct_inverse(ch, mylang, hierarchies)
  case.customize_case(mylang, ch, hierarchies)
  #argument_optionality.customize_arg_op(ch, mylang)
  # after all structures have been customized, customize inflection,
  # but provide the methods the components above have for their own
  # contributions to the lexical rules
  add_lexrules_methods = [case.add_lexrules,
                          argument_optionality.add_lexrules,
                          direct_inverse.add_lexrules]
  to_cfv = morphotactics.customize_inflection(ch, add_lexrules_methods,
                                              mylang, irules, lrules)
  process_cfv_list(to_cfv)

  # Call the other customization functions
  customize_person_and_number()
  customize_gender()
  customize_form()
  customize_tense()
  customize_aspect()
  customize_situation()
  customize_mood()
  customize_other_features()
  word_order.customize_word_order(mylang, ch, rules)
  customize_sentential_negation()
  customize_coordination()
  customize_yesno_questions()
  customize_test_sentences(grammar_path)
  customize_script(grammar_path)
  customize_pettdl(grammar_path)
  customize_roots()

  # Save the output files
  mylang.save()
  rules.save()
  irules.save()
  lrules.save()
  lexicon.save()
  roots.save()
  version_lsp.save()

  # Setup version control, if any
  setup_vcs(ch, grammar_path)

  return grammar_path


def get_grammar_path(isocode, language, destination):
  '''
  Using the language or iso-code, get a unique pathname
  for the grammar directory.
  '''
  # three possibilities for dir names. If all are taken, raise an exception
  for dir_name in [isocode, language, isocode + '_grammar']:
    grammar_path = os.path.join(destination, dir_name)
    # if grammar_path already exists as a file, it is likely the choices file
    if not (os.path.exists(grammar_path) and os.path.isfile(grammar_path)):
      return grammar_path
  raise Exception("Grammar directory not available.")

###############################################################
# Allow customize_matrix() to be called directly from the
# command line or shell scripts.

if __name__ == "__main__":
  customize_matrix(sys.argv[1], 'tgz')

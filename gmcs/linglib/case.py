from gmcs.utils import TDLencode
from gmcs.lib import TDLHierarchy
from gmcs.utils import get_name

######################################################################
# customize_case()
#   Create the type definitions associated with the user's choices
#   about case.

# case_names()
#   Create and return a list containing information about the cases
#   in the language described by the current choices.  This list consists
#   of tuples with three values:
#     [canonical name, friendly name, abbreviation]
def case_names(ch):
  # first, make two lists: the canonical and user-provided case names
  cm = ch.get('case-marking')
  canon = []
  user = []
  if cm == 'nom-acc':
    canon.append('nom')
    user.append(ch[cm + '-nom-case-name'])
    canon.append('acc')
    user.append(ch[cm + '-acc-case-name'])
  elif cm == 'erg-abs':
    canon.append('erg')
    user.append(ch[cm + '-erg-case-name'])
    canon.append('abs')
    user.append(ch[cm + '-abs-case-name'])
  elif cm == 'tripartite':
    canon.append('s')
    user.append(ch[cm + '-s-case-name'])
    canon.append('a')
    user.append(ch[cm + '-a-case-name'])
    canon.append('o')
    user.append(ch[cm + '-o-case-name'])
  elif cm in ['split-s']:
    canon.append('a')
    user.append(ch[cm + '-a-case-name'])
    canon.append('o')
    user.append(ch[cm + '-o-case-name'])
  elif cm in ['fluid-s']:
    a_name = ch[cm + '-a-case-name']
    o_name = ch[cm + '-o-case-name']
    canon.append('a+o')
    user.append('fluid')
    canon.append('a')
    user.append(a_name)
    canon.append('o')
    user.append(o_name)
  elif cm in ['split-n', 'split-v']:
    canon.append('nom')
    user.append(ch[cm + '-nom-case-name'])
    canon.append('acc')
    user.append(ch[cm + '-acc-case-name'])
    canon.append('erg')
    user.append(ch[cm + '-erg-case-name'])
    canon.append('abs')
    user.append(ch[cm + '-abs-case-name'])
  elif cm in ['focus']:
    canon.append('focus')
    user.append(ch[cm + '-focus-case-name'])
    canon.append('a')
    user.append(ch[cm + '-a-case-name'])
    canon.append('o')
    user.append(ch[cm + '-o-case-name'])

  # fill in any additional cases the user has specified
  for case in ch.get('case'):
    canon.append(case['name'])
    user.append(case['name'])

  # if possible without causing collisions, shorten the case names to
  # three-letter abbreviations; otherwise, just use the names as the
  # abbreviations
  abbrev = [ l[0:3] for l in user ]
  if len(set(abbrev)) != len(abbrev):
    abbrev = user

  return zip(canon, user, abbrev)

# Given the canonical (i.e. choices variable) name of a case, return
# its abbreviation from the list of cases, which should be created by
# calling case_names().  If there is no abbreviation, return the name.
def canon_to_abbr(name, cases):
  for c in cases:
    if c[0] == name:
      return c[2]
  return name

# Given the name of a case, return its abbreviation from the list of
# cases, which should be created by calling case_names().  If there
# is no abbreviation, return the name.
def name_to_abbr(name, cases):
  for c in cases:
    if c[1] == name:
      return c[2]
  return name

def init_case_hierarchy(ch, hierarchies):
  cm = ch.get('case-marking')
  cases = case_names(ch)

  hier = TDLHierarchy('case')

  # For most case patterns, just make a flat hierarchy.  For fluid-s,
  # split-n and split-v, however, a more articulated hierarchy is required.
  if cm in ['nom-acc', 'erg-abs', 'tripartite', 'split-s', 'focus']:
    for c in cases:
      hier.add(c[2], 'case', c[1])
  elif cm in ['fluid-s']:
    abbr = canon_to_abbr('a+o', cases)
    for c in cases:
      if c[0] in ['a', 'o']:
        hier.add(c[2], abbr, c[1])
      else:
        hier.add(c[2], 'case', c[1])
  elif cm in ['split-n', 'split-v']:
    nom_a = canon_to_abbr('nom', cases)
    acc_a = canon_to_abbr('acc', cases)
    erg_a = canon_to_abbr('erg', cases)
    abs_a = canon_to_abbr('abs', cases)
    if cm == 'split-v':
      for c in cases:
        hier.add(c[2], 'case', c[1])
    else:  # 'split-n':
      hier.add('a', 'case', 'transitive agent')
      hier.add('s', 'case', 'intransitive subject')
      hier.add('o', 'case', 'transitive patient')
      for c in cases:
        if c[2] == erg_a:
          hier.add(c[2], 'a', c[1])
        elif c[2] == nom_a:
          hier.add(c[2], 'a', c[1])
          hier.add(c[2], 's', c[1])
        elif c[2] == abs_a:
          hier.add(c[2], 's', c[1])
          hier.add(c[2], 'o', c[1])
        elif c[2] == acc_a:
          hier.add(c[2], 'o', c[1])
        else:
          hier.add(c[2], 'case', c[1])

  if not hier.is_empty():
    hierarchies[hier.name] = hier


# customize_case_type()
#   Create a type for case

def customize_case_type(mylang, hierarchies):
  if 'case' in hierarchies:
    hierarchies['case'].save(mylang)

# customize_case_adpositions()
#   Create the appropriate types for case-marking adpositions

def customize_case_adpositions(mylang, lexicon, ch):
  cases = case_names(ch)
  features = ch.features()
  to_cfv = []

  if ch.has_adp_case():
    comment = \
      ';;; Case-marking adpositions\n' + \
      ';;; Case marking adpositions are constrained not to\n' + \
      ';;; be modifiers.'
    mylang.add_literal(comment)

    mylang.add('+np :+ [ CASE case ].', section='addenda')

    typedef = \
      'case-marking-adp-lex := basic-one-arg & raise-sem-lex-item & \
          [ SYNSEM.LOCAL.CAT [ HEAD adp & [ CASE #case, MOD < > ], \
                               VAL [ SPR < >, \
                                     SUBJ < >, \
                                     COMPS < #comps >, \
                                     SPEC < > ]], \
            ARG-ST < #comps & [ LOCAL.CAT [ HEAD noun & [ CASE #case ], \
                                            VAL.SPR < > ]] > ].'
    mylang.add(typedef)

    if ch.has_mixed_case():
      mylang.add('+np :+ [ CASE-MARKED bool ].', section='addenda')
      mylang.add(
        'case-marking-adp-lex := \
         [ ARG-ST < [ LOCAL.CAT.HEAD.CASE-MARKED - ] > ].')

    # Lexical entries
    lexicon.add_literal(';;; Case-marking adpositions')

    for adp in ch.get('adp',[]):
      orth = adp.get('orth','')

      # figure out the abbreviation for the case this adp marks
      cn = ''
      abbr = ''
      for feat in adp.get('feat', []):
        if feat['name'] == 'case':
          cn = feat['value']
          break

      abbr = name_to_abbr(cn, cases)

      adp_type = TDLencode(abbr + '-marker')
      typedef = \
        adp_type + ' := case-marking-adp-lex & \
                        [ STEM < "' + orth + '" > ].'
      lexicon.add(typedef)

      to_cfv += [(adp.full_key, adp_type, 'adp')]

  return to_cfv

def customize_case(mylang, ch, hierarchies):
  # first we need to peek in things like lex-rules for when something
  # has mixed case, and replace with the appropriate type covering
  from gmcs.linglib.lexbase import ALL_LEX_TYPES
  cases = case_names(ch)
  for x in ALL_LEX_TYPES:
    for lex_type in ch[x]:
      convert_mixed_case(lex_type, hierarchies, cases)
    for pc in ch[x + '-pc']:
      convert_mixed_case(pc, hierarchies, cases)
      for lrt in pc['lrt']:
        convert_mixed_case(lrt, hierarchies, cases)
  # now output the case hierarchies
  customize_case_type(mylang, hierarchies)

def convert_mixed_case(item, hierarchies, cases):
  for feat in item.get('feat',[]):
    if feat['name'] == 'case' and ',' in feat['value']:
      v = [canon_to_abbr(c, cases) for c in feat['value'].split(', ')]
      feat['value'] = hierarchies['case'].get_type_covering(v)

def add_lexrules(ch):
  # only need to add rules for mixed_case
  if not ch.has_mixed_case():
    return
  for pc in ch['noun-pc']:
    if 'case' in [feat['name'] for lrt in pc['lrt'] for feat in lrt['feat']]:
      for c in case_names(ch):
        if ch.has_adp_case(c[0]):
          idx = ch[pc.full_key + '_lrt'].next_iter_num()
          lrt_key = pc.full_key + '_lrt' + str(idx)
          ch[lrt_key + '_name'] = get_name(pc) + '-synth-' + c[0]
          ch[lrt_key + '_feat1_name'] = 'case'
          ch[lrt_key + '_feat1_value'] = c[0]
          ch[lrt_key + '_lri1_inflecting'] = 'no'
          ch[lrt_key + '_lri1_orth'] = ''

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



def customize_verb_case(mylang, ch):
  cm = ch.get('case-marking')
  cases = case_names(ch)

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
          a_case = canon_to_abbr(c[0], cases)
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
          a_case = canon_to_abbr(c[0], cases)
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
        
      elif p[0] == 'trans' or len(c) == 2:  # transitive
        if p[0] == 'trans':
          a_case = ''
          o_case = ''
          a_head = ch.case_head()
          o_head = ch.case_head()
        else:
          a_case = canon_to_abbr(c[0], cases)
          o_case = canon_to_abbr(c[1], cases)
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
          a_case = canon_to_abbr(c[0], cases)
          a_head = ch.case_head(c[0])
          o_case = canon_to_abbr(c[1], cases)
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
          a_case = canon_to_abbr(c[0], cases)
          b_case = canon_to_abbr(c[1], cases)
          o_case = canon_to_abbr(c[2], cases)

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

      elif p[0] == 'ditrans' or len(c) == 3:            #ditrans
        
        if p[0] == 'ditrans':
          a_case = ''
          b_case = ''
          o_case = ''
        else:
          a_case = canon_to_abbr(c[0], cases)
          b_case = canon_to_abbr(c[1], cases)
          o_case = canon_to_abbr(c[2], cases)

        a_head = 'noun'
        b_head = 'noun'
        o_head = 'noun'
        if a_case and b_case and o_case:
          t_type = dir_inv + a_case + '-' + b_case + '-' + o_case + '-ditransitive-verb-lex'
        else:
          t_type = dir_inv + 'ditransitive-verb-lex'

        if t_type != 'ditransitive-verb-lex':
          mylang.add(t_type + ' := ditransitive-verb-lex.')

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


      else:     # intransitive
        if c[0] == 'intrans':
          s_case = ''
          s_head = ch.case_head()
        else:
          s_case = canon_to_abbr(c[0], cases)
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





##################
### VALIDATION ###
##################

def validate(choices, vr):
  cm = choices.get('case-marking')

  if not cm:
    vr.err('case-marking', 'You must specify if/how case is marked.')

  if cm in ('nom-acc', 'split-n', 'split-v'):
    validate_one_case(choices, vr, cm + '-nom')
    validate_one_case(choices, vr, cm + '-acc')
  if cm in ('erg-abs', 'split-n', 'split-v'):
    validate_one_case(choices, vr, cm + '-erg')
    validate_one_case(choices, vr, cm + '-abs')
  if cm in ('tripartite', 'split-s', 'fluid-s', 'focus'):
    validate_one_case(choices, vr, cm + '-a')
    validate_one_case(choices, vr, cm + '-o')
  if cm in ('tripartite'):
    validate_one_case(choices, vr, cm + '-s')
  if cm in ('focus'):
    validate_one_case(choices, vr, cm + '-focus')

  if cm == 'none' and 'case' in choices:
    for case in choices['case']:
      vr.err(case.full_key + '_name',
             'You may not specify additional cases ' +
             'if your language has no case marking.')

  if 'scale' in choices and not choices.get('scale-equal'):
    vr.err('scale-equal',
           'If you define a direct-inverse scale, ' +
           'you must say what direction the verb is ' +
           'when the agent and patient have equal rank.')

######################################################################
# validate_one_case(pre)
#   A helper function to validate the user's choices about one case.
#   pre is the first few characters of the associated choices names
#  (e.g. 'nom-acc-nom')

def validate_one_case(ch, vr, pre):
  if not ch.get(pre + '-case-name'):
    vr.err(pre + '-case-name', 'You must specify a name for every case.')


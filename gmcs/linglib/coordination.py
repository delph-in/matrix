from gmcs.utils import TDLencode

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf, agreement, np_number, mylang, rules, irules):
  mylang.add_literal(';;; Coordination Strategy ' + num)

  pn = pos + num
  if pos == 'n' or pos == 'np':
    headtype = 'noun'
  elif pos == 'adj':
    headtype = 'adj'
  elif pos == 'adp':
    headtype = 'adp'
    add_adposition_rules(mylang)
  else:
    headtype = 'verb'

  #this allows users to define agreement features in the choices file
  agr = agreement.split(',')
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

  # adding agreement constraints
  # something better should be done to get the right supertypes to the right
  # coordination phrases (consider function to split them in groups...)

  # que feature shared for all if wh-is present
  if 'que' in agr:
    share_wh_properties(mylang, pn, mid)

  if pos == 'n' or pos == 'np' or pos == 'adj':
    noun_feat = ['case','vc']
    for nf in noun_feat:
      if nf in agr:
        add_sharing_supertypes(mylang, pn, mid, nf) 
    

    if np_number and pos != 'adj':
      mylang.add(pn + '-top-coord-rule := \
               [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + np_number + ' ].')
  #adjectives also need to share png info for agreement with nouns
    elif pos == 'adj':
      if 'prd' in agr:
        add_sharing_supertypes(mylang, pn, mid, 'prd') 
      path = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.'
      add_shared_features(mylang, 'png', path, mid)
      add_sharing_supertypes(mylang, pn, mid, 'png') 
   #2011-11-07 predicative adjectives need to share their XARG
      path = 'SYNSEM.LOCAL.CONT.HOOK.'
      add_shared_features(mylang, 'xarg', path, mid)
      add_sharing_supertypes(mylang, pn, mid, 'xarg') 

  elif pos == 'v' or pos == 'vp' or pos == 's':
###removing vc from verbal features, causes problems for coordinating verbal forms
###TO DO: double check over-generation, if overgenerates: make inherit from left most
###only for verbs
    verb_feat = ['vfront', 'form', 'mc', 'inv']
    for vf in verb_feat:
      if vf in agr:      
        add_sharing_supertypes(mylang, pn, mid, vf) 
    if pos == 'v' or pos == 'vp':
      path = 'SYNSEM.LOCAL.CONT.HOOK.'
      add_shared_features(mylang, 'xarg', path, mid)
      add_sharing_supertypes(mylang, pn, mid, 'xarg') 
 
###should probably not be general, but since it won't do any harm....
    if pos == 's':
      mylang.add('s1-bottom-coord-rule := \
                        [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
  
  elif pos == 'adp':
    if 'prd' in agr:
      add_sharing_supertypes(mylang, pn, mid, 'prd')


def customize_coordination(mylang, ch, lexicon, rules, irules):
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
    agreement = ''
    agreement = cs.get('agreement')
    np_number = ''
    np_number = cs.get('npnumber')

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

  ####
  # agreement is only head features for now
  # inversion is also a head feature that needs to be shared
  # adding it where appropriate

    if ch.get('q-inv') == 'on':
      agreement += ',inv'
    if ch.get('has-adp') or ch.get('has-adj'):
      if ch.get('has-cop'):
        agreement +=',prd'
    agr_path = 'SYNSEM.LOCAL.CAT.HEAD.'
    agr =  agreement.split(',')
    for my_agr in agr:
      add_shared_features(mylang, my_agr, agr_path, mid)

###function create coordinated structures that share syntactic properties
###used for specific analyses
    feat = determine_syntactic_features_tobe_shared(ch)
    create_coord_sharing_cat_features(mylang, feat, mid)

###appending syntactic features to agreement to for next phase
###both agreement and syntactic features are relevant to determine
###dependent supertypes
    for f in feat:
      agreement += ',' + f
    if ch.get('wh-questions') == 'yes':
      agreement += ',que' 
 
    for pos in ('n', 'np', 'vp', 's', 'adj','adp'):
      if cs.get(pos):
        define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf, agreement,
    np_number, mylang, rules, irules)


###
# general function for created phrases that share features
#

def add_shared_features(mylang, f, path, mid):
  compl_path =  path + str.upper(f) + ' #' + f
  mylang.add(f + '-agr-top-coord-rule := top-coord-rule & \
                  [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].' )

  if mid:
    mylang.add(f + '-agr-mid-coord-rule := mid-coord-rule &\
               [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].')

  mylang.add(f + '-agr-bottom-coord-rule := bottom-coord-phrase &\
             [ ' + compl_path + ', \
               NONCONJ-DTR.' + compl_path + ' ].')

def add_sharing_supertypes(mylang, pn, mid, agr):
  vp_except = False
  if 'vp' in pn and (agr == 'inv'):
    vp_except = True
    if agr == 'inv':
      mylang.add('no-pass-inv-top-coord := top-coord-rule & \
                   [ LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                     RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
      mylang.add(pn + '-top-coord-rule := no-pass-inv-top-coord.')
  if not vp_except:
    mylang.add(pn + '-top-coord-rule :=  ' + agr + '-agr-top-coord-rule.')
  if mid:   
    mylang.add(pn + '-mid-coord-rule := ' + agr + '-agr-mid-coord-rule.')
  mylang.add(pn + '-bottom-coord-rule := ' + agr + '-agr-bottom-coord-rule.')  


def share_wh_properties(mylang, pn, mid):
  path = 'SYNSEM.NON-LOCAL.'
  f = 'que'
  add_shared_features(mylang, f, path, mid)
  add_sharing_supertypes(mylang, pn, mid, f)


#######
# GERMANIC
#

def determine_syntactic_features_tobe_shared(ch):
  features = []
  if determine_vfront(ch):
    features.append('vfront')

  wo = ch.get('word-order')
  if wo == 'v2' or wo == 'free':
    features.append('mc')
###vc cluster was turned off, but it turns out it is needed for nouns
###check why was turned off? Possibly excluding wrongly for VP-coordination?
###fix this
    if ch.get('verb-cluster') == 'yes': 
      features.append('vc')

  return features
  
def determine_vfront(ch):
  vfront = False
  if ch.get('split-cluster') == 'yes':
    if ch.get('vc-analysis') == 'aux-rule':
      vfront = True  
    if ch.get('split-analysis') == 'lex-rule':
      vfront = True
  return vfront

#####
# this function shares features across coordination that
# are related to specific analyses

def create_coord_sharing_cat_features(mylang, feat, mid):
  path = 'SYNSEM.LOCAL.CAT.'
  for f in feat:  
    add_shared_features(mylang, f, path, mid)


def add_adposition_rules(mylang):

  ###adp-rules must share their head because of PRD value
  mylang.add('adp-coord-phrase := event-coord-phrase & \
  [ SYNSEM.LOCAL.CAT.HEAD #head & adp, \
    LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head, \
    RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')

  mylang.add('basic-adp-top-coord-rule := adp-coord-phrase & \
               [ C-CONT [ RELS <! !>, \
	                  HCONS <! !> ]].')

  mylang.add('basic-adp-mid-coord-rule := adp-coord-phrase & \
               [ SYNSEM.LOCAL.COORD-REL #crel, \
                 C-CONT [ RELS <! #crel !>, \
	                  HCONS <! !> ] ].')

  mylang.add('adp-bottom-coord-phrase := bottom-coord-phrase & \
               [ SYNSEM.LOCAL.CAT.HEAD #head & adp, \
                 NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')


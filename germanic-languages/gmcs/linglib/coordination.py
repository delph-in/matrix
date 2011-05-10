from gmcs.utils import TDLencode


######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf, agreement, np_number, ch, mylang, rules, irules):
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

  if pos == 's':
   
    if 'form' in agr:

      mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                                  LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                                  RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].')
      if mid:   
        mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                                  LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                                  RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].')
      mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                                 NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].') 

  wo = ch.get('word-order')

  if wo == 'v2' or wo == 'free':
    mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.MC #mc, \
                                    LCOORD-DTR.SYNSEM.LOCAL.CAT.MC #mc, \
                                    RCOORD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    if mid:   
      mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.MC #mc, \
                                    LCOORD-DTR.SYNSEM.LOCAL.CAT.MC #mc, \
                                    RCOORD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.MC #mc, \
                                   NONCONJ-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    if ch.get('q-inv')== 'on': 
      if pos == 'v' or pos == 'vp' or pos == 's':
        mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                                    LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                                    RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
        if mid:   
          mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                                    LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                                    RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
        mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                                   NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
    if ch.get('split-cluster') == 'yes':
      if ch.get('vc-analysis') == 'aux-rule' or ch.get('split-analysis') == 'lex-rule':
        mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                   LCOORD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                   RCOORD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
        if mid:   
          mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                     LCOORD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                     RCOORD-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')
        mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.VFRONT #vf, \
                                     NONCONJ-DTR.SYNSEM.LOCAL.CAT.VFRONT #vf ].')

  if pos == 'n' or pos == 'np' or pos == 'adj':
    if 'case' in agr:
      mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
                                 LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
                                 RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')
      if mid:   
        mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
                                  LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
                                  RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')
      mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.CASE #case, \
                                 NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')
    if np_number and pos != 'adj':
      mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + 
                                 np_number + ' ].')
    elif pos == 'adj':
      mylang.add(pn + '-top-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png, \
                                 LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png, \
                                 RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png ].')
      if mid:   
        mylang.add(pn + '-mid-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png, \
                                  LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png, \
                                  RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png ].')
      mylang.add(pn + '-bottom-coord-rule := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png, \
                                 NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.PNG #png ].')
  # Now define the rule instances into rules.tdl.  As above, the mid
  # or left rule may not be necessary.

  rules.add(pn + '-top-coord := ' + pn + '-top-coord-rule.')
  if mid:
    rules.add(pn + '-mid-coord := ' + pn + '-mid-coord-rule.')
  rules.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  if left:
    rules.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')



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

    for pos in ('n', 'np', 'vp', 's', 'adj','adp'):
      if cs.get(pos):
        define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf, agreement, np_number, ch, mylang, rules, irules)

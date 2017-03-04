from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf, mylang, rules, irules, cs):
  mylang.add_literal(';;; Coordination Strategy ' + num)

  pn = pos + num
  if pos == 'n' or pos == 'np':
    headtype = 'noun'
  else:
    headtype = 'verb'

  # TODO what if it's not resolution? move this outside this function, I'd say
  resrules = []
  if cs.get('feat'):
    for feat in cs.get('feat'):  # TODO custom features/other features
      v = feat.get('value')
      templist = []
      for rule in feat.get('rule'): # TODO try to do this without using cs (bringing a list into this function?)
        ch1, ch2, par = tuple(rule['value'].split(", "))

        nm = '-' + ch1 + '-' + ch2

        if v == 'gend': # TODO fix this so it's less specific/more generalizable
          st = ch1 + '-' + ch2 + '-gend-coord-rule & '
        elif v == 'pers':
          st = ch1 + '-' + ch2 + '-per-coord-rule & '
        elif v == 'num':
          st = ch1 + '-' + ch2 + '-num-coord-rule & '

        templist += [(nm, st)]

      newlist = []
      if resrules: # if we have rules already, iterate through them
        for rule in resrules:
          for temp in templist:
            newlist += [(rule[0]+temp[0], rule[1]+temp[1])]
        resrules = newlist
      else:
        resrules = templist
  # TODO check - AT THIS POINT we should have a list, "rules", which contains all the combinations of the...
  # TODO ...various feature resolution rules



  # First define the rules in mylang.  Every strategy has a
  # top rule and a bottom rule, but only some have a mid rule, so if
  # the mid prefix argument $mid is empty, don't emit a rule.
  # Similarly, not all strategies have a left rule.
  # TODO could I just have a list of feature lists, and not worry about the name of them?
  # TODO for list in lists: for child1, child2, parent in list:
  # TODO probably not, because I do have to specify the correct path for the feature
  # TODO this may get tricky for languages that define their own features
  for nm, st in resrules:
    mylang.add(pn + nm + '-top-coord-rule :=\
                 basic-' + pos + '-top-coord-rule &\
                 ' + top + 'top-coord-rule &\
                 ' + st +\
                 '[ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].')
    if mid:
      mylang.add(pn + nm + '-mid-coord-rule :=\
                   basic-' + pos + '-mid-coord-rule &\
                   ' + mid + 'mid-coord-rule &\
                   ' + st +\
                   '[ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].')

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
           ' + pos + '-bottom-coord-phrase & pass-up-png-coord-rule &\
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
  for nm, st in resrules:
      rules.add(pn + nm + '-top-coord := ' + pn + nm + '-top-coord-rule.')
      if mid:
        rules.add(pn + nm + '-mid-coord := ' + pn + nm + '-mid-coord-rule.')
  rules.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  if left:
    rules.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')

def customize_feature_resolution(mylang, ch, cs):
  # TODO handle more than pseudospanish
  # TODO some of the more general types might be shared between coordination strategies
  # TODO handle empty lists
  # TODO handle the custom features

  mylang.add_literal(';;; Feature Resolution Rules')

  if cs.get('feat'):
    for feat in cs.get('feat'): # TODO custom features/other features
        v = feat.get('value')
        for rule in feat.get('rule'):
          ch1, ch2, par = tuple(rule['value'].split(", "))

          if v == 'gend': # TODO see if I can generate the feature path using just the name of the feature
            tn = ch1 + '-' + ch2 + '-gend-coord-rule:= coord-phrase &\
                         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + par + ','
            if ch1 != 'any':
              tn += 'LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + ch1 + ','
            if ch2 != 'any':
              tn += 'RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + ch2 + '].'

          elif v == 'pers':
            tn = ch1 + '-' + ch2 + '-per-coord-rule:= coord-phrase &\
                        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + par + ','
            if ch1 != 'any':
              tn += 'LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + ch1 + ','
            if ch2 != 'any':
              tn += 'RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + ch2 + '].'

          elif v == 'num':
            if (ch1 != 'any' and ch2 != 'any'):
              tn = ch1 + '-' + ch2 + '-num-coord-rule:= coord-phrase &\
                          [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + par + ','
            else: # TODO this handles if "any" is in both children and should be replicated for all
              # TODO but more ideally, this function should be more generalized
              tn = ch1 + '-' + ch2 + '-num-coord-rule:= coord-phrase &\
                          [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + par + '].'
            if ch1 != 'any':
              tn += 'LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + ch1 + ','
            if ch2 != 'any':
              tn += 'RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + ch2 + '].'

          mylang.add(tn) # TODO what if there isn't a tn here?

  mylang.add('pass-up-png-coord-rule:= bottom-coord-phrase &\
                 [SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png,\
                 NONCONJ-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ].')

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
    orthstr = orth_encode(orth)
    order = cs.get('order')

    pre = ''
    suf = ''

    tn = TDLencode(orth)
    if (len(ch.get('cs')) > 1):
      tn += csnum

    if mark == 'word':
      lexicon.add( tn + ' := conj-lex &\
                  [ STEM < "' + orthstr + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                    CFORM "' + csnum + '" ].')
      if pat == 'omni':
        lexicon.add( tn + '_nosem := nosem-conj-lex &\
                      [ STEM < "' + orthstr + '" >,\
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

    if cs.get('agr') == 'resolution':
      customize_feature_resolution(mylang, ch, cs)

    for pos in ('n', 'np', 'vp', 's'):
      if cs.get(pos):
        # TODO move the features resolution rules function so I can remove cs from the below function
        define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf, mylang, rules, irules, cs)

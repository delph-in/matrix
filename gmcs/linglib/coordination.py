from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf, mylang, rules, irules, resrules):
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
  # TODO could I just have a list of feature lists, and not worry about the name of them?
  # TODO probably not, because I do have to specify the correct path for the feature
  # TODO double check that this is all correct for custom features
  for nm, st in resrules: # TODO handle cases where we don't have nm and st
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


def customize_feature_resolution(mylang, ap):
  # TODO handle more than pseudospanish
  # TODO some of the more general types might be shared between coordination strategies
  # TODO handle empty lists
  # TODO handle the custom features

  mylang.add_literal(';;; Feature Resolution Rules')

  if ap.get('feat'):
    for feat in ap.get('feat'): # TODO custom features/other features
        v = feat.get('name')
        for rule in feat.get('rule'):
          ch1= rule.get('left') if rule.get ('left') else 'any'
          ch2 = rule.get('right') if rule.get('right') else 'any'
          par = rule.get('par') if rule.get ('par') else 'any' # TODO although it should always have parent

          # TODO to get the features (and thereby paths) from other-features:
          # for feature in ch.get('feature', []):
          #   feat = feature.get('name', '')
          #   type = feature.get('type', '')
          #   hier = TDLHierarchy(feat, type)
          #
          #   if feature.get('new', '') == 'yes':
          #     for value in feature.get('value', []):
          #       val = value.get('name')
          #       for supertype in value.get('supertype', []):
          #         stype = supertype.get('name')
          #         hier.add(val, stype) # TODO what does this do?
          #   else:
          #     if type == 'head':
          #       mylang.add('head :+ [ ' + feat.upper() + ' ' + feature.get('existing', '') + ' ].',
          #                  section='addenda')
          #     else:
          #       mylang.add('png :+ [ ' + feat.upper() + ' ' + feature.get('existing', '') + ' ].',
          #                  section='addenda')

          if v == 'gender': # TODO see if I can generate the feature path using just the name of the feature
            # TODO person, number, gender, pernum
            tn = ch1 + '-' + ch2 + '-gend-coord-rule:= coord-phrase &\
                         [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + par + ','
            if ch1 != 'any':
              tn += 'LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + ch1 + ','
            if ch2 != 'any':
              tn += 'RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND ' + ch2 + '].'

          elif v == 'person':
            tn = ch1 + '-' + ch2 + '-per-coord-rule:= coord-phrase &\
                        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + par + ','
            if ch1 != 'any':
              tn += 'LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + ch1 + ','
            if ch2 != 'any':
              tn += 'RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.PER ' + ch2 + '].'

          elif v == 'number':
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


def get_feature_resolution_names(ap):
  # TODO what if it's not resolution? move this outside this function, I'd say
  resrules = []
  if ap.get('feat'):
    for feat in ap.get('feat'):  # TODO custom features/other features
      v = feat.get('name')

      templist = []
      for rule in feat.get('rule'):
        ch1 = rule.get('left') if rule.get('left') else 'any'
        ch2 = rule.get('right') if rule.get('right') else 'any'
        par = rule.get('par') if rule.get('par') else 'any'

        nm = '-' + ch1 + '-' + ch2

        if v == 'gender':  # TODO fix this so it's less specific/more generalizable
          st = ch1 + '-' + ch2 + '-gend-coord-rule & '
        elif v == 'person':
          st = ch1 + '-' + ch2 + '-per-coord-rule & '
        elif v == 'number':
          st = ch1 + '-' + ch2 + '-num-coord-rule & '

        templist += [(nm, st)]

      # add the rules to resrules
      newlist = []
      if resrules:  # if we have rules already, iterate through them
        for rule in resrules:
          for temp in templist:
            newlist += [(rule[0] + temp[0], rule[1] + temp[1])]
        resrules = newlist
      else:
        resrules = templist
    return resrules


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

    # TODO below: maybe not a for loop if I'm connecting to specific agreement patterns?
    # TODO maybe add the connection to strategy in teh website and THEN change this code
    for ap in ch.get('ap'):# TODO make this so that specific agreement patterns can be linked to coord strategies
      if ap.get('agr') == 'resolution':
        customize_feature_resolution(mylang, ap)
        resol_rules = get_feature_resolution_names(ap)
      elif ap.get('agr') == 'conjunct':
        pass # TODO don't pass
    if not ch.get('ap'):
      resol_rules = [('', '')]

    for pos in ('n', 'np', 'vp', 's'):
      if cs.get(pos):
        define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf, mylang, rules, irules, resol_rules)

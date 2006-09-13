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
#   about basic word order.

def customize_word_order():
  pass


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

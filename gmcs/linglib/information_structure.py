from gmcs.linglib import features
from gmcs.utils import get_name
from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

tdls = []

def add_lexrules(mylang, tdl, _section):
  if tdl not in tdls:
    mylang.add(tdl, merge = True, section=_section)
    tdls.append(tdl)

def customize_information_structure(mylang, ch, rules, irules, lexicon, hierarchies):
  if ch.get('info-str-focus-lex') == 'on':

    affix_num = adp_num = mod_num = 1

    for fm in ch.get('focus-marker'):            

      _type = fm['type']
      _pos = fm['pos']
      _cat = fm['cat']
      _orth = fm['orth']

      _dtr = _head = ''
      if _cat == 'nouns':
        _dtr = 'noun-lex'
        _head = 'noun'
      elif _cat == 'verbs':
        _dtr = 'verb-lex'
        _head = 'verb'


      if _type == 'affix':
        tdl = 'infostr-lex-rule := infl-add-only-no-ccont-lex-rule.'
        add_lexrules(mylang, tdl, 'lexrules')
        tdl = 'focus-lex-rule := infostr-lex-rule & [ SYNSEM.LOCAL.CONT.ICONS <! focus !> ].'
        add_lexrules(mylang, tdl, 'lexrules')    
      elif _type == 'adp':
        pass
      else:
        pass

      
      if _type == 'affix':        
        _name = ''

        if _dtr != '':
          _name = 'focus-' + _cat + '-lex-rule'
          tdl = _name + ' := focus-lex-rule &'
          tdl += '[DTR ' + _dtr + ' ].'
          add_lexrules(myland, tdl, 'lexrules')

        irules_foc = 'focus-suffix-'+ str(affix_num)

        if _pos == 'before':
          irules_foc += ' :=\n%prefix '
        else:
          irules_foc += ' :=\n%suffix '

        irules_foc += '(* ' + _orth + ')\n' + _name + '.'
        irules.add_literal(irules_foc)
        affix_num += 1

      elif _type == 'adp':
      #The following line should be deleted after the lexicon page can handle adp for information structure.
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
            ARG-ST < #comps & [ LOCAL.CAT [ HEAD ' + _head + ' & [ CASE #case ], \
                                            VAL.SPR < > ]] > ].'
        mylang.add(typedef)
        
        orth = orth_encode(_orth)

        adp_type = TDLencode(_orth + '-marker')
        typedef = \
            adp_type + ' := case-marking-adp-lex & \
                        [ STEM < "' + orth + '" >, \
                          SYNSEM.LOCAL.CAT [ MKG fc \
			                     VAL.COMPS.FIRST.LOCAL.CONT.HOOK.ICONS-KEY focus ] ].'
        lexicon.add(typedef)       

      else:
        pass







##################
### VALIDATION ###
##################

def validate(choices):
  # add validation tests specific to this module
  pass

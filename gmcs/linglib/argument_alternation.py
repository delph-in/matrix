
######################################################################
# customize_argument_alternation()
#   Create the type definitions associated with argument_alternation

# GERMANIC-BASED LIBRARY ONLY!!!!
# This library contains possibilities to go beyond Germanic languages
# but is not based on any cross-linguistic research whatsoever!


def customize_argument_alternation(ch, mylang, climb_files, lrules, lexicon):
  climb_arg_alt = climb_files.get('arg-alternation')
  if ch.get('dative-shift') == 'on':
    customize_dative_shift(ch, mylang, lrules, lexicon, climb_arg_alt)


def customize_dative_shift(ch, mylang, lrules, lexicon, climb_arg_alt):

 #for now: assuming the shift always goes noun1-noun2 -> noun2-adp(noun1)
 #cases and proposition must be marked in choices
 
  dat_shift = '''dative-shift-lrule :=  val-change-only-lex-rule & const-lex-rule &
  [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,
			     COMPS < [ LOCAL [ CAT.HEAD noun,
					       CONT #cont2 ],
				       OPT - ], 
                                    [ LOCAL [ CAT.HEAD adp,
					      CONT #cont1 ],
                                      OPT - ] >,
			     SPR #spr,
			     SPEC #spec ] ],
    DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,
				 COMPS < [ LOCAL [ CAT.HEAD noun,
						   CONT #cont1 ] ],
                                         [ LOCAL [ CAT.HEAD noun,
						   CONT #cont2 ]] >,
				 SPR #spr,
				 SPEC #spec ] ] ].'''

  mylang.add(dat_shift)
  climb_arg_alt.add(dat_shift)

  original_cases = ch.get('ditrans-case-marking').split('-')
  old_c1 = original_cases[1]
  old_c2 = original_cases[2]

  mylang.add('dative-shift-lrule := \
   [ DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE ' + old_c1 + '], \
                                  [ LOCAL.CAT.HEAD.CASE ' + old_c2 + ' ] >].')
  climb_arg_alt.add('dative-shift-lrule := \
   [ DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE ' + old_c1 + '], \
                                  [ LOCAL.CAT.HEAD.CASE ' + old_c2 + ' ] >].')

  new_marking = ch.get('dat-shifted-pattern').split('-')
  new_m1 = new_marking[0]
  new_m2 = new_marking[1]

  
  mylang.add('dative-shift-lrule := \
   [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE ' + new_m1 + '], \
                                  [ LOCAL.CAT.HEAD.FORM ' + new_m2 + ' ] >].')
  climb_arg_alt.add('dative-shift-lrule := \
   [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.CASE ' + new_m1 + '], \
                                  [ LOCAL.CAT.HEAD.FORM ' + new_m2 + ' ] >].')

  lrules.add('dative-shift := dative-shift-lrule.')
  climb_arg_alt.add('dative-shift := dative-shift-lrule.',section='lrules')


###info for marking preposition
  mylang.add(new_m2 + ' := pform.', 'Form of preposition in dative shift', section='features')
  climb_arg_alt.add(new_m2 + ' := pform.', 'Form of preposition in dative shift', section='features')

  gen_cm_adp = '''case-marking-adp-lex := basic-one-arg & raise-sem-lex-item &
  [ SYNSEM.LOCAL.CAT [ HEAD adp &
                            [ MOD < > ],
                       VAL [ SPR < >,
                             SUBJ < >,
                             COMPS < #comps >,
                             SPEC < > ] ],
    ARG-ST < #comps &
             [ LOCAL.CAT [ HEAD noun &
                                [ CASE acc ],
                           VAL.SPR < > ] ] > ].'''
  mylang.add(gen_cm_adp, section='otherlex')
  climb_arg_alt.add(gen_cm_adp, comment='section=otherlex')


  mylang.add('dative-comp-adp-lex := case-marking-adp-lex & \
 [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + new_m2 + ' ].')

  climb_arg_alt.add('dative-comp-adp-lex := case-marking-adp-lex & \
 [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + new_m2 + ' ].')

  
  lexicon.add(new_m2 + '-dat-shift := dative-comp-adp-lex & \
                    [ STEM < "' + new_m2 + '" > ].') 
  climb_arg_alt.add(new_m2 + '-dat-shift := dative-comp-adp-lex & \
                    [ STEM < "' + new_m2 + '" > ].', section='lexicon') 

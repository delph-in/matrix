from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

def customize_sentential_negation(mylang, ch, lexicon, rules, lrules):
  # JDC 2012-06-01 Nowadays this function is something like 
  # the entry point for the sentential negation library.
  # So it makes sense to put some notes here. 

  # The first point is that the customization feature
  # 'negation=plus' is mapped to a series of different
  # lexical rule types based on the overall negation type
  # selected.  
  # 
  # This is done so that users can enjoy the simplicity of 
  # selecting a negation strategy and then marking the 
  # relevant rules 'negation=plus' without worrying 
  # too much about the details. 

  # However, because these lexical rules have different 
  # properties, in this file we map negation=plus to
  # the following values so that features.py and 
  # morphotactics.py can define the rules correctly.
  
  # negation values on lexical rules:
  #   'a' = standard simple negation, 
  #         * cont-change-only-lex-rule 
  #   'b' = lex rule with negation semantics,
  #         also requires NEGFORM on its complement
  #         this should only appear on auxiliaries
  #   'c' = form changing lex rule,
  #         changes form to negform and
  #         does not add semantics
  #         ...


  if ch.get('neg-head-feature') == 'on':
    mylang.add('head :+ [ NEGATED luk ].', section='addenda')

  exp = ch.get('neg-exp') # exponence of sentential negation: simple, bipartite
  if exp == '1':
    # we have a simple negation strategy
    if ch.get('infl-neg') == 'on':
      # two things to do:
      #  * add supertype to appropriate lri (for morphotactic.py to work)
      #  * change value of negation=plus to negation=a (for feature.py to work)
      for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
          for f in lrt['feat']:
            if 'negation' in f['name'] and f['value']=='plus':
              lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
                                            ['cont-change-only-lex-rule']) 
              f['value'] = 'a'
    elif ch.get('neg-aux') == 'on':
      pass
      # nothing to do, neg-aux is like any other aux, 
      # should be handled appropriately by word-order library
    elif ch.get('adv-neg') == 'on':
      # call create_neg_adv with proper parameters!
      # create_neg_adv_lex_item(mylang, ch, lexicon, rules, 1)
      customize_adv_neg(mylang, ch, lexicon, rules)
    elif ch.get('comp-neg') == 'on':
      # call create_neg_comp with proper parameters!
      # create_neg_comp_lex_item(mylang, ch, lexicon, rules, lrules)
      customize_comp_neg(mylang, ch, lexicon, rules, lrules)
  elif exp == '2': 
    bnegtype = ch.get('bineg-type')
    if bnegtype == 'infl-infl':
      customize_infl_infl_neg(ch)
    elif bnegtype == 'infl-head':
      customize_infl_head_neg()
    elif bnegtype == 'infl-comp':
      customize_infl_comp_neg(mylang,ch,lexicon)
    elif bnegtype == 'infl-mod':
      customize_infl_mod_neg()
    elif bnegtype == 'head-head':
      customize_head_head_neg()
    elif bnegtype == 'head-comp':
      customize_head_comp_neg()
    elif bnegtype == 'head-mod':
      customize_head_mod_neg()
    elif bnegtype == 'comp-comp':
      customize_comp_comp_neg()
    elif bnegtype == 'comp-mod':
      customize_comp_mod_neg()
    elif bnegtype == 'mod-mod':
      customize_mod_mod_neg()
#  if ch.get('neg-exp') == '2':
#    # see if we need to make any neg advs
#    if ch.get('neg1-type')[0] == 'f' or \
#      ch.get('neg2-type')[0] == 'f':
#      create_neg_adv_lex_item(mylang, ch, lexicon, rules, 2)

def customize_adv_neg(mylang, ch, lexicon, rules):
  # first add lexical type for negative adverb
  mylang.set_section('otherlex')
  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD.MOD < [ LOCAL.CAT.HEAD verb ] > ]].''',
             'Type for negative adverbs.')

  mylang.add_comment('neg-adv-lex',
    '''This adverb should go through a specialized phrase structure rule
       included with this grammar.''')

  # now parameterize as pre/posthead, no value here means both orders
  # should work
  if ch.get('neg-order') == 'before':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
  elif ch.get('neg-order') == 'after':
    mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')

  # constrain type of constituent modified by neg-adv
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
    
  # add spelling for neg-adverb
  if(ch.get('neg-adv-orth')):
    orth = ch.get('neg-adv-orth')
    orthstr = orth_encode(orth)
    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

  rules.add('adj-head-scop := adj-head-scop-phrase.')
  rules.add('head-adj-scop := head-adj-scop-phrase.',
          'Rule instances for head-modifier structures. Corresponding types\n' +
          'are defined in matrix.tdl.  The matrix customization script did\n' +
          'not need to add any further constraints, so no corresponding types\n' +
          'appear in ' + ch.get('language').lower() + '.tdl')
  mylang.add('+nvcdmo :+ [ MOD < > ].',
             'This grammar includes head-modifier rules.  To keep\n' +
             'out extraneous parses, constrain the value of MOD on\n' +
             'various subtypes of head.  This may need to be loosened later.\n' +
             'This constraint says that only adverbs, adjectives,\n' +
             'and adpositions can be modifiers.',
             section='addenda')
  # that's all folks!

def customize_comp_neg(mylang, ch, lexicon, rules, lrules):
  # the neg-comp analyses require the neg-head-feature choice
  # simulate it here if it's not on 
  if(ch.get('neg-head-feature')!='on'):
    mylang.add('head :+ [ NEGATED luk ].', section='addenda')

  # first add lexical type
  mylang.set_section('otherlex')
  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
             '''Type for negative selected comps. 
                This type uses the MOD list to get scopal semantics.
                Constrain head-modifier rules to be [NEGATED -] if you don't
                want this type to act as a modifer.''')

  # okay, now add the spelling and lexical instance
  if(ch.get('comp-neg-orth')):
    orth = ch.get('comp-neg-orth')
    orthstr = orth_encode(orth)
    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

  # we need the lexical rule that adds these types to the comps lists
  # of the verbs that selecte them
  if ch.get('comp-neg-order') == 'before':
    mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
               [ SYNSEM.LOCAL [ CAT.VAL [  SUBJ #subj,
                                           COMPS < canonical-synsem & 
                                                 [ LOCAL.CAT.HEAD [ NEGATED +,
                                MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] 
                                                   . #comps > ] ],
                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
                                              COMPS #comps ],
                                        CONT.HOOK #hook ] ].
               ''')

  elif ch.get('comp-neg-order') == 'after':
    mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
               [ SYNSEM.LOCAL.CAT.VAL [  SUBJ #subj,
                                         COMPS < #comps , canonical-synsem & 
                                             [ LOCAL.CAT.HEAD [ NEGATED +,
                                             MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] > ],
                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
                                              COMPS.FIRST #comps ],
                                    CONT.HOOK #hook ] ].
               ''')

  lrules.add('neg-lex-rule := neg-comp-add-lex-rule.')
  # deal with type of selecting verb: auxiliary verb or any finite verb
  if(ch.get('comp-neg-head')=='aux'):
    mylang.add('neg-comp-add-lex-rule := [ DTR aux-lex ].')
  elif(ch.get('comp-neg-head')=='v'):
    mylang.add('''neg-comp-add-lex-rule := [ DTR verb-lex &
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ] ].''')

  # all done!


#def create_neg_comp_lex_item(mylang, ch, lexicon, rules, lrules):
#
#  mylang.set_section('otherlex')
#  
#  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
#                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
#                                            COMPS < >,
#                                            SUBJ < > ],
#                                      HEAD [ NEGATED +, 
#                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
#             '''Type for negative selected comps. 
#                This type uses the MOD list to get scopal semantics.
#                Constrain head-modifier rules to be [NEGATED -] if you don't
#                want this type to act as a modifer.''')
#
#
#  
#  if(ch.get('comp-neg-orth')):
#    orth = ch.get('comp-neg-orth')
#    orthstr = orth_encode(orth)
#    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
#                [ STEM < \"'+ orthstr +'\" >,\
#                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')
#
#
#  # we need the lexical rule to add these types to the comps lists
#  # of the verbs that selecte them
#  if ch.get('comp-neg-order') == 'before':
#    mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
#               [ SYNSEM.LOCAL [ CAT.VAL [  SUBJ #subj,
#                                           COMPS < canonical-synsem & 
#                                                 [ LOCAL.CAT.HEAD [ NEGATED +,
#                                MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] 
#                                                   . #comps > ] ],
#                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
#                                              COMPS #comps ],
#                                        CONT.HOOK #hook ] ].
#               ''')
#
#  elif ch.get('comp-neg-order') == 'after':
#    mylang.add('''neg-comp-add-lex-rule := const-val-change-only-lex-rule &
#               [ SYNSEM.LOCAL.CAT.VAL [  SUBJ #subj,
#                                         COMPS < #comps , canonical-synsem & 
#                                             [ LOCAL.CAT.HEAD [ NEGATED +,
#                                             MOD < [ LOCAL.CONT.HOOK #hook ] > ] ] > ],
#                 DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
#                                              COMPS.FIRST #comps ],
#                                    CONT.HOOK #hook ] ].
#               ''')
#
#  lrules.add('neg-lex-rule := neg-comp-add-lex-rule.')
#
#  # deal with type of selecting verb: auxiliary verb or any finite verb
#  if(ch.get('comp-neg-head')=='aux'):
#    mylang.add('neg-comp-add-lex-rule := [ DTR aux-lex ].')
#  elif(ch.get('comp-neg-head')=='v'):
#    mylang.add('''neg-comp-add-lex-rule := [ DTR verb-lex &
#                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ] ].''')
#
#
#
#def create_neg_adv_lex_item(mylang, ch, lexicon, rules, exp):
#  mylang.set_section('otherlex')
#  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
#                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
#                                            COMPS < >,
#                                            SUBJ < > ],
#                                      HEAD.MOD < [ LOCAL.CAT.HEAD verb ] > ]].''',
#             'Type for negative adverbs.')
#
#  mylang.add_comment('neg-adv-lex',
#    '''Constrain the MOD value of this adverb to keep\n
#    it from modifying the kind of verbs which can select it,\n
#    To keep spurious parses down, as a starting point, we have\n
#    assumed that it only modifies verbs (e.g., non-finite verbs).''')
#
#  if exp == 1:
#    if ch.get('neg-order') == 'before':
#      mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
#    elif ch.get('neg-order') == 'after':
#      mylang.add('neg-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
#
#    if ch.get('neg-mod') == 's':
#      mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
#                                                                                     COMPS null ]].''')
#    elif ch.get('neg-mod') == 'vp':
#      mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
#                                                                                     COMPS null ]].''')
#    elif ch.get('neg-mod') == 'v':
#      mylang.add('''neg-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')
#    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
#    lexical Vs for V-level attachment of negative adverbs.''')
#  elif exp == 2:
#    if ch.get('neg1-type')[0] == 'f':
#      mylang.add('neg1-adv-lex := neg-adv-lex.')
#      if ch.get('neg1-order') == 'before':
#        mylang.add('neg1-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].')
#      elif ch.get('neg1-order') == 'after':
#        mylang.add('neg1-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].')
#
#      if ch.get('neg-mod') == 's':
#        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
#                                                                                       COMPS null ]].''')
#      elif ch.get('neg1-mod') == 'vp':
#        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
#                                                                                       COMPS null ]].''')
#      elif ch.get('neg1-mod') == 'v':
#        mylang.add('''neg1-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''')
#
#    if ch.get('neg2-type')[0] == 'f':
#      mylang.add('neg2-adv-lex := neg-adv-lex.')
#        
#      if ch.get('neg2-order') == 'before':
#        mylang.add('neg2-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD - ].', merge=True)
#      elif ch.get('neg2-order') == 'after':
#        mylang.add('neg2-adv-lex := [ SYNSEM.LOCAL.CAT.POSTHEAD + ].', merge=True)
#
#      if ch.get('neg2-mod') == 's':
#        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ null,
#                                                                                       COMPS null ]].''', merge=True)
#      elif ch.get('neg2-mod') == 'vp':
#        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.VAL [ SUBJ cons,
#                                                                                       COMPS null ]].''', merge=True)
#      elif ch.get('neg2-mod') == 'v':
#        mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LIGHT + ].''', merge=True)
#
#    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].','''verb-lex is HC-LIGHT - to allow us to pick out\n
#    lexical Vs for V-level attachment of negative adverbs.''')
#    
#
#  # ERB 2006-09-22 Validation should really make sure we have a value of
#  # neg-adv-orth before we get here, but just in case, checking first, since
#  # the script gets really unhappy if I try to write to an empty type.
#
#  if(ch.get('neg-adv-orth')):
#    orth = ch.get('neg-adv-orth')
#    orthstr = orth_encode(orth)
#    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
#                [ STEM < \"'+ orthstr +'\" >,\
#                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')
#
#  if(ch.get('neg1-adv-orth')):
#    orth = ch.get('neg1-adv-orth')
#    orthstr = orth_encode(orth)
#    lexicon.add(TDLencode(orth) + '1 := neg1-adv-lex &\
#                [ STEM < \"'+ orthstr +'\" >,\
#                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')
#
#  if(ch.get('neg2-adv-orth')):
#    orth = ch.get('neg2-adv-orth')
#    orthstr = orth_encode(orth)
#    lexicon.add(TDLencode(orth) + '2 := neg2-adv-lex &\
#                [ STEM < \"'+ orthstr +'\" > ].')
#
#
#  # ERB 2006-10-06 And of course we need the head-modifier rules, if we're
#  # going to have an independent modifier.  While we're at it, we need to
#  # contrain the MOD value on the rest of the head types to keep them
#  # from going nuts.
#
#  #if advAlone != 'never':
#  #rules.add('head-adj-scop := head-adj-scop-phrase.')
#  #rules.add('adj-head-scop := adj-head-scop-phrase.')
#  if ch.get('neg-exp') == '2' and ch.get('neg1-type')[0] == 'b' and ch.get('neg2-type') == 'fd':
#    # because neg1 is bound and neg2 is and adverb, we'll use the NEG-SATISFIED feature
#    # to engineer the dependency
#
#    mylang.add('''cat :+ [ NEG-SATISFIED luk ].''', section='addenda')
#    mylang.add('''clause :+ [ SYNSEM.LOCAL.CAT.NEG-SATISFIED na-or-+ ].''', section='addenda')
#    mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.NEG-SATISFIED + ].''', merge=True, section='otherlex')
#    mylang.add('''basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns, HEAD-DTR.SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns ].''', section='addenda')
#    mylang.add('''basic-head-subj-phrase :+ [ SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns, HEAD-DTR.SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns ].''', section='addenda')
#
#    # also, if we don't want neg2 to be able to occur alone, we require that the 
#    # verb it modifies is NEGATED +
#    mylang.add('''neg2-adv-lex := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [ LOCAL.CAT.HEAD.NEGATED + ] > ].''', merge=True, section='otherlex')
#
#    if ch.get('neg2-order')=='after':
#      mylang.add('''neg-head-adj-scop-phrase := head-adj-scop-phrase &
#                            [ SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns,
#                              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns ].''')
#      rules.add('head-adj-scop := neg-head-adj-scop-phrase.',
#            'Rule instances for head-modifier structures. Corresponding types\n' +
#             'are defined in matrix.tdl.  This rule copies NEG-SATISFIED up \n' +
#             'from the non-head daughter (the negative adv), other rules \n' +
#             'pass NEG-SATISFIED up the head path\n')
#    else:
#      mylang.add('''neg-adj-head-scop-phrase := adj-head-scop-phrase &
#                            [ SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns,
#                              NON-HEAD-DTR.SYNSEM.LOCAL.CAT.NEG-SATISFIED #ns ].''')
#      rules.add('adj-head-scop := neg-adj-head-scop-phrase.',
#            'Rule instances for head-modifier structures. Corresponding types\n' +
#             'are defined in matrix.tdl.  This rule copies NEG-SATISFIED up \n' +
#             'from the non-head daughter (the negative adv), other rules \n' +
#             'pass NEG-SATISFIED up the head path\n')
#
#  else:
#    rules.add('adj-head-scop := adj-head-scop-phrase.')
#    rules.add('head-adj-scop := head-adj-scop-phrase.',
#            'Rule instances for head-modifier structures. Corresponding types\n' +
#             'are defined in matrix.tdl.  The matrix customization script did\n' +
#             'not need to add any further constraints, so no corresponding types\n' +
#             'appear in ' + ch.get('language').lower() + '.tdl')
#  mylang.add('+nvcdmo :+ [ MOD < > ].',
#               'This grammar includes head-modifier rules.  To keep\n' +
#               'out extraneous parses, constrain the value of MOD on\n' +
#               'various subtypes of head.  This may need to be loosened later.\n' +
#               'This constraint says that only adverbs, adjectives,\n' +
#               'and adpositions can be modifiers.',
#               section='addenda')

# bipartite negation types
def customize_infl_infl_neg(ch):
  # mostly handled in feature processing
  # and custo system
  # need to change the value of negation=plus
  # and neg2=plus to let
  # feature processing know what kind of lex rules these are 

  for vpc in ch['verb-pc']:
    for lrt in vpc['lrt']:
      for f in lrt['feat']:
        if 'negation' in f['name'] and f['value']=='plus':
#          lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
#                                        ['add-only-rule'])
          lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
                                        [#'same-agr-lex-rule',
#                                         'same-ctxt-lex-rule',
#                                         'val-and-cont-change-lex-rule',
                                         'same-head-lex-rule',
                                         'same-val-lex-rule',
#                                         'same-hc-light-lex-rule',
#                                         'same-posthead-lex-rule',
#                                         'same-mc-lex-rule'
                                         ])
          f['value'] = 'b'
        if 'neg2' in f['name'] and f['value']=='plus':
#          lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +\
#                                        ['head-change-only-rule']) 
          f['name'] = 'negation'
          f['value'] = 'c'
def customize_infl_head_neg():
  pass
  # completely handled by customization system and 
  # existing infrastructure.
  
  # * neg-aux is a typical aux, 
  # * the required FORM value is added
  #   to the questionairre page automatically, 
  # * user selects that the inflector is FORM negform 
  #   and makes the position class required to enforce 
  #   complementary distribution of FORM 

def customize_infl_comp_neg(mylang,ch,lexicon):
  # neg affix on verb requires neg-adv with neg semantics 

  # this analysis requires the neg-head-feature, if it's not on
  # simulate it here
  if(ch.get('neg-head-feature')!='on'):
    mylang.add('head :+ [ NEGATED luk ].', section='addenda')

  # add type for neg-adv
  mylang.set_section('otherlex')
  mylang.add('''neg-adv-lex := basic-scopal-adverb-lex &
                 [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,
                                            COMPS < >,
                                            SUBJ < > ],
                                      HEAD [ NEGATED +, 
                                             MOD < [ LOCAL.CAT.HEAD verb ] > ] ] ].''',
             '''Type for negative selected comps. 
                This type uses the MOD list to get scopal semantics.
                Constrain head-modifier rules to be [NEGATED -] if you don't
                want this type to act as a modifer.''')

  # add lexical instance
  if(ch.get('comp-neg-orth')):
    orth = ch.get('comp-neg-orth')
    orthstr = orth_encode(orth)
    lexicon.add(TDLencode(orth) + ' := neg-adv-lex &\
                [ STEM < \"'+ orthstr +'\" >,\
                  SYNSEM.LKEYS.KEYREL.PRED \"neg_rel\" ].')

  # inflecting lexical rule must add neg-adv to comps list, 
  # handled in features.py 

def customize_infl_mod_neg():
  pass
def customize_head_head_neg():
  pass
def customize_head_comp_neg():
  pass
def customize_head_mod_neg():
  pass
def customize_comp_comp_neg():
  pass
def customize_comp_mod_neg():
  pass
def customize_mod_mod_neg():
  pass


##################
### VALIDATION ###
##################

def validate(ch, vr):
  if ch.get('infl-neg', default=False):
    for aux in ch.get('aux', []):
      for cf in aux.get('compfeature', []):
        if cf.get('name') == 'negation':
          mess = 'When inflectional negation is selected ' +\
                 '[negation +] should only be found on bound morphemes.'
          vr.err(cf.full_key + '_name', mess)
      for f in aux.get('feat', []):
        if f.get('name') == 'negation':
          mess = 'When inflectional negation is selected ' +\
                 '[negation +] should only be found on bound morphemes.'
          vr.err(f.full_key + '_name', mess)
    for verb in ch.get('verb', []):
      for cf in verb.get('compfeature', []):
        if cf.get('name') == 'negation':
          mess = 'When inflectional negation is selected ' +\
                 '[negation +] should only be found on bound morphemes.'
          vr.err(cf.full_key + '_name', mess)
      for f in verb.get('feat', []):
        if f.get('name') == 'negation':
          mess = 'When inflectional negation is selected ' +\
                 '[negation +] should only be found on bound morphemes.'
          vr.err(f.full_key + '_name', mess)

  neginfltype = ch.get('neg-infl-type')
#  negseladv = ch.get('neg-sel-adv')

  if ch.get('neg-aux', default=False):
    has_neg_aux = False
    for aux in ch.get('aux', []):
      if aux.get('name') == 'neg-aux':
        has_neg_aux = True
        break
    if has_neg_aux == False:
      vr.warn('neg-aux',
              'You\'ve selected neg-aux but there is no corresponding ' +\
              'type in the lexicon.')
  # ERB 2009-01-23 Commenting out the following because infl-neg is
  # now handled with customize_inflection.  We should eventually give
  # a warning if infl-neg is selected but no lexical rules actually
  # use it.  I think it would make sense for that warning to go
  # on the negation page.

  # If affix is indicated, must select prefix/suffix and
  # main/aux/either and give form.
#   if (ch.get('infl-neg') == 'on'):
#     if (not ch.get('neg-infl-type')):
#       mess = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
#       vr.err('neg-infl-type', mess)
#     if (not ch.get('neg-aff')):
#       mess = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
#       vr.err('neg-aff', mess)
#     if (not ch.get('neg-aff-orth')):
#       mess = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
#       vr.err('neg-aff-orth', mess)
#     # If aux is selected then has-aux = 'yes' must be chosen in word order section
#     if ((neginfltype == 'aux' or neginfltype == 'aux-main') and ch.get('has-aux') != 'yes'):
#         mess = 'You have not indicated on the word order page that your language has auxiliaries.'
#         vr.err('neg-infl-type', mess)

  # If adverb is indicated, must lexical entry, what it modifies, and
  # ind/selected modifier -- now only applicable under single negation
  # bipartite negs need to validate this for themeselves
  if (ch.get('adv-neg') == 'on') and (ch.get('neg-exp') == '1'):
#    if (not ch.get('neg-adv')):
#      mess = 'If sentential negation is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
#      vr.err('neg-adv', mess)
#    if (ch.get('neg-adv') == 'ind-adv'):
    if (not ch.get('neg-mod')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what type of constituent the adverb modifies.'
      vr.err('neg-mod', mess)
    if (not ch.get('neg-order')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what side of its host the adverb attaches to.'
      vr.err('neg-order', mess)
    if (not ch.get('neg-adv-orth')):
      mess = 'If sentential negation is expressed through an adverb, ' +\
             'you must specify the form of the adverb.'
      vr.err('neg-adv-orth', mess)

  if ch.get('comp-neg') == 'on':
    if ch.get('comp-neg-head') == 'aux' and ch.get('has-aux') != 'yes':
      mess = 'You have not indicated on the word order page ' +\
             'that your language has auxiliaries.'
      vr.err('comp-neg-head', mess)

   # If aux is selected then has-aux = 'yes' must be chosen in word
   # order section
#    if ((negseladv == 'aux' or negseladv == 'main-aux') and
#        ch.get('has-aux') != 'yes'):
#       mess = 'You have not indicated on the word order page ' +\
#             'that your language has auxiliaries.'
#     vr.err('neg-sel-adv', mess)

   # ERB 2009-01-23 Currently not possible to say how they combine.

#   # If both strategies are checked, then they must say how they combine:
#   if ((ch.get('infl-neg') == 'on') and (ch.get('adv-neg') == 'on')):
#     if (not ch.get('multi-neg')):
#       mess = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
#       vr.err('multi-neg', mess)

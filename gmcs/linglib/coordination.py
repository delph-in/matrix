from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(ch, num, pos, top, mid, bot, left, pre, suf, agreement, np_number, mc_inv_sh, trunc, mylang, rules, irules, climb_coord):
  mylang.add_literal(';;; Coordination Strategy ' + num)

  pn = pos + num
  if pos == 'n' or pos == 'np':
    headtype = 'noun'
  elif pos == 'adj' or pos == 'adv' or pos == 'adp':
    headtype = pos
    if pos == 'adp':
      add_adposition_rules(mylang, climb_coord)
  else:
    headtype = 'verb'

  #this allows users to define agreement features in the choices file

  agr = agreement.split(',')
  
  if mc_inv_sh == 'red': 
    agr.append('mc-red') 
    agr.append('inv-red')   
  # First define the rules in mylang.  Every strategy has a
  # top rule and a bottom rule, but only some have a mid rule, so if
  # the mid prefix argument $mid is empty, don't emit a rule.
  # Similarly, not all strategies have a left rule.
  tr_val = ''
  if trunc == 'on':
    tr_val = 'na'
  else:
    tr_val = '-'

  mylang.add(pn + '-top-coord-rule :=\
               basic-' + pos + '-top-coord-rule &\
               ' + top + 'top-coord-rule &\
               [ SYNSEM.LOCAL.COORD-STRAT "' + num + '", \
                 LCOORD-DTR.SYNSEM.LOCAL.COORD ' + tr_val + ' ].')
  climb_coord.add(pn + '-top-coord-rule :=\
               basic-' + pos + '-top-coord-rule &\
               ' + top + 'top-coord-rule &\
               [ SYNSEM.LOCAL.COORD-STRAT "' + num + '", \
                 LCOORD-DTR.SYNSEM.LOCAL.COORD ' + tr_val + ' ].')
  if mid:
    mylang.add(pn + '-mid-coord-rule :=\
                 basic-' + pos + '-mid-coord-rule &\
                 ' + mid + 'mid-coord-rule &\
                 [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].')
    climb_coord.add(pn + '-mid-coord-rule :=\
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
    climb_coord.add(pn + '-bottom-coord-rule :=\
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
    climb_coord.add_literal(rule,section='irules')
  else:
    rule = pn + '-bottom-coord-rule :=\
           ' + bot + 'bottom-coord-rule &\
           ' + pos + '-bottom-coord-phrase &\
           [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].'
    mylang.add(rule)
    climb_coord.add(rule)
    if bot == 'unary-':
      rule = pn + '-bottom-coord-rule :=\
             [ SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].'
      mylang.add(rule)
      climb_coord.add(rule)

  if left:
    # first the rule in mylang
    rule = pn + '-left-coord-rule :=\
           ' + bot + 'left-coord-rule &\
           ' + pos + '-bottom-coord-phrase &\
           [ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].'
    mylang.add(rule)
    climb_coord.add(rule)

    if pre or suf:
      # constrain the predicate
      mylang.add(pn + '-left-coord-rule :=\
                 [ SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].')
      climb_coord.add(pn + '-left-coord-rule :=\
                 [ SYNSEM.LOCAL.COORD-REL.PRED "_and_coord_rel" ].')

      # now the spelling change rule in irules.tdl
      rule = pn + '-left :=\n'
      if pre:
        rule += '  %prefix (* ' + pre + ')\n'
      else:
        rule += '  %suffix (* ' + suf + ')\n'
      rule += '  ' + pn + '-left-coord-rule.'
      irules.add_literal(rule)
      climb_coord.add_literal(rule,section='irules')

  # Now define the rule instances into rules.tdl.  As above, the mid
  # or left rule may not be necessary.

  rules.add(pn + '-top-coord := ' + pn + '-top-coord-rule.')
  climb_coord.add(pn + '-top-coord := ' + pn + '-top-coord-rule.',section='rules')
  if mid:
    rules.add(pn + '-mid-coord := ' + pn + '-mid-coord-rule.')
    climb_coord.add(pn + '-mid-coord := ' + pn + '-mid-coord-rule.')
  rules.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  climb_coord.add(pn + '-bottom-coord := ' + pn + '-bottom-coord-rule.')
  if left:
    rules.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')
    climb_coord.add(pn + '-left-coord := ' + pn + '-left-coord-rule.')

  # adding agreement constraints
  # something better should be done to get the right supertypes to the right
  # coordination phrases (consider function to split them in groups...)

  # que feature shared for all if wh-is present
  if 'que' in agr:
    share_wh_properties(mylang, climb_coord, pn, mid)
  
  if 'rel' in agr:
    share_rel_properties(mylang, climb_coord, pn, mid)

  if pos == 'n' or pos == 'np' or pos == 'adj':
    if trunc and pos == 'n':
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'gender')
    noun_feat = ['vc']
    if not pos == 'adj':
      noun_feat.append('case')
    else:
       ###Agreement properties
      case_agr = False
      for adjagr in ch.get('adjagr'):
        if adjagr.get('feat') == 'case':
          case_agr = True
      if case_agr:
        noun_feat.append('case')

    for nf in noun_feat:
      if nf in agr:
        add_sharing_supertypes(mylang, climb_coord, pn, mid, nf)
    

    if np_number and pos != 'adj':
      if 'coord' in np_number:
        mylang.add(pn + '-top-coord-rule := \
               [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM #num, \
                 RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM #num ].')
        climb_coord.add(pn + '-top-coord-rule := \
               [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM #num, \
                 RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM #num ].')
      else: 
        mylang.add(pn + '-top-coord-rule := \
               [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + np_number + ' ].')
        climb_coord.add(pn + '-top-coord-rule := \
               [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.NUM ' + np_number + ' ].')
  #adjectives also need to share png info for agreement with nouns
    if pos == 'adj':
      if 'prd' in agr:
        add_sharing_supertypes(mylang, climb_coord, pn, mid, 'prd') 
      path = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CONT.HOOK.INDEX.'
      add_shared_features(mylang, climb_coord, 'png', path, mid)
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'png') 
   #2011-11-07 predicative adjectives need to share their XARG
      path = 'SYNSEM.LOCAL.CONT.HOOK.'
      add_shared_features(mylang, climb_coord, 'xarg', path, mid)
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'xarg') 

  elif pos == 'v' or pos == 'vp' or pos == 's':
###removing vc from verbal features, causes problems for coordinating verbal forms
###TO DO: double check over-generation, if overgenerates: make inherit from left most
###only for verbs
    verb_feat = ['vfront', 'form']
    if not mc_inv_sh == 'red':
      verb_feat.append('mc')
      verb_feat.append('inv')
    else:
      verb_feat.append('mc-red')
      verb_feat.append('inv-red')
    for vf in verb_feat:
      if vf in agr:      
        add_sharing_supertypes(mylang, climb_coord, pn, mid, vf) 
    if pos == 'v' or pos == 'vp':
      path = 'SYNSEM.LOCAL.CONT.HOOK.'
      add_shared_features(mylang, climb_coord, 'xarg', path, mid)
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'xarg')
###taking head value from right most verb
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'head')
       


###should probably not be general, but since it won't do any harm....
    if pos == 's':
      mylang.add('s1-bottom-coord-rule := \
                        [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
      climb_coord.add('s1-bottom-coord-rule := \
                        [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
  
  elif pos == 'adp':
    if 'prd' in agr:
      add_sharing_supertypes(mylang, climb_coord, pn, mid, 'prd')


def customize_coordination(mylang, ch, lexicon, rules, irules, climb_files):
  """
  The main coordination customization routine
  """
  climb_coord = climb_files.get('coord')
  mylang.set_section('coord')
  climb_coord.set_section('mylang')

  if ch.get('truncing') == 'yes':
    mylang.add('trunc-coord-lex-rule := cat-change-only-lex-rule & \
                                                     inflecting-lex-rule & \
                     [ SYNSEM.LOCAL [ COORD na, \
                                      CAT.HEAD +njd ], \
                       DTR.SYNSEM.LOCAL [ CAT.HEAD +njd, \
                                          COORD - ] ].')
    climb_coord.add('trunc-coord-lex-rule := cat-change-only-lex-rule & \
                                                     inflecting-lex-rule & \
                     [ SYNSEM.LOCAL [ COORD na, \
                                      CAT.HEAD +njd ], \
                       DTR.SYNSEM.LOCAL [ CAT.HEAD +njd, \
                                          COORD - ] ].')
    rule_2 = \
    '''
    trunc-marker-suffix :=
%suffix (* -)
trunc-coord-lex-rule.
    '''
    irules.add_literal(rule_2)
    climb_coord.add_literal(rule_2,section='irules')
####also adding trunc lex item from Bart's grammar
    mylang.add('trunc-lex := word-or-lexrule & \
                     [ SYNSEM.LOCAL [ COORD na, \
                                      CAT.HEAD +njd ] ].')
    climb_coord.add('trunc-lex := word-or-lexrule & \
                     [ SYNSEM.LOCAL [ COORD na, \
                                      CAT.HEAD +njd ] ].')
####adding semantic properties of words
    #norm-ltop
    mylang.add('trunc-lex := [ SYNSEM [ LOCAL.CONT [ HOOK [ LTOP #ltop ], \
                          RELS.LIST.FIRST #keyrel ], \
             LKEYS.KEYREL #keyrel & [ LBL #ltop ] ] ].')
    climb_coord.add('trunc-lex := [ SYNSEM [ LOCAL.CONT [ HOOK [ LTOP #ltop ], \
                          RELS.LIST.FIRST #keyrel ], \
             LKEYS.KEYREL #keyrel & [ LBL #ltop ] ] ].')
    #norm-hook
    mylang.add('trunc-lex :=  [ SYNSEM [ LOCAL.CONT.HOOK.INDEX #index, \
             LKEYS.KEYREL.ARG0 #index ] ].')
    climb_coord.add('trunc-lex :=  [ SYNSEM [ LOCAL.CONT.HOOK.INDEX #index, \
             LKEYS.KEYREL.ARG0 #index ] ].')
    #single-rel
    mylang.add('trunc-lex := [ SYNSEM.LOCAL.CONT.RELS <! relation !> ].')
    climb_coord.add('trunc-lex := [ SYNSEM.LOCAL.CONT.RELS <! relation !> ].')
    #gender value of combined N should come from right-hand noun
    #GERMANIC ONLY: ASSUMING THERE IS GENDER (SHOULD CHECK WHETHER GENDER DEFINED)
    mylang.add('gender-agr-bottom-coord-rule := bottom-coord-phrase & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          NONCONJ-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    mylang.add('gender-agr-mid-coord-rule := mid-coord-rule & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    mylang.add('gender-agr-top-coord-rule := top-coord-rule & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    climb_coord.add('gender-agr-bottom-coord-rule := bottom-coord-phrase & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          NONCONJ-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    climb_coord.add('gender-agr-mid-coord-rule := mid-coord-rule & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    climb_coord.add('gender-agr-top-coord-rule := top-coord-rule & \
        [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend, \
          RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.GEND #gend ].')
    #Germanic only: taking AUX value from right most daughter (closest to correct)
    
    mylang.add('head-agr-bottom-coord-rule := bottom-coord-phrase & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')
    mylang.add('head-agr-mid-coord-rule := mid-coord-rule & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')
    mylang.add('head-agr-top-coord-rule := top-coord-rule & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')  
    climb_coord.add('head-agr-bottom-coord-rule := bottom-coord-phrase & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')
    climb_coord.add('head-agr-mid-coord-rule := mid-coord-rule & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')
    climb_coord.add('head-agr-top-coord-rule := top-coord-rule & \
        [ SYNSEM.LOCAL.CAT.HEAD #head, \
          RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')  
    
  for cs in ch.get('cs'):
    csnum = str(cs.iter_num())

    mark = cs.get('mark')
    pat = cs.get('pat')
    orth = cs.get('orth')
    orthstr = orth_encode(orth)
    order = cs.get('order')
    agreement = ''
    agreement = cs.get('agreement')
    np_number = ''
    np_number = cs.get('npnumber')
    trunc = cs.get('trunc')

    pre = ''
    suf = ''

    tn = TDLencode(orth)
    if (len(ch.get('cs')) > 1):
      tn += csnum

    if mark == 'word':
      if ',' in orth:
        words = orth.split(',')
        for w in words:
          w_str = orth_encode(w)
          if w_str != w:
            mywords = w.split(' ')
            w_pred = mywords[0] + '_' + mywords[1]
          else:
            w_pred = w
          lexicon.add(TDLencode(w_pred + '_c' + csnum) + ' := conj-lex &\
                  [ STEM < "' + w_str + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_' + w_pred + '_coord_rel",\
                    CFORM "' + csnum + '" ].')
          climb_coord.add(TDLencode(w_pred + '_c' + csnum) + ' := conj-lex &\
                  [ STEM < "' + w_str + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_' + w_pred + '_coord_rel",\
                    CFORM "' + csnum + '" ].', section='lexicon')
      else:
        lexicon.add(TDLencode(orth) + ' := conj-lex &\
                  [ STEM < "' + orthstr + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                    CFORM "' + csnum + '" ].')
        climb_coord.add(TDLencode(orth) + ' := conj-lex &\
                  [ STEM < "' + orthstr + '" >,\
                    SYNSEM.LKEYS.KEYREL.PRED "_and_coord_rel",\
                    CFORM "' + csnum + '" ].',section='lexicon')
      if pat == 'omni':
        lexicon.add( tn + '_nosem := nosem-conj-lex &\
                      [ STEM < "' + orthstr + '" >,\
                        CFORM "' + csnum + '" ].')
        climb_coord.add( tn + '_nosem := nosem-conj-lex &\
                      [ STEM < "' + orthstr + '" >,\
                        CFORM "' + csnum + '" ].',section='lexicon')

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
      add_shared_features(mylang, climb_coord, my_agr, agr_path, mid)

###function create coordinated structures that share syntactic properties
###used for specific analyses
    feat = determine_syntactic_features_tobe_shared(ch)
    create_coord_sharing_cat_features(mylang, climb_coord, feat, mid)

###appending syntactic features to agreement to for next phase
###both agreement and syntactic features are relevant to determine
###dependent supertypes
    for f in feat:
      agreement += ',' + f
    if ch.get('wh-questions') == 'yes':
      agreement += ',que' 
    if ch.get('rel-clause') == 'yes':
      agreement += ',rel'
####
###object-raising with mc-word-order and arg-composition aux
###cannot work if MC is shared and INV is passed up
###creating special case variable (Germanic specific)
###
    mc_inv_sh = 'all'
    if ch.get('v2-analysis') == 'filler-gap':
      mc_inv_sh = 'all'    
    elif ch.get('vc-analysis') == 'basic':
      if ch.get('obj-raising') == 'yes':
        mc_inv_sh = 'red' 
        agr.append('mc-red') 
        agr.append('inv-red')   


    for pos in ('n', 'np', 'vp', 's', 'adj','adp','adv'):
      if cs.get(pos):
        define_coord_strat(ch, csnum, pos, top, mid, bot, left, pre, suf, agreement,
    np_number, mc_inv_sh, trunc, mylang, rules, irules, climb_coord)
        if pos in ('n','np'):
          if ch.get('reflexives') == 'yes':
            add_np_restrictions(mylang, climb_coord, mid, pos, csnum)


def add_np_restrictions(mylang, climb_coord, mid, pn, csnum):

  mylang.add(pn + csnum + '-bottom-coord-rule := \
    [ NONCONJ-DTR.SYNSEM.LOCAL non-refl-local ].')
  climb_coord.add(pn + csnum + '-bottom-coord-rule := \
    [ NONCONJ-DTR.SYNSEM.LOCAL non-refl-local ].')
  if mid:
    mylang.add(pn + csnum + '-mid-coord-rule := \
         [ LCOORD-DTR.SYNSEM.LOCAL non-refl-local ].')
    climb_coord.add(pn + csnum + '-mid-coord-rule := \
         [ LCOORD-DTR.SYNSEM.LOCAL non-refl-local ].')
  mylang.add(pn + csnum + '-top-coord-rule := \
         [ LCOORD-DTR.SYNSEM.LOCAL non-refl-local ].')
  climb_coord.add(pn + csnum + '-top-coord-rule := \
         [ LCOORD-DTR.SYNSEM.LOCAL non-refl-local ].')
   


###
# general function for created phrases that share features
#

def add_shared_features(mylang, climb_coord, f, path, mid):
  compl_path =  path + str.upper(f) + ' #' + f
  mylang.add(f + '-agr-top-coord-rule := top-coord-rule & \
                  [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].' )
  climb_coord.add(f + '-agr-top-coord-rule := top-coord-rule & \
                  [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].' )

  if mid:
    mylang.add(f + '-agr-mid-coord-rule := mid-coord-rule &\
               [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].')
    climb_coord.add(f + '-agr-mid-coord-rule := mid-coord-rule &\
               [ ' + compl_path + ', \
                  LCOORD-DTR.' + compl_path + ', \
                  RCOORD-DTR.' + compl_path + ' ].')

  mylang.add(f + '-agr-bottom-coord-rule := bottom-coord-phrase &\
             [ ' + compl_path + ', \
               NONCONJ-DTR.' + compl_path + ' ].')
  climb_coord.add(f + '-agr-bottom-coord-rule := bottom-coord-phrase &\
             [ ' + compl_path + ', \
               NONCONJ-DTR.' + compl_path + ' ].')

def add_sharing_supertypes(mylang, climb_coord, pn, mid, agr):
  vp_except = False
  if 'vp' in pn and 'red' in agr:
    vp_except = True
    if agr == 'inv-red':
      mylang.add('no-pass-inv-top-coord := top-coord-rule & \
                   [ SYNSEM.LOCAL.CAT.HEAD.INV -, \
                     LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                     RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
      mylang.add(pn + '-top-coord-rule := no-pass-inv-top-coord.')
      climb_coord.add('no-pass-inv-top-coord := top-coord-rule & \
                   [ SYNSEM.LOCAL.CAT.HEAD.INV -, \
                     LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv, \
                     RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
      climb_coord.add(pn + '-top-coord-rule := no-pass-inv-top-coord.')
  if '-' in agr:
    agr_parts = agr.split('-')
    agr = agr_parts[0]
  if not vp_except:
    mylang.add(pn + '-top-coord-rule :=  ' + agr + '-agr-top-coord-rule.')
    climb_coord.add(pn + '-top-coord-rule :=  ' + agr + '-agr-top-coord-rule.')
  if mid:   
    mylang.add(pn + '-mid-coord-rule := ' + agr + '-agr-mid-coord-rule.')
    climb_coord.add(pn + '-mid-coord-rule := ' + agr + '-agr-mid-coord-rule.')
  mylang.add(pn + '-bottom-coord-rule := ' + agr + '-agr-bottom-coord-rule.')  
  climb_coord.add(pn + '-bottom-coord-rule := ' + agr + '-agr-bottom-coord-rule.')  


def share_wh_properties(mylang, climb_coord, pn, mid):
  path = 'SYNSEM.NON-LOCAL.'
  f = 'que'
  add_shared_features(mylang, climb_coord, f, path, mid)
  add_sharing_supertypes(mylang, climb_coord, pn, mid, f)


def share_rel_properties(mylang, climb_coord, pn, mid):
  path = 'SYNSEM.NON-LOCAL.'
  f = 'rel'
  add_shared_features(mylang, climb_coord, f, path, mid)
  add_sharing_supertypes(mylang, climb_coord, pn, mid, f)
  mylang.add(f + '-agr-top-coord-rule := \
                     [ LCOORD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  climb_coord.add(f + '-agr-top-coord-rule := \
                     [ LCOORD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  if mid:
    mylang.add(f + '-agr-mid-coord-rule := \
                     [ LCOORD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
    climb_coord.add(f + '-agr-mid-coord-rule := \
                     [ LCOORD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  mylang.add(f + '-agr-bottom-coord-rule := \
                     [ NONCONJ-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  climb_coord.add(f + '-agr-bottom-coord-rule := \
                     [ NONCONJ-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')

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

def create_coord_sharing_cat_features(mylang, climb_coord, feat, mid):
  path = 'SYNSEM.LOCAL.CAT.'
  for f in feat:  
    add_shared_features(mylang, climb_coord, f, path, mid)


def add_adposition_rules(mylang, climb_coord):

  ###adp-rules must share their head because of PRD value
  mylang.add('adp-coord-phrase := event-coord-phrase & \
  [ SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                  PRD #prd ], \
    LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                             PRD #prd ], \
    RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                             PRD #prd ] ].')

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

  climb_coord.add('adp-coord-phrase := event-coord-phrase & \
  [ SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                  PRD #prd ], \
    LCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                             PRD #prd ], \
    RCOORD-DTR.SYNSEM.LOCAL.CAT.HEAD adp & [ MOD #mod, \
                                             PRD #prd ] ].')

  climb_coord.add('basic-adp-top-coord-rule := adp-coord-phrase & \
               [ C-CONT [ RELS <! !>, \
	                  HCONS <! !> ]].')

  climb_coord.add('basic-adp-mid-coord-rule := adp-coord-phrase & \
               [ SYNSEM.LOCAL.COORD-REL #crel, \
                 C-CONT [ RELS <! #crel !>, \
	                  HCONS <! !> ] ].')

  climb_coord.add('adp-bottom-coord-phrase := bottom-coord-phrase & \
               [ SYNSEM.LOCAL.CAT.HEAD #head & adp, \
                 NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD #head ].')


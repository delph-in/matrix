from gmcs.linglib import case
from gmcs.linglib import subcategorization

######################################################################
# customize_feature_values(ch_dict, type_name, pos, features, cases)
#   In the passed-in choices dictionary, go through the 'feat'
#   iterator and specify the feature/value pairs found to the
#   passed-in type.

def process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=None, climbfile=None):
  for (ch_key, type_id, pos) in to_cfv:
    customize_feature_values(mylang, ch, hierarchies, ch[ch_key], type_id, pos,
                             tdlfile=tdlfile or mylang, climbfile = climbfile or mylang)

def customize_feature_values(mylang, ch, hierarchies, ch_dict, type_name, pos, features=None, cases=None, tdlfile=None, climbfile=None):

  if not features:
    features = ch.features()
  if not cases:
    cases = case.case_names(ch)
  if not tdlfile:
    tdlfile = mylang
#hack so I won't have to wait testing until all is done
  if not climbfile:
    climbfile = mylang


  pos_geom_prefix = ''
 

  ll_adj = False
  if pos == 'adj':
    if ch.get('2ndll') == 'on':
      for myll in ch.get('ll'):
        if myll.get('phen') == 'adj':
          ll_adj = True
###boolean indicates whether for a lexical item
###subject restrictions must be added through the first 
###element on the subject list
  indirect_subj_mark = False

  if pos == 'det':
    pos_geom_prefix = 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.'
  elif pos == 'con':
    pos_geom_prefix = 'HEAD-DTR.SYNSEM.'
  else:
    pos_geom_prefix = 'SYNSEM.'

  if pos == 'auxcomplement':
    iter_feat = 'compfeature'
  else:
    iter_feat = 'feat'

  for feat in ch_dict.get(iter_feat,[]):
    n = feat.get('name','')
    v = feat.get('value','').split(', ')
    #avoiding features with empty string as value are assigned
    if len(v) == 1 and v[0] == '':
      n = ''


    if n == 'case':
      v = [case.canon_to_abbr(c, cases) for c in v]
      if pos == 'adp': 
        pos_geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'

    geom_prefix = pos_geom_prefix
    # Germanic: Auxrule analysis requires a different prefix
    # No subj list to put feature on...geom_prefix2 used for this purpose
    # This also applies to filler-gap analyses for languages with fixed argument order (i.e. Dutch)
    geom_prefix2 = ''

    # The 'head' choice only appears on verb pcs, and allows the user
    # to specify features on the subject and object as well
    # It can also add mod for adjectives
    h = feat.get('head','')
    if h == 'subj':
      geom_prefix += 'LOCAL.CAT.VAL.SUBJ.FIRST.'
    # setting necessary value in case of 'aux-rule' analysis to geom_prefix2
    # also needed for arg-comp when using similar analysis to aux-rule (more
    # efficient)
    # adapting so that it only applies to agreement features for lexical rules
    # to prevent undesired interactions with copula
    # also needed for auxiliaries in general
      if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2': 
        v_val = ''
        if ch.get('vc-analysis') == 'aux-rule' or (ch.get('v2-analysis') == 'filler-gap' and ch.get('argument-order') == 'fixed'):
          v_val = ch_dict.get('valence')
          if pos == 'aux' or v_val == 'nom-inf' or v_val == 'nom-zuinf':
            indirect_subj_mark = True
        det_type = type_name.split('-')
        if 'rule' in det_type or indirect_subj_mark:
          geom_prefix2 = pos_geom_prefix
          geom_prefix2 += 'LOCAL.CAT.VAL.COMPS.'
          if v_val == 'nom-acc-inf':
            geom_prefix2 += 'REST.'
          geom_prefix2 += 'FIRST.'
    elif h == 'obj':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'
    elif h == 'obj2':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.REST.FIRST.'
    elif h == 'mod':
      if not ll_adj:
        geom_prefix += 'LOCAL.CAT.HEAD.MOD.FIRST.'
      else:
        geom_prefix += 'LOCAL.AGR.'
    elif h == 'higher':
      geom_prefix = 'SC-ARGS.FIRST.'
    elif h == 'lower':
      geom_prefix = 'SC-ARGS.REST.FIRST.'
    elif h == 'rel':
      geom_prefix = 'SYNSEM.NON-LOCAL.REL.LIST.FIRST.'
   # self_rel4: special thing for possessive pronouns
    # feature goes on RELS list.
    # TODO: only works if these features are defined last. That should be fixed
    elif h == 'self_rel4':
      geom_prefix = 'SYNSEM.LOCAL.CONT.RELS \
            <! relation, relation, relation, [ ARG0.'
    if pos == 'auxcomplement':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'

    geom = ''
    for f in features:
      if f[0] == n:
        geom = f[2]
###modifier features marked on adjective and done through agreement
###by rules if grammar intended for language learning
        if (h == 'mod' and ll_adj) or h == 'rel': 
          my_parts = geom.split('.')
          geom = ''
          if h == 'rel':
            geom = my_parts[3] + '.'
          geom += my_parts[4] + "." + my_parts[5]     

# geom2 covers aux-rule case
    geom2 = ''
    if geom:
      geom = geom_prefix + geom
      if geom_prefix2:
        path_parts = geom.split('.')
        ff_num = len(path_parts) - 1
        final_feat = path_parts[ff_num]
        geom2 = geom_prefix2 + 'LOCAL.CONT.HOOK.XARG.PNG.' + final_feat
###separated types only if dealing with lexical rules
###lexical items are already defined as aux or mainverb
        if 'rule' in det_type:
          type_name_1 = type_name + '-main-verb'
          type_name_2 = type_name + '-aux'
          
    # If the feature has a geometry, just specify its value;
    # otherwise, handle it specially.
    # HACK: making sure 'expl' morphological rules have main-verb subtypes...
    if geom:
      if n in hierarchies or (geom2 and 'expl' in det_type):
        if n in hierarchies:
          value = hierarchies[n].get_type_covering(v)
          expl = False
        else:
          value = v[0]
          expl = True
        if geom2:
          if 'rule' in det_type:
            tdlfile.add(type_name_1 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom + ' ' + value + ' ].')
            tdlfile.add(type_name_1 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].', 
                         merge=True)
            climbfile.add(type_name_1 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom + ' ' + value + ' ].')
            climbfile.add(type_name_1 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].', 
                         merge=True)
            if not expl:
              tdlfile.add(type_name_2 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom2 + ' ' + value + ' ].')
              tdlfile.add(type_name_2 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
                         merge=True)
              climbfile.add(type_name_2 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom2 + ' ' + value + ' ].')
              climbfile.add(type_name_2 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
                         merge=True)
####else case, if aux-rule and on lexical item rather than rule
          else:
            tdlfile.add(type_name +
                    ' := [ ' + geom2 + ' ' + value + ' ].',
                    merge=True)
            climbfile.add(type_name +
                    ' := [ ' + geom2 + ' ' + value + ' ].',
                    merge=True)
        else: 
          if h == 'self_rel4':
            suffix = ' ] !> ].'
            g = geom.split('.')
            geom = geom_prefix + g[len(g)-2] + '.' +  g[len(g)-1]
          else:
            suffix = ' ].'
          mytype =  type_name + ' := [ ' + geom + ' ' + value + suffix
          tdlfile.add(mytype,
                      merge=True)  
          climbfile.add(mytype,
                      merge=True)  
        if n == 'case' and ch.has_mixed_case():
          val = '-' if '-synth-' + value in type_name else '+'
          tdlfile.add(type_name +
                      ' := [ ' + geom + '-MARKED ' + val + ' ].',merge=True)
          climbfile.add(type_name +
                      ' := [ ' + geom + '-MARKED ' + val + ' ].',merge=True)
      else:
        for value in v:
          if h == 'self_rel4':
            suffix = ' ] !> ].'
            g = geom.split('.')
            geom = geom_prefix + g[len(g)-2] + '.' +  g[len(g)-1]
          else:
            suffix = ' ].'
          mytype =  type_name + ' := [ ' + geom + ' ' + value + suffix
          tdlfile.add(mytype, merge=True) 
          climbfile.add(mytype, merge=True) 
    elif n == 'argument structure':
      # constrain the ARG-ST to be passed up
      tdlfile.add(type_name + ' := [ ARG-ST #arg-st, DTR.ARG-ST #arg-st ].',
                  merge=True)
      climbfile.add(type_name + ' := [ ARG-ST #arg-st, DTR.ARG-ST #arg-st ].',
                  merge=True)

      # get the feature geometry of CASE
      for f in features:
        if f[0] == 'case':
          geom = f[2]

      for argst in v:
        # specify the subj/comps CASE values
        c = argst.split('-')
        if subcategorization.interpret_verb_valence(argst) == 'tverb':
          # if no case marking specified AVMS are blank
          a_case = o_case = ''
          # otherwise use the geometry and case name
          if len(c) > 1:
            a_case = geom + ' ' + case.canon_to_abbr(c[0], cases)
            o_case = geom + ' ' + case.canon_to_abbr(c[1], cases)
          tdlfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + a_case + '], ' +\
                                     '[ ' + o_case + '] > ].',
                      merge=True)
          climbfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + a_case + '], ' +\
                                     '[ ' + o_case + '] > ].',
                      merge=True)
        else:
          c = c[0]
          s_case = '' if c == 'intrans' \
                      else (geom + ' ' + case.canon_to_abbr(c, cases))
          tdlfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + s_case + '] > ].',
                      merge=True)
          climbfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + s_case + '] > ].',
                      merge=True)

    elif (n == 'negation' and v[0] == 'plus'):
      # ERB 2009-01-22 This is where we deal with the
      # negative affixes.
      # If neg-head-feature is on, then we also mark the verb
      # negated +.
      # and ensure that verbs start out negated -
      if ch.get('neg-head-feature') == 'on':
        tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.NEGATED + ].',merge=True)
        climbfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.NEGATED + ].',merge=True)

      tdlfile.add(type_name + ':= \
                     [ C-CONT [ HOOK [ XARG #xarg,\
	                     LTOP #ltop,\
	                     INDEX #ind ],\
	              RELS <! event-relation &\
	                      [ PRED "neg_rel",\
	                        LBL #ltop,\
	                        ARG1 #harg ] !>,\
	              HCONS <! qeq &\
	                       [ HARG #harg,\
	                         LARG #larg ] !> ],\
	              SYNSEM.LKEYS #lkeys,\
	            DTR [ SYNSEM [ LKEYS #lkeys,\
	                    LOCAL [ CONT.HOOK [ XARG #xarg,\
                                                INDEX #ind,\
	                                        LTOP #larg ],\
	                          CAT.HEAD verb]]]].',
                 'This lexical rule adds the neg_rel to the verb\'s\n\RELS list.',
                  merge=True)
      climbfile.add(type_name + ':= \
                     [ C-CONT [ HOOK [ XARG #xarg,\
	                     LTOP #ltop,\
	                     INDEX #ind ],\
	              RELS <! event-relation &\
	                      [ PRED "neg_rel",\
	                        LBL #ltop,\
	                        ARG1 #harg ] !>,\
	              HCONS <! qeq &\
	                       [ HARG #harg,\
	                         LARG #larg ] !> ],\
	              SYNSEM.LKEYS #lkeys,\
	            DTR [ SYNSEM [ LKEYS #lkeys,\
	                    LOCAL [ CONT.HOOK [ XARG #xarg,\
                                                INDEX #ind,\
	                                        LTOP #larg ],\
	                          CAT.HEAD verb]]]].',
                 'This lexical rule adds the neg_rel to the verb\'s\n\RELS list.',
                  merge=True)
    elif (n == 'requires-neg-adv' and v[0] == 'plus'):
      # this feature on a bound morpheme means that this morpheme is neg1 and 
      # introduces a dependency for neg2 as an independent adverb
      tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.NEG-SATISFIED - ].',merge=True)
      climbfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.NEG-SATISFIED - ].',merge=True)
    elif (n == 'negation' and v[0] == 'minus'):
      # JDC 2011-01-11 Users specify negation minus to indicate that a 
      # lexical type is not compatible with negation
      if ch.get('neg-head-feature') == 'on':
        tdlfile.add(type_name + ':= [ ARGS.FIRST.SYNSEM.LOCAL.CAT.HEAD.NEGATED - ].',merge=True)
        climbfile.add(type_name + ':= [ ARGS.FIRST.SYNSEM.LOCAL.CAT.HEAD.NEGATED - ].',merge=True)

    elif (n == 'question' and v[0] == 'plus'):
      # ERB 2009-07-01 Adding in semantics for question affixes
      tdlfile.add(type_name + ':= \
                     [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques ].',
                  merge=True)
      climbfile.add(type_name + ':= \
                     [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques ].',
                  merge=True)

    ## Specifiying OPT- on each user defined type instead of creating a supertype because
    #It the supertype would need to inherit from transitive-verb-lex and the code already puts
    #transitive-verb-lex as a supertype of user-defined typ thus causing an inheritance issue.
    #elif(n=='OPT' and v[0] == 'plus'):
      # SS 2009-05-26 argument optionality is added to user defined types here
      #if h == 'subj':
      #  tdlfile.add(type_name + ':= subj-drop-verb-lex.', merge = True)
      #if h == 'obj':
      #  tdlfile.add(type_name + ':= obj-drop-verb-lex.', merge = True)

    elif n == 'OPT':
      bool_val = {'plus': '+', 'minus': '-', 'uspec': '#'}[v[0].lower()]
      val_geom = {'subj': 'SUBJ', 'obj': 'COMPS', 'spr': 'SPR'}[h.lower()]
      if bool_val == '#':
        bool_val += h.lower() + '-loc'
        tdlfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.LOCAL %(bv)s,\
                 DTR.SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.LOCAL %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)
###HACK: for now, this is only possible for nouns: share other vals:
        tdlfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                       COMPS #comps, \
                                                       SPEC #spec ], \
                 DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                            COMPS #comps, \
                                            SPEC #spec ] ].' \
                  % {'id': type_name },
                  merge=True)
        climbfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.LOCAL %(bv)s,\
                 DTR.SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.LOCAL %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)
###HACK: for now, this is only possible for nouns: share other vals:
        climbfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                                       COMPS #comps, \
                                                       SPEC #spec ], \
                 DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ #subj, \
                                            COMPS #comps, \
                                            SPEC #spec ] ].' \
                  % {'id': type_name },
                  merge=True)

      else:
        tdlfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.OPT %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)
        climbfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.OPT %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)

    elif n == 'direction':
      if v[0] == 'dir':
        tdlfile.add(type_name + ' := dir-lex-rule.')
        climbfile.add(type_name + ' := dir-lex-rule.')
      else:
        tdlfile.add(type_name + ' := inv-lex-rule.')
        climbfile.add(type_name + ' := inv-lex-rule.')
    elif n == 'dirinv-type':
      d = v[0]
      if h == 'subj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < '+d+' > ].')
        climbfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < '+d+' > ].')
      elif h == 'obj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.COMPS < '+d+' > ].')
        climbfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.COMPS < '+d+' > ].')

# Note: customize case code is now in gmcs/linglib/case.py

# Note: direct inverse code is now in gmcs/linglib/direct_inverse.py

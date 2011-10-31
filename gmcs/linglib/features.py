from gmcs.linglib import case

######################################################################
# customize_feature_values(ch_dict, type_name, pos, features, cases)
#   In the passed-in choices dictionary, go through the 'feat'
#   iterator and specify the feature/value pairs found to the
#   passed-in type.

def process_cfv_list(mylang, ch, hierarchies, to_cfv, tdlfile=None):
  for (ch_key, type_id, pos) in to_cfv:
    customize_feature_values(mylang, ch, hierarchies, ch[ch_key], type_id, pos,
                             tdlfile=tdlfile or mylang)

def customize_feature_values(mylang, ch, hierarchies, ch_dict, type_name, pos, features=None, cases=None, tdlfile=None):

  if not features:
    features = ch.features()
  if not cases:
    cases = case.case_names(ch)
  if not tdlfile:
    tdlfile = mylang

  pos_geom_prefix = ''

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

    if n == 'case':
      v = [case.canon_to_abbr(c, cases) for c in v]

    geom_prefix = pos_geom_prefix
    # Germanic: Auxrule analysis requires a different prefix
    # No subj list to put feature on...geom_prefix2 used for this purpose
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
      if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2':
        geom_prefix2 = pos_geom_prefix
        geom_prefix2 += 'LOCAL.CAT.VAL.COMPS.FIRST.'
    elif h == 'obj':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'
    elif h == 'mod':
      geom_prefix += 'LOCAL.CAT.HEAD.MOD.FIRST.'
    elif h == 'higher':
      geom_prefix = 'SC-ARGS.FIRST.'
    elif h == 'lower':
      geom_prefix = 'SC-ARGS.REST.FIRST.'

    if pos == 'auxcomplement':
      geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'

    geom = ''
    for f in features:
      if f[0] == n:
        geom = f[2]

    # geom2 covers aux-rule case
    geom2 = ''
    if geom:
      geom = geom_prefix + geom
      if geom_prefix2:
        path_parts = geom.split('.')
        ff_num = len(path_parts) - 1
        final_feat = path_parts[ff_num]
        geom2 = geom_prefix2 + 'LOCAL.CONT.HOOK.XARG.PNG.' + final_feat
        type_name_1 = type_name + '-main-verb'
        type_name_2 = type_name + '-aux'

    # If the feature has a geometry, just specify its value;
    # otherwise, handle it specially.
    if geom:
      if n in hierarchies:
        value = hierarchies[n].get_type_covering(v)
        if geom2:
          tdlfile.add(type_name_1 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom + ' ' + value + ' ].')
          tdlfile.add(type_name_1 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].', 
                         merge=True)
          tdlfile.add(type_name_2 +
                      ' := ' + type_name + ' & ' +
                       ' [ ' + geom2 + ' ' + value + ' ].')
          tdlfile.add(type_name_2 +
                      ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
                         merge=True)
        else:  
          tdlfile.add(type_name +
                    ' := [ ' + geom + ' ' + value + ' ].',
                    merge=True)
        if n == 'case' and ch.has_mixed_case():
          val = '-' if '-synth-' + value in type_name else '+'
          tdlfile.add(type_name +
                      ' := [ ' + geom + '-MARKED ' + val + ' ].',
                      merge=True)
      else:
        for value in v:
          tdlfile.add(type_name +
                      ' := [ ' + geom + ' ' + value + ' ].',
                      merge=True)
    elif n == 'argument structure':
      # constrain the ARG-ST to be passed up
      tdlfile.add(type_name + ' := [ ARG-ST #arg-st, DTR.ARG-ST #arg-st ].',
                  merge=True)

      # get the feature geometry of CASE
      for f in features:
        if f[0] == 'case':
          geom = f[2]

      for argst in v:
        # specify the subj/comps CASE values
        c = argst.split('-')
        if case.interpret_verb_valence(argst) == 'tverb':
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
        else:
          c = c[0]
          s_case = '' if c == 'intrans' \
                      else (geom + ' ' + case.canon_to_abbr(c, cases))
          tdlfile.add(type_name + \
                      ' := [ ARG-ST < [ ' + s_case + '] > ].',
                      merge=True)

    elif (n == 'negation' and v[0] == 'plus'):
      # ERB 2009-01-22 This is where we deal with the
      # negative affixes.
      # If neg-head-feature is on, then we also mark the verb
      # negated +.
      if ch.get('neg-head-feature') == 'on':
        tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.NEGATED + ].',merge=True)

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


    elif (n == 'question' and v[0] == 'plus'):
      # ERB 2009-07-01 Adding in semantics for question affixes
      tdlfile.add(type_name + ':= \
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
      bool_val = {'plus': '+', 'minus': '-'}[v[0].lower()]
      val_geom = {'subj': 'SUBJ', 'obj': 'COMPS'}[h.lower()]
      tdlfile.add('%(id)s := [SYNSEM.LOCAL.CAT.VAL.%(vg)s.FIRST.OPT %(bv)s].' \
                  % {'id': type_name, 'vg': val_geom, 'bv': bool_val},
                  merge=True)

    elif n == 'direction':
      if v[0] == 'dir':
        tdlfile.add(type_name + ' := dir-lex-rule.')
      else:
        tdlfile.add(type_name + ' := inv-lex-rule.')
    elif n == 'dirinv-type':
      d = v[0]
      if h == 'subj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < '+d+' > ].')
      elif h == 'obj':
        tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.COMPS < '+d+' > ].')

# Note: customize case code is now in gmcs/linglib/case.py

# Note: direct inverse code is now in gmcs/linglib/direct_inverse.py

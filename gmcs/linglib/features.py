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

    # TJT 2014-11-05: moving this up to get the proper geometry for case
    # get the feature geometry of CASE
    if cases:
        for f in features:
            if f[0] == 'case':
                case_geom = f[2]
                break # Stop looking! TJT 2014-08-27

    # TJT 2014-08-15: changing this to a map for readability/speed
    prefix_map = { 'det': 'SYNSEM.LOCAL.CAT.VAL.SPEC.FIRST.',
                   'con': 'HEAD-DTR.SYNSEM.' }
    pos_geom_prefix = prefix_map[pos] if pos in prefix_map else 'SYNSEM.'

    iter_feat = 'feat' if pos != 'auxcomplement' else 'compfeature'
    basic_infl_neg_def = ''':= \
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
                          CAT.HEAD verb ] ] ] ]. '''

    # TJT Initializing head -> geom_prefix map outside of loop for speed
    # Map from head value to geometry prefix
    head_map = {
        'subj': 'LOCAL.CAT.VAL.SUBJ.FIRST.',
        'obj': 'LOCAL.CAT.VAL.COMPS.FIRST.',
        'higher': 'SC-ARGS.FIRST.',
        'lower': 'SC-ARGS.REST.FIRST.',
        'xarg': 'LOCAL.CONT.HOOK.XARG.',  # XARG for adjectives
        'mod': 'LOCAL.CAT.HEAD.MOD.FIRST.',  # MOD for adjectives
        'comp': 'LOCAL.CAT.VAL.COMPS.FIRST.', # COMP for copulas
    }

    for feat in ch_dict.get(iter_feat,[]):
        n = feat.get('name','')
        v = feat.get('value','').split(', ')

        if n == 'case':
            v = [case.canon_to_abbr(c, cases) for c in v]

        geom_prefix = pos_geom_prefix

        # EKN 2017-01-02 If adding AGREEMENT PNG features to a 
        # possessive marker or affix, they should be at
        # POSS.POSS-AGR, rather than at CONT.HOOK.INDEX.PNG.
        # Also, the PNG features of a possessor pronoun affix
        # should go on the ARG0 of the first thing in the
        # RELS list of the inflectional rule (which corresponds
        # to the pron_rel). The following checks to see if the
        # ch_dict that was passed in corresponds to a possessive
        # lexical rule or a possessor pronoun lexical rule, by
        # iterating through all features in the ch_dict to see
        # if any is named 'poss-stratN' or 'poss-pronN'. If so,
        # it sets a boolean flag (poss_lrt or poss_pron_lrt),
        # which is subsequently used to correctly adjust the 
        # feature geometry.
        poss_lrt=False
        poss_pron_lrt=False
        agreeing_element=''
        for feature in ch_dict.get('feat'):
            feat_name=feature.get('name')
            if 'poss-strat' in feat_name: 
                poss_lrt=True
                agreeing_element=feature.get('value')
            elif 'poss-pron' in feat_name:
                poss_pron_lrt=True
                agreeing_element='possessor'
            elif 'possessum' in feature.full_key:
                agreeing_element='possessum'
            elif 'possessor' in feature.full_key:
                agreeing_element='possessor'
            elif 'poss-pron' in feature.full_key:
                agreeing_element='possessor'      
        if pos=='poss-marker' or poss_lrt:
            if agreeing_element=='possessor':
                geom_prefix = 'SYNSEM.LOCAL.CAT.HEAD.POSSESSOR.POSS-AGR.'
            elif agreeing_element=='possessum':
                geom_prefix = 'SYNSEM.LOCAL.CAT.POSSESSUM.POSS-AGR.'
        if poss_pron_lrt:
            if feat.get('head')=='possessum':
                geom_prefix = 'DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG'
            else:
                geom_prefix = 'C-CONT.RELS <! [ ARG0.PNG'

        # The 'head' choice only appears on verb pcs, and allows the
        # user to specify features on the subject and object as well
        # TJT 2014-08-15: the 'head' choice now also appears on
        # adjectives, copulas, and their lexical rules, allowing
        # the user to specify which argument the adjective or copula
        # is agreeing with
        # EKN 2018-01-06: It now also appears on nouns, to distinguish
        # a noun's inherent features from features that agree with
        # another nominal element in a possessive phrase; however,
        # the head feature for possessive phrases is dealt with above
        head = feat.get('head','')
        if head in head_map: # TJT 2014-08-15: changing this to map for speed/easy reading
            if head in ('higher', 'lower'):
                geom_prefix = head_map[head] # TJT 2014-09-09: Higher/lower replace prefix
            else:
                geom_prefix += head_map[head]
        # TJT 2014-09-16: DTR is for incorporated adjectives
        if head == 'dtr':
            geom_prefix = "DTR." + geom_prefix
        # If auxcomplement, add additional definition on top of any head definition
        if pos == 'auxcomplement':
            geom_prefix += 'LOCAL.CAT.VAL.COMPS.FIRST.'

        # TJT 2014-05-08 adding the break and moving the concatenation up
        geom = ''
        for f in features:
            if f[0] == n:
                value = f[2]
                # If choice defined not to have value, don't define a geometry
                # and therefore skip to n==X specific code
                if value:
                    # TJT 2014-05-08: XARG is of type individual, others are of
                    # type local-min, therefore, strip off the extra path
                    if head == 'xarg':
                        value = value[len("LOCAL.CONT.HOOK.INDEX."):]
                    # TJT 2014-11-05: case on adjectives only works for attributive
                    # constructions and has a different geometry
                    if head in ('xarg','mod') and n == 'case':
                        geom_prefix = 'SYNSEM.LOCAL.CAT.HEAD.MOD.FIRST.LOCAL.CAT.HEAD.CASE'
                    # EKN 2018-1-6: when adding a possessive rule, the standard feature path
                    # won't work. This removes all but the feature name and value from the
                    # variable value.
                    if pos=='poss-marker' or poss_lrt:
                        value=value.replace('LOCAL.CONT.HOOK.INDEX.PNG.','')
                    if poss_pron_lrt:
                        value=value.replace('LOCAL.CONT.HOOK.INDEX.PNG','')
                    geom = geom_prefix + value
#           if head == 'mod':
#               geom += "] >"  # TJT 2014-05-27: close MOD
                break # TJT 2014-05-08 stop looking!

        # If the feature has a geometry, just specify its value;
        # otherwise, handle it specially.
        if geom:
            if n in hierarchies:
                value = hierarchies[n].get_type_covering(v)
                # EKN 2018-01-19: Add the closing brackets for the possessive
                # pronoun's feature path
                if poss_pron_lrt:
                    value=value+'] !>'
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

            for argst in v:
                # specify the subj/comps CASE values
                c = argst.split('-')
                if case.interpret_verb_valence(argst) == 'tverb':
                    # if no case marking specified AVMS are blank
                    a_case = o_case = ''
                    # otherwise use the geometry and case name
                    if len(c) > 1:
                        a_case = case_geom + ' ' + case.canon_to_abbr(c[0], cases)
                        o_case = case_geom + ' ' + case.canon_to_abbr(c[1], cases)
                    tdlfile.add(type_name + \
                                ' := [ ARG-ST < [ ' + a_case + '], ' + \
                                '[ ' + o_case + '] > ].',
                                merge=True)
                else:
                    c = c[0]
                    s_case = '' if c == 'intrans' \
                        else (case_geom + ' ' + case.canon_to_abbr(c, cases))
                    tdlfile.add(type_name + \
                                ' := [ ARG-ST < [ ' + s_case + '] > ].',
                                merge=True)

        # ERB 2009-01-22 This is where we deal with the
        # negative affixes.

        # In the new negation library, the 'meaning' of negation +
        # on a type is a function of the negation strategy chosen
        elif (n == 'negation' and v[0] == 'a'):
            # this is simple infl neg:

            tdlfile.add(type_name + basic_infl_neg_def,
                        'This adds negative semantics to the verb\'s\nRELS list.',
                        merge=True)
            # If neg-head-feature is on, then we also mark the verb
            # negated +.
            # and ensure that verbs start out negated -
            if ch.get('neg-head-feature') == 'on':
                tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.NEGATED + ].',merge=True)

        elif (n == 'negation' and v[0] == 'b'):
            # this is a negation lex rule that also requires negform on its
            # complement, should only attach to aux

            # to make the neg-rule a little more elegant we can define a form-and-cont-change-rule
            # and use it as a supertype for this rule

            tdlfile.add('''val-and-cont-change-lex-rule := same-head-lex-rule & same-hc-light-lex-rule &
                     same-posthead-lex-rule & same-mc-lex-rule & same-ctxt-lex-rule &
                     same-modified-lex-rule & same-light-lex-rule & same-non-local-lex-rule &
                     [ SYNSEM.LOCAL.CAT.VAL [ SPR #spr,
                                              SUBJ #subj,
                                              SPEC #spec ],
                       DTR.SYNSEM.LOCAL.CAT.VAL [ SPR #spr,
                                                  SUBJ #subj,
                                                  SPEC #spec ] ].''',section='addenda')

            tdlfile.add(type_name + basic_infl_neg_def, merge=True)
            # because this is a val changing rule, we need the complement verbs VAL
            # and HOOK to be copied up explicitly
            # also, we specify FORM negform on the complement
            tdlfile.add(type_name + ''':= [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL [ CAT [ VAL #val,
                                                                                     HEAD.FORM negform ],
                                                                               CONT.HOOK #hook ],
                                      DTR.SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL [ CAT.VAL #val,
                                                                                   CONT.HOOK #hook ] ]. ''',
                        merge=True)
            tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
                        merge=True)


        #    elif (n == 'negation' and v[0] == 'c'):
        #   'c'  is deprecated!
        elif (n == 'negation' and v[0] == 'd'):
            # this is a comps changing lex rule which adds a negadv to the
            # comps list

            ## shouldn't have to apply to AUXes
            #  tdlfile.add(type_name + ':= [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].',
            #              merge=True)
            tdlfile.add(type_name + ''':= [
        SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,
                                 SPEC #spec,
                                 SUBJ #subj,
                                 COMPS < canonical-synsem &
                                         [ LOCAL.CAT.HEAD [ NEGATED +,
                                         MOD < [ LOCAL.CONT.HOOK #hook ] > ] ]
                                         . #oldcomps > ] ],
        DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,
                                     SPEC #spec,
                                     SUBJ #subj,
                                     COMPS #oldcomps ],
                           CONT.HOOK #hook ] ].''', merge=True)

        elif (n == 'negation' and v[0] == 'e'):
            #  negation lex rule to set NEG-SAT to -
            tdlfile.add(type_name + ':= [ SYNSEM.NEG-SAT - ].')

        elif (n == 'negation' and v[0] == 'f'):
            # negation lex rule to add negative complement and
            # require that original comps is NEGATED +
            tdlfile.add(type_name + ''':= [ SYNSEM.LOCAL.CAT.VAL [ SPR #spr,
                           SPEC #spec,
                           SUBJ #subj,
                           COMPS < canonical-synsem &
                                   [ LOCAL [ CAT.HEAD [ NEGATED +,
                                                      MOD < [ LOCAL.CONT.HOOK #hook ] > ],
                                              CONT.RELS <! arg1-ev-relation !>   ] ] . [ FIRST [ LOCAL [ CAT [ VAL #v,
                                      HEAD verb & [ NEGATED + ] ],
                                CONT #c ],
                        NON-LOCAL #nl ] ] > ],
    DTR verb-lex &
        [ SYNSEM.LOCAL [ CAT [ VAL [ SPR #spr,
                                     SPEC #spec,
                                     SUBJ #subj,
                                     COMPS.FIRST [ LOCAL [ CAT.VAL #v,
                                                           CONT #c ],
                                                   NON-LOCAL #nl ] ],
                               HEAD.AUX + ],
                         CONT.HOOK #hook ] ] ].''', merge=True)

        elif (n == 'negation' and v[0] == 'g'):
            # negation lex rule to add dummy negative complement and
            # only apply to lexical verbs

            # also, flip negated + value
            tdlfile.add(type_name +''':= [ SYNSEM.LOCAL.CAT [ VAL [ SPR #spr,
                             SPEC #spec,
                             SUBJ #subj,
                             COMPS < canonical-synsem &
                                     [ LOCAL [ CAT.HEAD [ NEGATED +,
                                                        MOD < [ LOCAL.CONT.HOOK #hook ] > ],
                                                CONT.RELS <! !> ] ] . #oldcomps > ],
                       HEAD verb & [ NEGATED +,
                              AUX - ] ],
    DTR verb-lex &
        [ SYNSEM.LOCAL [ CAT [ VAL [ SPR #spr,
                                     SPEC #spec,
                                     SUBJ #subj,
                                     COMPS #oldcomps ],
                               HEAD.AUX - ],
                         CONT.HOOK #hook ] ] ].''', merge=True)

        elif (n == 'negation' and v[0] == 'h'):
            # comps adding neg rule which also sets NEG-SAT -
            tdlfile.add(type_name + ''':= [
        SYNSEM [ NEG-SAT -,
                 LOCAL [ CAT.VAL [ SPR #spr,
                                 SPEC #spec,
                                 SUBJ #subj,
                                 COMPS < canonical-synsem &
                                         [ LOCAL.CAT.HEAD [ NEGATED +,
                                         MOD < [ LOCAL.CONT.HOOK #hook ] > ] ]
                                         . #oldcomps > ] ] ],
        DTR.SYNSEM.LOCAL [ CAT.VAL [ SPR #spr,
                                     SPEC #spec,
                                     SUBJ #subj,
                                     COMPS #oldcomps ],
                           CONT.HOOK #hook ] ].''', merge=True)
        elif (n == 'negation' and v[0] == 'minus'):
            # JDC 2011-01-11 Users specify negation minus to indicate that a
            # lexical type is not compatible with negation
            if ch.get('neg-head-feature') == 'on':
                tdlfile.add(type_name + ':= [ ARGS.FIRST.SYNSEM.LOCAL.CAT.HEAD.NEGATED - ].',merge=True)

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
            geom_map = {'subj': 'SUBJ', 'obj': 'COMPS'}
            if head in geom_map:
                bool_val = {'plus': '+', 'minus': '-'}[v[0].lower()]
                val_geom = geom_map[head.lower()]
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
            if head == 'subj':
                tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < '+d+' > ].')
            elif head == 'obj':
                tdlfile.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.COMPS < '+d+' > ].')

# Note: customize case code is now in gmcs/linglib/case.py

# Note: direct inverse code is now in gmcs/linglib/direct_inverse.py

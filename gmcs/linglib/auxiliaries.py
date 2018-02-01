from gmcs.utils import TDLencode
from gmcs.utils import orth_encode

from gmcs.linglib import case
from gmcs.linglib import features



def set_supertypename(auxcomp):
    if auxcomp == 's':
        supertypename = 's-comp-aux'
    elif auxcomp == 'vp':
        supertypename = 'subj-raise-aux'
    else:
        supertypename = 'arg-comp-aux'
    return supertypename


#class Auxiliary:


#  def __init__(self, aux, ch, mylang, hierarchies):
#    auxcomp = ch.get('aux-comp')
#    set_supertypename(auxcomp)
#    self.arg_val_type = define_arg_str_and_valency(aux, auxcomp, ch, mylang)
#    multiaux = ch.get('multiple-aux')
#    self.auxiliary_type = create_semantics(aux, auxcomp, multiaux, mylang)
#    self.user_aux_type = customize_users_auxtype(aux, ch, mylang, hierarchies) 



def define_arg_str_and_valency(aux, auxcomp, ch, mylang, negaux):
    if negaux:
        norder = ch.get('comp-neg-order-head-comp')
    supertypename = set_supertypename(auxcomp)
    basic_typedef = supertypename + ' := aux-lex & \
                [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                         SPEC < > ] ].'
    mylang.add(basic_typedef)
    if auxcomp == 's':
        if not negaux:
            comp_spec_typedef = supertypename + ' := basic-one-arg & \
                            [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                     COMPS < #comps > ], \
                              ARG-ST < #comps & \
                                         [ LOCAL.CAT [ VAL [ SUBJ < >, \
                                                             COMPS < >, \
                                                             SPR < >, \
                                                             SPEC < > ], \
                                                       HEAD verb ]] > ].'
            if ch.get('multiple-aux') == 'no':
                auxrest_type = supertypename + ' := \
                         [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].'
                mylang.add(auxrest_type)
        else:
            if norder == 'after':
                comp_spec_typedef = supertypename + ' := basic-two-arg & \
                              [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                       COMPS < #comps,\
                                                               #negcomp & \
                                                     [ LOCAL.CAT.HEAD.NEGATED + ] > ], \
                                ARG-ST < #comps & \
                                           [ LOCAL.CAT [ VAL [ SUBJ < >, \
                                                               COMPS < >, \
                                                               SPR < >, \
                                                               SPEC < > ], \
                                                         HEAD verb ]], \
                                          #negcomp  > ].'
                if ch.get('multiple-aux') == 'no':
                    auxrest_type = supertypename + ' := \
                           [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ], [ ] > ].'
                    mylang.add(auxrest_type)
            else: #norder == 'before'
                comp_spec_typedef = supertypename + ''' := basic-two-arg &
                 [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, 
                                          COMPS < #negcomp , #comp > ],
                   ARG-ST < #negcomp &  
                            [ LOCAL.CAT.HEAD.NEGATED + ] ,
                            #comp &
                            [ LOCAL.CAT [ VAL [ SUBJ < >,
                                                COMPS < >,
                                                SPR < >,
                                                SPEC < > ],
                                          HEAD verb ] ] > ].'''
                if ch.get('multiple-aux') == 'no':
                    auxrest_type = supertypename + ' := \
                             [ ARG-ST < [ ] , [ LOCAL.CAT.HEAD.AUX - ] > ].'
                    mylang.add(auxrest_type)

                ###VP and V-compl have more in common

    else:
        if not negaux:
            comp_spec_typedef = supertypename + ' := \
                               [ SYNSEM.LOCAL [ CAT.VAL.SUBJ < #subj >, \
                                                CONT.HOOK.XARG #xarg ], \
                                 ARG-ST < #subj & \
                                          [ LOCAL [ CAT.VAL [ SUBJ < >, \
                                                              SPR < >, \
                                                              SPEC < >, \
                                                              COMPS < > ], \
                                                    CONT.HOOK.INDEX #xarg ] ], \
                                          [ ] > ].'
            if ch.get('multiple-aux') == 'no':
                auxrest_type = supertypename + ' := \
                           [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
                mylang.add(auxrest_type)
        else: #negaux
            comp_spec_typedef = supertypename + ' := \
                               [ SYNSEM.LOCAL [ CAT.VAL.SUBJ < #subj >, \
                                                CONT.HOOK.XARG #xarg ], \
                                 ARG-ST < #subj & \
                                          [ LOCAL [ CAT.VAL [ SUBJ < >, \
                                                              SPR < >, \
                                                              SPEC < >, \
                                                              COMPS < > ], \
                                                    CONT.HOOK.INDEX #xarg ] ], \
                                          [ ], [ ] > ].'
            if ch.get('multiple-aux') == 'no':
                if norder == 'after':
                    auxrest_type = supertypename + ' := \
                             [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ], [ ] > ].'
                else: #norder == 'before'
                    auxrest_type = supertypename + ' := \
                             [ ARG-ST < [ ], [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
                mylang.add(auxrest_type)

        if auxcomp == 'vp':
            if not negaux:
                comp_spec_typedef_2 = supertypename + ' := \
                                            trans-first-arg-raising-lex-item  & \
                     [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
                       ARG-ST < [ ], \
                                #comps & \
                                [ LOCAL.CAT [ VAL [ SUBJ < unexpressed >, \
                                                    COMPS < >, \
                                                    SPR < >, \
                                                    SPEC < > ], \
                                              HEAD verb ]] > ].'
            else: #negaux == True
                if norder=='after':
                    comp_spec_typedef_2 = supertypename + ' := \
                                              basic-three-arg  & \
                       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps, #negcomp >, \
                         ARG-ST < [ LOCAL.CONT.HOOK.INDEX #ind ], \
                                  #comps & \
                                  [ LOCAL [ CAT [ VAL [ SUBJ < unexpressed >, \
                                                      COMPS < >, \
                                                      SPR < >, \
                                                      SPEC < > ], \
                                                  HEAD verb ], \
                                            CONT.HOOK.XARG #ind ] ], \
                                  #negcomp & \
                                  [ LOCAL.CAT.HEAD.NEGATED + ] > ].'
                else: #norder =='before'
                    comp_spec_typedef_2 = supertypename + ' := \
                                              basic-three-arg  & \
                       [ SYNSEM.LOCAL.CAT.VAL.COMPS < #negcomp, #comps >, \
                         ARG-ST < [ LOCAL.CONT.HOOK.INDEX #ind ], \
                                  #negcomp & \
                                  [ LOCAL.CAT.HEAD.NEGATED + ], \
                                  #comps & \
                                  [ LOCAL [ CAT [ VAL [ SUBJ < unexpressed >, \
                                                      COMPS < >, \
                                                      SPR < >, \
                                                      SPEC < > ], \
                                                  HEAD verb ],\
                                            CONT.HOOK.XARG #ind ] ] > ].'
        elif auxcomp == 'v':
            comment = \
                '; Somewhat surprisingly, this inherits from basic-two-arg, so\n' + \
                '; that the non-local features are amalgamated from subj, the\n' + \
                '; lexical verb complement, but not the other complements, if any.'
            mylang.add_literal(comment)
            comp_spec_typedef_2 = supertypename + ' := basic-two-arg & \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps . #vcomps >, \
               ARG-ST < [ ], \
                      #comps & \
                      [ LIGHT +, \
                        LOCAL [ CAT [ VAL [ SUBJ < [ ] >, \
                                            COMPS #vcomps ], \
                                      HEAD verb ], \
                                CONT.HOOK.XARG #xarg ]] > ].'
        mylang.add(comp_spec_typedef_2)
        add_subj_tdl(aux, auxcomp, ch, mylang)

    mylang.add(comp_spec_typedef)

def add_subj_tdl(aux, auxcomp, ch, mylang):
    """
    A function to add subject related tdl to type definition if the complement
    is either a V or a VP.
    Called by def_arg_str_and_valency().
    typename = supertype variable in def_arg_str_and_valency()
    type = current definition of supertype including arg-str()
    """
    typename = set_supertypename(auxcomp)
    subj = aux.get('subj','')
    subjc = aux.get('subj_case','') #TODO: verify _-delimited key
    cases = case.case_names(ch)
    subjcase = case.canon_to_abbr(subjc, cases)

    if subj == 'adp':
        subjtype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].'
    else:
        subjtype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].'
        if subj == 'np-comp-case':
            scasetype = typename + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.CASE #case  ], \
          [ LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE #case ] > ] > ].'
            mylang.add(scasetype)
        elif subj == 'np-aux-case':
            scasetype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].'
            mylang.add(scasetype)

    mylang.add(subjtype)


def create_semantics(sem, aux, auxcomp, mylang, ch, hierarchies, negaux):
    if negaux:
        norder = ch.get('comp-neg-order-head-comp')
    supertypename = set_supertypename(auxcomp)
    evid_present = False
    for feat in aux.get('feat',[]):
        if feat.get('name') == 'evidential':
            evid_present = True
    if sem == 'add-pred' or evid_present:
        auxtypename = supertypename + '-with-pred'
        basic_typedef = auxtypename + ' := ' + supertypename + '.'
        if auxcomp == 'vp':
            if not negaux:
                typedef = auxtypename + ' := norm-sem-lex-item & \
                                          trans-first-arg-raising-lex-item-1 .'
            else: #negaux
                if norder=='after':
                    typedef = auxtypename + ''' := norm-sem-lex-item &
          [ ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ], [ ] >,
            SYNSEM [ LOCAL.CONT.HCONS <! qeq & 
                                        [ HARG #harg,
                                          LARG #larg ] !>,
                     LKEYS.KEYREL event-relation & 
                       [ ARG1 #harg ]]].'''
                else: #norder=='before'
                    typedef = auxtypename + ''' := norm-sem-lex-item &
          [ ARG-ST < [ ], [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] >,
            SYNSEM [ LOCAL.CONT.HCONS <! qeq & 
                                        [ HARG #harg,
                                          LARG #larg ] !>,
                     LKEYS.KEYREL event-relation & 
                       [ ARG1 #harg ]]].'''
        else:
            if auxcomp == 'v':
                arg_def = 'ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] >'
            else: # == 's'
                if not negaux:
                    arg_def =  'ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >'
                else:
                    if norder == 'after':
                        arg_def =  'ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ], [ ] >'
                    else:
                        arg_def =  'ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] >'

            comment = \
                '; Not inheriting from basic-verb-lex, so need to put in\n' + \
                '; event-relation by hand here.'
            mylang.add_literal(comment)

            typedef = auxtypename +  ' := hcons-lex-item & \
               [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq & \
                                                [ HARG #harg, \
                                                  LARG #larg ] !> ], \
                          LKEYS.KEYREL event-relation & \
                                       [ ARG1 #harg ]],' + arg_def + ' ].'

        mylang.add(basic_typedef)
        mylang.add(typedef)
    else:
        auxtypename = supertypename + '-no-pred'
        typedef = auxtypename + ' := ' + supertypename + ' & raise-sem-lex-item.'

        if auxcomp == 'v':

            comment = \
                '; Note that raise-sem-lex-item assumes the first complement is\n' + \
                '; where the HOOK comes from.  It\'s not clear to me how you\'d\n' + \
                '; tell that you had an argument composition auxiliary if it\n' + \
                '; wasn\'t appearing adjacent to the verb.'
            mylang.add_literal(comment)
        #### Restriction to stop semantically empty auxiliaries from spinning
        #### Check multiple aux: no need to add constraint when already present on
        #### supertype


        if not ch.get('multiple-aux') == 'no':
            comment = \
                '; To keep the semantically empty ones from spinning on\n' + \
                '; generation, require complement to be [AUX -].  The\n' + \
                '; FORM feature might be enough in the starter grammars,\n' + \
                '; but I don\'t want to rely on this.  Then again, [ AUX - ]\n' + \
                '; might not be true.'
            mylang.add_literal(comment)
            if auxcomp == 's':
                if not negaux:
                    arg_str = '< [ LOCAL.CAT.HEAD.AUX - ] >'
                else:
                    if norder == 'after':
                        arg_str = '< [ LOCAL.CAT.HEAD.AUX - ], [ ] >'
                    else:
                        arg_str = '< [ ], [ LOCAL.CAT.HEAD.AUX - ] >'
            else:
                arg_str = '< [ ], [ LOCAL.CAT.HEAD.AUX - ] >'


            auxres_type = auxtypename + ' := [ ARG-ST ' + arg_str + ' ].'
            mylang.add(auxres_type)

        mylang.add(typedef)

    customize_users_auxtype(auxtypename, aux, ch, mylang, hierarchies)


def customize_users_auxtype(auxtypename, aux, ch, mylang, hierarchies):
    """
    A utility that declares the userstype as subtype of supertype and
    calls the functions that specify feature values on the type.
    Called by customize_auxiliaries.
    userstype = userstypename in customize_auxiliaries
    supertype = userstypename in customize_auxiliaries
    """
    auxcomp = ch.get('aux-comp')
    userstypename = get_users_type_name(aux)
    features.customize_feature_values(mylang, ch, hierarchies, aux, userstypename, 'aux')
    features.customize_feature_values(mylang, ch, hierarchies, aux, userstypename, 'auxcomplement')
    mylang.add(userstypename + ':= ' + auxtypename + '.')
    if ch.get('word-order') == 'v2' and ch.get('subord-word-order') == 'vfinal':
        mylang.add(userstypename + ':='+ auxtypename
                   + '& [ SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.MC na-or-- ].')


def get_users_type_name(aux):
    name = aux.get('name', '')
    userstypename = name + '-aux-lex'
    return userstypename

def add_auxiliaries_to_lexicon(userstypename, sem, aux, lexicon, trigger):
    for stem in aux.get('stem',[]):
        orth = orth_encode(stem.get('orth'))
        id = stem.get('name')
        typedef = TDLencode(id) + ' := ' + userstypename + ' & \
                       [ STEM < "' + orth + '" > ].'
        lexicon.add(typedef)


        evid_present = False
        evid_value = None
        for feat in aux.get('feat',[]):
            if feat.get('name') == 'evidential':
                evid_present = True
                evid_value = feat.get('value')
        if sem == 'add-pred' or evid_present:
            pred = 'ev_' + str(evid_value) + '_rel'
            if not evid_present:
                pred = stem.get('pred')
            typedef = TDLencode(id) + \
                      ' := [ SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
            lexicon.add(typedef, merge=True)
        else:
            tense = aspect = mood = evidential = ''

            for feat in aux.get('feat',[]):
                if feat.get('name') == 'tense':
                    tense = feat.get('value')
                if feat.get('name') == 'aspect':
                    aspect = feat.get('value')
                if feat.get('name') == 'mood':
                    mood = feat.get('value')
                if feat.get('name') == 'evidential':
                    evidential = feat.get('value')

            grdef = TDLencode(id) +'_gr := arg0e_gtr & \
                    [ CONTEXT [ RELS <! [ '

            if tense == '' and aspect == '' and mood == '':
                grdef += 'PRED "non_existing_rel" ] !> ],'
            else:
                grdef += 'ARG0.E [ '
                if tense != '':
                    grdef += 'TENSE ' + tense + ','
                if aspect != '':
                    grdef += 'ASPECT ' + aspect + ','
                if mood != '':
                    grdef += 'MOOD ' + mood + ','

                grdef = grdef[:len(grdef)-1] + ' ] ] !> ], '

            grdef += 'FLAGS.TRIGGER "' + TDLencode(id) + '" ].'
            trigger.add(grdef)


def customize_auxiliaries(mylang, ch, lexicon, trigger, hierarchies):

    lexicon.add_literal(';;; Auxiliaries')
    for aux in ch.get('aux',[]):

        # negaux is a parameter to create auxiliries with extra
        # syntactic arguments in the head-comp neg bipartite construction
        negaux = False
        if ch.get('bineg-type') == 'head-comp':
            negaux = True if  'negation' in [f['name'] for f in aux['feat']] else False
        auxcomp = ch.get('aux-comp')
        userstypename = get_users_type_name(aux)

        # need to add a feature here for 'head-mod-neg' analysis
        if ch.get('bineg-type') == 'head-mod':
            if  'negation' in [f['name'] for f in aux['feat']]:
                mylang.add(userstypename + ':= [SYNSEM.NEG-SAT -].')
        sem = aux.get('sem', '')

        define_arg_str_and_valency(aux, auxcomp, ch, mylang, negaux)
        create_semantics(sem, aux, auxcomp, mylang, ch, hierarchies, negaux)
        add_auxiliaries_to_lexicon(userstypename, sem, aux, lexicon, trigger)





    ####Customize Auxiliaries old

    #def add_subj_tdl(type, subj, subjcase, mylang):
    #  """
    #  A function to add subject related tdl to type definition if the complement
    #  is either a V or a VP.
    #  Called by customize_auxiliaries().
    #  type = supertype variable in customize_auxiliaries()
    #  subj = subj variable in customize_auxiliaries()
    #  """
    #  if subj == 'adp':
    #    mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].')
    #  else:
    #    mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].')
    #    if subj == 'np-comp-case':
    #      mylang.add(type + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.CASE #case  ], \
    #                                        [ LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE #case ] > ] > ].')
    #    elif subj == 'np-aux-case':
    #      mylang.add(type + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].')






    #def customize_auxiliaries(mylang, ch, lexicon, hierarchies):

    #  lexicon.add_literal(';;; Auxiliaries')
    #  auxcomp = ch.get('aux-comp')
    #  wo = ch.get('word-order')
    #  auxorder = ch.get('aux-comp-order')
    #  vc = determine_vcluster(auxcomp, auxorder, wo, ch)#ch.get('v-cluster')

    # for aux in ch.get('aux',[]):
    #   name = aux.get('name','')
    #   userstypename = name + '-aux-lex'
    #   sem = aux.get('sem','')
    #   subj = aux.get('subj','')
    #   subjc = aux.get('subj_case','') #TODO: verify _-delimited key
    #   cases = case.case_names(ch)
    #   subjcase = case.canon_to_abbr(subjc, cases)

    # Lexical type for auxiliaries.
    # ASF 2008-11-25 added constraints SS.LOC.CONT.HOOK.XARG #xarg and
    # ARG-ST.FIRST.LOCAL.CONT.HOOK.INDEX #xarg for subj-raise-aux and
    # arg-comp-aux, to insure the subject is passed up when several auxs are
    # present in sentence. Implemented here, because it required least changes
    # it may be cleaner to have this in general verb-lex, as well as the first
    # ARG is #subj constraint (but not possible for aux with s-comp)

    #  if auxcomp == 'vp':
    #     supertype = 'subj-raise-aux'
    #     auxtypename = get_auxtypename(sem, supertype)

    #      typedef = supertype + ' := aux-lex & trans-first-arg-raising-lex-item  & \
    #                 [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #subj >, \
    #                                          COMPS < #comps >, \
    #                                          SPR < >, \
    #                                          SPEC < > ], \
    #                                  CONT.HOOK.XARG #xarg ], \
    #                   ARG-ST < #subj & \
    #                            [ LOCAL [ CAT.VAL [ SPR < >, \
    #                                                COMPS < > ],\
    #                                      CONT.HOOK.INDEX #xarg ] ], \
    #                            #comps & \
    #                            [ LOCAL.CAT [ VAL [ SUBJ < [ ] >, \
    #                                                COMPS < > ], \
    #                                          HEAD verb ]] > ].'
    #    mylang.add(typedef)
    #    add_subj_tdl(supertype, subj, subjcase, mylang)

    # ASF 2009-12-21 Changing conditions, we now have a question on whether
    # there can be more than on auxiliary per clause, this holds for all complements

    #      if ch.get('multiple-aux') == 'no':
    #        mylang.add(supertype + ' := [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].')
    #    if sem == 'add-pred':
    #      typedef = auxtypename + ' := ' + supertype + ' & norm-sem-lex-item & \
    #                                      trans-first-arg-raising-lex-item-1 .'
    #      mylang.add(typedef)

    ####CHECKED
    #   else:
    #     comment = \
    #       '; To keep the semantically empty ones from spinning on\n' + \
    #       '; generation, require complement to be [AUX -].  The\n' + \
    #       '; FORM feature might be enough in the starter grammars,\n' + \
    #       '; but I don\'t want to rely on this.  Then again, [ AUX - ]\n' + \
    #       '; might not be true.  Be sure to put in a comment.'
    #     mylang.add_literal(comment)
    # changed inheritance here to remove redundancy
    #     typedef = auxtypename + ' := ' + supertype + ' & raise-sem-lex-item & \
    #                   [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
    #     mylang.add(typedef)

    #   customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

    # elif auxcomp == 'v':
    #      supertype = 'arg-comp-aux'
    #      auxtypename = get_auxtypename(sem, supertype)
    ####CHECKED
    #   comment = \
    #     '; Somewhat surprisingly, this inherits from basic-two-arg, so\n' + \
    #     '; that the non-local features are amalgamated from subj, the\n' + \
    #     '; lexical verb complement, but not the other complements, if any.'
    #   mylang.add_literal(comment)

    #   typedef = supertype + ' := aux-lex & basic-two-arg & \
    #          [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #subj  >, \
    #                                   COMPS < #comps . #vcomps >, \
    #                                   SPR < >, \
    #                                   SPEC < > ], \
    #                           CONT.HOOK.XARG #xarg ], \
    #            ARG-ST < #subj & \
    #                     [ LOCAL [ CAT [ VAL [ SPR < >, \
    #                                           COMPS < > ]], \
    #                               CONT.HOOK.INDEX #xarg ]], \
    #                   #comps & \
    #                   [ LIGHT +, \
    #                     LOCAL [ CAT [ VAL [ SUBJ < [ ] >, \
    #                                         COMPS #vcomps ], \
    #                                   HEAD verb ], \
    #                             CONT.HOOK.XARG #xarg ]] > ].'
    #   mylang.add(typedef)
    #   add_subj_tdl(supertype, subj, subjcase, mylang)

    # ASF 2008-12-07 For now we restrict free word order with v-comp to
    # either verbal clusters or one auxiliary max
    # ASF 2009-12-21 Changing conditions, we now have a question on whether
    # there can be more than on auxiliary per clause, this holds for all complements

    #      if ch.get('multiple-aux') == 'no':
    #        mylang.add(supertype + ' := [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].')
    ###CHECKED
    #   if sem == 'add-pred':
    #     comment = \
    #       '; Not inheriting from basic-verb-lex, so need to put in\n' + \
    #       '; event-relation by hand here.'
    #     mylang.add_literal(comment)

    #        typedef = auxtypename + ' := ' + supertype + ' & hcons-lex-item & \
    #               [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq & \
    #                                                [ HARG #harg, \
    #                                                  LARG #larg ] !> ], \
    #                          LKEYS.KEYREL event-relation & \
    #                                       [ ARG1 #harg ]], \
    #                 ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] > ].'
    #        mylang.add(typedef)

    #    else:
    ###CHECKING WRONG PLACE SHOULD NOT BE DEPENDING ON MULTIPLE AUX
    #      comment = \
    #        '; Note that raise-sem-lex-item assumes the first complement is\n' + \
    #        '; where the HOOK comes from.  It\'s not clear to me how you\'d\n' + \
    #        '; tell that you had an argument composition auxiliary if it\n' + \
    #        '; wasn\'t appearing adjacent to the verb.'
    #      mylang.add_literal(comment)

    #        typedef = auxtypename + ' := ' + supertype + '  & raise-sem-lex-item & \
    #               [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
    #        mylang.add(typedef)

    #  customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

    #  elif auxcomp == 's':
    #      supertype = 's-comp-aux'
    #      auxtypename = get_auxtypename(sem, supertype)

    #    typedef = supertype + ' := aux-lex & basic-one-arg & \
    #           [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
    #                                    COMPS < #comps >, \
    #                                    SPR < >, \
    #                                    SPEC < > ], \
    #             ARG-ST < #comps & \
    #                      [ LOCAL.CAT [ VAL [ SUBJ < >, \
    #                                          COMPS < > ], \
    #                                    HEAD verb ]] > ].'
    #  mylang.add(typedef)

    # ASF 2009-12-21 Changing conditions, we now have a question on whether
    # there can be more than on auxiliary per clause, this holds for all complements

    #      if ch.get('multiple-aux') == 'no':
    #        mylang.add(supertype + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].')

    #     if sem == 'add-pred':
    #       mylang.add_literal('; S comp aux, with pred')

    #       typedef = auxtypename + ' := ' + supertype + ' & hcons-lex-item & \
    #               [ SYNSEM [ LOCAL.CONT.HCONS <! qeq & \
    #                                              [ HARG #harg, \
    #                                                LARG #larg ] !>, \
    #                          LKEYS.KEYREL event-relation & \
    #                                       [ ARG1 #harg ]], \
    #                 ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] > ].'
    #       mylang.add(typedef)

    #     else:
    #       mylang.add_literal('; S comp aux, no predicate')

    # LAP 2008-06-11 FIX this: literals may print more than once
    #   here and elsewhere in the loop
    ###CHECKED
    #     comment = \
    #         '; Better say [ AUX - ] on complement here, or we\'ll spin' + \
    #         ' on generation.'
    #     mylang.add_literal(comment)

    #       typedef = auxtypename + ' := ' + supertype + \
    #              ' & raise-sem-lex-item & \
    #              [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].'
    #       mylang.add(typedef)

    # customize_users_auxtype(aux, userstypename, auxtypename, mylang, ch, hierarchies)

    # add stems to lexicon
#    for stem in aux.get('stem',[]):
#      orth = stem.get('orth')
#      typedef = TDLencode(orth) + ' := ' + userstypename + ' & \
#                       [ STEM < "' + orth + '" > ].'
#      lexicon.add(typedef)

#      if sem == 'add-pred':
#        pred = stem.get('pred')
#        typedef = TDLencode(orth) + \
#                    ' := [ SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
#        lexicon.add(typedef, merge=True)


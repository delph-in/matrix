from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

######################################################################
# Coordination
#   Create the type definitions associated with the user's choices
#   about coordination.

######################################################################
# define_coord_strat: a utility function, defines a strategy

def define_coord_strat(num, pos, top, mid, bot, left, pre, suf, mylang,
                       rules, irules, resrules, mixed_strat=False):
    mylang.add_literal(';;; Coordination Strategy ' + num)

    pn = pos + num
    if pos == 'n' or pos == 'np':
        headtype = 'noun'
    else:
        headtype = 'verb'

    passup = ' pass-up-png-coord-rule &' if resrules != [('','')] else ''

    # First define the rules in mylang.  Every strategy has a
    # top rule and a bottom rule, but only some have a mid rule, so if
    # the mid prefix argument $mid is empty, don't emit a rule.
    # Similarly, not all strategies have a left rule.
    for nm, st in resrules:
        mylang.add(pn + nm + '-top-coord-rule :=\
                 basic-' + pos + '-top-coord-rule &\
                 ' + top + 'top-coord-rule &\
                 ' + st + \
                   coord_strat_features(num, nm, mixed_strat))
        if mid:
            mylang.add(pn + nm + '-mid-coord-rule :=\
                   basic-' + pos + '-mid-coord-rule &\
                   ' + mid + 'mid-coord-rule &\
                   ' + st + \
                       coord_strat_features(num, nm, mixed_strat))

    if pre or suf:
        # first the rule in mylang
        mylang.add(pn + '-bottom-coord-rule :=\
               ' + bot + 'bottom-coord-rule &' + passup + '\
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
           ' + pos + '-bottom-coord-phrase &' + passup + '\
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
           ' + pos + '-bottom-coord-phrase &' + passup + '\
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


def coord_strat_features(num, nm, mixed_strat):
    """Gets either just the COORD-STRAT number, or also the COORDAGR value,
     which is only used in a mixed strategy language"""
    if mixed_strat and 'conjunct' not in nm:
        return '[ SYNSEM.LOCAL [ COORDAGR res, ' \
               'COORD-STRAT "' + num + '" ] ].'
    else:
        return '[ SYNSEM.LOCAL.COORD-STRAT "' + num + '" ].'

def customize_feature_resolution(mylang, ch, ap):

    mylang.add_literal(';;; Feature Resolution Rules')

    if ap.get('feat'):
        for feat in ap.get('feat'):
            # figure some stuff out about the feature
            v = feat.get('name')

            featname = 'GEND' if v == 'gender' \
                else 'PER' if v == 'person' \
                else 'NUM' if v == 'number' \
                else 'PERNUM' if v == 'pernum' \
                else 'CASE' if v == 'case' \
                else v.upper()

            path = 'SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG.'  # this is default path, but it might change for custom features, also for case

            # if this is a custom feature, check whether it is semantic or syntactic
            if v.upper() == featname and (v != 'case' and v!= 'pernum'):
                for feature in ch.get('feature', []):  # find the right custom feature in the list...
                    feat = feature.get('name', '')
                    type = feature.get('type', '')  # ...and check the type
                    if feat == v:
                        if type == 'head':
                            path = 'SYNSEM.LOCAL.CAT.HEAD.'

            if v == 'case':
                path = 'SYNSEM.LOCAL.CAT.HEAD.' # special path for case
                mylang.add('bare-np-phrase := [ SYNSEM.LOCAL.CAT.HEAD.CASE #case,'  # TODO is there any reason I can't use this for fr as well?
                           'HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')
                mylang.add('pass-up-png-coord-rule := bottom-coord-phrase & \
              [  SYNSEM.LOCAL.CAT.HEAD.CASE #case,'
                           'NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.CASE #case ].')

            # now go through the rules and add them to the grammar
            for rule in feat.get('rule'):
                ch1 = rule.get('left') if rule.get('left') else 'any'
                ch2 = rule.get('right') if rule.get('right') else 'any'
                par = rule.get('par') if rule.get('par') else 'any' # the rule should always have a parent, but just in case

                if "," in ch1:
                    ch1_list = ch1.split(", ")
                    if ch2 == "nonmatching":
                        for ch1 in ch1_list:
                            for ch2 in ch1_list:
                                if ch1 != ch2:
                                    write_coord_rule(ch1, ch2, par, path, featname, mylang)
                    else: # ch1 is a list but ch2 is some more normal value
                        for ch1 in ch1_list:
                            write_coord_rule(ch1, ch2, par, path, featname, mylang)
                else: # ch1 is not a list
                    write_coord_rule(ch1, ch2, par, path, featname, mylang)



def write_coord_rule(ch1, ch2, par, path, featname, mylang):
    # now, write the phrase rule that corresponds to the ch1, ch2, par given in the choices file.

    tn = ch1 + '-' + ch2 + '-' + par + '-' + featname.lower() + '-coord-rule:= coord-phrase &'

    # the "same" value means we should identify whichever values are indicated to be "the same".
    par = "#" + featname.lower() if par == "same" else par  # TODO I don't like how clumsy this is
    ch1 = "#" + featname.lower() if ch1 == "same" else ch1
    ch2 = "#" + featname.lower() if ch2 == "same" else ch2

    if (ch1 == 'any' and ch2 == 'any' and par != 'any'):  # if both children are 'any', we just constrain the parent.
        tn += ' [ ' + path + featname + ' ' + par + '].'
    elif par == 'any':
        tn += ' [ '
    else:  # otherwise we add LCOORD and RCOORD constraints.
        tn += ' [ ' + path + featname + ' ' + par + ','

    punct = ',' if ch2 != 'any' else '].'

    if ch1 != 'any':
        tn += 'LCOORD-DTR.' + path + featname + ' ' + ch1 + punct
    if ch2 != 'any':
        tn += 'RCOORD-DTR.' + path + featname + ' ' + ch2 + '].'

    if tn:
        mylang.add(tn)


def get_feature_resolution_names(ap):
    resrules = []
    if ap.get('feat'):
        for feat in ap.get('feat'):
            v = feat.get('name')

            featname = 'gend' if v == 'gender' \
                else 'per' if v == 'person' \
                else 'num' if v == 'number' \
                else 'pernum' if v == 'pernum' \
                else v.lower()

            templist = []
            for rule in feat.get('rule'):
                ch1 = rule.get('left') if rule.get('left') else 'any'
                ch2 = rule.get('right') if rule.get('right') else 'any'
                par = rule.get('par') if rule.get('par') else 'any'

                # if ch1 or ch2 is "same", that's fine, no changes needed.

                if "," in ch1:
                    ch1_list = ch1.split(", ")
                    if ch2 == "nonmatching":
                        for ch1 in ch1_list:
                            for ch2 in ch1_list:
                                if ch1 != ch2:
                                    nm = '-' + ch1 + '-' + ch2 + '-' + par
                                    st = ch1 + '-' + ch2 + '-' + par + '-' + featname + '-coord-rule & '
                                    templist += [(nm, st)]
                    else:  # ch1 is a list but ch2 is some more normal value
                        for ch1 in ch1_list:
                            nm = '-' + ch1 + '-' + ch2 + '-' + par
                            st = ch1 + '-' + ch2 + '-' + par + '-' + featname + '-coord-rule & '
                            templist += [(nm, st)]
                else:  # ch1 is not a list
                    nm = '-' + ch1 + '-' + ch2 + '-' + par
                    st = ch1 + '-' + ch2 + '-' + par + '-' + featname + '-coord-rule & '
                    templist += [(nm, st)]



            # add the rules to resrules
            newlist = []
            if resrules:  # if we have rules already, iterate through them and combine with all the new ones
                for rule in resrules:
                    for temp in templist:
                        newlist += [(rule[0] + temp[0], rule[1] + temp[1])]
                resrules = newlist
            else:
                resrules = templist
        return resrules


def customize_conj_wo(mylang, ch, agr, csap):
    # get some values for the pattern we're modeling
    subj_on = True if (csap.get('target') == 'all' or csap.get('target') == 'subj') else False
    obj_on = True if (csap.get('target') == 'all' or csap.get('target') == 'obj') else False

    # if we ever want to handle multiple types (first conjunct, closest conjunct) of distinguished conjunct in a subject
    # or object, "before" and "after" should be sets, and you might need to use subtypes of phrase rules that inherit
    # from the normal phrase rule. (e.g. right-conjunct-head-subj-rule := head-subj-rule & ....)

    # "before" is the side (l/r) that the verb agrees with if the consistutent is before the verb. "After" is similar.
    if agr.get('order') == 'closest':
        before = 'r'
        after = 'l'
    elif agr.get('order') == 'first':
        before = 'l'
        after = 'l'
    elif agr.get('order') == 'last':
        before = 'r'
        after = 'r'

    # it looks like this type addendum is all we'll need for head-mod
    mylang.add('head-mod-phrase :+\
              [ SYNSEM.LOCAL.COORDAGR #cagr,\
                HEAD-DTR.SYNSEM.LOCAL.COORDAGR #cagr ].', '', section='addenda')

    # head-spec

    if ch.get('has-dets') == 'yes':
        if ch.get('noun-det-order') == 'noun-det':
            mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR ' + after + '].')
        if ch.get('noun-det-order') == 'det-noun':
            mylang.add('head-spec-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR ' + before + '].')

    customize_coordagr_word_order(ch, mylang, before, after, subj_on, obj_on)

def customize_coordagr_word_order(ch, mylang, before, after, subj_on, obj_on):
    # now, figure out which word order phrase rules we're modifying.
    wo = ch.get('word-order')

    hc = ''
    hs = ''

    # Head-comp order
    if obj_on == True:
        if wo == 'sov' or wo == 'osv' or wo == 'ovs' or wo == 'v-final':
            hc = 'comp-head'
            mylang.add(hc + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ before + '].')

        if wo == 'svo' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
            hc = 'head-comp'
            mylang.add(hc + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ after + '].')

    # Head-subj order

    if subj_on == True:
        if wo == 'osv' or wo == 'sov' or wo == 'svo' or wo == 'v-final':
            hs = 'subj-head'
            mylang.add(hs + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ before + '].')

        if wo == 'ovs' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
            hs = 'head-subj'
            mylang.add(hs + '-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ after + '].')

    if wo == 'free' or wo == 'v2':
        if subj_on == True:
            mylang.add('head-subj-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ after + '].')
            mylang.add('subj-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ before + '].')
        if obj_on == True:
            mylang.add('head-comp-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ after + '].')
            mylang.add('comp-head-phrase := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ before + '].')
            mylang.add('head-comp-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ after + '].')
            mylang.add('comp-head-phrase-2 := [ NON-HEAD-DTR.SYNSEM.LOCAL.COORDAGR '+ before + '].')

def customize_mixed_strat_resolution(ch, mylang, target):
    subj_on = True if target == 'subj' else False
    obj_on = True if target == 'obj' else False

    # the word order module is directional to handle the r/l orders of closest conjunct
    # but we only need to add `res` for resolution here. It's kind of a hack.
    before = 'res'
    after = 'res'

    customize_coordagr_word_order(ch, mylang, before, after, subj_on, obj_on)

def customize_conjunct_agreement(mylang, ch, agr, csap, cs):
    """add everything to the grammar that's needed for distinguished conjunct agreement"""
    # type addendum for distinguished conjunct
    mylang.add('local-min:+ [COORDAGR dir].', section='addenda')

    # define the COORDAGR feature
    mylang.set_section('features')
    mylang.add_literal(';;; Distinguished Conjunct Directon')
    mylang.add('dir := *top*.', '', True)
    mylang.add('l := dir.', '', True)
    mylang.add('r := dir.', '', True)

    # do we have a mixed strategy language? if so, what's the target on the fr pattern?
    for csap in cs.get('csap'):
        if csap.get('pat').startswith('fr'):
            mylang.add('res := dir.', '', True)
            mixed_strat_target = csap.get('target')
            customize_mixed_strat_resolution(ch, mylang, mixed_strat_target)

    mylang.set_section('phrases')

    mylang.add('bare-np-phrase := [ SYNSEM.LOCAL.COORDAGR #cagr,'
               'HEAD-DTR.SYNSEM.LOCAL.COORDAGR #cagr ].')

    mylang.add('pass-up-png-coord-rule := [ SYNSEM.LOCAL.COORDAGR #cagr,'
               'NONCONJ-DTR.SYNSEM.LOCAL.COORDAGR #cagr ].')


    # the coordination section rules
    mylang.set_section('coord')
    mylang.add_literal(';;; Distinguished Conjunct Rules')

    top_and_mid_rules = []

    mylang.add('same-coordagr-rule := coord-phrase &\
                [ SYNSEM.LOCAL.COORDAGR #cagr,\
                  RCOORD-DTR.SYNSEM.LOCAL.COORDAGR #cagr,\
                  LCOORD-DTR.SYNSEM.LOCAL.COORDAGR #cagr ].')

    if agr.get('order') == 'closest' or 'last':
        top_and_mid_rules += [('-right-conjunct', 'right-conjunct-coord-rule &')]
        mylang.add('right-conjunct-coord-rule := same-coordagr-rule &\
                  [ SYNSEM.LOCAL [ COORDAGR r,\
                                   CONT.HOOK.INDEX.PNG #png ],\
                    RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ].')

    if agr.get('order') == 'closest' or 'first':
        top_and_mid_rules += [('-left-conjunct', 'left-conjunct-coord-rule &')]
        mylang.add('left-conjunct-coord-rule := same-coordagr-rule &\
                  [ SYNSEM.LOCAL [ COORDAGR l,\
                                   CONT.HOOK.INDEX.PNG #png ],\
                    LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ].')

    #now the word order stuff
    customize_conj_wo(mylang, ch, agr, csap)
    return top_and_mid_rules

def customize_agreement_pattern(mylang, ch, csap, cs):
    rules = [('', '')]

    agr = ch.get(csap.get('pat'))  # agr has "name," "order," "full_key" for dconj, or has feature rules for resolution

    # for feature resolution, we create the parent rules and then create a list of rule names.
    if agr.full_key.startswith('fr'):
        customize_feature_resolution(mylang, ch, agr)
        rules = get_feature_resolution_names(agr)
    # for dist. conjunct, we can create the parent rules and just get the rule names at the same time.
    elif agr.full_key.startswith('dconj'):
        rules = customize_conjunct_agreement(mylang, ch, agr, csap, cs)

    mylang.add('bare-np-phrase := [ SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png,'  # TODO is there any reason I can't use this for fr as well?
               'HEAD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ].')
    mylang.add('pass-up-png-coord-rule := bottom-coord-phrase & \
        [SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png,\
        NONCONJ-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX.PNG #png ].')

    return rules


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

        # If this CS uses feature resolution, we will set up closest conjunct as a mixed strategy
        # (with additional rules and specifications).
        mixed_strategy = True if any([csap.get('pat').startswith('fr') for csap in cs.get('csap')]) \
                                 and any([csap.get('pat').startswith('dconj') for csap in cs.get('csap')]) \
            else False

        # make a list of the agreement rules for this coord strat
        if cs.get('csap'):
            agrrules = []
            for csap in cs.get('csap'):  # iterate through the agreement patterns used
                agrrules += customize_agreement_pattern(mylang, ch, csap, cs)
        # use an empty list if no agreement pattern is defined
        else:
            agrrules = [('', '')]


        for pos in ('n', 'np', 'vp', 's'):
            # don't use noun-y agreement rules with VP or S
            agrrules = [('', '')] if pos in ('vp', 's') else agrrules
            # now write the rules
            if cs.get(pos):
                define_coord_strat(csnum, pos, top, mid, bot, left, pre, suf, mylang,
                                   rules, irules, agrrules, mixed_strategy)

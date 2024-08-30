from gmcs.utils import get_name
from gmcs.linglib import lexbase

# Constants for lexical rules

ANC_LEX_RULE = 'anc-lex-rule := cat-change-with-ccont-lex-rule & same-non-local-lex-rule & \
  [ SYNSEM.LOCAL.CAT [ HEAD noun &\
                            [ NMZ +,\
                              MOD #mod ],\
                       VAL [ SPEC #spec ],\
                       MKG #mkg,\
                       HC-LIGHT #hc-light,\
                       POSTHEAD #posthead ],\
    DTR.SYNSEM.LOCAL [ CAT [ HEAD verb & [MOD #mod],\
                             VAL [ SPEC #spec ],\
                             MKG #mkg,\
                             HC-LIGHT #hc-light,\
                             POSTHEAD #posthead ] ] ].'

SENTENTIAL_LEX_RULE = 'sentential-lex-rule := anc-lex-rule & same-cont-lex-rule &\
  [ SYNSEM.LOCAL [ CAT [ VAL [ SPR #spr]]],\
    DTR.SYNSEM.LOCAL.CAT [ VAL [SPR #spr]],\
    C-CONT [ RELS.LIST < >,\
             HCONS.LIST < > ] ].'

TRANS_SENT_LEX_RULE = 'trans-sent-lex-rule := sentential-lex-rule &\
  [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ #subj, \
                               COMPS #comps]]],\
    DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj, \
                                COMPS #comps & cons]]].'

INTRANS_SENT_LEX_RULE = 'intrans-sent-lex-rule := sentential-lex-rule &\
 [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ #subj, \
                              COMPS #comps]]],\
    DTR.SYNSEM.LOCAL.CAT [ VAL [SUBJ #subj, \
                                COMPS #comps & null]]].'

TRANS_SENT_ALT_LEX_RULE = 'trans-sent-alt-lex-rule := sentential-lex-rule &\
  [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >,\
                               COMPS #comps]]],\
    DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >,\
                                COMPS #comps & cons]]].'

INTRANS_SENT_ALT_LEX_RULE = 'intrans-sent-alt-lex-rule := sentential-lex-rule &\
 [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >,\
                              COMPS #comps]]],\
    DTR.SYNSEM.LOCAL.CAT [ VAL [SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >,\
                                COMPS #comps & null]]].'

ANC_LOW_NMZ_LEX_RULE = 'anc-low-nmz-lex-rule := anc-lex-rule &\
[SYNSEM.LOCAL.CAT.VAL.SUBJ < >,\
 C-CONT [ RELS.LIST < [ PRED "nominalized_rel",\
                           LBL #ltop,\
                           ARG0 ref-ind & #arg0,\
                           ARG1 #arg1 ] >,\
             HCONS.LIST < qeq &\
                          [ HARG #arg1,\
                            LARG #larg ] >,\
             HOOK [ XARG #xarg,\
                    INDEX #arg0,\
                    LTOP #ltop ] ],\
 DTR.SYNSEM.LOCAL [CONT.HOOK [ XARG #xarg,\
                	      LTOP #larg ]]].'

NON_SENT_ANC_INTRANS_LEX_RULE_SUPERTPYE = 'non-sent-anc-intrans-lex-rule_supertype := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CAT.VAL.SPR <> ]] >,\
                          COMPS #comps],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS #comps & null]].'


NON_SENT_ANC_INTRANS_LEX_RULE = 'non-sent-anc-intrans-lex-rule := non-sent-anc-intrans-lex-rule_supertype &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CONT.HOOK.INDEX #subj, \
                                         CAT.HEAD.POSSESSOR possessive ]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >]].'

TRANS_POSS_ACC_LEX_RULE_SUPERTYPE = 'trans-poss-acc-lex-rule_supertype := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CAT.VAL.SPR <>]] >,\
                          COMPS #comps],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS #comps & cons]].'

TRANS_POSS_ACC_LEX_RULE = 'trans-poss-acc-lex-rule := trans-poss-acc-lex-rule_supertype &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CONT.HOOK.INDEX #subj, \
                                       CAT.HEAD.POSSESSOR possessive ]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >]].'

TRANS_ERG_POSS_LEX_RULE_SUPERTYPE = 'trans-erg-poss-lex-rule_supertype := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >,\
                           SPR < [LOCAL [CAT.VAL.SPR <>]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ  < [LOCAL.CONT.HOOK.INDEX #subj] >, \
                               COMPS <[]>]].'


TRANS_ERG_POSS_LEX_RULE = 'trans-erg-poss-lex-rule := trans-erg-poss-lex-rule_supertype &\
  [ SYNSEM.LOCAL.CAT.VAL [ SPR < [LOCAL [CONT.HOOK.INDEX #obj, \
                                         CAT.HEAD.POSSESSOR possessive]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [LOCAL.CONT.HOOK.INDEX #obj] > ]].'

TRANS_NOMINAL_LEX_RULE_SUPERTYPE = 'trans-nominal-lex-rule_supertype := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [LOCAL [CONT.HOOK.INDEX #obj,\
                                           CAT.VAL.SPR <>]] >,\
                       	   SPR < [LOCAL [CAT.VAL.SPR <>]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [LOCAL.CONT.HOOK.INDEX #obj] >]].'


TRANS_NOMINAL_LEX_RULE = 'trans-nominal-lex-rule := trans-nominal-lex-rule_supertype &\
  [ SYNSEM.LOCAL.CAT.VAL [ SPR < [LOCAL [CONT.HOOK.INDEX #subj, \
                                        CAT.HEAD.POSSESSOR possessive]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ  < [LOCAL.CONT.HOOK.INDEX #subj] >]].'

TRANS_NON_ERG_POSS_OBJ_ONLY_LEX = 'trans-non-erg-poss-obj-only-lex-rule := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CONT.HOOK.INDEX #obj, \
                                              CAT.VAL.SPR <>]] >,\
                          COMPS < [OPT +] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [LOCAL.CONT.HOOK.INDEX #obj] > ]].'

TRANS_ERG_POSS_SUBJ_ONLY_LEX = 'trans-erg-poss-subj-only-lex-rule := anc-low-nmz-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [SPR < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >,\
                          COMPS < [OPT +] > ],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >, \
                               COMPS <[]>]].'

COMP_ANC_LEX_RULE = 'comps-anc-lex-rule := anc-low-nmz-lex-rule & [ SYNSEM.LOCAL.CAT.VAL.SPR < [] >].'

COMP_ANC_INTRANS_LEX_RULE = 'comps-anc-intrans-lex-rule := comps-anc-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [COMPS < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >,\
                               COMPS < >]].'

COMP_OBJ_FIRST_TRANS_LEX_RULE = 'comp-obj-trans-lex-rule := comps-anc-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [COMPS < [LOCAL [CONT.HOOK.INDEX #obj, \
                                              CAT.VAL.SPR <>]], \
                                  [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >,\
                               COMPS < [LOCAL.CONT.HOOK.INDEX #obj] > ]].'

COMP_SUBJ_FIRST_TRANS_LEX_RULE = 'comp-subj-trans-lex-rule := comps-anc-lex-rule &\
  [ SYNSEM.LOCAL.CAT.VAL [COMPS < [LOCAL [CONT.HOOK.INDEX #subj, \
                                              CAT.VAL.SPR <>]], \
                                  [LOCAL [CONT.HOOK.INDEX #obj, \
                                              CAT.VAL.SPR <>]] >],\
    DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [LOCAL.CONT.HOOK.INDEX #subj] >,\
                               COMPS < [LOCAL.CONT.HOOK.INDEX #obj] > ]].'


#Constants for phrase-structure rules:

# A non-branching rule for nominalized clauses to form a NP, with nominalized_rel for the MRS.
# For sent/alt-sent nominalization.
HIGH_NMZ_CLAUSE = 'high-nominalized-clause-phrase := unary-phrase &\
  [ SYNSEM [ LOCAL [ CAT [ HEAD #head & noun &\
                                [ NMZ +,\
                                  MOD < > ],\
                           VAL [ SPR < [OPT +] >,\
                                 COMPS < >,\
                                 SUBJ < >,\
                                 SPEC < > ] ],\
                     COORD - ],\
             NON-LOCAL #nl ],\
    C-CONT [ RELS.LIST < [ PRED "nominalized_rel",\
                           LBL #ltop,\
                           ARG0 ref-ind & #arg0,\
                           ARG1 #arg1 ]>,\
             HCONS.LIST < qeq &\
                          [ HARG #arg1,\
                            LARG #larg ] >,\
             HOOK [ XARG #xarg,\
                    INDEX #arg0,\
                    LTOP #ltop ] ],\
    ARGS < [ SYNSEM [ NON-LOCAL #nl,\
                      LOCAL [ CAT [ HEAD #head,\
                                    VAL [ COMPS < >,\
                                          SUBJ < >,\
                                          SPR < >, \
                                          SPEC < > ] ],\
                              CONT.HOOK [ INDEX event \
                                          XARG #xarg,\
                                          LTOP #larg ],\
                              COORD - ] ] ] > ].'

########################
### HELPER FUNCTIONS ###
########################

# This assumes that the lrt is associated with nominalization.
def case_change_lrt(arg, lrt):
    """
    @param arg: obj or subj or obj2
    @param lrt: lexical rule type object
    @return: True if this lrt specifies case change on arg.
    """
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == arg:
            return True
    return False


def get_head_type(arg, lrt, ch):
    """
    Call a function from choices.py to determine what is the lexical rule's head.
    @param arg: obj or subj or obj2
    @param lrt: lexical rule type object
    @param ch: the entire choices object
    @return: string corresponding to the head type, such as "noun" or "adp" or "+np"
    """
    head_type = ''
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == arg:
            head_type = ch.case_head(f['value'])
    return head_type

def get_nmz_lexrules(ch):
    """
    Collect all lexical rule types from verbal
    position classes that involve nominalization.
    @param ch: the entire choices object
    @return: rules (list of tuples (lrt, the value of the nominalization feature on the Morphology page, verb-pc).
    """
    rules = []
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'nominalization' in f['name']:
                    rules.append((lrt, f['value'], vpc))
    return rules

def need_specialized_head_spec(ch):
    '''
    Called by word_order.py and adnominal_possession.py
    and determines whether a [NMZ +] version of the head-spec-phrase rule is needed. 
    Needed if there is at least one nominalization possessive strategy 
    that uses the poss-unary-phrase rule.
    @param ch: the entire choices object
    @return: True is a [NMZ +] subtype of the head-spec-phrase rule is necessary
    '''
    
    for strat in ch.get('nmz_poss_strat', []):
        nmz_poss_strat_name = strat.get('name')
        for poss_strat in ch.get('poss-strat', []):
            #A [NMZ +] head-spec-phrase subtype is not needed for juxtaposition strategies
            if get_name(poss_strat) == nmz_poss_strat_name and poss_strat.get('mark-loc') != 'neither':
                return True
        for pron in ch.get('poss-pron', []):
            #A [NMZ +] head-spec-phrase subtype is not needed for pronominal affixes
            if get_name(pron) == nmz_poss_strat_name and pron.get('type') != 'affix':
                return True 
    return False

def add_sem_empty_adp(mylang, nmz_lrt, type_name, geom, arg):
    '''
    Writes the feature path for semantically empty adpositions
    @param mylang:
    @param nmz_lrt: nominalization lrt
    @param type_name: name of the nominalization lrt
    @param geom: feature path
    @param arg: obj or subj or obj2
    '''
    for feat in nmz_lrt['feat']:
        if feat['name'] == 'form' and feat['value'].endswith("_sem"):
            head = feat['head']
            if head == arg:
                mylang.set_section('lexrules')
                mylang.add(type_name + ':= [ ' + geom +  ' adp ] > ].', merge=True)

def write_head_type(ch, mylang, nmz_lrt, type_name, geom, arg):
    '''
     Writes the head value for case-marked arguments
    @param mylang:
    @param nmz_lrt: nominalization lrt
    @param type_name: name of the nominalization lrt
    @param geom: feature path
    @param arg: obj or subj or obj2
    '''
    if case_change_lrt(arg, nmz_lrt):
        mylang.set_section('lexrules')
        head_type = get_head_type(arg, nmz_lrt, ch)
        mylang.add(
            type_name + ' := [ ' + geom + ' ' + head_type + '] > ].',merge=True)

def customize_non_user_nmz_features(ch, mylang, nmz_lrt, val, pos, type_name, geom):
    '''
    Adds all constraints to the nominalization lrts that will not be handled by them morphotactics library.
    This includes the HEAD value of case-marked arguments, the FORM value of semantically empty adps, and the ADV-MOD feature
    @param ch:  the entire choices object
    @param mylang:
    @param nmz_lrt: nominalization lrt
    @param val: the value of the nominalization feature on the Morphology page 
    @param pos: string representing what argument to add the features to:
    'nmz-subj-change' or 'nmz-comp-change' or 'nmz-second-comp-change' or 'nmz_adv-mod'
    @param type_name: name of the nominalization lrt
    @param geom: feature path
    '''
    for ns in ch.get('ns', ''):
        if val == ns.get('name'):

            nmz_type = ns.get('nmz_type')
            if ns.get('trans') == 'on':
                trans = True  
            else:
                trans = False

            adj = False
            adv = False
            if ns.get('adj') == 'on':
                adj = True
            if ns.get('adv') == 'on':
                adv = True

            #Add any constraints specified on the subject
            if (nmz_type == 'alt-sent' and pos == 'nmz-subj-change'):
                arg = 'subj'
                write_head_type(ch, mylang, nmz_lrt, type_name, geom, arg)
                add_sem_empty_adp(mylang, nmz_lrt, type_name, geom, arg)

            #Add any constraints specified on the object
            if (nmz_type == 'nominal' or nmz_type == 'erg-poss') and trans and pos == 'nmz-comp-change':
                arg = 'obj'
                write_head_type(ch, mylang, nmz_lrt, type_name, geom, arg)
                add_sem_empty_adp(mylang, nmz_lrt, type_name, geom, arg)

             #Add any constraints specified on the object
            if nmz_type == 'all-comps' and pos == 'nmz-comp-change':
                arg = 'obj'
                write_head_type(ch, mylang, nmz_lrt, type_name, geom, arg)
                add_sem_empty_adp(mylang, nmz_lrt, type_name, geom, arg)

             #Add any constraints specified on the second object
            if nmz_type == 'all-comps' and trans and pos == 'nmz-second-comp-change':
                arg = 'obj2'
                write_head_type(ch, mylang, nmz_lrt, type_name, geom, arg)
                add_sem_empty_adp(mylang, nmz_lrt, type_name, geom, arg)

            #Add ADV-MOD constraints to the nominalization lrt based on the desired modifier behavior 
            if pos == 'nmz_adv-mod':
                mylang.set_section('lexrules')
                #nominalized verb only modified by adjectives
                if adj and not adv:
                    mylang.add(type_name  + ' := [' +  geom + ' - ].',merge=True)
                #nominalized verb only modified by adverbs
                elif adv and not adj:
                    mylang.add(type_name + ' := [ ' + geom + ' + ].',merge=True)
                ##nominalized verb modified by neither
                elif not adv or not adj:
                    mylang.add(type_name + ' := [ ' + geom + ' na ].',merge=True)

def get_nmz_clause_wo(ch):
    '''
    Called by word_order.py and adnominal_possession.py
    Returns the word order in ANCs for the relevant strategies (poss-acc/erg-poss/nominal).
    Converts the value for erg-poss. The semantic agent (S) becomes the syntactic object (O)
    and the semantic object (O) becomes the syntactic specifer (S).
    @param ch:  the entire choices object
    @return: nmz_wo (a string consisting of the word order within ANCs)
    '''
    erg_poss_conversion =  {"sov": "osv",
                            "svo": "ovs",
                            "vso": "vos",
                            "osv": "sov",
                            "ovs": "svo",
                            "vos": "vso"}
    
    nmz_wo = None

    for ns in ch.get('ns'):

        nmz_type = ns.get('nmz_type')
        if nmz_type == "sentential" or nmz_type == 'alt-sent' or nmz_type == 'all-comps':
            continue
        elif ch.get('same-word-order') == 'yes':
            nmz_wo = ch.get('word-order')
            break
        else:
            nmz_wo = ch.get('nmz-clause-word-order')
            break
    if nmz_wo in [ "sov", "svo","vso", "osv", "ovs", "vos"] and nmz_type == 'erg-poss':
        nmz_wo = erg_poss_conversion[nmz_wo]

    return nmz_wo

def needs_anc_wo_feat(ch):
    '''
    Determines if the ANC-WO feature is necessary for a choices file
    @param ch:  the entire choices object
    @return: needs_anc_wo (a boolean indicating whether the ANC-WO feature is necessary for the choices file)
    '''
    head_final_wo = ['sov', 'osv', 'ovs', 'v-final']
    head_init_wo = ['svo', 'vos', 'vso', 'v-initial']

    verb_wo = ch.get('word-order')
    nmz_wo =  get_nmz_clause_wo(ch)

    needs_anc_wo = False

    #The ANC-WO feature is necessary whenever an additional head-comp rule needs 
    #to be added which is exclusive to POSS-ACC/ERG-POSS/NOMINAL ANCS
    if (nmz_wo in head_final_wo and verb_wo in head_init_wo) or (nmz_wo in head_init_wo and verb_wo in head_final_wo) or  nmz_wo in ['vso', 'osv'] or (verb_wo in ['free', 'v2'] and nmz_wo not in ['free', 'v2']):
        needs_anc_wo = True
    if nmz_wo in ['free', 'v2']:
        if (verb_wo not in ['free', 'v2']) or (verb_wo == 'free' and nmz_wo == 'v2') or (verb_wo == 'v2' and nmz_wo == 'free'):
            needs_anc_wo = True
    elif verb_wo in ['free', 'v2']:
        needs_anc_wo = True

    return needs_anc_wo


def need_det_rules(ch, ns):
    '''
    Determines if a nominalization strategy allows for action nominals which take both a determiner and a syntactic possessor
    @param ch:  the entire choices object
    @param na:  a nominalization strategy
    @return: boolean indicating whether the ns allows for both a determiner and a syntactic possesor
    '''
    if not ch.get('has-dets'):
        return False
    
    if ns.get('nmz_type') != 'all-comps' and (ns.get('det') == 'opt' or ns.get('det') == 'obl'):
        return True
        
    return False

############################
### MAIN LOGIC FUNCTIONS ###
############################

def customize_nmcs(mylang, ch, rules):
    """
    the main nominalized clause customization routine
    """
    if not ch.get('ns'):
        return
    
    add_lexrules(ch)  
    update_lexical_rules(mylang, ch)
    add_nmz_feature(mylang)
    add_anc_lex_supertype(mylang, ch)
    for ns in ch.get('ns'):
        nmz_type = ns.get('nmz_type')
        nmzrel = ns.get('nmzRel')
        single_arg = ns.get('single-arg')
        arg_order = ns.get('all_comps_arg_order')
        if (nmz_type == "sentential" or nmz_type == 'alt-sent') and nmzrel == 'yes':
            add_nmz_clause_phrases(ch, mylang, rules)
        add_nmz_lexrules(ch, mylang, ns, nmz_type, single_arg, arg_order)
        if ch.get("adv", '') or ch.get("adj", ''):
            add_nmz_mod_constraints(ch, mylang)
    if ch.get('cs'):
        add_coord_constraints(mylang, ch)
    handle_spr_restrictions(mylang, ch)
    if needs_anc_wo_feat(ch):
        set_anc_wo_value(ch, mylang)

#TODO --> validate so that if someone defines a nmz strategy that requires a morph rule, they actually make that rule on the morph page
def add_lexrules(ch):
    '''
    Adds additional lexical rules to the choices object that the user does not define 
    @param ch:  the entire choices object
    '''
    #If an action nominal has a pronominal possessor marked with an affix, an additional [NMZ +] lexical 
    #rule is added in addition to the one defined by the user for non-derived nouns    
    for pc in ch['noun-pc']:
        for lrt in pc['lrt']:
            for feat in lrt['feat']:
                if 'poss-pron' in feat['name'] and feat['value'] == 'plus':
                    poss_pron_name = feat['name']
                    rules_created = False
                    #A a nominalization strategy contains a pronominal affix possessive strategy
                    for strat in ch.get('nmz_poss_strat', []):
                        if strat.get('name') == poss_pron_name:
                            rules_created = True
                            idx = ch[pc.full_key + '_lrt'].next_iter_num()
                            lrt_key = pc.full_key + '_lrt' + str(idx)
                            ch[lrt_key + '_name'] = get_name(lrt) + '_anc'
                            feat_num = 1
                            for feat in lrt['feat']:
                                #Copy all features from the original lexical rule aside from the 'poss-pron' feature
                                #This feature is given a unique value 'ANC' so that this lexical rule can be identified later on.
                                if 'poss-pron' in feat['name']:
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                    ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = 'ANC'
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                else:
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                    ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = feat['value']
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                feat_num += 1
                            lri_num = 1
                            for lri in lrt['lri']:
                                ch[lrt_key + '_lri' + str(lri_num) +'_inflecting'] = lri['inflecting']
                                ch[lrt_key + '_lri' + str(lri_num) +'_orth'] = lri['orth']
                                lri_num += 1
                            require_num = 1
                            for require in lrt['require']:
                                 ch[lrt_key + '_require' + str(require_num) +'_others'] = require['others']
                                 require_num += 1
                            forbid_num = 1
                            for forbid in lrt['forbid']:
                                 ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = forbid['others']
                                 forbid_num += 1
                        if rules_created:
                            break

    #Three situations where an additional lrt has to be added based on an existing nominalization lrt 
    #(1) If the 'single-arg' option is selected for a nominalization strategy add the COMPS [OPT +] version of the nominalization lrt
    #(2) if the strategy applies to both transitive and intransitive verbs, add an intransitive version of the nominalization lrt
    #(2) if the strategy can take determiners in addition to possessors, add a nonpossessive version of the rule to the get the correct semantics
    for pc in ch['verb-pc']:
        real_rule_num = len(pc['lrt'])
        counter = 0
        for lrt in pc['lrt']:
            counter += 1
            if counter > real_rule_num:
                break
            for feat in lrt['feat']:
                if  feat['name'] == 'nominalization':
                    rules_created = False
                    nmz_strat = feat['value']
                    for ns in ch.get('ns'):
                        if ns.get('name') == nmz_strat:
                            #Nominalization strategy has 'single-arg' set to 'on'
                            if ns.get('single-arg') == 'on' and ns.get('trans') == 'on':
                                rules_created = True
                                idx = ch[pc.full_key + '_lrt'].next_iter_num()
                                lrt_key = pc.full_key + '_lrt' + str(idx)
                                ch[lrt_key + '_name'] = get_name(lrt) + '_impossible_arg'
                                feat_num = 1
                                #Copy all features from the original nominalization lrt
                                for feat in lrt['feat']:
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                    ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = feat['value']
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                    feat_num += 1
                                #Add a new feature to the lrt so that it can be identified later on
                                ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = 'anc-obligatory-arg'
                                lri_num = 1
                                for lri in lrt['lri']:
                                    ch[lrt_key + '_lri' + str(lri_num) +'_inflecting'] = lri['inflecting']
                                    ch[lrt_key + '_lri' + str(lri_num) +'_orth'] = lri['orth']
                                    lri_num += 1
                                require_num = 1
                                for require in lrt['require']:
                                    ch[lrt_key + '_require' + str(require_num) +'_others'] = require['others']
                                    require_num += 1
                                forbid_num = 1
                                for forbid in lrt['forbid']:
                                    ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = forbid['others']
                                    forbid_num += 1
                            #Nominalization strategy applies to both intransitive and transitive verbs
                            if ns.get('intrans') == 'on' and ns.get('trans') == 'on':
                                rules_created = True
                                idx = ch[pc.full_key + '_lrt'].next_iter_num()
                                lrt_key = pc.full_key + '_lrt' + str(idx)
                                ch[lrt_key + '_name'] = get_name(lrt) + '_intrans'
                                feat_num = 1
                                #Don't need to copy features corresponding to the object argument
                                if ns.get('nmz_type') == 'all-comps':
                                    obj_to_ignore = 'obj2'
                                else:
                                    obj_to_ignore = 'obj'
                                for feat in lrt['feat']:
                                    if feat['head'] != obj_to_ignore:
                                        ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                        ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = feat['value']
                                        ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                    feat_num += 1
                                #Add a new feature to the lrt so that it can be identified later on
                                ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = 'valence_intransitive'
                                lri_num = 1
                                for lri in lrt['lri']:
                                    ch[lrt_key + '_lri' + str(lri_num) +'_inflecting'] = lri['inflecting']
                                    ch[lrt_key + '_lri' + str(lri_num) +'_orth'] = lri['orth']
                                    lri_num += 1
                                require_num = 1
                                for require in lrt['require']:
                                    ch[lrt_key + '_require' + str(require_num) +'_others'] = require['others']
                                    require_num += 1
                                forbid_num = 1
                                for forbid in lrt['forbid']:
                                    ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = forbid['others']
                                    forbid_num += 1
                            
                            #The nominalization strategy can use determiners instead of a possessor
                            if  need_det_rules(ch, ns):
                                rules_created = True
                                idx = ch[pc.full_key + '_lrt'].next_iter_num()
                                lrt_key = pc.full_key + '_lrt' + str(idx)
                                ch[lrt_key + '_name'] = get_name(lrt) + '_det'
                                feat_num = 1
                                for feat in lrt['feat']:
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                    ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = feat['value']
                                    ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                    feat_num += 1
                                #Add a new feature to the lrt so that it can be identified later on
                                ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = 'anc_det'
                                lri_num = 1
                                for lri in lrt['lri']:
                                    ch[lrt_key + '_lri' + str(lri_num) +'_inflecting'] = lri['inflecting']
                                    ch[lrt_key + '_lri' + str(lri_num) +'_orth'] = lri['orth']
                                    lri_num += 1
                                require_num = 1
                                for require in lrt['require']:
                                    ch[lrt_key + '_require' + str(require_num) +'_others'] = require['others']
                                    require_num += 1
                                forbid_num = 1
                                for forbid in lrt['forbid']:
                                    ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = forbid['others']
                                    forbid_num += 1
                                for noun_pc in ch['noun-pc']:
                                    for lrt in noun_pc['lrt']:
                                        for feat in lrt['feat']:
                                            if 'poss-pron' in feat['name'] and feat['value'] == 'ANC':
                                                identifier = str(lrt).split("\n")[0].split("=")[0].split("_name")[0]
                                                ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = identifier
                                                forbid_num += 1
                            
                    if rules_created:
                        break

    for pc in ch['verb-pc']:
        real_rule_num = len(pc['lrt'])
        counter = 0
        for lrt in pc['lrt']:
            counter += 1
            if counter > real_rule_num:
                break

            needs_intrans = False
            for feat in lrt['feat']:
                if feat['name'] == 'nominalization':
                    nmz_strat = feat['value'] 
                    for feat in lrt['feat']:
                        if feat['name'] == 'valence_intransitive':
                            needs_intrans = True
                    if not needs_intrans:
                        continue
                    rules_created = False
                    for ns in ch.get('ns'):
                        #The nominalization strategy can use determiners instead of a possessor (have to do this step after all other rules are added)
                        if ns.get('name') == nmz_strat and need_det_rules(ch, ns):
                            rules_created = True
                            idx = ch[pc.full_key + '_lrt'].next_iter_num()
                            lrt_key = pc.full_key + '_lrt' + str(idx)
                            ch[lrt_key + '_name'] = get_name(lrt) + '_det'
                            feat_num = 1
                            for feat in lrt['feat']:
                                ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = feat['name']
                                ch[lrt_key + '_feat'+ str(feat_num) +'_value'] = feat['value']
                                ch[lrt_key + '_feat'+ str(feat_num) + '_head'] = feat['head']
                                feat_num += 1
                             #Add a new feature to the lrt so that it can be identified later on
                            ch[lrt_key + '_feat'+ str(feat_num) + '_name'] = 'anc_det'
                            lri_num = 1
                            for lri in lrt['lri']:
                                ch[lrt_key + '_lri' + str(lri_num) +'_inflecting'] = lri['inflecting']
                                ch[lrt_key + '_lri' + str(lri_num) +'_orth'] = lri['orth']
                                lri_num += 1
                            require_num = 1
                            for require in lrt['require']:
                                 ch[lrt_key + '_require' + str(require_num) +'_others'] = require['others']
                                 require_num += 1
                            forbid_num = 1
                            for forbid in lrt['forbid']:
                                 ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = forbid['others']
                                 forbid_num += 1
                            for noun_pc in ch['noun-pc']:
                                for lrt in noun_pc['lrt']:
                                    for feat in lrt['feat']:
                                        if 'poss-pron' in feat['name'] and feat['value'] == 'ANC':
                                            identifier = str(lrt).split("\n")[0].split("=")[0].split("_name")[0]
                                            ch[lrt_key + '_forbid' + str(forbid_num) +'_others'] = identifier
                                            forbid_num += 1
                    if rules_created:
                        break

def update_lexical_rules(mylang, ch):
    """
    Create appropriate lexical rule subtypes for nominalization.
    Add an appropriate supertype to each nominalizing verbal position class.
    Also adds any features to the nominalization lrt that will not be handled by the morphotactics library
    """
    path_subj = 'SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.LOCAL.CAT.HEAD'
    path_comps = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD'
    path_second_comps = 'SYNSEM.LOCAL.CAT.VAL.COMPS.REST.FIRST.LOCAL.CAT.HEAD'
    has_poss_strats = False
    if 'poss-strat' in ch or 'poss-pron' in ch:
        has_poss_strats = True
    non_poss_comp = '[SYNSEM.LOCAL.CAT.VAL.COMPS < [LOCAL.CAT [HEAD.POSSESSOR nonpossessive, \
                                                                        POSSESSUM nonpossessive]]>].'
    for lrt, val, vpc in get_nmz_lexrules(ch):
        for ns in ch.get('ns'):

            intrans = False
            trans = False
            if ns.get('name') == val:
                nmz_type = ns.get('nmz_type')
                needs_intrans = False
                if ns.get('intrans') == 'on':
                    intrans = True
                if ns.get('trans') == 'on':
                    trans = True  
                if intrans and trans:
                    for feat in lrt['feat']:
                        #lrt added by add_lexrules for intransitive verbs
                        if feat['name'] == 'valence_intransitive':
                            needs_intrans = True

                needs_extra_rule = False
                if ns.get('single-arg') == 'on':
                    for feat in lrt['feat']:
                        #lrt added by add_lexrules for the 'single-arg' analysis
                        if feat['name'] == "anc-obligatory-arg":
                            needs_extra_rule = True
                det_rules = False
                if need_det_rules(ch, ns):
                    for feat in lrt['feat']:
                        #lrt added by add_lexrules for the action nominals which take determiners instead of possessors
                        if feat['name'] == "anc_det":
                            det_rules = True

                if nmz_type == 'sentential':
                    if (intrans and not trans) or needs_intrans:
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['intrans-sent-lex-rule'])
                    elif trans:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-sent-lex-rule'])
                        if has_poss_strats:
                            mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                elif nmz_type == 'alt-sent':
                    if (intrans and not trans) or needs_intrans:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['intrans-sent-alt-lex-rule'])
                    elif trans:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-sent-alt-lex-rule'])
                        if has_poss_strats:
                            mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                    customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-subj-change', get_name(lrt) + '-lex-rule', path_subj)
                elif nmz_type == 'all-comps':
                    if (intrans and not trans) or needs_intrans:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['comps-anc-intrans-lex-rule'])
                        customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-comp-change', get_name(lrt) + '-lex-rule', path_comps)
                        if has_poss_strats:
                            mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                    elif trans:
                        if ns.get('all_comps_arg_order') == 'agent':
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['comp-subj-trans-lex-rule']) 
                        elif ns.get('all_comps_arg_order') == 'patient':
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['comp-obj-trans-lex-rule'])
                        if has_poss_strats:
                            mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                            mylang.add(get_name(lrt) + '-lex-rule := [SYNSEM.LOCAL.CAT.VAL.COMPS < [LOCAL.CAT [HEAD.POSSESSOR nonpossessive, \
                                                                        POSSESSUM nonpossessive]], [LOCAL.CAT [HEAD.POSSESSOR nonpossessive, \
                                                                        POSSESSUM nonpossessive]] >].')
                        customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-comp-change', get_name(lrt) + '-lex-rule', path_comps)
                        customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-second-comp-change', get_name(lrt) + '-lex-rule', path_second_comps)

                elif (intrans and not trans) or needs_intrans:
                    if det_rules:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['det-non-sent-anc-intrans-lex-rule'])
                    else:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['non-sent-anc-intrans-lex-rule'])
                elif nmz_type == 'poss-acc':
                    if needs_extra_rule:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-non-erg-poss-obj-only-lex-rule'])
                    elif det_rules:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['det-trans-poss-acc-lex-rule'])
                    else:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-poss-acc-lex-rule'])
                        mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                elif nmz_type == 'erg-poss':
                    if needs_extra_rule:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-erg-poss-subj-only-lex-rule'])
                    elif det_rules:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['det-trans-erg-poss-lex-rule'])
                    else:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['trans-erg-poss-lex-rule'])
                        mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                    customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-comp-change', get_name(lrt) + '-lex-rule', path_comps)

                elif nmz_type == 'nominal':
                    if needs_extra_rule:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                  ['trans-non-erg-poss-obj-only-lex-rule'])
                    elif det_rules:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['det-trans-nominal-lex-rule'])
                    else:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') +
                                                    ['trans-nominal-lex-rule'])
                        mylang.add(get_name(lrt) + '-lex-rule :=' + non_poss_comp)
                    customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz-comp-change', get_name(lrt) + '-lex-rule', path_comps)

def add_nmz_feature(mylang):
    """
    Add NMZ feature to addenda, verb, and nouns.
    """
    mylang.set_section('addenda')
    mylang.add('head :+ [ NMZ bool ].')
    mylang.set_section('noun-lex')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')


def add_anc_lex_supertype(mylang, ch): 
    mylang.set_section('lexrules')
    mylang.add(ANC_LEX_RULE)
    if ('poss-strat' in ch or 'poss-pron' in ch):
        mylang.add('anc-lex-rule := [ SYNSEM.LOCAL.CAT [HEAD [POSSESSOR #possessor], \
                                                        POSSESSUM #possessum ],\
                                     DTR.SYNSEM.LOCAL [ CAT [ HEAD [POSSESSOR #possessor & nonpossessive], \
                                                              POSSESSUM #possessum & nonpossessive ] ] ].')

def add_nmz_clause_phrases(ch, mylang, rules):
    """
    Add the non-branching rule for sentential nominalization which turns a nominalized clause into a NP.
    """
    mylang.set_section('phrases')
    mylang.add(HIGH_NMZ_CLAUSE)
    if 'poss-strat' in ch or 'poss-pron' in ch:
            mylang.add('high-nominalized-clause-phrase := [SYNSEM.LOCAL.CAT.VAL.SPR < [LOCAL.CAT.HEAD.POSSESSOR nonpossessive] >].')
    rules.add('high-nominalized-clause := high-nominalized-clause-phrase.')


def add_nmz_lexrules(ch, mylang, ns, nmz_type, single_arg, arg_order):
    """
    Add appropriate lexical rules.
    @param mylang: .
    @param ns: the nominalizaition strategy object
    @param nmz_type: sentential, alt-sent, all-comps, poss-acc, erg-poss, or nominal.
    @param single_arg: the 'single-arg' choice for ns (on or off)
    @param arg-order: which argument serves as the first complement for all-comps (agent or patient)
    """
    intrans = False
    trans= False
    if ns.get('intrans') == 'on':
        intrans = True
    if ns.get('trans') == 'on':
        trans = True

    if need_det_rules(ch, ns):
        det_rules = True
        det_rule_constraint = '[ SYNSEM.LOCAL.CAT.VAL [SPR < [OPT -, \
                                                              LOCAL.CAT.HEAD.POSSESSOR nonpossessive] >]].'
    else:
        det_rules = False
        
    mylang.set_section('lexrules')
    if nmz_type == 'sentential' or nmz_type == 'alt-sent':
        mylang.add(SENTENTIAL_LEX_RULE)
        if 'poss-strat' in ch or 'poss-pron' in ch:
            mylang.add('sentential-lex-rule := [SYNSEM.LOCAL.CAT.VAL.SUBJ < [LOCAL.CAT [HEAD.POSSESSOR nonpossessive, \
                                                                                        POSSESSUM nonpossessive]]>].')
        if nmz_type == 'sentential':
            if intrans:
                mylang.add(INTRANS_SENT_LEX_RULE)
            if trans:
                mylang.add(TRANS_SENT_LEX_RULE)
        else:
            if intrans:
                mylang.add(INTRANS_SENT_ALT_LEX_RULE)
            if trans:
                mylang.add(TRANS_SENT_ALT_LEX_RULE)

    else:
        mylang.add(ANC_LOW_NMZ_LEX_RULE)
        if nmz_type == 'all-comps':
            mylang.add(COMP_ANC_LEX_RULE)
            if 'poss-strat' in ch or 'poss-pron' in ch:
                mylang.add('comps-anc-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SPR < [LOCAL.CAT.HEAD.POSSESSOR nonpossessive] > ].')
            if intrans:
                mylang.add(COMP_ANC_INTRANS_LEX_RULE)
            if  trans and arg_order == 'agent':
                mylang.add(COMP_SUBJ_FIRST_TRANS_LEX_RULE)
            if trans and arg_order == 'patient':
                mylang.add(COMP_OBJ_FIRST_TRANS_LEX_RULE)
        elif intrans:
            lex_rule_name = 'non-sent-anc-intrans-lex-rule'
            mylang.add(NON_SENT_ANC_INTRANS_LEX_RULE_SUPERTPYE)
            mylang.add(NON_SENT_ANC_INTRANS_LEX_RULE)
            if det_rules:
                mylang.add('det-' + lex_rule_name + ':=' + lex_rule_name + '_supertype &' + det_rule_constraint)
        if trans and nmz_type == 'poss-acc':
            lex_rule_name = 'trans-poss-acc-lex-rule'
            mylang.add(TRANS_POSS_ACC_LEX_RULE_SUPERTYPE)
            mylang.add(TRANS_POSS_ACC_LEX_RULE)
            if det_rules:
                mylang.add('det-' + lex_rule_name + ':=' + lex_rule_name + '_supertype &' + det_rule_constraint)
        if trans and nmz_type == 'erg-poss':
            lex_rule_name = 'trans-erg-poss-lex-rule'
            mylang.add(TRANS_ERG_POSS_LEX_RULE_SUPERTYPE)
            mylang.add(TRANS_ERG_POSS_LEX_RULE)
            if det_rules:
                mylang.add('det-' + lex_rule_name + ':=' + lex_rule_name + '_supertype &' + det_rule_constraint)
        if trans and nmz_type == 'nominal':
            lex_rule_name = 'trans-nominal-lex-rule'
            mylang.add(TRANS_NOMINAL_LEX_RULE_SUPERTYPE)
            mylang.add(TRANS_NOMINAL_LEX_RULE)
            if det_rules:
                mylang.add('det-' + lex_rule_name + ':=' + lex_rule_name + '_supertype &' + det_rule_constraint)
        if trans and single_arg == 'on':
            mylang.add(lex_rule_name + ':=  [ SYNSEM.LOCAL.CAT.VAL.SPR < [OPT -] >].')
            if det_rules:
                mylang.add('det-' + lex_rule_name + ':=  [ SYNSEM.LOCAL.CAT.VAL.COMPS < [OPT +] >].')
                #Non-possessive argument cannot appear by itself along with a determiner
                mylang.add(lex_rule_name + ':=  [ SYNSEM.LOCAL.CAT.VAL.SPR < [LOCAL.CAT.HEAD.POSSESSOR possessive] >].')
            if nmz_type == 'poss-acc' or nmz_type == 'nominal':
                mylang.add(TRANS_NON_ERG_POSS_OBJ_ONLY_LEX)
            elif nmz_type == 'erg-poss':
                mylang.add(TRANS_ERG_POSS_SUBJ_ONLY_LEX)


def add_nmz_mod_constraints(ch, mylang):
    '''
    Add ADV-MOD feature to addenda, verb, nouns, adverb-lex-item and to all the nominalization lrts.
    The ADV-MOD feature is added to adjective lexical types within lexical_items.py
    '''
    mylang.set_section('addenda')
    mylang.add('head :+ [ ADV-MOD luk ].')
    mylang.set_section('noun-lex')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.ADV-MOD - ].')
    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.ADV-MOD + ].')
    if ch.get("adv", ''):
        mylang.add('adverb-lex-item := [ SYNSEM.LOCAL.CAT.HEAD.MOD < [LOCAL.CAT.HEAD +nv & [ADV-MOD + ] ] >].')
    for lrt, val, vpc in get_nmz_lexrules(ch):
        customize_non_user_nmz_features(ch, mylang, lrt, val, 'nmz_adv-mod', get_name(lrt) + '-lex-rule', 'SYNSEM.LOCAL.CAT.HEAD.ADV-MOD')

def add_coord_constraints(mylang, ch):
        '''
        Add constraints related to coordination
        '''
        mylang.set_section('addenda')
        mylang.add('bottom-coord-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD.NMZ #nmz,\
                    NONCONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ #nmz,\
                    CONJ-DTR.SYNSEM.LOCAL.CAT.HEAD.NMZ #nmz ].')
        mylang.add('unary-bottom-coord-rule :+ [ SYNSEM.LOCAL.CAT.HEAD.NMZ #nmz,\
                            ARGS < [ SYNSEM.LOCAL.CAT.HEAD.NMZ #nmz ] > ].')

        has_nmz_rel = False
        for ns in ch.get('ns'):
            nmz_type = ns.get('nmz_type')
            nmzrel = ns.get('nmzRel')
            if (nmz_type == "sentential" or nmz_type == 'alt-sent') and nmzrel == 'yes':
                has_nmz_rel = True
                break    

        #Needed to force SENT/ALT-SENT nominalized verbs to go through the high-nominalization phrase   
        if has_nmz_rel:
            mylang.add('np-coord-phrase :+ [ LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX ref-ind, \
                                            RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX ref-ind].')
            mylang.add('n-coord-phrase :+ [ LCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX ref-ind, \
                                            RCOORD-DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX ref-ind].')
        
def handle_spr_restrictions(mylang, ch):
    '''
    Add constraints related to the behavior of specifers for 
    poss-acc/erg-poss/nominal/all-comps nominalization types
    '''                        
    for lrt, val, vpc in get_nmz_lexrules(ch):
        for ns in ch.get('ns'):
            if ns.get('name') == val:
                nmz_type = ns.get('nmz_type')
                #Argument marked as a possessor must appear (only relevant for poss-acc/erg-poss/nominal)
                if ns.get('mand-spr'):
                    mylang.add(get_name(lrt) + '-lex-rule := [SYNSEM.LOCAL.CAT.VAL.SPR < [OPT -, \
                                                                                         LOCAL.CAT.HEAD.POSSESSOR possessive ] > ].')
                if ch.get('has-dets'):
                    if ns.get('det') == 'obl':
                        mylang.add(get_name(lrt) + '-lex-rule := [SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.OPT - ].')

                    elif ns.get('det') == 'imp':
                        if nmz_type == 'all-comps':
                            mylang.add(get_name(lrt) + '-lex-rule := [SYNSEM.LOCAL.CAT.VAL.SPR.FIRST.OPT +].')
                        else:
                            mylang.add(get_name(lrt) + '-lex-rule := [SYNSEM.LOCAL.CAT.VAL.SPR < [LOCAL.CAT.HEAD.POSSESSOR possessive ] > ].')

def set_anc_wo_value(ch, mylang):
    '''
    Set the ANC-WO value for all action nominals as well as for other lexical items
    which take complements
    ''' 
    mylang.add('head :+ [ ANC-WO bool ].', section='addenda')
    for pos in ['noun', 'tverb', 'comps','comp', 'aux']:
        if ch.get(pos) or pos in ['tverb', 'comp']:
            if pos == 'comps':
                name = 'clausal-verb-lex'
            elif pos == 'comp':
                for c in ch.get('comps'):
                    if c.get('comp'):
                        name = 'complementizer-lex-item'
            else:
                name = lexbase.LEXICAL_SUPERTYPES[pos]
            mylang.add(
                name + ' := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO - ].', merge=True)
    for ns in ch.get('ns'):
        if ns.get('trans') != 'on':
            continue
        else:
            nmz_type = ns.get('nmz_type')
            if nmz_type == 'sentential' or nmz_type == 'alt-sent':
                mylang.add('sentential-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO - ].', merge=True)
            elif nmz_type == 'all-comps':
                    mylang.add('comps-anc-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO - ].', merge=True)
            elif nmz_type == 'poss-acc':
                mylang.add('trans-poss-acc-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)
                if ns.get('single-arg') == 'on':
                    mylang.add('trans-non-erg-poss-obj-only-lex-rule:= [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)
            elif nmz_type == 'erg-poss':
                mylang.add('trans-erg-poss-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)
                if ns.get('single-arg') == 'on':
                    mylang.add('trans-erg-poss-subj-only-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)
            elif nmz_type == 'nominal':
                mylang.add('trans-nominal-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)
                if ns.get('single-arg') == 'on':
                    mylang.add('trans-non-erg-poss-obj-only-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.ANC-WO + ].', merge=True)

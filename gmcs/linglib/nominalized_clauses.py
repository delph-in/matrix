###
# Constants
# Admittedly, they are a bit ugly but the hope is they will prevent some number of typo-bugs.
###

HIGH_OR_MID = 'high-or-mid-nominalization-lex-rule'
MID = 'mid-nominalization-lex-rule'
LOW = 'low-nominalization-lex-rule'
LOW_NO_SUBJ_NO_COMPS = 'low-nmz-no-subjid-trans-lex-rule'
LOW_NO_SUBJ_COMPS = 'low-nmz-no-subjid-compsid-lex-rule'
LOW_SUBJ_NO_COMPS = 'low-nmz-subjid-trans-lex-rule'
LOW_SUBJ_COMPS = 'low-nmz-subjid-compsid-lex-rule'

NHS_SUPERTYPE = 'basic-head-subj-phrase'
NHS_DEF = '[ HEAD-DTR.SYNSEM [ LOCAL [ CONT.HOOK.INDEX ref-ind ],\
                               NON-LOCAL [ QUE 0-dlist,\
                                           REL 0-dlist ]]\
            NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR < > ].'

# A rule that does not allow case change on subject or object.
# Only good for high and mid nominalization.
HIGH_OR_MID_LEXRULE_SUBJ_ID = HIGH_OR_MID + ' := cat-change-with-ccont-lex-rule & same-cont-lex-rule &\
    [ SYNSEM.LOCAL [ CONT [ HOOK [ INDEX event ]],\
		   CAT [ HEAD verb &\
			      [ NMZ +,\
                     MOD #mod ],\
                         VAL [ SUBJ #subj,\
                               COMPS #comps,\
                               SPR #spr,\
                               SPEC #spec ],\
                         MC #mc,\
                         MKG #mkg,\
                         HC-LIGHT #hc-light,\
                         POSTHEAD #posthead ] ],\
    DTR.SYNSEM.LOCAL [ CAT [ HEAD [ MOD #mod ],\
                           VAL [ SUBJ #subj,\
                                 COMPS #comps,\
                                 SPR #spr,\
                                 SPEC #spec ],\
                           MC #mc,\
                           MKG #mkg,\
                           HC-LIGHT #hc-light,\
                           POSTHEAD #posthead ]],\
   C-CONT [ RELS <! !>, HCONS <! !> ] ].'

# A rule that allows case change on the subject but not the object.
# Only good for mid nominalization.
MID_LEXRULE_NO_SUBJ_ID = MID + ' := cat-change-with-ccont-lex-rule & same-cont-lex-rule &\
    [ SYNSEM.LOCAL [ CONT [ HOOK [ INDEX event ]],\
		   CAT [ HEAD verb &\
			      [ NMZ +,\
                     MOD #mod ],\
                         VAL [ SUBJ < [ LOCAL [ CAT [ VAL.SPR < > ],\
                                                CONT.HOOK.INDEX #subj ] ] >,\
                               COMPS #comps,\
                               SPR #spr,\
                               SPEC #spec ],\
                         MC #mc,\
                         MKG #mkg,\
                         HC-LIGHT #hc-light,\
                         POSTHEAD #posthead ] ],\
    DTR.SYNSEM.LOCAL [ CAT [ HEAD [ MOD #mod ],\
                           VAL [ SUBJ < [ LOCAL [ CONT.HOOK.INDEX #subj ] ] >,\
                                 COMPS #comps,\
                                 SPR #spr,\
                                 SPEC #spec ],\
                           MC #mc,\
                           MKG #mkg,\
                           HC-LIGHT #hc-light,\
                           POSTHEAD #posthead ]],\
   C-CONT [ RELS <! !>, HCONS <! !> ] ].'

# A rule that says nothing about the subject or object. Should only ever be used as a supertype.
LOW_NMZ = 'low-nominalization-lex-rule := cat-change-with-ccont-lex-rule &\
                [ SYNSEM.LOCAL.CAT [ HEAD noun & \
			    [ NMZ +,\
			    MOD #mod ],\
		        VAL [ SUBJ < [ LOCAL [ CAT [ VAL.SPR < > ],\
				      	      CONT.HOOK.INDEX #subj ] ] >,\
			     SPEC #spec,\
			     SPR < [ OPT + ]> ],\
		       MC #mc,\
		       MKG #mkg,\
		       HC-LIGHT #hc-light,\
		       POSTHEAD #posthead ],\
                C-CONT [ RELS <! [ PRED "nominalized_rel",\
		       LBL #ltop,\
		       ARG0 ref-ind & #arg0,\
		       ARG1 #arg1 ] !>,\
	            HCONS <! qeq &\
		        [ HARG #arg1,\
		      LARG #larg ] !>,\
	            HOOK [ INDEX #arg0,\
		        LTOP #ltop ]],\
                DTR.SYNSEM.LOCAL [ CAT [ HEAD [ MOD #mod ],\
			     VAL [ SUBJ < [ LOCAL [ CONT.HOOK.INDEX #subj ] ] >,\
				   SPEC #spec  ],\
			     MC #mc,\
			     MKG #mkg,\
			     HC-LIGHT #hc-light,\
			     POSTHEAD #posthead ],\
		       CONT.HOOK [ LTOP #larg ]]].'


# A rule that allows case change on both subject and object and requires that there is an object.
# Only good for transitive verbs.
LOW_LEXRULE_NO_SUBJ_ID_NO_COMPS_ID = LOW_NO_SUBJ_NO_COMPS + ' := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CAT [ VAL.SPR < > ],\
				      	                                   CONT.HOOK.INDEX #obj ] ] > ],\
                DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CONT.HOOK.INDEX #obj ] ] > ] ].'

# A rule that identifies the object of mother and daughter and works for intransitive verbs, too.
# Still allows case change on the subject.
LOW_LEXRULE_NO_SUBJ_ID_COMPS_ID = LOW_NO_SUBJ_COMPS + ' := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS #comps ],\
                  DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS #comps ] ].'

# A rule that allows case change on the object and requires that there is an object.
# Does not allow case change on the subject.
# Only good for transitive verbs.
LOW_LEXRULE_SUBJ_ID_NO_COMPS_ID = 'low-nmz-subjid-trans-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CAT [ VAL.SPR < > ],\
				      	                                   CONT.HOOK.INDEX #obj ] ] >,\
				      	                SUBJ #subj ],\
                DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CONT.HOOK.INDEX #obj ] ] >,\
                                            SUBJ #subj ] ].'

# A rule that identifies the object of mother and daughter and works for intransitive verbs, too.
# Does not allow case change on the subject.
LOW_LEXRULE_SUBJ_ID_COMPS_ID = 'low-nmz-subjid-compsid-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS #comps,\
                                        SUBJ #subj ],\
                  DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS #comps,\
                                                SUBJ #subj ] ].'

#A non-branching rule for nominalized clauses to form a NP, with nominalized_rel for the MRS.
# For high nominalization.
NMZ_CLAUSE = '-nominalized-clause-phrase := basic-unary-phrase &\
                                    [ SYNSEM.LOCAL.CAT [ HEAD noun &\
                                                [ NMZ + ],\
            		                VAL [ SPR < [ OPT + ] >,\
                                            SPEC < >,\
                                            COMPS < >,\
            		                        SUBJ < #subj > ]],\
                                    C-CONT [ RELS <! [ PRED "nominalized_rel",\
            	    	            LBL #ltop,\
            		                ARG0 ref-ind & #arg0,\
            		                ARG1 #arg1 ] !>,\
            	                    HCONS <! qeq &\
                		            [ HARG #arg1,\
            	    	            LARG #larg ] !>,\
            	                    HOOK [ INDEX #arg0,\
            		                LTOP #ltop ]],\
                                    ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb &\
            					     [ NMZ + ],\
                				    VAL [ COMPS < >,\
            	    				  SUBJ < #subj >,\
            		    			  SPR < >,\
            			    		  SPEC < > ]],\
            			            CONT.HOOK [ LTOP #larg ]]]] > ].'

#A non-branching rule for nominalized clauses to form a NP, semantically emtpy.
NO_REL_NLZ_CLAUSE = '-no-rel-nominalized-clause-phrase := basic-unary-phrase &\
  [ SYNSEM [ LOCAL.CAT [ HEAD noun &\
                            [ NMZ + ],\
                         VAL [ COMPS < >,\
                                        SUBJ < >,\
                                        SPR < >,\
                                        SPEC < > ]]],\
    C-CONT [ RELS <! !>,\
	     HCONS <! !>,\
	     HOOK [ LTOP #ltop ] ],\
    ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb &\
                                       [ NMZ + ],\
                                  VAL [ COMPS < >,\
                                        SUBJ < >,\
                                        SPR < >,\
                                        SPEC < > ] ],\
		CONT.HOOK [ LTOP #ltop ] ] ] ] > ].'

#A non-branching rule for nominalized clauses to form a NP, with nominalized_rel for the MRS.
# For middle nominalization.
SUBJ_NMZ_CLAUSE = '-nominalized-clause-phrase := basic-unary-phrase &\
                          [ SYNSEM.LOCAL.CAT [ HEAD noun &\
                            [ NMZ + ],\
		       VAL [ SPR < [ OPT + ] >,'\
                           'COMPS < >,\
					  SUBJ < >,\
					  SPEC < > ]],\
    C-CONT [ RELS <! [ PRED "nominalized_rel",\
		       LBL #ltop,\
		       ARG0 ref-ind & #arg0,\
		       ARG1 #arg1 ] !>,\
	     HCONS <! qeq &\
		    [ HARG #arg1,\
		      LARG #larg ] !>,\
	     HOOK [ INDEX #arg0,\
		    LTOP #ltop ]],\
    ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb &\
					 [ NMZ + ],\
				    VAL [ COMPS < >,\
					  SUBJ < >,\
					  SPR < >,\
					  SPEC < > ]],\
			      CONT.HOOK [ LTOP #larg ]]]] > ].'

def customize_nmcs(mylang, ch, rules):
    """
    the main nominalized clause customization routine
    """
    update_lexical_rules(mylang, ch)
    for ns in ch.get('ns'):
        level = ns.get('level')
        nmzrel = ns.get('nmzRel')
        add_nmz_feature(mylang)
        mylang.add('+nvcdmo :+ [ MOD < > ].')
        add_nonevent_subj_rules(ch, level, mylang, rules)
        add_nmz_lexrules(ch, level, mylang)
        add_nmz_clause_phrases(level, mylang, nmzrel, rules)


def add_nmz_clause_phrases(level, mylang, nmzrel, rules):
    """
    Add non-branching rules which turn a nominalized clause into a NP.
    @param level: high, mid, or low.
    @param nmzrel: yes or no.
    """
    mylang.set_section('phrases')
    if level == 'mid':
        mylang.set_section('phrases')
        mylang.add(level + NMZ_CLAUSE)
        rules.add(level + '-nominalized-clause := ' + level + '-nominalized-clause-phrase.')
    if level == 'high':
        mylang.set_section('phrases')
        if nmzrel == 'no':
            mylang.add(level + NO_REL_NLZ_CLAUSE)
            rules.add(level + '-no-rel-nominalized-clause := ' + level + '-no-rel-nominalized-clause-phrase.')
        elif nmzrel == 'yes':
            mylang.add(level + SUBJ_NMZ_CLAUSE)
            rules.add(level + '-nominalized-clause := ' + level + '-nominalized-clause-phrase.')


def add_nmz_lexrules(ch, level, mylang):
    """
    Add appropriate lexical rule supertypes (definitions).
    @param level: high, mid, or low.
    """
    mylang.set_section('lexrules')
    if level == 'high':
        mylang.add(HIGH_OR_MID_LEXRULE_SUBJ_ID)
    if level == 'mid':
        if case_change('subj', ch):
            mylang.add(MID_LEXRULE_NO_SUBJ_ID)
        else:
            mylang.add(HIGH_OR_MID_LEXRULE_SUBJ_ID)
    if level == 'low':
        mylang.add(LOW_NMZ)
        if case_change('subj', ch):
            if case_change('obj', ch):
                mylang.add(LOW_LEXRULE_NO_SUBJ_ID_NO_COMPS_ID)
            mylang.add(LOW_LEXRULE_NO_SUBJ_ID_COMPS_ID)
        else:
            if case_change('obj', ch):
                mylang.add(LOW_LEXRULE_SUBJ_ID_NO_COMPS_ID)
            mylang.add(LOW_LEXRULE_SUBJ_ID_COMPS_ID)


def add_nonevent_subj_rules(ch, level, mylang, rules):
    """
    Add head-subject rules what work with non-events (e.g. nominalized things, nouns).
    """
    super = ''
    if level == 'low' or level == 'mid':
        mylang.set_section('phrases')
        wo = ch.get('word-order')
        if wo == 'free' or wo == 'v2':
            typename1 = 'non-event-subj-head'
            typename2 = 'non-event-head-subj'
            rules.add(typename1 + ' := ' + typename1 + '-phrase.')
            rules.add(typename2 + ' := ' + typename2 + '-phrase.')
            mylang.add(typename1 + '-phrase := head-final-head-nexus &' + NHS_SUPERTYPE + '&' + NHS_DEF)
            mylang.add(typename2 + '-phrase := head-initial-head-nexus &' + NHS_SUPERTYPE + '&' + NHS_DEF)
        else:
            if wo == 'osv' or wo == 'sov' or wo == 'svo' or wo == 'v-final':
                typename = 'non-event-subj-head'
                rules.add(typename + ' := ' + typename + '-phrase.')
                super = 'head-final'
            elif wo == 'ovs' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
                typename = 'non-event-head-subj'
                rules.add(typename + ' := ' + typename + '-phrase.')
                super = 'head-initial'
            if not typename:
                raise Exception('Invalid combination of word order and nominalization choices.')
            mylang.add(typename + '-phrase := ' + NHS_SUPERTYPE + '&' + super + '&' + NHS_DEF)
            if wo in ['svo', 'vos', 'sov'] \
                    or (wo == 'ovs' and len([cs for cs in ch.get('comps') if cs['clause-pos-extra']]) == 0):
                mylang.add(typename + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', merge=True)


def case_change(arg, ch):
    """
    @param arg: obj or subj
    @return: True if there is any nominalization lexical rule anywhere
    in the choices that specifies case change on either subject or object.
    """
    has_nmz = False
    case_change = False
    for vpc in ch.get('verb-pc'):
        for lrt in vpc['lrt']:
            has_nmz = False
            case_change = False
            for f in lrt['feat']:
                if f['name'] == 'nominalization':
                    has_nmz = True
                    if has_nmz and case_change:
                        return True
                if f['name'] == 'case' and f['head'] == arg:
                    case_change = True
                    if has_nmz and case_change:
                         return True
    return has_nmz and case_change

# This assumes that the lrt is associated with nominalization.
def case_change_lrt(arg, lrt):
    """
    @param arg: obj or subj
    @param lrt:
    @return: True if this lrt specifies case change on arg.
    """
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == arg:
            return True
    return False

def get_head_type(arg, lrt, ch):
    """
    Call a function from choices.py to determine what is the lexical rule's head.
    @param arg: obj or subj
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
    @param ch:
    @return: rules (list of tuples (lrt, nominalization_value (e.g. "low")).
    """
    rules = []
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'nominalization' in f['name']:
                    rules.append((lrt,f['value']))
    return rules

def update_lexical_rules(mylang, ch):
    """
    Create appropriate lexical rule subtypes for nominalization.
    Add an appropriate supertype to each nominalizing verbal position class.
    """
    path_subj = 'SYNSEM.LOCAL.CAT.VAL.SUBJ.FIRST.LOCAL.CAT.HEAD'
    path_comps = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD'
    for lrt,val in get_nmz_lexrules(ch):
        for ns in ch.get('ns'):
            if ns.get('name') == val:
                level = ns.get('level')
                if level == 'high':
                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                        [HIGH_OR_MID])
                if level == 'mid':
                    if case_change_lrt('subj', lrt):
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [MID])
                        mylang.set_section('lexrules')
                        subj_head_type = get_head_type('subj', lrt, ch)
                        mylang.add(MID + ' := [ ' + path_subj + ' ' + subj_head_type + '] > ].')
                    else:
                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [HIGH_OR_MID])
                if level == 'low':
                    if case_change_lrt('subj', lrt):
                        if case_change_lrt('obj', lrt):
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [LOW_NO_SUBJ_NO_COMPS])
                            mylang.set_section('lexrules')
                            subj_head_type = get_head_type('subj', lrt, ch)
                            mylang.add(LOW_NO_SUBJ_NO_COMPS + ' := [ ' + path_subj + ' ' + subj_head_type + '] > ].')
                            obj_head_type = get_head_type('obj', lrt, ch)
                            mylang.add(LOW_NO_SUBJ_NO_COMPS + ' := [ ' + path_comps + ' ' + obj_head_type + '] > ].')
                        else:
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [LOW_NO_SUBJ_COMPS])
                            mylang.set_section('lexrules')
                            subj_head_type = get_head_type('subj', lrt, ch)
                            mylang.add(LOW_NO_SUBJ_COMPS + ' := [ ' + path_subj + ' ' + subj_head_type + '] > ].')
                    else:
                        if case_change_lrt('obj', lrt):
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [LOW_SUBJ_NO_COMPS])
                            obj_head_type = get_head_type('obj', lrt, ch)
                            mylang.add(LOW_SUBJ_NO_COMPS + ' := [ ' + path_comps + ' ' + obj_head_type + '] > ].')
                        else:
                            lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + [LOW_SUBJ_COMPS])

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

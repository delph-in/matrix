###
# Constants
###

NHS_SUPERTYPE = 'basic-head-subj-phrase'
NHS_DEF = '[ HEAD-DTR.SYNSEM [ LOCAL [ CONT.HOOK.INDEX ref-ind ],\
                               NON-LOCAL [ QUE 0-dlist,\
                                           REL 0-dlist ]]\
            NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR < > ].'

# A rule that does not allow case change on subject or object.
# Only good for high and mid nominalization.
HIGH_OR_MID_LEXRULE_SUBJ_ID = 'high-or-mid-nominalization-lex-rule := cat-change-with-ccont-lex-rule & same-cont-lex-rule &\
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
MID_LEXRULE_NO_SUBJ_ID = 'mid-nominalization-lex-rule := cat-change-with-ccont-lex-rule & same-cont-lex-rule &\
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
			    [ MOD #mod ],\
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
LOW_LEXRULE_NO_SUBJ_ID_NO_COMPS_ID = 'low-nmz-no-subjid-trans-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CAT [ VAL.SPR < > ],\
				      	                                   CONT.HOOK.INDEX #obj ] ] >,\
				      	                SUBJ < [ LOCAL [ CAT.VAL.SPR < >,\
				      	                                 CONT.HOOK.INDEX #subj ] ] > ],\
                DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CONT.HOOK.INDEX #obj ] ] >,\
                                            SUBJ < [ LOCAL [ CAT.VAL.SPR < >,\
				      	                                    CONT.HOOK.INDEX #subj ] ] > ] ].'

# A rule that identifies the object of mother and daughter and works for intransitive verbs, too.
# Still allows case change on the subject.
LOW_LEXRULE_NO_SUBJ_ID_COMPS_ID = 'low-nmz-no-subjid-compsid-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS #comps,\
                                        SUBJ < [ LOCAL [ CAT.VAL.SPR < >,\
				      	                                 CONT.HOOK.INDEX #subj ] ] > ],\
                  DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS #comps,\
                                                SUBJ < [ LOCAL [ CAT.VAL.SPR < >,\
				      	                                 CONT.HOOK.INDEX #subj ] ] > ] ].'

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


NMZ_CLAUSE = '-nominalized-clause-phrase := basic-unary-phrase &\
                                    [ SYNSEM.LOCAL.CAT [ HEAD noun,\
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

NO_REL_NLZ_CLAUSE = '-no-rel-nominalized-clause-phrase := basic-unary-phrase &\
  [ SYNSEM [ LOCAL.CAT [ HEAD noun,\
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

SUBJ_NMZ_CLAUSE = '-nominalized-clause-phrase := basic-unary-phrase &\
                          [ SYNSEM.LOCAL.CAT [ HEAD noun,\
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
        add_features(mylang)
        mylang.add('+nvcdmo :+ [ MOD < > ].')
        super = ''
        if level == 'low' or level == 'mid':
            mylang.set_section('phrases')
            wo = ch.get('word-order')
            #OZ: special case for free and v2 word order. Haven't yet tested on V2!
            if wo == 'free' or wo == 'v2':
               typename1 = 'non-event-subj-head'
               typename2 = 'non-event-head-subj'
               rules.add(typename1 + ' := ' + typename1 + '-phrase.')
               rules.add(typename2 + ' := ' + typename2 + '-phrase.')
               mylang.add(typename1 + '-phrase := head-final-head-nexus &' + NHS_SUPERTYPE + '&' + NHS_DEF)
               mylang.add(typename2 + '-phrase := head-initial-head-nexus &' + NHS_SUPERTYPE + '&' +NHS_DEF)
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
                #if wo in [ 'sov', 'svo', 'ovs', 'vos']: #OZ: ovs can turn into vso with extraposed complements
                #TODO: Need to handle OVS still! It seems to be a special case,
                # if extraposition is involved (see also word_order.py)
                if wo in ['svo', 'vos', 'sov'] or (wo == 'ovs' \
                        and len([ cs for cs in ch.get('comps') if cs['clause-pos-extra'] ])==0):
                       # The above just checks if any complementation strategy involves extraposition;
                       # but what if there are several different ones? Am I sure that it
                       # will be taken care of by INIT
                       # on the clausal verb?
                    mylang.add(typename + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].',merge=True)
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

def case_change(arg, ch):
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

'''
This assumes that the lrt is associated with nominalization.
'''
def case_change_lrt(arg, lrt):
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == arg:
            return True
    return False

def get_head_type(arg, lrt, ch):
    head_type = ''
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == arg:
            head_type = ch.case_head(f['value'])
    return head_type

def update_lexical_rules(mylang, ch):
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'nominalization' in f['name']:
                    for ns in ch.get('ns'):
                        if ns.get('name') == f['value']:
                            level = ns.get('level')
                            if level == 'high':
                                lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                    ['high-or-mid-nominalization-lex-rule'])
                            if level == 'mid':
                                if case_change_lrt('subj', lrt):
                                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                  ['mid-nominalization-lex-rule'])
                                    mylang.set_section('lexrules')
                                    subj_head_type = get_head_type('subj', lrt, ch)
                                    mylang.add(
                                        'mid-nominalization-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + subj_head_type + '] > ].')
                                else:
                                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                  ['high-or-mid-nominalization-lex-rule'])
                            if level == 'low':
                                if case_change_lrt('subj', lrt):
                                    if case_change_lrt('obj', lrt):
                                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                      ['low-nmz-no-subjid-trans-lex-rule'])
                                        mylang.set_section('lexrules')
                                        subj_head_type = get_head_type('subj', lrt, ch)
                                        mylang.add(
                                            'low-nmz-no-subjid-trans-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + subj_head_type + '] > ].')
                                        obj_head_type = get_head_type('obj', lrt, ch)
                                        mylang.add(
                                            'low-nmz-no-subjid-trans-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + obj_head_type + '] > ].')
                                    else:
                                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                  ['low-nmz-no-subjid-compsid-lex-rule'])
                                        mylang.set_section('lexrules')
                                        subj_head_type = get_head_type('subj', lrt, ch)
                                        mylang.add(
                                            'low-nmz-no-subjid-compsid-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + subj_head_type + '] > ].')
                                else:
                                    if case_change_lrt('obj', lrt):
                                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                      ['low-nmz-subjid-trans-lex-rule'])
                                        obj_head_type = get_head_type('obj', lrt, ch)
                                        mylang.add(
                                            'low-nmz-subjid-trans-lex-rule := [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + obj_head_type + '] > ].')
                                    else:
                                        lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                                                      ['low-nmz-subjid-compsid-lex-rule'])

def add_features(mylang):
    mylang.set_section('addenda')
    mylang.add('head :+ [ NMZ bool ].')
    mylang.set_section('noun-lex')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')

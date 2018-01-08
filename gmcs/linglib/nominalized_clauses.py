###
# Constants
###

NHS_SUPERTYPE = 'basic-head-subj-phrase'
NHS_DEF = '[ HEAD-DTR.SYNSEM [ LOCAL [ CONT.HOOK.INDEX ref-ind ],\
                               NON-LOCAL [ QUE 0-dlist,\
                                           REL 0-dlist ]]\
            NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR < > ].'

HIGH_OR_MID_LEXRULE = 'high-or-mid-nominalization-lex-rule := cat-change-with-ccont-lex-rule & same-cont-lex-rule &\
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

# A rule that says nothing about the object. Should only ever be used as a supertype.
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
LOW_LEXRULE_NO_COMPS_ID = 'low-nmz-trans-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CAT [ HEAD noun,\
		                                                         VAL.SPR < > ],\
				      	                                   CONT.HOOK.INDEX #obj ] ] > ],\
                DTR.SYNSEM.LOCAL.CAT.VAL [ COMPS < [ LOCAL [ CAT.HEAD noun, ' \
              '                         CONT.HOOK.INDEX #obj ] ] > ] ].'

# A rule that identifies the object of mother and daughter and works for intransitive verbs, too.
# Still allows case change on the subject.
LOW_LEXRULE_COMPS_ID = 'low-nmz-compsid-lex-rule := low-nominalization-lex-rule &\
                [ SYNSEM.LOCAL.CAT.VAL.COMPS #comps,\
                  DTR.SYNSEM.LOCAL.CAT.VAL.COMPS #comps ].'


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
    if not 'ns' in ch:
        return None
    update_lexical_rules(ch)
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
        type_names = []
        if level == 'mid' or level == 'high':
            mylang.add(HIGH_OR_MID_LEXRULE)
            type_names.append('high-or-mid-nominalization-lex-rule')
        if level == 'low':
            mylang.add(LOW_NMZ)
            if case_change(ch):
                mylang.add(LOW_LEXRULE_NO_COMPS_ID)
                type_names.append('low-nmz-trans-lex-rule')
            mylang.add(LOW_LEXRULE_COMPS_ID)
            type_names.append('low-nmz-compsid-lex-rule')
        possible_subj_head_types, possibl_obj_head_types = argument_types(ch)
        for type_name in type_names:
            if len(possible_subj_head_types) == 1:
                mylang.add(type_name + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + possible_subj_head_types[0] + '] >,\
                                             DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + possible_subj_head_types[0] + '] > ].')
            else:
                i = 0
                while i < len(possible_subj_head_types):
                    subtype = possible_subj_head_types[i] + '-subj-' + type_name
                    mylang.add(subtype + ' := [SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + possible_subj_head_types[i] + '] >,\
                                               DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD ' + possible_subj_head_types[i] + '] > ].')
                    i += 1
        mylang.set_section('phrases')
        if level == 'mid':
            mylang.add(level + NMZ_CLAUSE)
            rules.add(level + '-nominalized-clause := ' + level + '-nominalized-clause-phrase.')
        if level == 'high':
            if nmzrel == 'no':
                mylang.add(level + NO_REL_NLZ_CLAUSE)
                rules.add(level + '-no-rel-nominalized-clause := ' + level + '-no-rel-nominalized-clause-phrase.')
            elif nmzrel == 'yes':
                mylang.add(level + SUBJ_NMZ_CLAUSE)
                rules.add(level + '-nominalized-clause := ' + level + '-nominalized-clause-phrase.')


def argument_types(ch):
    possible_subj_head_types = []
    possible_obj_head_types = []
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'nominalization' in f['name']:
                    inputs = vpc.get('inputs')
                    for p in ch.patterns():
                        if 'clausal' in p[1]:
                            continue
                        rule_pattern = p[2]

                        p = p[0].split(',')  # split off ',dirinv', if present
                        dir_inv = ''
                        if len(p) > 1 and p[1] == 'dirinv':
                            dir_inv = 'dir-inv-'

                        if not rule_pattern:
                            c = p[0].split('-')  # split 'agentcase-patientcase'
                            if p[0] == 'trans' or len(c) > 1:  # transitive
                                if p[0] == 'trans':
                                    a_head = ch.case_head()
                                    o_head = ch.case_head()
                                else:
                                    a_head = ch.case_head(c[0])
                                    o_head = ch.case_head(c[1])
                                if a_head != '' and a_head not in possible_subj_head_types:
                                    possible_subj_head_types.append(a_head)
                                if o_head != '' and o_head not in possible_obj_head_types:
                                    possible_obj_head_types.append(o_head)
                            else:  # intransitive
                                if c[0] == 'intrans':
                                    s_head = ch.case_head()
                                else:
                                    s_head = ch.case_head(c[0])
                                if s_head != '' and s_head not in possible_subj_head_types:
                                    possible_subj_head_types.append(s_head)
    return possible_subj_head_types, possible_obj_head_types

def case_change(ch):
    for vpc in ch.get('verb-pc'):
        for lrt in vpc['lrt']:
            has_nmz = False
            case_change = False
            for f in lrt['feat']:
                if f['name'] == 'nominalization':
                    has_nmz = True
                    if has_nmz and case_change:
                        return True
                if f['name'] == 'case' and f['head'] == 'obj':
                    case_change = True
                    if has_nmz and case_change:
                        return True
    return has_nmz and case_change

'''
This assumes that the lrt is associated with nominalization.
'''
def case_change_lrt(lrt):
    for f in lrt['feat']:
        if f['name'] == 'case' and f['head'] == 'obj':
            return True
    return False

def update_lexical_rules(ch):
    for vpc in ch['verb-pc']:
        for lrt in vpc['lrt']:
            for f in lrt['feat']:
                if 'nominalization' in f['name']:
                    for ns in ch.get('ns'):
                        if ns.get('name') == f['value']:
                            level = ns.get('level')
                            if level == 'mid' or level == 'high':
                                lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                    ['high-or-mid-nominalization-lex-rule'])
                            if level == 'low':
                                if case_change_lrt(lrt):
                                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                     ['low-nmz-trans-lex-rule'])
                                else:
                                    lrt['supertypes'] = ', '.join(lrt['supertypes'].split(', ') + \
                                        ['low-nmz-compsid-lex-rule'])


def add_features(mylang):
    mylang.set_section('addenda')
    mylang.add('head :+ [ NMZ bool ].')
    mylang.set_section('noun-lex')
    mylang.add('noun-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')
    mylang.set_section('verb-lex')
    mylang.add('verb-lex := [ SYNSEM.LOCAL.CAT.HEAD.NMZ - ].')

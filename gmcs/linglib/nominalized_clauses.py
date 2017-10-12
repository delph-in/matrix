def customize_nmcs(mylang, ch, rules, lrules):
    """
    the main nominalized clause customization routine
    """
    for ns in ch.get('ns'):
        name = ns.get('name')
        level = ns.get('level')
        nmzrel = ns.get('nmzRel')

        if level == 'low':
            print('todo- low')
            for vpc in ch['verb-pc']:
                for lrt in vpc['lrt']:
                    for f in lrt['feat']:
                         if 'nominalization' in f['name']:
                            if 'supertypes' in lrt:
                                lrt['supertypes'] += ', nominalization-lex-rule'
                         else:
                                lrt['supertypes'] = 'nominalization-lex-rule'
            mylang.set_section('verblex')
            mylang.add('nominalization-lex-rule := cat-change-with-ccont-lex-rule &\
  [ SYNSEM.LOCAL.CAT [ HEAD noun & \
			    [ FORM #form,\
			      AUX #aux,\
			      INIT #init,\
			      MOD #mod ],\
		       VAL [ SUBJ < [ LOCAL [ CAT [ HEAD noun,\
		                            VAL.SPR < > ],\
				      	      CONT.HOOK.INDEX #subj ]] >,\
			     COMPS #comps,\
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
    DTR.SYNSEM.LOCAL [ CAT [ HEAD [ FORM #form,\
				    AUX #aux,\
				    INIT #init,\
				    MOD #mod ],\
			     VAL [ SUBJ < [ LOCAL.CONT.HOOK.INDEX #subj ] >,\
				   COMPS #comps,\
				   SPEC #spec  ],\
			     MC #mc,\
			     MKG #mkg,\
			     HC-LIGHT #hc-light,\
			     POSTHEAD #posthead ],\
		       CONT.HOOK [ LTOP #larg ]]].')
            lrules.add('nominalized-lex-rule')
        elif level == 'mid':
            print('todo- mid')
        elif level == 'high':
            mylang.set_section('phrases')
            if nmzrel == 'no':
                mylang.add('nominalized-clause-phrase := basic-unary-phrase &\
                                        [ SYNSEM.LOCAL.CAT [ HEAD noun,\
                		                VAL.SPR < > ] ].')
            elif nmzrel == 'yes':
                mylang.add('nominalized-clause-phrase := basic-unary-phrase &\
                        [ SYNSEM.LOCAL.CAT [ HEAD noun,\
		                VAL.SPR < > ],\
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
			            CONT.HOOK [ LTOP #larg ]]]] > ].')
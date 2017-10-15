def customize_nmcs(mylang, ch, rules, lrules):
    """
    the main nominalized clause customization routine
    """
    for ns in ch.get('ns'):
        name = ns.get('name')
        level = ns.get('level')
        nmzrel = ns.get('nmzRel')
        print(nmzrel)

        mylang.set_section('addenda')
        mylang.add('head :+ [ NMZ bool,\
	                        FORM form,\
	                        AUX bool,\
	                        INIT bool ].')
        mylang.add('+nvcdmo :+ [ MOD < > ].')
        mylang.set_section('features')
        mylang.add('form := *top*.')
        mylang.add('nonfinite := form.')
        mylang.add('finite := form.')

        for vpc in ch['verb-pc']:
            for lrt in vpc['lrt']:
                for f in lrt['feat']:
                    if 'nominalization' in f['name']:
                        if 'supertypes' in lrt:
                            lrt['supertypes'] += ', nominalization-lex-rule'
                        else:
                            lrt['supertypes'] = 'nominalization-lex-rule'

        if level == 'low' or 'mid':
            mylang.set_section('phrases')
            mylang.add('non-event-subj-head-phrase := basic-head-subj-phrase & head-final &\
                            [ SYNSEM.LOCAL.CAT.HEAD [ FORM #form ],\
                                HEAD-DTR.SYNSEM [ LOCAL [ CONT.HOOK.INDEX ref-ind,\
                                                        CAT [ HEAD [ FORM #form ],\
            				                                VAL.COMPS < > ]],\
                                                  NON-LOCAL [ QUE 0-dlist,\
                                                                REL 0-dlist ]]\
                                NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SPR < > ].')
            rules.add('non-event-subj-head := non-event-subj-head-phrase.')
        if level == 'mid' or 'high':
            mylang.set_section('lexrules')
            mylang.add('nominalization-lex-rule := cat-change-with-ccont-lex-rule &\
  [ SYNSEM.LOCAL [ CONT.HOOK.INDEX event,\
                    CAT [ HEAD verb & \
			    [ NMZ +,\
			      FORM #form,\
			      AUX #aux,\
			      INIT #init,\
			      MOD #mod ],\
		       VAL [ SUBJ < [ LOCAL [ CAT [ HEAD noun &\
						   [ CASE gen ],\
						  VAL.SPR < > ],\
						CONT.HOOK.INDEX #subj]] >,\
			     COMPS #comps,\
			     SPR #spr,\
			     SPEC #spec ],\
		       MC #mc,\
		       MKG #mkg,\
		       HC-LIGHT #hc-light,\
		       POSTHEAD #posthead ]],\
    DTR.SYNSEM.LOCAL.CAT [ HEAD [ FORM #form,\
				 AUX #aux,\
				 INIT #init,\
				 MOD #mod ],\
			   VAL [ SUBJ < [ LOCAL.CONT.HOOK.INDEX #subj ] >,\
				COMPS #comps,\
				 SPR #spr,\
				 SPEC #spec ],\
			   MC #mc,\
			   MKG #mkg,\
			   HC-LIGHT #hc-light,\
			   POSTHEAD #posthead ]].')
            lrules.add('nom-lex-rule := nominalized-lex-rule.')

        if level == 'low':
            print('todo- low')
            mylang.set_section('lexrules')
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
            lrules.add('nom-lex-rule := nominalized-lex-rule.')
        elif level == 'mid':
            mylang.set_section('phrases')
            mylang.add('nominalized-clause-phrase := basic-unary-phrase &\
                                    [ SYNSEM.LOCAL.CAT [ HEAD noun,\
            		                VAL [ SPR < >,\
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
            			            CONT.HOOK [ LTOP #larg ]]]] > ].')
            rules.add('nominalized-clause := nominalized-clause-phrase.')
        elif level == 'high':
            mylang.set_section('phrases')
            if nmzrel == 'no':
                mylang.add('nominalized-clause-phrase := basic-unary-phrase &\
                                        [ SYNSEM.LOCAL.CAT [ HEAD noun,\
                		                VAL [ SPR < [ OPT + ] >,\
                		                        COMPS < >,\
                		                        SPEC < >,\
                		                        SUBJ < > ]],\
                                        ARGS < [ SYNSEM [ LOCAL [ CAT [ HEAD verb &\
                					     [ NMZ + ],\
                    				    VAL [ COMPS < >,\
                	    				  SUBJ < >,\
                		    			  SPR < >,\
                			    		  SPEC < > ]],\
                			            CONT.HOOK [ LTOP #larg ]]]] > ].')
                rules.add('nominalized-clause := nominalized-clause-phrase.')
            elif nmzrel == 'yes':
                mylang.add('nominalized-clause-phrase := basic-unary-phrase &\
                          [ SYNSEM.LOCAL.CAT [ HEAD noun,\
		       VAL [ SPR < [ OPT + ] >,'
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
			      CONT.HOOK [ LTOP #larg ]]]] > ].')
                rules.add('nominalized-clause := nominalized-clause-phrase.')
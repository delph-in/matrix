def customize_nmcs(mylang, ch, rules, irules):
    """
    the main nominalized clause customization routine
    """
    for ns in ch.get('ns'):
        name = ns.get('name')
        level = ns.get('level')
        nmzrel = ns.get('nmzRel')

        if nmzrel == 'yes':
            #todo
            print('todo')
        else:
            if level == 'low':
                print('todo')
            elif level == 'mid':
                print ('todo')
            elif level == 'high':
                mylang.set_section('phrases')
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
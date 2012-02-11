
######################################################################
# create_comparative_basic_type()
#   Create the type definitions associated with comparatives

# GERMANIC-BASED LIBRARY ONLY!!!!
# This library contains possibilities to go beyond Germanic languages
# but is not based on any cross-linguistic research whatsoever!


def create_comparative_basic_type(ch, mylang, lexicon):
  typedef = \
  '''comparative-creating-lex-rule := same-non-local-lex-rule &
			     same-modified-lex-rule &
			     same-light-lex-rule &
			     same-ctxt-lex-rule &
			     same-agr-lex-rule &
			     same-head-lex-rule &
			     same-hc-light-lex-rule &
			     same-posthead-lex-rule &
                             basic-one-arg &
			     same-mc-lex-rule & inflecting-lex-rule &
  [ SYNSEM.LOCAL [ CAT [ HEAD adj,
			 VAL [ SUBJ #subj,
			       COMPS < #comp & [ OPT +,
                                         LOCAL [ CAT [ VAL.COMPS < > ],
						 CONT.HOOK.INDEX #arg2 ]] >,
			       SPR #spr,
			       SPEC #spec ] ],
                   CONT [ HOOK #hook,
		   	  HCONS #hcons,
			  RELS <! #rel & [ ARG0 #arg1 ], arg12-ev-relation &
                                     [ PRED "_comp_rel",
				       ARG1 #arg1,
				       ARG2 #arg2] !> ] ],
    DTR [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj,
				 COMPS < >,
				 SPR #spr,
				 SPEC #spec ],
                         CONT [ HOOK #hook,
	   		        HCONS #hcons,
			        RELS <! #rel !> ] ] ],
    ARG-ST < #comp > ].'''
  mylang.add(typedef)
  head = ''
  if ch.get('comparative-comp-head'):
    head = ch.get('comparative-comp-head')
    mylang.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + head + ' ] > ].')
  if ch.get('comparative-comp-form'):
    form = ch.get('comparative-comp-form')
    superform = ''
    if head == 'adp':
      superform += 'p'
      create_marker_position(mylang, form, lexicon)
    superform += 'form'
    mylang.add(form + ' := ' + superform + '.')
    mylang.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.FORM ' + form + ' ] > ].')


def create_marker_position(mylang, form, lexicon):
  ###TO DO: the case marking should probably go
  my_adp = \
   '''comp-marking-adp-lex := basic-marking-only-adp-lex & norm-sem-lex-item &
    [ SYNSEM.LOCAL [ CAT [ VAL.COMPS < [ LOCAL [ CAT.HEAD.CASE nom,
						 CONT.HOOK.INDEX #arg1 ] ] > ],
		     CONT.RELS <! arg1-ev-relation &
                                     [ PRED "_ellipis_ref_rel",
				       ARG1 #arg1 ] !> ] ].'''
  mylang.add(my_adp)
  mylang.add('comp-marking-adp-lex := \
     [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')

  lexicon.add(form + '-comparative := comp-marking-adp-lex & \
                    [ STEM < "' + form + '" > ].') 


######################################################################
# create_comparative_basic_type()
#   Create the type definitions associated with comparatives

# GERMANIC-BASED LIBRARY ONLY!!!!
# This library contains possibilities to go beyond Germanic languages
# but is not based on any cross-linguistic research whatsoever!


from gmcs.utils import TDLencode

def create_comparative_basic_type(ch, mylang, lexicon, trigger, climb_files):
  climb_comp = climb_files.get('compar')
  typedef = \
  '''comp-or-superlative-creating-lex-rule := same-non-local-lex-rule &
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
						 CONT.HOOK.LTOP #arg2 ]] >,
			       SPR #spr,
			       SPEC #spec ] ],
                   CONT [ HOOK #hook,
		   	  HCONS #hcons,
			  RELS <! #rel & [ ARG0 #arg1 ], arg12-ev-relation &
                                     [ ARG1 #arg1,
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
  mylang.add('comparative-creating-lex-rule := \
                                    comp-or-superlative-creating-lex-rule & \
                [ SYNSEM.LOCAL.CONT.RELS <! [ ], [ PRED "_comp_rel" ] !> ].')
  mylang.add('superlative-creating-lex-rule := \
                                    comp-or-superlative-creating-lex-rule & \
                [ SYNSEM.LOCAL.CONT.RELS <! [ ], [ PRED "_superl_rel" ] !> ].')
  climb_comp.add(typedef)
  climb_comp.add('comparative-creating-lex-rule := \
                                    comp-or-superlative-creating-lex-rule & \
                [ SYNSEM.LOCAL.CONT.RELS <! [ ], [ PRED "_comp_rel" ] !> ].')
  climb_comp.add('superlative-creating-lex-rule := \
                                    comp-or-superlative-creating-lex-rule & \
                [ SYNSEM.LOCAL.CONT.RELS <! [ ], [ PRED "_superl_rel" ] !> ].')
  head = ''
  if ch.get('comparative-comp-head'):
    head = ch.get('comparative-comp-head')
    mylang.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + head + ' ] > ].')
    climb_comp.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD ' + head + ' ] > ].')
  
  form = ''
  superform = ''
###take care of old and new version
  for f in ch.get('comparative-comp-form', []):
    form = f.get('name')
    form += '+'

  form = form.strip('+')
  if head == 'adp':
    superform += 'p'
    create_marker_position(mylang, form, lexicon, trigger, climb_comp)
    superform += 'form'

  if form and superform:
    mylang.add(form + ' := ' + superform + '.')
    climb_comp.add(form + ' := ' + superform + '.')
  elif form:
    mylang.add(form + ' := ' + form + '.')
    climb_comp.add(form + ' := ' + form + '.')
  mylang.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.FORM ' + form + ' ] > ].')
  climb_comp.add('comparative-creating-lex-rule := \
      [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD.FORM ' + form + ' ] > ].')

  stype = \
      '''basic-comp-super-lex := lex-item &
  [ SYNSEM [ LOCAL [ CAT [ HEAD [ MOD < [ ] > ] ],
		     CONT [ HOOK [ XARG #ind,
                                   INDEX #arg0  ],
                            RELS.LIST < [ LBL #hand,
                                          ARG1 #ind ],
                                        #altkeyrel &
                                        [ LBL #hand,
                                          ARG0 event,
                                          ARG1 #arg0 ] > ] ],
             LKEYS.ALTKEYREL #altkeyrel ] ].
      '''
  sub_t1 = \
  '''basic-comparative-lex := basic-comp-super-lex &
     [ SYNSEM.LOCAL.CONT.RELS.LIST < [ ], [ PRED "comp_rel" ] > ].
  '''
  sub_t2 = \
  '''basic-superlative-lex := basic-comp-super-lex &
     [ SYNSEM.LOCAL.CONT.RELS.LIST < [ ], [ PRED "superl_rel" ] > ].
  '''
  mylang.add(stype)
  mylang.add(sub_t1)
  mylang.add(sub_t2)
  climb_comp.add(stype)
  climb_comp.add(sub_t1)
  climb_comp.add(sub_t2)

def create_marker_position(mylang, form, lexicon, trigger, climb_comp):
  ###TO DO: the case marking should probably go
  my_adp = \
   '''comp-marking-adp-lex := basic-marking-only-adp-lex & norm-sem-lex-item &
    [ SYNSEM.LOCAL [ CAT [ VAL.COMPS < [ LOCAL [ CAT.HEAD.CASE nom,
						 CONT.HOOK.INDEX #arg1 ] ] > ],
		     CONT.RELS <! arg1-ev-relation &
                                     [ PRED "_ellipis_ref_rel",
				       ARG1 #arg1 ] !> ] ].'''
  mylang.add(my_adp)
  climb_comp.add(my_adp)
  stype = form + '-comp-marking-adp-lex'
  mylang.add(stype + ' := comp-marking-adp-lex & \
     [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
  climb_comp.add(stype + ' := comp-marking-adp-lex & \
     [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')

  lexicon.add(form + '-comparative := comp-marking-adp-lex & \
                    [ STEM < "' + form + '" > ].') 
  climb_comp.add(form + '-comparative := comp-marking-adp-lex & \
                    [ STEM < "' + form + '" > ].',section='lexicon') 

  grdef = TDLencode(form) +'_gr := generator_rule & \
                   [ CONTEXT [ RELS <! [ ARG0.SF ques ] !> ], \
                     FLAGS.TRIGGER "' + TDLencode(form) + '" ].'
  trigger.add(grdef)

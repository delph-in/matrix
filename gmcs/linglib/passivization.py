
######################################################################
# customize_passivization()
#   Create the type definitions associated with the user's choices
#   about passivization

# GERMANIC-BASED LIBRARY ONLY!!!!
# This library contains possibilities to go beyond Germanic languages
# but is not based on any cross-linguistic research whatsoever!

def customize_passivization(ch, mylang, lrules, lexicon, climb_files):

  climb_pass = climb_files.get('pass')
  climb_pass.set_section('mylang')
  vc = ch.get('vc-analysis')
  passive = ch.get('pass',[])

#careful: only one passive form allowed at present

  for p in passive:
    marking = p.get('marking')
    form = ''
    if marking == 'aux':
      form = 'pass-' + p.get('form')
    elif marking == 'morph':
      form = p.get('form')
    sform = p.get('dsubj-form')
     
###
    if form:
      mylang.add(form + ' := nonfinite.', section='features')
      climb_pass.add(form + ' := nonfinite.', comment='section=features')

    
###only lexical rule for now
###starting assumption: direct-object becomes subject
###subject becomes optional first argument on COMPS list

######SYNTACTIC PROPERTIES: ONLY COMP CHANGES, BUT LINKING CHANGES

    typedef = \
    '''gen-passive-lex-rule := local-change-only-lex-rule &
  [ SYNSEM.LOCAL [ CAT [ EDGE #edge,
                       VAL.SPR #spr,
                       HEAD verb &
                            [ MOD #mod,
                              FORM pass-participle,
                              AUX #aux,
                              INV #inv & - ],
                       VC #vc,
                       MC #mc ],
                   CONT [ HOOK [ GTOP #gtop,
				 LTOP #ltop, 
				 INDEX #index ],
			  RELS #rels,
			  HCONS #hcons ],
		   COORD #coord,
		   COORD-REL #coord-rel,
		   COORD-STRAT #coord-strat ],
    DTR.SYNSEM.LOCAL [ CAT [ EDGE #edge,
                           VAL.SPR #spr,
                           HEAD verb &
                                [ MOD #mod,
                                  FORM participle,
                                  AUX #aux,
                                  INV #inv ],
                           VC #vc,
                           MC #mc ], 
		       CONT [ HOOK [ GTOP #gtop,
				     LTOP #ltop,
				     INDEX #index ],
			      RELS #rels,
			      HCONS #hcons ],
		       COORD #coord,
		       COORD-REL #coord-rel,
		       COORD-STRAT #coord-strat ] ].'''
    mylang.add(typedef)
    climb_pass.add(typedef)

    pas_typedef = '''passive-lex-rule := gen-passive-lex-rule &
        [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < [ NON-LOCAL #nlo,
                                      LOCAL [ CONT #arg2 & [ HOOK.INDEX #xarg ],
                                              CAT.HEAD noun &
                                                       [ CASE nom ] ] ] >,
                             COMPS < [ NON-LOCAL #nls,
                                       OPT +,
                                       LOCAL [ CONT #arg1,
                                               CAT [ VAL.COMPS < >,
                                                     HEAD adp &
                                                          [ MOD < >,
                                                            FORM ''' + sform + '''] ] ] ] . #othercomps > ],
                         CONT.HOOK.XARG #xarg ],
          DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [ LOCAL.CONT #arg1,
                                          NON-LOCAL #nls ] >,
                                 COMPS < [ NON-LOCAL #nlo,
                                           LOCAL [ CONT #arg2,
                                                   CAT.HEAD noun &
                                                            [ CASE acc ] ] ] . #othercomps > ] ]. '''

    mylang.add(pas_typedef)
    climb_pass.add(pas_typedef)
    if ch.get('imp-passive') == 'yes':
      imp_p_typedef = '''imp-passive-lex-rule := gen-passive-lex-rule & 
         [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < [ LOCAL.CONT.HOOK.INDEX expl-ind ] >,
                                  COMPS < [ NON-LOCAL #nls,
                                       OPT +,
                                       LOCAL [ CONT #arg1,
                                               CAT [ VAL.COMPS < >,
                                                     HEAD adp &
                                                          [ MOD < >,
                                                            FORM ''' + sform + '''] ] ] ] . #othercomps > ],  
           DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < [ LOCAL.CONT #arg1,
                                          NON-LOCAL #nls ] >,
                                 COMPS < #othercomps > ] ].'''
      mylang.add(imp_p_typedef)
      climb_pass.add(imp_p_typedef)

    if ch.get('verb-to-adj-passive') == 'yes':
      v2a_typedef = '''participle-to-adj-rule := local-change-only-lex-rule &
  [ SYNSEM [ LOCAL [ CONT [ HOOK [ GTOP #gtop,
				   LTOP #ltop,
				   INDEX #index,
				   XARG #xarg ],
                          RELS #rels,
                          HCONS #hcons ],
                   COORD #coord,
                   COORD-REL #coord-rel,
                   COORD-STRAT #coord-strat,
                   CAT [ EDGE #edge,
                         VAL [ SPR #spr,
                               SUBJ < >,
                               COMPS < [ LOCAL [ CONT #arg1,
                                                     CAT [ VAL.COMPS < >,
                                                           HEAD adp &
                                                              [ MOD < >,
                                                                FORM ''' + sform + '''] ] ],
					     NON-LOCAL #nls,
			                     OPT + ] . #vcomps > ],
                         HEAD adj &
                              [ MOD < [ NON-LOCAL #nlo, 
					LOCAL.CONT #arg2 & [ HOOK.INDEX #xarg ] ] > ],
                         VC #vc,
                         MC #mc,
                         VFRONT + ] ],
	     NON-LOCAL #non-loc ],
    DTR.SYNSEM [ LOCAL [ CONT [ HOOK [ GTOP #gtop,
                                     LTOP #ltop,
                                     INDEX #index ],
                              RELS #rels,
                              HCONS #hcons ],
                       COORD #coord,
                       COORD-REL #coord-rel,
                       COORD-STRAT #coord-strat,
                       CAT [ EDGE #edge,
                             VAL [ SPR #spr,
				   SUBJ < [ LOCAL.CONT #arg1,
                                            NON-LOCAL #nls ] >,
                                   COMPS < [  NON-LOCAL #nlo,
					      LOCAL [ CONT #arg2,
						      CAT.HEAD noun &
                                                          [ CASE acc ] ] ] . #vcomps  > ],
                             VC #vc,
                             MC #mc,
                             VFRONT na-or-- ] ],
		 NON-LOCAL #non-loc ] ].'''
      mylang.add(v2a_typedef)
      climb_pass.add(v2a_typedef)

    if ch.get('verbal-particles') == 'yes' or ch.get('circumpositions') == 'yes':
      mylang.add('gen-passive-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.PART-FORM #pf, \
                          DTR.SYNSEM.LOCAL.CAT.HEAD.PART-FORM #pf ].')
      climb_pass.add('gen-passive-lex-rule := [ SYNSEM.LOCAL.CAT.HEAD.PART-FORM #pf, \
                          DTR.SYNSEM.LOCAL.CAT.HEAD.PART-FORM #pf ].')
    if ch.get("edge-related-res") == "yes":
      mylang.add('gen-passive-lex-rule := [ SYNSEM.LOCAL.CAT.EDGE #edge, \
                            DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
      climb_pass.add('gen-passive-lex-rule := [ SYNSEM.LOCAL.CAT.EDGE #edge, \
                            DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
    lrules.add('passive-lr := passive-lex-rule.')
    climb_pass.add('passive-lr := passive-lex-rule.',section='lrules') 
    if ch.get('imp-passive') == 'yes':
      lrules.add('imp-passive-lr := imp-passive-lex-rule.')
      climb_pass.add('imp-passive-lr := imp-passive-lex-rule.',section='lrules')

    if ch.get('verb-to-adj-passive') == 'yes': 
      lrules.add('part-to-adj := participle-to-adj-rule.')
      climb_pass.add('part-to-adj := participle-to-adj-rule.',section='lrules')

    lr_n = 'gen-passive-lex-rule'
    mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
    climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
    if marking == 'aux':
      mylang.add(lr_n + ' := \
           [ DTR.SYNSEM.LOCAL.CAT.HEAD.FORM ' + p.get('form') + ' ].')
      climb_pass.add(lr_n + ' := \
           [ DTR.SYNSEM.LOCAL.CAT.HEAD.FORM ' + p.get('form') + ' ].')
    elif marking == 'morph':
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].')
      climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.FORM #form ].')
###should be done properly using feature_code

    arg_res = p.get('arg_res', [])
    mtr_path = '[ SYNSEM.LOCAL.CAT.VAL.'
    dtr_path = '[ DTR.SYNSEM.LOCAL.CAT.VAL.'
    for res in arg_res:
      level = res.get('level')
      if level == 'mtr':
        genpath = mtr_path 
      else:
        genpath = dtr_path
      arg = res.get('arg')
      genpath += arg.upper() + '.FIRST.LOCAL.CAT.' 
      feats = res.get('feat', [])
      for f in feats:
        path = genpath  
        val = f.get('val')
        fname = f.get('name')
        if val == 'elist':
          val = '< >'
        if fname == 'head':
          path += 'HEAD ' + val
        elif fname == 'case':
          path += 'HEAD.CASE ' + val
        elif fname == 'form':
          path += 'HEAD.FORM ' + val
        elif fname == 'mod':
          path += 'HEAD.MOD ' + val
#comps can only do empty list for now
        elif fname == 'comps':
          path += 'VAL.COMPS ' + val
             
        mylang.add(lr_n + ' := ' + path + ' ].')
        climb_pass.add(lr_n + ' := ' + path + ' ].')

###passing up features that need to be passed up
    vc = False
    if ch.get('has-aux') == 'yes':
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX #aux, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.AUX #aux ].')
      climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX #aux, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.AUX #aux ].')
    if ch.get('q-inv'):
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv & -, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
      climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv & -, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
    if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2':
      vc = True
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VC #vc , \
                                 DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.MC #mc, \
                                 DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
      climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VC #vc , \
                                 DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
      climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.MC #mc, \
                                 DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
    if ch.get('vc-analysis') == 'basic':
#        if ch.get('v2-analysis') == 'mc':
#          mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VFRONT #vfront, \
#                                 DTR.SYNSEM.LOCAL.CAT.VFRONT #vfront ].')
#          mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VFRONT na-or-- ].')
#         
#        else:
        mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VFRONT +, \
                                 DTR.SYNSEM.LOCAL.CAT.VFRONT na-or-- ].')
        climb_pass.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VFRONT +, \
                                 DTR.SYNSEM.LOCAL.CAT.VFRONT na-or-- ].')
    else:
      typedef = \
        '''change-arg-order-rule := const-val-change-only-lex-rule &
  [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,
                             COMPS < #comp2,
                                     #comp1 >,
                             SPR #spr,
                             SPEC #spec ],
                       VC #vc,
                       VFRONT -,
                       EDGE #ed ],
    DTR.SYNSEM.LOCAL.CAT [ VAL [ SUBJ #subj,
                                 COMPS < #comp1,
                                         #comp2 >,
                                 SPR #spr,
                                 SPEC #spec ],
                           HEAD verb &
                                [ FORM participle,
                                  AUX -,
				  INV - ],
                           VC #vc,
                           VFRONT +,
                           EDGE #ed ] ].'''
      mylang.add(typedef)
      lrules.add('change-arg-order := change-arg-order-rule.')
      climb_pass.add(typedef)
      climb_pass.add('change-arg-order := change-arg-order-rule.',section='lrules')
      
###If the subject is marked by an adposition after passivization,
###we need something like a 'case-marking-adposition', but without case
###fix: prep or postp also general form plus derived forms...
    dsubj_mark = p.get('dem-subj-mark')
    if 'adp' in dsubj_mark:    
      typedef = \
      '''passive-marking-adp-lex := basic-marking-only-adp-lex & \
                        raise-sem-lex-item.'''
      mylang.add(typedef)
      climb_pass.add(typedef)
      if vc:
        mylang.add('passive-marking-adp-lex := \
                          [ SYNSEM.LOCAL.CAT.VC na-or-- ].')
        climb_pass.add('passive-marking-adp-lex := \
                          [ SYNSEM.LOCAL.CAT.VC na-or-- ].')
      if 'case' in dsubj_mark:
        m_parts = dsubj_mark.split('-')
        case = m_parts[1]
        mylang.add('passive-marking-adp-lex := \
                         [ ARG-ST < [ LOCAL.CAT.HEAD.CASE ' + case + ' ] > ].')
        climb_pass.add('passive-marking-adp-lex := \
                         [ ARG-ST < [ LOCAL.CAT.HEAD.CASE ' + case + ' ] > ].')
      sform = p.get('dsubj-form')
####make sure pform is introduced....
      
      sf = ''
      if ch.get('nachfeld') == 'yes' and 'pform' in ch.get('nf-forms'):
        sf += 'nf-'
      sf += 'form'
      mylang.add('pform := ' + sf + '.',section='features')
      mylang.add('passive-marking-adp-lex := \
                  [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + sform + ' ].')
      mylang.add(sform + ' := pform.',section='features')
      climb_pass.add('passive-marking-adp-lex := \
                  [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + sform + ' ].')
      climb_pass.add(sform + ' := pform.',comment='section=features')
      lexicon.add(sform + '-passive := passive-marking-adp-lex & \
                    [ STEM < "' + sform + '" > ].') 
      climb_pass.add(sform + '-passive := passive-marking-adp-lex & \
                    [ STEM < "' + sform + '" > ].',section='lexicon') 
      

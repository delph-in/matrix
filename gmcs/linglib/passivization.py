
######################################################################
# customize_passivization()
#   Create the type definitions associated with the user's choices
#   about passivization

# GERMANIC-BASED LIBRARY ONLY!!!!
# This library contains possibilities to go beyond Germanic languages
# but is not based on any cross-linguistic research whatsoever!

def customize_passivization(ch, mylang, lrules, lexicon):

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
###
    if form:
      mylang.add(form + ' := nonfinite.', section='features')

    
###only lexical rule for now
###starting assumption: direct-object becomes subject
###subject becomes optional first argument on COMPS list

    typedef = \
    '''
  passive-lex-rule := cat-change-only-lex-rule &
 [ SYNSEM [ LOCAL.CAT [ HEAD verb & [ MOD #mod ],
			VAL [ SUBJ < [ LOCAL.CONT #arg2,
                                       NON-LOCAL #nlo ] >,
                              SPR #spr,
			      COMPS < [ LOCAL.CONT #arg1,
                                        NON-LOCAL #nls,
					OPT + ] . #vcomps > ] ] ],
   DTR.SYNSEM.LOCAL [ CAT [ HEAD verb & [ MOD #mod ],
		  	    VAL [ SUBJ < [ LOCAL.CONT #arg1,
                                           NON-LOCAL #nls ] >,
                                  SPR #spr,
				  COMPS < [ LOCAL.CONT #arg2,
                                            NON-LOCAL #nlo ] . 
                                 #vcomps > ] ] ] ].'''
    mylang.add(typedef)
    lrules.add('passive-lr := passive-lex-rule.')
    lr_n = 'passive-lex-rule'
    mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + form + ' ].')
    if marking == 'aux':
      mylang.add(lr_n + ' := \
           [ DTR.SYNSEM.LOCAL.CAT.HEAD.FORM ' + p.get('form') + ' ].')
    elif marking == 'morph':
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.FORM #form, \
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

###passing up features that need to be passed up
    vc = False
    if ch.get('has-aux') == 'yes':
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.AUX #aux, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.AUX #aux ].')
    if ch.get('q-inv'):
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.HEAD.INV #inv & -, \
                             DTR.SYNSEM.LOCAL.CAT.HEAD.INV #inv ].')
    if ch.get('verb-cluster') == 'yes' and ch.get('word-order') == 'v2':
      vc = True
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.VC #vc , \
                                 DTR.SYNSEM.LOCAL.CAT.VC #vc ].')
      mylang.add(lr_n + ' := [ SYNSEM.LOCAL.CAT.MC #mc, \
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
      
###If the subject is marked by an adposition after passivization,
###we need something like a 'case-marking-adposition', but without case
###fix: prep or postp also general form plus derived forms...
    dsubj_mark = p.get('dem-subj-mark')
    if 'adp' in dsubj_mark:    
      typedef = \
      '''passive-marking-adp-lex := basic-marking-only-adp-lex & \
                        raise-sem-lex-item.'''
      mylang.add(typedef)
      if vc:
        mylang.add('passive-marking-adp-lex := \
                          [ SYNSEM.LOCAL.CAT.VC na-or-- ].')
      if 'case' in dsubj_mark:
        m_parts = dsubj_mark.split('-')
        case = m_parts[1]
        mylang.add('passive-marking-adp-lex := \
                         [ ARG-ST < [ LOCAL.CAT.HEAD.CASE ' + case + ' ] > ].')
      sform = p.get('dsubj-form')
####make sure pform is introduced....
      mylang.add('passive-marking-adp-lex := \
                  [ SYNSEM.LOCAL.CAT.HEAD.FORM ' + sform + ' ].')
      mylang.add(sform + ' := pform.',section='features')
      lexicon.add(sform + '-passive := passive-marking-adp-lex & \
                    [ STEM < "' + sform + '" > ].') 
      

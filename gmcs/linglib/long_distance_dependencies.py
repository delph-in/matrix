########################################
#
# GERMANIC ONLY: Long distance dependency implementations
#


def customize_long_distance_deps(ch, mylang, rules, climb_files):
  climb_ldd = climb_files.get('ldd')
  if ch.get('vcomp-ldd') == 'yes':
    add_basic_ldd_phrases(ch, mylang, rules, climb_ldd)

def add_basic_ldd_phrases(ch, mylang, rules, climb_file):
  climb_file.set_section('mylang')
  basic_arg = \
   '''basic-extracted-arg-phrase :+ [ HEAD-DTR.SYNSEM [ NON-LOCAL.REL 0-dlist & [ LIST < > ] ] ].'''
  mylang.add(basic_arg, section='addenda')
  climb_file.add(basic_arg, comment='section=addenda')
  if not ch.get('v2-analysis') == 'filler-gap':
    mylang.add('basic-extracted-arg-phrase :+ [ HEAD-DTR.SYNSEM.MODIFIED notmod ].')
   
  comp_ex = \
  '''extracted-comp-phrase := basic-extracted-comp-phrase &
  [ SYNSEM.LOCAL.CAT [ VAL.COMPS < >,
                       HEAD verb,
                       VC na-or-- ],
    HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].'''

  mylang.add(comp_ex)
  climb_file.add(comp_ex)
  
  subj_ex = \
  '''extracted-subj-phrase := basic-extracted-subj-phrase &
  [ SYNSEM.LOCAL.CAT [ MC #mc,
                       HEAD verb ],
    HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].'''

  mylang.add(subj_ex)
  climb_file.add(subj_ex)

  extract_adj = \
  '''ger-extracted-adj-phrase := extracted-adj-phrase &
  [ HEAD-DTR.SYNSEM [ LOCAL.CAT [ HEAD verb &
                                       [ FORM finite ],
                                  VAL [ SUBJ < >,
                                        COMPS < >,
                                        SPR < > ] ],
                      NON-LOCAL.QUE #que ],
    SYNSEM.NON-LOCAL [ SLASH <! [ CAT [ HEAD +nrp &
                                               [ PRD - ],
                                          VAL [ SUBJ < >,
                                                COMPS < >,
                                                SPR < > ] ] ] !>,
                         QUE #que ] ] ].'''
  mylang.add(extract_adj)
  climb_file.add(extract_adj)

  if not ch.get('v2-analysis') == 'filler-gap':
    rules.add('extracted-comp := extracted-comp-phrase.')
    climb_file.add('extracted-comp := extracted-comp-phrase.',section='rules')
  rules.add('extracted-subj := extracted-subj-phrase.')
  rules.add('extracted-adj := ger-extracted-adj-phrase.')
  climb_file.add('extracted-subj := extracted-subj-phrase.',section='rules')
  climb_file.add('extracted-adj := ger-extracted-adj-phrase.',section='rules')

  add_analysis_specific_constraints(ch, mylang, rules, climb_file)

  filler_head_basic(ch, mylang, rules, climb_file)

  add_ldd_additional_constraints_all(ch, mylang, climb_file)

def filler_head_basic(ch, mylang, rules, climb_file):
  typedef = \
  '''general-filler-head-phrase := basic-head-filler-phrase & head-final &
  [ SYNSEM.LOCAL [ CAT [ MC +,
                         VFRONT #vf ],
                   CONT.HOOK.INDEX.SF prop-or-ques ],
    HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD verb &
                                     [ FORM finite,
                                       MOD < > ],
                                VFRONT #vf,
                                VAL [ SUBJ < >,
                                      COMPS < >,
                                      SPR < >,
                                      SPEC < > ] ],
    NON-HEAD-DTR.SYNSEM [ NON-LOCAL.REL 0-dlist & [ LIST < > ] ] ].'''
  mylang.add(typedef)
  climb_file.add(typedef)
  if ch.get('v2-analysis') == 'filler-gap':
    mylang.add('general-filler-head-phrase := \
                 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.INV -, \
                                               MC na ] ].')
    mylang.add('general-filler-head-phrase := \
                 [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                          COMPS < > ], \
                            NON-LOCAL.SLASH 0-dlist ] ].')
    climb_file.add('general-filler-head-phrase := \
                 [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.INV -, \
                                               MC na ] ].')
    climb_file.add('general-filler-head-phrase := \
                 [ SYNSEM [ LOCAL.CAT.VAL [ SUBJ < >, \
                                          COMPS < > ], \
                            NON-LOCAL.SLASH 0-dlist ] ].')
    if not ch.get('vcomp-ldd') == 'yes':
      mylang.add('general-filler-head-phrase := \
         [ SYNSEM.LOCAL.CAT.VFRONT - ].')
      climb_file.add('general-filler-head-phrase := \
         [ SYNSEM.LOCAL.CAT.VFRONT - ].')
    else:
      add_ldd_additional_fg(mylang, climb_file)
      if ch.get('clz-optionality'):
        add_ldd_informal_vcomp(ch, mylang, rules, climb_file) 
  else:
    mc_ldd_constraints(ch, mylang, climb_file)

  fg = \
  '''filler-head-phrase := general-filler-head-phrase &
     [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE 0-dlist ].'''
  mylang.add(fg)
  climb_file.add(fg)

  
  rules.add('filler-head := filler-head-phrase.')
  climb_file.add('filler-head := filler-head-phrase.',section='rules')

  if ch.get('wh-questions') == 'yes':
    wfg = \
  '''wh-filler-head-phrase := general-filler-head-phrase &
  [ SYNSEM.LOCAL.CONT.HOOK.INDEX.SF ques,
    NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE 1-dlist ].'''
    mylang.add(wfg)
    climb_file.add(wfg)
    rules.add('wh-filler-head := wh-filler-head-phrase.')
    climb_file.add('wh-filler-head := wh-filler-head-phrase.',section='rules')
  

def add_analysis_specific_constraints(ch, mylang, rules, climb_file):
  fg = ''
  ac = ''
  ar = ''
  if ch.get('v2-analysis') == 'filler-gap':
    fg = True
    mylang.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.INV -].')
    mylang.add('extracted-subj-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
                                   HEAD.INV - ] ].')
    climb_file.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.INV -].')
    climb_file.add('extracted-subj-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT [ MC na, \
                                   HEAD.INV - ] ].')
  elif ch.get('vc-analysis') == 'basic':
    ac = True
    mylang.add('extracted-subj-phrase := \
                [ SYNSEM.LOCAL.CAT [ EDGE -, \
                                     HEAD.INV + ] ].')
    mylang.add('extracted-comp-phrase := \
          [ HEAD-DTR.SYNSEM.LOCAL.CAT [ MC bool, \
                                        VC na-or-+, \
                                        VFRONT na-or-+ ] ].')
    climb_file.add('extracted-subj-phrase := \
                [ SYNSEM.LOCAL.CAT [ EDGE -, \
                                     HEAD.INV + ] ].')
    climb_file.add('extracted-comp-phrase := \
          [ HEAD-DTR.SYNSEM.LOCAL.CAT [ MC bool, \
                                        VC na-or-+, \
                                        VFRONT na-or-+ ] ].')
   # mylang.add('ger-extracted-adj-phrase := \
   #             [ SYNSEM.LOCAL.CAT.SECOND + ].')
  elif ch.get('vc-analysis') == 'aux-rule':
    ar = True
    mylang.add('basic-extracted-arg-phrase :+ \
              [ SYNSEM [ NON-LOCAL.QUE #que, \
                         LOCAL.CAT.POSTHEAD - ], \
                HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].')
    mylang.add('extracted-subj-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE +, \
       SYNSEM.LOCAL.CAT.EDGE - ].')
    mylang.add('extracted-comp-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.INV -, \
                                   MC na-or--, \
                                   VC + ] ].')
    mylang.add('ger-extracted-adj-phrase := \
             [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')

    climb_file.add('basic-extracted-arg-phrase :+ \
              [ SYNSEM [ NON-LOCAL.QUE #que, \
                         LOCAL.CAT.POSTHEAD - ], \
                HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].')
    climb_file.add('extracted-subj-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE +, \
       SYNSEM.LOCAL.CAT.EDGE - ].')
    climb_file.add('extracted-comp-phrase := \
     [ HEAD-DTR.SYNSEM.LOCAL.CAT [ HEAD.INV -, \
                                   MC na-or--, \
                                   VC + ] ].')
    climb_file.add('ger-extracted-adj-phrase := \
             [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD.INV - ].')
    add_ldd_additional_constraints_aux_r(mylang, climb_file)
  if fg or ac:
    scd_cp = '[ SYNSEM.LOCAL.CAT.SECOND #scd, \
                HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND #scd ].'
    mylang.add('extracted-comp-phrase := ' + scd_cp) 
    mylang.add('extracted-subj-phrase := ' + scd_cp) 
    mylang.add('ger-extracted-adj-phrase := ' + scd_cp)
    climb_file.add('extracted-comp-phrase := ' + scd_cp) 
    climb_file.add('extracted-subj-phrase := ' + scd_cp) 
    climb_file.add('ger-extracted-adj-phrase := ' + scd_cp)
  if ac or ar:
    mylang.add('extracted-subj-phrase := \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na-or-+ ].')
    mylang.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.EDGE - ].')
    mylang.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')
    mylang.add('extracted-subj-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].') 
    mylang.add('ger-extracted-adj-phrase := \
                [ SYNSEM.LOCAL.CAT.EDGE -, \
                  HEAD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist & [ LIST < > ] ].')
    climb_file.add('extracted-subj-phrase := \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC na-or-+ ].')
    climb_file.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.EDGE - ].')
    climb_file.add('extracted-comp-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].')
    climb_file.add('extracted-subj-phrase := \
                [ SYNSEM.LOCAL.CAT.HEAD.FORM finite ].') 
    climb_file.add('ger-extracted-adj-phrase := \
                [ SYNSEM.LOCAL.CAT.EDGE -, \
                  HEAD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist & [ LIST < > ] ].')
    add_ldd_additional_constraints_nonfg(ch, mylang, climb_file)
    if ch.get('clz-optionality'):
      add_ldd_informal_vcomp(ch, mylang, rules, climb_file)

def mc_ldd_constraints(ch, mylang, climb_file):
  mylang.add('general-filler-head-phrase := \
             [ SYNSEM.LOCAL.CAT [ HEAD.INV +, \
                                  MC + ], \
               HEAD-DTR.SYNSEM.LOCAL.CAT [ EDGE +, \
                                           POSTHEAD + ] ].')
  mylang.add('basic-head-filler-phrase :+ [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
  climb_file.add('general-filler-head-phrase := \
             [ SYNSEM.LOCAL.CAT [ HEAD.INV +, \
                                  MC + ], \
               HEAD-DTR.SYNSEM.LOCAL.CAT [ EDGE +, \
                                           POSTHEAD + ] ].')
  climb_file.add('basic-head-filler-phrase :+ [ SYNSEM.NON-LOCAL.SLASH 0-dlist ].')
  if ch.get('vc-analysis') == 'basic':
    mylang.add('general-filler-head-phrase := \
                 [ SYNSEM.LOCAL.CAT.SECOND -, \
                   HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
    climb_file.add('general-filler-head-phrase := \
                 [ SYNSEM.LOCAL.CAT.SECOND -, \
                   HEAD-DTR.SYNSEM.LOCAL.CAT.SECOND + ].')
  else:
    mylang.add('general-filler-head-phrase := \
               [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC + ].')
    climb_file.add('general-filler-head-phrase := \
               [ HEAD-DTR.SYNSEM.LOCAL.CAT.MC + ].')

def add_ldd_additional_constraints_all(ch, mylang, climb_file):
####needed for long distance dependencies
  if not ch.get('edge-related-res'):
    mylang.add('cat :+ [ EDGE luk ].', 'EDGE is used to prevent or assure specific sequences of phrases in the structure.',section='addenda')
    climb_file.add('cat :+ [ EDGE luk ].', 'EDGE is used to prevent or assure specific sequences of phrases in the structure.',section='addenda')
  mylang.add('head-initial-head-nexus := \
                [ SYNSEM.LOCAL.CAT.EDGE #edge, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
  climb_file.add('head-initial-head-nexus := \
                [ SYNSEM.LOCAL.CAT.EDGE #edge, \
                  NON-HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
  if ch.get('v2-analysis') == 'filler-gap':
    mylang.add('head-subj-phrase := \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE + ].')
    climb_file.add('head-subj-phrase := \
                [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE + ].')

def add_ldd_additional_constraints_nonfg(ch, mylang, climb_file):
  mylang.add('head-subj-phrase := \
              [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE - ].')
  climb_file.add('head-subj-phrase := \
              [ HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE - ].')

  if ch.get('q-inv'):
    mylang.add('int-cl := \
               [ SYNSEM.LOCAL.CAT [ POSTHEAD -, \
                                    EDGE #edge ], \
                 HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')
    climb_file.add('int-cl := \
               [ SYNSEM.LOCAL.CAT [ POSTHEAD -, \
                                    EDGE #edge ], \
                 HEAD-DTR.SYNSEM.LOCAL.CAT.EDGE #edge ].')


def add_ldd_additional_constraints_aux_r(mylang, climb_file):
  mylang.add('head-comp-phrase-2 := \
            [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  mylang.add('basic-aux-verb-rule := \
             [ SYNSEM.NON-LOCAL.QUE #que, \
               NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].')
  climb_file.add('head-comp-phrase-2 := \
            [ NON-HEAD-DTR.SYNSEM.NON-LOCAL.REL 0-dlist ].')
  climb_file.add('basic-aux-verb-rule := \
             [ SYNSEM.NON-LOCAL.QUE #que, \
               NON-HEAD-DTR.SYNSEM.NON-LOCAL.QUE #que ].')

def add_ldd_additional_fg(mylang, climb_file):
  mylang.add('extracted-comp-phrase := \
             [ SYNSEM.LOCAL.CAT.VC na-or-- ].')
  mylang.add('extracted-subj-phrase := \
             [ SYNSEM.LOCAL.CAT.MC na ].')
  mylang.add('head-comp-sub-phrase := \
             [ SYNSEM.NON-LOCAL.SLASH #slash, \
               NON-HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash ].')
  climb_file.add('extracted-comp-phrase := \
             [ SYNSEM.LOCAL.CAT.VC na-or-- ].')
  climb_file.add('extracted-subj-phrase := \
             [ SYNSEM.LOCAL.CAT.MC na ].')
  climb_file.add('head-comp-sub-phrase := \
             [ SYNSEM.NON-LOCAL.SLASH #slash, \
               NON-HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash ].')

def add_ldd_informal_vcomp(ch, mylang, rules, climb_file):
  typedef = '''create-ldd-informal-vcomp-phrase := basic-informal-vcomp &
  [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 1-dlist ] > ].'''
  mylang.add(typedef) 
  climb_file.add(typedef) 
  rules.add('create-ldd-informal-vcomp := create-ldd-informal-vcomp-phrase.')
  climb_file.add('create-ldd-informal-vcomp := create-ldd-informal-vcomp-phrase.', section='rules')
  if ch.get('v2-analysis') == 'filler-gap':
    mylang.add( 'create-ldd-informal-vcomp-phrase := basic-informal-vcomp & \
            [ ARGS < [ SYNSEM.LOCAL [ CAT [ HEAD.INV -, \
                                            MC na ], \
                                      CONT.HOOK #hook & [ INDEX.SF prop ] ] ] >, \
              SYNSEM.LOCAL.CAT.EDGE -, \
              C-CONT.HOOK #hook ].')
    climb_file.add( 'create-ldd-informal-vcomp-phrase := basic-informal-vcomp & \
            [ ARGS < [ SYNSEM.LOCAL [ CAT [ HEAD.INV -, \
                                            MC na ], \
                                      CONT.HOOK #hook & [ INDEX.SF prop ] ] ] >, \
              SYNSEM.LOCAL.CAT.EDGE -, \
              C-CONT.HOOK #hook ].')
  else:
    mylang.add('basic-informal-vcomp := \
       [ SYNSEM.LOCAL.CAT [ MC -, \
                            EDGE + ] ].')
    mylang.add('create-informal-vcomp-phrase := \
        [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 0-dlist ] > ].')
    climb_file.add('basic-informal-vcomp := \
       [ SYNSEM.LOCAL.CAT [ MC -, \
                            EDGE + ] ].')
    climb_file.add('create-informal-vcomp-phrase := \
        [ ARGS < [ SYNSEM.NON-LOCAL.SLASH 0-dlist ] > ].')
    tdef = \
    '''create-ldd-informal-vcomp-phrase := \
  [ ARGS < [ SYNSEM.LOCAL.CONT.HOOK [ INDEX [ INSTLOC #il,
						  SORT #sort,
						  E #tam ],
					  GTOP #gtop,
					  LTOP #ltop,
					  XARG #xarg ] ] >,
    C-CONT.HOOK [ INDEX [ INSTLOC #il,
			  SF prop,
			  SORT #sort,
			  E #tam ],
                  GTOP #gtop,
		  LTOP #ltop,
		  XARG #xarg ] ].'''
    mylang.add(tdef)
    climb_file.add(tdef)
    if ch.get('vc-analysis') == 'aux-rule':
      mylang.add('basic-informal-vcomp := \
                 [ ARGS < [ SYNSEM.LOCAL.CAT.MC na-or-+ ] > ].')
      mylang.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.EDGE - ].')
      climb_file.add('basic-informal-vcomp := \
                 [ ARGS < [ SYNSEM.LOCAL.CAT.MC na-or-+ ] > ].')
      climb_file.add('head-final-head-nexus := [ SYNSEM.LOCAL.CAT.EDGE - ].')
    else:
      mylang.add('basic-informal-vcomp := \
                 [ SYNSEM.LOCAL.CAT.SECOND +, \
                   ARGS < [ SYNSEM.LOCAL.CAT.MC + ] > ].')
      climb_file.add('basic-informal-vcomp := \
                 [ SYNSEM.LOCAL.CAT.SECOND +, \
                   ARGS < [ SYNSEM.LOCAL.CAT.MC + ] > ].')

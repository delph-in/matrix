#
# Germanic specific library for extraposed relative clauses
#


def create_extraposition(ch, mylang, rules, climb_files):
  climb_expos = climb_files.get('extrapos')
  climb_expos.set_section('mylang')
  introducing_anchor_and_related_phrases(mylang, climb_expos)
  add_anchor_related_suptypes_and_constraints(ch, mylang, climb_expos)

  rules.add('expose-anchor := expose-anchor-rule.')
  rules.add('extraposed-rel := head-mod-phrase-extraposed.')
  climb_expos.add('expose-anchor := expose-anchor-rule.',section='rules')
  climb_expos.add('extraposed-rel := head-mod-phrase-extraposed.',section='rules')

  if ch.get('v2-analysis') == 'filler-gap':
    fg_wo_slash_constraints(mylang, climb_expos)
  else:
    mc_wo_slash_constraints(ch, mylang, climb_expos)

  if ch.get('vc-analysis') == 'aux-rule':
    mylang.add('cat :+ [ VFRONT luk ].', section='addenda')
    climb_expos.add('cat :+ [ VFRONT luk ].', comment='section=addenda')

def introducing_anchor_and_related_phrases(mylang, climb_expos):

  mylang.add('anchor-min := avm.') 
  mylang.add('anchor := anchor-min & \
                       [ ANCHS diff-list, \
                         TO-BIND list ].')
  climb_expos.add('anchor-min := avm.') 
  climb_expos.add('anchor := anchor-min & \
                       [ ANCHS diff-list, \
                         TO-BIND list ].')
  mylang.add('local :+ [ ANCHOR anchor-min ].','ANCHOR collects possible referents in the sentence to which extraposed relative clauses can be bounds.',section='addenda')
  climb_expos.add('local :+ [ ANCHOR anchor-min ].','ANCHOR collects possible referents in the sentence to which extraposed relative clauses can be bounds.\n' + 'section=addenda')

  mylang.add('lex-rule :+ [ SYNSEM.LOCAL.ANCHOR #anchor, \
                            DTR.SYNSEM.LOCAL.ANCHOR #anchor ].',section='addenda')
  mylang.add('lex-item :+ [ SYNSEM.LOCAL.ANCHOR [ TO-BIND < >, \
				                  ANCHS <! !> ] ].',section='addenda') 
  climb_expos.add('lex-rule :+ [ SYNSEM.LOCAL.ANCHOR #anchor, \
                            DTR.SYNSEM.LOCAL.ANCHOR #anchor ].\n' + 'section=addenda')
  climb_expos.add('lex-item :+ [ SYNSEM.LOCAL.ANCHOR [ TO-BIND < >, \
				                  ANCHS <! !> ] ].\n' + 'section=addenda') 

  typedef_1 = '''share-anchor-unary-phrase := unary-phrase &
  [ SYNSEM.LOCAL.ANCHOR #anchor,
    ARGS < [ SYNSEM.LOCAL.ANCHOR #anchor ] > ].'''
  mylang.add(typedef_1)
  climb_expos.add(typedef_1)

  typedef_2 = '''collect-anchor-phrase := basic-binary-headed-phrase & 
                   [ HEAD-DTR.SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first,       
                                                            LAST #between],
						    TO-BIND #tb ],
                    NON-HEAD-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #between,
                                                             LAST #last],
                    SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first,
                                                  LAST #last],
					  TO-BIND #tb ] ].'''
  mylang.add(typedef_2)
  climb_expos.add(typedef_2)

 ####word-order Germanic specific, same as constraints on phrase

  ep_type = '''head-mod-phrase-extraposed := head-mod-phrase & head-initial &
  [ SYNSEM [ NON-LOCAL.REL 0-dlist,
	     LOCAL.ANCHOR [ ANCHS 0-dlist & [ LIST < > ],
			    TO-BIND < > ] ],
    HEAD-DTR.SYNSEM 
           [ LOCAL [ CAT [ HEAD +vp & [ MOD < > ],
                           VAL [ SUBJ < >,
                                 COMPS < >,
                                 SPR < >,
				 SPEC < > ],
                           POSTHEAD +,
                           MC + ],
		     ANCHOR [ TO-BIND < #index > ] ],
	     LIGHT #light,
             MODIFIED #modif ],
    NON-HEAD-DTR rel-phrase & [ SYNSEM.LOCAL.CAT 
                      [ HEAD verb & [ MOD < [ LOCAL local &
                                         [ CONT.HOOK.INDEX #index ],
					   LIGHT #light,
                                           MODIFIED #modif ] >,
				       FORM finite ],
                         MC -,
                         VAL [ COMPS < >,
                               SPR < >,
			       SUBJ < > ] ] ],
    C-CONT.RELS <! !> ] ].'''
  mylang.add(ep_type)
  climb_expos.add(ep_type)

  ea_type = '''expose-anchor-rule := head-only & head-valence-head-nexus &
 [ SYNSEM [ LOCAL [ CAT #cat &  [ VAL [ SUBJ < >,
                               COMPS < > ],
                         MC +,
                         VFRONT na-or--,
                         HEAD verb &
                              [ FORM finite ] ],
		  CONT #cont,
		  ANCHOR [ TO-BIND < #index >,
			   ANCHS [ LIST #rest,
				   LAST < > ] ] ],
	    NON-LOCAL.SLASH <! !> ],
   HEAD-DTR.SYNSEM.LOCAL [ CAT #cat,
			   CONT #cont,
			   ANCHOR [ ANCHS [ LIST < #index & [ ] . #rest >,
					    LAST < > ] ] ] ].'''

  mylang.add(ea_type)
  climb_expos.add(ea_type)

def add_anchor_related_suptypes_and_constraints(ch, mylang, climb_expos):

  mylang.add('head-spec-phrase := \
               [ SYNSEM.LOCAL [ ANCHOR.ANCHS [ LIST < #index . #first >, \
                                               LAST #last ], \
                                CONT.HOOK.INDEX #index ], \
                 HEAD-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #first, \
                                                      LAST #last ] ].')

  mylang.add('bare-np-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')

  climb_expos.add('head-spec-phrase := \
               [ SYNSEM.LOCAL [ ANCHOR.ANCHS [ LIST < #index . #first >, \
                                               LAST #last ], \
                                CONT.HOOK.INDEX #index ], \
                 HEAD-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #first, \
                                                      LAST #last ] ].')

  climb_expos.add('bare-np-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')

  if ch.get('n_spec_spr') == 'yes':
    mylang.add('bare-np-spec_incl-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')
    climb_expos.add('bare-np-spec_incl-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')
  if ch.get('wh-questions') == 'yes':
    mylang.add('wh-spec-head-phrase := \
                  [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
                                   CONT.HOOK.INDEX #index ] ].')
    climb_expos.add('wh-spec-head-phrase := \
                  [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
                                   CONT.HOOK.INDEX #index ] ].')
  if ch.get('rel-clause') == 'yes':
    mylang.add('basic-rel-phrase := \
                     [ SYNSEM.LOCAL.ANCHOR [ ANCHS <! !>, \
			                     TO-BIND < > ] ],') 
    climb_expos.add('basic-rel-phrase := \
                     [ SYNSEM.LOCAL.ANCHOR [ ANCHS <! !>, \
			                     TO-BIND < > ] ],')  
  if ch.get('clz-optionality'):
    mylang.add('basic-informal-vcomp := \
                  [ ARGS < [ SYNSEM.LOCAL.ANCHOR.TO-BIND < > ] > ].')
    climb_expos.add('basic-informal-vcomp := \
                  [ ARGS < [ SYNSEM.LOCAL.ANCHOR.TO-BIND < > ] > ].')

  if ch.get('subj-drop'):
    mylang.add('decl-head-opt-subj-phrase :+ [ SYNSEM.LOCAL.ANCHOR #anchor, \
                                    HEAD-DTR.SYNSEM.LOCAL.ANCHOR #anchor ].')

  mylang.add('compound-noun-phrase := collect-anchor-phrase.')
  mylang.add('int-cl := share-anchor-unary-phrase.')
  mylang.add('extracted-comp-phrase := share-anchor-unary-phrase.')
  mylang.add('extracted-subj-phrase := share-anchor-unary-phrase.')
  mylang.add('ger-extracted-adj-phrase := share-anchor-unary-phrase.')
  mylang.add('general-filler-head-phrase := collect-anchor-phrase.')

  climb_expos.add('compound-noun-phrase := collect-anchor-phrase.')
  climb_expos.add('int-cl := share-anchor-unary-phrase.')
  climb_expos.add('extracted-comp-phrase := share-anchor-unary-phrase.')
  climb_expos.add('extracted-subj-phrase := share-anchor-unary-phrase.')
  climb_expos.add('ger-extracted-adj-phrase := share-anchor-unary-phrase.')
  climb_expos.add('general-filler-head-phrase := collect-anchor-phrase.')

  mylang.set_section('addenda')
  mylang.add('basic-head-opt-comp-phrase :+ share-anchor-unary-phrase.')
  mylang.add('basic-head-comp-phrase :+ collect-anchor-phrase.')
  mylang.add('general-basic-head-subj-phrase :+ collect-anchor-phrase.')
  mylang.add('head-mod-phrase-simple :+ collect-anchor-phrase.')

  climb_expos.add('basic-head-opt-comp-phrase :+ share-anchor-unary-phrase.',comment='section=addenda')
  climb_expos.add('basic-head-comp-phrase :+ collect-anchor-phrase.',comment='section=addenda')
  climb_expos.add('general-basic-head-subj-phrase :+ collect-anchor-phrase.',comment='section=addenda')
  climb_expos.add('head-mod-phrase-simple :+ collect-anchor-phrase.',comment='section=addenda')
  coord_1 = ''' coord-phrase :+    
                 [ LCOORD-DTR.SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first,      
                                                            LAST #between],
						    TO-BIND #tb ],
                    RCOORD-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #between,
                                                           LAST #last],
                    SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first,
                                                  LAST #last],
					  TO-BIND #tb ] ].'''

  coord_2 = '''bottom-coord-phrase :+ 
                   [ CONJ-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #first,
                                                            LAST #between],
                    NONCONJ-DTR.SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #between,
                                                             LAST #last],
                                                      TO-BIND < > & #tb ],
                    SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first,
                                                  LAST #last],
					  TO-BIND #tb ] ].'''

  mylang.add(coord_1)
  mylang.add(coord_2)
  climb_expos.add(coord_1,comment='section=addenda')
  climb_expos.add(coord_2,comment='section=addenda')


def fg_wo_slash_constraints(mylang, climb_expos):
  mylang.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH 0-dlist, \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash, \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH 0-dlist, \
                                       LOCAL.CAT.HEAD.MOD \
                                < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')
  climb_expos.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH 0-dlist, \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash, \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH 0-dlist, \
                                       LOCAL.CAT.HEAD.MOD \
                                < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')


def mc_wo_slash_constraints(ch, mylang, climb_expos):
  mylang.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH [ LIST #first, \
				          LAST #last ], \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash & [ LIST #middle, \
                                                            LAST #last ], \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH [ LIST #first, \
                                                         LAST #middle ], \
                                       LOCAL.CAT.HEAD.MOD \
                                 < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')
  climb_expos.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH [ LIST #first, \
				          LAST #last ], \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash & [ LIST #middle, \
                                                            LAST #last ], \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH [ LIST #first, \
                                                         LAST #middle ], \
                                       LOCAL.CAT.HEAD.MOD \
                                 < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')
  if ch.get('vc-analysis') == 'aux-rule' and ch.get('split-cluster') == 'yes':
    auxrule_insertion_additions(mylang, climb_expos)

def auxrule_insertion_additions(mylang, climb_expos):
  mylang.add('noncomp-aux-2nd-phrase := collect-anchor-phrase.')
  
  mylang.add('special-insert-aux-phrase := \
  [ HEAD-DTR.SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first, \
                                           LAST #between ], \
                                   TO-BIND #tb ], \
    INSERT-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #between, \
                                           LAST #last ], \
    SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first, \
                                  LAST #last ], \
                          TO-BIND #tb ] ].')

  climb_expos.add('noncomp-aux-2nd-phrase := collect-anchor-phrase.')
  
  climb_expos.add('special-insert-aux-phrase := \
  [ HEAD-DTR.SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first, \
                                           LAST #between ], \
                                   TO-BIND #tb ], \
    INSERT-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #between, \
                                           LAST #last ], \
    SYNSEM.LOCAL.ANCHOR [ ANCHS [ LIST #first, \
                                  LAST #last ], \
                          TO-BIND #tb ] ].')

#
# Germanic specific library for extraposed relative clauses
#


def create_extraposition(ch, mylang, rules):
  introducing_anchor_and_related_phrases(mylang)
  add_anchor_related_suptypes_and_constraints(ch, mylang)

  rules.add('expose-anchor := expose-anchor-rule.')
  rules.add('extraposed-rel := head-mod-phrase-extraposed.')

  if ch.get('v2-analysis') == 'filler-gap':
    fg_wo_slash_constraints(mylang)
  else:
    mc_wo_slash_constraints(ch, mylang)

def introducing_anchor_and_related_phrases(mylang):

  mylang.add('anchor-min := avm.') 
  mylang.add('anchor := anchor-min & \
                       [ ANCHS diff-list, \
                         TO-BIND list ].')
  mylang.add('local :+ [ ANCHOR anchor-min ].','ANCHOR collects possible referents in the sentence to which extraposed relative clauses can be bounds.',section='addenda')

  mylang.add('lex-rule :+ [ SYNSEM.LOCAL.ANCHOR #anchor, \
                            DTR.SYNSEM.LOCAL.ANCHOR #anchor ].',section='addenda')
  mylang.add('lex-item :+ [ SYNSEM.LOCAL.ANCHOR [ TO-BIND < >, \
				                  ANCHS <! !> ] ].',section='addenda') 

  typedef_1 = '''share-anchor-unary-phrase := unary-phrase &
  [ SYNSEM.LOCAL.ANCHOR #anchor,
    ARGS < [ SYNSEM.LOCAL.ANCHOR #anchor ] > ].'''
  mylang.add(typedef_1)

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

def add_anchor_related_suptypes_and_constraints(ch, mylang):

  mylang.add('head-spec-phrase := \
               [ SYNSEM.LOCAL [ ANCHOR.ANCHS [ LIST < #index . #first >, \
                                               LAST #last ], \
                                CONT.HOOK.INDEX #index ], \
                 HEAD-DTR.SYNSEM.LOCAL.ANCHOR.ANCHS [ LIST #first, \
                                                      LAST #last ] ].')

  mylang.add('bare-np-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')

  if ch.get('n_spec_spr') == 'yes':
    mylang.add('bare-np-spec_incl-phrase := \
      [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
 		       CONT.HOOK.INDEX #index ] ].')
  mylang.add('wh-spec-head-phrase := \
                  [ SYNSEM.LOCAL [ ANCHOR.ANCHS <! #index !>, \
                                   CONT.HOOK.INDEX #index ] ].')
  mylang.add('basic-rel-phrase := \
                     [ SYNSEM.LOCAL.ANCHOR [ ANCHS <! !>, \
			                     TO-BIND < > ] ],')  

  mylang.add('basic-informal-vcomp := \
                  [ ARGS < [ SYNSEM.LOCAL.ANCHOR.TO-BIND < > ] > ].')

  mylang.add('compound-noun-phrase := collect-anchor-phrase.')
  mylang.add('int-cl := share-anchor-unary-phrase.')
  mylang.add('extracted-comp-phrase := share-anchor-unary-phrase.')
  mylang.add('extracted-subj-phrase := share-anchor-unary-phrase.')
  mylang.add('ger-extracted-adj-phrase := share-anchor-unary-phrase.')
  mylang.add('general-filler-head-phrase := collect-anchor-phrase.')

  mylang.set_section('addenda')
  mylang.add('basic-head-opt-comp-phrase :+ share-anchor-unary-phrase.')
  mylang.add('basic-head-comp-phrase :+ collect-anchor-phrase.')
  mylang.add('general-basic-head-subj-phrase :+ collect-anchor-phrase.')
  mylang.add('head-mod-phrase-simple :+ collect-anchor-phrase.')
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


def fg_wo_slash_constraints(mylang):
  mylang.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH 0-dlist, \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash, \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH 0-dlist, \
                                       LOCAL.CAT.HEAD.MOD \
                                < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')


def mc_wo_slash_constraints(ch, mylang):
  mylang.add('head-mod-phrase-extraposed := \
               [ SYNSEM.NON-LOCAL.SLASH [ LIST #first, \
				          LAST #last ], \
                 HEAD-DTR.SYNSEM.NON-LOCAL.SLASH #slash & [ LIST #middle, \
                                                            LAST #last ], \
                 NON-HEAD-DTR.SYNSEM [ NON-LOCAL.SLASH [ LIST #first, \
                                                         LAST #middle ], \
                                       LOCAL.CAT.HEAD.MOD \
                                 < [ NON-LOCAL.SLASH #slash & 0-dlist ] > ] ].')
  if ch.get('vc-analysis') == 'aux-rule' and ch.get('split-cluster') == 'yes':
    auxrule_insertion_additions(mylang)

def auxrule_insertion_additions(mylang):
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

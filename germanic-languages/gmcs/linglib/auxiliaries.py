from gmcs.utils import TDLencode

from gmcs.linglib import case
from gmcs.linglib import features



def set_supertypename(auxcomp):
  if auxcomp == 's':
    supertypename = 's-comp-aux'
  elif auxcomp == 'vp':
    supertypename = 'subj-raise-aux'
  else:
    supertypename = 'arg-comp-aux'
  return supertypename


#class Auxiliary:


#  def __init__(self, aux, ch, mylang, hierarchies):
#    auxcomp = ch.get('aux-comp')
#    set_supertypename(auxcomp)
#    self.arg_val_type = define_arg_str_and_valency(aux, auxcomp, ch, mylang)
#    multiaux = ch.get('multiple-aux')
#    self.auxiliary_type = create_semantics(aux, auxcomp, multiaux, mylang)
#    self.user_aux_type = customize_users_auxtype(aux, ch, mylang, hierarchies) 

      

def define_arg_str_and_valency(aux, auxcomp, ch, mylang):
  
  if ch.get('verb-cluster') == 'yes':
    supertypename = set_germanic_supertypename(ch)
  else:
    supertypename = set_supertypename(auxcomp)
  basic_typedef = supertypename + ' := aux-lex & \
                [ SYNSEM.LOCAL.CAT.VAL [ SPR < >, \
                                         SPEC < > ] ].'
  mylang.add(basic_typedef)
 
  if ch.get('verb-cluster') == 'yes':
    comp_spec_typedef = define_germanic_arg_str_and_valency(ch, supertypename)
  elif auxcomp == 's':
    comp_spec_typedef = supertypename + ' := basic-one-arg & \
                            [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                                     COMPS < #comps > ], \
                              ARG-ST < #comps & \
                                         [ LOCAL.CAT [ VAL [ SUBJ < >, \
                                                             COMPS < >, \
                                                             SPR < >, \
                                                             SPEC < > ], \
                                                       HEAD verb ]] > ].'
    if ch.get('multiple-aux') == 'no':
      auxrest_type = supertypename + ' := \
                         [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].'
      mylang.add(auxrest_type)

###VP and V-compl have more in common

  else:
    comp_spec_typedef = supertypename + ' := \
                             [ SYNSEM.LOCAL [ CAT.VAL.SUBJ < #subj >, \
                                              CONT.HOOK.XARG #xarg ], \
                               ARG-ST < #subj & \
                                        [ LOCAL [ CAT.VAL [ SUBJ < >, \
                                                            SPR < >, \
                                                            SPEC < >, \
                                                            COMPS < > ], \
                                                  CONT.HOOK.INDEX #xarg ] ], \
                                        [ ] > ].'
    if ch.get('multiple-aux') == 'no':
      auxrest_type = supertypename + ' := \
                         [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].'
      mylang.add(auxrest_type)

    if auxcomp == 'vp':
      comp_spec_typedef_2 = supertypename + ' := \
                                          trans-first-arg-raising-lex-item  & \
                   [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps >, \
                     ARG-ST < [ ], \
                              #comps & \
                              [ LOCAL.CAT [ VAL [ SUBJ < [ ] >, \
                                                  COMPS < >, \
                                                  SPR < >, \
                                                  SPEC < > ], \
                                            HEAD verb ]] > ].'
    elif auxcomp == 'v':
      
      comment = \
          '; Somewhat surprisingly, this inherits from basic-two-arg, so\n' + \
          '; that the non-local features are amalgamated from subj, the\n' + \
          '; lexical verb complement, but not the other complements, if any.'
      mylang.add_literal(comment)
      comp_spec_typedef_2 = supertypename + ' := basic-two-arg & \
             [ SYNSEM.LOCAL.CAT.VAL.COMPS < #comps . #vcomps >, \
               ARG-ST < [ ], \
                      #comps & \
                      [ LIGHT +, \
                        LOCAL [ CAT [ VAL [ SUBJ < [ ] >, \
                                            COMPS #vcomps ], \
                                      HEAD verb ], \
                                CONT.HOOK.XARG #xarg ]] > ].'
    mylang.add(comp_spec_typedef_2)
    add_subj_tdl(aux, auxcomp, ch, mylang)

  mylang.add(comp_spec_typedef)    
  
def add_subj_tdl(aux, auxcomp, ch, mylang):
  """
  A function to add subject related tdl to type definition if the complement
  is either a V or a VP.
  Called by def_arg_str_and_valency().
  typename = supertype variable in def_arg_str_and_valency()
  type = current definition of supertype including arg-str()
  """
  typename = set_supertypename(auxcomp)
  subj = aux.get('subj','')
  subjc = aux.get('subj_case','') #TODO: verify _-delimited key
  cases = case.case_names(ch)
  subjcase = case.canon_to_abbr(subjc, cases)
  
  if subj == 'adp':
    subjtype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD adp ].'
  else:
    subjtype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD noun ].'
    if subj == 'np-comp-case':
      scasetype = typename + ' := [ ARG-ST < [ LOCAL.CAT.HEAD.CASE #case  ], \
          [ LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD.CASE #case ] > ] > ].'
      mylang.add(scasetype)
    elif subj == 'np-aux-case':
      scasetype = typename + ' := [ ARG-ST.FIRST.LOCAL.CAT.HEAD.CASE ' + subjcase + ' ].'
      mylang.add(scasetype)

  mylang.add(subjtype)
    

def create_semantics(sem, aux, auxcomp, mylang, ch, hierarchies):

  if ch.get('verb-cluster') == 'yes':
    supertypename = set_germanic_supertypename(ch)
  else:
    supertypename = set_supertypename(auxcomp)

  if sem == 'add-pred':
    auxtypename = supertypename + '-with-pred'
    basic_typedef = auxtypename + ' := ' + supertypename + '.' 
    if auxcomp == 'vp':
      typedef = auxtypename + ' := norm-sem-lex-item & \
                                        trans-first-arg-raising-lex-item-1 .'
    else:
      if auxcomp == 'v' and not ch.get('vc-analysis') == 'aux-rule':
        arg_def = 'ARG-ST < [ ], [ LOCAL.CONT.HOOK.LTOP #larg ] >'
      else:
        arg_def =  'ARG-ST < [ LOCAL.CONT.HOOK.LTOP #larg ] >'

      comment = \
        '; Not inheriting from basic-verb-lex, so need to put in\n' + \
        '; event-relation by hand here.'
      mylang.add_literal(comment)

      typedef = auxtypename +  ' := hcons-lex-item & \
               [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq & \
                                                [ HARG #harg, \
                                                  LARG #larg ] !> ], \
                          LKEYS.KEYREL event-relation & \
                                       [ ARG1 #harg ]],' + arg_def + ' ].'
      
    mylang.add(basic_typedef)
    mylang.add(typedef)
  else:   
    auxtypename = supertypename + '-no-pred'
    typedef = auxtypename + ' := ' + supertypename + ' & raise-sem-lex-item.'

    if auxcomp == 'v':
          
      comment = \
        '; Note that raise-sem-lex-item assumes the first complement is\n' + \
        '; where the HOOK comes from.  It\'s not clear to me how you\'d\n' + \
        '; tell that you had an argument composition auxiliary if it\n' + \
        '; wasn\'t appearing adjacent to the verb.'
      mylang.add_literal(comment)
#### Restriction to stop semantically empty auxiliaries from spinning
#### Check multiple aux: no need to add constraint when already present on 
#### supertype


    if not ch.get('multiple-aux') == 'no':   
      comment = \
        '; To keep the semantically empty ones from spinning on\n' + \
        '; generation, require complement to be [AUX -].  The\n' + \
        '; FORM feature might be enough in the starter grammars,\n' + \
        '; but I don\'t want to rely on this.  Then again, [ AUX - ]\n' + \
        '; might not be true.'
      mylang.add_literal(comment)
      if auxcomp == 's' or ch.get('vc-analysis') == 'aux-rule':
        arg_str = '< [ LOCAL.CAT.HEAD.AUX - ] >'
      else:
        arg_str = '< [ ], [ LOCAL.CAT.HEAD.AUX - ] >'
         

      auxres_type = auxtypename + ' := [ ARG-ST ' + arg_str + ' ].'   
      mylang.add(auxres_type)

    mylang.add(typedef)

  customize_users_auxtype(auxtypename, aux, ch, mylang, hierarchies)


def customize_users_auxtype(auxtypename, aux, ch, mylang, hierarchies):
  """
  A utility that declares the userstype as subtype of supertype and
  calls the functions that specify feature values on the type.
  Called by customize_auxiliaries.
  userstype = userstypename in customize_auxiliaries
  supertype = userstypename in customize_auxiliaries
  """
  auxcomp = ch.get('aux-comp')
  userstypename = get_users_type_name(aux)
  features.customize_feature_values(mylang, ch, hierarchies, aux, userstypename, 'aux')
  features.customize_feature_values(mylang, ch, hierarchies, aux, userstypename, 'auxcomplement')
  mylang.add(userstypename + ':= ' + auxtypename + '.')

def get_users_type_name(aux):
  name = aux.get('name', '')
  userstypename = name + '-aux-lex'
  return userstypename

def add_auxiliaries_to_lexicon(userstypename, sem, aux, lexicon):
  for stem in aux.get('stem',[]):
    orth = stem.get('orth')
    typedef = TDLencode(orth) + ' := ' + userstypename + ' & \
                       [ STEM < "' + orth + '" > ].'
    lexicon.add(typedef)

    if sem == 'add-pred':
      pred = stem.get('pred')
      typedef = TDLencode(orth) + \
                    ' := [ SYNSEM.LKEYS.KEYREL.PRED "' + pred + '" ].'
      lexicon.add(typedef, merge=True)


def customize_auxiliaries(mylang, ch, lexicon, hierarchies):

  lexicon.add_literal(';;; Auxiliaries')
  for aux in ch.get('aux',[]):
    auxcomp = ch.get('aux-comp')
    userstypename = get_users_type_name(aux)
    sem = aux.get('sem', '')

    define_arg_str_and_valency(aux, auxcomp, ch, mylang)
    create_semantics(sem, aux, auxcomp, mylang, ch, hierarchies)
    add_auxiliaries_to_lexicon(userstypename, sem, aux, lexicon)






#################################################
#
# Germanic specific rules
# 

def set_germanic_supertypename(ch):
  if ch.get('vc-analysis') == 'aux-rule':
    supertypename = 'one-comp-aux'
  else:
    supertypename = 'arg-comp-aux'

  return supertypename

def define_germanic_arg_str_and_valency(ch, supertypename):
  if ch.get('vc-analysis') == 'basic':
    typedef = basic_analysis_germanic_aux_arg_str(supertypename)
  else:
    typedef = aux_plus_verb_germanic_aux_arg_str(supertypename)

  return typedef

def basic_analysis_germanic_aux_arg_str(supertypename):

  typedef = supertypename + ' := aux-lex & basic-two-arg & \
    [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ #subj, \
                               COMPS < #comps . #vcomps >, \
                               SPR < >, \
                               SPEC < > ], \
                     CONT.HOOK.XARG #xarg ], \
      ARG-ST < [ ], \
                   #comps & \
                          [ LOCAL [ CAT [ VAL [ SUBJ #subj, \
                                                COMPS #vcomps ], \
                                          HEAD verb ], \
                                    CONT.HOOK.XARG #xarg ]] > ].'
  return typedef

def aux_plus_verb_germanic_aux_arg_str(supertypename):

  typedef = supertypename + ' := aux-lex & basic-one-arg & \
                 [ SYNSEM.LOCAL.CAT.VAL [ SUBJ < >, \
                                          COMPS < #comp >, \
                                          SPR < >, \
                                          SPEC < > ], \
                   ARG-ST < #comp & [ LOCAL.CAT.HEAD verb ] > ].'
  return typedef

from gmcs.utils import TDLencode
#from gmcs.linglib.morphotactics import all_position_classes

from string import join
from functools import partial


### RULE NAME GENERATORS ###

def added_arg_non_local_lex_rule_name(added_arg, total_args):
    return 'added-arg{}of{}-non-local-lex-rule'.format(added_arg, total_args)

def added_arg_applicative_lex_rule_name(added_arg, total_args):
    return 'added-arg{}of{}-applicative-lex-rule'.format(added_arg, total_args)

def added_arg_head_lex_rule_name(added_arg, head_constraint):
    if head_constraint:
        hc = head_constraint.lower()
        if hc == 'np' or hc == 'noun':
            head = 'np'
        elif hc == 'pp' or hc == 'adp':
            head = 'pp'
    return 'added-arg{}-{}-head-lex-rule'.format(added_arg, head)


# These are the shorthand names used to generate rules and rule names
rulenamefns = { 'subj-rem-op': (lambda: 'subj-rem-op-lex-rule'),
                'obj-rem-op': (lambda: 'obj-rem-op-lex-rule'),
                'added-arg-non-local': added_arg_non_local_lex_rule_name,
                'added-arg-applicative': added_arg_applicative_lex_rule_name,
                'added-arg-head-type': added_arg_head_lex_rule_name,
                'basic-applicative': (lambda: 'basic-applicative-lex-rule') }

# A little  bit of machinery to make it easy to find the names of generated rules
# (Gets called from morphotactics.py)
def lexrule_name(rule_type,*args):
    fn = rulenamefns.get(rule_type)
    if fn:
        return fn(*args)


############ RULE BUILDERS ################

def subj_rem_op_lex_rule():
    return  lexrule_name('subj-rem-op') + ''' := subj-change-only-lex-rule &
  [ SYNSEM.LOCAL.CAT.VAL.SUBJ < >,
    DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < unexpressed > ].'''

def obj_rem_op_lex_rule():
    return lexrule_name('obj-rem-op') + ''' := comps-change-only-lex-rule &
  [ SYNSEM.LOCAL.CAT.VAL.COMPS #comps,
    DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < unexpressed . #comps > ].'''


## These rules implement non-local diff list stitching for argument-adding
## lexical rules.
## NOTE: This might require revisiting if we ever have multiple non-local
## values (b/c order might matter); kicking that can down the road.

NEW_NON_LOCAL_FRAGMENT = '''[ NON-LOCAL [ SLASH [ LIST #smiddle,
	                       		          LAST #slast ],
			                  REL [ LIST #rmiddle,
			                        LAST #rlast ],
			                  QUE [ LIST #qmiddle,
				                LAST #qlast ] ] ] '''

def added_arg_non_local_lex_rule(added_arg, total_args):
    if added_arg > total_args or total_args == 1 or total_args > 3:
        raise Exception('Bad argument count ({} of {})'.format(added_arg, total_args))

    rulename = 'added-arg{}of{}-non-local-lex-rule'.format(added_arg, total_args)
    arglist = []
    for i in range(1,total_args+1):
        if i == added_arg:
            arglist.append(NEW_NON_LOCAL_FRAGMENT)
        else: 
            arglist.append('[ ]')
    arg_st = ',\n             '.join(arglist)
    rulevars = { 'rulename': rulename, 'arg-st': arg_st }
    rule = '''{rulename} := lex-rule &
  [ SYNSEM.NON-LOCAL [ SLASH [ LIST #sfirst,
			       LAST #slast ],
		       REL [ LIST #rfirst,
			     LAST #rlast ],
		       QUE [ LIST #qfirst,
			     LAST #qlast ] ],
    ARG-ST < {arg-st} >,
             
    DTR.SYNSEM.NON-LOCAL [ SLASH [ LIST #sfirst,
                                   LAST #smiddle ],
			   REL [ LIST #rfirst,
				 LAST #rmiddle ],
			   QUE [ LIST #qfirst,
				 LAST #qmiddle ] ] ].'''.format(**rulevars)
    return rule


## General applicative lexrules

# non-scopal
## NOTE: This expects to have val-change-ccont-lex-rule in its ancestry 
## somewhere. Normally that should be handled by the morphotactics library, 
## which uses val-change-ccont-lex-rule as the base supertype
## for any position class with valence-changing lexical rules.

# Generates the generic applicative rule
def basic_applicative_lex_rule():
    return  '''{rulename} := comps-change-only-lex-rule &
  [ C-CONT [ RELS <! event-relation &
                   [ ARG1 #evt ] !>,
             HCONS <! !> ],
    DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX #evt ].'''.format(rulename=lexrule_name('basic-applicative'))

CAUSATIVE_LEX_RULE = '''causative-lex-rule := val-change-with-ccont-lex-rule &
    same-spr-lex-rule &
    same-spec-lex-rule &
  [ C-CONT [ RELS <! event-relation &
                   [ LBL #ltop,
                     ARG0 #hidx,
                     ARG1 #causer,
                     ARG2 #causee,
                     ARG3 #harg ] !>,
             HCONS <! qeq & [ HARG #harg,
                              LARG #larg ] !>,
             HOOK [ LTOP #ltop,
                    INDEX #hidx,
                    XARG #causer ] ],
    SYNSEM.LOCAL.CAT.VAL [ SUBJ < #nsubj >,
                           COMPS < #osubj, #comp > ],
    ARG-ST < #nsubj & [ LOCAL.CONT.HOOK.INDEX #causer ],
             #osubj & [ LOCAL.CONT.HOOK.INDEX #causee ],
             #comp >,
    DTR [ ARG-ST < #osubj, 
                   #comp >,
          SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #osubj >,
                                   COMPS < #comp > ],
                         CONT.HOOK.LTOP #larg ] ] ].'''


# Generates the valence-specific constraint on the type of an added argument.
def added_arg_head_lex_rule(arg,head):
    rulename = lexrule_name('added-arg-head-type', arg, head)
    hc = head.lower()
    if hc == 'np':
        head_type = 'noun'
    elif hc == 'pp':
        head_type = 'adp'
    if arg == 1:
        constraint = '  [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ LOCAL.CAT.HEAD {} ] > ].'.format(head_type)
    elif arg == 2:
        constraint = '  [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ LOCAL.CAT.HEAD {} ], [ ] > ].'.format(head_type)
    elif arg == 3:
        constraint = '  [ SYNSEM.LOCAL.CAT.VAL.COMPS < [ ], [ LOCAL.CAT.HEAD {} ] > ].'.format(head_type)
    
    return rulename + ' := lex-rule &\n' + constraint
    



ADDED_ARG_APPLICATIVE_FRAGMENT = ''' #ncomp & [ LOCAL [ CAT [ VAL [ SPR < >,
                                                                    COMPS < > ] ],
                                                        CONT.HOOK.INDEX #nind ] ]'''
                            
# Generates the valence-specific applicative LR supertype.     
# Inherits from the valence-specific non-local rule and the generic applicative rule.   
def added_arg_applicative_lex_rule(added_arg, total_args):
    rulevars = {}
    arglist = ['#arg1']
    for i in range(1,total_args+1):
        if i == 1:
            continue
        elif i == added_arg:
            arglist.append(ADDED_ARG_APPLICATIVE_FRAGMENT)
        else: 
            arglist.append('#ocomp')
    rulevars['arg-st'] = ', '.join(arglist)
    rulevars['comps'] = '#ncomp, #ocomp' if added_arg == 2 else '#ocomp, #ncomp'
    rulevars['rulename'] = lexrule_name('added-arg-applicative', added_arg, total_args)
    rulevars['basic-applicative-rule'] = lexrule_name('basic-applicative')
    rulevars['added-arg-non-local-rule'] = lexrule_name('added-arg-non-local', added_arg, total_args)
                 
    rule = '''{rulename} := {basic-applicative-rule} & {added-arg-non-local-rule} &
  [ SYNSEM.LOCAL.CAT.VAL.COMPS < {comps} >,
    C-CONT [ RELS <! [ ARG2 #nind ] !> ],
    DTR [ SYNSEM.LOCAL [ CAT.VAL.COMPS < #ocomp > ],
	  ARG-ST < #arg1, #ocomp > ],
    ARG-ST < {arg-st} > ].'''.format(**rulevars)
    return rule

# Small helper to get proper set semantics
class FnWrapper:
    def __init__(self,name,fn,*args):
        self.name = name
        self.fn = fn if len(args) == 0 else partial(fn,*args)
    def __hash__(self):
        return hash(self.name)
    def __call__(self):
        return self.fn()


class LexRuleBuilder:
    def __init__(self):
        self.rules = set()
    def add(self,rule_name,rulegen,*rulegen_args):
        self.rules.add(FnWrapper(rule_name,rulegen,*rulegen_args))
    def generate_tdl(self,mylang):
        prev_section = mylang.section
        mylang.set_section('lexrules')
        for rule in self.rules:
            mylang.add(rule())
        mylang.set_section(prev_section)
            
####### MAIN INTERFACE ##########

def customize_valence_change(mylang, ch, lexicon, rules, irules, lrules):
    from gmcs.linglib.morphotactics import all_position_classes
    rules = LexRuleBuilder()
    for pc in all_position_classes(ch):
        pc_key = pc.full_key
        pc_inputs = pc.get('inputs',[])
        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
        for lrt in pc.get('lrt',[]):
            for vchop in lrt.get('valchg',[]):
                opname = vchop['operation'].lower()
                if opname == 'subj-rem':
                    rules.add('subj-rem-op', subj_rem_op_lex_rule)
                if opname == 'obj-rem':
                    rules.add('obj-rem-op', obj_rem_op_lex_rule)
                if opname == 'obj-add':
                    rules.add('basic-applicative', basic_applicative_lex_rule)
                    position = vchop.get('argpos','').lower()
                    argtype = vchop.get('argtype','').lower()
                    if position == 'pre':
                        rules.add('added-arg-head-type', added_arg_head_lex_rule, 2, argtype)
                        rules.add('added-arg-applicative', added_arg_applicative_lex_rule, 2, 3)
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 2, 3)
                    elif position == 'post':
                        rules.add('added-arg-head-type', added_arg_head_lex_rule, 3, argtype)
                        rules.add('added-arg-applicative', added_arg_applicative_lex_rule, 3, 3)
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 3, 3)
                if opname == 'subj-add':
                    if 'verb' in pc_inputs or 'iverb' in pc_inputs:
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 1, 2)
                    if 'verb' in pc_inputs or 'tverb' in pc_inputs:
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 1, 3)

    rules.generate_tdl(mylang)

# don't need to do anything to LRTs in choices
def add_lexrules(choices):
    pass


                

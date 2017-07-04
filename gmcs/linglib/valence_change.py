from gmcs.utils import TDLencode
#from gmcs.linglib.morphotactics import all_position_classes

from string import join
from functools import partial


### RULE NAME GENERATORS ###
# These functions return the name the rule generator will use
def subj_rem_op_lex_rule_name(transitive):
    tr_itr = 'tr' if transitive else 'itr'
    return 'subj-rem-{}-op-lex-rule'.format(tr_itr)

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

def causative_lex_rule_name(transitive):
    if transitive:
        return 'causative-transitive-lex-rule'
    else:
        return 'causative-intransitive-lex-rule'

# These are the shorthand names used to generate rules and rule names
# NB: the values here are *functions* that return the rule name.
rulenamefns = {'subj-rem-op': subj_rem_op_lex_rule_name,
               'obj-rem-op': (lambda: 'obj-rem-op-lex-rule'),
               'added-arg-non-local': added_arg_non_local_lex_rule_name,
               'added-arg-applicative': added_arg_applicative_lex_rule_name,
               'added-arg-head-type': added_arg_head_lex_rule_name,
               'basic-applicative': (lambda: 'basic-applicative-lex-rule'),
               'causative': causative_lex_rule_name}

# A little  bit of machinery to make it easy to find the names of generated rules
# (Gets called from morphotactics.py)
def lexrule_name(rule_type, *args):
    fn = rulenamefns.get(rule_type)
    if fn:
        return fn(*args)



############ RULE BUILDERS ################

SUBJ_REM_OP_ITR_RULE_TEMPLATE = '''{} := subj-change-only-lex-rule &
  [ SYNSEM.LOCAL.CAT.VAL.SUBJ < >,
    DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < unexpressed > ].'''

SUBJ_REM_OP_TR_RULE_TEMPLATE = '''{} := subj-and-comps-change-only-lex-rule &
  [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #comp & [ LOCAL.CONT.HOOK.INDEX #sind ] >,
                             COMPS #rest ] ],
    C-CONT.HOOK.XARG #sind,
    DTR.SYNSEM.LOCAL [ CAT.VAL [ SUBJ < unexpressed >,
                                 COMPS [ FIRST #comp,
                                         REST #rest ] ] ] ].'''

# no-ccont-lex-rule is in local-change-only
def subj_rem_op_lex_rule(transitive=True):
    if transitive:
        return SUBJ_REM_OP_TR_RULE_TEMPLATE.format(lexrule_name('subj-rem-op', True))
    else:
        return SUBJ_REM_OP_ITR_RULE_TEMPLATE.format(lexrule_name('subj-rem-op', False))
# rulevars = {}
 # if transitive:
 #     rulevars['subj'] = '[ LOCAL.CONT.HOOK #oind ]'
 #     rulevars['dtr-val'] = '''[ SUBJ < unexpressed >,
 #       COMPS.FIRST.LOCAL.CONT.HOOK #oind ]'''
 # else:
 #     rulevars['subj'] = ''
 #     rulevars['dtr-val'] = '[ SUBJ < unexpressed > ]'
#
#  return  lexrule_name('subj-rem-op', transitive) + ''' := subj-change-only-lex-rule &
#  [ SYNSEM.LOCAL.CAT.VAL.SUBJ < {subj} >,
#    DTR.SYNSEM.LOCAL.CAT.VAL {dtr-val} ].'''.format(**rulevars)

def obj_rem_op_lex_rule():
  return lexrule_name('obj-rem-op') + ''' := comps-change-only-lex-rule & no-ccont-lex-rule &
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

    rulevars = {}
    rulename = 'added-arg{}of{}-non-local-lex-rule'.format(added_arg, total_args)
    if added_arg == 1:
        # added subject; existing rules should apply
        rule = rulename + ' := same-non-local-lex-rule.'
    else:
        compslist = []
        for i in range(2, total_args+1):
            if i == added_arg:
                compslist.append(NEW_NON_LOCAL_FRAGMENT)
            else:
                compslist.append('[ ]')
        rulevars['rulename'] = rulename
        rulevars['comps'] = ', '.join(compslist)
        rule = '''{rulename} := lex-rule &
  [ SYNSEM [ LOCAL.CAT.VAL.COMPS < {comps} >,
             NON-LOCAL [ SLASH [ LIST #sfirst,
                                 LAST #slast ],
		         REL [ LIST #rfirst,
			       LAST #rlast ],
		         QUE [ LIST #qfirst,
			       LAST #qlast ] ] ],
    DTR.SYNSEM.NON-LOCAL [ SLASH [ LIST #sfirst,
                                   LAST #smiddle ],
			   REL [ LIST #rfirst,
				 LAST #rmiddle ],
			   QUE [ LIST #qfirst,
				 LAST #qmiddle ] ] ].'''.format(**rulevars)
    return rule


## General argument-adding lexrules

## NOTE: These expect to have val-change-ccont-lex-rule in its ancestry
## somewhere. Normally that should be handled by the morphotactics library,
## which uses val-change-ccont-lex-rule as the base supertype
## for any position class with valence-changing lexical rules.

# Generates the generic applicative (non-scopal) rule
def basic_applicative_lex_rule():
    return  '''{rulename} := comps-change-only-lex-rule &
  [ C-CONT [ RELS <! event-relation &
                   [ ARG1 #evt ] !>,
             HCONS <! !> ],
    DTR.SYNSEM.LOCAL.CONT.HOOK.INDEX #evt ].'''.format(rulename=lexrule_name('basic-applicative'))


# Generates the generic causative rule
def causative2_lex_rule(demoted_pos, transitive=True):
    rulevars = {}
    OSUBJ_ARG_FRAG = '#osubj & [ LOCAL.CONT.HOOK.INDEX #causee ]'
    arglist = ['#nsubj & [ LOCAL.CONT.HOOK.INDEX #causer ]']
    if demoted_pos.lower() == 'pre':
        # erstwhile subject should be LESS oblique than the object
        arglist.append(OSUBJ_ARG_FRAG)
        arglist.append('#comp')
        comps = '#osubj, #comp'
    else:
        arglist.append('#comp')
        arglist.append(OSUBJ_ARG_FRAG)
        comps = '#comp, #osubj'
    rulevars['rulename'] = lexrule_name('causative')
    rulevars['arg-st'] = ',\n             '.join(arglist)
    rulevars['comps'] = comps
    rule = '''{rulename} := same-spr-lex-rule &
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
    SYNSEM.LOCAL.CAT.VAL [ SUBJ < #nsubj & [ LOCAL.CAT.VAL [ SPR < >,
                                                             COMPS < > ] ] >,
                           COMPS < {comps} > ],
    ARG-ST < {arg-st}  >,
    DTR [ ARG-ST < #osubj, 
                   #comp >,
          SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #osubj >,
                                   COMPS < #comp > ],
                         CONT.HOOK.LTOP #larg ] ] ].'''.format(**rulevars)
    return rule

def causative_lex_rule(demoted_pos, transitive=True):
    rulevars = {}
    OSUBJ_ARG_FRAG = '#osubj & [ LOCAL.CONT.HOOK.INDEX #causee ]'
    compslist = []
    if demoted_pos.lower() == 'pre':
        # erstwhile subject should be LESS oblique than the object
        compslist.append(OSUBJ_ARG_FRAG)
        if transitive: compslist.append('#comp')
    else:
        if transitive: compslist.append('#comp')
        compslist.append(OSUBJ_ARG_FRAG)
    rulevars['rulename'] = lexrule_name('causative', transitive)
    rulevars['comps'] = ', '.join(compslist)
    rulevars['dtr-comps'] = '#comp' if transitive else ''
    rule = '''{rulename} := same-spr-lex-rule & same-spec-lex-rule &
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
    SYNSEM.LOCAL.CAT.VAL [ SUBJ < [ LOCAL [ CAT.VAL [ SPR < >,
                                                      COMPS < > ],
                                            CONT.HOOK.INDEX #causer ] ] >,
                           COMPS < {comps} > ],
    DTR [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ < #osubj >,
                                   COMPS < {dtr-comps} > ],
                         CONT.HOOK.LTOP #larg ] ] ].'''.format(**rulevars)
    return rule

# Generates the valence-specific constraint on the type of an added argument.
def added_arg_head_lex_rule(arg, head):
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
    



ADDED_ARG_APPLICATIVE_FRAGMENT = ''' [ LOCAL [ CAT [ VAL [ SPR < >,
                                                           COMPS < > ] ],
                                               CONT.HOOK.INDEX #nind ] ]'''
                            
# Generates the valence-specific applicative LR supertype.     
# Inherits from the valence-specific non-local rule and the generic applicative rule.   
def added_arg_applicative_lex_rule(added_arg, total_args):
    rulevars = {}
    compslist = []
    for i in range(1, total_args+1):
        if i == 1:
            continue
        elif i == added_arg:
            compslist.append(ADDED_ARG_APPLICATIVE_FRAGMENT)
        else: 
            compslist.append('#ocomp')
    rulevars['comps'] = ', '.join(compslist)
    rulevars['rulename'] = lexrule_name('added-arg-applicative', added_arg, total_args)
    rulevars['basic-applicative-rule'] = lexrule_name('basic-applicative')
    rulevars['added-arg-non-local-rule'] = lexrule_name('added-arg-non-local', added_arg, total_args)
                 
    rule = '''{rulename} := {basic-applicative-rule} & {added-arg-non-local-rule} &
  [ SYNSEM.LOCAL.CAT.VAL.COMPS < {comps} >,
    C-CONT [ RELS <! [ ARG2 #nind ] !> ],
    DTR [ SYNSEM.LOCAL [ CAT.VAL.COMPS < #ocomp > ] ] ].'''.format(**rulevars)
    return rule



##########

subj_rem_op_lex_rule_gen = subj_rem_op_lex_rule
obj_rem_op_lex_rule_gen = obj_rem_op_lex_rule
added_arg_applicative_lex_rule_gen = added_arg_applicative_lex_rule
added_arg_non_local_lex_rule_gen = added_arg_non_local_lex_rule
basic_applicative_lex_rule_gen = basic_applicative_lex_rule


rule_generators = {'subj-rem-op': {'name': subj_rem_op_lex_rule_name,
                                   'rule': subj_rem_op_lex_rule_gen},
                   'obj-rem-op':  {'name': (lambda: 'obj-rem-op-lex-rule'),
                                   'rule': obj_rem_op_lex_rule_gen },
                   'basic-applicative': {'name': (lambda: 'basic-applicative-lex-rule'),
                                         'rule': basic_applicative_lex_rule_gen},
                   'added-arg-applicative': {'name': added_arg_applicative_lex_rule_name,
                                             'rule': added_arg_applicative_lex_rule_gen},
                   'added-arg-non-local': {'name': added_arg_non_local_lex_rule_name,
                                           'rule': added_arg_non_local_lex_rule_gen}}


def gen_rulename(rule_type, *args):
    fn = rule_generators[rule_type]['name']
    return fn(*args)

def gen_rulebody(rule_type, *args):
    fn = rule_generators[rule_type]['rule']
    return fn(*args)


# Small helper classes to get proper set semantics:
# We want to create a set of all the generated rules, but we only want 
class FnWrapper(object):
    def __init__(self, label, fn, *args):
        self.label = label
        self.fn = fn if len(args) == 0 else partial(fn, *args)
    def __hash__(self):
        return hash(self.label)
    def __call__(self):
        return self.fn()

class LexRuleWrapper(object):
    def __init__(self, rule_type, *args):
        self.rule_name = lexrule_name(rule_type, *args)
        self.rule_body = lexrule_gen(rule_type, *args)


class LexRuleBuilder(object):
    def __init__(self):
        self.rules = set()
    def add(self, rule_label, rulegen, *rulegen_args):
        self.rules.add(FnWrapper(rule_label, rulegen, *rulegen_args))
    def generate_tdl(self, mylang):
        prev_section = mylang.section
        mylang.set_section('lexrules')
        for rule in self.rules:
            mylang.add(rule())
        mylang.set_section(prev_section)

####### MAIN INTERFACE ##########

# Add generic rules
def customize_valence_change(mylang, ch, lexicon, rules, irules, lrules):
    from gmcs.linglib.morphotactics import all_position_classes
    rules = LexRuleBuilder()
    for pc in ch['verb-pc']:
        pc_key = pc.full_key
        pc_inputs = pc.get('inputs', [])
        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
        for lrt in pc.get('lrt', []):
            for vchop in lrt.get('valchg', []):
                opname = vchop['operation'].lower()
                if opname == 'subj-rem':
                    rules.add('subj-rem-tr-op', subj_rem_op_lex_rule, True)
                    rules.add('subj-rem-itr-op', subj_rem_op_lex_rule, False)
                if opname == 'obj-rem':
                    rules.add('obj-rem-op', obj_rem_op_lex_rule)
                if opname == 'obj-add':
                    rules.add('basic-applicative', basic_applicative_lex_rule)
                    position = vchop.get('argpos', '').lower()
                    argtype = vchop.get('argtype', '').lower()
                    if position == 'pre':
                        rules.add('added-arg-head-type', added_arg_head_lex_rule, 2, argtype)
                        rules.add('added-arg-applicative', added_arg_applicative_lex_rule, 2, 3)
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 2, 3)
                    elif position == 'post':
                        rules.add('added-arg-head-type', added_arg_head_lex_rule, 3, argtype)
                        rules.add('added-arg-applicative', added_arg_applicative_lex_rule, 3, 3)
                        rules.add('added-arg-non-local', added_arg_non_local_lex_rule, 3, 3)
                if opname == 'subj-add':
                    position = vchop.get('argpos', '').lower()
                    inputval = vchop.get('input', '').lower()
                    if 'iverb' in pc_inputs:
                        rules.add('causative-intrans', causative_lex_rule, position, False)
                    if 'tverb' in pc_inputs:
                        rules.add('causative-trans', causative_lex_rule, position, True)
    rules.generate_tdl(mylang)

# Add new rule specifications (e.g. for transitive/intransitive variants)
def add_lexrules(ch):
    pass
#    for pc in ch['verb-pc']:
#        pc_key = pc.full_key
#        idx = pc['lrt'].next_iter_num() if 'lrt' in pc else 1
#        for lrt in pc.get('lrt', []):
#            for vchop in lrt.get('valchg', []):
#                opname = vchop['operation'].lower()
#                if opname in ['subj-add', 'obj-add', 'subj-rem']:
#                    inputs = vchop.get('inputs', []).split(',')
#                    if len(inputs) > 1:
#                        # need both, so make this one intrans-only
#                        vchop['inputs'] = 'intrans'
#                        lrt_base_name = lrt['name']
#                        lrt['name'] = lrt_base_name + '-itr'
#                        # and create new one
#                        idx = ch[pc_key + '_lrt'].next_iter_num()
#                        lrt_key = pc_key + '_lrt' + str(idx)
#                        ch[lrt_key + '_name'] = lrt_base_name + '-tr'
#                        valchg_idx = ch[pc_key + '_lrt']
                        
#                    if need_tv and need_iv:
#                        # split into transitive/intransitive-specialized versions
#                        key = pc.full_key + '_lrt' + str(idx)
#                        name = get_name(pc) + '-transitive'
#                        ch[key + '_name'] = name
#                        #ch[key + _
                    


                

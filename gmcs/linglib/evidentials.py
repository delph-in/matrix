from gmcs.utils import TDLencode
from gmcs.utils import orth_encode
from gmcs.lib import TDLHierarchy

#####################################
# Customization for Evidentiality
#
# Mike Haeger, 2017
#####################################

EVIDENTIAL_LEX_RULE = '''evidential-lex-rule := cont-change-only-lex-rule &
	same-spr-lex-rule &
	same-spec-lex-rule &
  [ C-CONT [ RELS <! event-relation &
				   [ LBL #ltop,
					 ARG0 event,
					 ARG1 individual,
					 ARG2 #harg,
					 ARG3 handle ] !>,
			 HCONS <! qeq & [ HARG #harg,
							  LARG #larg ] !>,
			 HOOK [ LTOP #ltop,
					INDEX #mainev,
					XARG #mainagent ] ],
	DTR.SYNSEM.LOCAL.CONT.HOOK [ LTOP #larg,
	                        XARG #mainagent,
	                        INDEX #mainev ] ].
'''
'''	RELS <! event-relation &
				   [ LBL #ltop,
					 ARG0 #evidev, ; evidential event
					 ARG1 individual, ; information receiver
					 ARG2 #harg, ; main event
					 ARG3 handle ] !>, ; information source'''

# ch = choices file dictionary
def customize_evidentials(mylang, ch, lexicon, rules, lrules, hierarchies):
	default_choices = ['firsthand', 'nonfirsthand', 'visual', 'nonvisual', 'inferential', 'reported', 'quoatative', 'everything-else']
	# define evidential inventory
	evidential_inventory = []
	evidential_definition = ch.get('evidential-definition')
	if evidential_definition != 'noevid':
	# if preset choices:
		if evidential_definition == 'choose':
			for term in default_choices:
				if ch.get(term) == 'on':
					evidential_inventory.append(term)
	#   deal with variable names
		elif evidential_definition == 'build':
			for term in ch['evidential']:
				term_name = term['name'].lower()
				evidential_inventory.append(term_name)

		prev_section = mylang.section
		mylang.set_section('lexrules')
		mylang.add(EVIDENTIAL_LEX_RULE)
		for term in evidential_inventory:
			infl_evid_def = term + '''-evidential-lex-rule := evidential-lex-rule & 
				[ C-CONT.RELS <! [ PRED "ev_''' + term + '''_rel" ] !> ].
				'''
			mylang.add(infl_evid_def)
		mylang.set_section(prev_section)

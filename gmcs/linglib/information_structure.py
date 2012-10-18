from gmcs.linglib import features
from gmcs.utils import get_name

def customize_information_structure(mylang, ch, rules, hierarchies):
  mylang.add( 'prosody-rule-dtr := word-or-lexrule.', merge = True, section='lexrules')
  mylang.add( """
focus-lex-rule := infl-add-only-no-ccont-lex-rule &
  [ SYNSEM.LOCAL.CONT.ICONS <! focus !>,
    DTR prosody-rule-dtr ].
""", merge = True, section='lexrules')

def add_lexrules(choices):
  pass



##################
### VALIDATION ###
##################

def validate(choices):
  # add validation tests specific to this module
  pass

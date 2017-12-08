import sys
import gmcs.tdl

# Atoms of a possessive strategy:
#
# The rule that combines possessor and possessum
#     Sometimes must be added separately
#     Sometimes already there
#
# For each affix:
# Lexical rule type
#
# For each non-affix:
# Lexical entries + rules to attach these words to their heads



# PRIMARY FUNCTION
def customize_adnominal_possession(mylang,ch,rules,irules,lexicon):
    mylang_real=tdl.TDLfile(mylang)
    print mylang
    
    mylang.set_section('adnom-poss')

# What customize_adnominal_possession does:
#    For each strategy (or strategy object?):
#        call customize_rules()
#        call customize_irules()
#        call customize_lexicon()


# SECONDARY FUNCTIONS
# def customize_rules(mylang,ch,rules)
#     IF: possessor is specifier-like, then add head-spec-hc
#     IF: possessor is modifier-like and it's possessor marking, then add head-mod.
#     IF: possessive phrases are out of order, then add a new rule with the right order
#         with a req that its inflected component be POSS +

# NOTE: customize_irules doesn't yet deal with situations where one marker is an affix and one isn't:
# NOTE: customize_irules and customize_lex both don't handle agreement yet

# def customize_irules(mylang,ch,irules) :
#     IF: either possessor or possessum mark is an affix, add the correct infl-rule
#     Possessor-marker:
#             If the possessor is specifier-like:
#                ADD possessor-lex-rule with SPEC<[possessum]>, carrying poss_rel
#             If the possessor is  modifier-like:
#                ADD possessor-lex-rule with MOD<[possessum]>, carrying poss_rel
#     Possessum-marker:
#        If the possessor is specifier-like:
#             If single-marking:
#                ADD possessum-lex-rule with SPR<[possessor]>, carrying poss_rel
#             If double-marking:
#                ADD possessum-lex-rule with SPR<[possessor]>
#        If the possessum is modifier-like:
#             If single-marking:
#                ADD possessum-lex-rule with COMPS<[possessor]>, carrying poss_rel
#             If double-marking
#                ADD possessum-lex-rule with COMPS<[possessor]>
#                ADD a feature [HEAD.POSS +] to the possessor infl rule.
#
#
#


# TODO: figure out what happens with double marking
# TODO: add in where the poss_rel is

# def customize_lex(mylang,ch,lexicon):
#     IF: either possessor or possessum mark is a separate word, add the correct lexical entry
#     Possessor-marker:
#        If the possessor is specifier-like:
#            ADD possessor-det-lex, with SPR<[possessor]>, SPEC<[possessum]>, carrying poss_rel
#             OR TRY:
#            ADD possessor-adp-lex, with COMPS<[possessor]>,SPEC<possessum]>
#        If the possessor is modifier-like:
#            ADD possessor-adp-lex, with COMPS<[possessor]>, MOD<[possessum]>
#     Possessum-marker:
#        If the possessor is specifier-like:
#             ADD possessum-noun-lex, with COMPS<[possessum]>, SPR<[possessor]>
#        If the possessor is modifier-like:
#             ADD possessum-noun-lex, with COMPS<[possessum][possessor]>






# Not sure making an object from the choices file is really that helpful, 
# but here's one way you could do it if you wanted to:
#
# A strategy object contains:
#
# Primitives from questionnaire:
# (Try not to add too many obscuring layers between choices file and this)
# juxtaposition: boolean (for convenience -- could be encoded in other variables)
# specifier vs. mod
# NOM: bool
# order: head-first or head-final or free
# mark-loc: head, dep, both
# possessor-affix: boolean
# possessor-sep-word: left-attach, right-attach, none
# possessum-affix: boolean
# possessum-sep-word: left-attach, right-attach, none
#
# Deduced properties:
# rule-type: head-spec-hc, head-comps, head-mod
#     DEPENDS ON: spec-like, mark-loc
# add-lex-rules: bool
#     DEPENDS ON: possessor-affix, possessum-affix
# add-lex-entries: bool
#     DEPENDS ON: possessor-sep-word, possessum-sep-word
# add-phrasal-rules: bool
#     DEPENDS ON: order, rule-type (anything but head-comp guarantees a rule add), 
#     [order info from word-order page]
#
# TDL-adding functions:
# Lexical rules.
#     DEPENDS ON: mark-loc, possessor-affix, possessum-affix
#     [Also unsure of how this interfaces with the stuff on Morphology page]
# Lexical entries.
#     DEPENDS ON: mark-loc, possessor-sep-word, possessum-sep-word
# Phrasal rules for attaching possessor and possessum:
#     DEPENDS ON: order, rule-type, [info from word-order lib]





# Choice 1:
# Order of possessor and possessum
#
# Consequence: 
# Depending on the preexistent order of head-spec, head-mod, and head-comps,
# May have to create a specific version of the above that has the appropriate order:
#     Whatever rule is used in this strategy will have to be added and with the right constraints.
# If preexistent order of head-spec, head-mod, and head-comps is already correct,
# Do nothing.


# Choice 2:
# Modifier vs. specifier
#
# Consequence:
# If specifier, always add head-spec-hc.
#     Also, MANY OTHER CHOICES FALL OUT FROM THIS
# If modifier, add either head-mod or leave in head-comp.
#     Also, MANY OTHER CHOICES FALL OUT FROM THIS

# Choice 3:
# Possessor = NOM vs NP
# 
# Consequence:
# If NOM, then require the possessor (in head-mod or head-spec or head-comps)
# to be SPR 1-dlist. 
# If NP, then require the possessor to be SPR olist.

# Choice 4:
# Complicated stuff

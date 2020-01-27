'''
Choices v. 29

Try to use these constants instead of literal strings, when possible.
This helps prevent typo bugs, which can sometimes be tricky to debug in python,
and makes the code much easier to maintain (suppose a convention for a particular
choice changes; you will only need to correct it once here rather than throughout
the code).

Append with new constants, if they are likely to be used across modules.
'''



### Choices for values of checkboxes, radio buttons ###

ON = 'on' # Choice convention for checked box
YES = 'yes' # Choice convention for selected radio button

ORTH = 'orth' # Choice convention for orthographies associated with stems and affixes.

### Section names ###

WORD_ORDER = 'word-order' # Choices section associated with word order subpage
VERB = 'verb' # Section containing verb lexical items
VALENCE = 'valence' # Valence attributes of a verb lexical item


### Widely used type names' prefixes ###
COMP_HEAD = 'comp-head'
HEAD_COMP = 'head-comp'

### OFTEN USED COMBINATIONS ###
OV_ORDERS = ['sov', 'ovs', 'osv', 'v-final']
VFINAL = ['sov','osv','v-final']
VO_ORDERS = ['svo', 'vos', 'vso', 'v-initial']

### WH-QUESTIONS

IN_SITU = 'in-situ' # No question phrase fronting
MTRX_FRONT = 'front-matrix' # Question phrases fronting in matrix clauses
EMBED_FRONT = 'front-embed' # Question phrases fronting in embedded clauses
WH_QUE_PTCL = 'wh-q-part' # Question particle for constituent questions
WH_QUE_INFL = 'wh-q-infl' # Inflectional paradigm for constituent questions
MULTI = 'multi' # Multiple (e.g. fronting)
ALL_OBLIG = 'all-oblig' # All question phrases are fronted obligatorily
SG_OBLIG = 'single-oblig' # One question phrase is obligatorily fronted
NO_MULTI = 'no-multi-ques' # No multiple questions in one clause
PIED = 'pied-pip' # Pied piping
PIED_ADP = 'pied-pip-adp' # Pied piping of specifically adpositions
EMB_PIP = 'embed-pied-pip' # Pied piping in embedded clauses
EMB_PIP_ADP = 'embed-pied-pip-adp' # Pied piping of adpositions in embedded clauses
OBL_PIP_NOUN = 'oblig-pied-pip-noun' # Obligatory pied piping of nouns
OBL_PIP_ADP = 'oblig-pied-pip-adp' # Obligatory pied piping of adpositions
EMB_OBL_PIP_N = 'embed-oblig-pied-pip-noun' # Obligatoriness of pied piping of nouns in embedded clauses
EMB_OBL_PIP_ADP = 'embed-oblig-pied-pip-adp' # Obligatoriness of pied piping of adpositions in embedded clauses
MTRX_FR_OPT = 'matrix-front-opt' # Optionality of fronting in matrix clauses
EMB_FR_OPT = 'embed-front-opt' # Optionality of fronting in embedded clauses
X_FR_OPT = 'from-embed-front-opt' # Optionality of fronting from the embedded clause
# across the boundary to the front of the matrix clause

### LEXICAL TYPE NAMES (SOME); see lexbase.py
WH_PRO = 'wh-pronoun-noun-lex'
INTER = 'inter' # Interrogative words flag
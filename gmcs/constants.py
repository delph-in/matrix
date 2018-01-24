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

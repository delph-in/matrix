# Attempt to flag specific features which were added to the grammar by a component.
# One such feature is INIT; it is not needed in all grammars but can be added by the Word Order library,
# the clausal complements library, the clausal modifiers library, and the Wh-questions library
# (and possibly other libraries).

USED_FEATURES = {'INIT':False}
USED_TYPES = {'qdet':False}
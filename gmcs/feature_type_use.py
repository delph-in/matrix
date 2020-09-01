# OZ 2020-09-01
# This module is a stub (and could be removed, along with a couple references which
# aren't doing anything useful right now).
# The reason I am leaving this in is to mark something I started,
# in the hopes it informs future developer.
# The idea is to collect some sort of statistics of how features are used
# and by which libraries.

# One feature is INIT; it is not needed in all grammars but can be added by the Word Order library,
# the clausal complements library, the clausal modifiers library, and the Wh-questions library
# (and possibly other libraries).

# How should this be populated exactly?
feature_stats = {}

# The below is just something I started, but it is conflated with just feature use
# (whether something was used or not) which is not likely to be necessary?
USED_FEATURES = {'INIT':False}
USED_TYPES = {'qdet':False,'qpro':False,'qadv':False,'qverb':False}
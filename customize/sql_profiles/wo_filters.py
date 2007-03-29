#################################################################
# Language-type specific filters for word order.  Note that these
# apply to all sentences, not just the ones with 'wo' mrs_tags,
# because all sentences have word order!
#
# Each filter provides a list of feature-value pairs from language
# type definitions that it cares about.
#
# When the constraint is an 'or' (e.g., wordorder is 'sov' or 'osv')
# we put both wordorder:sov and wordorder:osv on the list.  The
# query which generates the gold standard test suites looks for all
# filters which match the particulare feature-value pairs for the language
# type in question.


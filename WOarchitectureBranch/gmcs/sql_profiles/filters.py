"""
File: filters.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents:
    - filter_one_result - function that is no longer relevant
    - Filter - abstract class that maintains a list of semantic tags it applies to, a formatted list of
                         feat/val pairs, or lack of features, that define the language types it is relevant to,
                         and runs a filter on sentences
    - FalseFilter - subclass of filter that falis every sentence
    - MatchFilter - subclass of Filter that fails sentences not containing a regex.
    - NotMatchFilter - subclass of Filter that fails sentences containing a regex.
    - IfFilter - subclass of Filter that fails sentences containing one regex but not another
    - IfNotFilter - subclass of Filter failing sentences containg one regex and another
    - OrFilter - subclass of Filter failing sentences not containing at least one of two regexes.
    - AndFilter - deprecated subclass of Filter failing sentences not containing both of two
                      regexes.  Deprecated on 8/19/09 and decided to go with two MatchFilters to
                      create same functionality.  Comments of those two MatchFilters should reference
                      each other for documentation
    - AndNotFilter - deprecated subclass of Filter failing sentences either not containing one regex
                           or containing the other.  Deprecated on 9/5/09 due to logical equivalence with
                           one MatchFilter and one NotMatchFilter.  Comments of those two filters
                           should reference each other
    - NandFilter  - deprecated subclass of Filter failing sentences containing both of two regexes.
                         It is no longer used because it is logically equivalent to IfNotFilter
    - filter_results - function that is no longer relevant
    - getFilterID - function that, given a filter's name, returns its ID in MatrixTDB
    - insertFilter - function that adds a filter to the filter table of MatrixTDB.
    - insertManyFilteredSpecResults - function that inserts several specific filter results into
                                                      res_sfltr at a time.
    - insertFilteredSpecResult - function that inserts the result of running a SPECIFIC filter on a
                                            result/item into res_sfltr
    - insertFilteredResult - function that inserts the result of running a UNIVERSAL filter on a
                                     result/item into res_fltr
Tables Accessed: filter, res_fltr, res_sfltr
Tables Modified: filter, res_fltr, res_sfltr
"""

######################################################################
# This script interacts with the MySQL DB storing the string-mrs pairs
# to identify those pairs which are universally ungrammatical, and
# mark them as such. It actually groups together a bunch of different
# filters, each of which corresponds to a field in the DB.
#
# If each filter corresponds to a field, then there should be three
# possible values for each filter on each string-mrs pair:
#
#      2 -- filter is not applicable
#      1 -- filter is applicable and this string-mrs pair passes
#      0 -- filter is applicable and this string-mrs pair does not pass
#
# Filters are `applicable' to all strings for the mrs_ids they care
# about.  With the above system, assuming `1' and `2' both map to `true'
# and `0' to `false', universally ungrammatical strings are those for
# which the AND of the universal filters is false.  Potentially grammatical
# ones are those for which the AND is true.
#
# This file defines the general types of filters and the function which
# calls and applies them.  Other files will import this one, define
# particular filters, and invoke them.

######################################################################
# TODO
#
# 1. Rework things so that this can be invoke with particular mrs_ids,
# and only pull items with those mrs_ids out of the DB for consideration.
# This will be useful when we are adding to an existing DB. ... Or not:
# presumably, adding i_ids means adding new filters, which means that
# we need values for each of those filters for the rest of the strings.
#
# 2. Come up with a more transparent naming scheme for the filter classes.

######################################################################
# ALTERNATIVE PSEUDOCODE -- probably more efficient and compact
#
# select i-id from MatrixTDB
#
# For each i-id in ids
#    for each filter
#        add [filter_name, filter_value] to dictionary
#    update DB entry for i-id with appropriate filter_value for each
#                     filter_name

######################################################################
# Preliminaries

import MySQLdb
import re
import sys

#######################################################################
# filter_string(mrs_id,string)
#
# Calls each individual filter on string-mrs pair.  Returns an array
# where the keys are the names of the filter fields and the values are
# the values assigned by each filter to the string-mrs pair.  Each
# particular filter should return its name and its value.

def filter_one_result(mrs_id, sent, filter_list, filter_id_hash):
    """
    Function: filter_one_result
    Input:
        mrs_id - an mrs tag
        sent - a string
        filter_list - a list of Filters
        filter_id_hash - a hash that maps from a filter name to an ID
    Output: filter_values - a hash that maps from a filter ID to the  result of running that filter on
                                    the sent/mrs_id combo.
    Functionality: Runs a set of filters on the [incr_tsdb()] result/item that is sent and records the
                         "results" of those runs in a hash that it returns.
    Note: This code is untested, as it isn't currently called form any working code.  KEN belives it
              is obsolete now that he's written other code to run filters on results
    Called From:
        run_u_filters.update_res_fltr, which doesn't work.  See that function for details
        filters.filter_results, which doesn't seem to be called from anywhere
        test_u_filters.py, which I'm not using now.
    Another Version:
        This has been copied and modified in run_specific_filters.py
    """
    filter_values = {}                  # initialize output

    for f in filter_list:                 # for every filter...
        # run the filter on the sentence and store the result of that in the output hash
        filter_values[filter_id_hash[f.name]] = f.exe(mrs_id, sent)

        # NOTE: these next two lines are only how it should work for universal filters, not for
        # specific filters.
        if filter_values[filter_id_hash[f.name]] == 0:      # if it failed...
            break                                                       # ...then stop testing 

    return filter_values        # return output

########################################################################
# Filter class

class Filter:
    """
    Class: Filter
    Members:
        name - the name of the filter.  This should be unique across all instantiated Filters
        mrs_id_list - a list of mrs tags to which this filter applies
        comment - a string in English to explain what the filter is doing and why
        fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs to
             exist in a language type for this filter to care about a string in the language type
    Functionality: makes sure strings of a set of semantic classes and potentially that are of a
                         language type with or without a certain combination of features/values obey a
                         particular rule
    """    
    def __init__(self, name, mrs_id_list, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this Filter
            name - the name of the filter.  This should be unique across all instantiated Filters
            mrs_id_list - a list of mrs tags to which this filter applies
            comment - a string in English to explain what the filter is doing and why
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to exist in a language type for this filter to care about a string in the language type.
                 Default is None, which is the case for all universal filters
        Output: a new Filter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """ 
        self.name = name                      # assign argument name to member name
        self.mrs_id_list = mrs_id_list      # assign argument mrs_id_list to member mrs_id_list
        self.comment = comment           # assign argument comment to member comment
        self.fv = fv                                  # assign argument fv to member fv
        return

    def check_mrs_id(self, mrs_id):
        """
        Method: check_mrs_id
        Input:
            self - this Filter
            mrs_id - an mrs tag
        Output: True if mrs_id is in self.mrs_id_list, False otherwise
        Functionality: Checks to see if this filter cares about semantic class mrs_id
        Tables accessed: none
        Tables modified: none
        """
        return mrs_id in self.mrs_id_list

    # TODO: this function is wrong b/c it doesn't consider the fv lists in s_filters.py that end in a
    # colon.  E.g., fv = 'aux-verb:' which i think means if aux-verb is off given the context.  Of
    # course it looks like what's actually in the choices file is has-aux:, not aux-verb:
    def check_fv(self, fvset, fvlist = None):
        """
        I'm not sure if this is called from anywhere anymore.  It shouldn't be called from
        run_specific_filters, and in generate_s_profile I rely on the fltr_feat_grp table to tell me
        what filters apply to what language types.  I don't think this is relelvant anymore.
        It's called twice from itself, and once from Filter.exe.  That function is called once from
        debug_filter (which I'm not using), filter_one_result (which is not being called from any
        working code), test_s_filters (which I'm not using), and run_specific_filters.filter_one_result
        (which is not being called from anywhere).  So this _is_ irrelevant now.
        """
        # TODO: there has to be a cleaner way to do this.
        # TODO: abstract this fv thing into a class instead of this list structure.
        if fvlist is None:
            if self.fv is None:
                # if it has None for self.fv, then it's probs a u filter
                # and it cares regardless of the lt's fvs
                # TODO: clean this up: two points of return
                return True
            else:
                fvlist = self.fv
                
        if fvlist[0] == 'and':
            for item in fvlist[1:]:
                answer = self.check_fv(fvset, item)
                # as soon as I get a False, get out of here
                if not answer:
                    break
        elif fvlist[0] == 'or':
            for item in fvlist[1:]:
                answer = self.check_fv(fvset, item)
                # as soon as I get a True, get out of here
                if answer:
                    break
        else:
            # TODO: there has to be a cleaner way to do this.
            # now what?
            # i should always be a string here, not a list of strings        
            if type(fvlist) is list:
                # or if i'm a list i should be a list of just one string
                if len(fvlist) > 1:
                    print >> sys.stderr, self.name
                    print >> sys.stderr, self.fv
                    print >> sys.stderr, len(fvlist)
                    for fv in fvlist:
                        print >> sys.stderr, fv
                    raise TypeError, "Ill-formed fv"
                else:
                    fvlist = fvlist[0]
            if fvlist in fvset:
                answer = True
            else:
                answer = False

        return answer

    def exe(self,mrs_id,sent, fvSet):
        """
        Method: exe
        Input:
            self - this Filter
            mrs_id - an mrs tag
            sent - a sentence
            fvSet - probably a set of feature/value pairs
        Output:
            answer - the result of this filter on sent with mrs tag mrs_id given a language type
                         defined by fvSet
        Functionality: returns the value of this filter on sent with mrs tag mrs_id given a language
                             type defined by fvSet
        Tables accessed: none
        Tables modified: none
        Called from:
            - debug_filter, (which I'm not using
            - filters.filter_one_result, which is not being called from any working code
            - test_s_filters,  which I'm not using
            - run_specific_filters.filter_one_result, which is not being called from anywhere
        Note: This code is irrelevant.  It is not being called from any working or code being used.
                  It seems to have been intended for if running specific filters meant only applying
                  them to sentences on a language type, but we actually run specific filters on every
                  item that passed all universal filters and rely on the fltr_feat_grp to tell us if a filter
                  applies to a language type or not.
        """
        if self.check_mrs_id(mrs_id) and self.check_fv(fvSet):
            answer = self.apply_filter(sent)
        else:
            answer = 2

        return answer

    #Might not need this here.
    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this Filter
            sent - a sentence, a string of words
        Output: none
        Functionality: prints "Error" and asserts False.  Seems to be intended as an unnecessary
                             abstract function template
        Tables accessed: none
        Tables modified: none                             
        """
        print "Error"
        assert False    # this will raise an AssertionError

class FalseFilter(Filter):
    """
    Class: FalseFilter
    Superclass: Filter
    Members: None (besides those in Filter)
    Functionality: returns a fail result every time it is applied
    """
    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this FalseFilter
            sent - a sentence, a string of words
        Output: 0, a fail result for a filter
        Functionality: returns 0, a fail result, every time it is applied
        Tables accessed: none
        Tables modified: none
        """
        return 0


class MatchFilter(Filter):
    """
    Class: MatchFilter
    Superclass: Filter
    Members: re1 - a string representing a regular expression that must be present in a string
    Functionality: filters out strings that don't have re1 in them
    """    
    # Checking for something that must present
    def __init__(self, name, mrs_id_list, re1, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this MatchFilter
            name - the name of this MatchFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this MatchFilter applies
            re1 - a string representing a regular expression that must be present in a string for it
                    to pass this filter
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this filter to care about a string in the language type.
                 Default is None
        Output: a new MatchFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this MatchFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 is present in sent, 0 otherwise
        Functionality: applies this MatchFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """
        if re.search(self.re1, sent):       # if re1 is in sent...
            answer = 1                         # ...set output to 1, or pass
        else:                                      # if re1 is not in sent...
            answer = 0                         # ...set output to 0, or fail

        return answer                         # return output


class NotMatchFilter(Filter):
    """
    Class: NotMatchFilter
    Superclass: Filter
    Members: re1 - a string representing a regular expression that must not be present in a string
    Functionality: filters out strings that have re1 in them
    """    
    # Checking for something that can't be present.
    def __init__(self, name, mrs_id_list, re1, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this NotMatchFilter
            name - the name of this NotMatchFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this NotMatchFilter applies
            re1 - a string representing a regular expression that must be present in a string for it
                    to pass this filter
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this filter to care about a string in the language type.
                 Default is None
        Output: a new NotMatchFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """        
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this NotMatchFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 is absent in sent, 0 otherwise
        Functionality: applies this NotMatchFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """        
        if re.search(self.re1, sent):       # if re1 is in sent...
            answer = 0                         # ...set output to 0, or fail
        else:                                      # if re1 is not in sent...
            answer = 1                         # ...set output to 1, or pass

        return answer                         # return output


class IfFilter(Filter):
    """
    Class: IfFilter
    Superclass: Filter
    Members: re1, re2 - string representing regular expressions.  If re1 is present, r2 must be, too
    Functionality: filters out strings that have re1 in them but not re2
    """        
    # Checking for something that must be present, but only under
    # some particular condition.
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this IfFilter
            name - the name of this IfFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this IfFilter applies
            re1, re2 - strings representing regular expressions.  if re1 is present, re2 must be, too
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this IfFilter to care about a string in the language type.
                 Default is None
        Output: a new IfFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """                
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return
    
    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this IfFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 is in sent and self.re2 is not, 0 otherwise
        Functionality: applies this IfFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """                
        # if re1 is in sent and re2 is not...
        if re.search(self.re1,sent) and (not re.search(self.re2,sent)):
            answer = 0            # ...set output to 0, or fail
        else:                         # otherwise...
            answer = 1            # ...set output to 1, or pass

        return answer            # return output
    
class IfNotFilter(Filter):
    """
    Class: IfNotFilter
    Superclass: Filter
    Members: re1, re2 - strings representing regular expressions.  If re1 is present, r2 must not be
    Functionality: filters out strings that have both re1 and re2 in them
    """            
    # Checking for something that must not be present, but only under
    # some particular condition.
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this IfNotFilter
            name - the name of this IfNotFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this IfNotFilter applies
            re1, re2 - strings representing regular expressions.  if re1 is present, re2 must not be
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this IfNotFilter to care about a string in the language
                 type. Default is None
        Output: a new IfNotFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """                        
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this IfNotFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 and self.re2 are in sent, 0 otherwise
        Functionality: applies this IfNotFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """
        if re.search(self.re1,sent) and re.search(self.re2,sent):   # if both re1 and re2 are in sent
            answer = 0            # ...set output to 0, or fail
        else:                         # otherwise...
            answer = 1            # ...set output to 1, or pass

        return answer            # return output

class OrFilter(Filter):
    """
    Class: OrFilter
    Superclass: Filter
    Members: re1, re2 - strings representing regular expressions.  One (inclusive) or the other
                                  must be present
    Functionality: filters out strings that have neither re1 or re2 in them
    """                
    # Checking for two things, one of which must be present.  This is
    # logically equivalent to checking for something that must be present,
    # but only if something else isn't present (if not A then B), and
    # used to be called NegTrigMatchFilter.
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this OrFilter
            name - the name of this OrFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this OrFilter applies
            re1, re2 - strings representing regular expressions.  either (inclusive) re1 or re2 must be
                          present
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this OrFilter to care about a string in the language
                 type. Default is None
        Output: a new OrFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """                        
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this OrFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 (inclusive) or self.re2 are in sent, 0 otherwise
        Functionality: applies this OrFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """        
        if re.search(self.re1,sent) or re.search(self.re2,sent):   # if re1 (inclusive) or re2 are in sent
            answer = 1            # ...set output to 1, or pass
        else:                         # otherwise...
            answer = 0            # ...set output to 0, or fail

        return answer            # return output

class AndFilter(Filter):
    """
    Class: AndFilter
    Superclass: Filter
    Members: re1, re2 - strings representing regular expressions.  Both must be present
    Functionality: filters out strings that are missing re1, re2, or both
    History: 8/19/09: deprecated.  It is logically equivalent to two MatchFilters, and those should
                             be used in place of this one.  The comments of related MatchFilters should
                             reference each other for documentation
    """                    
    # Checking for two things that must both be present.    
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this AndFilter
            name - the name of this AndFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this AndFilter applies
            re1, re2 - strings representing regular expressions.  both must be present
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this AndFilter to care about a string in the language
                 type. Default is None
        Output: a new AndFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        History: 8/19/09: added warning message to tell user it was deprecated
        """
        # first warn user about creating depcreated class
        print >> sys.stderr, 'You are instantiating AndFilter, a deprecated class, for filter', name, \
                                      '.  Please use the logicallequivalence of two MatchFilter classes ' + \
                                      "instead.  Those two filters' comments should reference each other."
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this AndFilter
            sent - a sentence, a string of words
        Output: answer - 1 if self.re1 and self.re2 are both in sent, 0 otherwise
        Functionality: applies this AndFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """        
        if re.search(self.re1,sent) and re.search(self.re2,sent):   # if both re1 and re2 are in sent
            answer = 1            # ...set output to 1, or pass
        else:                         # otherwise...
            answer = 0            # ...set output to 0, or fail

        return answer            # return output

class AndNotFilter(Filter):
    """
    Class: AndNotFilter
    Superclass: Filter
    Members: re1, re2 - strings representing regular expressions.  re1 must be present, but re2
                                  must not be
    Functionality: a deprecated class that filters out strings that are either missing re1 or contain
                         re2
    History:
        9/5/09 - deprecated.  It is equivalent to a MatchFilter and a NotMatchFilter.
    """     
    # Checking for one things that must be present and another
    # thing that must NOT be present.
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this AndNotFilter
            name - the name of this AndNotFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this AndNotFilter applies
            re1, re2 - strings representing regular expressions.  re1 must be present, but re2 must
                          not be
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this AndNotFilter to care about a string in the
                 language type. Default is None
        Output: a new AndNotFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """                        
        # first warn user about creating depcreated class
        print >> sys.stderr, 'You are instantiating AndNotFilter, a deprecated class, for filter', \
                                      name, '.  Please use the logical equivalence of a MatchFilter and ' + \
                                      "a NotMatchFilter instead.  Those two filters' comments should " + \
                                      "reference each other."
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this AndNotFilter
            sent - a sentence, a string of words
        Output: answer - 0 if self.re1 is not in sent or if self.re2 is in sent, 1 otherwise
        Functionality: applies this AndNotFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """
        # if re1 is in sent and re2 is not...
        if re.search(self.re1,sent) and not re.search(self.re2,sent):
            answer = 1            # ...set output to 1, or pass
        else:                         # otherwise...
            answer = 0            # ...set output to 0, or fail

        return answer            # return output

class NandFilter(Filter):
    """
    Class: NandFilter
    Superclass: Filter
    Members: re1, re2 - strings representing regular expressions, both of which can't be present
                                 (but just one can be)
    Functionality: filters out strings that contain both re1 and re2
    History: 8/19/09: deprecated.  It is logically equivalent to IfNotFilter, and that one should be
                 used in place of this one
    """     
    # Checking for two things that can't both be present.  This is
    # the logical NAND (NOT AND) condition.
    def __init__(self, name, mrs_id_list, re1, re2, comment, fv = None):
        """
        Method: __init__
        Input:
            self - this NandFilter
            name - the name of this NandFilter.  Should be unique across all instantiated Filters
            mrs_id_list - the semantic tags of sentences to which this NandFilter applies
            re1, re2 - strings representing regular expressions.  re1 must be present, but re2 must
                          not be
            comment - description of filter
            fv - a formatted list defining what combination of feat/val pairs, or lack of features, needs
                 to  exist in a language type for this NandFilter to care about a string in the
                 language type. Default is None
        Output: a new NandFilter
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        History: 8/19/09: added warning message to tell user it was deprecated
        """
        # first warn user about creating depcreated class
        print >> sys.stderr, 'You are instantiating NandFilter, a deprecated class, for filter', name, \
                                      '.  Please use the logically equivalent class IfNotFilter instead.'
        Filter.__init__(self, name, mrs_id_list, comment, fv)   # call superclass constructor
        self.re1 = re1                                                          # set self.re1 to re1
        self.re2 = re2                                                          # set self.re2 to re2

        return

    def apply_filter(self, sent):
        """
        Method: apply_filter
        Input:
            self - this NandFilter
            sent - a sentence, a string of words
        Output: answer - 0 if self.re1 and self.re2 are both in sent, 1 otherwise
        Functionality: applies this NandFilter to a sentence regardless of semantic class or the
                             language type being evaluated, so those parts must be handled elsewhere.
        Tables accessed: none
        Tables modified: none
        """
        if re.search(self.re1,sent) and re.search(self.re2,sent):   # if both re1 and re2 are in sent...
            answer = 0            # ...set output to 0, or fail
        else:                         # otherwise...
            answer = 1            # ...set output to 1, or pass

        return answer            # return output

######################################################################
# This function will be called in other files which define filters
# and then invoke it.
# 6/19/07 This seems to be used only for the universal filters.

def filter_results(filter_list,filter_type):
    """
    KEN can't find that this is called from anywhere.
    """

    #Connect to MySQL server
    db = MySQLdb.connect(host="localhost", user="ebender",
                         passwd="tr33house", db="MatrixTDB")

    #Create a cursor

    cursor = db.cursor()

    #Check whether all of the filters already correspond to fields
    #in the DB, and if not, create those fields.

    # FIX ME: There's code in run_specific_filters.py that does
    # something similar.

    for f in filter_list:

        name = f.name
        mrs_id_list = f.mrs_id_list
        cursor.execute("SELECT * FROM filter WHERE filter_name = %s", (name))
        record = cursor.fetchall()
        if record == ():
            cursor.execute("INSERT INTO filter SET filter_name = %s, filter_type = %s",
                                                                                                               (name,filter_type))
    

    #Get list of ids from DB.  
    #6/19/07 We have 21 million records in result now, so we can't load them all at once.
    
    #6/20/07 Last night the process stopped at item 6,900,001.  Restarting

    #6/20/07 We seem to be missing entries for 6,900,002, and possibly a couple
    #of others.

    cursor.execute("SELECT r_result_id FROM result LIMIT 8052044, 100000")
    ids = cursor.fetchall()
    limit =  9608642

    while ids != ():

        print "Now working results " + str(limit) + " through " + str(limit + 1000000)

        #`ids' is now a tuple containing elements for each item in a 100,000 slice of the
        #relevant table.  Each of those elements is a tuple which contains
        #just the r_result_id.

        #Based on the r_result_id, we are now going to get the string
        #and mrs_id.

        for id in ids:
            key = id[0]
            cursor.execute("SELECT r_mrs, i_input FROM result,parse,item " + \
                                   "WHERE result.r_result_id = %s " + \
                                   "AND result.r_parse_id = parse.p_parse_id " + \
                                   "AND parse.p_i_id = item.i_id", (key))
            (mrs_id, string) = cursor.fetchone()

            filter_values = filter_one_result(mrs_id,string,filter_list)

            #Should do error checking here: Are all of the values legit?

            for filter_key in filter_values.keys():
                cursor.execute("SELECT filter_id FROM filter WHERE filter_name = %s",
                               (filter_key))
                f_id = cursor.fetchall()
                if len(f_id) > 1:
                    print "Error: Multiple filters with the same name." + filter_key
                else:
                    value = filter_values[filter_key]
                    if not value == 2:
                        f_id = f_id[0][0]
                        cursor.execute("INSERT INTO res_fltr SET rf_res_id = %s, rf_fltr_id = %s, rf_value = %s",
                                       (key, f_id, filter_values[filter_key]))

        cursor.execute("SELECT r_result_id FROM result LIMIT %s, 100000", (limit))
        ids = cursor.fetchall()
        limit += 100000

def getFilterID(fname, conn):
    """
    Function: getFilterID
    Input:
        fname - the name of a filter
        conn - a MatrixTDBConn
        TODO: Do I need to add 'type' here to distinguish between u and s that may have same
                   name?
    Output: answer - the ID of this filter in MatrixTDB, 0 if name not found in DB.
    Functionality: looks in MatrixTDB for a filter of this name.
    Tables accessed: filter
    Tables modified: none
    """
    # run the query to get the row(s) of filter_ids with that name
    row = conn.selQuery("SELECT filter_id FROM filter " + \
                                            "WHERE filter_name = %s", (fname))
    try:
        answer = row[0][0]      # get filter_id from first row returned...
    except IndexError:          # ...if no rows returned...
        answer = None                  # ...set answer to None
        
    return answer               # return output
    
def insertFilter(fname, ftype, conn):
    """
    Function: insertFilter
    Input:
        fname - the name of a filter
        ftype - the type of filter, either u (universal) or s (specific)
        conn - a MatrixTDBConn
    Output: fID - the ID of this filter added to MatrixTDB
    Functionality: adds this filter to the filter table of MatrixTDB.
    Tables accessed: filter
    Tables modified: filter
    """
    # insert the filter
    conn.execute("INSERT INTO filter SET filter_name = %s, filter_type = %s", (fname, ftype))
    fID = conn.selQuery("SELECT LAST_INSERT_ID()")[0][0]        # get its ID
    return fID                                                                              # return the ID

def insertManyFilteredSpecResults(resSfltrTuples, conn):
    """
    Function: insertManyFilteredSpecResults
    Input:
        resSfltrTuples - a set of tuples that are values to be added to res_sfltr
        conn - a MatrixTDBConn
    Output: none
    Functionality: Inserts several specific filter results into res_sfltr at a time.
    Tables accessed: res_sfltr
    Tables modified: res_sfltr
    """
    valuesClause = 'VALUES '            # initalize VALUES clause

    for rfr in resSfltrTuples:                 # for each res_sfltr row to insert...
        resID = rfr[0]                            # get its result id
        fID = rfr[1]                                # get its filter id
        result = rfr[2]                           # get its result...only entering 0's presently

        # and add that grouping to the values clause
        valuesClause += '(' + str(resID) + ',' + str(fID) + ',' + str(result) + '),'

    valuesClause = valuesClause[:-1]    # take off last comma from values clause

    # create entire INSERT statment
    insertStmt = 'INSERT INTO res_sfltr (rsf_res_id, rsf_sfltr_id, rsf_value) ' + valuesClause
    conn.execute(insertStmt)                # insert those values into res_sfltr
    
    return

"""
here is some old code I used to put in a try block that encoded insertFilteredSpecResult.
It's purpose was to catch an error that resulted if you tried to insert the same result/filter/id
combo to one that was already in res_sfltr.

the code had been in run_specific_filters.main

I'm just putting it here to save it since I'm not using it now that I went to
insertManyFilteredSpecResults

                try:
                    insertFilteredSpecResult(res_id, fID, applyResult

                except MySQLdb.IntegrityError:      # if we get an integrity error...
                    # get the result from the existing row for this result/filter combo that is causing
                    # problems
                    existResult = conn.selQuery("SELECT rsf_value FROM res_sfltr " + \
                                                              "WHERE rsf_res_id = %s " + \
                                                              "AND rsf_sfltr_id = %s", (res_id, fID))[0][0]
                    if existResult == 0:        # if it was already a fail...
                        pass        # ...we're okay here because the same fail result is already in there.
                    else:           # but if this result/filter combo was something other than a fail...
                        # ...give the user an error message
                        # TODO: figure out what to do here if this is necessary
                        print >> sys.stderr, "You are trying to change the value of running filter " + \
                                                      fID + " on result " + res_id + " and I don't know what " + \
                                                     "to do with that.  Ignoring for now."

"""
    
 
def insertFilteredSpecResult(resID, fltrID, appliedResult, conn):
    """
    Function: insertFilteredSpecResult
    Input:
        resID - the ID of a result in the result table
        fltrID - the ID of a SPECIFIC filter in the filter table
        appliedResult - the "result" of running the filter on the [incr_tsdb()] result/item.
        conn - a MatrixTDBConn, a connection to the MatrixTDB database        
    Output: none
    Functionality: inserts the result of running a SPECIFIC filter on a result/item into res_sfltr.
    Tables accessed: res_sfltr
    Tables modified: res_sfltr    
    """
    # insert the result
    conn.execute("INSERT INTO res_sfltr "+ \
                             "(rsf_res_id, rsf_sfltr_id, rsf_value) " + \
                         "VALUES (%s, %s, %s)", (resID, fltrID, appliedResult))
    return

def insertFilteredResult(resID, fltrID, appliedResult, conn):
    """
    Name: insertFilteredResult
    Input:
        resID - the ID of a result in the result table
        fltrID - the ID of a UNIVERSAL filter in the filter table
        appliedResult - the "result" of running the filter on the [incr_tsdb()] result/item.
        conn - a MatrixTDBConn, a connection to the MatrixTDB database        
    Output: none
    Functionality: inserts the result of running a UNIVERSAL filter on a result/item into res_fltr.
    Tables accessed: res_fltr
    Tables modified: res_fltr
    """
    # insert the result
    conn.execute("INSERT INTO res_fltr "+ \
                             "(rf_res_id, rf_fltr_id, rf_value) " + \
                         "VALUES (%s, %s, %s)", (resID, fltrID, appliedResult))
    return

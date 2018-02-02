### $Id: choices.py,v 1.24 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# imports

import re
from gmcs.util.misc import safe_int, get_valid_lines
from gmcs.linglib import case, clausalcomps

######################################################################
# globals

######################################################################
# Errors

class ChoicesFileParseError(Exception):
    def __init__(self, msg=''):
        self.msg = msg
    def __str__(self):
        return repr(self.msg)

######################################################################
# ChoiceCategory is a parent-class for ChoiceDict and ChoiceList.
# Any meta-information about choices should be encoded in
# ChoiceCategory, and ChoiceDict and ChoiceList should most likely
# just be empty classes inheriting from ChoiceCategory and their
# namesake datatype.

class ChoiceCategory:
    def __init__(self, full_key=None):
        self.full_key = full_key
        # When safe_get is true, index operations (e.g. choices['key']) will
        # return the default value if the key (or index) doesn't exist
        self.safe_get = True

    def get(self, key, default=None):
        # turn off safe_get so we can catch exceptions
        self.safe_get = False
        keys = [safe_int(k) for k in split_variable_key(key)]
        try:
            x = self
            for k in keys:
                x = x[k]
        except KeyError:
            x = default if default is not None else ''
        except IndexError:
            x = default if default is not None else ChoiceDict()
        except TypeError:
            x = default if default is not None else ''
        # reset safe_get
        self.safe_get = True
        return x

    def full_keys(self):
        full_keys = []
        if issubclass(self.__class__, ChoiceDict):
            for key in self:
                if issubclass(self[key].__class__,ChoiceCategory):
                    full_keys += self[key].full_keys()
                else:
                    if self.full_key:
                        full_keys += [self.full_key+'_'+key]
                    else:
                        full_keys += [key]
        elif issubclass(self.__class__, ChoiceList):
            for item in self:
                full_keys += item.full_keys()
        return full_keys

class ChoiceDict(ChoiceCategory, dict):

    def __getitem__(self, key):
        cur, remaining = get_next_key(key)
        try:
            retval = dict.__getitem__(self, key)
            if remaining:
                retval = retval[remaining]
        except KeyError, e:
            if self.safe_get:
                retval = ''
            else:
                raise e
        return retval

    def __setitem__(self, key, value):
        cur_key, remaining_keys = get_next_key(key)
        # keys will be empty if we are setting the value
        if not remaining_keys:
            dict.__setitem__(self, cur_key, value)
        # otherwise we need to descend to the next list
        else:
            if cur_key not in self:
                new_key = cur_key if not self.full_key \
                    else '_'.join([self.full_key, cur_key])
                new_list = ChoiceList(full_key=new_key)
                dict.__setitem__(self, cur_key, new_list)
            dict.__getitem__(self, cur_key)[remaining_keys] = value

    def __delitem__(self, key):
        cur, remaining = get_next_key(key)
        if remaining:
            #del self[cur][cur+remaining] # TJT 2014-09-02: This isn't working for some reason
            del self[cur][remaining] # TJT 2014-09-02: This isn't working for some reason
        elif cur in self:
            dict.__delitem__(self, cur)

    def iter_num(self):
        if self.full_key is not None:
            result = re.search('[0-9]+$', self.full_key)
            if result is not None:
                return int(result.group(0))
        return None

    def split_value(self, key):
        return [x for x in self[key].split(', ') if x != '']

    def walk(self, intermediates=False):
        if intermediates and self.full_key != None:
            yield (self.full_key, self)
        for key in self.keys():
            if isinstance(self[key], ChoiceCategory):
                for result in self[key].walk(intermediates):
                    yield result
            else:
                fullkey = key
                if self.full_key:
                    fullkey = self.full_key + '_' + key
                yield (fullkey, self[key])

    def __str__(self):
        return '\n'.join(
            '='.join(['_'.join([self.full_key, key]) if self.full_key else key,
                      self[key]]) \
                if not isinstance(self[key], ChoiceList) \
                else str(self[key])
            for key in self)

    def __repr__(self):
        return '\n'.join(
            '='.join(['_'.join([self.full_key, key]) if self.full_key else key,
                      self[key]]) \
                if not isinstance(self[key], ChoiceList) \
                else str(self[key])
            for key in self)


class ChoiceList(ChoiceCategory, list):

    def __getitem__(self, key):
        index, remaining = get_next_key(key)
        # TODO: Fix this
        # TJT 2014-11-18: this errors if get_next_key returns a string...
        # not sure why a string is ever returned. Need to investigate.
        if not isinstance(index, int):
            raise KeyError('Something went wrong with the backend system. Please contact the developers at matrix-dev@u.washington.edu')
        try:
            # subtract 1 for 1-based indices
            retval = list.__getitem__(self, index - 1)
            if remaining:
                retval = retval[remaining]
        except IndexError, e:
            if self.safe_get:
                retval = ChoiceDict()
            else:
                raise e
        return retval

    def __setitem__(self, key, value):
        index, remaining_keys = get_next_key(key)
        # create the dicts, if needed, then descend into the one at index
        # we overrode the len function, but in this case we want the original
        for i in range(list.__len__(self), index):
            self.append(None)
        if not remaining_keys:
            list.__setitem__(self, index - 1, value)
        else:
            if self[index] == None:
                list.__setitem__(self, index - 1, ChoiceDict(full_key=self.full_key + \
                                                                      str(index)))
            list.__getitem__(self, index - 1)[remaining_keys] = value

    def __delitem__(self, key):
        cur, remaining = get_next_key(key)
        if remaining:
            del self[cur][remaining]
        # delete only if the user specified this list
        elif cur <= list.__len__(self):
            # but don't actually delete list items, since that breaks indexing
            self[cur] = None

    # custom iterator ignores empty items (e.g. when a
    # user deletes an item in the middle of a list)
    def __iter__(self):
        """
        Iterate over only the none-empty indices.
        """
        for item in list.__iter__(self):
            if item is not None:
                yield item

    def walk(self, intermediates=False):
        if intermediates:
            yield (self.full_key, self)
        for item in self:
            for result in item.walk(intermediates):
                yield result

    def __len__(self):
        """
        Return the length of the ChoiceList, which is the number of
        non-empty indices in the list.
        """
        # The custom iterator only returns non-empty items, so just use that.
        return sum(1 for x in self)

    def is_empty(self):
        return len(self) == 0

    def get_first(self):
        """
        Return the first non-None list item.
        """
        # The custom iterator will take care of finding non-None items.
        i = iter(self)
        try:
            return i.next()
        except StopIteration:
            return None

    def get_last(self):
        """
        Return the last non-None list item.
        """
        # reversed bypasses the custom iterator, so we have to check manually.
        for d in reversed(self):
            if d is not None:
                return d
        return None

    def next_iter_num(self):
        if len(self) == 0: return 1
        return (self.get_last().iter_num() or 0) + 1

    def __str__(self):
        return '\n'.join(str(item) for item in self)

    def __repr__(self):
        return '\n'.join(str(item) for item in self)


######################################################################
# Helper functions

def get_choice(choice, choices):
    """
    Return the value of a choice from choice lines or a choices file.
    The choice must be fully specified choice (not a sub-structure).
    Returns None if the choice does not result in a value.
    """
    choice_lines = choices
    if type(choices) is str:
        choice_lines = open(choices).readlines()
    elif type(choices) is file:
        choice_lines = choices.readlines()

    for line in [l.strip() for l in choice_lines if '=' in l]:
        key, val = line.split('=')
        if key == choice:
            return val
    return None

# use the following re if keys like abc_def should be split:
#var_delim_re = re.compile(r'(\d+)?(?:_|$)')
# use the following re if final digits should be split
var_delim_re = re.compile(r'(\d+)(?:_|$)')
# use the following re if we only split when a digit precedes _
#var_delim_re = re.compile(r'(\d+)(?:_)')

def split_variable_key(key):
    """
    Split a compound variable key into a list of its component parts.
    """
    if key == '': return []
    return [k for k in var_delim_re.split(key) if k]

next_key_cache = {}

def get_next_key(complex_key):
    """
    Split a key grouping it by non-numbers and numbers.
    """
    # given a blank key, return None
    if not complex_key:
        return None, None
    # if the key is just a number, just return the number
    if isinstance(complex_key, int):
        return complex_key, ''
    try:
        subkeys = next_key_cache[complex_key]
    except KeyError:
        subkeys = var_delim_re.split(complex_key)
        if subkeys[0] == '':
            subkeys.pop(0)
        if subkeys[-1] == '':
            subkeys.pop()
    next_key = subkeys[0]
    rest = complex_key.replace(next_key,'',1).lstrip('_')
    if len(subkeys) > 1:
        next_key_cache[rest] = subkeys[1:]
    return safe_int(next_key), rest

######################################################################
# ChoicesFile is a class that wraps the choices file, a list of
# variables and values, and provides methods for loading, accessing,
# and saving them.

class ChoicesFile:

    # initialize by passing either a file name or file handle
    def __init__(self, choices_file=None):

        self.cached_values = {}
        self.choices = ChoiceDict()

        if choices_file is not None:
            try:
                f = choices_file
                if type(choices_file) == str:
                    f = open(choices_file, 'r')
                f.seek(0)
                lines = get_valid_lines(f.readlines())
                self.load_choices(lines)
                if type(choices_file) == str:
                    f.close()
            except IOError:
                pass # TODO: we should really be logging these

    def __str__(self):
        return str(self.choices)

    def __eq__(self, object):
        if not issubclass(object.__class__, ChoicesFile):
            return False
        else:
            if len(self.full_keys()) != len(object.full_keys()):
                print self.full_keys()
                print str(len(self.full_keys()))+"/"+str(len(object.full_keys()))
                return False
            else:
                for i in self.full_keys():
                    if object[i] != self[i]:
                        print object[i]
                        print self[i]
                        return False
        return True

    ############################################################################
    ### Choices file parsing functions

    def load_choices(self, choice_lines):
        """
        Load a ChoicesFile object from a list of strings (i.e. "choices").
        Old versions are "uprev"ed in two ways: preparse_uprev and
        postparse_uprev, which convert the choices from one version to
        another. Because preparse must work on the choices file lines,
        and postparse on the object, we must do them separately.
        """
        # attempt to get version first, since preparse_uprev() needs it
        self.version = int(get_choice('version', choice_lines) or 0)
        # some key-values cannot be parsed by the current system, so
        # we need to handle these first
        choice_lines = self.preparse_uprev(choice_lines)
        self.choices = self.parse_choices(choice_lines)
        self.postparse_uprev()

    def parse_choices(self, choice_lines):
        """
        Get the data structure for each choice in the choices file, then
        merge them all together into one data structure.
        """
        choices = ChoiceDict()
        for line in [l.strip() for l in choice_lines if l.strip() != '']:
            try:
                (key, value) = line.split('=',1)
                if key.strip() in ('section', 'version'):
                    continue
                choices[key.strip()] = value
            except ValueError:
                pass # TODO: log this!
            except AttributeError:
                pass # TODO: log this!
            except ChoicesFileParseError:
                pass # TODO: log this!
        return choices

    ############################################################################
    ### Choices access functions

    def get(self, key, default=None):
        return self.choices.get(key, default)

    def get_regex(self, pattern):
        pat = re.compile(pattern)
        return [(key, val) for (key, val) in self.walk(intermediates=True)
                if pat.match(key)]

    # A __getitem__ method so that ChoicesFile can be used with brackets,
    # e.g., ch['language'].
    def __getitem__(self, key):
        return self.get(key)

    def __setitem__(self, key, value):
        self.choices[key] = value
        self.__reset_full_keys(key)

    def delete(self, key, prune=False):
        del self[key]
        # full_key values will be corrupted if we pruned, so re-evaluate
        if prune:
            for k in self:
                self.__renumber_full_keys(k)
                self.__reset_full_keys(k)

    def __delitem__(self, key):
        del self.choices[key]

    def __contains__(self, key):
        if self.get(key):
            return True
        return False

    def __iter__(self):
        return self.choices.__iter__()

    def walk(self, intermediates=False):
        for result in self.choices.walk(intermediates):
            yield result

    def __len__(self):
        return len(self.choices)

    def __renumber_full_keys(self, key):
        """
        Starting at the given key, reset the list numbers in the full_key
        values of all choices contained by that key.
        """
        # make sure the current key exists (e.g. was not pruned)
        if key not in self:
            return
        for i, c in enumerate(self[key]):
            # TJT 2014-09-02: Enumerate starts at 0,
            # so add 1 to i
            i += 1
            c_type = type(c)
            if c_type is ChoiceDict:
                c.full_key = key + str(i)
            elif c_type is ChoiceList:
                c.full_key = key + str(c)
            else:
                continue
            self.__renumber_full_keys(c.full_key)

    def __reset_full_keys(self, key):
        """
        Starting at the given key, reset the full_key values of all
        choices contained by that key.
        """
        if key not in self or not isinstance(self[key], ChoiceCategory):
            return
        c = self[key]
        c.full_key = key
        if isinstance(c, ChoiceDict):
            for k in dict.keys(c):
                self.__reset_full_keys(key + '_' + k)
        elif isinstance(c, ChoiceList):
            for d in c:
                idx = split_variable_key(d.full_key)[-1]
                self.__reset_full_keys(key + str(idx))

    def keys(self):
        return self.choices.keys()

    def full_keys(self):
        return self.choices.full_keys()

    ############################################################################
    ### Up-revisioning handler

    def preparse_uprev(self, choice_lines):
        """
        Convert choices file lines before they are parsed. A choice can be
        removed by setting the key to None in the conversion method. This
        should only be done to ensure old choices files can be loaded (e.g.
        changing noun1 to noun1_value), and any actual conversion should be
        done in postparse upreving.
        """
        new_lines = []
        for line in choice_lines:
            try:
                (key, value) = line.split('=',1)
                if key in ('section', 'version'):
                    continue
                # currently the only problem is lines ending with numerals.
                # add a generic key ("value") after these to make them loadable.
                if key[-1].isdigit():
                    key += '_value'
                # add back to the lines
                if key is not None:
                    new_lines += ['='.join([key, value])]
            except ValueError:
                pass # TODO: log this!
            except ChoicesFileParseError:
                raise ChoicesFileParseError('Variable is multiply defined: %s' % key)

        return new_lines

    def postparse_uprev(self):
        if self.version < 1:
            self.convert_0_to_1()
        if self.version < 2:
            self.convert_1_to_2()
        if self.version < 3:
            self.convert_2_to_3()
        if self.version < 4:
            self.convert_3_to_4()
        if self.version < 5:
            self.convert_4_to_5()
        if self.version < 6:
            self.convert_5_to_6()
        if self.version < 7:
            self.convert_6_to_7()
        if self.version < 8:
            self.convert_7_to_8()
        if self.version < 9:
            self.convert_8_to_9()
        if self.version < 10:
            self.convert_9_to_10()
        if self.version < 11:
            self.convert_10_to_11()
        if self.version < 12:
            self.convert_11_to_12()
        if self.version < 13:
            self.convert_12_to_13()
        if self.version < 14:
            self.convert_13_to_14()
        if self.version < 15:
            self.convert_14_to_15()
        if self.version < 16:
            self.convert_15_to_16()
        if self.version < 17:
            self.convert_16_to_17()
        if self.version < 18:
            self.convert_17_to_18()
        if self.version < 19:
            self.convert_18_to_19()
        if self.version < 20:
            self.convert_19_to_20()
        if self.version < 21:
            self.convert_20_to_21()
        if self.version < 22:
            self.convert_21_to_22()
        if self.version < 23:
            self.convert_22_to_23()
        if self.version < 24:
            self.convert_23_to_24()
        if self.version < 25:
            self.convert_24_to_25()
        if self.version < 26:
            self.convert_25_to_26()
        if self.version < 27:
            self.convert_26_to_27()
        if self.version < 28:
            self.convert_27_to_28()
        if self.version < 29:
            self.convert_28_to_29()
        if self.version < 30:
            self.convert_29_to_30()
        if self.version < 31:
            self.convert_30_to_31()



        # As we get more versions, add more version-conversion methods, and:
        # if self.version < N:
        #   self.convert_N-1_to_N
        # Also update current_version method to reflect current N.

        # now reset the full keys in case something was changed
        for top_level_key in self:
            self.__reset_full_keys(top_level_key)

    # Return the keys for the choices dict
    def keys(self):
        return self.choices.keys()

    def clear_cached_values(self):
        self.cached_values = {}

    ######################################################################
    # Methods for accessing "derived" values -- that is, groups of values
    # that are implied by the list of choices, but not directly stored
    # in it.  For example, it is convenient to be able to get a list of
    # all features defined in the languages, even though they're not
    # all stored in a single place.

    def has_case(self, feat, case):
        """
        Return true if the feature has matching case or if case is empty.
        """
        return feat['name'] == 'case' and (feat['value'] == case or case == '' \
                                           or any(case == value for value in feat['value'].split(', ')))

    def has_noun_case(self, case = ''):
        """
        Returns True iff the target language has either morphologically or
        lexically marked case (restricting the calculation to the
        passed-in case if it's non-empty).
        """

        k = 'has_noun_case(' + case + ')'
        if self.cached_values.has_key(k):
            return self.cached_values[k]

        result = False

        # check lexical types
        for noun in self.get('noun'):
            for feat in noun.get('feat',[]):
                result = result or self.has_case(feat, case)

        # check morphemes
        for pcprefix in ('noun', 'verb', 'det', 'adj'):
            for pc in self.get(pcprefix + '-pc'):
                for lrt in pc.get('lrt',[]):
                    for feat in lrt.get('feat',[]):
                        result = result or self.has_case(feat, case)

        self.cached_values[k] = result

        return result


    def has_adp_only_infostr(self):
        """
        Returns True iff the target language has information structural adpositions
        without case-marking.
        If the check_opt argument is True, only return True
        if the adposition is optional.
        """
        if self.has_adp_case():
            return False

        for adp in self.get('adp'):
            #opt = adp.get('opt')
            for feat in adp.get('feat', []):
                if feat['name'] == 'information-structure meaning':
                    return True

        return False


    def has_adp_case(self, case = '', check_opt = False):
        """
        Returns True iff the target language has case-marking adpositions
        (restricting the calculation to the passed-in case if it's
        non-empty).  If the check_opt argument is True, only return True
        if the adposition is optional.
        """

        # TJT 2014-09-08: Rearranged this logic to check opt/check_opt minimally,
        # and return once a case-marking adposition is found
        for adp in self.get('adp'):
            opt = adp.get('opt')
            if opt or not check_opt:
                for feat in adp.get('feat', []):
                    if self.has_case(feat, case):
                        return True

        return False


    def has_optadp_case(self, case = ''):
        """
        Returns True iff the target language has optional case-marking
        adpositions (restricting the calculation to the passed-in case if
        it's non-empty).
        """

        return self.has_adp_case(case, True)


    def has_mixed_case(self, case = ''):
        """
        Returns True iff the target language has both case-marking
        adpositions and case on nouns (restricting the calculation to the
        passed-in case if it's non-empty).
        """

        return self.has_noun_case(case) and self.has_adp_case(case)


    # case_head()
    def case_head(self, case = ''):
        """
        Returns the appropriate head type for case-marked arguments in the
        target language (restricting the calculation to the passed-in case
        if it's non-empty).
        """

        has_noun = self.has_noun_case(case)
        has_adp = self.has_adp_case(case)
        has_optadp = self.has_optadp_case(case)

        if (has_noun and has_adp) or has_optadp:
            return '+np'
        elif self.has_adp_only_infostr():
            return '+np'
        elif has_adp:
            return 'adp'
        else:
            return 'noun'


    def has_dirinv(self):
        """
        Returns True iff the target language has a direct-inverse scale.
        """
        return 'scale' in self.choices


    def has_SCARGS(self):
        """
        Returns True iff the target language requires the SC-ARGS feature,
        which contains the arguments in the order they are ranked by the
        direct-inverse hierarchy.
        """
        result = False

        for verb in self.get('verb'):
            for feat in verb.get('feat', []):
                result = result or feat['head'] in ('higher', 'lower')

        for verb_pc in self.get('verb-pc'):
            for lrt in verb_pc.get('lrt',[]):
                for feat in lrt.get('feat',[]):
                    result = result or feat['head'] in ('higher', 'lower')

        return result


    # patterns()
    #   Create and return a list containing information about the
    #   case-marking patterns implied by the current case choices.
    #   This list consists of tuples:
    #       [ canonical pattern name,
    #         friendly pattern name,
    #         rule?,
    #         direct-inverse? ]
    #   A pattern name is:
    #       (in)?transitive \(subject case-object case)
    #   In a canonical name (which is used in the choices file), the
    #   case names are the same as those used in the choices variable
    #   names.  The friendly name uses the names supplied by the
    #   user.  The third element is either True if the case pattern
    #   is one that should be used in lexical rules or False if it
    #   should be used on lexical types (subtypes of verb-lex).  The
    #   fourth argument is true if the verb follows a direct-inverse
    #   marking pattern.
    def patterns(self):
        cm = self.get('case-marking')
        cases = case.case_names(self)

        patterns = []

        # Fill in the canonical names based on the case-marking.
        if cm == 'nom-acc':
            patterns += [ ['nom', '', False] ]
            patterns += [ ['nom-acc', '', False] ]
        elif cm == 'erg-abs':
            patterns += [ ['abs', '', False] ]
            patterns += [ ['erg-abs', '', False] ]
        elif cm == 'tripartite':
            patterns += [ ['s_case', '', False] ]
            patterns += [ ['a_case-o_case', '', False] ]
        elif cm == 'split-s':
            patterns += [ ['a_case', '', False] ]
            patterns += [ ['o_case', '', False] ]
            patterns += [ ['a_case-o_case', '', False] ]
        elif cm == 'fluid-s':
            patterns += [ ['a_case', '', False] ]
            patterns += [ ['o_case', '', False] ]
            patterns += [ ['a_case+o_case', '', False] ]
            patterns += [ ['a_case-o_case', '', False] ]
        elif cm == 'split-n':
            patterns += [ ['s_case', '', False] ]
            patterns += [ ['a_case-o_case', '', False] ]
        elif cm == 'split-v':
            patterns += [ ['nom', '', True] ]
            patterns += [ ['abs', '', True] ]
            patterns += [ ['nom-acc', '', True] ]
            patterns += [ ['erg-abs', '', True] ]
        elif cm == 'focus':
            patterns += [ ['focus', '', True] ]
            patterns += [ ['focus-o_case', '', True] ]
            patterns += [ ['a_case-focus', '', True] ]

        # Add intransitive and transitive, which are always available.
        patterns += [ ['intrans', '', False] ]
        patterns += [ ['trans', '', False] ]

        # Fill in the friendly names based on the canonical names
        w = None
        for i in range(0, len(patterns)):
            if patterns[i][0] in ['trans', 'intrans']:
                patterns[i][1] = patterns[i][0] + 'itive'
                if cm != 'none':
                    patterns[i][1] += ' (case unspecified)'
            else:
                w = patterns[i][0].split('-')
                for j in range(0, len(w)):
                    for c in cases:
                        if w[j] == c[0]:
                            w[j] = c[1]
                if len(w) == 1:
                    patterns[i][1] = 'intransitive (%s)' % (w[0])
                elif len(w) == 2:
                    patterns[i][1] = 'transitive (%s-%s)' % (w[0], w[1])

        # extend the patterns to include direct-inverse, as needed
        if self.has_dirinv():
            for i in range(0, len(patterns)):
                if patterns[i][0] == 'trans' or patterns[i][0].find('-') != -1:
                    patterns += [ [ patterns[i][0] + ',dirinv',
                                    patterns[i][1] + ', direct-inverse',
                                    patterns[i][2] ] ]

        # Extend the patterns to include clausal complement strategies
        for ccs in self['comps']:
            patterns += [ [ 'trans,%s'%(ccs.full_key), 'transitive-clausal-%s (case unspecified)' % (ccs.full_key), False] ]
            if w and w[0] and w[1]:
                if clausalcomps.is_nominalized_complement(ccs):
                    patterns += [ [ '%s-%s,%s'% (w[0],w[1],ccs.full_key), 'transitive-clausal-%s (%s-%s)' % (ccs.full_key,w[0],w[1]), False] ]
            if w and w[0] and not cm == 'focus':
                patterns += [ [ '%s,%s'%(w[0],ccs.full_key), 'transitive-clausal-%s (%s-unspecified)' % (ccs.full_key,w[0]), False] ]
        return patterns


    # numbers()
    #   Create and return a list containing information about the values
    #   of the number feature implied by the current choices.
    #   This list consists of tuples:
    #     [name, supertype;supertype;...]
    def numbers(self):
        numbers = []

        for n in self.get('number'):
            name = n['name']
            stype = ';'.join([s['name'] for s in n.get('supertype',[])]) or 'number'
            numbers += [[name, stype]]

        return numbers

    # persons()
    #   Create and return a list containing information about the values
    #   of the person feature implied by the current choices.
    #   This list consists of tuples:
    #     [name, supertype]
    def persons(self):
        persons = []

        person = self.get('person')
        if person == '1-2-3':
            persons += [['1st', 'person']]
            persons += [['2nd', 'person']]
            persons += [['3rd', 'person']]
        elif person == '1-2-3-4':
            persons += [['1st', 'person']]
            persons += [['2nd', 'person']]
            persons += [['3rd', 'person']]
            persons += [['4th', 'person']]
        elif person == '1-non-1':
            persons += [['1st', 'person']]
            persons += [['non-1st', 'person']]
        elif person == '2-non-2':
            persons += [['2nd', 'person']]
            persons += [['non-2nd', 'person']]
        elif person == '3-non-3':
            persons += [['3rd', 'person']]
            persons += [['non-3rd', 'person']]

        return persons


    # pernums()
    #   Create and return a list containing information about the values
    #   of the pernum feature implied by the current choices.  A pernum
    #   feature is implied when the user has specified that the
    #   first-person plural has sub-types.
    #   This list consists of tuples:
    #     [name, supertype;supertype;...]
    def pernums(self):
        pernums = []

        fp = self.get('first-person')
        if fp and fp != 'none':
            num_leaves = []
            num_supers = []
            for n in self.numbers():
                if not n[0] in num_leaves:
                    num_leaves += [n[0]]
                for st in n[1].split(';'):
                    if st not in num_supers:
                        num_supers += [st]
                st = n[1]
                if st == 'number':
                    st = 'pernum'
                pernums += [[n[0], st]]
            for st in num_supers:
                if st in num_leaves:
                    num_leaves.remove(st)

            per_leaves = []
            for p in self.persons():
                if p[0] not in per_leaves:
                    per_leaves += [p[0]]
                st = p[1]
                if st == 'person':
                    st = 'pernum'
                pernums += [[p[0], st]]

            for n in num_leaves:
                for p in per_leaves:
                    pn = p[0] + n
                    pernums += [[pn, p + ';' + n]]
                    if p == '1st':
                        if fp == 'incl-excl':
                            for num in self.get('incl-excl-number').split(', '):
                                if num == n:
                                    pernums += [[pn + '_incl', pn]]
                                    pernums += [[pn + '_excl', pn]]
                        elif fp == 'other':
                            for p_st in self.get('person-subtype'):
                                name = p_st['name']
                                for num in p_st['number'].split(', '):
                                    if num == n:
                                        pernums += [[pn + '_' + name, pn]]

        return pernums


    # genders()
    #   Create and return a list containing information about the
    #   genders implied by the current choices.
    #   This list consists of tuples:
    #     [name, supertype;supertype;...]
    def genders(self):
        genders = []

        for g in self.get('gender'):
            name = g['name']
            stype = ';'.join([s['name'] for s in g.get('supertype',[])]) or 'gender'
            genders += [[name, stype]]

        return genders

    # forms()
    #   Create and return a list containing the values of the FORM
    #   feature that constrains the form of verbs as
    #   defined in the current choices.
    #   This list consists of tuples:
    #     [name, supertype]
    def forms(self):
        if 'form-fin-nf' in self and self['form-fin-nf'] == 'on':
            forms = [['form','form'],['finite','form'],['nonfinite','form']]
            for f in self.get('form-subtype'):
                name = f['name']
                stype = f.get('supertype') if f.get('supertype') else 'form'
                forms += [[name, stype]]
            return forms
        return []

    # tenses()
    #   Create and return a list containing information about the values
    #   of the TENSE feature implied by the current choices.
    #   This list consists of tuples:
    #     [tense name]
    def tenses(self):
        tenses = []

        tdefn = self.get('tense-definition')

        if tdefn == 'choose':
            for ten in ('past', 'present', 'future', 'nonpast', 'nonfuture'):
                if ten in self.choices:
                    tenses += [[ten]]
                    for t_st in self.get(ten + '-subtype'):
                        tenses += [ [t_st['name']] ]
        elif tdefn == 'build':
            for ten in self.get('tense'):
                tenses += [ [ten['name']] ]

        return tenses

    # aspects()
    #   Create and return a list containing information about the values
    #   of the viewpoint ASPECT feature implied by the current choices.
    #   This list consists of tuples:
    #     [aspect name]
    def aspects(self):
        aspects = []

        for asp in self.get('aspect'):
            aspects += [[asp['name']]]

        if len(aspects) == 0 and ('perimper' in self.choices):
            for asp in ('perfective', 'imperfective'):
                aspects += [[asp]]

        return aspects

    # situations()
    #   Create and return a list containing information about the values
    #   of the SITUATION aspect feature implied by the current choices.
    #   This list consists of tuples:
    #     [situation name]
    def situations(self):
        return [[situation['name']] for situation in self.get('situation')]

    # moods()
    #   Create and return a list containing information about the values
    #   of the MOOD feature implied by the current choices.
    #   This list consists of tuples:
    #      [mood name]
    def moods(self):
        moods = []

        for md in self.get('mood'):
            moods += [[md['name']]]

        if len(moods) == 0 and ('subjind' in self.choices):
            for md in ('subjunctive', 'indicative'):
                moods += [[md]]

        return moods

    def evidentials(self):
        evidentials = []

        evidential_definition = self.get('evidential-definition')

        if evidential_definition == 'choose':
            for evid in ('firsthand', 'nonfirsthand', 'visual', 'nonvisual', 'inferential', 'reported', 'quotative', 'everythingelse'):
                if evid in self.choices:
                    evidentials += [[evid]]
        elif evidential_definition == 'build':
            for evid in self.get('evidential'):
                evidentials += [ [evid['name']] ]

        return evidentials

    def types(self):
        """
        Create and return a list containing type names. FIX - these are
        based on the choices file. Need to include required types and
        inferred types as well. This list consists of tuples: [(type, name)]
        """
        return [(self.choices[t]['name'], t)
                for t in ('noun', 'verb', 'aux', 'det')
                if t in self.choices and 'name' in self.choices[t]]

    def __get_features(self, feat_list, i1, i2, label, tdl, cat, customized):
        """
        If there are values available for the given feature, construct a
        list of the feature label, values, tdl code and category for that feature.
        """
        values = ';'.join([x[i1] + '|' + x[i2] for x in feat_list])
        if values:
            return [ [label, values, tdl, cat, customized] ]
        return []

    def index_features(self):
        """
        Return the list of features that are marked on INDEX.
        """
        return ['person','number','gender'] \
               + [f['name'] for f in self['feature'] if f['type'] == 'index']

    # features()
    #   Create and return a list containing information about the
    #   features in the language described by the current choices.  This
    #   list consists of tuples with four strings:
    #       [feature name, list of values, feature geometry, category]
    #   Note that the feature geometry is empty if the feature requires
    #   more complex treatment that just FEAT=VAL (e.g. negation).
    #   The list of values is separated by semicolons, and each item in the
    #   list of values is a pair of the form 'name|friendly name'.
    #   The category string can have the values 'noun' or 'verb' or 'both' depending
    #   whether the features are appropriate for "nouny" or "verby" things.

    #   SSH (2012-06-20)
    #   A flag feature 'customized' is added, which indicates whether the feature
    #   is created in the customization system by users. A feature is specified as
    #   either 'customized=y' or 'customized=n'.

    def features(self):
        features = []

        # Case
        features += self.__get_features(case.case_names(self), 0, 1, 'case',
                                        'LOCAL.CAT.HEAD.CASE', 'noun', 'y')
        # Number, Person, and Pernum
        pernums = self.pernums()
        if pernums:
            features += self.__get_features(pernums, 0, 0, 'pernum',
                                            'LOCAL.CONT.HOOK.INDEX.PNG.PERNUM', 'noun', 'y')
        else:
            features += self.__get_features(self.numbers(), 0, 0, 'number',
                                            'LOCAL.CONT.HOOK.INDEX.PNG.NUM', 'noun', 'y')
            features += self.__get_features(self.persons(), 0, 0, 'person',
                                            'LOCAL.CONT.HOOK.INDEX.PNG.PER', 'noun', 'y')

        # Gender
        features += self.__get_features(self.genders(), 0, 0, 'gender',
                                        'LOCAL.CONT.HOOK.INDEX.PNG.GEND', 'noun', 'y')

        # Case patterns
        features += self.__get_features(self.patterns(), 0, 1,
                                        'argument structure', '', 'verb', 'n')

        # Form
        features += self.__get_features(self.forms(), 0, 0, 'form',
                                        'LOCAL.CAT.HEAD.FORM', 'verb', 'y')

        # Tense
        features += self.__get_features(self.tenses(), 0, 0, 'tense',
                                        'LOCAL.CONT.HOOK.INDEX.E.TENSE', 'verb', 'y')

        # Viewpoint Aspect
        features += self.__get_features(self.aspects(), 0, 0, 'aspect',
                                        'LOCAL.CONT.HOOK.INDEX.E.ASPECT', 'verb', 'y')

        #Situation Aspect
        features += self.__get_features(self.situations(), 0, 0, 'situation',
                                        'LOCAL.CONT.HOOK.INDEX.E.SITUATION', 'verb', 'y')
        #Mood
        features += self.__get_features(self.moods(), 0, 0, 'mood',
                                        'LOCAL.CONT.HOOK.INDEX.E.MOOD', 'verb', 'y')
        # Evidentials
        features += self.__get_features(self.evidentials(), 0, 0, 'evidential',
                                        '', 'verb', 'y')
        # Direction
        if self.has_dirinv():
            features += [ ['direction', 'dir|direct;inv|inverse', '', 'verb', 'y'] ]

        # Negation
        if  'infl-neg' in self.choices or 'neg-aux' in self.choices:
            features += [ ['negation', 'plus|plus;minus|minus', '', 'verb', 'y'] ]
        # if 'neg1b-neg2b' in self.choices:
        #  features += [ ['neg2', 'plus|plus', '', 'verb' ] ]

        # Possessives EKN 2017-01-13
        for strat in self.get('poss-strat'):
            if strat.get('possessor-type')=='affix' or strat.get('possessum-type')=='affix':
                strat_name=strat.full_key
                features += [ [ strat_name, 'possessor|possessor;possessum|possessum;nonpossessive|nonpossessive', '', 'noun', 'y'] ]
        for pron in self.get('poss-pron'):
            if pron.get('type')=='affix':
                pron_name=pron.full_key
                features += [ [ pron_name,'plus|plus;minus|minus', '', 'noun', 'y' ] ]

        # Questions
        if 'q-infl' in self.choices:
            features += [ ['question', 'plus|plus', '', 'verb', 'y'] ]

        # Information Structure
        infostr_values = 'focus|focus;topic|topic;contrast|contrast;semantic-focus|non-contrastive-focus;contrast-focus|contrastive-focus;aboutness-topic|non-contrastive-topic;contrast-topic|contrastive-topic;focus-or-topic|focus-or-topic;contrast-or-focus|contrast-or-focus;contrast-or-topic|contrast-or-topic;non-topic|non-topic;non-focus|non-focus;bg|background'
        #mkg_values = 'fc|focus;tp|topic;fc-only|focus-only;tp-only|topic-only;fc-+-tp|focus-and-topic;non-tp|non-topic;non-fc|non-focus;unmkg|unmarking'
        #features += [ ['information-structure marking', mkg_values, 'LOCAL.CAT.MKG', 'both', 'n'] ]
        features += [ ['information-structure meaning', infostr_values, 'LOCAL.CONT.HOOK.ICONS-KEY', 'both', 'n'] ]

        # Nominalization
        if 'ns' in self.choices:
            nom_types = ''
            for ns in self.choices.get('ns'):
                if nom_types == '':
                    nom_types += (ns.get('name') + '|' + ns.get('name'))
                else:
                    nom_types += (';' + ns.get('name') + '|' + ns.get('name'))
            features += [ ['nominalization', nom_types, '', 'verb', 'y'] ]


        # Argument Optionality
        if 'subj-drop' in self.choices or 'obj-drop' in self.choices:
            features +=[['OPT', 'plus|plus;minus|minus', '', 'verb', 'y']]

        perm_notperm_string = 'permitted|permitted;not-permitted|not-permitted'
        # Overt Argument
        if self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and \
                        self.get('obj-mark-drop') == 'obj-mark-drop-req':
            features += [['overt-arg', perm_notperm_string, '', '', '']]
        elif self.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and \
                        self.get('obj-mark-drop') == 'obj-mark-drop-req':
            features += [['overt-arg', perm_notperm_string, '', '', '']]
        elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and \
                        self.get('subj-mark-drop') == 'subj-mark-drop-req':
            features += [['overt-arg', perm_notperm_string, '', '', '']]
        elif self.get('obj-mark-no-drop') == 'obj-mark-no-drop-not' and \
                        self.get('obj-mark-drop') == 'obj-mark-drop-opt' :
            features += [['overt-arg', perm_notperm_string, '', '', '']]
        elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-not' and \
                        self.get('subj-mark-drop') == 'subj-mark-drop-opt' :
            features += [['overt-arg', perm_notperm_string, '', '', '']]
        elif self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and \
                        self.get('subj-mark-drop') == 'subj-mark-drop-req':
            features += [['overt-arg', perm_notperm_string, '', '', '']]

        # Dropped Argument
        #if self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt' and \
        #   self.get('obj-mark-drop') == 'obj-mark-drop-req':
        #  features += [['dropped-arg', perm_notperm_string, '']]
        #if self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt' and \
        #     self.get('subj-mark-drop') == 'subj-mark-drop-req':
        #  features += [['dropped-arg', perm_notperm_string, '']]
        if self.get('obj-mark-drop') == 'obj-mark-drop-not' and \
                        self.get('obj-mark-no-drop') == 'obj-mark-no-drop-req':
            features += [['dropped-arg', perm_notperm_string,'', '', '']]
        elif self.get('obj-mark-drop') == 'obj-mark-drop-not' and \
                        self.get('obj-mark-no-drop') == 'obj-mark-no-drop-opt':
            features += [['dropped-arg', perm_notperm_string,'', '', '']]
        elif self.get('obj-mark-drop') == 'obj-mark-drop-opt' and \
                        self.get('obj-mark-no-drop') == 'obj-mark-no-drop-req':
            features += [['dropped-arg', perm_notperm_string, '', '', '']]
        elif self.get('subj-mark-drop') == 'subj-mark-drop-not' and \
                        self.get('subj-mark-no-drop') == 'subj-mark-no-drop-req':
            features += [['dropped-arg', perm_notperm_string,'', '', '']]
        elif self.get('subj-mark-drop') == 'subj-mark-drop-not' and \
                        self.get('subj-mark-no-drop') == 'subj-mark-no-drop-opt':
            features += [['dropped-arg', perm_notperm_string,'', '', '']]
        elif self.get('subj-mark-drop') == 'subj-mark-drop-opt' and \
                        self.get('subj-mark-no-drop') == 'subj-mark-no-drop-req':
            features += [['dropped-arg', perm_notperm_string,'', '', '']]

            #elif self.get('subj-mark-drop') == 'subj-mark-drop-opt') and self.get('subj-mark-no-drop') == 'subj-mark-no-drop-req': features += [['dropped-arg', perm_notperm_string, '']]

        for feature in self.get('feature'):
            feat_name = feature['name']
            feat_type = feature['type']
            feat_cat = feature['cat']
            values = ''

            if feature['new'] == 'yes':
                values = ';'.join([val['name'] + '|' + val['name']
                                   for val in feature.get('value', [])])
            else:
                if feature['existing'] == 'bool':
                    values = '+|+;-|-'
                elif feature['existing'] == 'luk':
                    values = 'na-or-+|na-or-+;na-or--|na-or--;+-or--|+-or--;na|na;+|+;-|-'

            geom = ''
            if feat_type == 'head':
                geom = 'LOCAL.CAT.HEAD.' + feat_name.upper()
            else:
                geom = 'LOCAL.CONT.HOOK.INDEX.PNG.' + feat_name.upper()

            if len(values) > 0:
                features += [ [feat_name, values, geom, feat_cat, 'y'] ]

        return features


    ######################################################################
    # Conversion methods: each of these functions assumes the choices
    # file has already been loaded, then converts an older version into
    # a newer one, updating both old key names and old value names.
    # These methods can be called in a chain: to update from version 2
    # to 5, call convert_2_to_3, convert_3_to_4, and convert_4_to_5, in
    # that order.
    #
    # The mehods should consist of a sequence of calls to
    # convert_value(), followed by a sequence of calls to convert_key().
    # That way the calls always contain an old name and a new name.
    def current_version(self):
        return 31

    def convert_value(self, key, old, new, partial=False):
        if key in self:
            if not partial and self[key] == old:
                self[key] = new
            elif partial:
                self[key] = self[key].replace(old, new)

    def convert_key(self, old, new, key_prefix=''):
        if key_prefix:
            old = '_'.join([key_prefix, old])
            new = '_'.join([key_prefix, new])
        if old in self:
            self[new] = self[old]
            self.delete(old)

    # For example, combine nf-subform and fin-subform
    # into one key, 'form-fin-nf', and put the values
    def combine_keys(self, new, old1, old2):
        if old1 in self and old2 in self:
            tmp = self[old1]
            self[old1].extend(self[old2])
            self[new] = self[old1]
            self.delete(old1)
            self.delete(old2)

    def convert_0_to_1(self):
        self.convert_key('wordorder', 'word-order')

        self.convert_value('hasDets', 't', 'yes')
        self.convert_value('hasDets', 'nil', 'no')
        self.convert_key('hasDets', 'has-dets')

        self.convert_value('NounDetOrder', 'HeadSpec', 'noun-det')
        self.convert_value('NounDetOrder', 'SpecHead', 'det-noun')
        self.convert_key('NounDetOrder', 'noun-det-order')

        self.convert_key('infl_neg', 'infl-neg')

        self.convert_key('neg-aff-form', 'neg-aff-orth')

        self.convert_key('adv_neg', 'adv-neg')

        self.convert_value('negmod', 'S', 's')
        self.convert_value('negmod', 'VP', 'vp')
        self.convert_value('negmod', 'V', 'v')
        self.convert_key('negmod', 'neg-mod')

        self.convert_value('negprepostmod', 'pre', 'before')
        self.convert_value('negprepostmod', 'post', 'after')
        self.convert_key('negprepostmod', 'neg-order')

        self.convert_value('multineg', 'bothopt', 'both-opt')
        self.convert_value('multineg', 'bothobl', 'both-obl')
        self.convert_value('multineg', 'advobl', 'adv-obl')
        self.convert_value('multineg', 'inflobl', 'infl-obl')
        self.convert_key('multineg', 'multi-neg')

        self.convert_key('cs1n', 'cs1_n')

        self.convert_key('cs1np', 'cs1_np')

        self.convert_key('cs1vp', 'cs1_vp')

        self.convert_key('cs1s', 'cs1_s')

        self.convert_key('cs1pat', 'cs1_pat')

        self.convert_key('cs1mark', 'cs1_mark')

        self.convert_key('cs1orth', 'cs1_orth')

        self.convert_key('cs1order', 'cs1_order')

        self.convert_key('cs2n', 'cs2_n')

        self.convert_key('cs2np', 'cs2_np')

        self.convert_key('cs2vp', 'cs2_vp')

        self.convert_key('cs2s', 'cs2_s')

        self.convert_key('cs2pat', 'cs2_pat')

        self.convert_key('cs2mark', 'cs2_mark')

        self.convert_key('cs2orth', 'cs2_orth')

        self.convert_key('cs2order', 'cs2_order')

        self.convert_value('ques', 'qpart', 'q-part')

        self.convert_key('qinvverb', 'q-inv-verb')

        self.convert_value('qpartposthead', '-', 'before')
        self.convert_value('qpartposthead', '+', 'after')
        self.convert_key('qpartposthead', 'q-part-order')

        self.convert_key('qpartform', 'q-part-orth')

        self.convert_key('noun1pred', 'noun1_pred')

        self.convert_value('noun1spr', 'nil', 'imp')
        self.convert_key('noun1spr', 'noun1_det')

        self.convert_key('noun2pred', 'noun2_pred')

        self.convert_value('noun2spr', 'nil', 'imp')
        self.convert_key('noun2spr', 'noun2_det')

        self.convert_key('ivpred', 'iverb-pred')

        self.convert_value('iverbSubj', 'pp', 'adp')
        self.convert_key('iverbSubj', 'iverb-subj')

        self.convert_key('iverb-nonfinite', 'iverb-non-finite')

        self.convert_key('tvpred', 'tverb-pred')

        self.convert_value('tverbSubj', 'pp', 'adp')
        self.convert_key('tverbSubj', 'tverb-subj')

        self.convert_value('tverbObj', 'pp', 'adp')

        self.convert_key('tverbObj', 'tverb-obj')

        self.convert_key('tverb-nonfinite', 'tverb-non-finite')

        self.convert_key('auxverb', 'aux-verb')

        self.convert_key('auxsem', 'aux-sem')

        self.convert_key('auxpred', 'aux-pred')

        self.convert_value('auxcomp', 'S', 's')
        self.convert_value('auxcomp', 'VP', 'vp')
        self.convert_value('auxcomp', 'V', 'v')
        self.convert_key('auxcomp', 'aux-comp')

        self.convert_value('auxorder', 'left', 'before')
        self.convert_value('auxorder', 'right', 'after')
        self.convert_key('auxorder', 'aux-order')

        self.convert_value('auxsubj', 'noun', 'np')
        self.convert_key('auxsubj', 'aux-subj')

        self.convert_key('det1pred', 'det1_pred')

        self.convert_key('det2pred', 'det2_pred')

        self.convert_key('subjAdpForm', 'subj-adp-orth')

        self.convert_value('subjAdp', 'pre', 'before')
        self.convert_value('subjAdp', 'post', 'after')
        self.convert_key('subjAdp', 'subj-adp-order')

        self.convert_key('objAdpForm', 'obj-adp-orth')

        self.convert_value('objAdp', 'pre', 'before')
        self.convert_value('objAdp', 'post', 'after')
        self.convert_key('objAdp', 'obj-adp-order')

        self.convert_key('negadvform', 'neg-adv-orth')


    def convert_1_to_2(self):
        # The old 'ques' radio button has been converted into a series of
        # checkboxes, of which 'inv' has been renamed 'q-inv' and 'int'
        # has been removed.
        if 'ques' in self:
            ques = self.get('ques')
            self.delete('ques')
            if ques == 'inv':
                ques = 'q-inv'
            if ques != 'int':
                self[ques] = 'on'

    def convert_2_to_3(self):
        # Added a fuller implementation of case marking on core arguments,
        # so convert the old case-marking adposition stuff to the new
        # choices
        S = self.get('iverb-subj')
        A = self.get('tverb-subj')
        O = self.get('tverb-obj')

        Sorth = Aorth = Oorth = ''
        Sorder = Aorder = Oorder = ''
        if S == 'adp':
            Sorth = self.get('subj-adp-orth')
            Sorder = self.get('subj-adp-order')
        if A == 'adp':
            Aorth = self.get('subj-adp-orth')
            Aorder = self.get('subj-adp-order')
        if O == 'adp':
            Oorth = self.get('obj-adp-orth')
            Aorder = self.get('obj-adp-order')

        if Sorth == '' and Aorth == '' and Oorth == '':
            if len(self):  # don't add this if the choices file is empty
                self['case-marking'] = 'none'
        elif Sorth == Aorth and Sorth != Oorth:
            self['case-marking'] = 'nom-acc'
            self['nom-case-label'] = 'nominative'
            self['acc-case-label'] = 'accusative'
            if Aorth:
                self['nom-case-pat'] = 'np'
                self['nom-case-order'] = Aorder
            else:
                self['nom-case-pat'] = 'none'
            if Oorth:
                self['acc-case-pat'] = 'np'
                self['acc-case-order'] = Oorder
            else:
                self['acc-case-pat'] = 'none'
        elif Sorth != Aorth and Sorth == Oorth:
            self['case-marking'] = 'erg-asb'
            self['erg-case-label'] = 'ergative'
            self['abs-case-label'] = 'absolutive'
            if Aorth:
                self['erg-case-pat'] = 'np'
                self['erg-case-order'] = Aorder
            else:
                self['erg-case-pat'] = 'none'
            if Oorth:
                self['abs-case-pat'] = 'np'
                self['abs-case-order'] = Oorder
            else:
                self['abs-case-pat'] = 'none'
        else:
            self['case-marking'] = 'tripartite'
            self['s-case-label'] = 'subjective'
            self['a-case-label'] = 'agentive'
            self['o-case-label'] = 'objective'
            if Sorth:
                self['s-case-pat'] = 'np'
                self['s-case-order'] = Sorder
            else:
                self['s-case-pat'] = 'none'
            if Aorth:
                self['a-case-pat'] = 'np'
                self['a-case-order'] = Aorder
            else:
                self['a-case-pat'] = 'none'
            if Oorth:
                self['o-case-pat'] = 'np'
                self['o-case-order'] = Oorder
            else:
                self['o-case-pat'] = 'none'

        self.delete('iverb-subj')
        self.delete('tverb-subj')
        self.delete('tverb-obj')
        self.delete('subj-adp-orth')
        self.delete('subj-adp-order')
        self.delete('obj-adp-orth')
        self.delete('obj-adp-order')

    def convert_3_to_4(self):
        # Added a fuller implementation of case marking on core arguments,
        # so convert the old case-marking adposition stuff to the new
        # choices. Also, convert nouns, verbs, dets to the iterator keys.
        self.convert_key('iverb', 'verb1_orth')
        self.convert_key('iverb-pred', 'verb1_pred')
        self.convert_key('iverb-non-finite', 'verb1_non-finite')
        if self.get('verb1_orth'):
            self['verb1_valence'] = 'intrans'
        self.convert_key('tverb', 'verb2_orth')
        self.convert_key('tverb-pred', 'verb2_pred')
        self.convert_key('tverb-non-finite', 'verb2_non-finite')
        if self.get('verb2_orth'):
            self['verb2_valence'] = 'trans'
        self.convert_key('det1pred', 'det1_pred')
        self.convert_key('det2pred', 'det2_pred')
        # the following were converted in preparse_uprev
        for key in ('noun1', 'noun2', 'det1', 'det2'):
            self.convert_key(key + '_value', key + '_orth')

    def convert_4_to_5(self):
        # An even fuller implementation of case marking, with some of the
        # work shifted off on Kelly's morphology code.
        # Get a list of choices-variable prefixes, one for each case
        prefixes = []
        cm = self.get('case-marking')
        if cm == 'nom-acc':
            prefixes.append('nom')
            prefixes.append('acc')
        elif cm == 'erg-abs':
            prefixes.append('erg')
            prefixes.append('abs')
        elif cm == 'tripartite':
            prefixes.append('s')
            prefixes.append('a')
            prefixes.append('o')

        cur_ns = 1       # noun slot
        cur_nm = 1       # noun morph
        cur_ni = 'noun'  # noun input
        last_ns_order = ''

        cur_ds = 1       # det slot
        cur_dm = 1       # det morph
        cur_ni = 'det'   # det input
        last_ds_order = ''

        cur_adp = 1

        for p in prefixes:
            label = self.get(p + '-case-label')
            pat = self.get(p + '-case-pat')
            order = self.get(p + '-case-order')
            orth = self.get(p + '-case-orth')

            # create noun slot and morph
            if pat in ('noun', 'noun-det'):
                if last_ns_order and last_ns_order != order:
                    cur_ni = 'noun-slot' + str(cur_ns)
                    cur_ns += 1
                    cur_nm = 1

                ns_pre = 'noun-slot' + str(cur_ns)
                nm_pre = ns_pre + '_morph' + str(cur_nm)

                self[ns_pre + '_input1_type'] = cur_ni
                self[ns_pre + '_name'] = 'case'
                self[ns_pre + '_order'] = order

                self[nm_pre + '_name'] = label
                self[nm_pre + '_orth'] = orth
                self[nm_pre + '_feat1_name'] = 'case'
                self[nm_pre + '_feat1_value'] = label
                cur_nm += 1

            # create det slot and morph
            if pat in ('det', 'noun-det'):
                if last_ds_order and last_ds_order != order:
                    cur_di = 'det-slot' + str(cur_ds)
                    cur_ds += 1
                    cur_dm = 1

                ds_pre = 'det-slot' + str(cur_ds)
                dm_pre = ds_pre + '_morph' + str(cur_dm)

                self[ds_pre + '_input1_type'] = cur_di
                self[ds_pre + '_name'] = 'case'
                self[ds_pre + '_order'] = order

                self[dm_pre + '_name'] = label
                self[dm_pre + '_orth'] = orth
                self[dm_pre + '_feat1_name'] = 'case'
                self[dm_pre + '_feat1_value'] = label
                cur_dm += 1

            # create adposition
            if pat == 'np':
                adp_pre = 'adp' + str(cur_adp)
                self[adp_pre + '_orth'] = orth
                self[adp_pre + '_order'] = order
                self[adp_pre + '_feat1_name'] = 'case'
                self[adp_pre + '_feat1_value'] = label

        self.convert_key('nom-case-label', 'nom-acc-nom-case-name')
        self.convert_key('acc-case-label', 'nom-acc-acc-case-name')

        self.convert_key('erg-case-label', 'erg-abs-erg-case-name')
        self.convert_key('abs-case-label', 'erg-abs-abs-case-name')

        self.convert_key('s-case-label', 'tripartite-s-case-name')
        self.convert_key('a-case-label', 'tripartite-a-case-name')
        self.convert_key('o-case-label', 'tripartite-o-case-name')

        for p in ['nom', 'acc', 'erg', 'abs', 's', 'a', 'o']:
            self.delete(p + '-case-pat')
            self.delete(p + '-case-order')
            self.delete(p + '-case-orth')

        for verb in self['verb']:
            v = verb.get('valence')
            if v == 'intrans':
                if cm == 'none':
                    pass
                elif cm == 'nom-acc':
                    verb['valence'] = 'nom'
                elif cm == 'erg-abs':
                    verb['valence'] = 'abs'
                elif cm == 'tripartite':
                    verb['valence'] = 's'
            elif v == 'trans':
                if cm == 'none':
                    pass
                elif cm == 'nom-acc':
                    verb['valence'] = 'nom-acc'
                elif cm == 'erg-abs':
                    verb['valence'] = 'erg-abs'
                elif cm == 'tripartite':
                    verb['valence'] = 'a-o'

    def convert_5_to_6(self):
        self.convert_key('aux-order', 'aux-comp-order')
        self.convert_key('aux-verb', 'aux1_orth')
        self.convert_value('aux-sem', 'pred', 'add-pred')
        self.convert_key('aux-sem', 'aux1_sem')
        self.convert_key('aux-comp', 'aux1_comp')
        self.convert_key('aux-pred', 'aux1_pred')
        self.convert_key('aux-subj', 'aux1_subj')
        if self.get('aux1_orth'):
            self['has-aux'] = 'yes'
        elif len(self):  # don't add this if the choices file is empty
            self['has-aux'] = 'no'

        for verb in self['verb']:
            self.delete('_'.join([verb.full_key, 'non-finite']))

    def convert_6_to_7(self):
        # Lexical types now have multiple stems
        for lextype in ['noun', 'verb', 'det']:
            for lt in self[lextype]:
                self.convert_key('orth', 'stem1_orth', key_prefix=lt.full_key)
                self.convert_key('pred', 'stem1_pred', key_prefix=lt.full_key)

        if not self.get('person') and len(self):
            self['person'] = 'none'

    def convert_7_to_8(self):
        # Other features no longer use the magic word 'root', they instead
        # use the name of the feature.
        for feature in self['feature']:
            fname = feature['name']
            for value in feature.get('value',[]):
                for st in value.get('supertype',[]):
                    self.convert_value(st.full_key + '_name', 'root', fname)

    def convert_8_to_9(self):
        # finite and nonfinite feature value name changes
        # in aux complement form values
        for aux in self['aux']:
            self.convert_value(aux.full_key + '_compform','fin','finite')
            self.convert_value(aux.full_key + '_compform', 'nf', 'nonfinite')
        # in slot feature values
        for lextype in ['aux','det','verb','noun']:
            for slot in self[lextype + '-slot']:
                for morph in slot.get('morph',[]):
                    for feat in morph.get('feat',[]):
                        self.convert_value(feat.full_key + '_value','fin','finite')
                        self.convert_value(feat.full_key + '_value','nf','nonfinite')

    def convert_9_to_10(self):
        """
        Previous versions defined (only) nonfinite compforms for each
        auxiliary iff the aux comp was constrained to be nonfinite.
        The current version creates a hierarchy of verb forms and then
        for each aux constrains the form of the complement. For each
        auxiliary, this conversion takes the value of the specified
        (nonfinite) compform and assigns it as the value of a member of
        the nonfinite hierarchy as well as the value of the auxiliary's
        compform.
        """
        self.convert_key('non-past', 'nonpast')
        self.convert_key('non-future', 'nonfuture')

        for aux in self['aux']:
            v = aux.get('nonfincompform', '')
            k = 'nf-subform' + str(aux.iter_num()) + '_name'
            self.convert_value(aux.full_key + '_compform', 'nonfinite', v)

            if 'nonfincompform' in aux:
                self[k] = v
                self.delete(aux.full_key + '_nonfincompform')

    def convert_10_to_11(self):
        """
        Previous versions allowed only one stem per auxiliary type.
        This conversion changes auxiliary orth and pred values to stem1 orth and pred.
        """
        for aux in self['aux']:
            self.convert_key('orth', 'stem1_orth', key_prefix=aux.full_key)
            self.convert_key('pred', 'stem1_pred', key_prefix=aux.full_key)

    def convert_11_to_12(self):
        """
        Previously the kind of comp (s, vp, v) was defined for each auxiliary type.
        Since our current word order implementation couldn't handle differences on this level anyway,
        this is no answered by one question for all auxiliaries.
        This conversion gives aux-comp the value of the first aux's comp, and deletes all type specific aux-comp values.
        """

        if self.get('has-aux') == 'yes':
            # just need the first (non-empty) item
            auxval = [aux.get('comp') for aux in self['aux']][0]
            self['aux-comp'] = auxval
            for aux in self['aux']:
                self.delete(aux.full_key + '_comp')

    def convert_12_to_13(self):
        """
        ERB: stupidly used "+" as a feature value.  Updating this
        to "plus".  Feature name was "negation".
        """
        for lextype in ['aux','det','verb','noun']:
            for lt in self[lextype + '-slot']:
                for morph in lt.get('morph',[]):
                    for feat in morph.get('feat',[]):
                        if feat['name'] == 'negation':
                            self.convert_value(feat.full_key + '_value','+','plus')

    def convert_13_to_14(self):
        """
        Revised the Person subpage.  Convert the old single radio button
        for defining subtypes under 1p-non-sg into the choices for defining
        your own subtypes.
        """
        numbers = [num['name'] for num in self['number']]
        # The following assumes the first number is Singular and that there
        # are more than one number (such as Plural, Dual, etc)
        number = ', '.join(numbers[1:])

        fp = self.get('first-person')
        subtypes = []
        if fp == 'incl-excl':
            self['incl-excl-number'] = number
        elif fp == 'min-incl':
            subtypes = ['min', 'incl']
        elif fp == 'aug-incl':
            subtypes = ['aug']
        elif fp == 'min-aug':
            subtypes = ['min', 'incl', 'aug']

        if len(subtypes) and len(number):
            self['first-person'] = 'other'
            for person_subtype in self['person-subtype']:
                for st in subtypes:
                    person_subtype['name'] = st
                    person_subtype['number'] = number

    def convert_14_to_15(self):
        """
        Revised slot co-occurrence constraints in the Lexicon subpage.
        Before, there were three iterators, req, disreq, and forces, each
        of which contained a single choice, type.  Now there's a single
        iterator, constraint, that contains the choices type (req, disreq,
        or forces) and other-slot.
        """

        for slotprefix in ('noun', 'verb', 'det', 'aux'):
            for slot in self.get(slotprefix + '-slot',[]):
                constraints = []

                for contype in ('forces', 'req', 'disreq'):
                    for ct in slot.get(contype, []):
                        constraints += [ [ contype, ct.get('type') ] ]
                    if contype in slot:
                        del slot[contype]

                for i, c in enumerate(constraints):
                    constraint_key = slot.full_key + '_constraint%d' % (i+1)
                    self[constraint_key + '_type'] = c[0]
                    self[constraint_key + '_other-slot'] = c[1]

    def convert_15_to_16(self):
        """
        Removes the feature MARK. Converts MARK feature to featureX_name (Other Features)
        where X places it at the end of the list of other features:
        --mark -> featureX_name=mark
        --featureX_type=head
        All MARK values are converted to featureX values:
        --markY_name=mY -> featureX_valueY_name=mY
        --featureX_valueY_supertype_name=mark
        """
        mvalues = [mark['name'] for mark in self['mark']]

        if len(mvalues) == 0:
            return
        next_feat_index = len(self.get('feature',[])) + 1
        feat_key = 'feature%d' % (next_feat_index)

        self[feat_key + '_name'] = 'mark'
        self[feat_key + '_type'] = 'head'

        for i, mv in enumerate(mvalues):
            val_key = '_value%d' % (i+1)
            self[feat_key + val_key + '_name'] = mv
            self[feat_key + val_key + '_supertype1_name'] = 'mark'

    def convert_16_to_17(self):
        """
        Relates to Auxiliary complement feature definition:
        --replaces 'compvalue' with 'value'
        --replaces compform=Y with compfeatureX_name=form, compfeature_value=Y
        """
        for aux in self['aux']:
            complementform = aux.get('compform')
            for cf in aux.get('compfeature',[]):
                self.convert_key('compvalue', 'value', key_prefix=cf.full_key)
            index = str(len(aux.get('compfeature', [])) + 1)
            new_key = aux.full_key + '_compfeature' + index
            self[new_key + '_name'] = 'form'
            self[new_key + '_value'] = complementform
            #self.delete(aux.full_key + '_compform', prune=True)
            self.delete(aux.full_key + '_compform')

    def convert_17_to_18(self):
        """
        Retrofitted yesno questions library to integrate question affixes
        with morphotactic infrastructure.  'aux-main' possibility for
        q-infl-type said in the prose 'any finite verb', but I don't think
        we had actually implemented this.  This translation does not
        put [FORM fin] on the q-infl rule, since this rule will end up
        as a separate path from any other verbal inflection, again mimicking
        what was going on in the old system.
        """
        if self.get('q-infl') == 'on':
            n = self['verb-slot'].next_iter_num() if 'verb-slot' in self else 1
            pref = 'verb-slot' + str(n)
            if self.get('ques-aff') == 'suffix':
                self[pref + '_order'] = 'after'
            if self.get('ques-aff') == 'prefix':
                self[pref + '_order'] = 'before'
            if self.get('q-infl-type') == 'main':
                self[pref + '_input1_type'] = 'iverb'
                self[pref + '_input2_type'] = 'tverb'
            if self.get('q-infl-type') == 'aux':
                self[pref + '_input1_type'] = 'aux'
            if self.get('q-infl-type') == 'aux-main':
                self[pref + '_input1_type'] = 'verb'
            if 'ques-aff-orth' in self:
                self[pref + '_morph1_orth'] = self.get('ques-aff-orth')
            self[pref + '_name'] = 'q-infl'
            self[pref + '_morph1_feat1_name'] = 'question'
            self[pref + '_morph1_feat1_value'] = 'plus'
            self[pref + '_morph1_feat1_head'] = 'verb'
            self[pref + '_opt'] = 'on'

    def convert_18_to_19(self):
        """
        sentence1, sentence2, etc. were converted in preparse_uprev to be
        sentence1_value, etc. Change those to a more appropriate key.
        """
        for sent in self.get('sentence', []):
            self.convert_key(sent.full_key + '_value', sent.full_key + '_orth')

    def convert_19_to_20(self):
        """
        Removed question on verbal clusters from the word order library. They are
        now formed when free word order has v-comp and more than one auxiliary per
        clause, and always for VSO, OSV when having v-comp, other conditions remain
        as before. For other word orders the verbal cluster question is ignored.
        """
        if self.get('has-aux') == 'yes':
            wo = self.get('word-order')
            if self.get('aux-comp') == 'v':
                if wo == 'free':
                    self.convert_key('v-cluster','multiple-aux')
                elif wo == 'vso' or wo == 'osv':
                    if self.get('v-cluster') == 'no':
                        self.convert_value('aux-comp','v','vp')
        else:
            pass
        #if v-comp if free word order if v-cluster more than one aux, if no cluster 1max
        # if svo,ovs do nothing, else v-comp is vp-comp

    def convert_20_to_21(self):
        """
        Inflectional rules no longer have three constraint types (req,
        forces, disreq), but two (require, forbid). Customization will
        later determine if 'requires' applies to a previous or following
        slot. Also added: 'require' can take a disjunctive set (require A
        or B), but since this is marked on ..._other-slot with a
        comma-separated list, we don't need to do anything here. And also,
        constraints can be marked on morphemes, but nothing needs to be
        done for that, either. Further, since constraints can now be
        marked on lexical types as well, slot optionality is less
        meaningful than obligatoriness across all possible input types.
        """
        for slotprefix in ('noun', 'verb', 'det', 'aux', 'adj'):
            for slot in self.get(slotprefix + '-slot'):
                for const in slot.get('constraint',[]):
                    self.convert_value(const.full_key + '_type', 'forces', 'require')
                    self.convert_value(const.full_key + '_type', 'req', 'require')
                    self.convert_value(const.full_key + '_type', 'disreq', 'forbid')
                if 'opt' in slot:
                    del slot['opt']
                else:
                    slot['obligatory'] = 'on'

    def convert_21_to_22(self):
        """
        Constraints are no longer generic and specifying a type, but are
        specific (e.g. require1, forbid1) and only specify the other slot.
        """
        for x in ('noun', 'verb', 'det', 'aux', 'adj'):
            for x_type in ('', '-slot'):
                for slot in self.get(x + x_type):
                    constraints = {'require': [], 'forbid': []}
                    if 'constraint' not in slot: continue
                    for const in slot.get('constraint',[]):
                        if const['type'] == 'require':
                            constraints['require'] += [const['other-slot']]
                        elif const['type'] == 'forbid':
                            constraints['forbid'] += [const['other-slot']]
                    del slot['constraint']
                    for i, req in enumerate(constraints['require']):
                        key = slot.full_key + '_require' + str(i + 1) + '_other-slot'
                        self[key] = req
                    for i, fbd in enumerate(constraints['forbid']):
                        key = slot.full_key + '_forbid' + str(i + 1) + '_other-slot'
                        self[key] = fbd

    def convert_22_to_23(self):
        """
        Lexical rules are no longer divided into Slots and Morphs, but
        Position Classes, Lexical Rule Types, and Lexical Rule Instances,
        and LRTs can inherit from other LRTs. Also, LRTs without LRIs
        should be given a blank one (since now it is possible for LRTs
        to exist that cannot themselves be realized).
        """
        def convert_constraint(lex, constraint):
            """
            Nested function to help with converting constraints.
            """
            for c in lex.get(constraint,[]):
                self.convert_value(c.full_key + '_other-slot',
                                   '-slot', '-pc', partial=True)
                self.convert_key('other-slot', 'others', key_prefix=c.full_key)

        from gmcs.linglib.lexbase import LEXICAL_CATEGORIES
        for lex_cat in LEXICAL_CATEGORIES:
            for lex_type in self[lex_cat]:
                convert_constraint(lex_type, 'require')
                convert_constraint(lex_type, 'forbid')
            for slot in self[lex_cat + '-slot']:
                # constraints
                convert_constraint(slot, 'require')
                convert_constraint(slot, 'forbid')
                # normalize order values
                self.convert_value(slot.full_key + '_order', 'before', 'prefix')
                self.convert_value(slot.full_key + '_order', 'after', 'suffix')
                # inputs
                all_inps = ', '.join([inp['type'] for inp in slot['input']])
                del self[slot.full_key + '_input']
                self[slot.full_key + '_inputs'] = all_inps.replace('-slot', '-pc')
                # morphs and orths
                for morph in slot['morph']:
                    if 'orth' in morph:
                        morph['lri1_inflecting'] = 'yes'
                        self.convert_key('orth', 'lri1_orth', key_prefix=morph.full_key)
                    else:
                        morph['lri1_inflecting'] = 'no'
                        morph['lri1_orth'] = ''
                self.convert_key(slot.full_key + '_morph', slot.full_key + '_lrt')
            # finally, change -slot keys to -pc
            self.convert_key(lex_cat + '-slot', lex_cat + '-pc')

    def convert_23_to_24(self):
        """
        This uprev only fixes test sentences marked ungrammatical with a * at
        the beginning of the string, since * can now be allowed as punctuation
        (if the user adds it as a parsable punctuation in the general page).
        """
        for sentence in self['sentence']:
            if sentence.get('orth','').startswith('*'):
                sentence['star'] = 'on'
                sentence['orth'] = sentence['orth'].lstrip('*')

    def convert_24_to_25(self):
        """
        This uprev converts the old choices about punctuation characters
        to the new format.  If the old file doesn't have a choice about punctuation
        then there's nothing to do, the new default behavior should be identical.
        If the old file has a punctuation-chars key, we just need to set
        punctuation-chars=keep-list, and put the chars into the punctuation-chars-list
        key.
        """
        if self.get('punctuation-chars'):
            self.convert_key('punctuation-chars', 'punctuation-chars-list')
            self['punctuation-chars'] = 'keep-list'

    def convert_25_to_26(self):
        """
        This uprev converts the old choices that do not work with mtr.tdl and do not
        contain feature#_cat and feature#_new in the Other Feature.
        Some choices files have vlaue names that should be used only in mtr.tdl, which include
        a, u, i, etc. The names should be changed. On the other hand, nouny vs. verby /
        existing vs. new are required on the Other Feature.

        This uprev also adds exp=1 to old choices files so that they are compatible
        with the new negation library.
        """
        if (self.get('infl-neg')) and (not self.get('neg-exp')):
            self['neg-exp']='1'

        mtr = [ 'e', 'i', 'h', 'p', 'u', 'x', 'E', 'I', 'H', 'P', 'U', 'X' ]
        for g in self.get('gender'):
            name = g['name']
            if name in mtr:
                self.convert_value(g.full_key + '_name', name, '_'+name)

        inproper_case_names = [ 'a', 'o', 's', 'A', 'O', 'S' ]

        cm = self.get('case-marking')
        for name in inproper_case_names:
            case_name = self.get(cm + '-' + name + '-case-name')
            if case_name == name:
                self.convert_value(cm + '-' + name + '-case-name', case_name, case_name + '_case')

        for lex_cat in ['aux','det','verb','noun', 'adp']:
            for lex_type in self[lex_cat]:
                for feat in lex_type['feat']:
                    name = feat['name']
                    value = feat['value']
                    if value in mtr:
                        feat['value'] = '_'+value

                    if name == 'case':
                        tmp = ''
                        cs = value.split(', ')
                        for i in range(0, len(cs)):
                            if cs[i] in inproper_case_names:
                                tmp += cs[i] + '_case'
                            else:
                                tmp += cs[i]
                            if i < (len(cs) - 1):
                                tmp += ', '
                        feat['value'] = tmp

                valence = lex_type['valence']
                tmp = ''
                delimiter = '-'
                if valence.find('+') > -1:
                    delimiter = '+'
                argst = valence.split(delimiter)
                for i in range(0, len(argst)):
                    if argst[i] in inproper_case_names:
                        tmp += argst[i] + '_case'
                    else:
                        tmp += argst[i]
                    if i < (len(argst) - 1):
                        tmp += delimiter
                lex_type['valence'] = tmp


            for pc in self[lex_cat + '-pc']:
                for lrt in pc['lrt']:
                    for feat in lrt['feat']:
                        name = feat['name']
                        value = feat['value']
                        if name == 'case':
                            vs = value.split(', ')
                            tmp = ''
                            for i in range(0, len(vs)):
                                if vs[i] in inproper_case_names:
                                    tmp += vs[i] + '_case'
                                else:
                                    tmp += vs[i]
                                if i < (len(vs) - 1):
                                    tmp += ', '
                            self.convert_value(feat.full_key + '_value', value, tmp)
                        if name == 'argument structure':
                            argst = value.split('-')
                            tmp = ''
                            for i in range(0, len(argst)):
                                if argst[i] in inproper_case_names:
                                    tmp += argst[i] + '_case'
                                else:
                                    tmp += argst[i]
                                if i < (len(argst) - 1):
                                    tmp += '-'
                            self.convert_value(feat.full_key + '_value', value, tmp)


        for feature in self.get('feature'):
            if not feature.has_key('new'):
                feature['new'] = 'yes'
                feature['cat'] = 'both'

    def convert_26_to_27(self):
        """
        This uprev converts the names involving topicality in other features.
        """
        for feature in self.get('feature'):
            self.convert_value(feature.full_key + '_name', 'topicality', '_topicality')
            for value in feature['value']:
                self.convert_value(value.full_key + '_name', 'topic', '_topic')
                self.convert_value(value.full_key + '_name', 'non-topic', '_non-topic')
                for supertype in value['supertype']:
                    self.convert_value(supertype.full_key + '_name', 'topicality', '_topicality')

        for scale in self.get('scale'):
            for feat in scale['feat']:
                self.convert_value(feat.full_key + '_name', 'topicality', '_topicality')
                self.convert_value(feat.full_key + '_value', 'topic', '_topic')
                self.convert_value(feat.full_key + '_value', 'non-topic', '_non-topic')

        for cat in ['noun-pc', 'verb-pc']:
            for pc in self.get(cat):
                self.convert_value(pc.full_key + '_name', 'topic', '_topic')
                for lrt in pc['lrt']:
                    for feat in lrt['feat']:
                        self.convert_value(feat.full_key + '_name', 'topicality', '_topicality')
                        self.convert_value(feat.full_key + '_value', 'topic', '_topic')
                        self.convert_value(feat.full_key + '_value', 'non-topic', '_non-topic')

    def convert_27_to_28(self):
        """
        This uprev converts uppercase affixes into lowercase ones, because
        ACE does not handle uppercase suffixes.
        """
        for lex_cat in ['det','verb','noun',]:
            for pc in self[lex_cat + '-pc']:
                for lrt in pc['lrt']:
                    for lri in lrt['lri']:
                        orth = lri['orth']
                        self.convert_value(lri.full_key + '_orth', orth, orth.lower())



    def convert_28_to_29(self):
        """
        Updates the treatment of FORM.
        FORM will no longerbe implicit with has-aux=yes.
        It will be explicitly initialized in Other Features section.
        FORM will no longer be available in Tense, Aspect, and Mood section.
        FORM will no longer be a special hierarchy but will look much like other features,
        except finite and nonfinite values will be initialized by default.
        """
        if self.get('noaux-fin-nf') == 'on':
            self.convert_key('noaux-fin-nf', 'form-fin-nf')
        elif self.get('has-aux') == 'yes' or 'nf-subform' in self or 'fin-subform' in self:
            self['form-fin-nf'] = 'on'
        for subtype in self.get('nf-subform'):
            subtype['supertype'] = 'nonfinite'
        for subtype in self.get('fin-subform'):
            subtype['supertype'] = 'finite'
        if 'nf-subform' in self and 'fin-subform' in self:
            self.combine_keys('form-subtype','nf-subform','fin-subform')
        elif 'nf-subform' in self:
            self.convert_key('nf-subform', 'form-subtype')
        elif 'fin-subform' in self:
            self.convert_key('fin-subform', 'form-subtype')

    def convert_29_to_30(self):
        '''
        Updates clausal valency: inserts 'trans,' before valency starting with 'comps'.
        '''
        for v in self.get('verb'):
            if v['valence'].startswith('comps'):
                v['valence'] = 'trans,' + v['valence']

    def convert_30_to_31(self):
        for cs in self.get('comps'):
            self.convert_key('complementizer','stem',cs.full_key)

########################################################################
# FormData Class
# This Class acts like form data which would normally
# be sent from the server. Used for testing purposes.

class FormData:
    def __init__(self):
        self.data = {}

    def __getitem__(self, key):
        if key in self.data:
            return self.data[key]
        else:
            self.data[key] = FormInfo(key, None)
            return self.data[key]

    def __setitem__(self, key, value):
        self.data[key] = value

    def has_key(self, key):
        return key in self.data

    def keys(self):
        return self.data.keys()

class FormInfo:
    def __init__(self, key, value):
        self.key = key
        self.value = value

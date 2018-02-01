"""
File: stringmod.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents:
    glue - function that glues together a list of words joining them with a space
    StringMod - class that encapsulates types of modifications that can be done to harvester
                      strings to generate semantically equivalent seed strings
    StringModAddAff - subclass of StringMod that adds an affix to a string
    StringModOne - abstract subclass of StringMod
    StringModAddWord - subclass of StringModOne that adds a word to a string
    StringModChangeWord - subclass of StringModOne that changes all occurences of one word
                                         to another in a string
    StringModDropWord - subclass of StringModOne that removes all occurences of a word in a
                                     string
    string_mods - a list of instantiated StringMods that are the modifications we are using to
                         create seed strings from harvester strings
Tables accessed: none
Tables modified: none
History:
    9/4/09 - KEN changed nf from being part of an atomic word to being an affix
"""
#################################################################################
# StringMod class creates objects which modify strings in semantically
# neutral ways.  Call by add_permutes.py.

# It's time to update add_permutes so that it handles the non-semantics changing
# variations.  That is, we will ask the developers to now:

# 1) provide harvester strings
# 2) add instances of StringMod to represent semantically neutral
#    modifications.

# We should still record non-harvester seed strings in the DB so we can look
# them up and see if they have already been done.  That way, when someone comes
# up with a new variation that requires making new seed strings from existing
# harvesters, we can rerun without worrying about creating duplicates.

# The existing semantically-neutral variations are:

# mrs_id_list		variation
# -----------		---------
# all			add p-nom
# trans			add p-acc
# all			add aux
# trans			tv -> tv-nf
# intrans			iv -> iv-nf
# neg			drop neg  (we'll need to add a filter that says we 
# 				should only get neg semantics when we see
# 				some sort of overt negation)
# neg			add neg-
# ques			drop qpart (for inversion ex.)
# ques			add -ques

# The above are all independent.  That is, for any given harvester string,
# we want to try the powerset of all of the applicable variations.

# So: 

# variation a
# 	apply and pass through variation b
# 	don't apply and pass through variation b

# variation b
# 	apply and pass through variation c
# 	don't apply and pass through variation c

# etc.

# Because the seed strings are essentially just bags of words,
# it doesn't matter what order we do things in.  On the other hand,
# when we get to more complex clauses (and, ahem, coordination) we
# might need to count things within the input string.

# g.py defines global variables for classes of mrs tags.

import g
import re
from copy import deepcopy

#############################################################################
# Helper function.  I bet this is already defined, but can't find it right
# now

def glue(words):
    """
    Function: glue
    Input: words - a list of words to be put together into one string
    Output: string - words separated by a space
    Functionality: glues together a list of words joining them with a space
    Tables accessed: none
    Tables modified: none
    TODO: This could be achieved more quickly with something like ' '.join(words)
    """
    string = ''                     # initialize output
    l = len(words)               # get the length of the list
    e = l - 1                       # get the length of the list - 1
    for w in words[0:e]:        # for all but the last word...
        string += w                 # tack the word onto the output
        string += ' '                   # along with a space
    string += words[l-1]        # for the last word, just tack it on with no space

    return string                   # return output



#############################################################################
# String modification class.  These have two properties:
# the list of mrs_ids they apply to (again we want to have a single
# place to declare that) and the modification that they perform.

# The input to the StringMod modifications is a list of strings,
# where strings in turn are triples of lists: [words, prefixes, suffixes].

class StringMod:
    """
    Class: StringMod
    Members: mrs_id_list - a list of mrs tags to which this modification applies
    Functionality: encapsulates types of modifications that can be done to harvester strings to
                         generate semantically equivalent seed strings
    """
    def __init__(self,mrs_id_list):
        """
        Method: __init__
        Input:
            self - this StringMod
            mrs_id_list - a list of mrs tags to which this modification should apply
        Output: a new StringMod
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        self.mrs_id_list = mrs_id_list

    def applies(self,mrs_id):
        """
        Method: applies
        Input:
            self - this StringMod
            mrs_id - an mrs tag
        Output: True if this modification applies to mrs_id, False otherwise.
        Functionality: tells if this StringMod applies to a given mrs tag or not.
        Tables accessed: none
        Tables modified: none
        """
        return mrs_id in self.mrs_id_list   # check for mrs_id existence in mrs_id_list

class StringModAddAff(StringMod):
    """
    Class: StringModAddAff
    Superclass: StringMod
    Members:
        mrs_id_list - a list of mrs tags to which this modification applies
        affix - the affix this StringMod will add to sentences
        name - an identifier
    Functionality: encapsulates modifications that can be done to harvester strings to generate
                         semantically equivalent seed strings where an affix is added to the string
    """
    def __init__(self, mrs_id_list, affix):
        """
        Method: __init__
        Input:
            self - this StringModAddAff
            mrs_id_list - the list of mrs tags this StringMod applies to
            affix -the affix this StringMod will add to sentences
        Output: a new StringModAddAff instance
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        StringMod.__init__(self,mrs_id_list)
        self.affix = affix
        self.name = affix

    def modify(self, string_list):
        """
        Method: modify
        Input:
            self - this StringModAddAff
            string_list - a list of strings to be modified where a string is a trip of words, prefixes, and
                             suffixes
        Output: return_strings - a list of the strings in string_list with this StringMod's affix added
                                          as a prefix and, in separate strings, as a suffix
        Functionality: Adds this StringMod's affix as a prefix and a suffix to every input string
        Tables accessed: none
        Tables modified: none
        """
        print self.name             # print name of this StringMod
        return_strings = []         # initialize output
        for string in string_list:                      # for each string in input
            print "Input is " + str(string)           # print out the string

            # add the affix as a prefix
            return_strings.append(self.modstring1(deepcopy(string)))

            # add the affix as a suffix
            return_strings.append(self.modstring2(deepcopy(string)))
            print "Output is " + str(return_strings)    # print all output strings

        return return_strings                                # return output

    def modstring1(self, string):
        """
        Method: modstring1
        Input:
            self - this StringModAddAff
            string - the string to be modified where the string is a triple of words, prefixes, and
                       suffixes
        Output: string with this StringMod's affix added to the prefixes with a dash following it
        Functionality: adds this StringMod's affix as a prefix to string
        Tables accessed: none
        Tables modified: none
        """
        [words,prefixes,suffixes] = string      # unpack string
        prefixes.append(self.affix + "-")         # add this stringmod's affix as a prefix

        return [words, prefixes, suffixes]         # return string with prefix added

    def modstring2(self,string):
        """
        Method: modstring2
        Input:
            self - this StringModAddAff
            string - the string to be modified where the string is a triple of words, prefixes, and
                       suffixes
        Output: string with this StringMod's affix added to the suffixes with a dash before it
        Functionality: adds this StringMod's affix as a suffix to string
        Tables accessed: none
        Tables modified: none
        """
        [words,prefixes,suffixes] = string                # unpack string
        suffixes.append("-" + self.affix)                   # add this stringmod's affix as a suffix

        return [words,prefixes,suffixes]                    # return string with suffix added


class StringModOne(StringMod):
    """
    Class: StringModOne
    Superclass: StringMod
    Members:
        mrs_id_list - a list of mrs tags to which this modification applies
    Functionality: encapsulates modifications that can be done to harvester strings to generate
                         semantically equivalent seed strings TODO: finish this docstring
    """
    def __init__(self, mrs_id_list):
        """
        Method: __init__
        Input:
            self - this StringModOne
            mrs_id_list - a list of mrs tags to which this modification should apply
        Output: a new StringModOne
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        StringMod.__init__(self, mrs_id_list)       # call superclass constructor

    # These ones only produce one output string
    # per input string.

    def modify(self, string_list):
        """
        Method: modify
        Input:
            self - this StringModOne
            string_list - a list of strings to be modified where a string is a trip of words, prefixes, and
                             suffixes
        Output: return_strings - a copy of string_list with each string modified according to
                                           modstring
        Functionality: Modifies copies of input strings according to modstring method.
        Tables accessed: none
        Tables modified: none
        """
        return_strings = []                                         # initialize output

        for string in string_list:                                  # for every input string
            result = self.modstring(deepcopy(string))    # add the string to the output

            # if the string had length > 0 (which should always be the case since these strings
            # are triples)...
            if len(result) > 0:
                return_strings.append(result)           # ...add the string to the output
        return return_strings                               # return output

    def modstring(self, string):
        """
        Method: modstring
        Input:
            self - this StringModOne
            string - a string to be modified
        Output: string - same as input
        Functionality: returns its input
        Tables accessed: none
        Tables modified: none
        Note: This probably isn't used...I believe subclasses override it and those will be used.
        """
        return string

class StringModAddWord(StringModOne):
    """
    Class: StringModAddWord
    Superclass: StringModOne
    Members:
        mrs_id_list - a list of mrs tags to which this modification applies
        word - the word this StringMod will add to sentences
        name - an identifier
    Functionality: encapsulates modifications that can be done to harvester strings to generate
                         semantically equivalent seed strings where a word is added to the string
    """
    def __init__(self, mrs_id_list, word):
        """
        Method: __init__
        Input:
            self - this StringModAddWord
            mrs_id_list - the list of mrs tags this StringMod applies to
            word -the word this StringMod will add to sentences
        Output: a new StringModAddWord instance
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        StringModOne.__init__(self, mrs_id_list)    # call superclass constructor
        self.word = word                                       # set member word to input word
        self.name = word                                        # set member name to input word

    def modstring(self, string):
        """
        Method: modstring
        Input:
            self - this StringModAddWord
            string - a string to be modified
        Output: [words, prefixes, suffixes] - same as input string but with this StringMod's word
                                                            member added to word
        Functionality: adds this StringMod's word member to string
        Tables accessed: none
        Tables modified: none
        """
        print self.name                                 # print out name/word
        [words,prefixes,suffixes] = string       # unpack variable
        print "Input is " + str(words)               # print out input string
        words.append(self.word)                    # add this StringMod's word to word list
        print "Output is " + str(words)            # print out output string

        return [words,prefixes,suffixes]          # return output

class StringModChangeWord(StringModOne):
    """
    Class: StringModChangeWord
    Superclass: StringModOne
    Members:
        mrs_id_list - a list of mrs tags to which this modification applies
        old_word - the word this StringMod will change from in sentences
        new_word - the word this StringMod will change to in sentences        
        name - an identifier
    Functionality: encapsulates modifications that can be done to harvester strings to generate
                         semantically equivalent seed strings where a word is changed in a string
    """
    # Assuming for now that each word we might change
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.
    # (I think it already changes all occurences)

    def __init__(self, mrs_id_list, old_word, new_word):
        """
        Method: __init__
        Input:
            self - this StringModChangeWord
            mrs_id_list - the list of mrs tags this StringMod applies to
            old_word - the word this StringMod will change from in sentences
            new_word - the word this StringMod will change to in sentences        
        Output: a new StringModAddWord instance
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        StringModOne.__init__(self,mrs_id_list)         # call superclass constructor
        self.old_word = old_word                            # set old_word member to old_word input
        self.new_word = new_word                        # set new_word member to new_word input
        self.name = old_word + " to " + new_word        # build name member

    def modstring(self, string):
        """
        Method: modstring
        Input:
            self - this StringModChangeWord
            string - a string to be modified
        Output: [words, prefixes, suffixes] - same as input string but with all occurences of
                                                            old_word changed to new_word
        Functionality: changes this StringMod's old_word to new_word in string
        Tables accessed: none
        Tables modified: none
        """
        # TODO: make this more efficient.
        [words, prefixes, suffixes] = string                # unpack string
        print self.name                                            # print name
        print "Input is " + str(words)                          # print input string words
        s = glue(words)                                           # put the words together into one string

        # substitute old_word out for new_word in glued together string
        t = re.sub(self.old_word,self.new_word,s)
        words = t.split(' ')                                        # split the words back up
        print "Output is " + str(words)                       # print output string words
        return [words,prefixes,suffixes]                    # return output

class StringModDropWord(StringModOne):
    """
    Class: StringModDropWord
    Superclass: StringModOne
    Members:
        mrs_id_list - a list of mrs tags to which this modification applies
        drop_word - the word this StringMod will remove from sentences
        name - an identifier
    Functionality: encapsulates modifications that can be done to harvester strings to generate
                         semantically equivalent seed strings where a word is removed fromthe string
    """
    # Assuming for now that each word we might drop
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.
    # (I think this has already been fixed.)

    def __init__(self, mrs_id_list, drop_word):
        """
        Method: __init__
        Input:
            self - this StringModDropWord
            mrs_id_list - the list of mrs tags this StringMod applies to
            drop_word -the word this StringMod will remove from to sentences
        Output: a new StringModDropWord instance
        Functionality: constructor
        Tables accessed: none
        Tables modified: none
        """
        StringModOne.__init__(self, mrs_id_list)     # call superclass constructor
        self.drop_word = drop_word                        # set drop_word member to drop_word input
        self.name = "drop " + drop_word                 # set name member

    def modstring(self, string):
        """
        Method: modstring
        Input:
            self - this StringModDropWord
            string - a string to be modified
        Output: answer - same as input string but with this StringMod's drop_word member
                                 removed from words.  If drop_word was not in string, then this is just
                                    an empty list.
        Functionality: removes this StringMod's drop_word member from string
        Tables accessed: none
        Tables modified: none
        """
        [words, prefixes, suffixes] = string        # unpack string
        print self.name                                   # print name
        print "Input is " + str(words)                  # print input string words
        c = words.count(self.drop_word)             # count number of times drop_word is in input
        i = 0                                                   # initialize index

        while i < c:                                         # for each occurence of drop_word in input
            words.remove(self.drop_word)            # remove drop_word
            i += 1                                              # increment index
        print "Output is " + str(words)               # print output words

        if c > 0:                                               # if drop_word did occur in sentence
            answer = [words, prefixes, suffixes]          # return new output string
        else:                                                   # otherwise, our output would be the same...
            answer = []                                           # so just return an empty list

        return answer                                       # return output


# this list describes the kinds of modifications we can make to harvester strings to come up with
# semantically equivalent seed strings.  The permutations part comes later...this just adds/
# changes/drops words and affixes.
string_mods = [ StringModAddWord(g.all,"p-nom"),
                StringModAddWord(g.trans,"p-acc"),
                StringModAddWord(g.all, "aux"),
                StringModAddAff(g.neg, "neg"),
                StringModAddAff(g.ques, "ques"),

                # 9/4/09 KEN rem'd these two ChangeWord mods and added the AddAff mod for nf
                # StringModChangeWord(g.trans,"tv","tv-nf"),
                # StringModChangeWord(g.intrans,"iv","iv-nf"),
                StringModAddAff(g.all, 'nf'),

                # assume harvesters for negation have 'neg' as word.                
                StringModDropWord(g.neg, "neg"),
                StringModDropWord(g.ques, "qpart"), # assume harvesters for questions have 'qpart'.
                StringModChangeWord(["foo"],"bar","foo") # for testing
                ]

# Semantically non-neutral variations are:

# n2 subj/n1 subj
# det 
# neg
# ques
# iv/tv


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

    string = ''
    l = len(words)
    e = l - 1
    for w in words[0:e]:
        string += w
        string += ' '
    string += words[l-1]

    return string



#############################################################################
# String modification class.  These have two properties:
# the list of mrs_ids they apply to (again we want to have a single
# place to declare that) and the modification that they perform.

# The input to the StringMod modifications is a list of strings,
# where strings in turn are triples of lists: [words, prefixes, suffixes].

class StringMod:

    def __init__(self,mrs_id_list):
        self.mrs_id_list = mrs_id_list

    def applies(self,mrs_id):
        return mrs_id in self.mrs_id_list

class StringModAddAff(StringMod):


    def __init__(self,mrs_id_list,affix):
        StringMod.__init__(self,mrs_id_list)
        self.affix = affix
        self.name = affix

    def modify(self,string_list):
        print self.name
        return_strings = []
        for string in string_list:
            print "Input is " + str(string)
            return_strings.append(self.modstring1(deepcopy(string)))
            return_strings.append(self.modstring2(deepcopy(string)))
            print "Output is " + str(return_strings)
        return return_strings

    def modstring1(self,string):
        [words,prefixes,suffixes] = string
        prefixes.append(self.affix + "-")

        return [words,prefixes,suffixes]

    def modstring2(self,string):
        [words,prefixes,suffixes] = string
        suffixes.append("-" + self.affix)
        return [words,prefixes,suffixes]


class StringModOne(StringMod):

    def __init__(self,mrs_id_list):
        StringMod.__init__(self,mrs_id_list)

    # These ones only produce one output string
    # per input string.

    def modify(self,string_list):
        return_strings = []
        for string in string_list:
            result = self.modstring(deepcopy(string))
            if len(result) > 0:
                return_strings.append(result)
        return return_strings

    def modstring(self,string):
        return string
    
    
class StringModAddWord(StringModOne):

    def __init__(self,mrs_id_list,word):
        StringModOne.__init__(self,mrs_id_list)
        self.word = word
        self.name = word
        
    def modstring(self,string):
        print self.name
        [words,prefixes,suffixes] = string
        print "Input is " + str(words)
        words.append(self.word)
        print "Output is " + str(words)
        return [words,prefixes,suffixes]

class StringModChangeWord(StringModOne):

    # Assuming for now that each word we might change
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.

    def __init__(self,mrs_id_list,old_word,new_word):
        StringModOne.__init__(self,mrs_id_list)
        self.old_word = old_word
        self.new_word = new_word
        self.name = old_word + " to " + new_word

    def modstring(self,string):
        [words,prefixes,suffixes] = string
        print self.name
        print "Input is " + str(words)
        s = glue(words)
        t = re.sub(self.old_word,self.new_word,s)
        words = t.split(' ')
        print "Output is " + str(words)
        return [words,prefixes,suffixes]

class StringModDropWord(StringModOne):

    # Assuming for now that each word we might drop
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.


    def __init__(self,mrs_id_list,drop_word):
        StringModOne.__init__(self,mrs_id_list)
        self.drop_word = drop_word
        self.name = "drop " + drop_word

    def modstring(self,string):
        [words,prefixes,suffixes] = string
        print self.name
        print "Input is " + str(words)
        c = words.count(self.drop_word)
        i = 0
        while i < c:
            words.remove(self.drop_word)
            i += 1
        print "Output is " + str(words)
        if c > 0:
            return [words,prefixes,suffixes]
        else:
            return []



string_mods = [ StringModAddWord(g.all,"p-nom"),
                StringModAddWord(g.trans,"p-acc"),
                StringModAddWord(g.all, "aux"),
                StringModAddAff(g.neg, "neg"),
                StringModAddAff(g.ques, "ques"),
                StringModChangeWord(g.trans,"tv","tv-nf"),
                StringModChangeWord(g.intrans,"iv","iv-nf"),
                StringModDropWord(g.neg, "neg"),  # assume harvesters for negation have 'neg' as word.
                StringModDropWord(g.ques, "qpart"), # assume harvesters for questions have 'qpart'.
                StringModChangeWord(["foo"],"bar","foo") # for testing
                ]
                

                   


# Semantically non-neutral variations are:

# n2 subj/n1 subj
# det 
# neg
# ques
# iv/tv


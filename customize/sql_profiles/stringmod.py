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

class StringModAddAff:

    def __init__(self,mrs_id_list,affix):
        StringMod.__init__(self,mrs_id_list)
        self.affix = affix

    def modify(self,string_list):
        return_strings = []
        for string on string_list:
            return_strings += self.modstring1(string)
            return_strings += self.modstring2(string)
        return return_strings

    def modstring1(self,string):
        [words,prefixes,suffixes] = string
        prefixes += self.affix + "-"
        return [words,prefixes,suffixes]

    def modstring2(self,string):
        [words,prefixes,suffixes] = string
        suffixes += "-" + self.affix
        return [words,prefixes,suffixes]


class StringModOne(StringMod)

    def __init__(self,mrs_id_list):
        StringMod.__init__(self,mrs_id_list)

    # These ones only produce one output string
    # per input string.

    def modify(self,string_list):
        return_strings = []
        for string on string_list:
            return_strings += self.modstring(string)
        return return_strings

    def modstring(self,string):
        return string
    
    
class StringModAddWord(StringModOne):

    def __init__(self,mrs_id_list,word):
        StringModOne.__init__(self,mrs_id_list)
        self.word = word
        
    def modstring(self,string):
        [words,prefixes,suffixes] = string
        words += self.word
        return [words,prefixes,suffixes]

class StringModChangeWord(StringModOne):

    # Assuming for now that each word we might change
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.

    def __init__(self,mrs_id_list,old_word,new_word):
        StringModOne.__init__(self,mrs_id_list)
        self.old_word = old_word
        self.new_word = new_word

    def modstring(self,string):
        [words,prefixes,suffixes] = string
        s = glue(words)
        re.sub(self.old_word,self.new_word,s)
        words = s.split(' ')
        return [words,prefixes,suffixes]

class StringModDropWord(StringModOne):

    # Assuming for now that each word we might drop
    # occurs only once per string.  This will need to
    # be generalized. _FIX_ME_.


    def __init__(self,mrs_id_list,drop_word):
        StringModOne.__init__(self,mrs_id_list)
        self.drop_word

    def modstring(self,string):
        [words,prefixes,affixes] = string
        s = glue(words)
        re.sub(self.drop_word,"",s)
        words = s.split(' ')
        return [words,prefixes,suffixes]



string_mods = [ StringModAddWord(g.all,"p-nom"),
                StringModAddWord(g.trans,"p-acc"),
                StringModAddWord(g.all, "aux"),
                StringModAddAff(g.neg, "neg"),
                StringModAddAff(g.ques, "ques"),
                StringModChangeWord(g.trans,"tv","tv-nf"),
                StringModChangeWord(g.intrans,"iv","iv-nf"),
                StringModDropWord(g.neg, "neg"),  # assume harvesters for negation have 'neg' as word.
                StringModDropWord(g.ques, "qpart") # assume harvesters for questions have 'qpart'.
                ]
                

#############################################################################
# Helper function.  I bet this is already defined, but can't find it right
# now

def glue (words):

    string = ''
    l = len(words)
    e = l - 2
    for w in words[0:e]:
        string += w
        string += ' '
    string += words[l]

    return string

                   


# Semantically non-neutral variations are:

# n2 subj/n1 subj
# det 
# neg
# ques
# iv/tv


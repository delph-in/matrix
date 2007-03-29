#######################################################################
# DB-ified version of make_master_resource.  Assumes harvester profile
# (item, parse, result and run) and string list (seed_str, str_lst) are
# loaded into MatrixTDB.  Further assumes that mrs table has been populated
# (see populate_mrs_table.py).
#
# For each string in str_lst, generate (unique) permutations.
# For each permutation, create records in item, parse, and result,
# using result information from harvester for the relevant
# mrs_id.  For now, copy over parse information as well, but we might
# figure out how to put in better default values later.
#

#######################################################################
# TODO
#
# 1. Abtract out handling of prefixes and suffixes so we can add new ones
# without having to dig in this code.
#
# 2. Generalize so that this can take a specific range of string_list
# sl_id values and only target those.
#
# 3. Figure out what needs to be filled in to the other fields in item,
# parse and result that I don't really care about.
#
# 4. Figure out how to get auto-generated ids for further use.

#######################################################################
# Preliminaries

import MySQLdb
import re
import sys
from copy import deepcopy

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

######################################################################
# A function that takes a list of words and returns a list of lists
# containing all permutations of those words; no duplicates

def uniq_permute(s):

    # Break off prefixes and suffixes, to store them somewhere for
    # reattaching to each possible word as prefixes or suffixes later.
    
    # Break off prefixes and suffixies
    # _FIX_ME_ This should eventually be a parameter that gets defined
    # somewhere.
    
    s = re.sub('neg-','neg- ',s)
    s = re.sub('-neg',' -neg',s)
    s = re.sub('co1-', 'co1- ',s)
    s = re.sub('-co', ' -co',s)
    s = re.sub('co2-', 'co2- ',s)
    string = s.split(' ')

    # Step through string array to separate out bases from prefixes
    # from suffixes.
    words = []
    prefixes = []
    suffixes = []

    for item in string:
        if re.search(r'-$',item):
            prefixes.append(item)
        else:
            if re.search(r'^-', item):
                suffixes.append(item)
            else:
                words.append(item)
    
    # Call permute_outer_helper to actually do the permuting

    perms = permute_helper(words)

    # Create larger list appending each suffix and prefix to each possible
    # word in each permutation.

    perms = add_prefix_suffix(perms,prefixes,suffixes)

    # Glue words back together with white space.


    # Return result
    
    return perms


######################################################################
# Recursive permute function called by uniq_permute() above.

def permute_helper(words):

    # Permute_helper should keep track of words that it has used already
    # at that level, and not try a word if it's already done one which
    # is the same.

    tested = []

    # Collecting the permutations to return here

    perms = []

    # Base case: We're down to just one word, return the list containing
    # just that singleton list.

    if len(words) == 1:
        return [words]

    # Otherwise, try each word in the list, make sure we haven't tried it
    # at this level before, and recursively call permute_helper.
    
    for i in range (0,len(words)):

        if words[i] not in tested:
            # Record that we're using this word now.
            tested.append(words[i])

            # Make a new list with the rest of the words.
            rest_words = []
            for j in range (0,len(words)):
                if i != j:
                    rest_words.append(words[j])

            # Get all the permutations of the rest of the words.
            rest_perms = permute_helper(rest_words)
            
            # Prepend our word to each permutation, and store result in perms.

            for r in rest_perms:
                perm = [words[i]]
                perm += r
                perms.append(perm)

    return perms


#######################################################################
# add_prefix_suffix(perms,prefixes,suffixes) Function to reattach
# prefixes and suffixes in all possible ways, respecting their status
# as prefixes or suffixes

def add_prefix_suffix(perms,prefixes,suffixes):

    #This is going to be expensive, so don't bother if there aren't any
    #affixes!

    if len(prefixes) > 0:

        # Take affix-free permutations and return a list of
        # permutations where each has the right number of prefixes
        # and together they represent all the ways the prefixes can
        # be attached.

        prefixed_perms = attach_affixes(perms, prefixes)

        # Now take each word in each of the prefixed_perms and generate
        # the actual spellings, allowing for all possible orders of the
        # prefixes.

        flag = 'pre'
        
        prefixed_perms = fix_affix_spelling(prefixed_perms,flag)

    else:
        prefixed_perms = perms

    if len(suffixes) > 0:

        # Take the prefixed permutations and return a list of
        # permutations where each of the suffixes is also attached
        # and together they represent all the ways the suffixes can
        # be attached.

        suffixed_perms = attach_affixes(prefixed_perms, suffixes)

        # Now take each word in each of the prefixed_perms and generate
        # the actual spellings, allowing for all possible orders of the
        # suffixes.

        flag = 'suf'

        suffixed_perms = fix_affix_spelling(suffixed_perms,flag)

    else:
        suffixed_perms = prefixed_perms

    return suffixed_perms


def attach_affixes(perms, affixes):

    # This recursive function keeps attaching affixes until
    # we're all out.  We're going to get some duplicates here if
    # we have repeat affixes, but I'm not worrying about that for
    # now since it's not in my current test data. _FIX_ME_

    # base case

    if len(affixes) == 0:
        return perms

    # recusrive step
    new_perms = []
    affix = affixes[0]

    # take each permutation
    for perm in perms:

        # adding the result to new_perms as we go.
        for i in range (0,len(perm)):
            save = perm[i]
            list = [perm[i],affix]
            perm[i] = list
            snapshot = deepcopy(perm)
            new_perms.append(snapshot)
            perm[i] = save

    return attach_affixes(new_perms,affixes[1:])


def verify_perms(return_perms) :
    if type(return_perms) != list :
        raise ValueError, "outer list is bad"

    for item in return_perms :
        if type(item) != list :
            raise ValueError, "inner list is bad"

        for i in item :
            if type(i) != str :
                raise ValueError, "inner string is bad"
                

def fix_affix_spelling(perms,flag):

    #print perms

    # Now take each word in each of the prefixed_perms and generate
    # the actual spellings, allowing for all possible orders of the
    # suffixes.

    return_perms = []
    # verify_perms(return_perms)

    for perm in perms:

        #print perm

        # Go through the permutations, looking for words with associated
        # prefixes.  For each of those words, generate the possible surface forms
        # (all orders of prefixes).  We get back one permutation per spelling
        # of each of the words.  I think what I really want here is a lattice
        # that I can then read off...  For now, I'll do it recursively.

        word = perm[0]
        #print word
        #print type(word)

        if type(word) == str:
            # No affixes here
            #print "no affixes here"
            words = [word]
        else:
            # Let's my_unpack the nested list to get all of the affixes
            [stem,affixes] = my_unpack(word,[])

            #Now make all possible spellings
            words = glue_on_affixes(affixes,stem,flag)

        # base case
        #print words

        if len(perm) == 1:
            return_perms = [words]
            verify_perms(return_perms)
        else:
            # Make a new list with the rest of the words.

            #rest_words = copy(perm[1:])

            #rest_words = []
            #for j in range (1,len(perm)):
            #    rest_words.append(perm[j])

            rest_words = perm[1:]

            # Now create new perms starting with each word on words
            # followed by fix_affix_spelling(rest_words)

            new_input = [rest_words]
            rest_perms = fix_affix_spelling(new_input,flag)
            #print "rest_perms is: " + str(rest_perms)

            #print "words are: " + str(words)

            for w in words:

                #print "w is: " + str(w)
                temp = [w]
                # Prepend our word to each permutation, and store result in perms.
                for r in rest_perms:

                    if type(r) == str:
                        s = [r]
                    else:
                        s = r
                    #print "r is: " + str(r)
                    perm = [w] + s
                    #print "perm is: " + str(perm)
                    #verify_perms(return_perms)
                    return_perms.append(perm)
                    #verify_perms(return_perms)
  
    #print return_perms
    return return_perms



#######################################################################
# My_unpack: takes nested lists created by add_affixes and my_unpacks them

def my_unpack(nested_list,affix_list):

    # base case
    if type(nested_list) == str:
        return_value = [nested_list,affix_list]
        return return_value


    # recursive step
    affix_list.append(nested_list[1])
    return my_unpack(nested_list[0],affix_list)

    

#######################################################################
# Glue_on_affixes: takes a list of affixes, a word, and "pre" or "suf"
# makes all possible orders of the affixes and attaches them, according
# to "pre" or "suf".  Returns a list of words.

def glue_on_affixes(affixes,stem,flag):

    if len(affixes) == 0:
        print "Error: We shouldn't call glue_on_affixes if we have no affixes."
        sys.exit

    affix_perms = permute_helper(affixes)

    words = []
    
    # If we're attaching suffixes, take each possible order of them
    # and add them onto the end of the stem in that order.

    if flag == "suf":
        for ap in affix_perms:
            w = stem
            for a in ap:
                w += a
            words.append(w)

    # If we're attaching prefixes, take each possible order of them
    # and add them onto the beginning of the stem, in that order.

    if flag == "pre":
        for ap in affix_perms:
            w = ''
            for a in ap:
                w += a
            w += stem
            words.append(w)

    return words

#######################################################################
# Main program

# Find all of the string-mrs_id pairs in str_lst

# Actual plan:
cursor.execute("SELECT seed_str_value, sl_mrs_tag, mrs_value  FROM seed_str, str_lst,mrs WHERE seed_str.seed_id = str_lst.sl_seed_id AND str_lst.sl_mrs_tag = mrs.mrs_tag")

# For testing purposes:
# cursor.execute("SELECT seed_str_value, sl_mrs_tag, mrs_value  FROM seed_str, str_lst,mrs WHERE seed_str.seed_id = str_lst.sl_seed_id AND str_lst.sl_mrs_tag = mrs.mrs_tag AND seed_str.seed_id = 57")


records = cursor.fetchall()

# Process each of the returned records:

for record in records:

    string = record[0]
    mrs_id = record[1]
    mrs_value = record[2]

    # Find all (unique) permutations of the string

    perms = uniq_permute(string)
    #perms = [[string]]
    
    # Add item, parse, result records for each permutation

    for perm in perms:
        # Insert record into item, retrieving i_id
        # Deliberately leave i_wf uninstantiated, since we need to fill
        # that in on the basis of the filters

        input = ''
        for w in perm:
            input += w
            input += ' '
        
        length = len(perm)

        #_FIX_ME_ Find appropriate default values/fill in other column
        
        cursor.execute("INSERT INTO item SET i_input = %s, i_length = %s",(input,length))

        cursor.execute("SELECT LAST_INSERT_ID()")
        i_id_tuple = cursor.fetchone()
        i_id = i_id_tuple[0]

        # _FIX_ME_ Find appropriate default values
        # Assuming one reading per parse at this point.  Will need to map
        # multiple ones in when we figure out how many are legit in particular
        # language.
        cursor.execute("INSERT INTO parse SET p_i_id = %s, p_readings = 1",
                       (i_id))

        cursor.execute("SELECT LAST_INSERT_ID()")
        p_parse = cursor.fetchone()
        p_parse_id = p_parse[0]

        # _FIX_ME_ Find appropriate default values

        cursor.execute("INSERT INTO result SET r_parse_id = %s, r_mrs_tag = %s, r_mrs = %s", (p_parse_id,mrs_id,mrs_value))


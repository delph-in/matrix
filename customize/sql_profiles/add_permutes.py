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
import sys
from copy import deepcopy
from copy import copy
from stringmod import string_mods
from stringmod import glue
from post_permutes import post_permutes

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

######################################################################
# A function that takes a list of words and returns a list of lists
# containing all new seed strings to find permutations of.

def process_harvester(s,mrs_tag):

    # Read in harvester string + mrs_tag and create the other seed
    # strings in the equivalence class.   This function will
    # return a list of seed strings (including the harvester string)
    # and their associated prefixes and suffixes.  This assumes
    # that all prefixes and suffixes are handled in the StringMod
    # functions, and all harvester strings are affix-free.

    strings = create_seed_strings_from_harvester(s,mrs_tag)

    # Normalize the seed strings for storage in the DB
    # Check for any seed strings not already in the DB, and add them.

    new_strings = []

    for s in strings:

        [words,prefixes,affixes] = s
        words.sort()
        prefixes.sort()
        affixes.sort()
        storage_form = words + prefixes + affixes
        storage_string = glue(storage_form)

        cursor.execute("SELECT seed_str_value, seed_id FROM seed_str WHERE seed_str_value = %s",(storage_string)) 
        seed_tuple = cursor.fetchall()
        
        if len(seed_tuple) > 1:
            raise ValueError, "MatrixTDB.seed_str contains more than one seed string with the same form."

        if len(seed_tuple) == 0:
            # We have a new seed string. Don't even have to check if it's a new
            # seed-mrs pair.  Just add it to the list.  And add them to the DB.
            new_strings.append(s)
            cursor.execute("INSERT INTO seed_str SET seed_str_value = %s",(storage_string))
            cursor.execute("SELECT LAST_INSERT_ID()")
            seed_id_tuple = cursor.fetchone()
            seed_id = seed_id_tuple[0]
            cursor.execute("INSERT INTO str_lst SET sl_mrs_tag = %s, sl_seed_id = %s",(mrs_tag,seed_id))

        if len(seed_tuple) == 1:
            # We have an existing seed string.  Check to see if we already have it
            # paired with this MRS.
            cursor.execute("SELECT seed_str_value, sl_mrs_tag FROM seed_str, str_lst WHERE sl_seed_id = seed_id AND seed_str_value = %s AND sl_mrs_tag = %s", (storage_string,mrs_tag))
            mrs_tuple = cursor.fetchall()

            if len(mrs_tuple) > 1:
                raise ValueError, "A string is paired with the same mrs_tag twice in MatrixTDB.str_lst."
            
            if len(mrs_tuple) == 0:
                # We haven't seen this string with this MRS yet, so add it.
                new_strings.append(s)
                cursor.execute("SELECT seed_id FROM seed_str WHERE seed_str_value = %s",(storage_string))
                seed_id_tuple = cursor.fetchone()
                seed_id = seed_id_tuple[0]
                cursor.execute("INSERT INTO str_lst SET sl_mrs_tag = %s, sl_seed_id = %s",(mrs_tag,seed_id))

            # else, we've seen this seed-mrs pair already, so do nothing.

    # Okay, pass out the list of new_strings and their common mrs_tag so that we can
    # then run them through the permute functions.

    return new_strings


def uniq_permute(s):

    [words,prefixes,suffixes] = s

    # Call permute_outer_helper to actually do the permuting

    perms = permute_helper(words)

    # Create larger list appending each suffix and prefix to each possible
    # word in each permutation.

    perms = add_prefix_suffix(perms,prefixes,suffixes)

    # Return result
    
    return perms


##############################################################################
# Before we do the permutation, we need to get the master list of seed
# strings on the basis of the harvester strings.
# create_seed_strings_from_harvester takes a harvester string and
# calls create_seed_strings on it to return the semantic equivalence
# class, including the harvester.

def create_seed_strings_from_harvester(harv,mrs_tag):

    # Find out which string modifications are appropriate for
    # the mrs tag in question.  

    mods_list = []

    for mod in string_mods:
        if mod.applies(mrs_tag):
            mods_list.append(mod)

    # Break harvester string into sequence of words.

    words = harv.split(' ')

    # Call create_seed_strings_helper with the list of
    # string modifications.

    # Strings are now triples of words, prefixes, suffixes.
    # Make this a list containing one such string, since
    # helper functions expect to be working with a list of
    # such objects.

    input_string = [[words,[],[]]]
    return_strings = create_seed_strings(input_string,mods_list)

    return return_strings

def create_seed_strings(string_list,mods_list):

    # Base case: If we've reached the end of the mods_list
    # return the string_list.
    
    if mods_list == []:
        return string_list
    else:
        # New string list is the old one appended to the a list
        # with every string in it modified according to the
        # first thing on mods_list.

        # This means that every modification is optional and
        # independent.
        
        string_list_copy = deepcopy(string_list)
        more_strings =  mods_list[0].modify(string_list_copy)
        string_list = string_list + more_strings
        return create_seed_strings(string_list,mods_list[1:])
    



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
        sys.exit()

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
# get_harvester_strings_to_update(osp_id): given a user-supplied
# osp_id or 'a', return the list of harvester strings to process.
# harvester strings are tuples of [string,mrs_tag,osp_id]

def get_harvester_strings_to_update(osp_id):

    harvs = []

    if type(osp_id) == int:

        cursor.execute("SELECT hs_string, hs_mrs_tag FROM harv_str WHERE hs_cur_osp_id = %s",(osp_id))
        harv_tuples = cursor.fetchall()

    elif osp_id == 'a':

        cursor.execute("SELECT hs_string, hs_mrs_tag,hs_cur_osp_id FROM harv_str",())
        harv_tuples = cursor.fetchall()

    else:
        raise ValueError, "Invalid osp_id."


    if len(harv_tuples) == 0:
        raise ValueError, "No harvester strings in MatrixTDB for that osp_id."
        

    if type(osp_id) == int:
        for harv_tup in harv_tuples:
            (hs_string,hs_mrs_tag) = harv_tup
            harvs.append([hs_string,hs_mrs_tag,osp_id])

    elif osp_id == 'a':
        for harv_tup in harv_tuples:
            (hs_string,hs_mrs_tag,osp_id) = harv_tup
            harvs.append([hs_string,hs_mrs_tag,osp_id])

    return harvs


#######################################################################
# insert_item(input, length, osp_id, mrs_tag)
#   Insert an item into the 'item' table, along with the corresponding
#   entries in the 'parse' and 'result' tables.

def insert_item(input, length, osp_id, mrs_tag):
    # if the length (# of words) isn't specified, calculate it
    if length == -1:
        length = 

    cursor.execute("INSERT INTO item SET i_input = %s, i_length = %s, i_osp_id = %s, i_author = %s",(input,length,osp_id,"add_permutes.py"))
        
    cursor.execute("SELECT LAST_INSERT_ID()")
    i_id_tuple = cursor.fetchone()
    i_id = i_id_tuple[0]

    cursor.execute("INSERT INTO parse SET p_i_id = %s, p_readings = 1, p_osp_id = %s",
                   (i_id,osp_id))

    cursor.execute("SELECT LAST_INSERT_ID()")
    p_parse = cursor.fetchone()
    p_parse_id = p_parse[0]
            
    cursor.execute("INSERT INTO result SET r_parse_id = %s, r_mrs = %s, r_osp_id = %s", (p_parse_id,mrs_tag,osp_id))

    return i_id


#######################################################################
# Main program

# Ask the user for a specific osp_id to work from, or 'all'.
# If specific osp_id, just look for harvester strings belonging to that
# osp_id.

def main():
    osp_id = raw_input("Please input original source profile id (osp_id) for the source\n profile you're working with.  If you've updated the string\n modifications and wish to update seed strings for all harvester\n strings, enter 'a' ")

    harv = get_harvester_strings_to_update(osp_id)

    for h in harv:

        [hs,mrs_tag,osp_id] = h  # Get harvester string and its tag and the osp_id for that string
        new_strings = process_harvester(hs,mrs_tag) # run stringmods and find all new seed strings for that harvester

        for s in new_strings:
            
            perms = uniq_permute(s) # Get all permutations of the string

            for p in perms:

                input = ''
                for w in p:
                    input += w
                    input += ' '
                
                length = len(p)

                insert_item(input, length, osp_id, mrs_tag)

                for pp in post_permutes:
                    if mrs_tag == pp.mrs_id:
                        for s in pp.apply(input):
                            insert_item(s, -1, osp_id, pp.new_mrs_id)


if __name__ == "__main__":
  main()

"""
File: add_permutes.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents: (listed, when possible, in order of execution)
    main code that creates appropriate connection to database and calls main()
    main - prompts user for a source profile id, then generates every possible permutation of every
              possible seed string for the harvester strings in that source profile and inserts them into
              the MatrixTDB database
    get_harvester_strings_to_update - gets all the harvester strings and their mrs tags for a given
                                                      original source profile id or for all source profiles
    process_harvester - function that creates seed strings from harvester string and makes
                                 sure that database is updated with those seed strings and associated
                                 with the right mrs_tags
    create_seed_strings_from_harvester - helper function to process_harvester that creates a list
                                                            of seed strings from a harvester string
    create_seed_strings - helper function to create_seed_strings_from_harvester that runs string
                                    mods on strings recursively
    uniq_permute - function that creates every possible permutation of a string
    permute_helper - creates a list of all the permutations of the words or affixes in a list
    add_prefix_suffix - helper function to uniq_permute that takes a list of words, prefixes, and
                               suffixes and adds the prefixes and suffixes to words in every possible
                                permutation
    attach_affixes - recursive helper function to add_prefix_suffix that creates more attaches given
                            affixes to given words in every possible permutation
    fix_affix_spelling - recursive helper function to add_prefix_suffix that takes the nested list
                               format of attach_affixes and actually puts the affixes on the word correctly
    my_unpack - helper function to fix_affix_spelling that unpacks the nested list output format of
                        attach_affixes
    glue_on_affixes - recursive helper function to fix_affix_spelling that permutes a list of affixes
                             and attaches each permutation onto a word
    verify_perms - verifies the output of fix_affix_spelling
    insert_item - inserts a string into the item_tsdb, parse, and result tables
    insert_many_items - function that, like insert_item (except does for many string/mrs pairs, not
                                   just one), inserts a set of string/mrs pairs into the item_tsdb, parse, and
                                   result tables
    runUnivFltrs - function that runs a string/mrs combo through universal filters until one fails to
                        see if we have a string/mrs combo that passes all universal filters
Tables Accessed: seed_str, str_lst, item_tsdb, parse, result, harv_str
Tables Updated: seed_str, str_lst, item_tsdb, parse, result
History:
    9/8/09 - made changes to only insert those permutes that pass all universal filters
"""
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
from matrix_tdb_conn import MatrixTDBConn
from u_filters import filter_list as ufilter_list

######################################################################
# A function that takes a list of words and returns a list of lists
# containing all new seed strings to find permutations of.

def process_harvester(s, mrs_tag, conn):
    """
    Function: process_harvester
    Input:
        s - a harvester string
        mrs_tag - the semantic class for s
        conn - a MatrixTDBConn, a connection to the MatrixTDB database        
    Output: new_strings - a list of seed strings created from s that we either haven't seen at all
                                    before in the database or that we haven't seen paired with mrs_tag.
                                    These strings are triples of words, prefixes, and suffixes
    Functionality: Creates seed strings from harvester string and makes sure that database is
                         updated with those seed strings and associated with the right mrs_tags
    Tables Accessed: seed_str, str_lst
    Tables Modified: seed_str, str_lst
    """
    # Take in harvester string + mrs_tag and create the other seed
    # strings in the equivalence class.   This function will
    # return a list of seed strings (including the harvester string)
    # and their associated prefixes and suffixes.  This assumes
    # that all prefixes and suffixes are handled in the StringMod
    # functions, and all harvester strings are affix-free.

    # get the seed strings from the harvester string s.
    # this function takes a normal sentence string put returns a list of strings that are triples
    # of words, prefixes, and suffixes
    strings = create_seed_strings_from_harvester(s, mrs_tag)

    # Normalize the seed strings for storage in the DB
    # Check for any seed strings not already in the DB, and add them.

    new_strings = []                                # initialize output list

    for s in strings:                                           # for every seed string
        [words,prefixes,affixes] = s                      # unpack its variables
        words.sort()                                            # sort its words
        prefixes.sort()                                         # sort its prefixes
        affixes.sort()                                           # sort its suffixes
        storage_form = words + prefixes + affixes # concatenate those three lists

        # and separate them all by space in one long string as the storage string
        storage_string = glue(storage_form) 

        # check to see if this string is in the db already
        seed_tuple = conn.selQuery("SELECT seed_str_value, seed_id FROM seed_str " + \
                                                  "WHERE seed_str_value = %s", (storage_string)) 
        
        if len(seed_tuple) > 1:             # if it's already in the database more than once...
            # ...raise an error
            # TODO: consider making this the primary key if we want uniqueness.
            raise ValueError, "MatrixTDB.seed_str contains more than one seed string with the " + \
                                      "same form."

        if len(seed_tuple) == 0:        # if it's not in the database already
            # We have a new seed string. Don't even have to check if it's a new
            # seed-mrs pair.  Just add it to the list.  And add them to the DB.
            new_strings.append(s)       # add it to the list of new strings

            # insert the string into the database
            conn.execute("INSERT INTO seed_str SET seed_str_value = %s", (storage_string))
            seed_id = conn.selQuery("SELECT LAST_INSERT_ID()")[0][0]    # get the string's ID

            # insert into str_lst, which links the string's ID to its mrs tag.
            conn.execute("INSERT INTO str_lst " + \
                                 "SET sl_mrs_tag = %s, sl_seed_id = %s",(mrs_tag,seed_id))
            
        elif len(seed_tuple) == 1:      # if the seed string was already in the database once
            # We have an existing seed string.  Check to see if we already have it
            # paired with this MRS.
            mrs_tuple = conn.selQuery("SELECT seed_str_value, sl_mrs_tag " + \
                                                     "FROM seed_str, str_lst " + \
                                                     "WHERE sl_seed_id = seed_id AND seed_str_value = %s " + \
                                                      "AND sl_mrs_tag = %s", (storage_string, mrs_tag))

            if len(mrs_tuple) > 1:      # if it's paired with mrs_tag more than once
                # raise an error
                raise ValueError, "A string is paired with the same mrs_tag twice in table str_lsti."
            
            elif len(mrs_tuple) == 0:     # if it's not paired with that mrs_tag
                # We haven't seen this string with this MRS yet, so add it.
                new_strings.append(s)   # consider it a new string and add it to list

                # get the seed string's ID from database
                seed_id = conn.selQuery("SELECT seed_id FROM seed_str " + \
                                                      "WHERE seed_str_value = %s",(storage_string))[0][0]

                # insert into str_lst to link the string's ID to its semantic tag
                conn.execute("INSERT INTO str_lst " + \
                                     "SET sl_mrs_tag = %s, sl_seed_id = %s",(mrs_tag, seed_id))

            # else, we've seen this seed-mrs pair already, so do nothing.

    # print a status message telling the user how many new string/mrs pairs we found
    print "Process harvester found", len(new_strings), "seed strings that are either not in the " + \
            "database or not paired with the semantic class given for the harvester string."

    # Okay, pass out the list of new_strings for mrs_tag so that we can
    # then run them through the permute functions.
    return new_strings

def uniq_permute(s):
    """
    Function: uniq_permute
    Input: s - a seed string that is a triple of words, prefixes, and suffixes
    Output: perms - a list of every possible permutation of the words, prefixes, and suffixes in s
                            perms will be a list of lists, where the outer list is each permutation and the
                            inner lists are lists of words
    Functionality: Creates every possible permutation of s
    Tables Accessed: none
    Tables Modified: none
    """
    [words, prefixes, suffixes] = s         # unpack the string's variables

    # Call permute_helper on the words to actually do the permuting of the words
    perms = permute_helper(words)

    # Create larger list appending each suffix and prefix to each possible
    # word in each permutation.
    perms = add_prefix_suffix(perms, prefixes, suffixes)

    return perms                                 # return output


##############################################################################
# Before we do the permutation, we need to get the master list of seed
# strings on the basis of the harvester strings.
# create_seed_strings_from_harvester takes a harvester string and
# calls create_seed_strings on it to return the semantic equivalence
# class, including the harvester.

def create_seed_strings_from_harvester(harv, mrs_tag):
    """
    Function: create_seed_strings_from_harvester
    Input:
        harv - a harvester string
        mrs_tag - harv's mrs tag
    Output: return-strings - all the seed strings for harv where the strings are triples of words,
                                      prefixes, and suffixes
    Functionality: Creates a list of seed strings from a harvester string
    Tables accessed: none
    Tables modified: none
    """
    # Find out which string modifications are appropriate for
    # the mrs tag in question.  

    mods_list = []                          # initialize list of string mods to perform on this harvester

    for mod in string_mods:             # for every possible string modification...
        if mod.applies(mrs_tag):         # ...if it applies to the semantic class mrs_tag...
            mods_list.append(mod)     # ...add it to the list of modifications to make

    # Break harvester string into sequence of words.
    words = harv.split(' ')

    # Call create_seed_strings_helper with the list of
    # string modifications.

    # Strings are now triples of words, prefixes, suffixes.
    # Make this a list containing one such string, since
    # helper functions expect to be working with a list of
    # such objects.
    input_string = [[words, [], []]]

    # run every mod in mods_list on every string in input_string to get a list of seed_strings from
    # the harvester string
    return_strings = create_seed_strings(input_string, mods_list)
 
    return return_strings                           # return output

def create_seed_strings(string_list, mods_list):
    """
    Function: create_seed_strings
    Input:
        string_list - a list of strings to be modified, where a string is a triple of words, prefixes, and
                         suffixes
        mods_list - the list of modifications to be performed on each string in string_list
    Output: answer - string_list plus the recursive result of running every mod in mods_list on
                             everything in string_list appended to it
    Functionality: runs every mod in mods_list on every string in string_list recursively.
    Tables accessed: none
    Tables modified: none
    """
    if mods_list == []:                 # Base case: If we've reached the end of the mods_list...
        answer = string_list          # ...set string_list input to output
    else:
        # New string list is the old one appended to the a list
        # with every string in it modified according to the
        # first thing on mods_list.

        # This means that every modification is optional and
        # independent.
        
        string_list_copy = deepcopy(string_list)                        # create a copy of string_list

        # create a list more_strings that consists of the first thing in mods_list being run on
        # everything in string_list
        more_strings =  mods_list[0].modify(string_list_copy)

        # put the new more_strings into the list string_list
        string_list = string_list + more_strings
        # TODO: is there a reason to do this recursively and not just as a loop?
        # TODO: I think we can avoid this copying, too, and make things more efficient.
        # b/c I think the modifies are non-destructive to their input lists...and some of them do
        # some copying too.

        # recursively run all old and new strings on the cdr of mods_list (all other mods after the
        # first) and set that list to output
        answer = create_seed_strings(string_list, mods_list[1:])

    return answer               # return output

######################################################################
# Recursive permute function called by uniq_permute() above and glue_on_affixes

def permute_helper(words):
    """
    Function: permute_helper
    Input: words - a list of words in a seed string or a list of affixes we're permuting
    Output: perms - a list of permuations of words
    Functionality: Creates a list of all the permutations of the words or affixes in words
    Tables Accessed: None
    Tables Modified: None
    """
    # TODO: look into a cheaper way to do this involving sets

    # Permute_helper should keep track of words that it has used already
    # at that level, and not try a word if it's already done one which
    # is the same.

    tested = []         # initialize list that keeps track of words it has used already at this level

    # Collecting the permutations to return here

    perms = []          # initialize output

    # Base case: We're down to just one word, return the list containing
    # just that singleton list.
    if len(words) == 1:
        # TODO: really?  we take a list of one word and put it in another list?  look into this
        perms = [words]      # put the list in another list and set to output
    else:                           # otherwise, if we have more than just one word...
        # ...try each word in the list, make sure we haven't tried it
        # at this level before, and recursively call permute_helper.
        
        for i in range(len(words)):             # for every word in the input list
            if words[i] not in tested:           # if we haven't used this word yet...
                # ...Record that we're using this word now.
                tested.append(words[i])

                # Make a new list with the rest of the words.
                rest_words = []
                # TODO: could be done faster with a set probably
                # ...maybe not given a word might repeat in a sentence and we don't want to lose that
                for j in range (len(words)):               # for every word in the input list...
                    if i != j:                                      # ...that is not the current word we're using...
                        rest_words.append(words[j])   # ...add it to rest_words list

                # Recursively get all the permutations of the rest of the words.
                rest_perms = permute_helper(rest_words)
                
                # Prepend our word to each permutation, and store result in perms.

                for r in rest_perms:        # for every permutation of the rest of the words we get...
                    perm = [words[i]]       # set this permutation to be prepended with current word
                    perm += r                  # add in permutation of rest of words
                    perms.append(perm)  # and add that to the output list

    return perms                        # return output


#######################################################################
# add_prefix_suffix(perms,prefixes,suffixes) Function to reattach
# prefixes and suffixes in all possible ways, respecting their status
# as prefixes or suffixes

def add_prefix_suffix(perms, prefixes, suffixes):
    """
    Function: add_prefix_suffix
    Input:
        perms - a list of lists of words, where each inner list is a unique permutation of the words
                    in a seed string
        prefixes - a list of prefixes for the seed string 
        suffixes - a list of suffixes for the seed string
    Output: suffixed_perms - same as perms except bigger, with prefixes and suffixes added to
                                         words in every possible permutation
    Functionality: takes a list of words, prefixes, and suffixes and adds the prefixes and suffixes to
                         words in every possible permutation
    Tables Accessed: none
    Tables Modified:none
    """

    #This is going to be expensive, so don't bother if there aren't any
    #affixes!

    if len(prefixes) > 0:   # if there is at least one prefix

        # Take affix-free permutations and return a list of
        # permutations where each has the right number of prefixes
        # and together they represent all the ways the prefixes can
        # be attached.

        prefixed_perms = attach_affixes(perms, prefixes)  #  add prefixes to permutations
        # at this point the words might just be words but if they have prefixes they will be nested in
        # the form [[[word, prefix], prefix], prefix]

        # Now take each word in each of the prefixed_perms and generate
        # the actual spellings, allowing for all possible orders of the
        # prefixes.
        flag = 'pre'                        # set flag to pre

        # put the prefixes onto every word in every possible way.
        prefixed_perms = fix_affix_spelling(prefixed_perms, flag)
        
        # at this point, prefixed_perms is a list of permutations where each permutation is a list
        # of words and each word is just a string, with or without prefixes.
    else:                                       # if there are no prefixes
        prefixed_perms = perms       # just call prefixed_perms perms

    if len(suffixes) > 0:                   # if there is at least one suffix

        # Take the prefixed permutations and return a list of
        # permutations where each of the suffixes is also attached
        # and together they represent all the ways the suffixes can
        # be attached.

        suffixed_perms = attach_affixes(prefixed_perms, suffixes)   # add suffixes to permutations
        # at this point the words might just be words but if they have suffixes they will be nested in
        # the form [[[word, suffix], suffix], suffix]

        # Now take each word in each of the prefixed_perms and generate
        # the actual spellings, allowing for all possible orders of the
        # suffixes.
        flag = 'suf'

        # put the suffixes on every applicable word in every possible way
        suffixed_perms = fix_affix_spelling(suffixed_perms, flag)
        # at this point, suffixed_perms is a list of permutations where each permutation is a list
        # of words and each word is just a string, with or without affixes.
    else:                                                   # if there are no suffixes
        suffixed_perms = prefixed_perms       # then just assign prefixed_perms to output variable

    return suffixed_perms                           # return output


def attach_affixes(perms, affixes):
    """
    Function: attach_affixes
    Input:
        perms - a list of lists of words, where each inner list is a unique permutation of the words
                    in a seed string
        affixes - a list of affixes, either prefixes or suffixes, for the seed string 
    Output: answer - more permutations of perms with the affixes applied.  It attaches affixes in
                             the form: [[[word, affix], affix], affix], nesting them in such a way.
    Functionality: Creates more permutations where for every permutation in perms there will now
                         be several permutations, where each new permutation has all of the affixes
                         added to one of its words
    Tables Accessed: none
    Tables Modified: none
    """

    # This recursive function keeps attaching affixes until
    # we're all out.  We're going to get some duplicates here if
    # we have repeat affixes, but I'm not worrying about that for
    # now since it's not in my current test data. _FIX_ME_

    # base case

    if len(affixes) == 0:       # if there are no affixes
        answer = perms            # just return the input list of permutations
    else:                               # but if we do have affixes
        # recursive step
        new_perms = []          # initialize a list to store permutations with affixes added to words
        affix = affixes[0]          # we will be concerned with first affix in list

        # take each permutation
        for perm in perms:                          # for each permutation

            # adding the result to new_perms as we go.
            for i in range (len(perm)):             # for each word in that permutation
                save = perm[i]                        # get that word

                # create a list of that word followed by the first affix
                wordAffixList = [perm[i],affix]                  
                perm[i] = wordAffixList                 # and put that list where the word was

                # now make a copy of the permutation with its word replaced with the list [word,affix]   
                snapshot = deepcopy(perm)
                new_perms.append(snapshot)      # and add that copy to the list of new permutations
                perm[i] = save                            # replace the word back into the permutation

        # set to output the results of recursively running this function using the new affix-added
        # permutations we came up with here and the rest of the affixes we didn't use here
        answer =  attach_affixes(new_perms, affixes[1:])

    return answer                                           # return output

def verify_perms(return_perms) :
    """
    Function: verify_perms
    Input: a list of permutations of words where each word has had its affixes attached in every
             possible permutation
    Output: none, but a ValueError will be raised if the form of return_perms is incorrect
    Functionality: verifies the output of fix_affix_spelling.
    Tables accessed: none
    Tables modified: none
    """
    if type(return_perms) != list :                 # if it's not a list...
        raise ValueError, "Error: verify_perms: outer list is bad"   # ...that's a problem

    for item in return_perms :                        # for each permutation...
        if type(item) != list :                             # ...if it is not a list...
            raise ValueError, "Error: verify_perms: inner list is bad"   # ...that is a problem

        for i in item :                                     # for every word in a permutation...
            if type(i) != str :                             # ...if it is not a string...
                raise ValueError, "Error: verify_perms: inner string is bad"         # ...that is a problem
                

def fix_affix_spelling(perms, flag):
    """
    Function: fix_affix_spelling
    Input:
        perms - a list of permutations of words where the words have affixes nested in the form
                    [[[word, affix], affix], affix]
        flag - either 'pre' or 'suf' indicating what type of affix is in perms
    Output: return_perms - a list of permutations of words just like perms except the affixes have
                                      been attached to their words in every possible permutation.
    Functionality: Converts a list of permutations of words where the words have affixes nested in
                        the form [[[word, affix], affix], affix] to a larger list of permutations where each
                        word has had its affixes attached in every possible permutation
    Tables Accessed: none
    Tables Modified: none
    """
    # Now take each word in each of the prefixed_perms and generate
    # the actual spellings, allowing for all possible orders of the
    # suffixes.

    return_perms = []                       # initialize output

    for perm in perms:                      # for every permutation in input
        # Go through the permutations, looking for words with associated
        # prefixes.  For each of those words, generate the possible surface forms
        # (all orders of prefixes).  We get back one permutation per spelling
        # of each of the words.  I think what I really want here is a lattice
        # that I can then read off...  For now, I'll do it recursively.

        word = perm[0]                      # get the first word of the permutation

        if type(word) == str:               # if it is a string
            # No affixes here
            words = [word]                  # don't worry about it, just create a list of this word
        else:                       # otherwise it should be a list nested like [[[word, affix], affix], affix]
            # Let's my_unpack the nested list to get all of the affixes
            # now change that nested format to a format like [word, [affix, affix, affix]]
            [stem, affixes] = my_unpack(word, [])

            #Now make all possible spellings
            # permute the affixes and get a list of spellings that is the stem with each possible
            # permutation attached.
            words = glue_on_affixes(affixes, stem, flag)

        # base case

        if len(perm) == 1:                       # if this permutation  was just one word long
            # the line below is causing problems
            # return_perms = [words]          # set return_perms to all possible spellings of that word
            # return_perms = words # attempt to fix failed; errors out in verify_perms
            # return_perms += words # attempt to fix failed: errors out in verify_perms
            # return_perms.append(words) # attempt to fix failed: does same thing as original

            # you need to put each possible spelling as a one-word permutation list in a list
            # of those permutations
            return_perms = [[word] for word in words]   # attempt at fixing problem...works?
            verify_perms(return_perms)    # verify the format of the return list
        else:                                        # but if this permutation had more than one word in it
            # Make a new list with the rest of the words.
            rest_words = perm[1:]

            # Now create new perms starting with each word on words
            # followed by fix_affix_spelling(rest_words)
            new_input = [rest_words]                                # set that new list to new_input

            # recursively call this function on the rest of the words in the permutation
            rest_perms = fix_affix_spelling(new_input, flag)

            for w in words:                         # for each spelling of the first word in the permutation
                temp = [w]                          # put the spelling in temp variable
                # Prepend our word to each permutation, and store result in perms.
                for r in rest_perms:              # for each permutation for all the words after this one...
                    if type(r) == str:              # ...if it's a string...
                        s = [r]                         # ...put it in a list...
                    else:                               # ...if it's not a string...
                        s = r                           # ...then don't do anything to it
                    perm = [w] + s                      # add that permutation on to this spelling
                    return_perms.append(perm)   # and put that combo into the output list

    return return_perms                                 # return output

#######################################################################
# My_unpack: takes nested lists created by add_affixes and my_unpacks them

def my_unpack(nested_list, affix_list):
    """
    Function: my_unpack
    Input:
        nested_list - a list that is [[[word, affix], affix], affix] nested indefinitely deep.  Created as
                           such by attach_affixes
        affix_list - a list of affixes that have been unpacked thus far in this recursive function
    Output: return_value - the same as nested list, but unpacked as [word, [affix, affix, affix]]
    Functionality: Unpacks the list format produced by attach_affixes into a list whose first
                         element is a word and whose second element is a list of affixes
    Tables accessed: none
    Tables modified: none
    """
    # base case
    if type(nested_list) == str:                        # if the nested_list is a string, it's just word, ...
        return_value = [nested_list, affix_list]      # ...so we're done, now just return
    else:                                                # if nested_list is a list, there's more unpcaking to do
        # recursive step
        affix_list.append(nested_list[1])      # get the next affix and put it on the affix_list
        # and recursively go deeper
        return_value = my_unpack(nested_list[0], affix_list)
        
    return return_value                             # return output

#######################################################################
# Glue_on_affixes: takes a list of affixes, a word, and "pre" or "suf"
# makes all possible orders of the affixes and attaches them, according
# to "pre" or "suf".  Returns a list of words.

def glue_on_affixes(affixes, stem, flag):
    """
    Function: glue_on_affixes
    Input:
        affixes - a list of affixes
        stem - a string that is a stem word
        flag - either 'pre' or 'suf' depending on what type of affixes are in affixes list
    Output: words - a list of words that is the stem with every possible permutation of affixes
                            attached to its beginning or end, as determined by flag
    Functionality: Permutes the list of affixes and attaches each permutation onto stem, resulting
                         in a list of all possible words
    Tables accessed: none
    Tables modified: none
    """
    if len(affixes) == 0:       # if there are no affixes
        # we shouldn't be here, so print an error
        print "Error: We shouldn't call glue_on_affixes if we have no affixes."
        sys.exit()              # and exit

    affix_perms = permute_helper(affixes)   # put those affixes into every possible order

    words = []                                          # initialize output list of affixed stem
    
    # If we're attaching suffixes, take each possible order of them
    # and add them onto the end of the stem in that order.
    if flag == "suf":
        for ap in affix_perms:                   # for each permutation of the affixes
            w = stem                                # initialize w to be stem
            for a in ap:                              # for every affix in the permutation...
                w += a                               # ...put it on the end of the built-up word
            words.append(w)                     # put affixed stem into output list of words

    # If we're attaching prefixes, take each possible order of them
    # and add them onto the beginning of the stem, in that order.
    if flag == "pre":
        for ap in affix_perms:                  # for each permutation of the affixes   
            w = ''                                     # initialize w to be empty string
            for a in ap:                              # for every affix in permutation
                w += a                              # tag the prefix on to current built up word
            w += stem                             # put the stem on last
            words.append(w)                     # put the affixed stem into output list of word

    return words                                    # return output

#######################################################################
# get_harvester_strings_to_update(osp_id): given a user-supplied
# osp_id or 'a', return the list of harvester strings to process.
# harvester strings are tuples of [string,mrs_tag,osp_id]

def get_harvester_strings_to_update(osp_id, conn):
    """
    Function: get_harvester_strings_to_update
    Input:
        osp_id - the id of an original source profile for which we want to add permutations.
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: harvs - a list of lists.  The inner lists have a length of three and include the harvester
                          string, its mrs tag, and its original source profile id.
    Functionality: Gets all the harvester strings and their mrs tags for a given original source
                         profile id or for all source profiles
    Tables accessed: harv_str
    Tables modified: none
    """

    harvs = []                          # initialize output list of harvester strings

    try:
        # if the user's input was a string, try to convert it to an int
        osp_id = int(osp_id)
    except ValueError:
        if osp_id != 'a': # but if they sent in 'a', it won't work, so don't worry about it.
            # but if it was a non-int and non-a string, raise an error.
            raise ValueError, "Invald osp_id given to get_harvester_strings_to_update"
    

    # if it was an int (or an int-as-string that got converted above) they sent in...
    if type(osp_id) == int:
        # ...get the rows of harvester strings most recently imported by  that original source profile
        harv_tuples = conn.selQuery("SELECT hs_string, hs_mrs_tag, hs_cur_osp_id " + \
                                                   "FROM harv_str " + \
                                                  "WHERE hs_cur_osp_id = %s", (osp_id))
    # but if they want to add permutes for all harvester strings...
    # TODO: this 'a' functionality hasn't been tested, here or throughout the code
    elif osp_id == 'a':
        # ...get the rows for every harvester string in the database
        harv_tuples = conn.selQuery("SELECT hs_string, hs_mrs_tag,hs_cur_osp_id " + \
                                                   "FROM harv_str",())
    else:
        # I can't imagine I would be here given the checking I do up above, but I'll leave this in just
        # in case
        raise ValueError, "Invalid osp_id given to get_harvester_strings_to_update, msg 2"

    # if there were no harvester strings most recently imported by that profile id...
    if len(harv_tuples) == 0: 
        raise ValueError, "No harvester strings in MatrixTDB for that osp_id."  # ...raise an error
        
    for harv_tup in harv_tuples:                                        # for each row of harvester string
        (hs_string,hs_mrs_tag, row_osp_id) = harv_tup       # assign columns to variables
        harvs.append([hs_string, hs_mrs_tag, row_osp_id]) # append variables to output

    return harvs                                                            # return output

#######################################################################
# insert_item(instring, length, osp_id, mrs_tag)
#   Insert an item into the 'item' table, along with the corresponding
#   entries in the 'parse' and 'result' tables.

def insert_item(instring, osp_id, mrs_tag, conn):
    """
    Function: insert_item
    Input:
        instring - a string permutation to be added to the database
        length - the number of words/tokens in instring
        osp_id - the source profile id of the harvester string that created instrng
        mrs_tag - the semantic tag for instring
    Output: i_id - the unique id for instring inserted as item in the database
    Functionality: inserts a string into the item_tsdb, parse, and result tables
    Tables accessed: item_tsdb, parse, result
    Tables modified: item_tsdb, parse, result
    """

    length = len(instring.split())        # calculate num tokens in string

    # insert into item_tsdb table
    conn.execute("INSERT INTO item_tsdb " + \
                     "SET i_input = %s, i_length = %s, i_osp_id = %s, i_author = %s",
                                                                        (instring, length, osp_id, "add_permutes.py"))
    i_id = conn.selQuery("SELECT LAST_INSERT_ID()")[0][0]       # get its unique item id

    # insert into parse table
    conn.execute("INSERT INTO parse SET p_i_id = %s, p_readings = 1, p_osp_id = %s",
                                                                                                                        (i_id, osp_id))

    # insert into result table
    p_parse_id = conn.selQuery("SELECT LAST_INSERT_ID()")[0][0]           
    conn.execute("INSERT INTO result SET r_parse_id = %s, r_mrs = %s, r_osp_id = %s",
                                                                                                (p_parse_id, mrs_tag, osp_id))

    return i_id             # return output

def insert_many_items(stringList, osp_id, conn):
    """
    Function: insert_many_items
    Input:
        stringList - a list of (string, mrsTag) tuples that need to be inserted into item_tsdb, parse,
                         and result
        osp_id - the original source profile these strings have their genesis in
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: like insert_item (except does for many string/mrs pairs, not just one), inserts a
                         set of string/mrs pairs into the item_tsdb, parse, and result tables
    History: This was written on 8/10/09 for the purpose of making add_permutes go faster.  It's
                 reduced the adding permutes of neg1 and ques1 from 30 hours to 15 minutes.
    Tables accessed: item_tsdb, parse, result
    Tables modified: item_tsdb, parse, result
    """
    # because I call this on x mod 10000 == 0, I call this with lists of length 0
    # and because I start by inserting just the first one, check to see there are actually items
    # in the list
    if len(stringList) > 0:
        # lock the three relevant tables from other people writing to them while this function is
        # working with them
        conn.execute("LOCK TABLES item_tsdb WRITE, parse WRITE, result WRITE")

        # since we may execute several inserts, group as a transaction so they can be rolled
        # back should there be a problem like a disconnect in this function
        conn.execute("START TRANSACTION")
        instring = stringList[0][0]             # extract the string from the tuple
        mrs_tag = stringList[0][1]            # extract the mrs tag from the tuple
        initID = insert_item(instring, osp_id, mrs_tag, conn) # insert first string to get a starting ID

        # set the next ID for parse and result to the id just created so it gets incremented in loop
        # below
        nextID = initID
        itemVals = ''                               # initialize VALUES clause for inserts into item_tsdb
        parseVals = ''                             # initialize VALUES clause for inserts into parse
        resultVals = ''                             # initialize VALUES clause for inserts into result

        # string that ends every set of values for inserts into item_tsdb        
        itemEndAll = ',' + str(osp_id) + ",'add_permutes.py'),"

        # string that ends every set of values for inserts into parse
        parseEndAll = ",1,"+ str(osp_id) + ")," 

        # string that ends every set of values for inserts into parse
        resultEndAll = "'," + str(osp_id) + "),"

        for stringMrsTuple in stringList[1:]:       # for the second thru last string/mrs pair sent in...
            instring = stringMrsTuple[0]             # ...get the string...
            mrs_tag = stringMrsTuple[1]            # ...and the mrs tag

            # increment the parse and result ID to what the next item id will be set to
            nextID += 1                                    
            length = len(instring.split())              # get number of words in string

            # add this pair's values to the VALUES clause of the item_tsdb INSERT statement
            itemVals = itemVals + "('" + instring + "'," + str(length) + itemEndAll

            # add this pair's values to the VALUES clause of the parse INSERT statement
            parseVals = parseVals + "(" + str(nextID) + parseEndAll

            # add this pair's values to the VALUES clause of the result INSERT statement
            resultVals = resultVals + "("+ str(nextID) + ",'" + mrs_tag + resultEndAll

        # strip off the last comma from the VALUES clause of the item_tsdb INSERT statement
        itemVals = itemVals[:-1]

        # strip off the last comma from the VALUES clause of the item_tsdb INSERT statement
        parseVals = parseVals[:-1]

        # strip off the last comma from the VALUES clause of the item_tsdb INSERT statement
        resultVals = resultVals[:-1]

        # excute insert into item_tsdb        
        conn.execute('INSERT INTO item_tsdb (i_input, i_length, i_osp_id, i_author) ' + \
                             'VALUES ' + itemVals)

        # excute insert into parse        
        conn.execute('INSERT INTO parse (p_i_id, p_readings, p_osp_id) ' + \
                             'VALUES ' + parseVals)

        # excute insert into result        
        conn.execute('INSERT INTO result (r_parse_id, r_mrs, r_osp_id) ' + \
                             'VALUES ' + resultVals)

        conn.execute("COMMIT")                  # commit all inserts
        conn.execute("UNLOCK TABLES")    # release lock
    else:       # if there are no items in the list...
        pass   # ...just get out of here

    return        

#######################################################################
# Main program

# Ask the user for a specific osp_id to work from, or 'all'.
# If specific osp_id, just look for harvester strings belonging to that
# osp_id.

def main(osp_id, conn):
    """
    Function: main
    Input:
        osp_id - the id of an original source profile in MatrixTDB
        conn - a MatrixTDBConn, a connection to the MatrixTDB database    
    Output: none
    Functionality: generates every possible permutation of every possible seed string for the
                         harvester strings in the input source profile and inserts them into the
                         MatrixTDB database
    Tables Accessed: seed_str, str_lst, item_tsdb, parse, result, harv_str
    Tables Updated: seed_str, str_lst, item_tsdb, parse, result
    History:
        9/8/09 - started changing code not to really add all permutes...just those that pass all
                    universal filters
    """
    # get the osp's harvester strings and mrs tags
    harv = get_harvester_strings_to_update(osp_id, conn)

    for h in harv:                                                      # for every harvester string
        [hs, mrs_tag, osp_id] = h                               # unpack its variables

        # run stringmods, find all new seed strings for that harvester, and update database with
        # those seed strings and link them to mrs_tag in db.  new_strings is a list of triples of
        # words, prefixes and affixes
        new_strings = process_harvester(hs, mrs_tag, conn)

        # for every seed string we either hadn't seen before or hadn't seen paired with mrs_tag
        for s in new_strings:
            perms = uniq_permute(s)     # Get all permutations of the string
            print >> sys.stderr, "Processing", len(perms), "permutations of", s       # for monitoring
            passAllList = []                # initialize list of string/mrs pairs that pass all universal filters

            for p in perms:                         # for every permutation
                instring = ''                   # initialize instring to empty string
                
                for w in p:                     # for every word in permutation 
                    instring += w           # tack it...
                    instring += ' '             # ...and a space onto instring

                instring = instring[:-1]    # strip off final space

                # if the string/tag combo passes all universal filters...
                if runUnivFltrs(instring, mrs_tag):

                    # ...add this string/mrs pair to list to be inserted                    
                    passAllList.append((instring, mrs_tag))

            # print monitoring message of how many permutes passed all universal filters
            print >> sys.stderr, 'inserting', len(passAllList), 'items that passed all universal filters'

            # insert the parse/result/item for those items that pass all universal filters
            insert_many_items(passAllList, osp_id, conn)
            passAllList = []                    # and reset the list of items that passed all univs to empty

### KEN commenting out for now.  See e-mail exchange with Emily on 8/18 for details.
### Basically, it causes processing problems for generate_s_profile.
### Should add back in when ready to debug...but for now just want non-coordinating harv
### grammar to work.
### Note: when adding this code back, integrate it with new strategy only to insert those that
### pass all universal filters.
#                for pp in post_permutes:                # for every possible post_permute
#                    if mrs_tag == pp.mrs_id:            # if it applies to this harvester string's sem class
#
#                        # for every coordination that PostPermute produces...
#                        for s in pp.applyMe(instring):
#                            # add this string and its new mrs tag to list to be inserted
#                            stringList.append((s, pp.new_mrs_id))
### END KEN commenting out post-permutes code

    return

def runUnivFltrs(string, mrs):
    """
    Function: runUnivFltrs
    Input:
        string - the string to run the filters on
        mrs - the mrs tag of the string
    Output: answer - True if the string/mrs combo passes all universal filters.  False otherwise.
    Functionality: runs string/mrs combo through universal filters until one fails to see if we have
                         a string/mrs combo that passes all universal filters
    Tables accessed: None
    Tables modified: None
    """

    for f in ufilter_list:                           # for each universal filter
        if mrs in f.mrs_id_list:                # if that filter applies to this mrs tag
            if f.apply_filter(string) == 0:    # apply the filter.  If the result is 0 for fail...
                answer = False                 # ...set the output to Flase
                break                               # and stop looking...we don't have one that passes all
    else:                                           # if we never get one that fails...
        answer = True                         # ...set output to True

    return answer                               # return output

# set to true for running on my machine.  set to False before commiting to repository.
moduleTest = False

if __name__ == "__main__":      # only run if run as main module...not if imported
    try:
        osp_id = sys.argv[1]        # get the osp from the command line
    except IndexError:               # if the user didn't give it...

        # ...throw an error message indicating how to call the function
        print >> sys.stderr, 'Usage: python add_permutes.py osp_id [username] [password]'
        sys.exit()                       # and exit

    try:                                        # try to get...
        username = sys.argv[2]       # ...username...
        password = sys.argv[3]       # ...and password off of command line

        # if successful, create a connection using that username and password
        myconn = MatrixTDBConn('2', username, password)

    except IndexError:                               # if no username and/or password supplied...
        myconn = MatrixTDBConn('2')           # connect to MySQL server and prompt for them
        
    main(osp_id, myconn)                         # run the main function
    myconn.close()                                   # close the connection to the database
elif moduleTest:                        # or if i'm testing, run it on MatrixTDB2
    myconn = MatrixTDBConn('2')       # connect to MySQL server
    # ... and notify the user moduleTest is set to True.
    print >> sys.stderr, "Note: module testing turned on in add_permutes.py.  " + \
                                 "Unless testing locally, set moduleTest to False."
    
    # get the id of the original source profile the user wants to add permutes for.  This is given to
    # you by import_from_itsdb.
    osp_id = raw_input("Please input original source profile id (osp_id) for the source\n" + \
                                 "profile you're working with.  If you've updated the string\n" + \
                                  "modifications and wish to update seed strings for all harvester\n" + \
                                  "strings, enter 'a' ")

    main(osp_id, myconn)                              # run the main function
    myconn.close()                             # close connection to db

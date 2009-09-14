"""
File: generate_s_profile.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim)
Date: 6/24/09
Project: MatrixTDB RA, summer 2009
Owner: Emily M. Bender
Contents:
    - definitions of queries that built to find all grammatical items in MatrixTDB given a language
      type ID and the IDs of items that failed exactly one specific filter relevant to a given language
      type ID
    - main function that gets all grammatical items for a language type id and generates a profile
      including them and a selective sampling of ungrammatical items
    - getGrammItems function that generates a set of all items that passed all relevant filters for a
      language type             
    - genProfile function that generates a tsdb++ profile at path from itemIds
    - copyProfile - function that makes a copy of a [incr_tsdb()] profile
    - getReadings - function that converts a set of item IDs, which may correspond to identical
                           strings, to a dict where the string gives us the IDs for that string
    - getFailOneItems - function that creates a set of itemIDs that failed exactly one specific filter.
                                 Ensures that the item's string doesn't have a grammatical reading under a
                                 different semantic pairing.  Tries to represent as many filters as possible,
                                 but makes sure each filter is represnted at most once
    - genParseFile - function that creates the parse file in an [incr_tsdb()] profile
    - genItemFile - function that creates the item file in an [incr_tsdb()] profile
    - genResultFile - function that creates the result file in an [incr_tsdb()] profile
    - genEmptyFile -function that creates an empty file in an [incr_tsdb()] profile
    - genRelationsFile - function that creates the relations file in a [incr_tsdb()] profile
    - genFile - obsolete function that generates a file in a incr_tsdb() profile, but does item,
                    parse, and result in a naive, incorrect maner
    - genItemList - obsolete function that writes the items from item_tsdb whose i_id column
                          matches in idset to a file.
    - readInIds - function that reads in a stored set of IDs to save time when testing.  It is not a
      production function
    - __main__ code to get lt id from command line and call main on it
    - test code meant only for testing this module on my PC
Tables accessed: item_tsdb, parse, result, res_fltr, res_sfltr, fltr_feat_grp, lt_feat_grp
Tables modified: none
"""
###############################################################
# Script to generate [incr tsdb()] profile with gold-standard
# for some language type on the basis of MatrixTDB and a lt_id.

# Usage: python generate_s_profile.py <lt_id>

import sys, os, shutil, db_utils
from matrix_tdb_conn import MatrixTDBConn
from relations_def import relFileContents

# this query could be used to select all items.  However, with 20.7 million rows in the database,
# the query runs for over 25 minutes and uses up all the RAM on my machine, so I'm not using it
# now.  Note that if we can count on all item IDs being sequential, one way of doing this could
# be to use Python's iterator functionality to give it a range and create the IDs as necessary.
# Even without counting on that, we could give it the lowest and highest, have it generate a range
# and then deal with queries that return no items
# NOTE: Abandoned on 9/6/09 when I had 8.7 rows in item_tsdb and things got too slow
# NOTE: Resurrectec on 9/14/09 when we changed add_permutes to add only items that passed
# all universal filter to item_tsdb/parse/result
queryItemIDs="""SELECT i_id
                         FROM item_tsdb;"""

# this query was intended to generate all items that failed any universal filter, and that set could
# then be subtracted from all items in order to generate the list of items that passed all u filters.
# however due to the amount of time and memory needed to get all item IDs, this is not being
# used currently
# NOTE: Abandoned on 9/6/09 when I had 8.7 rows in parse and result and things got too slow
queryItemsFailedUFltrs="""SELECT p.p_i_id
                                        FROM parse p
                                            INNER JOIN result r ON p.p_parse_id = r.r_parse_id
                                            INNER JOIN res_fltr rf ON r.r_parse_id = rf.rf_res_id
                                        WHERE rf.rf_value = 0"""

# a query to get all the result IDs that pass all universal filters
# TODO: can I speed this up by getting rid of the NOT IN clause and instead doing an OUTER
# JOIN where r_result_id is null?
# NB: This is not being used now thatadd_permutes is only inserting into item_tsdb/parse/result
# those items that pass all universal filters
queryItemsPassAllUnivs="""SELECT r_result_id
                                            FROM result
                                            WHERE r_result_id NOT IN
                                                    (SELECT rf_res_id
                                                        FROM res_fltr
                                                        WHERE rf_value = 0)"""

# This query generates all items that failed any specific filter.  The list of items returned from this
# is subtracted from the list of items that passed all universal filters in order to get a list of items
# that passed all filters.  The query needs to have the language type id and a semicolon added
# at the end
# NB: I don't do DISTINCT here because it takes the db longer than it does for python to convert
# it to a set which gives me the same result in the end.
queryItemsFailedSFltrs="""SELECT p_i_id FROM parse
                                            INNER JOIN result ON p_parse_id = r_parse_id
                                            INNER JOIN res_sfltr ON r_result_id = rsf_res_id
                                            INNER JOIN fltr_feat_grp ON rsf_sfltr_id = ffg_fltr_id
                                            INNER JOIN lt_feat_grp ON ffg_grp_id = lfg_grp_id
                                        WHERE rsf_value = 0
                                            AND lfg_lt_id = %s"""

# This query gives me a list of item IDs that failed exactly one specific filter
queryItemsFailOne="""SELECT p_i_id, rsf_sfltr_id FROM
                                    (SELECT p_i_id, rsf_sfltr_id, count(*) numFails FROM parse
                                     INNER JOIN result ON p_parse_id = r_parse_id
                                     INNER JOIN res_sfltr ON r_result_id = rsf_res_id
                                     INNER JOIN fltr_feat_grp ON rsf_sfltr_id = ffg_fltr_id
                                     INNER JOIN lt_feat_grp ON ffg_grp_id = lfg_grp_id
                                     WHERE rsf_value = 0 AND lfg_lt_id = %s
                                     GROUP BY p_i_id) subq
                                     WHERE numFails = 1"""

def main(lngTypeID, dbroot, profilename, conn):
    """
    Function: main
    Input:
        lngTypeID - the id of the language type for which we want to generate the tsdb++ profile
        dbroot - the directory to write the output [incr_tsdb()] profiles to
        profilename - the name to give to the output [incr_tsdb()] profile
        conn - a MatrixTDBConn, a connection to the MatrixTDB database        
    Output: none
    Functionality: Finds all grammatical items for a language type ID and generates a profile
                         for those items. TODO: update this docstring
    Tables accessed: item_tsdb, parse, result, res_fltr, res_sfltr, fltr_feat_grp, lt_feat_grp
    Tables modified: none
    TODO: set up something somwhere to generate profpath folders if they don't exist
    """
    # get a list of all items that pass all universal filters    
    selResults = conn.selQuery(queryItemIDs)
    upass = db_utils.selColumnToSet(selResults)   # ...and convert to a set of IDs

    # monitoring progress
    print >> sys.stderr, "found", len(upass), "items that passed all universal filters"

    # get itemIDs that also pass specific filters relevant to this language type
    passAll = getGrammItems(lngTypeID, upass, conn)

    # monitoring progress
    print >> sys.stderr, "found", len(passAll), "items that also passed all specific filters for " + \
                                  "language type"

    # get a dict that has all the strings that passed all relevant filters as the keys and a list of
    # the IDs that are that string as its values
    readingCounter = getReadings(lngTypeID, passAll, conn)

    # get a sampling of item IDs that fail exactly  one specific filter
    failOne = getFailOneItems(lngTypeID, readingCounter, conn)

    # monitoring progress
    print >> sys.stderr, "sampled", len(failOne), "items that failed exactly one specific filter " + \
                                  "relevant to this language type"

    # generate a profile with items that passed all relevant filters and a sampling of those that
    # failed exactly one specific filter
    genProfile(readingCounter, failOne, dbroot, profilename, conn)

    # create a copy of that profile so that its items can be processed in [incr_tsdb()] for
    # comparison
    copyProfile(dbroot, profilename, 'gold')

    return

def copyProfile(dbroot, profilename, identifier):
    """
    Function: copyProfile
    Input:
        dbroot - the directory where the [incr_tsdb()] profile to be copied is located
        profilename - the name of the profile to copy
        identifier - the string to append to the new profile's name to distinguish it
    Output: none
    Functionality: Makes a copy of a [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    """
    profPath = dbroot + profilename + '/'                   # get full path to files in original profile
    newProfPath = profPath[:-1] + identifier + '/'        # set up path to new profile

    # copy the files from the original profile to the new one
    shutil.copytree(profPath, newProfPath)
    
    return

def getReadings(ltID, passAll, conn):
    """
    Function: getReadings
    Input:
        ltID - the id of a language type
        passAll - a set of IDs of items that pass all filters relevant to language type ltID
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: stringsToIDs - a dict whose keys are the strings associated with the IDs in passAll
                                     and whose values are the IDs that correspond to each string
    Functionality: converts a set of item IDs, which may correspond to identical strings, to a dict
                         where the string gives us the IDs for that string
    Tables accessed: item_tsdb
    Tables modified: none
    """
    # what I want to do here is take the item IDs in passAll and create a dict going from
    # string to a list of IDs and then use that when creating the parse file

    stringsToIDs = {}               # initialize output dict

    for itemID in passAll:          # for every ID that passed all filters
        # ...get its string
        i_input = conn.selQuery("SELECT i_input FROM item_tsdb WHERE i_id  = %s",
                                                                                                                    (itemID))[0][0]
        try:
            stringsToIDs[i_input].append(itemID)        # add that ID to the string in the output dict
        except KeyError:                                        # unless we haven't add this string yet
            stringsToIDs[i_input] = [itemID]                # in which case we initialize as a list of that ID

    return stringsToIDs                                         # return output dict

def getGrammItems(ltID, upass, conn):
    """
    Function: getGrammItems
    Input:
        ltID - a language type ID
        upass - a set of item IDs that passed all universal filters
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: itemsPassedAllFltrs - a set of item IDs, each of which passed all universal filters and
                                                 all specific filters relevant to language type ltID
    Functionality: generates a set of all items that passed all relevant filters
    Tables accessed: parse, result, res_sfltr, fltr_feat_grp, lt_feat_grp
    Tables modified: none
    """

    # query database for all items that failed any specific filter for this language type...
    selResults = conn.selQuery(queryItemsFailedSFltrs, (ltID))
    itemsFailedSFltrs = db_utils.selColumnToSet(selResults)         # ...and convert to a set of IDs

    # monitoring progress
    print >> sys.stderr, "found", len(itemsFailedSFltrs), "items that failed specific filters " + \
                                  " to this language type"

    # get set of grammatical items  These are item/mrs pairings.  So while a sentence may be
    # ungrammatical when paired with some mrs tag, when represented as an item id here it is
    # an item/mrs pair that is grammatical for this language type.
    # TODO: there is still some work to be done (where?) where in [incr_tsdb()] you get two
    # results rows per parse row.  (Update: is there?  I think i handle that in main and getReadings
    # now)
    itemsPassedAllFltrs = upass.difference(itemsFailedSFltrs)

    return itemsPassedAllFltrs  # return the item IDs that passed all relevant filters

def getFailOneItems(lt_id, passAllStringsToIDs, conn):
    """
    Function: getFailOneItems
    Input:
        ltID - a language type ID
        passAllStringsToIDs - a dict whose keys are strings who passed all relevant features for
                                         language type in question and whose values are lists of item IDs
                                         that correspond those strings
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: unGrammItemIDs - a set of items that failed exactly one filter.  We try to get as many
                                            filters as possible represented as these failing filters and only
                                            want each filter represented once
    Functionality: creates a set of itemIDs that failed exactly one specific filter.  Ensures that
                         the item's string doesn't have a grammatical reading under a different semantic
                         pairing.  Tries to represent as many filters as possible, but makes sure each
                         filter is represnted at most once
    Tables accessed: parse, result, res_sfltr, fltr_feat_grp, lt_feat_grp, item_tsdb
    Tables modified: none
    """
    # keeps track of filters represented in set of ungrammatical items that failed exactly one filter
    # because if possible we want to just make sure each filter is represented once in that set
    usedFilters = set() 
    unGrammItemIDs = set()          # initialize output set of ungrammatical items to export

    # run a query to get a list of items that failed exactly one specific filter
    failOneRows = conn.selQuery(queryItemsFailOne, (lt_id))

    # monitoring progress
    print >> sys.stderr, "There are", len(failOneRows), "total items that failed exactly one " + \
                                  "specific filters relevant to this language type"

    for row in failOneRows:             # for every item that failed exactly one specific filter...
        itemID = row[0]                    # ...get the item ID
        filterID = row[1]                    # get the filter id

        # if I haven't added this filter to the list of filters that are represented in the list of
        # ungrammatical items to export....
        if filterID not in usedFilters:
            # ...then get the string for the failed item
            # TODO: I have the line below in another function.  Make into one function.
            # TODO: consider getting i_input from queryItemsFailOne
            i_input = conn.selQuery("SELECT i_input from item_tsdb WHERE i_id = %s",
                                                                                                                        (itemID))[0][0]
            # check to verify the string doesn't have a grammatical reading with a different semantics
            # pairing
            if not i_input in passAllStringsToIDs:

                usedFilters.add(filterID)       # if not, then add this filter to the list of filters represented
                unGrammItemIDs.add(itemID) # and add the item to the set of ungrammatical items
                # Note: I don't break here because I want to check all items that failed one.
                # I skip as early as possible with the if statement that checks filterID not in
                # usedFilters

    return unGrammItemIDs                                                                # return output

def genParseFile(passReadingCounter, failOne, profpath, conn):
    """
    Function: genParseFile
    Input:
        passReadingCounter - a dict whose keys are strings that passed all universal filters and
                                         whose values are the lists of IDs of item/mrs pairings that passed all
                                         relevant filters for a language type
        failOne - a set of itemIDs that failed exactly one filter.  As many filters as possible are
                     represented as these failing filters but each filter is represented at most once
        profpath - the full path of the [incr_tsdb()] profile where this parse file should be written
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: creates the parse file for a [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    """

    # going through readingCounter instead of through the passAll set ensures I don't write a string
    # twice when it has two readings.
    # in the case where a parse row has two result rows i think we're okay with just one row
    # in the parse and item tables, but want two (or multiple) in the result tables

    outlines = []               # initialize list of lines to go in output file
    
    for itemString in passReadingCounter.keys():    # for each string that passed all relevant filters
        # get the list of IDs for all semantic pairings that passed
        idList = passReadingCounter[itemString]
        firstID = idList[0]                     # we just need one ID for parse and item, so just tkae first
        numReadings = len(idList)       # get the number of grammatical readings

        # get the data for this row in parse table from MatrixTDB
        row = conn.selQuery("SELECT * FROM parse WHERE p_i_id = %s", (firstID))[0]
        rowstring = ''          # initalize string in output file representing that row

        for column in row[:3]:                      # for the first three columns in that row...
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'

        rowstring = rowstring + str(numReadings) + '@'      # put in number of readings

        # put in rest of columns except for last one, which is in MatrixTDB, but not [incr_tsdb()]
        for column in row[4:-1]:
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'
                
        rowstring = rowstring[:-1]      # strip off last @
        outlines.append(rowstring)    # add that output line to the list of outputlines

    # now do the fails
    for failID in failOne:                  # for each of the parses sampled that were ungrammatica.
        numReadings = 0               # set readings to 0

        # TODO: lots of repeat code here with pass loop...combine
        # get the data for this row in parse table from MatrixTDB        
        row = conn.selQuery("SELECT * FROM parse WHERE p_i_id = %s", (failID))[0]
        rowstring = ''          # initalize string in output file representing that row

        
        for column in row[:3]:                      # for first three columns...
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'

        rowstring = rowstring + str(numReadings) + '@'      # put in number of readings

        # put in rest of columns except for last one, which is in MatrixTDB, but not [incr_tsdb()]
        for column in row[4:-1]:
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'
                
        rowstring = rowstring[:-1]      # strip off last @
        outlines.append(rowstring)    # add that output line to the list of outputlines

        
    outfile = open(profpath+'parse', 'wb')  # after getting all items, open file to write to
    
    for line in outlines:                           # for each line of output...
        outfile.write(line + '\n')                  # ...write it to the file followed by newline
        
    outfile.close()                                  # close file

    return

def genItemFile(passReadingCounter, failOne, profpath, conn):
    """
    Function: genItemFile
    Input:
        passReadingCounter - a dict whose keys are strings that passed all universal filters and
                                         whose values are the lists of IDs of item/mrs pairings that passed all
                                         relevant filters for a language type
        failOne - a set of itemIDs that failed exactly one filter.  As many filters as possible are
                     represented as these failing filters but each filter is represented at most once
        profpath - the full path of the [incr_tsdb()] profile where this parse file should be written
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: creates the item file for a [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    TODO: there's an awful lot of overlap with genParseFile.  refactor.
    """
    outlines = []                                                       # initialize list of lines to go in output file    

    # for every string that passed all relevant filters...
    # (regardless of how many grammatical semantic pairings it has, we only want each string
    # printed once in this file and in the parse file)
    for itemString in passReadingCounter.keys():
        
        # get the list of item IDs that represent grammatical mrs pairings for that string
        idList = passReadingCounter[itemString]
        firstID = idList[0]         # we'll just take the first ID in that list as our profile's ID

        # get the row in the item_tsdb table in MatrixTDB for that ID
        row = conn.selQuery("SELECT * FROM item_tsdb WHERE i_id = %s", (firstID))[0]
        rowstring = ''          # initalize string in output file representing that row

        # for every column in that row except the last, which is not in [incr_tsdb()] item files...
        for column in row[:-1]:
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if it's numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'

        rowstring = rowstring[:-1]      # strip off last @
        outlines.append(rowstring)    # add that output line to the list of outputlines

    # now for every ID in our sampling of items that failed exactly one filter...
    for failID in failOne:
        # TODO: lots of repeat code here with pass loop...combine
        row = conn.selQuery("SELECT * FROM item_tsdb WHERE i_id = %s", (failID))[0]
        rowstring = ''          # initalize string in output file representing that row

        
        for column in row[:-1]:     # MatrixTDB keeps track of one row that [incr_tsdb()] doesn't
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'

        rowstring = rowstring[:-1]      # strip off last @
        outlines.append(rowstring)    # add that output line to the list of outputlines

        
    outfile = open(profpath+'item', 'wb')  # after getting all items, open file to write to
    
    for line in outlines:                           # for each line of output...
        outfile.write(line + '\n')                  # ...write it to the file followed by newline
        
    outfile.close()                                  # close file

    return

def genResultFile(passReadingCounter, failOne, profpath, conn):
    """
    Function: genResultFile
    Input:
        passReadingCounter - a dict whose keys are strings that passed all universal filters and
                                         whose values are the lists of IDs of item/mrs pairings that passed all
                                         relevant filters for a language type
        failOne - a set of itemIDs that failed exactly one filter.  As many filters as possible are
                     represented as these failing filters but each filter is represented at most once
        profpath - the full path of the [incr_tsdb()] profile where this parse file should be written
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: creates the parse file for a [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    TODO: there's an awful lot of overlap with genParseFile.  refactor.
    """
    outlines = []               # initialize list of lines to go in output file    

    # for every string that passed all relevant filters...
    for itemString in passReadingCounter.keys():

        # get the list of semantic pairings that were grammatical for it
        idList = passReadingCounter[itemString]

        # since I use the first ID as the parseID in genParseFile, use it here as my parseID
        parseID = idList[0] 

        # for every grammatical semantic pairing
        for resID in idList:

            # get the data for the output row from the database
            try: # debugging
                row = conn.selQuery("SELECT r.r_parse_id, r.r_result_id, r.r_time, r.r_ctasks, " + \
                                            "r.r_ftasks, r.r_etasks, r.r_stasks, r.r_size, r.r_aedges, " + \
                                            "r.r_pedges, r.r_derivation, r.r_surface, r.r_tree, " + \
                                            "m.mrs_value, r.r_flags " + \
                                            "FROM result r " + \
                                            "INNER JOIN parse p ON r.r_parse_id = p.p_parse_id " + \
                                            "INNER JOIN mrs m ON r.r_mrs = m.mrs_tag " + \
                                            "WHERE mrs_current = 1 AND p.p_i_id = %s", (resID))[0]
            except IndexError:
                print >> sys.stderr, "sel query in genResultFile returns no rows on resID", resID

            # initalize string in output file representing that row
            rowstring = str(parseID) + '@' + str(resID) + '@'

        
            for column in row[2:]:                  # for all but first two columns, which I do above
                try:
                    # ...add it to the output row...
                    rowstring = rowstring + column + '@'
                except TypeError:
                    # ...or if its numerical data, add it to the output row this way
                    rowstring = rowstring + str(column) + '@'

            rowstring = rowstring[:-1]      # strip off last @
            outlines.append(rowstring)    # add that output line to the list of outputlines

    outfile = open(profpath+'result', 'wb')  # after getting all items, open file to write to
    
    for line in outlines:                           # for each line of output...
        outfile.write(line + '\n')                  # ...write it to the file followed by newline
        
    outfile.close()                                  # close file

    return

def genEmptyFile(profpath, filename):
    """
    Function: genEmptyFile
    Input:
        profpath - the path of the [incr_tsdb()] profile we are creating
        filename - the name of the empty file to create
    Output: none
    Functionality: Creates an empty file in an [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    """
    # TODO: is there a better, quicker, way to do the next line?
    outfile = open(profpath+filename, 'wb')  #open file, in essence creating it
    outfile.close()                                # close file without writing anything to it

    return

def genFile(itemIds, profpath, conn, filename):
    """
    Function: genFile
    Input:
        itemIds - a set of item IDs
        profpath - the path to write the profile to
        conn - a MatrixTDBConn
        filename - the name of the file to generate
    Output: none
    Functionality: generates a file in a incr_tsdb() profile.
    Note: There is a case where one parse record can have multilpe reuslt records that I don't
              handle yet, either here or in the database.
    Note: This function is no longer used now that genItemFile, genParseFile, and genResultFile
             exist as more sophsicated (and correct functions)
    """
    numIrrelClms = 0
    if filename == "item":
        # ...set up query for item file
        query = "SELECT * FROM item_tsdb WHERE i_id = "
        numIrrelClms = 1                    # there is 1 irrelevant column in item_tsdb
# case below commented out due to existence of genParseFile now
#    elif filename == "parse":
        # ...set up query for parse file
#        query = "SELECT * FROM parse WHERE p_i_id = "
#        numIrrelClms = 1                    # there is 1 irrelevant column in parse
# TODO: comment out below as well when I've written that....
#    elif filename == "result":
        # ...set up query for result file.  We use the tag in result.r_mrs to get the actual semantics
        # stored in mrs table.  It will return multiple rows there unless you set mrs_current to 1
#        query = "SELECT r.r_parse_id, r.r_result_id, r.r_time, r.r_ctasks, r.r_ftasks, " + \
#                        "r.r_etasks, r.r_stasks, r.r_size, r.r_aedges, r.r_pedges, r.r_derivation, " + \
#                        "r.r_surface, r.r_tree, m.mrs_value, r.r_flags, r.r_wf " + \
#                    "FROM result r " + \
#                    "INNER JOIN parse p ON r.r_parse_id = p.p_parse_id " + \
#                    "INNER JOIN mrs m ON r.r_mrs = m.mrs_tag " + \
#                    "WHERE mrs_current = 1 AND p.p_i_id = "
        # there are 3 irrelevant columns in result, but I excluded all but one of them from the query
        # since I had to replace r_mrs with mrs_value from the mrs table anyway.  I would have
        # eliminated all of them and set this variable to 0, but then the slice row[:-numIrrelClms]
        # returns 0 columns so we have this hack instead.
#        numIrrelClms = 1    
    else:
        pass

    outlines = []               # initialize list of lines to go in output file

    # TODO: would it be faster to craft a line that was more like WHERE p.p_i_id = ...OR ... OR
    # and do one query rather than do all of these queries?
    
    for iid in itemIds:         # for each item in input...
        # ...get the relevant row from the relevant table
        row = conn.selQuery(query + str(iid) + ';')[0]
        rowstring = ''          # initalize string in output file representing that row

        # don't worry about the last, irrelevant columns, but for every relevant column in the row...
        for column in row[:-numIrrelClms]:   
            try:
                # ...add it to the output row...
                rowstring = rowstring + column + '@'
            except TypeError:
                # ...or if its numerical data, add it to the output row this way
                rowstring = rowstring + str(column) + '@'
                
        rowstring = rowstring[:-1]      # strip off last @
        outlines.append(rowstring)    # add that output line to the list of outputlines
        
    outfile = open(profpath+filename, 'wb')  # after getting all items, open file to write to
    
    for line in outlines:                           # for each line of output...
        outfile.write(line + '\n')                  # ...write it to the file followed by newline
        
    outfile.close()                                  # close file

    return

def genRelationsFile(path):
    """
    Function: genRelationsFile
    Input: path - the path to write the relations file to
    Output: none
    Functionality: creates the relations file in a [incr_tsdb()] profile
    Tables accessed: none
    Tables modified: none
    """
    outfile = open(path+'relations', 'wb')        # open the relations file

    # write its contents, imported from relations_def, formatted with correct newlines
    outfile.write(relFileContents)
    outfile.close()                                     # close the file
    
    return

def genProfile(readingCounter, failOne, dbroot, profilename, conn):
    """
    Function: genProfile
    Input:
        readingCounter - a dict whose keys are strings that passed all universal filters and whose
                                 values are the lists of IDs of item/mrs pairings that passed all universal
                                 filters
        failOne - a set of itemIDs that failed exactly one filter.  As many filters as possible are
                     represented as these failing filters but each filter is represented at most once
        dbroot - the directory in which the [incr_tsdb()] should be written
        profilename - the name of the [incr_tsb()] to be created
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: generates a [inr_tsdb()] profile given some items that passed all relevant filters
                         and some items that failed exactly one specific filter
    Tables accessed: none
    Tables modified: none
    """

    path = dbroot + profilename + '/'       # bulid path of output [incr_tsdb()] profile
    
    try:
        os.listdir(path)                                # check to see if path already exists
    except:
        os.makedirs(path)                           # if it doesn't, create it

    genItemFile(readingCounter, failOne, path, conn)         # generate the item file
    genParseFile(readingCounter, failOne, path, conn)       # generate the parse file    
    genResultFile(readingCounter, failOne, path, conn)      # generate the result file
    genRelationsFile(path)                                              # generate the relations file

    # a list of the files needed in a profile but whose contents we don't care about
    emptyProfileFiles = ['analysis', 'daughter', 'decision', 'edge', 'fold', 'item-phenomenon', 'item-set',
                                  'output', 'parameter', 'phenomenon', 'preference', 'rule', 'run', 'score', 'set',
                                  'tree', 'update']

    for fname in emptyProfileFiles:         # for each profile file we don't care about...
        genEmptyFile(path, fname)          # ...create it as an empty file

    return

def readInIds(idfile):
    """
    Function: readInIds
    Input: idfile - the file containing one item ID per line
    Output: a set of the IDs in idfile
    Functionality: reads in a stored set of IDs to save time when testing.  This is not a production
                         function: it only serves to get around running the 20 minute query that finds all
                         items that didn't fail any universal filters
    Note: This is basically throwaway code.
    Tables accessed: none
    Tables modified: none
    """
    infile = open(idfile)           # open the file
    lines = infile.readlines()    # put its lines in a list
    infile.close()                      # close the file

    # remove newlines from lines, turn each into a long, and create a set from a list
    # comprehensionof those
    idset = set([long(line.strip()) for line in lines])
    return idset                    # return that set of IDs

def genItemList(readingCounter, failOne, itemListName, conn):
    """
    Function: genItemList
    Input:
        idset - the i_ids of the items to put in the item list
        itemListName - the filename of the item list
        conn - a MatrixTDBConn, a connection to the MatrixTDB database
    Output: none
    Functionality: writes the items from item_tsdb whose i_id column matches in idset to a file.
                         Just writes the i_input column, the actual item string.  This is used to import
                         as a list of items into [incr_tsdb()] so it can process them and it can be used
                         as the gold standard to compare the profile generated by genProfile to.
    Tables accessed: item_tsdb
    Tables modified: none
    NOTE: i'm no longer using this now that I'm copying the orig profile with copyProfile
    """
    outfile = open(itemListName, 'wb')               # open output file

    for itemString in readingCounter.keys():                  # TODO: comment this function
        outfile.write(itemString + '\n')                         # write the item's string as a line to output file

    # now do the fails
    for failID in failOne:
        # TODO: lots of repeat code here with pass loop...combine
        item = conn.selQuery("SELECT i_input FROM item_tsdb WHERE i_id = %s", (failID))[0][0]
        outfile.write(item + '\n')                          # write the item's string as a line to output file        

    outfile.close()                                           # close output file

    return

# set to true for running on windows on my machine.  set to False before commiting to repository.
moduleTest = False 

if __name__ == '__main__':      # only run if run as main module...not if imported
    try:
        lt_id = sys.argv[1]         # get the language type id from the command line
        profDBroot = sys.argv[2]     # get the db root from the command line
        profilename = sys.argv[3]  # get the name of the profile to generate
    except IndexError:              # if they didn't give a command line argument...
         # ...give user usage info
        print >> sys.stderr, "Usage: python generate_s_profile.py language_type_id dbroot " + \
                                                                                                                      "profileName"
    try:                                        # try to get...
        username = sys.argv[4]       # ...username...
        password = sys.argv[5]       # ...and password off of command line

        # if successful, create a connection using that username and password
        myconn = MatrixTDBConn('2', username, password)

    except IndexError:                               # if no username and/or password supplied...
        myconn = MatrixTDBConn('2')           # connect to MySQL server and prompt for them

    # run main on that language type id to generate profile        
    main(lt_id, profDBroot, profilename, myconn)
    myconn.close()                              # close connection to MySQL database
# the code following here only exists for testing on my local PC.  It can be ignored.
elif moduleTest:
    print >>sys.stderr, "Warning: moduleTest on for generate_s_profile"
    lt_id = raw_input("Enter language type id:\n")
    profDBroot = raw_input("Enter full path of db root, ending in '/':\n")
    profilename = raw_input("Enter name of profile to generate:\n")
    myconn = MatrixTDBConn('2')    
    main(lt_id, profDBroot, profilename, myconn)
    # TODO: set up code to create directories if they don't exist
    myconn.close()                                          # TODO: close connections throughout code

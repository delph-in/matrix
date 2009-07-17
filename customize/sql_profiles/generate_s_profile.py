"""
File: generate_s_profile.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim)
Date: 6/24/09
Project: MatrixTDB RA, summer 2009
Owner: Emily M. Bender
Contents:
    - definitions of queries that built to find all grammatical items in MatrixTDB given a language
      type ID
    - main function that gets all grammatical items for a language type id and generates a profile
      including them
    - getGrammItems function that generates a set of all items that passed all relevant filters for a
      language type             
    - genProfile function that generates a tsdb++ profile at path from itemIds
    - genFile function that generates a file in a incr_tsdb() profile.
    - readInIds function that reads in a stored set of IDs to save time when testing.  It is not a
      production function
    - __main__ code to get lt id from command line and call main on it
    - test code meant only for testing this module on my PC
"""
###############################################################
# Script to generate [incr tsdb()] profile with gold-standard
# for some language type on the basis of MatrixTDB and a lt_id.

# Usage: python generate_s_profile.py <lt_id>

import sys
import db_utils
from matrix_tdb_conn import MatrixTDBConn
from relations_def import relFileContents

# this query could be used to select all items.  However, with 20.7 million rows in the database,
# the query runs for over 25 minutes and uses up all the RAM on my machine, so I'm not using it
# now.  Note that if we can count on all item IDs being sequential, one way of doing this could
# be to use Python's iterator functionality to give it a range and create the IDs as necessary.
# Even without counting on that, we could give it the lowest and highest, have it generate a range
# and then deal with queries that return no items
queryItemIDs="""SELECT i_id
                         FROM item_tsdb;"""

# this query was intended to generate all items that failed any universal filter, and that set could
# then be subtracted from all items in order to generate the list of items that passed all u filters.
# however due to the amount of time and memory needed to get all item IDs, this is not being
# used currently
queryItemsFailedUFltrs="""SELECT p.p_i_id
                                        FROM parse p, result r, res_fltr rf
                                        WHERE rf.rf_value = 0
                                        AND rf.rf_res_id = r.r_result_id
                                        AND r.r_parse_id = p.p_parse_id;"""

# This query generates all item IDs where the item passed all universal filters.  It takes between
# 19-24 minutes to run currently on MatrixTDB.
# TODO: this query could be made better by changing last two things in where clause to
# inner joins
queryItemsPassedUFltrs="""SELECT i_id FROM item_tsdb
                                          WHERE i_id NOT IN
                                          (SELECT p.p_i_id from parse p, result r, res_fltr rf
                                          WHERE rf.rf_value=0
                                          AND rf.rf_res_id=r.r_result_id
                                          AND r.r_parse_id=p.p_parse_id);"""

# This query generates all items that failed any specific filter.  The list of items returned from this
# is subtracted from the list of items that passed all universal filters in order to get a list of items
# that passed all filters.  The query needs to have the language type id and a semicolon added
# at the end
# TODO: this query could be improved by changing some conditions to inner joins
# NB: I don't do DISTINCT here because it takes the db longer than it does for python to convert
# it to a set which gives me the same result in the end.
queryItemsFailedSFltrs="""SELECT p_i_id FROM parse
                                            INNER JOIN result ON p_parse_id = r_parse_id
                                            INNER JOIN res_sfltr ON r_result_id = rsf_res_id
                                            INNER JOIN fltr_feat_grp ON rsf_sfltr_id = ffg_fltr_id
                                            INNER JOIN lt_feat_grp ON ffg_grp_id = lfg_grp_id
                                        WHERE rsf_value = 0
                                            AND lfg_lt_id = %s"""

def main(lngTypeID, profpath):
    """
    Function: main
    Input:
        lngTypeID - the id of the language type for which we want to generate the tsdb++ profile
        profpath - the path where the profile should be written
    Output: none
    Functionality: Finds all grammatical items for a language type ID and generates a profile
                         for those items.
    TODO: Selectively sample ungrammatical items for inclusion in profile as well.
    TODO: set up something somewhere to generate profpath folders if they don't exist
    """
    grammaticalItemIDs = getGrammItems(lngTypeID)
    genProfile(grammaticalItemIDs, profpath)

    return

def getGrammItems(ltID, conn):
    """
    Function: getGrammItems
    Input:
        ltID - a language type ID
        conn - a MatrixTDBConn
    Output: itemsPassedAllFltrs - a set of item IDs, each of which passed all universal filters and
                                                 all specific filters relevant to language type ltID
    Functionality: generates a set of all items that passed all relevant filters
    """
    # query database for all items that passed all universal filters...
    selResults = conn.selQuery(queryItemsPassedUFltrs)
    itemsPassedUFltrs = db_utils.selColumnToSet(selResults)   # ...and convert to a set of IDs

    # query database for all items that failed any specific filter for this language type...
    selResults = conn.selQuery(queryItemsFailedSFltrs, (ltID))
    itemsFailedSFltrs = db_utils.selColumnToSet(selResults)         # ...and convert to a set of IDs

    # get set of grammatical items
    itemsPassedAllFltrs = itemsPassedUFltrs.difference(itemsFailedSFltrs)

    return itemsPassedAllFltrs  # return the item IDs that passed all filters


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
    """
    numIrrelClms = 0
    if filename == "item":
        # ...set up query for item file
        query = "SELECT * FROM item_tsdb WHERE i_id = "
        numIrrelClms = 1                    # there is 1 irrelevant column in item_tsdb
    elif filename == "parse":
        # ...set up query for parse file
        query = "SELECT * FROM parse WHERE p_i_id = "
        numIrrelClms = 1                    # there is 1 irrelevant column in parse
    elif filename == "result":
        # ...set up query for result file
        query = "SELECT r.* FROM result r " + \
                    "JOIN parse p " + \
                    "ON r.r_parse_id = p.p_parse_id " + \
                    "WHERE p.p_i_id = "
        numIrrelClms = 3                    # there are 3 irrelevant columns in result
    else:
        pass

    outlines = []               # initialize list of lines to go in output file
    
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
        outlines.append(rowstring)    # add that output line to the list of otuputlines
        
    outfile = open(profpath+filename, 'w')  # after getting all items, open file to write to
    
    for line in outlines:                           # for each line of output...
        outfile.write(line + '\n')                  # ...write it to the file followed by newline
        
    outfile.close()                                  # close file

    return

def genRelationsFile(path):
    """
    Function: genRelationsFile
    Input: path - the path to write the relations file to
    Output: none
    Functionality: create the relations file in a [incr_tsdb()] profile
    """
    outfile = open(path+'relations')        # open the relations file
    outfile.write(relFileContents)           # write its contents, imported from relations_def
    outfile.close()                                # close the file
    
    return

def genProfile(itemIds, path, conn):
    """
    Function: genProfile
    Input:
        itemIDs - a set of item IDs
        path - the path to write the profile to
        conn - a MatrixTDBConn
    Output: none
    Functionality: generates a tsdb++ profile at path from itemIds
    """
    genFile(itemIds, path, conn, "item")       # generate the item file
    genFile(itemIds, path, conn, "parse")     # generate the parse file
    genFile(itemIds, path, conn, "result")     # generate the result file

    emptyset = set()
    genFile(emptyset, path, conn, "analysis")
    genFile(emptyset, path, conn, "daugther")
    genFile(emptyset, path, conn, "decision")
    genFile(emptyset, path, conn, "edge")
    genFile(emptyset, path, conn, "fold")
    genFile(emptyset, path, conn, "item-phenomenon")
    genFile(emptyset, path, conn, "item-set")
    genFile(emptyset, path, conn, "output")
    genFile(emptyset, path, conn, "parameter")
    genFile(emptyset, path, conn, "phenomenon")
    genFile(emptyset, path, conn, "preference")
    genRelationsFile(path)
    genFile(emptyset, path, conn, "rule")
    genFile(emptyset, path, conn, "run")
    genFile(emptyset, path, conn, "score")
    genFile(emptyset, path, conn, "set")
    genFile(emptyset, path, conn, "tree")
    genFile(emptyset, path, conn, "update")
    return

def readInIds(idfile):
    """
    Function: readInIds
    Input: idfile - the file containing one item ID per line
    Output: a set of the IDs in idfile
    Functionality: reads in a stored set of IDs to save time when testing.  This is not a production
                         function: it only serves to get around running the 20 minute query that finds all
                         items that didn't fail any universal filters
    """
    infile = open(idfile)           # open the file
    lines = infile.readlines()    # put its lines in a list
    infile.close()                      # close the file

    # remove newlines from lines, turn each into a long, and create a set from a list
    # comprehensionof those
    idset = set([long(line.strip()) for line in lines])
    return idset                    # return that set of IDs

def genItemList(idset, itemListName, conn):
    outfile = open(itemListName, 'w')
    for iid in idset:
        item = conn.selQuery("SELECT i_input " + \
                                         "FROM item_tsdb " + \
                                         "WHERE i_id = %s", (iid))[0][0]
        outfile.write(item + '\n')
    outfile.close()
    return

# set to true for running on my machine.  set to False before commiting to repository.
moduleTest = True 

if __name__ == '__main__':      # only run if run as main module...not if imported
    try:
        lt_id = (sys.argv[1])         # get the language type id from the command line
        main(lt_id)                      # run main on that language type id to generate profile
    except IndexError:              # if they didn't give a command line argument...
        print "Usage: python generate_s_profile.py language_type_id" # ...give user usage info
# the code following here only exists for testing on my local PC.  It can be ignored/
elif moduleTest:                    
    print >>sys.stderr, "Warning: moduleTest on for generate_s_profile"
    lt_id = 21
    #main(lt_id)
    # TODO: set something up to create directories if they don't exist
    sqlprofpath = "C:\\RA\matrix\customize\sql_profiles\\"
    profilepath = sqlprofpath + "testoutprofile\\profile20090712\\"
    itemListName = sqlprofpath + "itemlist20090712"
    #grammItemIDsLT1 = sqlprofpath + "15grammItemsLT1.txt"
    myconn = MatrixTDBConn('2')
    #idset = readInIds(grammItemIDsLT1)
    print >> sys.stderr, "getting grammatical items"
    idset = getGrammItems(lt_id, myconn)
    #print >> sys.stderr, "generating profile"
    #genProfile(idset, profilepath, myconn)
    #genItemList(idset, itemListName, myconn)    # TODO: do i need to add this to main?
    #myconn.close()                                          # TODO: close connections throughout code

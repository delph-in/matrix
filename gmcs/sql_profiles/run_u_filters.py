#!/usr/local/bin/python2.5
"""
File: run_u_filters.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents: (listed, when possible, in order of execution)
    main code that creates appropriate connection to database and calls main()
    main - Asks the user if the want to replace the result of some filters or if they want to run
             filters on new results that they've imported.  The replace case is not working.  In the
             add case, it gets an osp_id then calls add_to_res_fltr to record the first fail of every
             result in that original source profile in res_fltr.
    find_filter_from_name - looks through filter_list for a Filter with name n and returns it
    update_res_fltr - Doesn't work.  See function doco below.
    make_string - Doesn't work.  See function doco below.
    add_to_res_fltr - Gathers all [incr_tsdb()] results from an original source profile (osp_id) and
                             runs each filter on each result, recording the "result" of the first fail on every
                             string in res_fltr.  Also informs the user how many items failed a filter and
                             how many passed all filters.
    insertManyUnivFails - function that inserts several universal filter rails into res_fltr at a time.
Tables Accessed: result, filter, fltr_mrs, res_fltr, parse, item_tsdb
Tables Modified: filter, fltr_mrs, res_fltr
Note: The case in main where a user wants to replace results for certain filters is untested an
         known not to work.  See comments in main and update_res_fltr for more details.
"""

import filters, db_utils
import sys, datetime, MySQLdb
from u_filters import filter_list
from matrix_tdb_conn import MatrixTDBConn
##########################################################################
# find_filter_from_name(n, filter_list)

def find_filter_from_name(n, filter_list):
    """
    Function: find_filter_from_name
    Input:
        n - the name of a filter
        filter_list - a list of Filters
    Output: answer - the first (ther3e should be only one) Filter in filter_list with name n
    Functionality: looks through filter_list for a Filter with name n and returns it
    Tables accessed: none
    Tables modified: none
    """
    for f in filter_list:       # for every filter
        if f.name == n:     # check its name...if it matches
            answer = f       # set the output to f
            break               # and quit the loop
    else:                       # if we go through entire loop without a match...
        # ...raise an error
        raise ValueError, "Unknown filter: " + n

    return answer           # return output

##########################################################################
# make string from list

def make_string(int_list, limit):
    """
    Function: make_string
    Input:
        int_list - a list of integers that are filter_ids
        limit - an integer, the size we want to limit the query to
    Output: output - a string that is the SQL query that ???
    Functionality: Generates a SQL query that ??
    Tables accessed: generates SQL query to access result and fltr_mrs
    Tables modified: none
    Note: KEN inherited this function.  It's not clear what it is supposed to do, and it doesn't work
             (at least due to it trying to access the item table, which doesn't exist.)
    """

    # For when we are only updating a fe filters:

    #    output = "SELECT r_result_id, r_mrs FROM result FORCE INDEX (PRIMARY), fltr_mrs WHERE r_mrs = fm_mrs_tag AND (fm_fltr_id = "
    #    l = len(int_list)
    #    e = l - 1
    #    for i in int_list[0:e]:
    #        output += str(i)
    #        output += ' or fm_fltr_id = '
    #    output += str(int_list[l-1])
    #    output += ") LIMIT "
    #    output += str(limit)
    #    output += ", 100000"

    output = "SELECT r_result_id, i_input, r_mrs " + \
             "FROM result FORCE INDEX (PRIMARY), item " + \
             "WHERE r_parse_id = i_id LIMIT " + str(limit) + ", 100000"

    return output

##########################################################################
# update_res_fltr(filter_list)

def update_res_fltr(filter_list, conn):
    """
    Function: update_res_fltr
    Input:
        filter_list - a list of Filters whose results we want to replace in res_fltr table
        conn - a MatrixTDBConn, a connection to the MatrixTDB database            
    Output:
    Functionality: ???
    Tables accessed: filter, fltr_mrs, res_fltr, result
    Tables modified: filter, fltr_mrs, res_fltr
    Note: KEN is currently unaware of what this inherited function is supposed to do. It is called
             from main in the case that a user wants to replace the results of certain filters.  It is
              known not to work because the query assigned to f_string by make_string tries to
              access the non-existent item table.
    """

    filter_ids = []                             # initalize list of Filter IDs
    filter_id_hash = {}                      # initailze hash from name to ID

    for f in filter_list:                       # for each filter in input list...
        name = f.name                    # ...get its name...
        mrs_tag_list = f.mrs_id_list   #... and the list of mrs tags to which it applies

        filter_id = filters.getFilterID(name, conn)     # get the ID of this filter from the database

        if not filter_id:                                           # if the filter isn't in the DB...
            filter_id = insertFilter(name, "u")             # ...insert it

        for mrs_tag in mrs_tag_list:                        # for every mrs tag this filter applies to...
            # get the row that links the filter to the mrs tag.
            # TODO: figure out why is this table not inserted into if we're adding results for new
            # strings
            row = conn.selQuery("SELECT * FROM fltr_mrs " + \
                                "WHERE fm_fltr_id = %s " + \
                                "AND fm_mrs_tag = %s", (filter_id, mrs_tag))
            if row == ():           # if there's no row there...
                # ...then insert it
                conn.execute("INSERT INTO fltr_mrs " + \
                             "SET fm_fltr_id = %s, fm_mrs_tag = %s, fm_value = 1",
                             (filter_id, mrs_tag))
        filter_ids.append(filter_id)            # add the filter's id to the list of filter IDs
        filter_id_hash[name] = filter_id     # add the filter's name/ID to a hash to get IDs quickly

    ### at this point KEN got confused and couldn't keep up the commenting.
    ### The function seems to have been left in a state of being debugged

    limit = 8700000

    print filter_ids
    f_string = make_string(filter_ids,limit)
    print f_string

    limit += 100000

    #cursor.execute("SELECT r_result_id, r_mrs FROM result, fltr_mrs WHERE r_mrs = fm_mrs_tag AND (fm_fltr_id = %s) LIMIT 10",(f_string))
    ids = conn.selQuery(f_string)
    dupes = []

    while ids != ():

        update_string = "Now working results " + str(limit-100000) + " through " + str(limit)
        print update_string

        outfile = open('ufltrs_updates','a')
        outfile.write(update_string)
        outfile.write("\n")
        outfile.close()

        #`ids' is now a tuple containing elements for each item in a 100,000 slice of the
        #relevant table.  Each of those elements is a tuple which contains
        #just the r_result_id.

        #Based on the r_result_id, we are now going to get the string
        #and mrs_id.

        for ID in ids:

            (key, string, mrs_id) = ID

            filter_values = filters.filter_one_result(mrs_id,string,filter_list,filter_id_hash)

            #Should do error checking here: Are all of the values legit?

            for f_id in filter_values.keys():
                value = filter_values[f_id]

                if not value == 2:
                    num = cursor.execute("UPDATE res_fltr SET rf_value = %s " + \
                                         "WHERE rf_res_id = %s AND  rf_fltr_id = %s",
                                         (value, key, f_id))
                    if num == 0:
                        cursor.execute("SELECT * FROM res_fltr " + \
                                       "WHERE rf_res_id = %s AND rf_fltr_id = %s", (key, f_id))
                        row = cursor.fetchall()
                        if row == ():
                            cursor.execute("INSERT INTO res_fltr " + \
                                           "SET rf_value = %s, rf_res_id =%s, rf_fltr_id = %s",
                                           (value, key, f_id))
                        if len(row) > 1:
                            dupes.append(row)
                    if num > 1:
                        cursor.execute("SELECT * FROM res_fltr " + \
                                       "WHERE rf_res_id = %s AND rf_fltr_id = %s", (key, f_id))
                        dupes.append(cursor.fetchall())

        else:
            update_string = "Last item updated: " + str(key) + " at " + str(datetime.datetime.now())
            print update_string
            outfile = open('ufltrs_updates','a')
            outfile.write(update_string)
            outfile.write("\n")
            outfile.close()

        f_string = make_string(filter_ids,limit)
        ids = conn.selQuery(f_string)
        limit += 100000

    return dupes

#########################################################################
#     add_to_res_fltr(osp_id)

def add_to_res_fltr(osp_id, conn):
    """
    Function: add_to_res_fltr
    Input:
        osp_id - the id of an original source profile in the database.
        conn - a MatrixTDBConn, a connection to the MatrixTDB database        
    Output: none
    Functionality: Gathers all [incr_tsdb()] results from an original source profile (osp_id) and runs
                         each filter on each result, recording the "result" of the first fail on every
                         string in res_fltr.  Also informs the user how many items failed a filter and how
                         many passed all fitlers.
    Author: KEN (Scott Halgrim, captnpi@u.washington.edu)
    Date: 7/6/09
    Tables accessed: item_tsdb, parse, result
    Tables modified: res_fltr
    TODO: recomment this after efficientizing on 8/10/09
    """
    # define the query that gets result IDs, input strings, and mrs tags for results in the OSP that
    # have not failed any u filters in previous runs
    # Note: Joining in the parse table here is kind of silly since when we add permutes we always
    # set parse id = item id = result id, but I'll do it here anyway because it shouldn't add that
    # much time to the query and in case we change things in the future.
    resultsToFilterRows = conn.selQuery("SELECT r.r_result_id, i.i_input, r.r_mrs " + \
                                        "FROM result r INNER JOIN item_tsdb i "  + \
                                        "ON r.r_result_id = i.i_id " + \
                                        "WHERE r.r_osp_id = %s ", (osp_id))

    count = len(resultsToFilterRows)          # get the number of items/results in that osp

    if count == 0:                                      # if there are no results for that osp in the database
        # tell the user it's an error
        print >> sys.stderr, "That is not a valid osp_id. It has no items/results to filter."
        sys.exit()                                                                                  # and exit
    else:                                                                                            # otherwise...
        print >> sys.stderr, 'There are', count, 'results in that osp to filter.' # ...monitoring message

    # intialize a dict whose keys are filter names and whose values are their IDs in the database
    fltrNamesToIDs = {}
    failedResults = set()          # initalize a set of items/results that have failed at least one filter
    passedAllResults = set()      # initalize a set of items/results that passed all filters
    failedResFltrPairs = set()      # initialize a set of result ID/filter ID pairs that we will enter to db

    for row in resultsToFilterRows:          # for each result to filter
        resultID = row[0]                         # get the result ID
        seedString = row[1]                    # get the string
        mrsTag = row[2]                         # get the semantic tag

        for f in filter_list:                                      # for every filter
            if mrsTag in f.mrs_id_list:                     # if the filter applies to this string's mrs tag
                try:
                    fID = fltrNamesToIDs[f.name]            # try getting the filter's ID from the hash
                except KeyError:                                 # if it's not there
                    fID = filters.getFilterID(f.name, conn)  # get the filter's ID from the database

                    if not fID:                     # if the filter wasn't in the database...
                        # ...insert it.
                        fID = filters.insertFilter(f.name, 'u', conn)

                    # If I got from the database or inserted it, enter it into the name/ID hash
                    fltrNamesToIDs[f.name] = fID

                # get the "result" of applying this filter to the profile result.  It will be 0 for fail, 1 for
                # pass, and could even be 2 for semantic tag doesn't apply
                applyResult = f.apply_filter(seedString)

                if applyResult == 0:                # if it failed...

                    # add the string to the set of failed results so we know to stop testing it
                    failedResults.add(resultID)

                    # and add the result/filter pair into a set of fails to enter into res_fltr
                    failedResFltrPairs.add((resultID, fID))

                    if ((len(failedResFltrPairs) % 1000) == 0):     # if I have 1000 fails to enter
                        # print monitoring messages
                        print >> sys.stderr, "inserting 1000 ufilter fails, up to", \
                            len(failedResults)
                        print >> sys.stderr, "passedAllResults up to", len(passedAllResults)
                        insertManyUnivFails(failedResFltrPairs, conn)   # insert those fails into res_fltr
                        failedResFltrPairs.clear()                                  # clear set of fails

                        # this break makes sure we don't check any string more than we have to
                        # Once we get a fail, we're gone
                    break
        else:
            # if the item/result passed every filter, add it to the list of strings that passed
            # everything so we can move on in the while loop
            passedAllResults.add(resultID)

    print >> sys.stderr, "flushing last", len(failedResFltrPairs), "fails" # print monitoring message
    insertManyUnivFails(failedResFltrPairs, conn)   # insert the last fails that were the mod of 1000

    # tell the user how many item/results failed at least one filter
    print >> sys.stderr, len(failedResults), " strings failed at least one filter."

    # tell the user how many item/results passed all universal filters. 
    print >> sys.stderr, len(passedAllResults), " strings passed all universal filters."
    # next line is an experiment I tried on 8/12/09 but went a different direction on 8/13
    # updatePassAllTable(passedAllResults, osp_id, conn)

    # tell the user how many item/results both passed all filters and failed at least one
    # sanity check...should be 0
    print >> sys.stderr, len(failedResults.intersection(passedAllResults)), " strings failed at " + \
                                                                            "least one filter while also passing all relevant filters."

    return

def updatePassAllTable(passAll, ospToUpdate, conn):
    # NOT USING FOR NOW 8/13/09
    # probably quickest just to delete all and re-insert all
    if len(passAll) > 0:
        print >> sys.stderr, "rebuilding res_pass_univ"
        ospRows = conn.selQuery("SELECT DISTINCT r_osp_id FROM result")
        ospIDs = db_utils.selColumnToSet(ospRows)
        tableState = {}

        for osp in ospIDs:
            if osp != ospToUpdate:
                resultRows = conn.selQuery("SELECT rpu_res_id FROM res_pass_univ " + \
                                           "INNER JOIN result on rpu_res_id = r_result_id " + \
                                           "WHERE r_osp_id = %s", (osp))
                resultIDs = db_utils.selColumnToSet(resultRows)

                if len(resultIDs) > 0:
                    tableState[osp] = resultIDs

        conn.execute("DELETE FROM res_pass_univ")

        for osp in tableState.keys():
            resultIDs = tableState[osp]
            valuesClause = 'VALUE '

            for resID in resultIDs:
                valuesClause += '(' + str(resID) + '),'

            valuesClause = valuesClause[:-1]
            insertStmt = 'INSERT INTO res_pass_univ (rpu_res_id) ' + valuesClause
            conn.execute(insertStmt)

        valuesClause = 'VALUE '

        for resID in passAll:
            valuesClause += '(' + str(resID) + '),'

        valuesClause = valuesClause[:-1]
        insertStmt = 'INSERT INTO res_pass_univ (rpu_res_id) ' + valuesClause
        conn.execute(insertStmt)

        return

def insertManyUnivFails(failResFltrPairs, conn):
    """
    Function: insertManyUnivFails
    Input:
        failResFltrPairs - a set of tuples that are values to be added to res_fltr
        conn - a MatrixTDBConn
    Output: none
    Functionality: Inserts several universal filter rails into res_fltr at a time.
    Tables accessed: res_fltr
    Tables modified: res_fltr
    """
    if len(failResFltrPairs) > 0:            # just ensure there are some to add
        valuesClause = 'VALUES '        # initialize 'VALUES ' clause
        for rftuple in failResFltrPairs:     # for every tuple to add
            resID = rftuple[0]                  # get its result id
            fID = rftuple[1]                      # get its filter ID

            # add the result/filter/fail result to VALUES clause            
            valuesClause += '(' + str(resID) + ',' + str(fID) + ',0),'

        valuesClause = valuesClause[:-1] # take off last comma

        # build entire INSERT statement
        insertStmt = 'INSERT INTO res_fltr (rf_res_id, rf_fltr_id, rf_value) ' + valuesClause
        conn.execute(insertStmt)            # insert fails into res_fltr

    return


##########################################################################
# Main program
def main(osp_id, conn):
    """
    Function: main
    Input:
        osp_id - an original source profile id
        conn - a MatrixTDBConn, a connection to the MatrixTDB database    
    Output: none
    Functionality: deletes any existing results for the osp entered, then calls add_to_res_fltr to
                         record the first fail of every result in that original source profile in res_fltr.
    Tables Accessed: result, filter, fltr_mrs, res_fltr, parse, item_tsdb
    Tables Modified: filter, fltr_mrs, res_fltr
    """
    # delete all existing results for the given osp in res_fltr
    conn.execute("DELETE FROM res_fltr WHERE rf_res_id in " + \
                 "(SELECT r_result_id FROM result " + \
                 "WHERE r_osp_id = %s)", (osp_id))

    # TODO: look into possible disconnect with add_permutes.  where that function does not
    # update the osp_id of a result if someone imports a harvester string/mrs tag that generates
    # a seed/mrs combo that already exists...then this function would not pick up on it...and
    # it might confuse the user and, worse, might not get all filter/result combos in there if
    # the original importer hadn't made it this far.

    # ...run every filter on every result/item in osp_id and record first fail of every string into res_fltr
    add_to_res_fltr(osp_id, conn)

    return

# set to true for running on my machine.  set to False before commiting to repository.
moduleTest = False

if __name__ == "__main__":      # only run if run as main module...not if imported
    try:
        osp_id = sys.argv[1]        # get the osp from the command line
    except IndexError:               # if the user didn't give it...

        # ...throw an error message indicating how to call the function
        print >> sys.stderr, 'Usage: python run_u_filters.py osp_id [username] [password]'
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
    # and notify the user moduleTest is set to True.
    print >> sys.stderr, "Note: module testing turned on in run_u_filters.py.  " + \
                         "Unless testing locally, set moduleTest to False."

    # ask the user for the original source profile of the id they want to filter.
    osp_id = raw_input("\nWhat is the osp_id for the results you would lke to filter: ")

    myconn = MatrixTDBConn('2')       # connect to MySQL server
    main(osp_id, myconn)                  # run the main function
    myconn.close()                            # close connection to MySQL server

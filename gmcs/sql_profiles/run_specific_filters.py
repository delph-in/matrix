"""
File: run_specific_filters.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: summer '09 (KEN started working on it then)
Project: MatrixTDB
Project Owner: Emily M. Bender
Contents:
    - last_group_id - global variable that tracks the highest group id (fg_grp_id) in feat_grp
    - queryItemsPassedUFltrs - text of a query (no longer used) that finds all the results in an osp
                                             that pass all universal filters
    - queryItemsOSP - text of a query that gets the strings and mrs tags in an osp id
    - main code that connects to the appropriate database and calls main
    - main - function that calls update_tables_for_filters to make sure every specific filter is in the
                database, is associated with the right feature groups, and every language type is
                associated with the right feature groups.  Then asks user for an original source profile
                id and runs all specific filters on all items/results in that osp and records all filter/result
                combos that fail in res_sfltr
    - update_tables_for_filters - function that makes sure every specific filter is in the database, is
                                            associated with the right feature groups, and also makes sure
                                            every language type is associated with the right feature groups
    - update_filter_table - function that finds a filter by name in MatrixTDB and returns its ID.  Also
                                    enters filter into databse if not there
    - update_groups_table - function that converts the feat/val spec of filter into either a big 'and' or
                                       an 'or' of 'ands' and makes sure that all of the 'and's are grouped as
                                       group IDs in feat_grp and returns a list of those group IDs
    - normalize_group_spec - function that converts the fv member of a Filter to a full binary tree
    - update_groups_table_helper - recursive function that finds a list of group IDs that correspond
                                                  to the input spec and also makes sure that the ands in the
                                                  group spec given have group IDs assigned to them and returns
                                                  a list of them
    - update_group_filter_table - function that assocates filter with feature a set of given feature
                                             groups and only those only those eature groups                               
    - update_all_lts_in_lfg - function that makes sure every language type int he database is linked
                                      to the right feature groups
    - create_choices_from_lt_id - function that extracts a dict of feat/val pairs (or lack of features)
                                               from database that define a language type
Tables accessed: filter, feat_grp, fltr_feat_grp, lt, lt_feat_grp, res_sflt
Tables modified: filter, feat_grp, fltr_feat_grp, lt_feat_grp, res_sfltr
"""
#################################################################
# Read in a list of specific filters, update the filter, feat_group
# and fltr_group tables appropriately, then filter the possibly
# grammatical strings and put results in res_sfltr.

# TODO:
#
# 1. Generalize so we can add additional strings without rerunning
# the ones we already know about.
#
# 2. Generalize so we can pass in a set of filters defined in some
# other file.  For now, edit the line a couple lines down to change
# the file.

# Get the filter list.

import MySQLdb, sys, datetime, filters
#from filters import filter_one_result
from sql_lg_type import update_lt_in_lfg
from matrix_tdb_conn import MatrixTDBConn

# Edit this line to import different filters.

from s_filters import filter_list

# Global variables
# initialize global variable that tracks the highest group id (fg_grp_id) in feat_grp
last_group_id = None

# This query generates all item IDs where the item passed all universal filters for a given osp id
# TODO: can I speed this up by getting rid of the NOT IN clause and instead doing an OUTER
# JOIN where r_result_id is null?
# NB: This is not being used now thatadd_permutes is only inserting into item_tsdb/parse/result
# those items that pass all universal filters
queryItemsPassedUFltrs = """SELECT r_result_id, r_mrs,  i_input
                                            FROM result
                                                INNER JOIN parse on  r_parse_id = p_parse_id
                                                INNER JOIN item_tsdb ON p_i_id = i_id
                                            WHERE r_osp_id = %s
                                                AND r_result_id NOT IN
                                                    (SELECT rf_res_id
                                                        FROM res_fltr
                                                        WHERE rf_value = 0)"""

# gets the id, mrs tag, and string for all those items in an OSP
queryItemsOSP = """SELECT r_result_id, r_mrs,  i_input
                                            FROM result
                                                INNER JOIN parse on  r_parse_id = p_parse_id
                                                INNER JOIN item_tsdb ON p_i_id = i_id
                                            WHERE r_osp_id = %s"""

#################################################################
# update_tables_for_filters(filter_list) makes sure that the
# tables in the DB defining the filters are up to date.
#
# We have to make sure that the filter is in the filter table,
# that the groups of feature-value pairs it uses are in the
# fv_groups table, and that the filter is linked to its groups
# through the fltr_groups table.

def update_tables_for_filters(filter_list, conn):
    """
    Function: update_tables_for_filters
    Input:
        filter_list - the list of specific Filters
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: filter_id_hash - a dict of all the filters where the keys are the filters' names and the
                                      values are their IDs in MatrixTDB
    Functionality: Makes sure every specific filter is in the database, is associated with the right
                         feature groups, and also makes sure every language type is associated with
                         the right feature groups
    Tables accessed: filter, feat_grp, fltr_feat_grp, lt, lt_feat_grp
    Tables modified: filter, feat_grp, fltr_feat_grp, lt_feat_grp
    """
    filter_id_hash = {}             # initialize output

    for f in filter_list:               # for every specific Filter
        # Make sure it's in the filter table, and get its id.
        # If it's not in the table, add it and return its id.
        f_id = update_filter_table(f, conn)

        # Add filter to output filter hash for quick access later
        filter_id_hash[f.name] = f_id

        # Make sure all of the groups it needs are in the groups table and put their IDs in groups
        groups = update_groups_table(f, conn)

        # Associate the filter with those groups and only those groups
        update_group_filter_table(f_id, groups, conn)

    # Make sure that all of the existing language types
    # are updated to see the new groups added, if any.
    # KEN moved this line out of the for loop b/c it was a time sink and I don't see why it needed
    # to be run once for every filter...seems it can be run at end.

    # make sure every language type in databse is associated with the right feature groups
    update_all_lts_in_lfg(conn)

    return filter_id_hash   # return output

###################################################################
# Every time we add filters and feature groups, we should make
# sure that lt_feat_grp is up to date, too.

def update_all_lts_in_lfg(conn):
    """
    Function: update_all_lts_in_lfg
    Input: conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: none
    Functionality: Makes sure every language type int he database is linked to the right feature
                         groups
    Tables accessed: lt, lt_feat_grp, feat_grp
    Tables modified: lt_feat_grp
    """
    # get all the language types in the database
    lt_ids = conn.selQuery("SELECT lt_id FROM lt")

    for lt_id in lt_ids:        # for each language type in the database...
        # get a dict of feat/val combos or lack of features that define this language type
        # NOTE: this choices variable is a dict, not a ChoicesFile
        choices = create_choices_from_lt_id(lt_id[0], conn)

        # ensure the language type is linked to the right feature groups
        update_lt_in_lfg(choices, lt_id[0], conn)

    return

################################################################
# Sometimes we want to get a choices object out of the lt_feat_grp
# table, instead of a file
# NOTE: This does not actually return a ChoicesFile object, but a dict instead.

def create_choices_from_lt_id(lt_id, conn):
    """
    Function: create_choices_from_lt_id
    Input:
        lt_id - an ID of a language type in the database
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: choices - a dict of all the feature/value pairs.  A value is null or '' if meant to indicate
                              the lack of a feature
    Functionality: Extracts a dict of feat/val pairs (or lack of features) from database that define
                         a language type
    Tables accessed: lt_feat_grp, feat_grp
    Tables modified: none
    Note: This returns choices, which is a dict of feat/val pairs, not a ChoicesFile
    """
    choices = {}            # initialize output dict

    # get the feature groups that define this language type
    fg_ids = conn.selQuery("SELECT lfg_grp_id " + \
                           "FROM lt_feat_grp " + \
                           "WHERE lfg_lt_id = %s",(lt_id))

    # for each feature group that defines this language type...
    for fg_id in fg_ids:
        # get the feat/val combos that define the group
        fvs = conn.selQuery("SELECT fg_feat, fg_value " + \
                            "FROM feat_grp " + \
                            "WHERE fg_grp_id = %s",(fg_id[0]))
        if len(fvs) == 1:       # if only one feat/val combo was returned
            fv = fvs[0]            # ...this is a singleton group, so get the feature...
            choices[fv[0]] = fv[1] # and update that key's value in the output to its value 

    return choices          # return output, a dict of feat/val pairs, not a ChoicesFile

#################################################################
# update_filter_table(filter): Make sure the filter itself is
# in the filter table, and get its id.

def update_filter_table(f, conn):
    """
    Function: update_filter_table
    Input:
        f - a Filter
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: f_id - the id of f in the database
    Functionality: finds f by name in MatrixTDB and returns its ID.  Also enters f into databse if
                         not there
    Tables accessed: filter
    Tables modified: filter
    """
    # Make sure each filter is recorded in the filter table

    name = f.name       # get the filter's name

    try:
        # get the filter's ID from the databse
        f_id = conn.selQuery("SELECT filter_id FROM filter where filter_name = %s",(name))[0][0]
    except IndexError:
        # we'll be here if the query above returned no rows, so insert it
        # Since "s" is the default value for filter_type, we don't need to specify it.
        # TODO: verify there are protections in the db so we can't insert dup filter names        
        conn.execute("INSERT INTO filter SET filter_name = %s", (name))

        # and get its id
        f_id = conn.selQuery("SELECT LAST_INSERT_ID()")[0][0]

    return f_id         # return the filter's ID

#################################################################
# update_groups_table(filter): Take the specification of feature-
# value groups from the filter definition and check whether those
# groups are defined in the feat_groups tables.  If not, add them.

def update_groups_table(fltr, conn):
    """
    Functio: update_groups_table
    Input:
        fltr - a Filter
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: groups - a list of group IDs that correspond to the 'and' parts of the normalized spec
                             of fltr
    Functionality: converts the feat/val spec of filter into either a big 'and' or an 'or' of 'ands' and
                         makes sure that all of the 'and's are grouped as group IDs in feat_grp and
                         returns a list of those group IDs
    Tables accessed: feat_grp
    Tables modified: feat_grp
    """
    global last_group_id        # declare the highest group id in feat_grp table to be global var

    # Get the specification of filters.  This is the definition of what combination of feat/val pairs
    # or absence of features that this filter cares about
    group_spec = fltr.fv

    # Transform to disjunctive normal form
    gs = normalize_group_spec(group_spec)

    # Get starting id:
    try:
        # get the last highest feature group id in database
        last_group_id = conn.selQuery("SELECT MAX(fg_grp_id) FROM feat_grp")[0][0]
    except IndexError:              # or if there are none there...
        last_group_id = 0             # ...call the highest id 0

    # Update the groups table as necessary, storing list of group_ids of the 'ands' in gs
    # in 'groups'
    groups = update_groups_table_helper(gs, [], conn)

    return groups                   # return output

#################################################################
# update_groups_table_helper(gs,groups): Recursive helper function
# for update_groups_table().

# **** START HERE **** This isn't respecting the AND groups
# ERB 2007-07-18 Fixed now?

def update_groups_table_helper(gs, groups, conn):
    """
    Function: update_groups_table_helper
    Input:
        gs - a formatted list that is a feat/val specification that describes what language types a
               Filter applies to.
        groups - a list of group IDs found at earlier calls of this function that are the group IDs for
                     and parts of the spec
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: groups - same as input groups but with group IDs for the ands in this group spec
                            appended to it
    Functionality: recursive function that finds a list of group IDs that correspond to the input spec
                         and also makes sure that the ands in the group spec given have group IDs
                         assigned to them and returns a list of them
    Tables accessed: feat_grp
    Tables modified:feat_grp
    """
    # _FIX_ME_ there's some refactoring to do here.

    global last_group_id                        # declare last_group_id as global variable

    # Find the groups from the gs CNF

    # gs[0] = 'or', rest are either 'and'-tuples or strings
    if gs[0] == 'or':                               # if the operator of the spec is an 'or'...
        for g in gs[1:]:                            # ...then for each daughter...
            # ...rescursively call this function on gs and groups and assign output to groups
            groups = update_groups_table_helper(g, groups, conn)

        answer = groups                         # set final value of groups to output

    # gs is ['f:v']
    # if the spec is just a string or a list of one feat/val combo...    
    elif len(gs) == 1 or type(gs) == str:
        if len(gs) == 1:            # ...if it's a list of length 1...
            gs = gs[0]               # ...set gs to its only item
        [f,v] = gs.split(':')         # split the item into feature and value
        try:
            # get the group id of this feat/val combo as a singleton group
            g_id = conn.selQuery("SELECT fg_grp_id FROM " + \
                                 "(SELECT fg_grp_id, fg_feat, fg_value, count(*) num " + \
                                 "FROM feat_grp " + \
                                 "GROUP BY fg_grp_id) subq " + \
                                 "WHERE num = 1 AND fg_feat = %s AND fg_value = %s",
                                 (f,v))[0][0]
            groups.append(g_id)     # and append that group id to groups list
        except IndexError:  # but if that feat/val combo isn't in the database as a singleton group...
            last_group_id += 1      # ...increment the highest group id...

            # ...and insert this feat/val combo into db as a group with that new highest group id
            conn.execute("INSERT INTO feat_grp " + \
                         "SET fg_grp_id = %s, fg_feat = %s, fg_value = %s", (last_group_id,f,v))
            groups.append(last_group_id)        # and append that id to the list of groups
        answer = groups                               # set output to groups

    # gs is 'and'-tuple
    elif gs[0] == 'and':            # if the operator of the root is 'and'
        #create complex query, with lists of variable length, depending on how many features there
        #are in the list.  We know there's at least two things on this list.
        from_string = 'FROM feat_grp AS fg1 '       # initialize from clause
        where_string = 'WHERE '                         # initialize where clause
        feat_string = ''                                          # initialize list of features in where clause
        value_string = ''                                        # initialize list of values in where clause

        # looks like this loop creates a long list of joining
        for i in range (2, len(gs)):                                    # for index of daughter after first...
            j = i - 1                                                         # ...set j to index of prev daughter
            from_string += ', feat_grp AS fg' + str(i) + ' ' # concatente new table alias to from string

            if i != 2:                                  # if this isn't the first time through the loop...
                where_string += 'AND '        # ...add an AND to where string

            # add the net feature/value combo to the where string
            where_string += 'fg' + str(j) + '.fg_grp_id = fg' + str(i) + '.fg_grp_id '

        # looks like this loop creates a long list of feat/val specs to find
        for i in range (1,len(gs)):             # for each daughter...
            g = gs[i].split(':')                    # get its feature/value

            # add the feature to where clause            
            feat_string += 'AND fg' + str(i) + '.fg_feat = "' + g[0] + '" '

            # add the value to where clause
            value_string += 'AND fg' + str(i) + '.fg_value = "' + g[1] + '" '

            # set the start of the query
        prefix = 'SELECT feat_grp.fg_grp_id FROM feat_grp WHERE feat_grp.fg_grp_id IN ' + \
                 '(SELECT fg1.fg_grp_id '

        # set the end of the query
        suffix = ') GROUP BY feat_grp.fg_grp_id HAVING count(*) = ' + str(len(gs)-1)

        # build final query string
        query_string = prefix + from_string + where_string + feat_string + value_string + suffix

        # we need to escape any backslashes for the purpose of the query
        # this was added when we started adding backreferences to our fv lists in filters
        query_string = query_string.replace('\\', r'\\')

        # execute query string, getting the group id that encompasses all of the feat/val combos
        # of the input spec
        rows = conn.selQuery(query_string)

        if len(rows) == 0:              # if there were no rows returned...
            last_group_id += 1        # ...increment highest group id
            for fv in gs[1:]:               # for each daughter feat/val in spec...
                [f,v] = fv.split(':')          # ...break up into feature and value

                # and add as a part of the new group id
                conn.execute("INSERT INTO feat_grp " + \
                             "SET fg_grp_id = %s, fg_feat = %s, fg_value = %s",
                             (last_group_id, f, v))
            groups.append(last_group_id)        # add new group id to output groups list

        else:                   # but if we got a group id back encompassing these feat/val combos...
            groups.append(rows[0][0])           # ...add that id to output groups list

        answer =  groups                            # set groups to output

    return answer                                      # return output

################################################################
# update_group_filter_table(f_id, groups): Update the table which
# relates filters to the groups of features they index.  In the
# general case, there are many feature groups per filter (logical
# or).

def update_group_filter_table(f_id, groups, conn):
    """
    Function: update_group_filter_table
    Input:
        f_id - a filter id
        groups - a list of group IDs that apply to this filter
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: none
    Functionality: Assocates filter with feature groups and only those feature groups
    Tables accessed: fltr_feat_grp
    Tables modified: fltr_feat_grp
    """
    #update fltr_lt each time, to make sure it stays consistent.

    # delete any existing intersection between this fliter and feature groups
    conn.execute("DELETE FROM fltr_feat_grp WHERE ffg_fltr_id = %s", (f_id))

    # Now put in the current values.
    for g in groups:        # for each group that applies to this filter...
        # ...update intersection table between the filter and those feature groups
        conn.execute("INSERT INTO fltr_feat_grp SET ffg_fltr_id = %s, ffg_grp_id = %s",(f_id,g))

    return

#################################################################
# normalize_group_spec(group_spec): Takes in the group specification
# from a filter definition and converts it to disjunctive normal
# form (an or of ands).
# TODO: There's a problem here in that it doesn't deal with "not"s, which seem to be
#     handled implicitly in s_filters.py by assigning a feature without a value.
#     E.g, fv=['aux-verb:'] seems to mean the filter applies to any lt without the aux-verb feature
#         of any value.  (update: i think this is dealt with mostly now)

def normalize_group_spec(group_spec):
    """
    Function: normalize_group_spec
    Input: group_spec - the fv member of a Filter that is a formatted list defining what combination
                                 of feat/val pairs, or lack of features, needs to exist in a language type for
                                 a filter to care about a string in the language type
    Output: gs - input list converted to a fully binary tree
    Functionality: converts the fv member of a Filter to a full binary tree
    Tables accessed: none
    Tables modified: none
    """
    # These are nested lists with at least three elements each where the first element 
    # of each list is the string 'or' or 'and', and the remaining elements are either
    # strings of the form 'feature:value' or group_specs themselves.
    #
    # _FIX_ME_ Consider replacing this list-based version with a tree class and a parser
    # that takes a string with parens and parses it into a tree.

    # First handle the case where we have just one fv pair:

    if len(group_spec) == 1:                    # if the list is of length one...
        if type(group_spec[0]) == str:         # ...and its only member is a string...
            return group_spec                     # ...just return that list
        # ...but if it's of length one and the only member is not a string...
        else:
            raise ValueError, "Ill-formed group spec."  # ...then it's ill-fomed, so raise an error

    #Take possibly flat tree and make it binary for easier processing.
    gs = make_binary_gs(group_spec)

    finished = False                # TODO: what does this do?

    # Because with a binary tree, one top-down pass isn't guaranteed to
    # give a normalized form.  So, keep calling helper function until there's
    # nothing left to do.  _FIX_ME_ I'm sure there's a more efficient way to do this.
    while finished == False:                        # until I have a run where nothing is done...
        # ...keep calling the function that actually does the conversion of the spec to discjunctive
        # normal form
        # initialize the input to True so that if this is a simple one where nothing has to be done
        # then True is returned
        [gs, finished] = normalize_gs_helper(gs, True)

    gs = flatten_binary_gs(gs)      # flatten the specification

    return gs                               # return output


#################################################################
# normalize_gs_helper(gs, finished): Recursive helper function for
# normalize_group_spec().  `finished' tracks whether there was
# anything to do on this pass.  _FIX_ME_ Perhaps there is some way
# to do it in one pass?

def normalize_gs_helper(gs, finished):
    """
    Function: normalize_gs_helper
    Input: gs - binarized formatted list defining what combination of feat/val pairs, or lack of
                    features, needs to exist in a language type for a filter to care about a string in the
                    language type
    Output: - return_value, a list consisting of:
                        - gs converted, at least partially, to disjunctive normal form
                        - boolean indicating if there was something to be done on this pass
    Functionality: recursive function that converts a logical combination of feat/val pairs to a
                         logically eqiuvalent form where the root operator is 'or'
    Tables accessed: none
    Tables modified: none
    """
    # TODO: this function seems big, can it be made smaller?  broken up? refactored?
    #if finished == False:
    #return [gs,finished]

    #Change finished to False as soon as we do something.  Then pass it back
    #through the recursive calls for this pass.

    #Base case: The gs is just a string.

    if type(gs) == str:                         # if input is a string...
        # ...set output to a list of that string plus same val for finished as input
        # returning input as boolean output works since this is a recursive function...that way we
        # don't lose the value of what was coming in from higher up in the tree
        return_value = [gs, finished]

    #Otherwise, we have something to recursively handle.
    else:
        left_dtr = gs[1]                    # get the left daughter
        right_dtr = gs[2]                  # get the right daughter

        #Operator is an or.  Recurse through daughters.
        if gs[0] == 'or':
            # recurse on left daughter and input value of finished
            [new_left_dtr, f1] = normalize_gs_helper(left_dtr, finished)

            # recurse on right daughter and output of recursing on left daughter
            [new_right_dtr, f2] = normalize_gs_helper(right_dtr, f1)

            # create a new fv specification comprised of the original operator 'or' and
            # the result of recursing on left and right daughters
            new_gs = ['or', new_left_dtr, new_right_dtr]

            # set output to that new fv spec and the finished output of recursing on right daughter
            return_value = [new_gs, f2]

        # If we've made it this far, operator is 'and'.
        # in case where daughter operators are also 'and' or strings
        elif ((left_dtr[0] == 'and' or type(left_dtr) == str) and
                  (right_dtr[0] == 'and' or type(right_dtr) == str)):

            # recurse on left daughter and input value of finished            
            [new_left_dtr, f1] = normalize_gs_helper(left_dtr, finished)

            # recurse on right daughter and output of recursing on left daughter            
            [new_right_dtr, f2] = normalize_gs_helper(right_dtr, f1)

            # create a new fv specification comprised of the original operator 'and' and
            # the result of recursing on left and right daughters
            new_gs = ['and', new_left_dtr, new_right_dtr]

            # set output to that new fv spec and the finished output of recursing on right daughter            
            return_value = [new_gs, f2]

        #Now the cases where we have to change something.

        #Operator is 'and'.  Left dtr is 'or' and right dtr is string.
        elif (left_dtr[0] == 'or' and type(right_dtr) == str):
            # what we do in this case is change something like ['and', ['or', ldld, ldrd], rd]
            # to something like ['or', ['and', ldld, rd], ['and', ldrd, rd]]
            # which are logically equivalent

            # set finished to False because we can't be sure we've fixed everything even after
            # making this change
            finished = False

            # recurse on spec that is current operator (and), the left daughter of left daughter, and
            # the right daughter and finished is False
            [new_left_dtr, foo] = normalize_gs_helper(['and', left_dtr[1], right_dtr], False)

            # recurse on spec that is current operator (and), the right daughter of left daughter,
            # and the right daughter and finished is False
            [new_right_dtr, foo] = normalize_gs_helper(['and', left_dtr[2], right_dtr], False)

            # create a new spec that is the left daughter's operator (or) and the results of recursing
            # on the left daughter's daughters and the right daugther
            new_gs = ['or', new_left_dtr, new_right_dtr]

            # set output value to that new spec and False
            return_value = [new_gs, finished]

        #Operator is 'and'.  Left dtr is string, right dtr is 'or'.
        elif (right_dtr[0] == 'or' and type(left_dtr) == str):
            # what we do in this case is change something like ['and', ;ld, ['or', rdld, rdrd]]
            # to something like ['or', ['and', ld, rdld], ['and', ld, rdrd]]
            # which are logically equivalent

            # set finished to False because we can't be sure we've fixed everything even after
            # making this change
            finished = False

            # recurse on spec that is current operator (and), the left daughter, and the left daughter
            # of the right daughter and finished is False
            [new_right_dtr, foo] = normalize_gs_helper(['and', left_dtr, right_dtr[1]], False)

            # recurse on spec that is current operator (and), the left daughter, and the right daughter
            # of the right daughter and finished is False            
            [new_left_dtr, foo] = normalize_gs_helper(['and', left_dtr, right_dtr[2]], False)

            # create a new spec that is the right daughter's operator (or) and the results of recursing
            # on the left daughter and the right daughter's daughters
            new_gs = ['or', new_left_dtr, new_right_dtr]

            # set output value to that new spec and False            
            return_value = [new_gs,finished]

        #Operator is 'and'.  Both dtrs are 'or'.
        elif (right_dtr[0] == 'or' and left_dtr[0] == 'or'):  #probably redundant to check
            # in this case we take something like ['and', ['or', ldld, ldrd], ['or', rdld, rdrd]] and turn it
            # into something like
            # ['or', ['and', ldld, rdld], ['and', ldld, rdrd], ['and', ldrd, rdld], ['and', ldrd, rdrd]] which is
            # logically equivalent, then we binarize that spec so it's more like a right-branching thing:
            # ['or', ['and', ldld, rdld], ['or', ['and', ldld, rdrd], ['or'....

            # set finished to False because we can't be sure we've fixed everything even after
            # making this change
            finished = False

            # recurse on spec that is current operator (and), the left daughter's left daughter, and the
            # right daughters's left daughter, and finished is False
            [dtr1, foo] = normalize_gs_helper(['and', left_dtr[1], right_dtr[1]], False)

            # recurse on spec that is current operator (and), the left daughter's left daughter, and the
            # right daughters's right daughter, and finished is False
            [dtr2, foo] = normalize_gs_helper(['and', left_dtr[1], right_dtr[2]], False)

            # recurse on spec that is current operator (and), the left daughter's right daughter, and
            # the right daughters's left daughter, and finished is False
            [dtr3, foo] = normalize_gs_helper(['and', left_dtr[2], right_dtr[1]], False)

            # recurse on spec that is current operator (and), the left daughter's right daughter, and
            # the right daughters's right daughter, and finished is False
            [dtr4, foo] = normalize_gs_helper(['and', left_dtr[2], right_dtr[2]], False)

            # make a new global spec that is both daughters' operator (or) and the results of
            # recursing on an and operator with all four permutations of the input spec's
            # granddaughters
            new_gs = ['or', dtr1, dtr2, dtr3, dtr4]

            # binarize that new spec
            binary_new_gs = make_binary_gs(new_gs)

            # set output value to that binarized spec and False
            return_value = [binary_new_gs ,finished]

        #Operator is 'and'.  Left dtr is 'or'.  Right dtr is 'and'.
        elif (left_dtr[0] == 'or' and right_dtr[0] == 'and'):
            # in this case we take something like ['and', ['or', ldld, ldrd], ['and', rdld, rdrd]] and
            # convert it to something like ['or', ['and', ldld, rdld, rdrd], ['and', ldrd, rdld, rdrd]] (but with
            # both daughters binarized), which is logically equivalent

            # set finished to False because we can't be sure we've fixed everything even after
            # making this change            
            finished = False

            # create a new spec that is this node's operator (and) with the left daughter's left
            # daughter and both daughters of the right daughter
            flat_dtr_1 = ['and', left_dtr[1], right_dtr[1], right_dtr[2]]

            # binarize that spec
            bin_dtr_1 = make_binary_gs(flat_dtr_1)

            # create a new spec that is this node's operator (and) with the left daughter's right
            # daughter and both daughters of the right daughter
            flat_dtr_2 = ['and', left_dtr[2], right_dtr[1], right_dtr[2]]

            # binarize that spec
            bin_dtr_2 = make_binary_gs(flat_dtr_2)

            # recurse on the first binarized spec and False
            [dtr1, foo] = normalize_gs_helper(bin_dtr_1, False)

            # recurse on the second binarized spec and False            
            [dtr2, foo] = normalize_gs_helper(bin_dtr_2, False)

            # create a new spec that is the left daughter's operator (or) and the result of recursing
            # on our two binarized specs
            new_gs = ['or', dtr1, dtr2]

            # set output value to that new spec and False
            return_value = [new_gs, finished]

        #Operator is 'and'.  Left dtr is 'and'.  Right dtr is 'or'.
        elif (left_dtr[0] == 'and' and right_dtr[0] == 'or'):
            # in this case we take something like ['and', ['and', ldld, ldrd], ['or', rdld, rdrd]] and
            # convert it to something like ['or', ['and', ldld, ldrd, rdld], ['and', ldld, ldrd, rdrd]] (but with
            # both daughters binarized), which is logically equivalent

            # set finished to False because we can't be sure we've fixed everything even after
            # making this change                        
            finished = False

            # create a new spec that is this node's operator (and) with the right daughter's left
            # daughter and both daughters of the left daughter            
            flat_dtr_1 = ['and', right_dtr[1], left_dtr[1], left_dtr[2]]

            # binarize that spec
            bin_dtr_1 = make_binary_gs(flat_dtr_1)

            # create a new spec that is this node's operator (and) with the right daughter's right
            # daughter and both daughters of the left daughter                        
            flat_dtr_2 = ['and', right_dtr[2], left_dtr[1], left_dtr[2]]

            # binarize that spec
            bin_dtr_2 = make_binary_gs(flat_dtr_2)

            # recurse on the first binarized spec and False            
            [dtr1, foo] = normalize_gs_helper(bin_dtr_1, False)

            # recurse on the first binarized spec and False            
            [dtr2, foo] = normalize_gs_helper(bin_dtr_2, False)

            # create a new spec that is the left daughter's operator (or) and the result of recursing
            # on our two binarized specs
            new_gs = ['or', dtr1, dtr2]

            # set output value to that new spec and False
            return_value = [new_gs, finished]
        else:
            # if none of the above condtions are true, then we have an error, so print the spec...
            print gs
            raise ValueError, "Something is wrong with Group_spec."     # ...and raise an error

    return return_value         # return output

###########################################################################
# filter_one_result copied & modified from filters.py

def filter_one_result(mrs_id, sent,filter_list,filter_id_hash):
    """
    Called From:
        nowhere
    Another Version:
        According to the comment line above, this was inspired by filters.filter_one_result
    """

    filter_values = {}

    for f in filter_list:
        filter_values[filter_id_hash[f.name]] = f.exe(mrs_id, sent)

    return filter_values


#################################################################
# make_binary_gs(group_spec): Takes group_spec with arbitrary
# arity and converts it to binary format.

def make_binary_gs(group_spec):
    """
    Function: make_binary_gs
    Input: group_spec - the fv member of a Filter that is a formatted list defining what combination
                                 of feat/val pairs, or lack of features, needs to exist in a language type for
                                 a filter to care about a string in the language type
    Output: return_value - group_spec binarized with only two daughters
    Functionality: recursive function that binarizes a list of feat/val combos.  Makes sure the list to
                         all depths is a list of three elements, an operator (and or or) and two lists that
                         are also binarized in the same manner.  At the leaves, the lists turn into just
                         strings that are a feat/val combo
    Tables accessed: none
    Tables modified: none
    """
    l = len(group_spec)                                         # get length of group_spec

    # Check syntax of group_spec.

    if not type(group_spec) == list:                        # if group_spec is not a list...
        raise ValueError, "Group_spec is not a list." # ...raise an error

    # if group_spec is shorter than 3 elements (the case where it's one element is handled in
    # normalize_group_spec)...
    if l < 3:
        # ...raise an error
        raise ValueError, "Group_spec does not have enough elements (must be at least 3)."

    # if group_spec doesn't start with either 'and' or 'or' as its first element...
    if not (group_spec[0] == 'and'
            or group_spec[0] == 'or'):
        raise ValueError, "Group_spec does not being with 'and' or 'or'."           # ...raise an error

    # Check if we have more than two daughters

    if l > 3:                                               # if we do have more than two daughters...
        op = group_spec[0]                          # ...get the operator (and or or)
        left_dtr = group_spec[1]                    # get the left daughter (car the list)

        # make the right daughter the operator plus the rest of the daughters (the cdr of the input
        # list)
        right_dtr = [op] + group_spec[2:]

        # and call this function recursively on the cdr of the list
        binary_right_dtr = make_binary_gs(right_dtr)

        # put the results of binarzing the cdr of the list into a single right-daughter element
        return_value = [op, left_dtr, binary_right_dtr]

    elif l == 3:                                                    # otherwise, if we have exactly two daughters...
        if type(group_spec[1]) == str:                          # ...if the left daughter is a string...
            left_dtr = group_spec[1]                              # ...make it the left daughter
        else:                                                             # ...if the left daughter is not a string...     
            left_dtr = make_binary_gs(group_spec[1])    # ...call this function recursively on it            

        if type(group_spec[2]) == str:                          # ...if the right daughter is a string         
            right_dtr = group_spec[2]                             # ...make it the right daughter
        else:                                                               # ...if the right dauther is not a string...
            right_dtr = make_binary_gs(group_spec[2])    # ...call this function recursively on it            

        # set output to first element of input (the operator) plus binarized left and right daughters
        return_value = [group_spec[0], left_dtr, right_dtr]

    return return_value                                             # return output

#################################################################
# flatten_binary_gs(gs): Takes group_spec in binary format and
# flattens it.  Result should be at most two layers deep (an
# or of ands).

def flatten_binary_gs(gs):
    """
    Function: flatten_binary_gs
    Input: gs - a binarized formatted list in disjunctive normal form defining what combination of
                    feat/val pairs, or lack of features, needs to exist in a language type for a filter to
                    care about a string in the language type
    Output: answer - list like input but flattened where ors and ands are grouped
    Functionality: Takes a feature/value specification list and flattens it.  If a node has an 'or'
                          operator, it promotes the elements of any daughter it has with an 'or' operator
                          as well.  Same for 'and'
    Tables accessed: none
    Tables modified: none
    """
    # base case

    if type(gs) == str:                       # if the spec is just a string...
        answer = [gs]                         # ...just put it into a list and set it to output
    else:                                          # but if it's not a string, it's a list...
        op = gs[0]                               # ...get its operator ('and' or 'or')
        new_dtrs = [op]                       # initalize output to list starting with operator
        for d in gs[1:]:                          # for each daugther of input spec
            if type(d) == str:                   # if the daughter is a string...
                new_dtrs.append(d)          # ...append it to output list
            else:                                   # if it's not a string...
                d = flatten_binary_gs(d)    # recursively call this function on it

                # if the first element of the flattened daughter is now equal to the same operator
                # of this node...
                if d[0] == op:
                    #[or1, [or2, [and, a, b] [and, c, d]], e]
                    #op = or1
                    #first nd = [or2, [and, a, b] [and, c, d]]
                    #second nd = 3

                    # ...promote each of those dtrs a level while also flattening them.
                    for nd in d[1:]:                        # take each granddaughter...
                        new_dtrs.append(nd)          # ...and append it to the output list

                        #if type(nd) == str:

                        #else:
                        #new_dtrs += [flatten_binary_gs(nd)]

                # if the flattened daughter's operator is different from that of this node...
                else:
                    # ...don't promote but do flatten recursively.
                    # do this by appending the flattened daughter itself (not the granddaughter)
                    # TODO: this seems redundant to recusively call this on d here as well as above...
                    # ...seems like we could save considerable time by not doing this
                    new_dtrs.append(flatten_binary_gs(d))

        # assign list with flattened daughters to output
        answer = new_dtrs

    return answer       # return output


#######################################################################
# make_string: for constructing the main select query

def make_string(limit):
    """
    Called from: nowhere
    Functionality: seems to be designed to pick up from some point where processing a large
                         number of items needed to be broken into smaller chunks
    """

    output = "SELECT r_result_id, r_mrs, i_input " + \
             "FROM result, item  " + \
             "WHERE result.r_parse_id = item.i_id " + \
             "AND r_wf = 1  " + \
             "AND result.r_result_id < 12000000 LIMIT " + str(limit) + ", 100000"
    return output

def getLT_FVs(fvdict):
    answer = set()
    for k in fvdict.keys():
        answer.add(k+':'+fvdict[k])
    return answer



############################################################################
# Main program

def main(osp_id, conn):
    """
    Function: main
    Input:
        osp_id - an original source profile id    
        conn - a MatrixTDBConn, a connection to MatrixTDB
    Output: none
    Functionality: Calls update_tables_for_filters to make sure every specific filter is in the
                         database, is associated with the right feature groups, and every language type
                         is associated with the right feature groups.  Then asks user for an original
                         source profile id and runs all specific filters on all items/results in that osp and
                         records all filter/result combos that fail in res_sfltr
    Tables accessed: filter, feat_grp, fltr_feat_grp, lt, lt_feat_grp, res_sfltr
    Tables modified: filter, feat_grp, fltr_feat_grp, lt_feat_grp, res_sfltr
    """

    # First check whether the filters are properly recorded in
    # filter and fltr_lt

    # make sure every specific filter is in the database, is associated with the right feature groups
    # and that every language type in database is also associated with right feature groups
    # also get a hash that goes from a filter's name to its ID
    # TODO: this takes a while...figure out a way to only call it in certain instances.
    filter_id_hash = update_tables_for_filters(filter_list, conn)

    # Then pull all strings that pass the universal filters, and record
    # their values for the specific filters.  Perhaps update this if
    # we're moving the non universally ungrammatical strings to another
    # table.

    # ERB 2007-06-22 Picking up where I left off in a run that crashed.

    #f_string = make_string(7000000)

    #cursor.execute(f_string)
    #ids = cursor.fetchall()

    #limit = 7100000

    # TODO: Consider, is there a reason we don't ask about adding or replacing here
    # like we did in u_filters?  Is there a reason we didn't ask for the osp_id first like we
    # did in u_filters?

    # get the set of item/results from that osp that pass all universal filters
    passAllUnivs = conn.selQuery(queryItemsOSP, (osp_id))

    # delete all existing results for that osp in res_sfltr
    # TODO: consider speeding this up by just putting the osp id on res_sfltr
    conn.execute("DELETE FROM res_sfltr WHERE rsf_res_id in " + \
                 "(SELECT r_result_id FROM result " + \
                 "WHERE r_osp_id = %s)", (osp_id))

    if len(passAllUnivs) == 0:                  # if no strings in that osp_id pass all u filters...
        print "That is not a valid osp_id."   # ...give error message

    # for purposes of logging/monitoring, tell user how many results we have left to filter
    print "There are " + str(len(passAllUnivs)) + " results to filter."


    # TODO: some of this code is repeated in run_u_filters...consider combining

    # initalize counter that is used to indicate progress in logging messages
    thousandCounter = 1
    resultsToInsert = set()     # intialize set that will store up many specific filter results to insert

    for i in range(len(passAllUnivs)):             # for every result that passed all u filters
        row = passAllUnivs[i]                        # get the row 
        res_id, mrs_id, string = row   # get its result_id, its mrs tag, and the string

        if ((i % 1000) == 0):
            print >> sys.stderr, "working on string number", i # for monitoring progress only

        # TODO: why do all the string results above have a space at the end?  consider fixing.

        for f in filter_list:                                       # for each specific filter...
            if f.check_mrs_id(mrs_id):                     # ...see if it applies to this sem tag...
                applyResult = f.apply_filter(string)       # ...if so, apply the filter
            else:                                                   # if it doesn't care about this sem class...
                # TODO: this also happens in filter. (exe, i think)  fix it so it only happens one place.
                applyResult = 2                               # ...give it a "result" of don't care

            fID = filter_id_hash[f.name]                   # get the filter's ID

            # TODO: Consider not having two separate tables res_fltr and res_sfltr...they're the
            # same or have to filter tables, too, to make that parallel.
            # TODO: deal with case where the filters have already run on these results.  should
            # i overwrite them?  inform user?  can't just insert again due to uniqueness constraints.
            # TODO: figure out how to make this go faster if we only want to run new filters.
            if applyResult == 0:            # if the result/item failed the filter...

                # ...add the result/filter combo to the set of results to be added to res_sfltr
                resultsToInsert.add((res_id, fID, applyResult))

                if ((len(resultsToInsert) % 1000) == 0):                # if we have 1000 results to insert

                    # print logging message
                    print >> sys.stderr, 'inserting 1000 specific results, up to', 1000*thousandCounter
                    thousandCounter += 1                                    # increment thousand counter

                    # insert those results into database
                    filters.insertManyFilteredSpecResults(resultsToInsert, conn)
                    resultsToInsert.clear()                                     # clear set of results to insert

    # print logging message about the final results that were the mod 1000 of the whole group
    print >> sys.stderr, 'flushing final', len(resultsToInsert), 'specific results into res_sfltr'

    # insert final set of specific results for those where i didn't get up to 1000
    filters.insertManyFilteredSpecResults(resultsToInsert, conn)

    return

# set to true for running on my machine.  set to False before commiting to repository.
moduleTest = False

# consider passing in conn intsead of using a global variable.
if __name__ == "__main__":      # only run if run as main module...not if imported
    try:
        osp_id = sys.argv[1]        # get the osp from the command line
    except IndexError:               # if the user didn't give it...

        # ...throw an error message indicating how to call the function
        print >> sys.stderr, 'Usage: python run_specific_filters.py osp_id [username] [password]'
        sys.exit()                       # and exit

    try:                                        # try to get...
        username = sys.argv[2]       # ...username...
        password = sys.argv[3]       # ...and password off of command line

        # if successful, create a connection using that username and password
        myconn = MatrixTDBConn('2', username, password)

    except IndexError:                               # if no username and/or password supplied...
        myconn = MatrixTDBConn('2')           # connect to MySQL server and prompt for them

    # update database with specific filters, make sure they are associated with correct feature
    # groups, make sure language types are associated with right language types, then run
    # specific filters on an osp id provided by user and record all fails
    main(osp_id, myconn)
    myconn.close()                              # close connection to MySQL server
elif moduleTest:                        # or if i'm testing, run it on MatrixTDB2
    # notify user moduleTest is set to True
    print >> sys.stderr, "Note: module testing turned on in run_specific_filters.py.  " + \
                         "Unless testing locally, set moduleTest to False."

    # get the osp_id for the results we want to filter.  Doing it by osp_id instead of just all of the
    # strings allows us to import new strings and not re-run the specific filters on every single
    # string we already have in the database
    osp_id = raw_input("\nWhat is the osp_id for the results you would lke to filter: ")

    myconn = MatrixTDBConn('2')       # connect to MySQL server 

    # update database with specific filters, make sure they are associated with correct feature
    # groups, make sure language types are associated with right language types, then run
    # specific filters on an osp id provided by user and record all fails
    main(osp_id, myconn)
    myconn.close()                              # close connection to MySQL server

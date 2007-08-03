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

import MySQLdb
import sys
import datetime
#from filters import filter_one_result
from sql_lg_type import update_lt_in_lfg

# Edit this line to import different filters.

from s_filters import filter_list

# Global variables

last_group_id = None

#################################################################
# update_tables_for_filters(filter_list) makes sure that the
# tables in the DB defining the filters are up to date.
#
# We have to make sure that the filter is in the filter table,
# that the groups of feature-value pairs it uses are in the
# fv_groups table, and that the filter is linked to its groups
# through the fltr_groups table.


def update_tables_for_filters(filter_list):

    filter_id_hash = {}

    for f in filter_list:
        #print "filter name: " + f.name
        #print "filter group spec: " + str(f.fv)
        
        # Make sure it's in the filter table, and get its id.
        f_id = update_filter_table(f)

        # Make sure all of the groups it needs are in the groups table.
        groups = update_groups_table(f)

        # Make delete all of the existing rows for that filter in
        # groups_filter, and reinstantiate.
        update_group_filter_table(f_id,groups)

        # Make sure that all of the existing language types
        # are updated to see the new groups added, if any.

        update_all_lts_in_lfg()

        # Add filter to filter hash for quick access later
        filter_id_hash[f.name] = f_id

    return filter_id_hash

###################################################################
# Every time we add filters and feature groups, we should make
# sure that lt_feat_grp is up to date, too.

def update_all_lts_in_lfg():

        cursor.execute("SELECT lt_id FROM lt")
        lt_ids = cursor.fetchall()

        for lt_id in lt_ids:
            choices = create_choices_from_lt_id(lt_id[0])
            update_lt_in_lfg(choices,lt_id[0],cursor)

################################################################
# Sometimes we want to get a choices object out of the lt_feat_grp
# table, instead of a file

def create_choices_from_lt_id(lt_id):

    choices = {}

    cursor.execute("SELECT lfg_grp_id FROM lt_feat_grp WHERE lfg_lt_id = %s",(lt_id))
    fg_ids = cursor.fetchall()

    for fg_id in fg_ids:

        cursor.execute("SELECT fg_feat, fg_value FROM feat_grp WHERE fg_grp_id = %s",(fg_id[0]))
        fvs = cursor.fetchall()
        if len(fvs) == 1:

            #This is a singleton group, so add the info to choices
            fv = fvs[0]
            choices[fv[0]] = fv[1]

    return choices

#################################################################
# update_filter_table(filter): Make sure the filter itself is
# in the filter table, and get its id.

def update_filter_table(filter):
    # Make sure each filter is recorded in the filter table
    
    name = filter.name

    cursor.execute("SELECT filter_id FROM filter where filter_name = %s",(name))
    f_id = cursor.fetchone()
    if f_id == None:
        # Since "s" is the default value for filter_type, we don't need to specify it.
        cursor.execute("INSERT INTO filter SET filter_name = %s",(name))
        cursor.execute("SELECT LAST_INSERT_ID()")
        f_id = cursor.fetchone()[0]
    else:
        f_id = f_id[0]
        
    return f_id

#################################################################
# update_gropus_table(filter): Take the specification of feature-
# value groups from the filter definition and check whether those
# groups are defined in the feat_groups tables.  If not, add them.

def update_groups_table(filter):

    # print filter.name

    global last_group_id
    
    # Get the specification of filters.
    group_spec = filter.fv

    # Transform to disjunctive normal form
    gs = normalize_group_spec(group_spec)

    # Get starting id:
    cursor.execute("SELECT MAX(fg_grp_id) FROM feat_grp");
    last_group_id = cursor.fetchone()[0]
    if last_group_id == None:
        last_group_id = 0
    #print "last_group_id is: " + str(last_group_id)

    # Update the groups table as necessary, storing list of group_ids
    # in 'groups'

    groups = update_groups_table_helper(gs,[])

    return groups

#################################################################
# update_groups_table_helper(gs,groups): Recursive helper function
# for update_groups_table().

# **** START HERE **** This isn't respecting the AND groups
# ERB 2007-07-18 Fixed now?

def update_groups_table_helper(gs,groups):

    # print "Recursing, groups is: " + str(groups)

    # _FIX_ME_ there's some refactoring to do here.

    global last_group_id

    # Find the groups from the gs CNF

    # gs[0] = 'or', rest are either 'and'-tuples or strings
    if gs[0] == 'or':
        for g in gs[1:]:
            groups = update_groups_table_helper(g,groups)
        #print "case 1, returning groups as: " + str(groups)
        return groups
    
    # gs is ['f:v']
    if len(gs) == 1 or type(gs) == str:
        if len(gs) == 1:
            gs = gs[0]
        [f,v] = gs.split(':')
        #print "SELECT fg_grp_id FROM feat_grp WHERE fg_feat = %s and fg_value = %s", (f,v)
        cursor.execute("SELECT fg_grp_id FROM feat_grp WHERE fg_feat = %s and fg_value = %s", (f,v))
        g_id = cursor.fetchone()
        if g_id == None:
            last_group_id += 1
            cursor.execute("INSERT INTO feat_grp SET fg_grp_id = %s, fg_feat = %s, fg_value = %s", (last_group_id,f,v))
            groups.append(last_group_id)
        else:
            groups.append(g_id[0])
        #print "case 2, returning groups as: " + str(groups)
        return groups

    # gs is 'and'-tuple
    if gs[0] == 'and':
        #create complex query, with lists of variable length, depending on how many features there
        #are in the list.  We know there's at least two things on this list.
        from_string = 'FROM feat_grp AS fg1 '
        where_string = 'WHERE '
        feat_string = ''
        value_string = ''

        for i in range (2,len(gs)):
            j = i - 1
            from_string += ', feat_grp AS fg' + str(i) + ' '
            if i != 2:
                where_string += 'AND '

            where_string += 'fg' + str(j) + '.fg_grp_id = fg' + str(i) + '.fg_grp_id '

        for i in range (1,len(gs)):
            g = gs[i].split(':')
            feat_string += 'AND fg' + str(i) + '.fg_feat = "' + g[0] + '" '
            value_string += 'AND fg' + str(i) + '.fg_value = "' + g[1] + '" ' 

        prefix = 'SELECT feat_grp.fg_grp_id FROM feat_grp WHERE feat_grp.fg_grp_id IN (SELECT fg1.fg_grp_id '
        suffix = ') GROUP BY feat_grp.fg_grp_id HAVING count(*) = ' + str(len(gs)-1)
    
        query_string = prefix + from_string + where_string + feat_string + value_string + suffix
        #print query_string
        cursor.execute(query_string)

        g_id = cursor.fetchone()

        # See if we already have this group.  If not, add it.
        if g_id == None:
            last_group_id += 1
            for fv in gs[1:]: 
                [f,v] = fv.split(':')
                cursor.execute("INSERT INTO feat_grp SET fg_grp_id = %s, fg_feat = %s, fg_value = %s", (last_group_id,f,v))
            groups.append(last_group_id)
        else:
            groups.append(g_id[0])
        #print "case 3, returning groups as: " + str(groups)
        return groups

################################################################
# udpate_group_filter_table(f_id,groups): Update the table which
# relates filters to the groups of features they index.  In the
# general case, there are many feature groups per filter (logical
# or).

def update_group_filter_table(f_id,groups):

    #print "f_id is: " + str(f_id)
    #print "groups is: " + str(groups)
    
    #update fltr_lt each time, to make sure it stays consistent.
    cursor.execute("SELECT ffg_id FROM fltr_feat_grp WHERE ffg_fltr_id = %s",(f_id))
    fl_ids = cursor.fetchall()
    if not fl_ids == ():
        cursor.execute("DELETE FROM fltr_feat_grp WHERE ffg_fltr_id = %s",(f_id))

    # Now put in the current values.
    for g in groups:
        cursor.execute("INSERT INTO fltr_feat_grp SET ffg_fltr_id = %s, ffg_grp_id = %s",(f_id,g))

#################################################################
# normalize_group_spec(group_spec): Takes in the group specification
# from a filter definition and converts it to disjunctive normal
# form (an or of ands).

def normalize_group_spec(group_spec):

    #print "normalize input :" + str(group_spec)

    # These are nested lists with at least three elements each where the first element 
    # of each list is the string 'or' or 'and', and the remaining elements are either
    # strings of the form 'feature:value' or group_specs themselves.
    #
    # _FIX_ME_ Consider replacing this list-based version with a tree class and a parser
    # that takes a string with parens and parses it into a tree.

    # First handle the case where we have just one fv pair:

    if len(group_spec) == 1:
        if type(group_spec[0]) == str:
            return group_spec
        else:
            raise ValueError, "Ill-formed group spec."

    #Take possibly flat tree and make it binary for easier processing.

    gs = make_binary_gs(group_spec)

    finished = False

    #Because with a binary tree, one top-down pass isn't guaranteed to
    #give a normalized form.  So, keep calling helper function until there's
    #nothing left to do.  _FIX_ME_ I'm sure there's a more efficient way to do this.

    while finished == False:
        [gs,finished] = normalize_gs_helper(gs,True)
        #print "current gs :" + str(gs)

    gs = flatten_binary_gs(gs)

    return gs


#################################################################
# normalize_gs_helper(gs,finished): Recursive helper function for
# normalize_group_spec().  `finished' tracks whether there was
# anything to do on this pass.  _FIX_ME_ Perhaps there is some way
# to do it in one pass?

def normalize_gs_helper(gs,finished):

    #print "normalize input: " + str(gs)

    #if finished == False:
        #print "normalize output: " + str([gs,finished])
        #return [gs,finished]

    #Change finished to False as soon as we do something.  Then pass it back
    #through the recursive calls for this pass.

    #Base case: The gs is just a string.

    if type(gs) == str:
        #print "input to/output from case 1: " + str([gs,finished])
        return_value = [gs,finished]

    #Otherwise, we have something to recursively handle.

    else:
        left_dtr = gs[1]
        right_dtr = gs[2]

        #Operator is an or.  Recurse through daughters.
        if gs[0] == 'or':
            #print "input to case 2: " + str([gs,finished])
            [new_left_dtr,f1] = normalize_gs_helper(left_dtr,finished)
            [new_right_dtr,f2] = normalize_gs_helper(right_dtr,f1)
            new_gs = ['or', new_left_dtr, new_right_dtr]
            return_value = [new_gs,f2]
            #print "return value from case 2: " + str(return_value)

        #If we've made it this far, operator is 'and'.

        #Dtr operators are 'and'. Recurse through daughters.
        elif ((left_dtr[0] == 'and' or type(left_dtr) == str) and
              (right_dtr[0] == 'and' or type(right_dtr) == str)):
            #print "input to case 3: " + str([gs,finished])
            [new_left_dtr,f1] = normalize_gs_helper(left_dtr,finished)
            [new_right_dtr,f2] = normalize_gs_helper(right_dtr,f1)
            new_gs = ['and', new_left_dtr, new_right_dtr]
            return_value = [new_gs,f2]
            #print "return value from case 3: " + str(return_value)
        
        #Now the cases where we have to change something.

        #Operator is 'and'.  Left dtr is 'or', right dtr is string.
        elif (left_dtr[0] == 'or' and type(right_dtr) == str):
            #print "input to case 4: " + str([gs,finished])
            finished = False
            ##print "recursing through: " + str(['and',left_dtr[1],right_dtr])
            [new_left_dtr,foo] = normalize_gs_helper(['and',left_dtr[1],right_dtr],False)
            ##print "recursing through: " + str(['and',left_dtr[2],right_dtr])
            [new_right_dtr,foo] = normalize_gs_helper(['and',left_dtr[2],right_dtr],False)
            new_gs = ['or',new_left_dtr,new_right_dtr]
            return_value = [new_gs,finished]
            #print "return value from case 4: " + str(return_value)

        #Operator is 'and'.  Left dtr is string, right dtr is 'or'.
        elif (right_dtr[0] == 'or' and type(left_dtr) == str):
            #print "input to case 5: " + str([gs,finished])
            finished = False
            [new_right_dtr,foo] = normalize_gs_helper(['and',left_dtr,right_dtr[1]],False)
            [new_left_dtr,foo] = normalize_gs_helper(['and',left_dtr,right_dtr[2]],False)
            new_gs = ['or',new_left_dtr,new_right_dtr]
            return_value = [new_gs,finished]
            #print "return value from case 5: " + str(return_value)

        #Operator is 'and'.  Both dtrs are 'or'.
        elif (right_dtr[0] == 'or' and left_dtr[0] == 'or'):  #probably redundant to check
            #print "input to case 6: " + str([gs,finished])
            finished = False
            [dtr1,foo] = normalize_gs_helper(['and',left_dtr[1],right_dtr[1]],False)
            [dtr2,foo] = normalize_gs_helper(['and',left_dtr[1],right_dtr[2]],False)
            [dtr3,foo] = normalize_gs_helper(['and',left_dtr[2],right_dtr[1]],False)
            [dtr4,foo] = normalize_gs_helper(['and',left_dtr[2],right_dtr[2]],False)
            new_gs = ['or',dtr1,dtr2,dtr3,dtr4]
            binary_new_gs = make_binary_gs(new_gs)
            return_value = [binary_new_gs,finished]
            #print "return value from case 6: " + str(return_value)

        #Operator is 'and'.  Left dtr is 'or'.  Right dtr is 'and'.
        elif (left_dtr[0] == 'or' and right_dtr[0] == 'and'):
            finished = False
            flat_dtr_1 = ['and',left_dtr[1],right_dtr[1],right_dtr[2]]
            flat_dtr_2 = ['and',left_dtr[2],right_dtr[1],right_dtr[2]]
            bin_dtr_1 = make_binary_gs(flat_dtr_1)
            bin_dtr_2 = make_binary_gs(flat_dtr_2)
            [dtr1,foo] = normalize_gs_helper(bin_dtr_1,False)
            [dtr2,foo] = normalize_gs_helper(bin_dtr_2,False)
            new_gs = ['or',dtr1,dtr2]
            return_value = [new_gs,finished]

        #Operator is 'and'.  Left dtr is 'and'.  Right dtr is 'or'.
        elif (left_dtr[0] == 'and' and right_dtr[0] == 'or'):
            finished = False
            flat_dtr_1 = ['and',right_dtr[1],left_dtr[1],left_dtr[2]]
            flat_dtr_2 = ['and',right_dtr[2],left_dtr[1],left_dtr[2]]
            bin_dtr_1 = make_binary_gs(flat_dtr_1)
            bin_dtr_2 = make_binary_gs(flat_dtr_2)
            [dtr1,foo] = normalize_gs_helper(bin_dtr_1,False)
            [dtr2,foo] = normalize_gs_helper(bin_dtr_2,False)
            new_gs = ['or',dtr1,dtr2]
            return_value = [new_gs,finished]


        else:
            print gs
            raise ValueError, "Something is wrong with Group_spec."

    #print "return value: " + str(return_value)
    return return_value

###########################################################################
# filter_one_result copied & modified from filters.py

def filter_one_result(mrs_id, sent,filter_list,filter_id_hash):

    filter_values = {}

    for f in filter_list:
        filter_values[filter_id_hash[f.name]] = f.exe(mrs_id, sent)

    return filter_values


#################################################################
# make_binary_gs(group_spec): Takes group_spec with arbitrary
# arity and converts it to binary format.

def make_binary_gs(group_spec):

    #print "Passed in " + str(group_spec)
    l = len(group_spec)
    #print "Length is " + str(l)
    
    # Check syntax of group_spec. 
    if not type(group_spec) == list:
        raise ValueError, "Group_spec is not a list."
    if len(group_spec) < 3:
        raise ValueError, "Group_spec does not have enough elements (must be at least 3)."
    if not (group_spec[0] == 'and'
            or group_spec[0] == 'or'):
        raise ValueError, "Group_spec does not being with 'and' or 'or'."

    # Check if we have more than two daughters

    if len(group_spec) > 3:
        #print "group_spec is :" + str(group_spec)
        op = group_spec[0]
        #print "op is :" + str(op)
        left_dtr = group_spec[1]
        right_dtr = [op] + group_spec[2:]
        #print "making binary: " + str(right_dtr)
        binary_right_dtr = make_binary_gs(right_dtr)
        return_value = [op,left_dtr,binary_right_dtr]


    # If we have just two daughters

    if len(group_spec) == 3:

        #print "gs[0] is :" + str(group_spec[0])

        if type(group_spec[1]) == str:
            left_dtr = group_spec[1]
        else:
            #print "making binary: " + str(group_spec[1])
            left_dtr = make_binary_gs(group_spec[1])

        if type(group_spec[2]) == str:
            right_dtr = group_spec[2]
        else:
            #print "making binary: " + str(group_spec[2])
            right_dtr = make_binary_gs(group_spec[2])


        return_value = [group_spec[0]]
        return_value += [left_dtr,right_dtr]

    #print "now binary: " + str(return_value)
    return return_value

#################################################################
# flatten_binary_gs(gs): Takes group_spec in binary format and
# flattens it.  Result should be at most two layers deep (an
# or of ands).
     
def flatten_binary_gs(gs):

    #print "flatten input: " + str(gs)

    # base case

    if type(gs) == str:
        #print "flatten output: " + str([gs])
        return [gs]

    op = gs[0]
    new_dtrs = [op]
    for d in gs[1:]:

        #print "dtr: " + str(d)
        if type(d) == str:
            new_dtrs.append(d)
        else:
            d = flatten_binary_gs(d)
            if d[0] == op:
                #[or1, [or2, [and, a, b] [and, c, d]], e]
                #op = or1
                #first nd = [or2, [and, a, b] [and, c, d]]
                #second nd = 3
                # Same operator, so we need to promote each of those dtrs a level,
                # while also flattening them.
                #print "dtr's op: " + str(d[0])
                for nd in d[1:]:
                    new_dtrs.append(nd)

                    #if type(nd) == str:

                    #else:
                #new_dtrs += [flatten_binary_gs(nd)]
                #print "new dtrs: " + str(new_dtrs)
            else:
                # Different daughter, so don't promote, but do flatten recursively.
                new_dtrs.append(flatten_binary_gs(d))

    #print "flatten output: " + str(new_dtrs)

    return new_dtrs
            

#######################################################################
# make_string: for constructing the main select query

def make_string(limit):

    output = "SELECT r_result_id, r_mrs, i_input FROM result, item  WHERE result.r_parse_id = item.i_id AND r_wf = 1  and result.r_result_id < 12000000 LIMIT " + str(limit) + ", 100000"
    return output



############################################################################
# Main program

# Set up the cursor

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

cursor = db.cursor()
    



def main():

    # First check whether the filters are properly recorded in
    # filter and fltr_lt

    filter_id_hash = update_tables_for_filters(filter_list)

    # Then pull all strings that pass the universal filters, and record
    # their values for the specific filters.  Perhaps update this if
    # we're moving the non universally ungrammatical strings to another
    # table.

    # ERB 2007-06-22 Picking up where I left off in a run that crashed.

    f_string = make_string(7000000)

    cursor.execute(f_string)
    ids = cursor.fetchall()

    limit = 7100000

    while ids != ():

        print "Working on results " + str(limit-100000) + " through " + str(limit)

        for id in ids:
            (res_id, mrs_id, string) = id

            filter_values = filter_one_result(mrs_id,string,filter_list,filter_id_hash)

            for f_id in filter_values.keys():
                value = filter_values[f_id]

                if value != 2:
                    cursor.execute("INSERT LOW_PRIORITY INTO res_sfltr SET rsf_res_id = %s, rsf_sfltr_id = %s, rsf_value = %s",
                                   (res_id, f_id, value))

        else:
            print "Updated item " + str(res_id) + " at " + str(datetime.datetime.now())

        f_string = make_string(limit)
        cursor.execute(f_string)
        ids = cursor.fetchall()
        limit += 100000


if __name__ == "__main__":
    main ()
    #update_all_lts_in_lfg()


# Testing:

#print test

#print make_binary_gs(test)
#print flatten_binary_gs(test)
#gs =  normalize_group_spec(test)
#print(gs)
#update_groups_table_helper(gs,[])


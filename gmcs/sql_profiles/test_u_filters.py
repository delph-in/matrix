#!/usr/local/bin/python2.5

###############################################################################
# Utility for testing filters.  Allows user to pass in a particular string of
# interest and see what each filter does with it.
#
# Usage: python test_u_filters <string> (<filter name>*)


###############################################################################
# Preliminaries

import MySQLdb
import filters
from u_filters import filter_list
import sys

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

###############################################################################
# Main program

# Find the results containing that string in the DB

input = sys.argv[1]
fs = []

if len(sys.argv) > 2:
    for i in range (2,len(sys.argv)):
        f = sys.argv[i]
        fs.append(f)

cursor.execute("SELECT r_result_id,r_mrs_tag FROM result,parse,item WHERE item.i_input = %s AND result.r_parse_id = parse.p_parse_id AND parse.p_i_id = item.i_id", (input))

# List of r_result_id, r_flags tuples:

candidates = cursor.fetchall()

if len(candidates) == 0:
    print "Sorry, no results match the string you requested."

for cand in candidates:

    mrs_id = cand[1]
    print mrs_id
    r_id = cand[0]
    filter_values = filters.filter_one_result(mrs_id,input,filter_list)

    print_values = []

    if len(fs) > 0:
        for key in fs:
            print_value = [key, filter_values[key]]
            print_values.append(print_value)
    else:
        print_values = filter_values

    print str(r_id) + " : " + str(print_values)
    

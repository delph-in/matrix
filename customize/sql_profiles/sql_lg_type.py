###############################################################
# Read in choices file and create appropriate representations
# in MatrixTDB.lt,lt_grp.

# python sql_lg_type.py <choices file> [r|p]
# r = random lt
# p = purpose-built lt

# TODO
# 1. Fix this so that we also store the f-v pairs and can update
# lt_grp for an existing language type in light of new groups
# (which come in with new filters)

import sys
import MySQLdb
sys.path.append("..")
from utils import read_choices

###############################################################
# check_lt_for_fvs(fvs): Check whether all of the feature-value
# pairs in a group are represented in the language type.

def check_lt_for_fvs(fvs):

    for fv in fvs:
        (f,v) = fv
        if (not choices.has_key(f) or
            not choices[f] == v):
            return False

    return True

###############################################################
# Main Program


choices = read_choices(sys.argv[1])

# Set up the cursor

db = MySQLdb.connect(host="localhost", user="ebender",
                      passwd="tr33house", db="MatrixTDB")

cursor = db.cursor()

# Add a row for the language type

cursor.execute("INSERT INTO lt SET lt_origin = %s", sys.argv[2])
cursor.execute("SELECT LAST_INSERT_ID()")
lt_id = cursor.fetchone()[0]

# Get all feature groups from DB

cursor.execute("SELECT fg_grp_id FROM feat_grp")
g_ids = cursor.fetchall()

print g_ids

for g_id in g_ids:

#    print lt_id
#    print g_id
#    print type(g_id)
#    print len(g_id)
#    print g_id[0]

    # Strip off extra layer of parens
    g_id = g_id[0]

#    print g_id

    # Get all of the feature-value pairs in a group
    cursor.execute("SELECT fg_feat, fg_value FROM feat_grp WHERE fg_grp_id = %s",(g_id))
    fvs = cursor.fetchall()

    # Check if they are all in the lt definition
    if check_lt_for_fvs(fvs):
        cursor.execute("INSERT INTO lt_feat_grp SET lfg_lt_id = %s, lfg_grp_id =%s",(lt_id,g_id))

# Okay, we're done.  Print out the lt_id for future reference.

print "Language type id (lt_id) is: " + str(lt_id)

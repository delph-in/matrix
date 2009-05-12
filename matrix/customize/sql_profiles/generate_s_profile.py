###############################################################
# Script to generate [incr tsdb()] profile with gold-standard
# for some language type on the basis of MatrixTDB and a lt_id.

# python generate_s_profile.py <lt_id>

import sys
import MySQLdb

lt_id = sys.argv[1]

# Set up the cursor

db = MySQLdb.connect(host="localhost", user="ebender",
                      passwd="tr33house", db="MatrixTDB")

cursor = db.cursor()

# Find all of the groups in the language type

cursor.execute("SELECT ffg_fltr_id FROM lt_feat_grp,fltr_feat_grp WHERE lt_feat_grp.lfg_lt_id = %s AND lt_feat_grp.lt_grp_id = fltr_feat_grp.ffg_grp_id",(lf_id))
g_ids = cursor.fetchall()

# Now find all of the filters that are relevant

f_ids = []

for g_id in g_ids:
    cursor.execute("SELECT ffg_fltr_id FROM fltr_feat_grp WHERE ffg_grp_is

###############################################################
# Read in choices file and create appropriate representations
# in MatrixTDB.lt,lt_grp.

# python sql_lg_type.py <choices file> [r|p]
# r = random lt
# p = purpose-built lt

# ERB 2007-06-06 Updating this so that we put in singleton
# feature groups for all f:v pairs in the lt definition that aren't
# already in the DB.  These won't (initially anyway) have
# filters that care about them, but I don't see that that will hurt
# anything.

# When we take a lt and look for its filters, we get all the
# feature groups that correspond to the language type, and then
# get all of the filters that correspond to those feature groups.

# Then I can define a function 'update_lt_in_lfg' that
# looks at the lt definition in terms of fv pairs and finds all
# new groups that should be added for it.

# mysql> show columns in lt;
# +-----------+---------+------+-----+---------+----------------+
# | Field     | Type    | Null | Key | Default | Extra          |
# +-----------+---------+------+-----+---------+----------------+
# | lt_id     | int(11) | NO   | PRI | NULL    | auto_increment |
# | lt_origin | char(2) | YES  |     | NULL    |                |
# +-----------+---------+------+-----+---------+----------------+

# mysql> show columns in feat_grp;
# +-----------+----------+------+-----+---------+----------------+
# | Field     | Type     | Null | Key | Default | Extra          |
# +-----------+----------+------+-----+---------+----------------+
# | fg_id     | int(11)  | NO   | PRI | NULL    | auto_increment |
# | fg_grp_id | int(11)  | YES  | MUL | NULL    |                |
# | fg_feat   | char(20) | YES  | MUL | NULL    |                |
# | fg_value  | char(20) | YES  |     | NULL    |                |
# +-----------+----------+------+-----+---------+----------------+

# mysql> show columns in fltr_feat_grp;
# +-------------+---------+------+-----+---------+----------------+
# | Field       | Type    | Null | Key | Default | Extra          |
# +-------------+---------+------+-----+---------+----------------+
# | ffg_id      | int(11) | NO   | PRI | NULL    | auto_increment |
# | ffg_fltr_id | int(11) | YES  | MUL | NULL    |                |
# | ffg_grp_id  | int(11) | YES  | MUL | NULL    |                |
# +-------------+---------+------+-----+---------+----------------+

# mysql> show columns in lt_feat_grp;
# +------------+---------+------+-----+---------+----------------+
# | Field      | Type    | Null | Key | Default | Extra          |
# +------------+---------+------+-----+---------+----------------+
# | lfg_id     | int(11) | NO   | PRI | NULL    | auto_increment |
# | lfg_lt_id  | int(11) | YES  | MUL | NULL    |                |
# | lfg_grp_id | int(11) | YES  | MUL | NULL    |                |
# +------------+---------+------+-----+---------+----------------+


import sys
import MySQLdb
sys.path.append("..")
from utils import read_choices

###############################################################
# check_lt_for_fvs(fvs,choices): Check whether all of the feature-value
# pairs in a group are represented in the language type.

def check_lt_for_fvs(fvs,choices):

    for fv in fvs:
        (f,v) = fv
        if (not choices.has_key(f) or
            not choices[f] == v):
            return False

    return True

###############################################################
# check_lt_for_grp_ids(grp_ids,choices): Check whether all of the feature-value
# pairs in a set of grp_ids (e.g., from an existing lt) are
# represented in a (new) set of choices.

def check_lt_for_grp_ids(grp_ids,choices):

    # grp_ids is one of those obnoxious tuples from cursor.fetchal()

    for grp_id_tuple in grp_ids:
        grp_id = grp_ids[0]

        cursor.execute("SELECT fg_feat, fg_value FROM feat_grp WHERE fg_grp_id = %s",(grp_id))

        # Just check the first feature from each feature group
        # since the non-singleton groups should all be redundant
        # to the singleton groups for these purposes.  This is
        # cheaper than deciding whether each group is singleton
        # before we check it.

        (f,v) = cursor.fetchone()
        if (not choices.has_key(f) or
            not choices[f] == v):
            return False

    return True

###############################################################
# check_existing_lt_for_completeness(lt_id,choices): Now go
# the other way: Check whether all of the fv pairs in choices
# are already in the lt.

def check_existing_lt_for_completeness(lt_id,choices):

    for f in choices.keys():
        cursor.execute("SELECT fg_grp_id FROM feat_grp, lt_feat_grp WHERE fg_feat = %s AND fg_value = %s AND lfg_grp_id = fg_grp_id AND lfg_lt_id = %s",(f,v,lt_id))
        res = cursor.fetchall()
        if len(res) == 0: # we found a fv pair which isn't already in lfg_feat_grp for the lt.
            return False

    return True

###############################################################
# lt_exists(choices): Check whether we've already created
# the language type represented by the f:v dictionary choices

def lt_exists(choices):

    # Check existing language types until we find one that
    # is consistent (i.e., subsumed by choices)

    cursor.execute("SELECT lt_id FROM lt")
    lt_ids = cursor.fetchall

    for lt_id_tuple in lt_ids:  
        lt_id = lt_id_tuple[0]


        cursor.execute("SELECT lfg_grp_id FROM lt_feat_grp WHERE lfg_lt_id = %s",(lt_id))
        grp_ids = cursor.fetchall()

        if check_lt_for_grp_ids(grp_ids,choices):

            # Now make sure that all of the information in choices is
            # also in that lt.

            if check_existing_lt_for_completeness(grp_ids,choices):
                return True

    return False
        

###############################################################
# singleton_group_exists(f,v): Check whether the pair f:v is
# already in as a singleton group

def singleton_group_exists(f,v):

    cursor.execute("SELECT fg_grp_id FROM feat_grp WHERE fg_feat = %s and fg_value = %s", (f,v))
    grp_ids = cursor.fetchall()

    if len(grp_ids) == 0: # No entries at all for that fv pair
        return False

    for grp_id in grp_ids:
        cursor.execute("SELECT fg_id FROM feat_grp WHERE fg_grp_id = %s",(grp_id[0]))
        rows = cursor.fetchall()
        if len(rows) == 1: # Found an entry that is the sole member of its group
            return True

    return False # All entries were in groups with other features.
    

###############################################################
# update_feat_group(choices): Make sure that all f:v pairs
# are in feat_group as singleton groups.

def update_feat_group(choices):

    for f in choices.keys():
        v = choices[f]
        if not singleton_group_exists(f,v):
            cursor.execute("SELECT MAX(fg_grp_id) FROM feat_grp")
            fg_grp_id = cursor.fetchone()[0]
            fg_grp_id = fg_grp_id + 1
            cursor.execute("INSERT INTO feat_group SET fg_grp_id = %s and fg_feat = %s and fg_value = %s",(fg_grp_id,f,v))

###############################################################
# update_lt_in_lfg(choices,lt_id): Update the lt_feat_grp table to
# reflect all currently defined feature groups which correspond
# to this language type.

def update_lt_in_lfg(choices,lt_id):

    # Get all feature groups from DB

    cursor.execute("SELECT fg_grp_id FROM feat_grp")
    g_ids = cursor.fetchall()

    for g_id in g_ids:

        # Strip off extra layer of parens
        g_id = g_id[0]

        # Get all of the feature-value pairs in a group
        cursor.execute("SELECT fg_feat, fg_value FROM feat_grp WHERE fg_grp_id = %s",(g_id))
        fvs = cursor.fetchall()

        # Check if they are all in the lt definition
        if check_lt_for_fvs(fvs,choices):
            cursor.execute("INSERT INTO lt_feat_grp SET lfg_lt_id = %s, lfg_grp_id =%s",(lt_id,g_id))



###############################################################
# create_or_update_lt(choices): Given a new language type described
# in a dictionary of f:v pairs, create an entry in lt, put
# all of the singleton f:v pairs into feat_group (if they're
# not already there, and update lt_lfg to list all of the
# feature groups appropriate for this language type.

def create_or_update_lt(choices):

    # Check if the language type already exists, if so,
    # call update_lt_in_lfg instead and return lt_id.

    lt_id = lt_exists(choices)  # Return lt_id or False
    if lt_id:
        update_lt_in_lfg(choices,lt_id)
        return lt_id

    # Put all f:v pairs in to feat_group as singleton groups

    update_feat_group(choices)
        
    # Add a row for the language type
    
    cursor.execute("INSERT INTO lt SET lt_origin = %s", sys.argv[2])
    cursor.execute("SELECT LAST_INSERT_ID()")
    lt_id = cursor.fetchone()[0]

    # Update lt_feat_group and return lt_id

    update_lt_in_lfg(choices,lt_id)
    return lt_id


###############################################################
# Main Program


choices = read_choices(sys.argv[1])

# Set up the cursor

db = MySQLdb.connect(host="localhost", user="ebender",
                      passwd="tr33house", db="MatrixTDB")

cursor = db.cursor()

lt_id = create_or_update_lt(choices)

# Okay, we're done.  Print out the lt_id for future reference.

print "Language type id (lt_id) is: " + str(lt_id)

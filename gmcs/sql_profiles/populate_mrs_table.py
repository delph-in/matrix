#######################################################################
# Short script for populating the mrs table in MatrixTDB.  Uses result,
# parse, item from harvester profile (as read into MatrixTDB) and
# str_lst (string list, giving mrs_ids for each harvester string).
# Puts actual MRSs into mrs table linked to mrs_ids.

# mysql> show columns in mrs;
# +-----------+---------------+------+-----+---------+-------+
# | Field     | Type          | Null | Key | Default | Extra |
# +-----------+---------------+------+-----+---------+-------+
# | mrs_tag   | char(20)      | YES  |     | NULL    |       |
# | mrs_value | varchar(1000) | YES  |     | NULL    |       |
# +-----------+---------------+------+-----+---------+-------+

# At the same time, we should add the mrs_ids to the result table.
# Perhaps replace the actual MRS with the mrs_ids?  For now, storing
# mrs_id in r_mrs_tag.

# mysql> show columns in result;
# +--------------+---------------+------+-----+---------+-------+
# | Field        | Type          | Null | Key | Default | Extra |
# +--------------+---------------+------+-----+---------+-------+
# | r_parse_id   | int(11)       | NO   |     |         |       |
# | r_result_id  | int(11)       | YES  |     | NULL    |       |
# | r_time       | int(11)       | YES  |     | NULL    |       |
# | r_ctasks     | int(11)       | YES  |     | NULL    |       |
# | r_ftasks     | int(11)       | YES  |     | NULL    |       |
# | r_etasks     | int(11)       | YES  |     | NULL    |       |
# | r_stasks     | int(11)       | YES  |     | NULL    |       |
# | r_size       | int(11)       | YES  |     | NULL    |       |
# | r_aedges     | int(11)       | YES  |     | NULL    |       |
# | r_pedges     | int(11)       | YES  |     | NULL    |       |
# | r_derivation | varchar(1000) | YES  |     | NULL    |       |
# | r_surface    | varchar(1000) | YES  |     | NULL    |       |
# | r_tree       | varchar(1000) | YES  |     | NULL    |       |
# | r_mrs        | varchar(1000) | YES  |     | NULL    |       |
# | r_mrs_tag    | char(20)      | YES  |     | NULL    |       |
# +--------------+---------------+------+-----+---------+-------+

########################################################################
# Preliminaries

import MySQLdb

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

########################################################################
# Main program

# Get list of harvester string ids

cursor.execute("SELECT sl_mrs_tag, seed_str_value FROM seed_str, str_lst WHERE str_lst.sl_seed_type = %s and str_lst.sl_seed_id = seed_str.seed_id", ("h"))

pairs = cursor.fetchall()

# This next bit is a hack because the inputs aren't unique.  Not sure
# we can work around this so well, but will need to rethink when it comes
# to adding more harvester strings to the DB.

for pair in pairs:
    input = pair[1]

    # Get the i_id corresponding to the string
    cursor.execute("SELECT i_id FROM item WHERE i_input = %s",(input))
    ids = cursor.fetchall()
    if len(ids) > 1:
        print "Error: More than one item with the same i_input."
        sys.exit
    i_id = ids[0]

    # Now find the results corresponding to the i_id.  Assuming
    # this is unique for now.

    cursor.execute("SELECT r_mrs,r_result_id FROM parse, result WHERE parse.p_i_id = %s AND parse.p_parse_id = result.r_parse_id", (i_id))
    mrss = cursor.fetchall()
    if len(mrss) > 1:
        print "Error: More than one mrs associated with harvester string."
        sys.exit
    mrs = mrss[0]
    mrs_value = mrs[0]
    r_result_id = mrs[1]

    # insert mrs_id, mrs_string into mrs

    mrs_id = pair[0]
    cursor.execute("INSERT INTO mrs SET mrs_tag = %s, mrs_value = %s",
                   (mrs_id,mrs_value))

    # insert mrs_id into result.r_mrs_tag

    cursor.execute("UPDATE result SET r_mrs_tag = %s WHERE r_result_id = %s",
                   (mrs_id,r_result_id))

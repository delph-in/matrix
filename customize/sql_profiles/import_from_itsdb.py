############################################################
# Script for importing data from [incr tsdb()] profile into
# MatrixTDB.

# Usage:

# python import_from_itsdb.py <dir> <h-m> <choices>

# Input:
#
# 1) <dir>: Directory containing [incr tsdb()] profile with harvester
# strings parsed by harvester grammar.
#
# 2) <h-m>: File linking harvester strings to mrs_tags.
#
# 3) <choices>: Choices file from which the harvester grammar was
# created.

# First, we look through the harvester-strings to make sure
# that they each correspond to an i_input field in item.
# If not, return an error.

# Then we look through the harvester-mrs_tag pairs and
# see if any of the mrs_tags are already known.  If so,
# we query the user, to determine which case we're in:

# 1) Inadvertent reuse of mrs_tag; mrs_tag corresponds to
# a different harvester string than before

# 1a) User means for it to be a different mrs, and needs
# to be a new name.

# 1b) User means for it to be the same mrs, and should
# be modifying modstring.py rather than adding a new
# harvester string

# 2) mrs_tag and harvester string are already known and
# already in correspondence. User means to update the
# mrs_value.

# Case 1) should result in the user getting an error
# message with directions about what to do.

# Case 2) should result in a new entry in MatrixTDB.mrs
# pairing the mrs_tag with the new mrs_value.  The
# old entry in MatrixTDB.mrs should be updated so that
# the mrs_current is now - and mrs_date is the current
# date.

# If there are any new MRSs to store (i.e., we are in case
# 2 above and/or there are new mrs_tags listed), then:

# 1) Look up or create LT entry in lt and lt_feat_grp
# 2) Ask user for comment to store about this source profile
# 3) Create a new row in orig_source_profile with information
# about this source profile.  
# 4) Update harvester string table with strings and mrs_tags
# 5) Update mrs table with mrs_tags and mrs_values
# 6) Add rows from tsdb_profile/item,parse,result to sp_item,
# sp_parse, sp_result, replacing mrs in sp_result with mrs_tag
# 7) Print out osp_orig_src_prof_id for user to use as input
# to add_permutes.py
# 8) Print out message about updating g.py to map include ne
# mrs_tags in the mrs_tag sets they belong in, and updating
# u_filters.py and s_filters.py to handle new strings, and updating
# stringmod.py as well.

##########################################################################
# Preliminaries

import shutil
import sys
import os
import datetime
import MySQLdb

sys.path.append("..")

from validate import validate_choices
from sql_lg_type import create_or_update_lt
from utils import read_choices

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()


###########################################################################
# Validating file relating harvester strings to mrs.
# Check format (@ delimited)
# Check that each i_input in item corresponds to a row in harv_mrs file
# and vice versa
# Check that each harvester string is unique (within file) and that
# each mrs_tag is unique (within file).

def validate_string_list(harv_mrs,dir):

  mrs_dict = {}

  f = open(harv_mrs)
  for l in f.readlines():
    l = l.strip()
    l = l.split('@')
    if len(l) != 2:
        raise ValueError, "File " + harv_mrs + "is not a well-formed input file. Should be mrs_tag@string"
    mrs_dict[l[1]] = l[0]
  f.close()

  items = []
  
  i = open(dir + "item", 'r')
  for l in i.readlines():
      l = l.strip()
      l = l.split('@')
      items.append(l[6])

  for item in items:
      if not mrs_dict.has_key(item):
          raise ValueError, "Item " + item + " in " + dir + "item is not represented in " + harv_mrs + "."

  mrs_keys = mrs_dict.keys()

  for string in mrs_keys:
      if items.count(string) == 0:
          raise ValueError, "Item " + string + " in " + harv_mrs + " is not represented in " + dir + "item."
      if mrs_keys.count(string) > 1:
          raise ValueError, "Item " + string + " appears more than once in " + harv_mrs + "."

  mrs_tags = mrs_dict.values()

  for tag in mrs_tags:
      if mrs_tags.count(tag) > 1:
          raise ValueError, "Mrs tag " + string + " appears more than once in " + harv_mrs + "."
  
  return mrs_dict



###########################################################################
# Checking for known mrs_tags and deciding what to do

def check_for_known_mrs_tags(mrs_dict):

    new_mrs_tags = []
    known_mrs_tags = []

    cursor.execute("SELECT mrs_tag FROM mrs")
    mrs_tuple = cursor.fetchall()

    db_tags = []

    for tup in mrs_tuple:
        db_tags += tup[0]

    input_tags = mrs_dict.values()
    input_strings = mrs_dict.keys()

    for tag in input_tags:
        if db_tags.count(tag) > 0:
            known_mrs_tags += tag
        else:
            new_mrs_tags += tag

    if len(known_mrs_tags) == 0:
        res  =  raw_input("All of the mrs_tags you are reporting are new.\n If this is right, press 'y' to continue.  Press any other key to abort. ")
        if res != 'y':
            sys.exit()
            
    if len(known_mrs_tags) > 0:

        # We have some known mrs tags.  Find out if they correspond to
        # the same string as before or not.
        same_string_tags = []
        diff_string_tags = []

        for tag in known_mrs_tags:
            # We can assume that each mrs_tag appears only once
            # in mrs_dict, because we checked in validate_string_list()
            newstring = True
            for string in input_strings:
                if mrs_dict[string] == tag:
                    same_string_tags += tag
                    newstring = False
                if newstring:
                    diff_string_tags += tag

        if len(diff_string_tags) > 0:
            print "The following mrs_tags are already in MatrixTDB with\n different harvester strings associated with them.\n"
            print diff_string_tags
            print "\n"
            print "Either you have inadvertently reused an mrs_tag or you\n should be modifying stringmod.py rather than\n adding harvester strings.\n"
            print "MatrixTDB has not been modified.\n"
            sys.exit()
        elif len(same_string_tags) > 0:
            print "The following mrs_tags already exist in MatrixTDB with\n the harvester string indicated.\n"
            res = raw_input("If you mean to update the MRSs associated with them,\n press 'y' to continue (any other key will abort): ")
            if res != 'y':
                sys.exit()

            if len(new_mrs_tags) > 0:
                print "In addition, you are adding hte follwoing new mrs tags:\n"
                print new_mrs_tags
                print "\n"
                res = raw_input("If this is correct, press 'y' to continue (any other key to abort): ")
                if res != 'y':
                    sys.exit()

    return [new_mrs_tags,known_mrs_tags]


###########################################################################
# Create new row in orig_source_profile

def update_orig_source_profile(lt_id):

  user = os.environ['USER']
  comment = raw_input("Enter a description of this source profile (1000 char max) :")
  t = datetime.datetime.now()
  timestamp = t.strftime("%Y-%m-%d %H:%M")

  cursor.execute("INSERT INTO orig_source_profile SET osp_developer_name = %s, osp_prod_date = %s, osp_orig_lt_id = %s and osp_comment = %s", (user,timestamp,lt_id,comment))
  cursor.execute("SELECT LAST_INSERT_ID()")
  osp_id = cursor.fetchone()[0]

  return [osp_id,timestamp]

###########################################################################
# Update harvester string table with strings and mrs_tags

def update_harv_str(new_mrs_tags,known_mrs_tags,mrs_dict,osp_id):

  # Assume that harv_str has fields hs_id (auto increment), hs_string, hs_mrs_tag and hs_init_osp_id and hs_cur_osp_id
  for tag in new_mrs_tags:
    harv = find_string(tag,mrs_dict)
    cursor.execute("INSERT INTO harv_str SET hs_string = %s,  hs_mrs_tag = %s, hs_init_osp_id = %s, hs_cur_osp_id = %s",(harv,tag,osp_id,osp_id))

  # For known mrs_tags that we're going over, update hs_current_osp_id to correspond to current source profile

  for tag in known_mrs_tags:
    cursor.execute("UPDATE INTO harv_str SET hs_cur_osp_id = %s WHERE hs_mrs_tag = %s",(osp_id,tag))
    
    

###########################################################################
# Find string corresponding to a given mrs_tag.  Need a little subroutine
# here because the strings are the keys in mrs_dict, not vice versa.  Also
# Note that we can assume that the tags are unique in mrs_dict.values(), i.e.,
# no two strings have the same tag as their value.

def find_string(tag,mrs_dict):

  strings = mrs_dict.values()
  for s in strings:
    if mrs_dict[s] == tag:
      return True

  raise ValueError, "find_string() was passed an mrs tag with no corresponding string."

###########################################################################
# Update mrs table with mrs_tags and mrs_values
# _FIX_ME_ Still assuming one mrs per harvester string, but this doesn't
# allow for ambiguous harvester strings, which we will surely arrive at one day.

def update_mrs(mrs_dict,osp_id,new_mrs_tags,known_mrs_tags,dir,timestamp):

  # Read in info from itsdb files

  mrs_values = {}
  harv_id = {}
  parse_id = {}
  input_strings = mrs_dict.keys()
  
  i = open(dir + "item", 'r')
  for l in i.readlines():
    l = l.strip()
    l = l.split('@')
    harv_id[l[6]] = l[0]  #key is i_input, value is i_id
  i.close()

  p = open(dir + "parse", 'r')
  for l in p.readlines():
    l = l.strip()
    l = l.split('@')
    parse_id[l[2]] = l[0] #key is p_i_id, value is parse_id
  p.close()

  r = open(dir + "result", 'r')
  for l in r.readlines():
    l = l.strip()
    l = l.split('@')
    mrs_values[l[0]] = l[13] #key is parse_id, value is mrs_value
  r.close()
       
  # For new mrs tags, create a new entry in mrs:

  for tag in new_mrs_tags:

    i_input = find_string(tag,mrs_dict)
    i_id = harv_id[i_input]
    p_id = parse_id[i_id]
    mrs_value = mrs_values[p_id]
    current = 1
    
    cursor.execute("INSERT INTO mrs SET mrs_tag = %s, mrs_value = %s, mrs_current =%s, mrs_osp_id = %s",(tag,mrs_value,current,osp_id))

  # For known mrs tags, deprecate the most recent entry and then create a new entry 
  
  for tag in known_mrs_tags:

    cursor.execute("SELECT mrs_id FROM mrs WHERE mrs_tag = %s AND mrs_current = 1",(tag))
    mrs_to_deprecate = cursor.fetchone()[0]

    cursor.execute("UPDATE mrs SET mrs_current = 0 and mrs_date = %s WHERE mrs_id = %s",(timestamp,mrs_to_deprecate))

    i_input = find_string(tag)
    i_id = harv_id[i_input]
    p_id = parse_id[i_id]
    mrs_value = mrs_values[p_id]

    cursor.execute("INSERT INTO mrs SET mrs_tag = %s, mrs_value = %s, mrs_current =%s, mrs_osp_id = %s",(tag,mrs_value,current,osp_id))

##########################################################################
# Reading in tsdb files

def read_profile_file(file):
  contents = []
  f = open(file, 'r')
  for l in f.readlines():
    l = l.strip()
    contents.append(l.split('@'))
  f.close()
  return contents


##########################################################################
# Finally, we're ready to import the data from the itsdb profile into
# MatrixTDB.

# 6) Add rows from tsdb_profile/item,parse,result to sp_item,
# sp_parse, sp_result, replacing mrs in sp_result with mrs_tag

def import_to_sp(itsdb_dir,osp_id):

  spi_ids = {}
  spp_ids = {}
  spr_ids = {}

  items = read_profile_file(itsdb_dir + "item")

  for item in items:
    cursor.execute("INSERT INTO sp_item SET spi_input = %s, spi_wf = %s, spi_length = %s, spi_author = %s, spi_osp_id = %s",(item[6],item[7],item[8],item[10],osp_id))
    # We're not using the tsdb i_id because in the general case we'll be adding to a table.
    # So, we need to get the spi_id and link it to the i_id for use in the next load.
    cursor.execute("SELECT LAST_INSERT_ID()")
    spi_id = cursor.fetchone()[0]
    spi_ids[item[0]] = spi_id

  parses = read_profile_file(itsdb_dir + "parse")

  for parse in parses:
    cursor.execute("INSERT INTO sp_parse SET spp_run_id = %s, spp_i_id = %s, spp_readings = %s, spp_osp_id = %s",(parse[1],spi_ids[parse[2]],parse[3],osp_id))
    cursor.execute("SELECT LAST_INSERT_ID()")
    spp_parse_id = cursor.fetchone()[0]
    spp_parse_ids[parse[0]] = spp_parse_id

  results = read_profile_file(itsdb_dir + "result")

  for result in results:

    # We need to get the mrs_tag to insert instead of the mrs_value.  Since we have two ways to access the mrs_tag
    # (through the value and the harvester string), try both and see if they match, by way of error checking.
    mrs_value = result[13]
    cursor.execute = ("SELECT mrs_tag FROM mrs WHERE mrs_value = %s AND mrs_current = 1",(mrs_value))
    mrs_tag_1 = cursor.fetchone()[0]
    cursor.execute = ("SELECT spi_input FROM sp_item, sp_parse WHERE spi_id = spp_i_id AND spp_parse_id = %s",(spp_parse_ids[result[0]]))
    harv_string = cursor.fetchone()[0]
    mrs_tag_2 = mrs_dict[harv_string]

    if mrs_tag_1 != mrs_tag_2:
      raise ValueError, "import_to_sp found inconsistent mrs tags."

    cursor.execute("INSERT INTO sp_result SET spr_parse_id = %s, spr_mrs = %s, spr_osp_id = %s",(spp_parse_ids[result[0]],mrs_tag_1,osp_id))

##########################################################################
# Main program

# Check that inputs are all well formed

itsdb_dir = sys.argv[1]
harv_mrs = sys.argv[2]
choices_file = sys.argv[3]

if not (os.path.exists(itsdb_dir) and
        os.path.exists(itsdb_dir + "item") and
        os.path.exists(itsdb_dir + "parse") and
        os.path.exists(itsdb_dir + "result")):
    raise ValueError, "The first argument must be the path to a directory containing an [incr tsdb()] profile, ending in /."

mrs_dict = validate_string_list(harv_mrs,itsdb_dir)

wrong = validate_choices(choices_file)
choices = read_choices(choices_file)

if len(wrong) > 0:
    raise ValueError, "Invalid choices file."

# Check for presence of known mrs tags, and query user

[new_mrs_tags,known_mrs_tags] = check_for_known_mrs_tags(mrs_dict)

# 1) Look up or create LT entry in lt and lt_feat_grp
# This will also make sure that lt_feat_grp is up to date
# for an existing language type.  Defined in sql_lg_type.py

lt_id = create_or_update_lt(choices)

# 2) Ask user for comment to store about this source profile
# 3) Create a new row in orig_source_profile with information
# about this source profile.  

[osp_id,timestamp] = update_orig_source_profile(lt_id)

# 4) Update harvester string table with strings and mrs_tags
# Make hs_current_osp_id reflect current source profile for
# mrs_tags getting new mrs_values.

update_harv_str(new_mrs_tags,known_mrs_tags,mrs_dict,osp_id)

# 5) Update mrs table with mrs_tags and mrs_values

update_mrs(harv_mrs,osp_id,new_mrs_tags,known_mrs_tags,itsdb_dir,timestamp)

# 6) Add rows from tsdb_profile/item,parse,result to sp_item,
# sp_parse, sp_result, replacing mrs in sp_result with mrs_tag

import_to_sp(itsdb_dir,"item",osp_id)
import_to_sp(itsdb_dir,"parse",osp_id)
import_to_sp(itsdb_dir,"result",osp_id)


# 7) Print out osp_orig_src_prof_id for user to use as input
# to add_permutes.py

print "The original source profile id for your profile is: " + osp_id

# 8) Print out message about updating g.py to map include ne
# mrs_tags in the mrs_tag sets they belong in, and updating
# u_filters.py and s_filters.py to handle new strings, and updating
# stringmod.py as well.

if len(known_mrs_tags) > 0:
    print "The following mrs_tags now have updated mrs_values in MatrixTDB:\n" + known_mrs_tags

if len(new_mrs_tags) > 0:
    print "The following mrs_tags and their corresponding strings and values have been added to MatrixTDB:\n" + new_mrs_tags + "\n"
    print "Be sure to map them to the right sets of mrs_tags\n in g.py (for proper functioning of existing filters).\n"
    print "Be sure to update s_filters.py, u_filters.py and\n potentially stringmod.py to reflect your new strings."
    print "Then run add_permutes.py with the osp_id " + osp_id + "."
    print "Then run run_u_filters.py and run_specific_filters.py."


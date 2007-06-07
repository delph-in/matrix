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

sys.path.append("..")

from validate import validate_choices
from sql_lg_type import create_or_update_lt

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
        input =  "All of the mrs_tags you are reporting are new.\n If this is right, press 'y' to continue.  Press any other key to abort."
        if input != 'y':
            sys.exit
            
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
            sys.exit
        elif len(same_string_tags) > 0 
            print "The following mrs_tags already exist in MatrixTDB with\n the harvester string indicated.\n"
            input "If you mean to update the MRSs associated with them,\n press 'y' to continue (any other key will abort): "
            if input != 'y':
                sys.exit

            if len(new_mrs_tags) > 0:
                print "In addition, you are adding hte follwoing new mrs tags:\n"
                print new_mrs_tags
                print "\n"
                print "If this is correct, press 'y' to continue (any other key to abort): "
                if input != 'y':
                    sys.exit

    return [new_mrs_tags,known_mrs_tags]


###########################################################################
# Create new row in orig_source_profile

def update_orig_source_profile(lt_id)

###########################################################################
# Update harvester string table with strings and mrs_tags

def update_harv_str(harv_mrs,osp_id)

###########################################################################
# Update mrs table with mrs_tags and mrs_values

def update_mrs(harv_mrs,osp_id,new_mrs_tags,known_mrs_tags)

##########################################################################
# Main program

# Check that inputs are all well formed

itsdb_dir = sys.argv[1]
harv_mrs = sys.argv[2]
choices = sys.argv[3]

if not (os.path.exists(itsdb_dir) and
        os.path.exists(itsdb_dir + "item") and
        os.path.exists(itsdb_dir + "parse") and
        os.path.exists(itsdb_dir + "result"))
    raise ValueError "The first argument must be the path to a directory containing an [incr tsdb()] profile, ending in /."

mrs_dict = validate_string_list(harv_mrs,itsdb_dir)

wrong = validate_choices(choices)

if len(wrong) > 0:
    raise ValueError "Invalid choices file."

# Check for presence of known mrs tags, and query user

[new_mrs_tags,known_mrs_tags] = check_for_known_mrs_tags(mrs_dict)

# 1) Look up or create LT entry in lt and lt_feat_grp
# This will also make sure that lt_feat_grp is up to date
# for an existing language type.  Defined in sql_lg_type.py

lt_id = create_or_update_lt(choices)

# *** TO HERE ***

# 2) Ask user for comment to store about this source profile
# 3) Create a new row in orig_source_profile with information
# about this source profile.  

osp_id = update_orig_source_profile(lt_id)

# 4) Update harvester string table with strings and mrs_tags

update_harv_str(harv_mrs,osp_id)

# 5) Update mrs table with mrs_tags and mrs_values

update_mrs(harv_mrs,osp_id,new_mrs_tags,known_mrs_tags)

# 6) Add rows from tsdb_profile/item,parse,result to sp_item,
# sp_parse, sp_result, replacing mrs in sp_result with mrs_tag

import_to_sp_item(itsdb_dir + "item",osp_id)
import_to_sp_parse(itsdb_dir + "parse",osp_id)
import_to_sp_result(itsdb_dir + "result",osp_id)

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


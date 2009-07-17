"""
File: import_from_tsdb.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim) - taking over from ???
Date: 7/12/09 - taken over on this date
Project: MatrixTDB RA, summer 2009
Project Owner: Emily M. Bender
Contents:
  - TODO: ???update later???
"""
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

# TODO: Verify and update as necessary the steps below in this section.

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

import sys
import os
import datetime
from validate import validate_choices
from sql_lg_type import create_or_update_lt
from choices import ChoicesFile
from matrix_tdb_conn import MatrixTDBConn

sys.path.append("..")           # add parent directory to path

###########################################################################
# Validating file relating harvester strings to mrs.
# Check format (@ delimited)
# Check that each i_input in item corresponds to a row in harv_mrs file
# and vice versa
# Check that each harvester string is unique (within file) and that
# each mrs_tag is unique (within file).

def validate_string_list(harv_mrs, itsdbDir):
    """
    Function: validate_string_list
    Input:
        harv_mrs - a string containing the harvester strings and their mrs tags.
        itsdbDir - a directory of a [incr_tsdb()] profile
    Output: mrs_dict - a dict whose keys are the harvester strings and whose values are the mrs
                                tags
    Functionality:  validates the combo of file with harvester strings and the [incr_tsdb()] profile.
                         Ensures each item in the profile is a harvester string and vice-versa.  Ensures
                         each harvester string is unique and that each given mrs tag is unique.
    """
    mrs_dict = {}               # initialize output dict
    f = open(harv_mrs)      # open the harvester string file

    # convert lines in f to arrays of harvester string and then mars tag
    hlines = [line.strip().split('@') for line in f.readlines()]
    f.close()                       # close the harvester string file
    
    for line in hlines:         # for every harvester string line in harvester string file...
        try:
            # ...insert the mrs tag as the value of the string as the key
            mrs_dict[line[1]] = line[0]
        except IndexError:                  # if the array doesn't have enough items...
            # ...raise an error indicating problem.
            raise ValueError, "File " + harv_mrs + " is not a well-formed input file. " + \
                                      "Line format should be mrs_tag@string"

    items = []                                              # initialize list of items from source profile
    itemFile = open(itsdbDir + "item", 'r')        # open item file from source profile

    # split item file up into array of lines where each line is an array of its @-separated elements
    # TODO: don't i have to split it and then access an index to get the actual string?
    ilines = [line.strip().split('@') for line in itemFile.readlines()]

    for item in ilines:                             # for each item in the profile...
        if not mrs_dict.has_key(item):      # ..verify it is was given in the harv string file
            # ...and if not, raise an error.
            raise ValueError, "Item " + item + " in " + itsdbDir + "item is not represented in " + \
                                      harv_mrs + "."

    mrs_keys = mrs_dict.keys()          # get the set of given harvester strings

    for string in mrs_keys:                  # for each harvester string...
        if items.count(string) == 0:        # ...verify it is in the profile as an item...
            # ...if not, raise an error.
            raise ValueError, "Item " + string + " in " + harv_mrs + " is not represented in " + \
                                      itsdbDir + "item."
        if mrs_keys.count(string) > 1:   # ...also verify it doesn't appear multiple times as an item
            # ...if so, raise an error.
            raise ValueError, "Item " + string + " appears more than once in " + harv_mrs + "."

    mrs_tags = mrs_dict.values()        # get the set of given mrs tags.

    for tag in mrs_tags:                        # for each given mrs tag...
        if mrs_tags.count(tag) > 1:         # ...verify it only appears once in the harv file.
            # if it appears more than once ,raise an error.
            raise ValueError, "Mrs tag " + string + " appears more than once in " + harv_mrs + "."
  
    return mrs_dict                             # return output

###########################################################################
# Checking for known mrs_tags and deciding what to do

def check_for_known_mrs_tags(mrs_dict, conn):
    """
    Function: check_for_known_mrs_tags
    Input: 
        mrs_dict - a dict whose keys are the harvester strings and whose tags are the mrs tags for
                        those strings
        conn - a MatrixTDBConn
    Output:
        new_mrs_tags - TODO: update this docstring
        known_mrs_tags
    Functionality: 
    """

    new_mrs_tags = set()          # initialize output set of new mrs tags
    known_mrs_tags = set()       # initialize output set of known mrs tags

    # get every mrs tag out of the db
    rows = conn.selQuery("SELECT mrs_tag FROM mrs")

    db_tags = set([row[0] for row in rows])  # put mrs tags in database into a set

    # get a set of the mrs tags given in harvester_mrs input file
    input_tags = set(mrs_dict.values())
    input_strings = set(mrs_dict.keys())    # get a set of the harv strings given in same file

    for tag in input_tags:                           # for each mrs tag given...
        if tag in db_tags:                             # ...if that tag is already in the db
            known_mrs_tags.add(tag)           # ...add it to the set of known tags
        else:                                              # ...otherwise...
            new_mrs_tags.add(tag)               # ...add it to the set of new tags

    if len(known_mrs_tags) == 0:                # if every given tag is new...
        # ...ask user if that was the intent.
        res  =  raw_input("All of the mrs_tags you are reporting are new.\n" + \
                                  "If this is right, press 'y' to continue.  Press any other key to abort. ")
        if res != 'y':      # if they say anything but yes...
            sys.exit()    # ...exit
            
    else:           # we have some mrs tags given that we already knew about.  We want to verify
                      # they correspond to the same strings as before.
        same_string_tags = []           # initalize list of strings that are the same
        diff_string_tags = []               # initialize list of strings that are different.

        for tag in known_mrs_tags:          # for every mrs tag that we already knew about...
            s1 = find_string(tag,mrs_dict)     # get the corresponding harvester string as s1
            try:
                s2 = db.selQuery("SELECT hs_string FROM harv_str " + \
                                           "WHERE hs_mrs_tag = %s",(tag))[0][0]
            except IndexError:
                s2 = ''

            # TODO: test this function with the new indenting.
            if s1 == s2:
                same_string_tags.append(tag)
            else:
                diff_string_tags.append(tag)

            if len(diff_string_tags) > 0:
                print "The following mrs_tags are already in MatrixTDB with\n different harvester strings associated with them.\n"
                print diff_string_tags
                print "\n"
                print "Either you have inadvertently reused an mrs_tag or you\n should be modifying stringmod.py rather than\n adding harvester strings.\n"
                print "MatrixTDB has not been modified.\n"
                sys.exit()
            elif len(same_string_tags) > 0:
                print "The following mrs_tags already exist in MatrixTDB with\n the harvester string indicated.\n"
                print same_string_tags
                res = raw_input("If you mean to update the MRSs associated with them,\n press 'y' to continue (any other key will abort): ")
                if res != 'y':
                    sys.exit()

            if len(new_mrs_tags) > 0:
                print "In addition, you are adding hte following new mrs tags:\n"
                print new_mrs_tags
                print "\n"
                res = raw_input("If this is correct, press 'y' to continue (any other key to abort): ")
                if res != 'y':
                    sys.exit()

    return [new_mrs_tags,known_mrs_tags]


###########################################################################
# Create new row in orig_source_profile

def update_orig_source_profile(lt_id):
    try:
        user = os.environ['USER']
    except KeyError:
        # so it works on Windows XP, too
        user = os.environ['USERNAME']

    comment = raw_input("Enter a description of this source profile (1000 char max): ")

    if comment == "":
        # TODO: make this more graceful
        raise ValueError, "You must enter a comment."

    t = datetime.datetime.now()
    timestamp = t.strftime("%Y-%m-%d %H:%M")

    # this is how it should be, but osp_comment doesn't exist right now.
    db.execute("INSERT INTO orig_source_profile " + \
                      "SET osp_developer_name = %s, osp_prod_date = %s, " + \
                       "osp_orig_lt_id = %s, osp_comment = %s", (user,timestamp,lt_id,comment))

    osp_id = db.selQuery("SELECT LAST_INSERT_ID()")[0][0]

    return [osp_id,timestamp]

###########################################################################
# Update harvester string table with strings and mrs_tags

def update_harv_str(new_mrs_tags,known_mrs_tags,mrs_dict,osp_id):

    # Assume that harv_str has fields hs_id (auto increment), hs_string, hs_mrs_tag and
    # hs_init_osp_id and hs_cur_osp_id

    # print new_mrs_tags
    for tag in new_mrs_tags:
        harv = find_string(tag,mrs_dict)
        db.execute("INSERT INTO harv_str " + \
                         "SET hs_string = %s,  hs_mrs_tag = %s, hs_init_osp_id = %s, " + \
                          "hs_cur_osp_id = %s", (harv,tag,osp_id,osp_id))

    # For known mrs_tags that we're going over, update hs_current_osp_id to correspond to current source profile

    for tag in known_mrs_tags:
        db.execute("UPDATE harv_str SET hs_cur_osp_id = %s " + \
                         "WHERE hs_mrs_tag = %s",(osp_id,tag))

    return    

###########################################################################
# Find string corresponding to a given mrs_tag.  Need a little subroutine
# here because the strings are the keys in mrs_dict, not vice versa.  Also
# Note that we can assume that the tags are unique in mrs_dict.values(), i.e.,
# no two strings have the same tag as their value.

def find_string(tag, mrs_dict):
    """
    Function: find_string
    Input:
        tag - an mrs tag
        mrs_dict -a dict whose keys are harvester strings and whose keys are mrs tags for those
                      strings
    Output: answer - the harvester string corresponding to the mrs tag tag in mrs_dict
    Functionality: returns the harvester string corresponding to an mrs tag in mrs_dict.  raises a
                         ValueError if tag isn't in mrs_dict
    """
    for (key, value) in mrs_dict.items():       # for each key/value pair in mrs_dict
        if value == tag:                                # look for the tag matching input tag
            answer = key                              # if found, set answer to corresponding harv string
            break                                         # and exit loop
    else:                                                  # else is for for loop, not if stmt.
        # if tag was never found in mrs_dict.values(), raise error
        raise ValueError, "Error: import_from_itsdb.find_string() was passed an mrs tag with " + \
                                  "no corresponding harvester string."

    return answer                                       # return output

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

    i_input = find_string(tag, mrs_dict)
    i_id = harv_id[i_input]
    p_id = parse_id[i_id]
    mrs_value = mrs_values[p_id]
    current = 1
    
    db.execute("INSERT INTO mrs SET mrs_tag = %s, mrs_value = %s, " + \
                      "mrs_current =%s, mrs_osp_id = %s",(tag,mrs_value,current,osp_id))

  # For known mrs tags, deprecate the most recent entry and then create a new entry 
  
  for tag in known_mrs_tags:

    # i think these two queries should be updated to be just one.
    mrs_to_deprecate = db.selQuery("SELECT mrs_id FROM mrs " + \
                                                     "WHERE mrs_tag = %s " + \
                                                     "AND mrs_current = 1",(tag))[0][0]

    db.execute("UPDATE mrs SET mrs_current = 0, mrs_date = %s " + \
                     "WHERE mrs_id = %s",(timestamp,mrs_to_deprecate))

    i_input = find_string(tag,mrs_dict)
    i_id = harv_id[i_input]
    p_id = parse_id[i_id]
    mrs_value = mrs_values[p_id]

    db.execute("INSERT INTO mrs " + \
                     "SET mrs_tag = %s, mrs_value = %s, mrs_current = 1, " + \
                      "mrs_osp_id = %s",(tag,mrs_value,osp_id))

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

def import_to_sp(itsdb_dir,osp_id, mrs_dict):
    spi_ids = {}
    spp_ids = {}

    items = read_profile_file(itsdb_dir + "item")

    for item in items:
        db.execute("INSERT INTO sp_item " + \
                         "SET spi_input = %s, spi_wf = %s, spi_length = %s, spi_author = %s, " + \
                         "spi_osp_id = %s",(item[6],item[7],item[8],item[10],osp_id))
        # We're not using the tsdb i_id because in the general case we'll be adding to a table.
        # So, we need to get the spi_id and link it to the i_id for use in the next load.
        spi_id = db.selQuery("SELECT LAST_INSERT_ID()")[0][0]
        spi_ids[item[0]] = spi_id

    parses = read_profile_file(itsdb_dir + "parse")

    for parse in parses:
        db.execute("INSERT INTO sp_parse " + \
                         "SET spp_run_id = %s, spp_i_id = %s, spp_readings = %s, spp_osp_id = %s",
                                                                           (parse[1],spi_ids[parse[2]],parse[3],osp_id))
        spp_parse_id = db.selQuery("SELECT LAST_INSERT_ID()")[0][0]
        spp_ids[parse[0]] = spp_parse_id

    results = read_profile_file(itsdb_dir + "result")

    for result in results:

        # We need to get the mrs_tag to insert instead of the mrs_value.  Since we have two ways to
        # access the mrs_tag (through the value and the harvester string), try both and see if they
        # match, by way of error checking.

        mrs_value = result[13]
        mrs_tag_1 = db.selQuery("SELECT mrs_tag FROM mrs " + \
                                              "WHERE mrs_value = %s " + \
                                                    "AND mrs_current = 1",(mrs_value))[0][0]

        harv_string = db.selQuery("SELECT spi_input FROM sp_item, sp_parse " + \
                                              "WHERE spi_id = spp_i_id AND spp_parse_id = %s",
                                                                                                      (spp_ids[result[0]]))[0][0]
        mrs_tag_2 = mrs_dict[harv_string]

        #if mrs_tag_1 != mrs_tag_2:
        #  raise ValueError, "import_to_sp found inconsistent mrs tags."
    
        db.execute("INSERT INTO sp_result " + \
                         "SET spr_parse_id = %s, spr_mrs = %s, spr_osp_id = %s",
                                                                                     (spp_ids[result[0]],mrs_tag_2,osp_id))

    return

def validateProfile(itsdbDir):
    """
    Function: validateProfile
    Input: itsdbDir - a path to an [incr_tsdb()] profile
    Ouptut: answer - True if itsdbDir points to a valid profile, False otherwise
    Functionality: validates that itsdbDir exists and contains the three main files: item, result, and
                         parse
    """
    # check that the path exists as well as the three files we're importing
    if (os.path.exists(itsdb_dir) and os.path.exists(itsdb_dir + "item") and
            os.path.exists(itsdb_dir + "parse") and os.path.exists(itsdb_dir + "result")):
        answer = True           # if so, set output to True
    else:                             # if not, ...
        answer = False          # ...set output to False

    return answer               # return output

##########################################################################
# Main program

# Check that inputs are all well formed
def main(itsdb_dir, harv_mrs, choices_filename, conn = MatrixTDBConn()):
    """
    Function: main
    Input:
        itsdb_dir - a path to an [incr_tsdb()] profile
        harv_mrs - a path to and the name of a file linking the test items to mrs tags
        choices_filename - a path to and name of a choices file from the customization system
    Output: none
    Functionality: TOOD: write later, after re-factoring.
    """
    if not validateProfile(itsdb_dir):          # if the [incr_tsdb()] profile isn't valid...
        # ...raise an error
        raise ValueError, "The first argument must be the path to a directory containing a valid " + \
                                  "[incr tsdb()] profile, ending in /."

    # validate that harv/mrs file is formatted right and corresponds to [incr_tsdb()] profile
    # this will raise an error if not.
    # also assign harvstring/mrsTag as key/value pairs to mrs_dict
    mrs_dict = validate_string_list(harv_mrs, itsdb_dir)

    # validate the choices file.  put those choices that are into the dict wrong
    wrong = validate_choices(choices_filename)

    # if any choices in the file were wrong...
    if len(wrong) > 0:
        # ...inform user and exit.
        raise ValueError, "Invalid choices file.  Wrong choices: " + str(wrong)

    # convert the choices file into a ChoicesFile object.
    choices = ChoicesFile(choices_filename)

    # Check for presence of known mrs tags, and query user
    [new_mrs_tags,known_mrs_tags] = check_for_known_mrs_tags(mrs_dict, conn)
    
    # 1) Look up or create LT entry in lt and lt_feat_grp
    # This will also make sure that lt_feat_grp is up to date
    # for an existing language type.  Defined in sql_lg_type.py
    lt_id = create_or_update_lt(choices,db)

  # 2) Ask user for comment to store about this source profile
  # 3) Create a new row in orig_source_profile with information
  # about this source profile.  

  [osp_id,timestamp] = update_orig_source_profile(lt_id)

  # 4) Update harvester string table with strings and mrs_tags
  # Make hs_current_osp_id reflect current source profile for
  # mrs_tags getting new mrs_values.

  update_harv_str(new_mrs_tags,known_mrs_tags,mrs_dict,osp_id)

  # 5) Update mrs table with mrs_tags and mrs_values

  update_mrs(mrs_dict,osp_id,new_mrs_tags,known_mrs_tags,itsdb_dir,timestamp)

  # 6) Add rows from tsdb_profile/item,parse,result to sp_item,
  # sp_parse, sp_result, replacing mrs in sp_result with mrs_tag

  import_to_sp(itsdb_dir,osp_id, mrs_dict)

  # 7) Print out osp_orig_src_prof_id for user to use as input
  # to add_permutes.py

  print "The original source profile id for your profile is: " + str(osp_id)

  # 8) Print out message about updating g.py to map include ne
  # mrs_tags in the mrs_tag sets they belong in, and updating
  # u_filters.py and s_filters.py to handle new strings, and updating
  # stringmod.py as well.

  if len(known_mrs_tags) > 0:
      print "The following mrs_tags now have updated mrs_values in MatrixTDB:\n"
      print known_mrs_tags

  if len(new_mrs_tags) > 0:
      print "The following mrs_tags and their corresponding strings and values have been added to MatrixTDB:\n" + str(new_mrs_tags) + "\n"
      print "Be sure to map them to the right sets of mrs_tags\n in g.py (for proper functioning of existing filters).\n"
      print "Be sure to update s_filters.py, u_filters.py and\n potentially stringmod.py to reflect your new strings."
      print "Then run add_permutes.py with the osp_id " + str(osp_id) + "."
      print "Then run run_u_filters.py and run_specific_filters.py."

  return

# set to true for running on my machine.  set to False before commiting to repository.
moduleTest = True

if __name__ == '__main__':      # only run if run as main module...not if imported
  if len(sys.argv < 4):                # if the user gave too few arguments....
    # ...give an error message indicating usage.
    print >> sys.stderr, "Usage: python import_from_itsdb.py itsdb_directory " + \
                                 "harv_mrs_filename choices_filename"
  else:                                             # if the user gave at least three arguments...
    # ...get the directory of the [incr_tsdb()] directory from the command line...
    itsdbDir = sys.argv[1]

    # ...and the name of the file linking test sentences to mrs tags...
    harvMrsFilename = sys.argv[2]

    # ...and the name of the choices file for the customization system
    choicesFilename = sys.argv[3]
    
    #Connect to MySQL server
    conn = MatrixTDBConn()

    # call the main program to import a source profile into MatrixTDB
    main(itsdbDir, harvMrsFilename, choicesFilename, conn)
    
elif moduleTest:                        # if we are testing this module locally...
  conn = MatrixTDBConn('2')      # ...create a connection to the smaller, newer database...

  # ... and notify the user moduleTest is set to True.
  print >> sys.stderr, "Note: module testing turned on in import_from_itsdb.py.  " + \
                               "Unless testing locally, set moduleTest to False."
  mypath = "C:\RA\matrix\customize\sql_profiles\harv2\\" # set root path
  itsdbDir = mypath + "sourceprof\\attempt1\\"                 # set path to source [incr_tsdb()] profile

  # set path and filename of file linking test sentences to mrs tags
  harvMrsFilename = mypath + "harv_mrs_2"
  choicesFilename = mypath + "choices1"     # set path to and name of choices file

    # call the main program to import a source profile into MatrixTDB2
   # main(istdbDir, harvMrsFilename, choicesFilename, conn)

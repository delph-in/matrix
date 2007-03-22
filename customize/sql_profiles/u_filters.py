#!/usr/local/bin/python2.4

######################################################################
# This script interacts with the MySQL DB storing the string-mrs pairs
# to identify those pairs which are universally ungrammatical, and
# mark them as such. It actually groups together a bunch of different
# filters, each of which corresponds to a field in the DB.
#
# If each filter corresponds to a field, then there should be three
# possible values for each filter on each string-mrs pair:
#
#      2 -- filter is not applicable
#      1 -- filter is applicable and this string-mrs pair passes
#      0 -- filter is applicable and this string-mrs pair does not pass
#
# Filters are `applicable' to all strings for the mrs_ids they care
# about.  With the above system, assuming `1' and `2' both map to `true'
# and `0' to `false', universally ungrammatical strings are those for
# which the AND of the universal filters is false.  Potentially grammatical
# ones are those for which the AND is true.

######################################################################
# TODO
#
# 1. Rework things so that this can be invoke with particular mrs_ids,
# and only pull items with those mrs_ids out of the DB for consideration.
# This will be useful when we are adding to an existing DB. ... Or not:
# presumably, adding i_ids means adding new filters, which means that
# we need values for each of those filters for the rest of the strings.


######################################################################
# PSEUDOCODE
#
# for each filter
#   select (i_id,mrs_id,string) for all strings in DB
#                                   (or all new strings, later)
#   check mrs_id
#
#   if filter is not relevant
#      add i_id to 2-list
#
#   else if filter matches string (catch it as `bad')
#      add i_id to 0-list
#
#   else
#      add i_id to 1-list
#
# for each i_id on 0-list
#    insert 0 as value of filter field in record for i_id
#
# for each i_id on 1-list
#    insert 1 as value of filter field in record for i_id
#
# for each i_id on 2-list
#    insert 2 as value of filter field in record for i_id

######################################################################
# ALTERNATIVE PSEUDOCODE -- probably more efficient and compact
#
# Select all items (string-mrs pairs) from DB
# Countrows
#
# For each i-id in (0,countrows)
#    for each filter
#       append filter name to filter_list
#       append filter value to filter_values_list
#    check that filter_list and filter_values_list ended up same length
#    update DB entry for i-id with filter_list and filter_values_list

######################################################################
# Preliminaries

import MySQLdb
import re
import sys

######################################################################
# Main program

#Connect to MySQL server _FIX_ME_ update with actual db name

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="EmilyTest")

#Create a cursor

cursor = db.cursor()

#Get list of ids from DB.  _FIX_ME_ update with actual table and field names

cursor.execute("SELECT st_id FROM StrTst")
ids = cursor.fetchall

#`ids' is now a tuple containing elements for each item in the
#relevant table.  Each of those elements is a tuple which contains
#just the st_id.

for id in ids:
    key = id[0]
    cursor.execute("SELECT mrs_id, string FROM StrTst where st_id = %s", (key))
    (mrs_id, string) = cursor.fetchone()

    filter_values = filter_string(mrs_id,string)

    #Should do error checking here: Are all of the values legit?

    for filter_key in filter_values.keys()
        cursor.execute("UPDATE StrTst SET %s = %s WHERE st_id = %s",
                       (filter_key, filter_values[filter_key], key))


#######################################################################
# filter_string(mrs_id,string)
#
# Calls each individual filter on string-mrs pair.  Returns an array
# where the keys are the names of the filter fields and the values are
# the values assigned by each filter to the string-mrs pair.  Each
# particular filter should return its name and its value.

def filter_string(mrs_id, string):

    filter_values = {}
    (key, value) = filter1(mrs_id, string)
    filter_values[key] = value

    # Add in calls to the rest of the filters.

########################################################################
# Filter class


########################################################################
# Filters

# If n1 is the subject, and there is no determiner for it, any p-nom
# in the sentence needs to be adjacent to n1.

def filter1(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo1','wo3','wo6','neg1','ques1']): # _FIX_ME_ define onlist or find equiv
        if re.search('p-nom',sent) and not re.search ('p-nom n1|n1 p-nom', sent):
            value = 0
        else:
            value = 1

    return ("filter1",value)
        
# If n2 is the subject, and there is no determiner for it, any p-nom
# in the sentence needs to be adjacent to n2.

def filter2(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo7','wo10','neg3','ques3']): # _FIX_ME_ define onlist or find equiv
        if re.search('p-nom',sent) and not re.search ('p-nom n2|n2 p-nom', sent):
            value = 0
        else:
            value = 1

    return ("filter2",value)

# If n1 is the object, and there is no determiner for it, any p-acc
# in the sentence needs to be adjacent to n1.

def filter3(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo7','wo10','neg3','ques3']): # _FIX_ME_ define onlist or find equiv
        if re.search('p-acc',sent) and not re.search ('p-acc n1|n1 p-acc', sent):
            value = 0
        else:
            value = 1

    return ("filter3",value)
        
# If n2 is the object, and there is no determiner for it, any p-acc
# in the sentence needs to be adjacent to n2.

def filter4(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo3','wo5','neg1','ques1']): # _FIX_ME_ define onlist or find equiv
        if re.search('p-nom',sent) and not re.search ('p-acc n2|n2 p-acc', sent):
            value = 0
        else:
            value = 1

    return ("filter4",value)

# If there's only one det, and it's attached to n1, it must be
# adjacent to n1

def filter5(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo2','wo5','wo10']):
        if not re.search('det n1|n1 det',sent):
            value = 0
        else:
            value = 1

    return ("filter5",value)

# If there's only one det, and it's attached to n2, it must be
# adjacent to n2

def filter6(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo6','wo9']):
        if not re.search('det n2|n2 det',sent):
            value = 0
        else:
            value = 1

    return ("filter6",value)

# If there are two dets, each one has to be next to a noun,
# and no fair putting them both next to the same noun.

def filter7(mrs_id, sent):

    value = 2
    if onlist(mrs_id,['wo4','wo8','neg2','neg4','ques2','ques4']):
        if not (re.search(r'det (n1|n2).*det (n1|n2)',sent) or
                re.search(r'det (n1|n2).*(n1|n2) det',sent) or
                re.search(r'(n1|n2) det.*det (n1|n2)',sent) or
                re.search(r'(n1|n2) det.*(n1|n2) det',sent)):
            value = 2
        else:
            value = 1

    return ("filter7",value)

# If n1 is the subject and it does have a determiner attached to
# it, if there's a p-nom, it has to form a coherent NP with n1 and det.

# If n2 is the subject and it does have a determiner attached to
# it, if there's a p-nom, it has to form a coherent NP with n2 and det.

# If n1 is the object and it does have a determiner attached to
# it, if there's a p-acc, it has to form a coherent NP with n1 and det.

# If n2 is the object and it does have a determiner attached to
# it, if there's a p-acc, it has to form a coherent NP with n1 and det.


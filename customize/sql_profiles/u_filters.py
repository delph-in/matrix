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
# ALTERNATIVE PSEUDOCODE -- probably more efficient and compact
#
# select i-id from MatrixTDB
#
# For each i-id in ids
#    for each filter
#        add [filter_name, filter_value] to dictionary
#    update DB entry for i-id with appropriate filter_value for each
#                     filter_name

######################################################################
# Preliminaries

import MySQLdb
import re
import sys

#######################################################################
# filter_string(mrs_id,string)
#
# Calls each individual filter on string-mrs pair.  Returns an array
# where the keys are the names of the filter fields and the values are
# the values assigned by each filter to the string-mrs pair.  Each
# particular filter should return its name and its value.

def filter_string(mrs_id, sent):

    filter_values = {}

    for f in filter_list:
        key = f.name
        filter_values[key] = f.exe(mrs_id, sent)

    return filter_values

########################################################################
# Filter class

class Filter:

    def __init__(self,name,mrs_id_list,comment):
        self.name = name
        self.mrs_id_list = mrs_id_list

    def check_mrs_id(self,mrs_id):
        return mrs_id in self.mrs_id_list

    def exe(self,mrs_id,sent):
        if not self.check_mrs_id(mrs_id):
            return 2
        else:
            return self.apply_filter(sent)

    #Might not need this here.
    def apply_filter(self,sent):
        print "Error"
        assert False

class AndNotFilter(Filter):        

    def __init__(self,name,mrs_id_list,re1,re2,comment):
        Filter.__init__(self,name,mrs_id_list,comment)
        self.re1 = re1
        self.re2 = re2

    def apply_filter(self,sent):
        if re.search(self.re1,sent) and not re.search(self.re2,sent):
            return 0
        else:
            return 1

class NotFilter(Filter):

    def __init__(self,name,mrs_id_list,re1,comment):
        Filter.__init__(self,name,mrs_id_list,comment)
        self.re1 = re1

    def apply_filter(self,sent):
        if not re.search(self.re1,sent):
            return 0
        else:
            return 1


########################################################################
# Filters

filter_list = [

    AndNotFilter(name = "uf1",
                 mrs_id_list = ['wo1','wo3','wo6','neg1','ques1'],
                 re1 = 'p-nom',
                 re2 = 'p-nom n1|n1 p-nom',
                 comment = "If n1 is the subject, and there is no determiner for it, any p-nom in the sentence needs to be adjacent to n1."),

    AndNotFilter(name = "uf2",
                 mrs_id_list = ['wo7','wo10','neg3','ques3'],
                 re1 = 'p-nom',
                 re2 = 'p-nom n2|n2 p-nom',
                 comment = "If n2 is the subject, and there is no determiner for it, any p-nom in the sentence needs to be adjacent to n2."),

    AndNotFilter(name = "uf3",
                 mrs_id_list = ['wo7','wo10','neg3','ques3'],
                 re1 = 'p-acc',
                 re2 = 'p-acc n1|n1 p-acc',
                 comment = "If n1 is the object, and there is no determiner for it, any p-acc in the sentence needs to be adjacent to n1."),

    AndNotFilter(name = "uf4",
                 mrs_id_list = ['wo3','wo5','neg1','ques1'],
                 re1 = 'p-acc',
                 re2 = 'p-acc n2|n2 p-acc',
                 comment = "If n2 is the object, and there is no determiner for it, any p-acc in the sentence needs to be adjacent to n2."),

    NotFilter(name = "uf5",
              mrs_id_list = ['wo2','wo5','wo10'],
              re1 = 'det n1|n1 det',
              comment = "If there's only one det, and it's attached to n1, it must be adjacent to n1."),

    NotFilter(name = "uf6",
              mrs_id_list = ['wo6','wo9'],
              re1 = 'det n2|n2 det',
              comment = "If there's only one det, and it's attached to n1, it must be adjacent to n1.")]



# If there are two dets, each one has to be next to a noun,
# and no fair putting them both next to the same noun.

#def filter7(mrs_id, sent):

#    value = 2
#    if onlist(mrs_id,['wo4','wo8','neg2','neg4','ques2','ques4']):
#        if not (re.search(r'det (n1|n2).*det (n1|n2)',sent) or
#                re.search(r'det (n1|n2).*(n1|n2) det',sent) or
#                re.search(r'(n1|n2) det.*det (n1|n2)',sent) or
#                re.search(r'(n1|n2) det.*(n1|n2) det',sent)):
#            value = 0
#        else:
#            value = 1
#
#    return ("uf7",value)

# If n1 is the subject and it does have a determiner attached to
# it, if there's a p-nom, it has to form a coherent NP with n1 and det.

# If n2 is the subject and it does have a determiner attached to
# it, if there's a p-nom, it has to form a coherent NP with n2 and det.

# If n1 is the object and it does have a determiner attached to
# it, if there's a p-acc, it has to form a coherent NP with n1 and det.

# If n2 is the object and it does have a determiner attached to
# it, if there's a p-acc, it has to form a coherent NP with n1 and det.

######################################################################
# Main program

#Connect to MySQL server _FIX_ME_ update with actual db name

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

#Get list of ids from DB.  _FIX_ME_ update with actual table and field names

cursor.execute("SELECT i_id FROM item")
#ids = cursor.fetchall()
ids = cursor.fetchmany(5)

print ids

#`ids' is now a tuple containing elements for each item in the
#relevant table.  Each of those elements is a tuple which contains
#just the st_id.

for id in ids:
    key = id[0]
    cursor.execute("SELECT i_comment, i_input FROM item where i_id = %s", (key))
    (mrs_id, string) = cursor.fetchone()

    filter_values = filter_string(mrs_id,string)

    #Should do error checking here: Are all of the values legit?

    #    for filter_key in filter_values.keys()
    #        cursor.execute("UPDATE StrTst SET %s = %s WHERE st_id = %s",
    #                       (filter_key, filter_values[filter_key], key))

    print filter_values
    


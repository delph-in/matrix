#!/usr/local/bin/python2.5

import filters
from u_filters import filter_list
import sys
import MySQLdb
import datetime

##########################################################################
# find_filter_from_name(n,filter_list)

def find_filter_from_name(n,filter_list):

    for f in filter_list:
        if f.name == n:
            return f

    raise ValueError, "Unknown filter: " + n

##########################################################################
# make string from list

def make_string(int_list,limit):

    # For when we are only updating a fe filters:

#    output = "SELECT r_result_id, r_mrs FROM result FORCE INDEX (PRIMARY), fltr_mrs WHERE r_mrs = fm_mrs_tag AND (fm_fltr_id = "
#    l = len(int_list)
#    e = l - 1
#    for i in int_list[0:e]:
#        output += str(i)
#        output += ' or fm_fltr_id = '
#    output += str(int_list[l-1])
#    output += ") LIMIT "
#    output += str(limit)
#    output += ", 100000"

    output = "SELECT r_result_id, i_input, r_mrs FROM result FORCE INDEX (PRIMARY), item WHERE r_parse_id = i_id LIMIT " + str(limit) + ", 100000"

    return output

##########################################################################
# update_res_fltr(filter_list)

def update_res_fltr(filter_list):

    filter_ids = []
    filter_id_hash = {}
    
    for f in filter_list:
            
        name = f.name
        mrs_tag_list = f.mrs_id_list
    
        cursor.execute("SELECT filter_id FROM filter WHERE filter_name = %s", (name))
        filter_id = cursor.fetchone()

        if filter_id == ():
            cursor.execute("INSERT INTO filter SET filter_name = %s, filter_type = %s", (name,"u"))
            cursor.execute("SELECT LAST_INSERT_ID()")
            filter_id = cursor.fetchone()[0]
        else:
            filter_id = filter_id[0]

        for mrs_tag in mrs_tag_list:
            cursor.execute("SELECT * FROM fltr_mrs WHERE fm_fltr_id = %s AND fm_mrs_tag = %s",(filter_id,mrs_tag))
            row = cursor.fetchall()
            if row == ():
                cursor.execute("INSERT INTO fltr_mrs SET fm_fltr_id = %s, fm_mrs_tag = %s, fm_value = 1",(filter_id,mrs_tag))

        filter_ids.append(filter_id)
        filter_id_hash[name] = filter_id


    limit = 8700000

    print filter_ids
    f_string = make_string(filter_ids,limit)
    print f_string

    limit += 100000

    #cursor.execute("SELECT r_result_id, r_mrs FROM result, fltr_mrs WHERE r_mrs = fm_mrs_tag AND (fm_fltr_id = %s) LIMIT 10",(f_string))
    cursor.execute(f_string)

    ids = cursor.fetchall()
    
    dupes = []

    while ids != ():

        update_string = "Now working results " + str(limit-100000) + " through " + str(limit)
        print update_string
        file = open('ufltrs_updates','a')
        file.write(update_string)
        file.write("\n")
        file.close()

        #`ids' is now a tuple containing elements for each item in a 100,000 slice of the
        #relevant table.  Each of those elements is a tuple which contains
        #just the r_result_id.

        #Based on the r_result_id, we are now going to get the string
        #and mrs_id.

        for id in ids:

            (key, string, mrs_id) = id
            
            filter_values = filters.filter_one_result(mrs_id,string,filter_list,filter_id_hash)

            #Should do error checking here: Are all of the values legit?

            for f_id in filter_values.keys():
                value = filter_values[f_id]

                if not value == 2:
                    num = cursor.execute("UPDATE res_fltr SET rf_value = %s WHERE rf_res_id = %s AND  rf_fltr_id = %s",
                                         (value, key, f_id))
                    if num == 0:
                        cursor.execute("SELECT * FROM res_fltr WHERE rf_res_id = %s AND rf_fltr_id = %s", (key, f_id))
                        row = cursor.fetchall()
                        if row == ():
                            cursor.execute("INSERT INTO res_fltr SET rf_value = %s, rf_res_id =%s, rf_fltr_id = %s", (value, key, f_id))
                        if len(row) > 1:
                            dupes.append(row)
                    if num > 1:
                        cursor.execute("SELECT * FROM res_fltr WHERE rf_res_id = %s AND rf_fltr_id = %s", (key, f_id))
                        dupes.append(cursor.fetchall())

        else:
            update_string = "Last item updated: " + str(key) + " at " + str(datetime.datetime.now())
            print update_string
            file = open('ufltrs_updates','a')
            file.write(update_string)
            file.write("\n")
            file.close()

        f_string = make_string(filter_ids,limit)
        cursor.execute(f_string)
        ids = cursor.fetchall()
        limit += 100000

    return dupes

#########################################################################
#     add_to_res_fltr(osp_id)

def add_to_res_fltr(osp_id):

    print "Ooops, this hasn't been coded yet."
        


##########################################################################
# Main program

# Set up cursor

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

cursor = db.cursor()


# Determine whether we are running u_filters because there are new
# items in the DB or because we need to update the values in res_fltr
# for particular u_filters.

ans = ''

while (ans != 'r' and ans != 'a'):
    ans = raw_input("Are you [r]eplacing the results for certain filters on existing items\n or [a]dding rows to res_fltr for new items? [r/a]: ")

# First the case where we're replacing values

if ans == 'r':

    name = raw_input("\nType the name of the first filter you would like to update: ")

    names = []

    while (name != 'end' and name != ''):
        names.append(name)
        name = raw_input("Type another filter name, or 'end' (or <ret>) if you have no more: ")

    current_filter_list = []

    for n in names:
        current_filter_list.append(find_filter_from_name(n,filter_list))

    conf = raw_input("\nI will now delete all rows in res_fltr for the filters\n" + str(current_filter_list) + "\n and repopulate with new values.  Confirm: [y/n] ")

    if conf != 'y':
        print "\nAborting.  Now rows modified."
        sys.exit()

    # Now that we know which filters and have the go ahead, do the DB work:

    dupes = update_res_fltr(current_filter_list)

    # Save those dupes to a file.

    f = open('dupes', 'w')
    for d in dupes:
        for i in d:
            for c in i:
                f.write(str(c))
                f.write(",")
            f.write("\n")

    f.close()

# Next the case where we're adding to res_fltr for new strings

if ans == 'a':

    osp_id = raw_input("\nWhat is the osp_id for the results you would lke to filter: ")

    cursor.execute("SELECT count(r_result_id) FROM result where r_osp_id = %s",(osp_id))
    count = cursor.fetchone()[0]

    if count == 0:
        print "That is not a valid osp_id."
        sys.exit()
    else:
        print "There are " + str(count) + " results to filter."


    add_to_res_fltr(osp_id)



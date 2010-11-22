
# For debugging a particular filter.

import filters
import g
import MySQLdb
import sys

result_id = sys.argv[1]

f =  filters.NotFilter(name = "uf13",
                       mrs_id_list = g.ques,
                       re1 = '^.*[a-z]+.*qpart.*[a-z]+.*$',
                       comment = "If the question particle appears in the middle of the string, then this is clearly no good. --- Applies to examples with matrix questions.  Different filter will be needed once we do embedded questions.")


#Connect to MySQL server
db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

cursor.execute("SELECT i_input, r_mrs FROM item, result WHERE r_result_id = %s and r_parse_id = i_id",(result_id))
res = cursor.fetchone()

print f.exe(res[1],res[0])

    


    

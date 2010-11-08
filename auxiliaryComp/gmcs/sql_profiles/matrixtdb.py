#For setting up cursor when doing python interactively.
#Import this file.

import MySQLdb

#Connect to MySQL server

db = MySQLdb.connect(host="localhost", user="ebender",
                     passwd="tr33house", db="MatrixTDB")

#Create a cursor

cursor = db.cursor()

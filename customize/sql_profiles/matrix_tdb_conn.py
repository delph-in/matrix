"""
File: matrix_tdb_conn.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim)
Date: 6/24/09
Project: MatrixTDB RA, summer 2009
Owner: Emily M. Bender
Contents:
    - MatrixTDBConn class which further abstracts connection details to MatrixTDB using
      MySQLdb
Tables accessed: depends on query given to execute or selQuery
Tables modified: depends on query given to execute
"""

import MySQLdb.connections, getpass, sys

class MatrixTDBConn(MySQLdb.connections.Connection):
    """
    Class: MatrixTDBConn
    Superclass: MySQLdb.connections.Connection
    Members:
        uname - the username for this connection
        pword - the password for self.uname
        connID - the id of the connection to the MySQL database
        tdbCursor - the cursor of the connection
        dbSuffix - the suffix to add to the name of the database.  For connecting to my fresh
                       database, MatrixTDB2, for example.  Defaults to the empty string.
    Functionality: connection to MatrixTDB MySQL database. Abstracts some details of
                        MySQLdb pacakge.
    """        
    def __init__(self, dbSuffix='', username='', password=''):
        """
        Method: __init__
        Input:
            self - this MatrixTDBConn instance
        Output: a new MatrixTDBConn
        Functionality: constructor.  Note that it asks for username and password from user.
                             Creates a connection to MatrixTDB, gets its connection id, and creates a
                             cursor for executing SQL statements.
        Tables accessed: none
        Tables modified: none
        """

        if username == '':                                          # if user didn't give a username
            self.uname=raw_input("Username:")          # prompt user for username and store
        else:                                                           # otherwise
            self.uname = username                           # assign given username to uname

        if password == '':                                          # if user didn't give a password
            self.pword=getpass.getpass("Password:")  # prompt user for password and store
        else:                                                           # otherwise
            self.pword = password                              # assign given password to pword
            
        self.dbSuffix = dbSuffix                                 # store suffix
        self.connect()                                              # connect to database

        return

    def connect(self):
        """
        Method: connect
        Input: self - this MatrixTDBConn
        Output: none
        Functionality: connects to MatrixTDB database using the information stored in its members
        Tables accessed: none
        Tables modified: none
        """
        # connect to the database
        MySQLdb.connections.Connection.__init__(self, host="capuchin.ling.washington.edu", \
                                                                        user=self.uname, passwd=self.pword, \
                                                                        db="MatrixTDB"+self.dbSuffix)

        self.connID = self.thread_id()      # get the connection ID
        self.tdbCursor = self.cursor()           # get the connection's cursor
        return

    def execute(self, query,args=None):
        """
        Method: execute
        Input:
            self - this MatrixTDBConn
            query - the query to be executed
            args - arguments to pass along to the cursor
        Output: number of rows affected, if any
        Functionality: executes a query through this connection
        Tables accessed: depends on query
        Tables modified: depends on query        
        """
        try:
            answer = self.tdbCursor.execute(query, args)        # for now, just pass call on to cursor
        except MySQLdb.OperationalError:                    # but if I lost my connection
            print >> sys.stderr, 'lost connection...reconnecting'   # inform user
            self.connect()                                                          # reconnect
            answer = self.tdbCursor.execute(query, args)        # and try again
        return answer                                                   # return its output

    def selQuery(self, query, args=None):
        """
        Method: selQuery
        Input:
            self - this MatrixTDBConn
            query - the select query to be executed
            args - arguments to pass along to cursor to execute
        Output: rows - a tuple of rows selected where each row is a tuple made up of the columns
                             in each row selected
        Functionality: Runs a select query and returns the rows returned
        Tables accessed: those mentioned in query
        Tables modified: none
        TODO: test boundary case of no rows selected.  Is it None or an empty tuple
        """
        self.execute(query, args)       # execute the query
        return self.tdbCursor.fetchall()      # return all rows

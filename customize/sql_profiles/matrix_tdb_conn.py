"""
File: matrix_tdb_conn.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim)
Date: 6/24/09
Project: MatrixTDB RA, summer 2009
Owner: Emily M. Bender
Contents:
    - MatrixTDBConn class which further abstracts connection details to MatrixTDB using
      MySQLdb
"""

import MySQLdb.connections

class MatrixTDBConn(MySQLdb.connections.Connection):
    """
    Class: MatrixTDBConn
    Superclass: MySQLdb.connections.Connection
    Members:
        connID - the id of the connection to the MySQL database
        cursor - the cursor of the connection
    Functionality: connection to MatrixTDB MySQL database. Abstracts some details of
                        MySQLdb pacakge.
    """        
    def __init__(self):
        """
        Method: __init__
        Input:
            self - this MatrixTDBConn instance
        Output:
            a new MatrixTDBConn
        Functionality: constructor.  Note that it asks for username and password from user.
                             Creates a connection to MatrixTDB, gets its connection id, and creates a
                             cursor for executing SQL statements.
        """
        self.uname=raw_input("Username:")   # prompt user for username password
        # TODO: fix so it doesn't show what the user types
        self.pword=raw_input("Password:")     # prompt user for database password

        # connect to the database
        MySQLdb.connections.Connection.__init__(self, host="capuchin.ling.washington.edu", user=self.uname, \
                                               passwd=self.pword, db="MatrixTDB")

        self.connID = self.thread_id()      # get the connection ID
        self.cursor = self.cursor()           # get the connection's cursor
        return

    def execute(self, query):
        """
        Method: execute
        Input:
            self - this MatrixTDBConn
            query - the query to be executed
        Output: number of rows affected, if any
        Functionality: executes a query through this connection
        """        
        return self.cursor.execute(query)

    def selQuery(self, query):
        """
        Method: selQuery
        Input:
            self - this MatrixTDBConn
            query - the select query to be executed
        Output: rows - a tuple of rows selected where each row is a tuple made up of the columns
                             in each row selected
        Functionality: Runs a select query and returns the rows returned
        TODO: test boundary case of no rows selected.  Is it None or an empty tuple
        """
        self.execute(query)
        return self.cursor.fetchall()

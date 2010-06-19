"""
File: db_utils.py
Author: KEN (captnpi@u.washington.edu, Scott Halgrim)
Date: 6/25/09
Project: MatrixTDB RA, summer 2009
Owner: Emily M. Bender
Contents: A set of database utilities
    - selColumnToSet - converts the items in the first column of the results of a select query to a
                                 set of distinct item
Tables accessed: none
Tables modified: none
"""

def selColumnToSet(resultTuple):
    """
    Function: selColumnToSet
    Input: resultTuple - the results of a select query as returned by a MatrixTDBConn
    Output: answer - the distinct items in the first column of the input results
    Functionality: Converts the items in the first column of the results of a select query to a set
                         of distinct items
    Tables accessed: none
    Tables modified: none
    """
    # for each row, get the item in the first column and put those in a list
    colList = [row[0] for row in resultTuple]
    
    answer = set(colList)           # convert that list to a set
    return answer                      # return output

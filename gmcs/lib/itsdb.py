#
# Module for [incr tsdb()] profiles
#
# Author: Michael Wayne Goodman <goodmami@uw.edu>
#
# This module supports the loading of [incr tsdb()] profiles.

import os
import re
from collections import defaultdict

##############################################################################
### Global variables

_relations_filename = 'relations'
_field_delimiter = '@'

##############################################################################
### Non-class (i.e. static) functions

def get_relations(path):
    """
    Parse the relations file and return a dictionary describing the database
    structure.

    @param profile_directory: The directory where the relations file exists.
    @param relations_filename: The filename containing the database relations.
                               Defaults to 'relations'.
    """

    relations = defaultdict(list)
    relations_table_re = re.compile(r'^(\w.+):$')
    f = open(path)
    current_table = None
    for line in f:
        table_match = relations_table_re.search(line)
        if table_match is not None:
            current_table = table_match.groups()[0]
        elif len(line.strip()) > 0 and current_table is not None:
            fields = line.split()
            relations[current_table].append(fields[0])
            #TODO: Add data type, key, partial key, comment? etc.
            #TODO: Import db to proper db (sqlite, etc)
    return relations


##############################################################################
### Profile classes

class TsdbProfile:

    def __init__(self, root_directory):
        """
        Given the directory of a [incr TSDB()] profile, analyze the
        database structure and prepare for reading.

        @param root_directory: The directory where the profile files are
            stored.
        """

        self.root = root_directory

    def write_profile(self, profile_directory, relations_filename, tables):
        """
        Using self.relations as a schema, write the profile data out to
        the specified profile directory.

        @param profile_directory: The directory where the profile will
            be written.
        @param relations_file: The relations file for the profile.
        @param tables: A dictionary of database rows.
        """
        import shutil
        relations = get_relations(relations_filename)
        shutil.copyfile(relations_filename, os.path.join(profile_directory,
                                                         _relations_filename))
        for tbl_name in tables.keys():
            tbl = open(os.path.join(profile_directory, tbl_name), 'w')
            for row in tables[tbl_name]:
                print >>tbl, _field_delimiter.join(
                    row.get(f, '') for f in relations[tbl_name])
            tbl.close()

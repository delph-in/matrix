#!/usr/local/bin/python2.5

# !!!!! This is being replaced by add_permutes.py !!!!!!

######################################################################
# ERB 3/21/07 Reworking the profiles code to work with MySQL DB.
# This script will create the string-mrs pairs (in appropriate tsdb++
# format) for further filtering.
#
# Other scripts will apply universal and language particular filters,
# reading the records out of the MySQL DB and updating them as required.
#
# I'm assuming that with the DB, I can get away with storing all of
# the string-mrs pairs, including all the ones that are always ungrammatical.
#
# python permute.py <string_list> <inprofile> <outprofile> 

######################################################################
# imports

import shutil
import sys
import os

sys.path.append("..")

from copy import copy
import re

#from utils import read_choices
#from random import randint
#from randgram import random_validated_grammar
from getopt import getopt

import math

######################################################################
# factorial(n)

# We call factorial a lot, so I'm precomputing it for a bunch of
# relevant numbers.

def factorial(n):
    if not n >= 0:
        raise ValueError("n must be >= 0")
    if math.floor(n) != n:
        raise ValueError("n must be exact integer")
    if n+1 == n:
        raise OverflowError("n too large")
    if n == 4:
        return 24
    if n == 5:
        return 120
    if n == 6:
        return 720
    if n == 7:
        return 5040
    if n == 8:
        return 40320
    if n == 9:
        return 362880
    if n == 10:
        return 3628800
    if n == 11:
        return 39916800

    result = 1
    factor = 2
    while factor <= n:
        result *= factor
        factor += 1
    return result

######################################################################
# Copy the other files, which we need even though some are empty

def copy_other_files(in_profile,out_profile):
    shutil.copy(in_profile + 'analysis', out_profile + 'analysis')
    shutil.copy(in_profile + 'daughter', out_profile + 'daughter')
    shutil.copy(in_profile + 'decision', out_profile + 'decision')
    shutil.copy(in_profile + 'edge', out_profile + 'edge')
    shutil.copy(in_profile + 'fold', out_profile + 'fold')
    shutil.copy(in_profile + 'item-phenomenon', out_profile + 'item-phenomenon')
    shutil.copy(in_profile + 'item-set', out_profile + 'item-set')
    shutil.copy(in_profile + 'output', out_profile + 'output')
    shutil.copy(in_profile + 'parameter', out_profile + 'parameter')
    shutil.copy(in_profile + 'phenomenon', out_profile + 'phenomenon')
    shutil.copy(in_profile + 'preference', out_profile + 'preference')
    shutil.copy(in_profile + 'relations', out_profile + 'relations')
    shutil.copy(in_profile + 'rule', out_profile + 'rule')
    shutil.copy(in_profile + 'run', out_profile + 'run')
    shutil.copy(in_profile + 'score', out_profile + 'score')
    shutil.copy(in_profile + 'set', out_profile + 'set')
    shutil.copy(in_profile + 'tree', out_profile + 'tree')
    shutil.copy(in_profile + 'update', out_profile + 'update')


######################################################################
# Functions for reading and writing profile files.  Each file in
# memory is stored as a dict of entries indexed by the identifier of
# the entry.  Each entry is stored as a dict containing the
# important fields in the entry -- see functions below for details.

def read_profile_file(file):
    contents = []
    f = open(file, 'r')
    for l in f.readlines():
        l = l.strip()
        contents.append(l.split('@'))
    f.close()
    return contents


def write_profile_file(contents, file):
    f = open(file, 'w')
    for c in contents:
        for w in c[0:1]:
            f.write(w)
        for w in c[1:]:
            f.write('@' + w)
        f.write('\n')
    f.close()


######################################################################
# A function that takes a list of words and returns a list of lists
# containing all permutations of those words

def permute_helper(words, i, fun):
    n = len(words)
    if (i == n):
        fun(words)
    else:
        old = words[i]
        for j in range(i,n):
            words[i] = words[j]
            words[j] = old
            permute_helper(words, i+1, fun)
            words[j] = words[i]
        words[i] = old

class Permlist :
    def __init__(self,mrs_id) :
        self.perms = []
        self.keeps = []
        self.mrs_id = mrs_id
        self.filtered = 0
        self.u_kept = 0

    def add(self, words) :
        perm = words[0]
        for w in words[1:]:
            perm += ' ' + w
        perm = re.sub('neg- ','neg-',perm)
        perm = re.sub(' -neg','-neg',perm)
        perm = re.sub('co- ','co-',perm)
        perm = re.sub(' -co','-co',perm)
        global est

        if est < 4:
            keep_prob = 2
        else:
            keep_prob = est // 4

        if not (re.search('-$',perm) or re.search('^-',perm)):
            self.perms.append(perm)


    def get(self):
        return self.perms

    def length(self):
        return len(self.perms)

######################################################################
# This calls the permute_helper above

def permute(s,mrs_id):
    permlist = Permlist(mrs_id)
    #Break off neg- and co- affixes.  Note that we're assuming the seed
    #strings will provide both prefix and suffix examples to work from.
    #Could consider noticing one and generating the other here, but we're
    #not.
    s = re.sub('neg-','neg- ',s)
    s = re.sub('-neg',' -neg',s)
    s = re.sub('co1-', 'co1- ',s)
    s = re.sub('-co', ' -co',s)
    s = re.sub('co2-', 'co2- ',s)
    string = s.split(' ')
    global est
    est = factorial(len(string))
    permute_helper(string, 0, permlist.add)
    perms = permlist.get()

    if len(perms) > 1000:
        print 'found ' + str(len(perms)) + ' permutations of ' + s
    return perms


######################################################################
# validate_string_list(string_list)
#   Since the string_list file is produced by hand, we should check
#   it for formatting compliance before relying on it.

def validate_string_list(string_list):
    wrong = []

    for s in string_list:
        if len(s) != 3:
            wrong.append(s)

    if wrong != []:
        print 'Error in string_list: The following strings are improperly formatted:'
        print wrong
        sys.exit


######################################################################
# make_intermediate_resource(in_profile, out_profile, string_list)
#   Given a tsdb++ profile in in_profile that containts harvester
#   strings with their parses and MRSs ids and a table that lists
#   all harvester and other seed-strings and their mrs-ids, create
#   a new profile in out_profile that contains all the harverster and
#   seed strings matched with appropriate MRSs.  No filtering or
#   permutations yet.

def make_intermediate_resource(string_list_file, in_profile, out_profile):
    # Make sure the profile paths end in slashes
    if in_profile[-1] != '/':
        in_profile += '/'
    if out_profile[-1] != '/':
        out_profile += '/'

    # Read in the items, parses, results, and strings
    items = read_profile_file(in_profile + 'item')
    parses = read_profile_file(in_profile + 'parse')
    results = read_profile_file(in_profile + 'result')
    # String list is an @-delimited file with the following fields:
    # mrs-id (alphanumeric string)
    # status (`h' or `s')
    # string

    # I'm assuming that mrs-ids are unique among harvester strings.
    # BUT: Multiple harvester strings might have the same actual MRS.
    # ALSO: A single harvester string might have multiple MRSs.  It
    # will still only have one mrs-id.
    string_list = read_profile_file(string_list_file)
    validate_string_list(string_list)

    # Loop once through items, putting mrs-id in i-comment for
    # the harvester string.

    for i in items:
        string = i[6]
        mrs_id = ''

        for m in string_list:
            if m[2] == string:
                mrs_id = m[0]

        i[9] = mrs_id

        if not mrs_id:
            print 'Warning: The harvester string ' + string + ' has no associated mrs-id.'
            print 'This means many filters won\'t work on permutations of this string.'

    # Figure out where to start with i-id, parse-id, and result-id

    next_i = 0
    for i in items:
        if i[0] >= next_i:
            next_i = int(i[0]) + 1
    next_p = 0
    for p in parses:
        if p[0] >= next_p:
            next_p = int(p[0]) + 1
    next_r = 0
    for r in results:
        if r[1] >= next_r:
            next_r = int(r[1]) + 1


    # For each seed string in string_list
    # Look through items looking for existing string with same
    # mrs-id (could be original harvester, could be another seed string)
    # If found, create new item with new string, copying parse and
    # result information from existing item
    # If not found, print a warning to STDOUT and add no item

    # ERB 2006-10-12 Oops: This was adding each item multiple times,
    # if the mrs_id was already in there multiply.  That gets big
    # fast!  Added break statement so that it moves on to the next
    # string after adding at most _one_ item for the previous string.

    for s in string_list:
        if s[1] == 's':
            found = ''
            for i in copy(items):
                if i[9] == s[0]:
                    found = 't'
                    for p in copy(parses):
                        if p[2] == i[0]:
                            for r in copy(results):
                                if r[0] == p[0]:
                                    new_r = copy(r)
                                    new_r[1] = str(next_r)
                                    next_r += 1
                                    new_r[0] = str(next_p)
                                    results.append(new_r)
                            new_p = copy(p)
                            new_p[0] = str(next_p)
                            next_p += 1
                            new_p[2] = str(next_i)
                            parses.append(new_p)
                    new_i = copy(i)
                    new_i[0] = str(next_i)
                    next_i += 1
                    new_i[6] = s[2]
                    items.append(new_i)
                    break
            if not found:
                print 'Warning: No harvester string for mrs-id ' + s[0] +'. String not added: ' + s[2]

    # Write out the items, parses, and result
    if not os.path.exists(out_profile):
        os.mkdir(out_profile)
    write_profile_file(items, out_profile + 'item')
    write_profile_file(parses, out_profile + 'parse')
    write_profile_file(results, out_profile + 'result')

    # Copy the other files, which we need even though they are empty
    copy_other_files(in_profile,out_profile)


######################################################################
# make_master_resource(string_list_file,in_profile, out_profile)
#   Given a tsdb++ profile in in_profile that contains harvest- and
#   seed-strings with their parses and MRSs, create a new profile
#   in out_profile that contains all the permutations of those
#   sentences matched with appropriate MRSs. No filtering at this point
#   beyond disallowing prefixes as final elements and suffixes as
#   initial elements.

def make_master_resource(string_list_file, in_profile, out_profile):
    # Make sure the profile paths end in slashes
    if in_profile[-1] != '/':
        in_profile += '/'
    if out_profile[-1] != '/':
        out_profile += '/'

    # Determine rate at which to keep universally ungrammatical examples,
    # so we don't get swamped.

    string_list = read_profile_file(string_list_file)
    local_est = 0
    local_max = 0
    local_total = 0
    len_list = len(string_list)

    for m in copy(string_list):
        m = re.sub('-neg',' neg',m[2])
        m = re.sub('neg-','neg ',m)
        m = re.sub('co1-','co1 ',m)
        m = re.sub('-co1',' co1',m)
        m = re.sub('co2-','co2 ',m)
        m = re.sub('-co2',' co2',m)
        words = m.split(' ')
        l = len(words)
        local_total += l
        if l > local_max:
            local_max = l
        local_est += factorial(l)

    avg_len = local_total / len_list
    print 'Total permutations considered: ' + str(local_est)
    print 'Longest string is ' + str(local_max) + ' words long'
    print 'Average string length is ' + str(avg_len) + ' words'

    # Read in the items, parses, and results
    items = read_profile_file(in_profile + 'item')
    parses = read_profile_file(in_profile + 'parse')
    results = read_profile_file(in_profile + 'result')

    next_i = 0
    for i in items:
        if i[0] >= next_i:
            next_i = int(i[0]) + 1
    next_p = 0
    for p in parses:
        if p[0] >= next_p:
            next_p = int(p[0]) + 1
    next_r = 0
    for r in results:
        if r[1] >= next_r:
            next_r = int(r[1]) + 1


    # Pass through the item list, permuting each item and, if the new
    # permuted sentence passes the filters, adding it to the item list
    for i in copy(items):
        perms = permute(i[6],i[9])
        # ERB 2006-10-18
        # For coordination strings especially, we have a lot of redundant
        # strings within each perms list.
        perms.sort()
        for j in range(len(perms) -1, -1, -1):
            if j > 0:
                if perms[j] == perms[j - 1]:
                    #      if (j == 0 or perms[j] != perms[j - 1]) and \
                    #             (j == len(perms) - 1 or perms[j] != perms[j + 1]):
                    del perms[j]

        for perm in perms[1:]:
            # Make a new item...but first, copy any parses that refer to
            # the item being permuted, and any results that refer to
            # those parses
            for p in copy(parses):
                if p[2] == i[0]:
                    for r in copy(results):
                        if r[0] == p[0]:
                            new_r = copy(r)
                            new_r[1] = str(next_r)
                            next_r += 1
                            new_r[0] = str(next_p)
                            results.append(new_r)
                    new_p = copy(p)
                    new_p[0] = str(next_p)
                    next_p += 1
                    new_p[2] = str(next_i)
                    parses.append(new_p)
            new_i = copy(i)
            new_i[0] = str(next_i)
            next_i += 1
            new_i[6] = perm
            items.append(new_i)
        print 'Done with item '+ i[0]

    # Write out the items, parses, and results
    if not os.path.exists(out_profile):
        os.mkdir(out_profile)
    write_profile_file(items, out_profile + 'item')
    write_profile_file(parses, out_profile + 'parse')
    write_profile_file(results, out_profile + 'result')

    # Copy the other files, which we need even though they are empty
    copy_other_files(in_profile,out_profile)


######################################################################
# Main program

string_list_file = sys.argv[1]
in_profile = sys.argv[2]
out_profile = sys.argv[3]

make_intermediate_resource(string_list_file, in_profile, out_profile)
make_master_resource(string_list_file, in_profile, out_profile)


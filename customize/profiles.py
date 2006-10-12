#!/usr/local/bin/python2.4

######################################################################
# imports

import os

from copy import copy
import re

from utils import read_choices

from randgram import random_validated_grammar

import shutil
import sys

from getopt import getopt

######################################################################
# globals

choices = {}


######################################################################
# ch(s)
#   Return the value of choice s, or '' if it has none

def ch(s):
  if choices.has_key(s):
    return choices[s]
  else:
    return ''

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
# filter_word_order(sent, mrs_id)

def filter_word_order(sent, mrs_id):

  #General filters:
  #If we're not talking about coordination seed strings, we expect
  #NPs to be some sequence of det, p-nom|p-acc, and s or o.
  

  return False


######################################################################
# filter_sentential_negation(sent, mrs_id)

def filter_sentential_negation(sent, mrs_id):
  return False


######################################################################
# filter_coordination(sent)
#   Take a sentence which consists of:
#     det, s1, s2, s3, iv, iv1, iv2, iv3, co
#   ...and return True iff the sentence cannot be grammatical.
#
#   This function assumes a single coordination strategy (cs1)

def filter_coordination(sent, mrs_id):
  if ch('cs1'):
    if re.search('co co', sent):
      return True
    elif re.search('co-', sent) and re.search('-co', sent):
      return True
    elif ch('cs1pat') == 'a' and re.match('co', sent):
      return True
    elif ch('cs1order') == 'before' and re.search('co$', sent):
      return True
    elif ch('cs1order') == 'after' and re.search('^co', sent):
      return True

  return False


######################################################################
# filter_yesno_questions(sent)

def filter_yesno_questions(sent, mrs_id):

  # The question particle has to be sentence-initial or sentence-final
  if re.match('^.+qpart.+$', sent):
    return True

  # If the language doesn't use question particles, we shouldn't see any
  elif ch('ques') != 'qpart' and ch('ques') and re.match('^.*qpart.*$', sent):
    return True

  # Follow the specification on whether the question particles are
  # sentence initial or sentence final
  elif ch('ques') == 'qpart' and ch('qpartposthead') == '+' and re.match('^qpart.*$', sent):
    return True
  elif ch('ques') == 'qpart' and ch('qpartposthead') == '-' and re.match('^.*qpart$', sent):
    return True

  return False


######################################################################
# filter_lexicon(sent)

def filter_lexicon(sent, mrs_id):
  return False


######################################################################
# filter_sentence(sent)
#   Return True iff the sentence cannot be grammatical

def filter_sentence(sent, mrs_id):
  return filter_word_order(sent, mrs_id) or \
         filter_sentential_negation(sent, mrs_id) or \
         filter_coordination(sent, mrs_id) or \
         filter_yesno_questions(sent, mrs_id) or \
         filter_lexicon(sent, mrs_id)


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

def permute_helper(words):
  perms = []
  if len(words) == 0:
    return perms
  elif len(words) == 1:
    perms.append(words)
  else:
    for i in range(len(words)):
      temp_words = copy(words)
      temp_words.pop(i)
      temp_perms = permute_helper(temp_words)
      for p in temp_perms:
        p.insert(0, words[i])
        perms.append(p)

  return perms


######################################################################
# This calls the permute_helper above

def permute(s):
  perms = []
  wordlists = permute_helper(s.split(' '))
  for words in wordlists:
    perm = words[0]
    for w in words[1:]:
      perm += ' ' + w
    perms.append(perm)
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
  # ALSO: A single harvester string migth have multiple MRSs.  It
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
      print 'Warning: The string ' + string + ' has no associated mrs-id.'

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
# make_universal_resource(in_profile, out_profile)
#   Given a tsdb++ profile in in_profile that contains harvest- and
#   seed-strings with their parses and MRSs, create a new profile
#   in out_profile that contains all the permutations of those
#   sentences matched with appropriate MRSs, filtering out any
#   sentences that are known to be ungrammatical even in the absence
#   of any grammar.

def make_universal_resource(in_profile, out_profile):
  # Make sure the profile paths end in slashes
  if in_profile[-1] != '/':
    in_profile += '/'
  if out_profile[-1] != '/':
    out_profile += '/'
    
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
    for perm in permute(i[6])[1:]:
      if not filter_sentence(perm, i[9]):
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

  # Write out the items, parses, and results
  if not os.path.exists(out_profile):
    os.mkdir(out_profile)
  write_profile_file(items, out_profile + 'item')
  write_profile_file(parses, out_profile + 'parse')
  write_profile_file(results, out_profile + 'result')

  # Copy the other files, which we need even though they are empty
  copy_other_files(in_profile,out_profile)


######################################################################
# make_gold_standard(in_profile, out_profile)
#   Given a tsdb++ profile in in_profile, remove from that profile
#   any sentences that are ungrammatical with respect to the
#   currently-loaded choices.  After that, collapse any sentences
#   that happen to be identical into single items.

def make_gold_standard(in_profile, out_profile):
  # Make sure the profile paths end in slashes
  if in_profile[-1] != '/':
    in_profile += '/'
  if out_profile[-1] != '/':
    out_profile += '/'
    
  # Read in the items, parses, and results
  items = read_profile_file(in_profile + 'item')
  parses = read_profile_file(in_profile + 'parse')
  results = read_profile_file(in_profile + 'result')

  # Pass through the item list, checking to see if each item passes
  # the filters -- if not, mark it for removal by settings its id
  # to -1
  for i in items:
    if filter_sentence(i[6], i[9]):
      for p in parses:
        if p[2] == i[0]:
          for r in results:
            if r[0] == p[0]:
              r[1] = -1
          p[0] = -1
      i[0] = -1

  # Pass through the lists *backwards*, removing any items whose id
  # has been set to -1
  for i in range(len(items) - 1, -1, -1):
    if items[i][0] == -1:
      del items[i]
  for p in range(len(parses) - 1, -1, -1):
    if parses[p][0] == -1:
      del parses[p]
  for r in range(len(results) - 1, -1, -1):
    if results[r][1] == -1:
      del results[r]

  # Now we need to collapse any duplicate sentences -- that is, if
  # two sentences contain the same words, they should be made into
  # a single item.  This happens in ??? steps:
  #  1) Make a list of the sentences, sort it, and remove any
  #     sentence that doesn't occur more than once.
  #  2) For each of those sentences, find all items that contain it
  #  3) For all but the first of those items, find the parses that
  #     point to them
  #  4) Point all results that point to those parses to the first of
  #     them, and remove the rest of the parses

  # make a list of sentences and sort them
  sent = []
  for i in items:
    sent.append(i[6])
  sent.sort()

  # remove the unique sentences
  for i in range(len(sent) - 1, -1, -1):
    if (i == 0 or sent[i] != sent[i - 1]) and \
       (i == len(sent) - 1 or sent[i] != sent[i + 1]):
      del sent[i]

  # now remove duplicates
  for i in range(len(sent) - 1, 0, -1):
    if sent[i] and sent[i] == sent[i - 1]:
      del sent[i]

  # find the ids of items with duplicate sentences
  sent_IDs = []
  for s in sent:
    IDs = []
    for i in items:
      if i[6] == s:
        IDs.append(i[0])
    sent_IDs.append(IDs)

  # mark extra parses and items for removal, pointing results to the
  # remaining, single parse
  for IDs in sent_IDs:
    # find the new single parse ID
    for p in parses:
      if p[2] == IDs[0]:
        the_parse_ID = p[0]
        break
    # for the rest of the ids, mark items with that id for removal,
    # and mark parses pointing to those items for removal
    for ID in IDs[1:]:
      for i in items:
        if i[0] == ID:
          i[0] = -1
      for p in parses:
        if p[2] == ID:
          for r in results:
            if r[0] == p[0]:
              r[0] = the_parse_ID
          p[0] = -1

  # finally, remove items and parses that have been marked with -1
  for i in range(len(items) - 1, -1, -1):
    if items[i][0] == -1:
      del items[i]
  for p in range(len(parses) - 1, -1, -1):
    if parses[p][0] == -1:
      del parses[p]

  # Write out the items, parses, and result  
  if not os.path.exists(out_profile):
    os.mkdir(out_profile)
  write_profile_file(items, out_profile + 'item')
  write_profile_file(parses, out_profile + 'parse')
  write_profile_file(results, out_profile + 'result')

  # Copy the other files, which we need even though they are empty
  copy_other_files(in_profile,out_profile)

######################################################################
# main program

# profiles.py -i          Create intermediate resource and stop
# profiles.py -u          Create univeral resource and gold profile
# profiles.py <file>      Create gold standard profile for language
#                         described in <file> on the basis of universal
#                         resource in u_profile.
# profiles.py -u <file>   Update u_profile and create gold standard profile
#                         on the basis of updated u_profile.

(options, args) = getopt(sys.argv[1:],'iu')

i_flag = ''
u_flag = ''

for o in options:
  if re.search('i',o[0]):
    i_flag = 'true'
  if re.search('u',o[0]):
    u_flag = 'true'

#The intermediate resource doesn't involve any filtering, and
# so we shouldn't recreate it with each run. Make the user specify
# with the -i flag that the intermediate resource needs to be
# recreated.

if i_flag:
  make_intermediate_resource('string_list','profile','i_profile')
else:
  # The universal resource takes a long time, so enable skipping it
  # for testing language-specific filters
  if not u_flag:
    make_universal_resource('i_profile', 'u_profile')

  # See if the user specified a choices_file to use
  if len(args) > 0:
    choices_file = args[0]
  else:
    choices_file = 'rand_choices'
    random_validated_grammar(choices_file)
  
    choices = read_choices(choices_file)

  make_gold_standard('u_profile', 'g_profile')

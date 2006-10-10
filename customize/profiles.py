#!/usr/local/bin/python2.4

######################################################################
# imports

import os

from copy import copy
import re

from utils import read_choices

from randgram import random_validated_grammar


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
# filter_word_order(sent)

def filter_word_order(sent):
  return False


######################################################################
# filter_sentential_negation(sent)

def filter_sentential_negation(sent):
  return False


######################################################################
# filter_coordination(sent)
#   Take a sentence which consists of:
#     det, s1, s2, s3, iv, iv1, iv2, iv3, co
#   ...and return True iff the sentence cannot be grammatical.
#
#   This function assumes a single lexically-marked coordination
#   strategy marked by co.

def filter_coordination(sent):
  if re.match('^.*co co.*$', sent):
    return True
  elif ch('cs1order') == 'before' and re.match('^.*co$', sent):
    return True
  elif ch('cs1order') == 'after' and re.match('^co.*$', sent):
    return True
  elif ch('cs1pat') == 'a' and re.match('^.*co.*$', sent):
    return True

  return False


######################################################################
# filter_yesno_questions(sent)

def filter_yesno_questions(sent):
  return False


######################################################################
# filter_lexicon(sent)

def filter_lexicon(sent):
  return False


######################################################################
# filter_sentence(sent)
#   Return True iff the sentence cannot be grammatical

def filter_sentence(sent):
  return filter_word_order(sent) or \
         filter_sentential_negation(sent) or \
         filter_coordination(sent) or \
         filter_yesno_questions(sent) or \
         filter_lexicon(sent)


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
      if not filter_sentence(perm):
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
    if filter_sentence(i[6]):
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

  # Write out the items, parses, and results
  if not os.path.exists(out_profile):
    os.mkdir(out_profile)
  write_profile_file(items, out_profile + 'item')
  write_profile_file(parses, out_profile + 'parse')
  write_profile_file(results, out_profile + 'result')


######################################################################
# main program

make_universal_resource('profile', 'u_profile')

choices_file = 'rand_choices'
random_validated_grammar(choices_file)
choices = read_choices(choices_file)

make_gold_standard('u_profile', 'g_profile')

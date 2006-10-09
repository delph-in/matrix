#!/usr/local/bin/python2.4

######################################################################
# imports

import sys
write = sys.stdout.write
import re

import utils
load_choices = utils.load_choices


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
# filter_word_order(in_sent)

def filter_word_order(in_sent):
  out_sent = in_sent
  return out_sent


######################################################################
# filter_sentential_negation(in_sent)

def filter_sentential_negation(in_sent):
  out_sent = in_sent
  return out_sent


######################################################################
# filter_coordination(in_sent)
#   Take a list of sentences, which are strings consisting of:
#     det, s1, s2, s3, iv, iv1, iv2, iv3, co
#   ...and filter them -- if the sentence matches a pattern that
#   cannot be grammatical, remove it.  Return the filtered list.
#
#   This function assumes a single lexically-marked coordination
#   strategy marked by co.

def filter_coordination(in_sent):
  out_sent = []
  for s in in_sent:
    # Test various ungrammatical conditions, doing nothing if they
    # are true.  If none of them are true, add the sentence to the
    # output list.
    if re.match('^.*co co.*$', s):
      pass
    elif ch('cs1order') == 'before' and re.match('^.*co$', s):
      pass
    elif ch('cs1order') == 'after' and re.match('^co.*$', s):
      pass
    elif ch('cs1pat') == 'a' and re.match('^.*co.*$'):
      pass
    else:
      out_sent.append(s)
  
  return out_sent


######################################################################
# filter_yesno_questions(in_sent)

def filter_yesno_questions(in_sent):
  out_sent = in_sent
  return out_sent


######################################################################
# filter_lexicon(in_sent)

def filter_lexicon(in_sent):
  out_sent = in_sent
  return out_sent


######################################################################
# filter_sentences(choices_file)
#   Read the choices from choices_file, then read sentences from
#   stdin.  Filter out bad sentences, then print the remaining ones.

def filter_sentences(choices_file):
  global choices
  choices = load_choices(choices_file)

  sent = sys.stdin.readlines()

  for i in range(len(sent)):
    sent[i] = sent[i].strip()

  sent = filter_word_order(sent)
  sent = filter_sentential_negation(sent)
  sent = filter_coordination(sent)
  sent = filter_yesno_questions(sent)
  sent = filter_lexicon(sent)

  for s in sent:
    print s


######################################################################
# main program

filter_sentences(sys.argv[1])

#!/usr/local/bin/python

######################################################################
# imports

import os
import tdl

######################################################################
# globals

choice = {}

mylang = None
rules = None
irules = None
lrules = None
lexicon = None
roots = None


######################################################################
# customize_word_order()
#   Create the type definitions associated with the user's choices
#   about basic word order.

def customize_word_order():
  pass


######################################################################
# customize_sentential_negation()
#   Create the type definitions associated with the user's choices
#   about sentential negation.

def customize_sentential_negation():
  pass


######################################################################
# customize_coordination()
#   Create the type definitions associated with the user's choices
#   about coordination.

def customize_coordination():
  pass


######################################################################
# customize_yesno_questions()
#   Create the type definitions associated with the user's choices
#   about matrix yes/no questions.

def customize_yesno_questions():
  pass


######################################################################
# customize_lexicon()
#   Create the type definitions associated with the user's test
#   lexicon.

def customize_lexicon():
  pass


######################################################################
# customize_test_sentences()
#   Create the script file entries for the user's test sentences.

def customize_test_sentences():
  pass


######################################################################
# customize_matrix(path)
#   Create and prepare for download a copy of the matrix based on
#   the choices file in the directory 'path'.  This function
#   assumes that validation of the choices has already occurred.

def customize_matrix(path):
  try:
    f = open(path + '/choices', 'r')
    lines = f.readlines()
    f.close()
    for l in lines:
      l = l.strip()
      w = l.split('=')
      choice[w[0]] = w[1]
  except:
    pass

  path += '/matrix/'

  # TODO: copy the matrix
  if not os.path.exists(path):
    os.mkdir(path)

  mylang =  tdl.TDLfile(path + choice['language'].lower() + '.tdl')
  rules =   tdl.TDLfile(path + 'rules.tdl')
  irules =  tdl.TDLfile(path + 'irules.tdl')
  lrules =  tdl.TDLfile(path + 'lrules.tdl')
  lexicon = tdl.TDLfile(path + 'lexicon.tdl')
  roots =   tdl.TDLfile(path + 'roots.tdl')

  customize_word_order()
  customize_sentential_negation()
  customize_coordination()
  customize_yesno_questions()
  customize_lexicon()
  customize_test_sentences()

  mylang.save()
  rules.save()
  irules.save()
  lrules.save()
  lexicon.save()
  roots.save()

  # TODO: zip it up

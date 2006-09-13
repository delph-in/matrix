#!/usr/local/bin/python

######################################################################
# imports

######################################################################
# globals

choice = {}
wrong = []


######################################################################
# validate_word_order()
#   Validate the user's choices about basic word order.

def validate_word_order():
  pass


######################################################################
# validate_sentential_negation()
#   Validate the user's choices about sentential negation.

def validate_sentential_negation():
  pass


######################################################################
# validate_coordination()
#   Validate the user's choices about coordination.

def validate_coordination():
  pass


######################################################################
# validate_yesno_questions()
#   Validate the user's choices about matrix yes/no questions.

def validate_yesno_questions():
  pass


######################################################################
# validate_lexicon()
#   Validate the user's choices about the test lexicon.

def validate_lexicon():
  pass


######################################################################
# validate_test_sentences()
#   Validate the user's choices about test sentences.

def validate_test_sentences():
  pass


######################################################################
# validate_choices(path)
#   Validate the choices file found in the directory 'path'.  Return
#   the names of choice file variables that are incorrect (stored
#   in the list 'wrong'.

def validate_choices(path):
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

  validate_word_order()
  validate_sentential_negation()
  validate_coordination()
  validate_yesno_questions()
  validate_lexicon()
  validate_test_sentences()

  return wrong

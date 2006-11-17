#!/usr/local/bin/python2.4

######################################################################
# imports

from random import randint
from StringIO import StringIO

import utils
tokenize_def = utils.tokenize_def
import validate
validate_choices = validate.validate_choices


######################################################################
# globals

varname = []
var = {}

######################################################################
# lexicon: Storing the pos lexicon forms here and pred names


# Note that by making the non-finite forms of the verbs the same
# as the finite forms, we're cutting out a class of grammars (validation
# won't accept those that sepecify non-finite forms).  The seed strings
# current assume it's this way anyway, but it would be nice to elaborate
# the seed strings and be able to test this functionality.

lexicon = {'noun1': 'n1', 'noun1pred': '_n1_n_rel', \
           'noun2': 'n2', 'noun2pred': '_n2_n_rel', \
           'tverb': 'tv', 'tvpred': '_tv_v_rel', \
           'iverb': 'iv', 'ivpred': '_iv_v_rel', \
           'auxverb': 'aux', 'auxpred': 'auxpred', \
           'det1': 'det', 'det1pred': '_det_q_rel', \
           'det2': 'det2', 'det2pred': '_det2_q_rel', \
           'subjAdpForm': 'p-nom', \
           'objAdpForm': 'p-acc', \
           'negadvform': 'neg' , \
           'qpartform': 'qpart', \
           'neg-aff-form': 'neg', \
           'cs1orth': 'co1', 'cs2orth': 'co2', \
           'iverb-nonfinite': 'iv', 'tverb-nonfinite': 'tv', \
           'language': 'rand_language', \
           'sentence1': 'sentence1', 'sentence2': 'sentence2' }


def load_vars():
  # only need to load once
  if len(varname) and len(var):
    return

  f = open('matrixdef', 'r')
  line = f.readlines()
  f.close()
  
  i = 0
  while i < len(line):
    word = tokenize_def(line[i])
    if len(word) == 0:
      pass
    elif word[0] == 'Label':
      pass
    elif word[0] == 'Separator':
      pass
    elif word[0] == 'Section':
      # don't bother to put in the first 'Section' or else first line is blank
      if len(varname):
        varname.append(word[0])
    elif word[0] == 'Check':
      vn = word[1]
      varname.append(vn)
      var[vn] = []
      var[vn].append('BOOLEAN')
    elif word[0] == 'Radio':
      vn = word[1]
      varname.append(vn)
      # In case of nested radio buttons, we might be looking
      # at a button in a series that was started some lines previously.
      if not var.has_key(vn):
        var[vn] = []
      i += 1
      while line[i] != '\n':
        word = tokenize_def(line[i])
        rval = word[1]
        var[vn].append(rval)
        i += 1
    elif word[0] == 'Text':
      vn = word[1]
      varname.append(vn)
      var[vn] = []
      var[vn].append(lexicon[vn])
    i += 1


def random_grammar(choices_file):
  load_vars()
  choice = {}
  for k in var.keys():
    v = var[k]
    if len(v) == 1 and v[0] == 'BOOLEAN':
      if randint(0, 1):
        choice[k] = 'on'
    else:
      # One out of N times, don't specify a value
      N = 3
      if randint(1, N) != 1:
        choice[k] = v[randint(0, len(v) - 1)]

  if type(choices_file) == str:
    f = open(choices_file, 'w')
  else:
    f = choices_file

  for k in varname:
    if k == 'Section':
      f.write('\n')
    elif choice.has_key(k):
      f.write(k + '=' + choice[k] + '\n')

  if type(choices_file) == str:
    f.close()


def random_validated_grammar(choices_file, do_extra):
  load_vars()
  count = 0
  while True:
    count += 1
    sio = StringIO()
    random_grammar(sio)
    wrong = validate_choices(sio, do_extra)
    if len(wrong) == 0:
      f = open(choices_file, 'w')
      f.write(sio.getvalue())
      f.close()
      return count

#########################################################
# main program, for when we want to call it independently
# to make some grammars for us.

#count = random_validated_grammar('rand_choices', True)
#print 'Whew!  Grammar number ' + str(count) + ' validated.'

#!/usr/local/bin/python2.4

######################################################################
# imports

import os

from copy import copy
import re

from utils import read_choices

from random import randint

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
#
# ERb 2006-10-12 This was getting unweidly, so I broke it up a bit.

def word_order_general_filters(sent, mrs_id):

  #General filters:
  #If we're not talking about coordination seed strings, we expect
  #NPs to be some sequence of det, p-nom|p-acc, and s or o.
  #Furthermore, the dets have to be inside the ps.
  #BUT: p-nom can attach to s and p-acc can attach to o, I think.
  if not re.search('coord',mrs_id):
    if re.search('wo1',mrs_id):
      if re.search('p-nom',sent) and not re.search('p-nom n1|n1 p-nom', sent):
        return True

    # These mrs_ids have a det attached to n1.
    if re.search('wo2|wo5|wo10',mrs_id):
      if not re.search('det n1|n1 det',sent):
        return True

    # These ones have one attached to o.
    if re.search('wo6|wo9',mrs_id):
      if not re.search('det n2|n2 det',sent):
        return True

    if re.search('wo2',mrs_id):
      if re.search('p-nom', sent) and not re.search('p-nom det|p-nom n1|det p-nom|n1 p-nom',sent):
        return True

    # Possibly add some of the neg or ques examples as mrs_ids this
    # one gets applied to.
    if re.search('wo3',mrs_id):
      # If there's only one adposition, it has to be next to a noun
      if ((re.search('p-nom',sent) and not re.search('p-acc',sent)) or
          (re.search('p-acc',sent) and not re.search('p-nom',sent))):
        if not re.search('p-(nom|acc) (n1|n2)|(n1|n2) p-(nom|acc)',sent):
          return True

      # If there are two adpositions, each has to be next to a noun
      if re.search('p-nom',sent) and re.search('p-acc',sent):
        if not (re.search('p-(nom|acc) (n1|n2).*(n1|n2) p-(nom|acc)',sent) or
                re.search('p-(nom|acc) (n1|n2).*p-(nom|acc) (n1|n2)',sent) or
                re.search('(n1|n2) p-(nom|acc).*p-(nom|acc) (n1|n2)',sent) or
                re.search('(n1|n2) p-(nom|acc).*(n1|n2) p-(nom|acc)',sent)):
          return True

    # Possibly add some of the neg or ques examples as mrs_ids this
    # one gets applied to.
    if re.search('wo4|wo8',mrs_id):
      # The determiners both have to be adjancent to nouns:
      # Can't just use the filters for wo2/5 and wo6 together, because
      # they would license n1 det n2.
      if not (re.search('det (n1|n2).*det (n1|n2)',sent) or
              re.search('det (n1|n2).*(n1|n2) det',sent) or
              re.search('(n1|n2) det.*det (n1|n2)',sent) or
              re.search('(n1|n2) det.*(n1|n2) det',sent)):
        return True

    if re.search('wo4|wo5|wo6',mrs_id):
      # If there's only one adposition, it has to be next to a noun or a det
      if ((re.search('p-nom',sent) and not re.search('p-acc',sent)) or
          (re.search('p-acc',sent) and not re.search('p-nom',sent))):
        if not re.search('p-(nom|acc) (n1|n2|det)|(n1|n2|det) p-(nom|acc)',sent):
          return True

      # If there are two adpositions, each one has to be next to a noun or a det
      # And no fair taking the noun and det from one NP and using them for
      # both Ps:  p-nom n1 det p-acc tv aux n2 det
      # There's probably a simpler way to do this, but as a first pass:
      if not (re.search('p-(nom|acc) (n1|n2).*(n1|n2) p-(nom|acc)',sent) or
              re.search('p-(nom|acc) (n1|n2).*p-(nom|acc) (n1|n2)',sent) or
              re.search('(n1|n2) p-(nom|acc).*p-(nom|acc) (n1|n2)',sent) or
              re.search('(n1|n2) p-(nom|acc).*(n1|n2) p-(nom|acc)',sent) or
              re.search('p-(nom|acc) det (n1|n2).*(n1|n2) p-(nom|acc)',sent) or
              re.search('p-(nom|acc) det (n1|n2).*p-(nom|acc) (n1|n2)',sent) or
              re.search('(n1|n2) det p-(nom|acc).*p-(nom|acc) (n1|n2)',sent) or
              re.search('(n1|n2) det p-(nom|acc).*(n1|n2) p-(nom|acc)',sent) or
              re.search('p-(nom|acc) (n1|n2).*(n1|n2) det p-(nom|acc)',sent) or
              re.search('p-(nom|acc) (n1|n2).*p-(nom|acc) det (n1|n2)',sent) or
              re.search('(n1|n2) p-(nom|acc).*p-(nom|acc) det (n1|n2)',sent) or
              re.search('(n1|n2) p-(nom|acc).*(n1|n2) det p-(nom|acc)',sent) or
              re.search('p-(nom|acc) det (n1|n2).*(n1|n2) det p-(nom|acc)',sent) or
              re.search('p-(nom|acc) det (n1|n2).*p-(nom|acc) det (n1|n2)',sent) or
              re.search('(n1|n2) det p-(nom|acc).*p-(nom|acc) det (n1|n2)',sent) or
              re.search('(n1|n2) det p-(nom|acc).*(n1|n2) det p-(nom|acc)',sent)):
        return True

  return False

def word_order_specific_filters(sent, mrs_id):
  #Now do things that are specific to constraints from the choices file.

  wo = ch('wordorder')
  #For the seed strings where the 's' is the actual subject
  #Check wither the actual order of major constituents matches
  #the order indicated in the choices file.
  if re.match('^wo[1-6]$',mrs_id):
    if re.search('s.*o',wo) and re.search('n2.*n1',sent):
      return True
    if re.search('o.*s',wo) and re.search('n1.*n2',sent):
      return True
    if re.search('o.*v',wo) and re.search('tv.*n2',sent):
      return True
    if re.search('v.*o',wo) and re.search('n2.*tv',sent):
      return True
    if re.search('s.*v',wo) and re.search('(tv|iv).*n1',sent):
      return True
    if re.search('v.*s',wo) and re.search('n1.*(tv|iv)',sent):
      return True

  #Likewise, for the seed strings where the 'o' is the actual subject
  if re.match('^wo([7-9]|10)$',mrs_id):
    if re.search('s.*o',wo) and re.search('n1.*n2',sent):
      return True
    if re.search('o.*s',wo) and re.search('n2.*n1',sent):
      return True
    if re.search('o.*v',wo) and re.search('tv.*n1',sent):
      return True
    if re.search('v.*o',wo) and re.search('n1.*tv',sent):
      return True
    if re.search('s.*v',wo) and re.search('(tv|iv).*n2',sent):
      return True
    if re.search('v.*s',wo) and re.search('n2.*(tv|iv)',sent):
      return True

  #For, the v-final and v-initial cases, just check whether any arguments
  #show up on the wrong side of the verb.
  if re.search('wo',mrs_id):
    if wo == 'v-final' and re.search('(tv|iv).*(n1|n2)',sent):
      return True
    if wo == 'v-initial' and re.search('(n1|n2).*(tv|iv)',sent):
      return True

  #A very general filter to save us some time, in some cases:
  if ch('hasDets') == 'no' and re.search('det',sent):
    return True


  #Det-n order
  #These are not relative to particular mrs_ids because the only strings
  #with dets left by the general filters should have the dets adjacent to
  #the right n.
  # ... still have to worry about specific mrs_ids because of det n det
  #pattern.  I need to know how many dets there are.
  # Still, these are fairly general.  I expect to add lots of mrs_ids here.

  if ch('NounDetOrder') == 'HeadSpec':
    if re.search('wo[2569]|wo10',mrs_id) and \
       re.search('det (n1|n2)',sent):
      return True

    if re.search('wo[48]',mrs_id) and \
       (re.search('det .*det (n1|n2)',sent) or \
        re.search('det (n1|n2) .*det (n1|n2)', sent) or \
        re.search('det (n1|n2) .*(n1|n2) det', sent)):
      return True

  if ch('NounDetOrder') == 'SpecHead':
    if re.search('wo[2569]|wo10',mrs_id) and \
       re.search('(n1|n2) det',sent):
      return True

    if re.search('wo[48]',mrs_id) and \
       (re.search('(n1|n2) det .*det ',sent) or \
        re.search('(n1|n2) det .*(n1|n2) det', sent) or \
        re.search('det (n1|n2) .*(n1|n2) det', sent)):
      return True


  return False

def filter_word_order(sent, mrs_id):
  return word_order_general_filters(sent, mrs_id) or\
         word_order_specific_filters(sent, mrs_id)

######################################################################
# filter_sentential_negation(sent, mrs_id)

def filter_sentential_negation(sent, mrs_id):

  # Because we spelled the adverb and the affix the same:
  negadv = re.compile(r'(\s|^)neg(\s|$)')

  if re.search('neg',mrs_id):
    # If we haven't selected inflectional negation, we shouldn't see the affixes
    if ch('infl_neg') != 'on' and re.search('-neg|neg-',sent):
      return True
    # If we haven't select adverbial negation, we shouldn't see the adverbs
    if ch('adv_neg') != 'on' and re.search(negadv,sent):
      return True
    # If both are required, we should see always see both affix & adv.
    if ch('multineg') == 'bothobl':
      if not (re.search('-neg|neg-',sent) and re.search(negadv,sent)):
        return True
    # If complementary dist, we should only see one or the other.
    if ch('multineg') == 'comp':
      if re.search('-neg|neg-',sent) and re.search(negadv,sent):
        return True
    # If affix can't occur without adverb, it shouldn't
    if ch('multineg') == 'advobl' and re.search('-neg|neg-',sent):
      if not re.search(negadv,sent):
        return True
    # If adverb can't occur without affix, it shouldn't
    if ch('multineg') == 'inflobl' and re.search(negadv,sent):
      if not re.search('-neg|neg-',sent):
        return True

    # If affix only attaches to aux, shouldn't attach to anything else.
    if ch('neg-infl-type') == 'aux' and re.search('[^(aux)]-neg|neg-[^(aux)]',sent):
      return True
    # If affix only attaches to main verb, shouldn't attach to anything else.
    if ch('neg-infl-type') == 'main' and re.search('[^(tv)(iv)]-neg|neg-[^(tv)(iv)]',sent):
      return True

    # If we've got an independent modifier, it should show up on the correct
    # side of the verb.  Note that this only applies in sentences without
    # negative affixes.  If there's both an affix and an adverb, we treat
    # the adverb as selected, regardless of what the user put in.
    if ch('neg-adv') == 'ind-adv' and not re.search('-neg|neg-',sent):
      if ch('negprepostmod') == 'left' and re.search('(tv|iv).* neg',sent):
        return True
      if ch('negprepostmod') == 'right' and re.search('neg.* (tv|iv)',sent):
        return True

      #Again, if we know we're talking about an independent modifier, can
      #say where it has to appear wrt V, VP, or S.
      #Are these downcased in the input?
      #So far both neg1 and neg2 involve transitive verbs, so not worrying
      #about the intransitive case.
      if ch('negmod') == 's':
        if re.search('n[12].* neg.* n[12].* tv',sent) or \
           re.search('n[12].* neg.* tv.* n[12]',sent) or \
           re.search('n[12].* n[12].* neg.* tv',sent) or \
           re.search('tv.* neg.* n[12].* n[12]',sent) or \
           re.search('tv.* n[12].* neg.* n[12]',sent) or \
           re.search('n[12].* tv.* neg.* n[12]',sent):
          return True
      if ch('negmod') == 'vp':
        pass #FIX_ME
      if ch('negmod') == 'v':
        if not re.search('neg (aux|tv)|(aux|tv) neg',sent):
          return True


  return False


######################################################################
# filter_coordination(sent)
#   Take a sentence which consists of:
#     det, s1, s2, s3, iv, iv1, iv2, iv3, co
#   ...and return True iff the sentence cannot be grammatical.

def filter_coordination(sent, mrs_id):
  # for most of these filters to work, there must be at least one
  # coordination strategy, and if there are multiple ones, they
  # must agree in some parameter
  order = None
  pat = None
  first_strat = True
  for i in (1, 2):
    i = str(i)
    if ch('cs' + i):
      if first_strat:
        first_strat = False
        order = ch('cs' + i + 'order')
        pat = ch('cs' + i + 'pat')
      else:
        if order != ch('cs' + i + 'order'):
          order = None
        if pat != ch('cs' + i + 'pat'):
          pat = None

  if order and re.search('co co', sent):
    return True
  elif order and re.search('co-', sent) and re.search('-co', sent):
    return True
  elif pat == 'a' and re.search('co', sent):
    return True
  elif order == 'before' and re.search('co$', sent):
    return True
  elif order == 'after' and re.search('^co', sent):
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

# ERB 2006-10-12 The general filters related to this are all in
# filter_word_order.

def filter_lexicon(sent, mrs_id):

    # This assumes that noun1 has the form n1, noun2 has the form n2, etc.
  if re.search('wo|neg|ques',mrs_id):
    if ch('noun1spr') == 'obl' and re.search('n1', sent):
      if not re.search('n1 det|det n1',sent):
        return True
    if ch('noun2spr') == 'obl' and re.search('n2', sent):
      if not re.search('n2 det|det n2',sent):
        return True
    #This one assumes that all nouns are n+digit(s), and that we only
    #see n1 once/string.
    if ch('noun1spr') == 'nil':
      if re.search('n1 det', sent):
        if re.search('n1 det n2', sent):
          if re.search('n1 det n2 det',sent):
            return True
          elif ch('noun2spr') == nil:
            return True
        else:
          return True
      if re.search('det n1', sent):
        if re.search('n2 det n1', sent):
          if re.search('det n2 det n1',sent):
            return True
          elif ch('noun2spr') == nil:
            return True
        else:
          return True
            
    #This one assumes that all nouns are n+digit(s), and that we only
    #see n2 once/string.
    if ch('noun2spr') == 'nil':
      if re.search('n2 det', sent):
        if re.search('n2 det n1', sent):
          if re.search('n2 det n1 det'):
            return True
          elif ch('noun1spr') == nil:
            return True
        else:
          return True
      if re.search('det n2', sent):
        if re.search('n1 det n2', sent):
          if re.search('det n1 det n2', sent):
            return True
          elif ch('noun1spr') == nil:
            return True
        else:
          return True

    if ch('iverbSubj') == 'pp':
      # We're talking about intransitive verbs here.  n1 is always
      # the subject.
      if re.search('wo[12]$',mrs_id):
        if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
          return True

    if ch('iverbSubj') == 'np':
      if re.search('wo[12]$',mrs_id):
        if re.search('p-nom',sent):
          return True

    #I think we don't have to worry about picking up a det or a p-nom from the other np,
    #because the general filters should have taken out those cases. ??
    # _FIX_ME_ need to add neg and ques examples here.
    if ch('tverbSubj') == 'pp':
      if re.search('wo[3-6]',mrs_id):
        if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
          return True
      # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
      if re.search('wo[7-9]|wo10',mrs_id):
        if not re.search('p-nom n2|n2 p-nom|p-nom det n2|p-nom n2 det|n2 det p-nom|det n2 p-nom',sent):
          return True

    if ch('tverbSubj') == 'np' and re.search('p-nom',sent):
        return True

    # _FIX_ME_ need to add neg and ques examples here.
    if ch('tverbObj') == 'pp':
      if re.search('wo[3-6]',mrs_id):
        if not re.search('p-acc n2|n2 p-acc|p-acc det n2|p-acc n2 det|n2 det p-acc|det n2 p-acc',sent):
          return True
      # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
      if re.search('wo[7-9]|wo10',mrs_id):
        if not re.search('p-acc n1|n1 p-acc|p-acc det n1|p-acc n1 det|n1 det p-acc|det n1 p-acc',sent):
          return True
      
    if ch('tverbObj') == 'np' and re.search('p-acc',sent):
        return True

    # _FIX_ME_ need to add neg and ques examples here.
    if ch('objAdp') == 'pre':
      if re.search('wo[1-6]$',mrs_id):
        if re.search('n2 p-acc|n2 det p-acc',sent):
          return True
    if ch('objAdp') == 'post':
      if re.search('wo[1-6]$',mrs_id):
        if re.search('p-acc n2|p-acc det n2',sent):
          return True
    if ch('subjAdp') == 'pre':
      if re.search('wo[1-6]$',mrs_id):
        if re.search('n1 p-nom|n1 det p-nom',sent):
          return True
    if ch('subjAdp') == 'post':
      if re.search('wo[1-6]$',mrs_id):
        if re.search('p-nom n1|p-nom det n1',sent):
          return True

    # Aux cases:
    # _FIX_ME_ worry about questions here
    # 1. No auxiliaries

    if ch('language') != '' and ch('auxverb') == '' and re.search('aux',sent):
        return True
      
    # 2. Arg composition => adjacent to v, let's say for now even in free word order
    #        may be further specified as left only or right only.

    if re.search('wo|neg',mrs_id):
      if ch('auxverb') and ch('auxcomp')=='V':
        if ch('auxorder') == 'right':
          if re.search('aux',sent) and not re.search('(tv|iv) (neg )*aux',sent):
            return True
        elif ch('auxorder') == 'left':
          if re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)',sent):
            return True
        elif re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)|(tv|iv) (neg )*aux',sent):
          return True
      
      # 3. VP comp, so S attaches outside: aux n1,n2 tv or aux n1 tv
      #        may be further specified as left only or right only

      if ch('auxverb') and ch('auxcomp')=='VP':
        if re.search('aux .*n1 .*n2 .*tv|aux .*n2 .*n1 .*tv|tv .*n2 .*n1 .*aux|tv .*n1 .*n2 aux',sent):
          return True
        if re.search('aux .*n1 .*iv|iv .*n1 .*aux',sent):
          return True
    
    # 4. S comp, attaches outside S and O.
    #        may be further specified as left only or right only

      if ch('auxverb') and ch('auxcomp') == 'S':
        if re.search('(n1|n2) .*aux .*(tv|iv)',sent) or \
           re.search('(tv|iv) .*aux .*(n1|n2)',sent):
          return True

    # This left or right only seems to work across all complementation cases.
    
      if ch('auxorder') == 'right' and re.search('aux .*(tv|iv)',sent):
        return True
      if ch('auxorder') == 'left' and re.search('(tv|iv) .*aux',sent):
        return True
      
      
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

# FIX_ME: permute_helper should also do the right thing with affixes.

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

  print 'found ' + str(len(perms)) + ' permutations'
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
# maybe_keep_filtered()
#   One time out of N, return True, otherwise False

def maybe_keep_filtered():
  N = 100
  return randint(1, N) == 1


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

  survived = 0
  killed = 0

  # Pass through the item list, permuting each item and, if the new
  # permuted sentence passes the filters, adding it to the item list
  for i in copy(items):
    for perm in permute(i[6])[1:]:
      filtered = filter_sentence(perm, i[9])
      keep_filtered = maybe_keep_filtered()
      if not filtered or keep_filtered:
        survived += 1
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
        # if the sentence should have been filtered, remember that
        if filtered:
          new_i[7] = '0'
        items.append(new_i)
      else:
        killed += 1

  print str(survived) + ' strings survived into universal resource.'
  print str(killed) + ' strings were filtered.'

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
  filtered = 0
  
  for i in items:
    if filter_sentence(i[6], i[9]):
      if maybe_keep_filtered():
        i[7] = '0'
      else:
        filtered += 1
        for p in parses:
          if p[2] == i[0]:
            for r in results:
              if r[0] == p[0]:
                r[1] = None
            p[0] = None
        i[0] = None

  print str(len(items)) + ' items received from universal resource.'
  print str(filtered) + ' items filtered.'

  # Pass through the lists *backwards*, removing any items, parses, or
  # results whose IDs have been set to None
  for i in range(len(items) - 1, -1, -1):
    if items[i][0] == None:
      del items[i]
  for p in range(len(parses) - 1, -1, -1):
    if parses[p][0] == None:
      del parses[p]
  for r in range(len(results) - 1, -1, -1):
    if results[r][1] == None:
      del results[r]

  # Now we need to collapse any duplicate sentences -- that is, if
  # two sentences contain the same words, they should be made into
  # a single item.  This happens in four steps:
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
        # grammatical IDs at the beginning, ungrammatical ones at the end
        if i[7] == '1':
          IDs.insert(0, i[0])
        else:
          IDs.append(i[0])
    sent_IDs.append(IDs)

  # mark extra parses and items for removal, pointing results to the
  # remaining, single parse
  for IDs in sent_IDs:
    # because of the way the IDs list was constructed, if any of the
    # sentences were grammatical, the first will be one of them;
    # conversely, if the first is ungrammatical, they all are
    the_item_ID = None
    for i in items:
      if i[0] == IDs[0] and i[7] == '1':
        the_item_ID = i[0]
        break
    # find the parse ID for the one grammatical sentence
    the_parse_ID = None
    for p in parses:
      if p[2] == the_item_ID:
        the_parse_ID = p[0]
        break
    # for the rest of the ids, mark items with that id for removal,
    # and mark parses pointing to those items for removal
    for ID in IDs:
      if ID != the_item_ID:
        for i in items:
          if i[0] == ID:
            i[0] = None
            grammatical = (i[7] == '1')
        for p in parses:
          if p[2] == ID:
            for r in results:
              if r[0] == p[0]:
                if grammatical:
                  r[0] = the_parse_ID
                else:
                  r[1] = None
            p[0] = None

  # finally, remove items, parses, and results marked with None
  for i in range(len(items) - 1, -1, -1):
    if items[i][0] == None:
      del items[i]
  for p in range(len(parses) - 1, -1, -1):
    if parses[p][0] == None:
      del parses[p]
  for r in range(len(results) - 1, -1, -1):
    if results[r][1] == None:
      del results[r]

  print str(len(items)) + ' unique items found.'

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
#                         on the basis of updated u_profile and choices
#                         file <file>

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

### $Id: profiles.py,v 1.34 2008-05-28 21:08:12 sfd Exp $

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

import math

######################################################################
# globals

choices = {}
killed = 0
survived = 0
u_kept = 0
est = 0

######################################################################
# ch(s)
#   Return the value of choice s, or '' if it has none

def ch(s):
    if choices.has_key(s):
        return choices[s]
    else:
        return ''

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
# filter_word_order(sent, mrs_id)
#
# ERb 2006-10-12 This was getting unweidly, so I broke it up a bit.

def word_order_general_filters(sent, mrs_id):

    #If the coordination filters passed us a sentence that still has 'co' in
    #it, reject.

    if not re.search('co',mrs_id) and re.search('co',sent):
        return True

    #General filters:
    #If we're not talking about coordination seed strings, we expect
    #NPs to be some sequence of det, p-nom|p-acc, and s or o.
    #Furthermore, the dets have to be inside the ps.
    #BUT: p-nom can attach to s and p-acc can attach to o, I think.
    if not re.search('co',mrs_id):
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
        if re.search('wo4|wo8|neg[24]|ques[24]',mrs_id):
            # The determiners both have to be adjancent to nouns:
            # Can't just use the filters for wo2/5 and wo6 together, because
            # they would license n1 det n2.
            if not (re.search(r'det (n1|n2).*det (n1|n2)',sent) or
                        re.search(r'det (n1|n2).*(n1|n2) det',sent) or
                        re.search(r'(n1|n2) det.*det (n1|n2)',sent) or
                        re.search(r'(n1|n2) det.*(n1|n2) det',sent)):
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
    if re.search('wo[1-6]$|neg[12]$',mrs_id) or (re.search('ques[12]',mrs_id) and \
                                                         re.search('qpart',sent)):
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
    if re.match('^wo([7-9]|10)$|^neg[34]',mrs_id) or \
            (re.search('ques[34]',mrs_id) and re.search('qpart',sent)):
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
    if ch('hasDets') == 'nil' and re.search('det',sent):
        return True


    #Det-n order
    #These are not relative to particular mrs_ids because the only strings
    #with dets left by the general filters should have the dets adjacent to
    #the right n.
    # ... still have to worry about specific mrs_ids because of det n det
    #pattern.  I need to know how many dets there are.
    # Still, these are fairly general.  I expect to add lots of mrs_ids here.

    # ERB 2006-11-28 Oops: Not that general.  I was ruling out 'n1 det n2 tv'
    # for an SOV spec-head language because the det which goes with n2 was
    # adjacent to and after n1.  Need to pin this down a little more.

    if ch('NounDetOrder') == 'HeadSpec':
        # These ones have just one determiner, and it's on n1
        if re.search('wo[25]|wo10',mrs_id) and \
                re.search('det n1',sent):
            return True

        # These ones have just one determiner, and it's on n2
        if re.search('wo[69]',mrs_id) and \
                re.search('det n2',sent):
            return True

        # These one have two determiners
        if re.search('wo[48]|neg[24]|ques[24]',mrs_id) and \
                (re.search('det .*det (n1|n2)',sent) or \
                         re.search('det (n1|n2) .*det (n1|n2)', sent) or \
                         re.search('det (n1|n2) .*(n1|n2) det', sent)):
            return True

    if ch('NounDetOrder') == 'SpecHead':
        if re.search('wo[2569]|wo10',mrs_id) and \
                re.search('(n1|n2) det',sent):
            return True

        if re.search('wo[48]|neg[24]|ques[24]',mrs_id) and \
                (re.search('(n1|n2) det .*det ',sent) or \
                         re.search('(n1|n2) det .*(n1|n2) det', sent) or \
                         re.search('det (n1|n2) .*(n1|n2) det', sent)):
            return True

    return False

def filter_word_order(sent, mrs_id):
    return word_order_general_filters(sent, mrs_id) or \
           word_order_specific_filters(sent, mrs_id)

######################################################################
# filter_sentential_negation(sent, mrs_id)

def filter_sentential_negation(sent, mrs_id,phase):

    if re.search('neg-',sent) and not re.search (r'neg-(tv|iv|aux)',sent):
        return True
    if re.search('-neg',sent) and not re.search (r'(tv|iv|aux)-neg',sent):
        return True

    # That's all for the general features

    if phase == 'u':
        return False

    # Because we spelled the adverb and the affix the same:
    negadv = re.compile(r'(\s|^)neg(\s|$)')

    if re.search('neg',mrs_id):
        # If we haven't selected inflectional negation, we shouldn't see the affixes
        if ch('infl_neg') != 'on' and re.search('-neg|neg-',sent):
            return True
        # If we haven't selected adverbial negation, we shouldn't see the adverbs
        if ch('adv_neg') != 'on' and re.search(negadv,sent):
            return True
        # If we only selected affix, we should see one in every negated sentence
        if ch('adv_neg') != 'on' and ch('infl_neg') == 'on':
            if not re.search('-neg|neg-',sent):
                return True
        # If we only selected adverbs, we should see one in eveyr negated sentence
        if ch('adv_neg') == 'on' and ch('infl_neg') != 'on':
            if not re.search(negadv,sent):
                return True
        # If both are required, we should see always see both affix & adv.
        if ch('infl_neg') == 'on' and ch('adv_neg') == 'on' and ch('multineg') == 'bothobl':
            if not (re.search('-neg|neg-',sent) and re.search(negadv,sent)):
                return True
        # If complementary dist, we should only see one or the other.
        if ch('infl_neg') == 'on' and ch('adv_neg') == 'on' and ch('multineg') == 'comp':
            if re.search('-neg|neg-',sent) and re.search(negadv,sent):
                return True
        # If affix can't occur without adverb, it shouldn't
        if ch('infl_neg') == 'on' and ch('adv_neg') == 'on' and \
                        ch('multineg') == 'advobl' and re.search('-neg|neg-',sent):
            if not re.search(negadv,sent):
                return True
        # If adverb can't occur without affix, it shouldn't
        if ch('infl_neg') == 'on' and ch('adv_neg') == 'on' and \
                        ch('multineg') == 'inflobl' and re.search(negadv,sent):
            if not re.search('-neg|neg-',sent):
                return True

        if ch('infl_neg') == 'on':
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

        if ch('adv_neg') == 'on':
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
                    if re.search(r'n[12].* neg.* n[12].* tv',sent) or \
                            re.search(r'n[12].* neg.* tv.* n[12]',sent) or \
                            re.search(r'n[12].* n[12].* neg.* tv',sent) or \
                            re.search(r'tv.* neg.* n[12].* n[12]',sent) or \
                            re.search(r'tv.* n[12].* neg.* n[12]',sent) or \
                            re.search(r'n[12].* tv.* neg.* n[12]',sent):
                        return True
                if ch('negmod') == 'vp':
                    #If neg-adv attaches to a VP, then it can't intervene between
                    #intervening the object and the verb.  But which is the object?
                    #that depends on which mrs we're talking about.
                    #Likewise, the subject can appear between the neg and the verb.
                    if re.search(r'neg[12]',mrs_id):
                        if re.search(r'tv.* neg.* n2|n2.* neg.* tv.*',sent):
                            return True
                        if re.search(r'neg.* n1.* tv|tv.* n1* neg',sent):
                            return True
                    if re.search(r'neg[34]',mrs_id):
                        if re.search(r'tv.* neg.* n1|n1.* neg.* tv.*',sent):
                            return True
                        if re.search(r'neg.* n2.* tv|tv.* n2* neg',sent):
                            return True
                if ch('negmod') == 'v':
                    #In this case, we have to be right next to the aux or the v.
                    if not re.search(r'neg (aux|tv)|(aux|tv) neg',sent):
                        return True

            if ch('neg-adv') == 'sel-adv' or \
                    (re.search(r'(\s|^)neg(\s|$)',sent) and re.search('-neg|neg-',sent)):
                # These are the cases where we're dealing with a selected
                # complement negative adverb.  Such things should show up
                # where we expect verbal complements: i.e., to the correct side
                # of the verb, and either inside or outside the subject, as
                # requires.
                if ch('wordorder') == 'svo' or \
                                ch('wordorder') == 'vso' or \
                                ch('wordorder') == 'v-initial':
                    if re.search(r'(\s|^)neg .*tv',sent):
                        return True
                if ch('wordorder') == 'ovs' or \
                                ch('wordorder') == 'osv' or \
                                ch('wordorder') == 'v-final':
                    if re.search(r'tv.* neg(\s|$)',sent):
                        return True
                #This is one interpretation of what we might mean.  I think it
                #matches what the tdl actually does.  It might not match the best
                #interpretation of the prose on the web form.
                if ch('wordorder') == 'vso':
                    if re.search(r'neg[12]',mrs_id) and re.search(r'tv.* neg.* n1',sent):
                        return True
                    if re.search(r'neg[34]',mrs_id) and re.search(r'tv.* neg.* n2',sent):
                        return True
                if ch('wordorder') == 'osv':
                    if re.search(r'neg[12]',mrs_id) and re.search(r'n1.* neg.* tv',sent):
                        return True
                    if re.search(r'neg[34]',mrs_id) and re.search(r'n2.* neg.* tv',sent):
                        return True

    return False


######################################################################
# replace_coord(sent, pat, order, co, x, y)
#   A helper function for filter_coordination.  Given a sentence sent,
#   a coordination pattern pat, an order of the coord mark order,
#   a regexp pattern for the coordination mark co, a regexp pattern
#   for the items being coordinated x, and a replacement string y,
#   replace all instances of x coordination with y.
#
#   Note: these all assume exactly three-way coordination

def replace_coord(sent, pat, order, co, x, y):

    if pat == 'mono' and order == 'before':
        regexp = x + ' ' + x + ' ' + co + ' ' + x
        sent = re.sub(regexp, y, sent)
        regexp = x + ' ' + co + ' ' + x + ' ' + co + ' ' + x
        sent = re.sub(regexp, y, sent)
    elif pat == 'mono' and order == 'after':
        regexp = x + ' ' + x + ' ' + x + ' ' + co
        sent = re.sub(regexp, y, sent)
        regexp = x + ' ' + x + ' ' + co + ' ' + x + ' ' + co
        sent = re.sub(regexp, y, sent)
    elif pat == 'poly' and order == 'before':
        regexp = x + ' ' + co + ' ' + x + ' ' + co + ' ' + x
        sent = re.sub(regexp, y, sent)
    elif pat == 'poly' and order == 'after':
        regexp = x + ' ' + x + ' ' + co + ' ' + x + ' ' + co
        sent = re.sub(regexp, y, sent)
    elif pat == 'omni' and order == 'before':
        regexp = co + ' ' + x + ' ' + co + ' ' + x + ' ' + co + ' ' + x
        sent = re.sub(regexp, y, sent)
    elif pat == 'omni' and order == 'after':
        regexp = x + ' ' + co + ' ' + x + ' ' + co + ' ' + x + ' ' + co
        sent = re.sub(regexp, y, sent)
    elif pat == 'a':
        regexp = x + ' ' + x + ' ' + x
        sent = re.sub(regexp, y, sent)

    return sent


######################################################################
# filter_coordination(sent)
#   Take a sentence which consists of:
#     det, n1, n2, n3, iv, co1, co2
#   ...and return True iff the sentence cannot be grammatical.

def filter_coordination(sent, mrs_id,no_coord):
    if not re.search('co', mrs_id): # HEYEMILY: mrs_id indicates coordination
        return False

    # ERB 2006-10-16: The replace stuff  is only firing if we have a choices
    # file loaded with at least one coordination strategy defined.  We
    # shouldn't pass these on to get killed in the universal resource
    # just because no coordination strategy is defined.  But, if we
    # have a test grammar with no coordination strategy, need to know that
    # we should kill all of these.
    if no_coord:
        return True

    # ERB 2006-10-17 A few universal filters, just focusing on n1
    # coordination for now.

    # If there's an overt coordination mark and it's not adjacent to
    # an n, filter.

    if re.search('co.*n[12]',mrs_id):
        if re.search('co',sent) and \
                not re.search('co[12] n1|co[12]-n1|n1 co[12]|n1-co[12]',sent):
            return True

    # In these examples at least, we shouldn't get sequences of coordination
    # marks next to each other.

    if re.search('co.*n[12]',mrs_id):
        if re.search('co[12] co[12]',sent):
            return True

    # Now language specific filters.  If we don't have a choices file
    # loaded in, just return False for now.

    if not (ch('cs1') or ch('cs2')):
        return False

    # ERB 2006-10-16 First check if we've got the right kind of mark.

    # Assume we don't.
    if ch('cs1') or ch('cs2'):
        has_right_mark = False
        for i in (1,2):
            i = str(i)
            if ch('cs' + i):
                # If asyndeton strategy exists and there's no 'co' in the sentence, good.
                if ch('cs' + i + 'pat') == 'a' and not re.search('co', sent):
                    has_right_mark == True

                else:
                    mark = ch('cs' + i + 'mark')
                    markform = 'co' + i
                    markword = re.compile(r'(\s|^)'+markform+r'(\s|$)')
                    markaff = re.compile(r'-'+markform+r'|'+markform+r'-')
                    # If we\'re supposed to see a word, look for the  word.
                    # Assuming only one coordination per seed-string now.
                    if mark == 'word' and re.search(markword,sent):
                        has_right_mark = True

                        # If we\'re supposed to see affixes, look for affixes
                        # Assuming only one coordination per seed-string now.
                    if mark == 'affix' and not re.search(markaff,sent):
                        has_right_mark = True

        if not has_right_mark:
            return True

    # pre-processing to reduce the complexity later:
    #   separate the affix versions of 'co' and just treat them as words
    sent = re.sub('co1-', 'co1 ', sent)
    sent = re.sub('-co1', ' co1', sent)
    sent = re.sub('co2-', 'co2 ', sent)
    sent = re.sub('-co2', ' co2', sent)

    for i in (1, 2):
        i = str(i)
        if ch('cs' + i):
            pat = ch('cs' + i + 'pat')
            order = ch('cs' + i + 'order')

            # make regexp patterns and replacements for the various phrase types
            # nouns:
            n_pat = 'n[123]'
            n_rep = 'n1'

            # NPs: (note that case-marking adpositions aren't handled)
            # ERB 2006-10-16 I'm not sure they need to be explicitly handled until
            # we worry about PP coordination.  As long as nothing is replacing
            # them away, filter_word_order will check and make sure they're in the
            # right place.

            # ERB 2006-10-16 On the other hand, hasDets doesn't necessarily
            # mean that determiners are required.  At present, that's handled
            # in the noun lexical entries.
            if ch('hasDets') == 't':
                if ch('NounDetOrder') == 'HeadSpec':
                    np_pat = n_pat + ' det'
                    np_rep = n_rep + ' det'
                else:
                    np_pat = 'det ' + n_pat
                    np_rep = 'det ' + n_rep
            else:
                np_pat = n_pat
                np_rep = n_rep

            # VPs: (note that only intransitive verbs are handled)
            vp_pat = 'iv'
            vp_rep = 'iv'


            # ERB 2006-10-16 Here is where I think we get in trouble with the
            # case-marking adpositions. Maybe a more general solution would
            # be to send pieces of the string independently to filter_sentence.
            # This would be easy with poly- and omni- strategies, because you
            # just cut at the coordination mark and send each piece.  With
            # mono- and a-, you'd have to try each possible segmentation, I guess
            # and that could get expensive.  At least we'd only have to do it
            # for sentences with mrs_id indicating s coordination...

            # Ss: (again, only with intransitive verbs)
            if ch('wordorder') in ('sov', 'svo', 'osv', 'v-final'):
                s_pat = np_pat + ' ' + vp_pat
                s_rep = np_rep + ' ' + vp_rep
            elif ch('wordorder') in ('vso', 'ovs', 'vos', 'v-initial'):
                s_pat = vp_pat + ' ' + np_pat
                s_rep = vp_rep + ' ' + np_rep
            elif ch('wordorder') == 'free':
                s_pat = '(' + np_pat + ' ' + vp_pat + ')|(' + \
                        vp_pat + ' ' + np_pat + ')'
                s_rep = np_rep + ' ' + vp_rep

            # replace coordinated Ns with a single N
            if ch('cs' + i + 'n'):
                sent = replace_coord(sent, pat, order, 'co' + i, n_pat, n_rep)

            # replace coordinated NPs with a single NP
            if ch('cs' + i + 'np'):
                sent = replace_coord(sent, pat, order, 'co' + i, np_pat, np_rep)

            # replace coordinated VPs with a single VP
            if ch('cs' + i + 'vp'):
                sent = replace_coord(sent, pat, order, 'co' + i, vp_pat, vp_rep)

            # replace coordinated Ss with a single S
            if ch('cs' + i + 's'):
                sent = replace_coord(sent, pat, order, 'co' + i, s_pat, s_rep)

    if ch('cs1') or ch('cs2'):
        new_mrs_id = ''
        if re.search('co.*n1',mrs_id): new_mrs_id = 'wo1'
        if re.search('co.*n2',mrs_id): new_mrs_id = 'wo2'
        return filter_sentence(sent, new_mrs_id, 'g')

    return False

######################################################################
# filter_yesno_questions(sent)

def filter_yesno_questions(sent, mrs_id, phase):

    # The question particle has to be sentence-initial or sentence-final
    if re.match(r'^.+qpart.+$', sent):
        return True

    # That's it for general filteres.
    if phase == 'u':
        return False

    # If the question strategy is intonation only, then none of the
    # strings attached to the ques[1234] MRSs should be accepted:
    if ch('ques') == 'int' and re.search('ques',mrs_id):
        return True

    # If the language doesn't use question particles, we shouldn't see any
    if ch('ques') != 'qpart' and ch('ques') and re.match('^.*qpart.*$', sent):
        return True


    # If the language does use question particles, we should only see the
    # question semantics when qpart is present.  (Currently, the choice here
    # is radio buttons, rather than checkboxes.  Eventually this should be
    # generalized.)

    if ch('ques') == 'qpart' and \
            re.search('ques',mrs_id) and \
            not re.search('qpart',sent):
        return True

    # Follow the specification on whether the question particles are
    # sentence initial or sentence final
    if ch('ques') == 'qpart' and ch('qpartposthead') == '+' and re.match('^qpart.*$', sent):
        return True
    if ch('ques') == 'qpart' and ch('qpartposthead') == '-' and re.match('^.*qpart$', sent):
        return True

    # Now for the tricky part.  If the language uses inversion, then
    # we should only allow the ques MRSs when we get the appropriate word order.
    # The inversion lexical rule (as written) makes the subject be the first
    # thing on the COMPS list, of either an auxiliary or a main verb.
    # I'd actally be really surprised to find this strategy outside SVO languages
    # anyway...


    # Again, these are written assuming that only one strategy at a time is
    # allowed for questions.  If that's not the case, we'll need to rule out
    # the possibility that we're looking at strings with another strategy first,
    # e.g., by checking for the presence of qpart.

    if re.search('ques',mrs_id):

        # S V O -> V S O
        # V O S -> V S O
        # S V O -> Aux S V O (for either VP or V comp auxiliaries.  S comp auxes just
        #                     shouldn't allow this option).
        # V O S -> Aux S V O
        # S V O -> V O S Aux (if the aux follows)
        # V O S -> V O S Aux

        if ch('wordorder') == 'svo' or ch('wordorder') == 'vos':
            if re.search('ques[12]',mrs_id):
                if not ((re.search('tv.* n1.* n2',sent) and not re.search('aux',sent)) or \
                                re.search('aux.* n1.* tv.* n2',sent) or \
                                re.search('tv.* n2.* n1.* aux',sent)):
                    return True
            if re.search('ques[34]',mrs_id):
                if not ((re.search('tv.* n2.* n1',sent) and not re.search('aux', sent)) or \
                                re.search('aux.* n2.* tv.* n1',sent) or \
                                re.search('tv.* n1.* n2.* aux',sent)):
                    return True

        # O V S -> O S V
        # S O V -> O S V
        # O V S -> O V S Aux
        # S O V -> O V S Aux
        # O V S -> Aux S O V
        # S O V -> Aux S O V

        if ch('wordorder') == 'ovs' or ch('wordorder') == 'sov':
            if re.search('ques[12]',mrs_id):
                if not ((re.search('n2.* n1.* tv',sent) and not re.search('aux',sent)) or \
                                re.search('n2.* tv.* n1.* aux',sent) or \
                                re.search('aux.* n2.* n1.* tv',sent)):
                    return True
            if re.search('ques[34]',mrs_id):
                if not ((re.search('n1.* n2.* tv',sent) and not re.search('aux',sent)) or \
                                re.search('n1.* tv.* n2.* aux',sent) or \
                                re.search('aux.* n1.* n2.* tv',sent)):
                    return True

        # V S O -> V S O
        # V S O -> Aux S V O (VP auxes won't work in VSO languages)
        # V S O -> V O S Aux
        # O S V -> O S V
        # O S V -> O V S Aux (VP auxes won't work in OSV languages)
        # O S V -> Aux S O V

        if ch('wordorder') == 'vso' or ch('wordorder') == 'osv':
            if re.search('ques[12]',mrs_id):
                if not ((re.search('tv.* n1.* n2',sent) and not re.search('aux',sent)) or \
                                re.search('aux.* n1.* tv.* n2',sent) or \
                                re.search('tv.* n2.* n1.* aux',sent)):
                    return True
            if re.search('ques[34]',mrs_id):
                if not ((re.search('tv.* n2.* n1',sent) and not re.search('aux',sent)) or \
                                re.search('aux.* n2.* tv.* n1',sent) or \
                                re.search('tv.* n1.* n2.* aux',sent)):
                    return True

                    # As for V-final or V-initial languages, the predictions are weird.
                    # That is, we get homophony with declaratives.  I think we should just
                    # constrain validate.py to refuse to implement this strategy in V-final
                    # or V-initial languages.  ... In fact, I'm going to do that.
                    # Likewise for free word order.  How could you use word order to
                    # mark questions, if word order is free in general?

    return False


######################################################################
# filter_lexicon(sent)

# ERB 2006-10-12 The general filters related to this are all in
# filter_word_order.

def filter_lexicon(sent, mrs_id):

    # Figure out if we're in a case where the auxiliary is controlling
    # the pos of the subj.

    auxsubj = False
    if re.search('aux',sent) and \
            (ch('auxcomp') == 'VP' or ch('auxcomp') == 'V'):
        auxsubj = True



    # Checking for -nf forms, if appropriate.  The only seed strings
    # with the -nf forms (so far) are those with auxiliaries.  Should
    # eventually test some strings with -nf forms and no aux.... _FIX_ME_

    if re.search('wo|neg|ques',mrs_id):
        if ch('iverb-nonfinite') == 'iv-nf':
            if re.search('aux',sent) and re.search('iv',sent) \
                    and not re.search('iv-nf',sent):
                return True

        if ch('tverb-nonfinite') == 'tv-nf':
            if re.search('aux',sent) and re.search('tv',sent) \
                    and not re.search('tv-nf',sent):
                return True

        if not ch('iverb-nonfinite') and re.search('iv-nf',sent):
            return True

        if not ch('tverb-nonfinite') and re.search('tv-nf',sent):
            return True

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
                    elif ch('noun2spr') == 'nil':
                        return True
                else:
                    return True
            if re.search('det n1', sent):
                if re.search('n2 det n1', sent):
                    if re.search('det n2 det n1',sent):
                        return True
                    elif ch('noun2spr') == 'nil':
                        return True
                else:
                    return True

        #This one assumes that all nouns are n+digit(s), and that we only
        #see n2 once/string.
        if ch('noun2spr') == 'nil':
            if re.search('n2 det', sent):
                if re.search('n2 det n1', sent):
                    if re.search('n2 det n1 det', sent):
                        return True
                    elif ch('noun1spr') == 'nil':
                        return True
                else:
                    return True
            if re.search('det n2', sent):
                if re.search('n1 det n2', sent):
                    if re.search('det n1 det n2', sent):
                        return True
                    elif ch('noun1spr') == 'nil':
                        return True
                else:
                    return True

        if ch('iverbSubj') == 'pp':
            # We're talking about intransitive verbs here.  n1 is always
            # the subject.

            # BUT: The auxiliary might want an np subject, so if there's an
            # aux, we have to consider it separately.
            if re.search('wo[12]$',mrs_id):
                if not auxsubj:
                    if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
                        return True

        if ch('iverbSubj') == 'np':

            # Likewise here, the aux might want a pp subject.

            if re.search('wo[12]$',mrs_id) and not auxsubj:
                if re.search('p-nom',sent):
                    return True

        #I think we don't have to worry about picking up a det or a p-nom from the other np,
        #because the general filters should have taken out those cases. ??

        if ch('tverbSubj') == 'pp' and not auxsubj:
            if re.search('wo[3-6]|neg[12]|ques[12]',mrs_id):
                if not re.search(r'p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
                    return True
            # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
            if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
                if not re.search(r'p-nom n2|n2 p-nom|p-nom det n2|p-nom n2 det|n2 det p-nom|det n2 p-nom',sent):
                    return True

        if ch('tverbSubj') == 'np' and re.search('p-nom',sent) and not auxsubj:
            return True


        if ch('tverbObj') == 'pp':
            if re.search('wo[3-6]|neg[12]|ques[12]',mrs_id):
                if not re.search(r'p-acc n2|n2 p-acc|p-acc det n2|p-acc n2 det|n2 det p-acc|det n2 p-acc',sent):
                    return True
            # possibly redundant: right now there are no seed strings with p-nom or p-acc and these mrs_ids.
            if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
                if not re.search(r'p-acc n1|n1 p-acc|p-acc det n1|p-acc n1 det|n1 det p-acc|det n1 p-acc',sent):
                    return True

        if ch('tverbObj') == 'np' and re.search('p-acc',sent):
            return True

        if ch('objAdp') == 'pre':
            if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
                if re.search('n2 p-acc|n2 det p-acc',sent):
                    return True
        if ch('objAdp') == 'post':
            if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
                if re.search('p-acc n2|p-acc det n2',sent):
                    return True
        if ch('subjAdp') == 'pre':
            if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
                if re.search('n1 p-nom|n1 det p-nom',sent):
                    return True
        if ch('subjAdp') == 'post':
            if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
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
                    if re.search('aux',sent) and not re.search('(tv|iv)(-nf)* (neg )*aux',sent):
                        return True
                elif ch('auxorder') == 'left':
                    if re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)',sent):
                        return True
                elif re.search('aux',sent) and not re.search('aux (neg )*(tv|iv)|(tv|iv)(-nf)* (neg )*aux',sent):
                    return True

            # 3. VP comp, so S attaches outside: aux n1,n2 tv or aux n1 tv
            #        may be further specified as left only or right only

            if ch('auxverb') and ch('auxcomp')=='VP':
                if re.search('aux .*n1 .*n2 .*tv|aux .*n2 .*n1 .*tv|tv(-nf)* .*n2 .*n1 .*aux|tv(-nf)* .*n1 .*n2 aux',sent):
                    return True
                if re.search('aux .*n1 .*iv|iv(-nf)* .*n1 .*aux',sent):
                    return True

                    # 4. S comp, attaches outside S and O.
                    #        may be further specified as left only or right only

            if ch('auxverb') and ch('auxcomp') == 'S':
                if re.search('(n1|n2) .*aux .*(tv|iv)',sent) or \
                        re.search('(tv|iv)(-nf)* .*aux .*(n1|n2)',sent):
                    return True

                    # This left or right only seems to work across all complementation cases.

            if ch('auxorder') == 'right' and re.search('aux .*(tv|iv)',sent):
                return True
            if ch('auxorder') == 'left' and re.search('(tv|iv)(-nf)* .*aux',sent):
                return True

        # Now worry about the pos of the subject in sentences with auxiliaries:

        if auxsubj:

            if ch('auxsubj') == 'noun':
                if re.search('wo|neg|ques',mrs_id):
                    if re.search('p-nom',sent):
                        return True

            if ch('auxsubj') == 'adp':
                if re.search('wo[1-6]$|neg[12]|ques[12]',mrs_id):
                    if not re.search('p-nom n1|n1 p-nom|p-nom det n1|p-nom n1 det|n1 det p-nom|det n1 p-nom',sent):
                        return True
                if re.search('wo[7-9]|wo10|neg[34]|ques[34]',mrs_id):
                    if not re.search('p-nom n2|n2 p-nom|p-nom det n2|p-nom n2 det|n2 det p-nom|det n2 p-nom',sent):
                        return True

    return False


######################################################################
# filter_sentence(sent, mrs_id)
#   Return True iff the sentence cannot be grammatical

def filter_sentence(sent, mrs_id, phase):

    #If we're in the gold-standard creation phase, check whether
    #we should just reject all coordintation examples.
    no_coord = False
    if phase == 'g':
        if not (ch('cs1') or ch('cs2')):
            no_coord = True

        #   if mrs_id == 'neg2':
        #     wo = filter_word_order(sent, mrs_id)
        #     neg = filter_sentential_negation(sent, mrs_id, phase)
        #     co = filter_coordination(sent,mrs_id,no_coord)
        #     q = filter_yesno_questions(sent,mrs_id,phase)
        #     lex = filter_lexicon(sent,mrs_id)

        #     if wo:
        #       return wo
        #     if neg:
        #       print 'neg filtered ' + sent
        #       return neg
        #     if co:
        #       print 'coord filtered ' + sent
        #       return co
        #     if q:
        #       print 'questions filtered ' + sent
        #       return q
        #     if lex:
        #       print 'lexicon filtered ' + sent
        #       return lex


    return filter_word_order(sent, mrs_id) or \
           filter_sentential_negation(sent, mrs_id,phase) or \
           filter_coordination(sent, mrs_id, no_coord) or \
           filter_yesno_questions(sent, mrs_id, phase) or \
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

# def permute_helper(words):
#   perms = []
#   if len(words) == 0:
#     return perms
#   elif len(words) == 1:
#     perms.append(words)
#   else:
#     for i in range(len(words)):
#       temp_words = copy(words)
#       temp_words.pop(i)
#       temp_perms = permute_helper(temp_words)
#       for p in temp_perms:
#         p.insert(0, words[i])
#         perms.append(p)
#   return perms

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
            if filter_sentence(perm,self.mrs_id,'u'):
                self.filtered += 1
                if self.u_kept < 4:
                    if maybe_keep_filtered(keep_prob):
                        self.u_kept += 1
                        self.keeps.append(perm)
            else:
                self.perms.append(perm)



    def get(self):
        return self.perms

    def get_keeps(self):
        return self.keeps

    def filter_count(self):
        return self.filtered

    def keeper_count(self):
        return self.u_kept

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
    keeps = permlist.get_keeps()
    global killed
    killed += permlist.filter_count()
    global u_kept
    u_kept += permlist.keeper_count()
    global survived
    survived += len(perms)

    if len(perms) > 1000:
        print 'found ' + str(len(perms)) + ' permutations of ' + s
    return [perms, keeps]


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

# ERB 2006-10-15 I think we're going to want to retain ungrammatical
# ones at different rates for the u_profile and the g_profile.

def maybe_keep_filtered(N):
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
# make_universal_resource(string_list_file,in_profile, out_profile)
#   Given a tsdb++ profile in in_profile that contains harvest- and
#   seed-strings with their parses and MRSs, create a new profile
#   in out_profile that contains all the permutations of those
#   sentences matched with appropriate MRSs, filtering out any
#   sentences that are known to be ungrammatical even in the absence
#   of any grammar.

def make_universal_resource(string_list_file, in_profile, out_profile):
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
        [perms, keeps] = permute(i[6],i[9])
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
        for keep in keeps:
            for p in copy(parses):
                if p[2] == i[0]:
                    new_p = copy(p)
                    new_p[0] = str(next_p)
                    next_p += 1
                    new_p[2] = str(next_i)
                    parses.append(new_p)
                    # No need to put anything in results for ungrammatical examples.
            new_i = copy(i)
            new_i[0] = str(next_i)
            next_i += 1
            new_i[6] = keep
            new_i[7] = '0'
            items.append(new_i)
        print 'Done with item '+ i[0]

    #  print 'considered ' + str(total_perms) + 'total permutations.'
    print str(u_kept) + ' universally ungrammatical examples kept.'
    print str(survived) + ' total strings survived into universal resource.'
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

    # ERB 2006-10-15 If the item was a negative example kept in
    # the first pass, don't bother trying to filter it, just keep it.
    filtered = 0
    wf = 0
    g_kept = 0

    # ERB 2006-10-17 Determine rate at which to keep ungrammatical
    # examples, so we don't get swamped.

    u_received = len(items)
    if u_received >= 100:
        local_keep_prob = u_received // 100
    else:
        local_keep_prob = 10

    for i in items:
        if i[7] == '1':
            if filter_sentence(i[6], i[9], 'g'):
                filtered += 1
                if maybe_keep_filtered(local_keep_prob):
                    g_kept += 1
                    i[7] = '0'
                else:
                    for p in parses:
                        if p[2] == i[0]:
                            for r in results:
                                if r[0] == p[0]:
                                    r[1] = None
                            p[0] = None
                    i[0] = None
            else:
                wf += 1

    print str(len(items)) + ' items received from universal resource.'
    print str(filtered) + ' items filtered.'
    print str(wf) + ' grammatical examples found.'
    print str(g_kept) + ' locally ungrammatical examples kept.'

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

# profiles.py -i          Go with existing i_profile
# profiles.py -u          Go with existing u_profile
# profiles.py -g          Don't make gold standard: stop with u_profile.
# profiles.py <file>      Update u_profile and create gold standard profile
#                         described in <file>.
# profiles.py -u <file>   Create gold standard profile for language
#                         on the basis of existing u_profile and choices
#                         file <file>

# ERB 2006-10-17 To enable parallelization of the easy kind, now
# passing in file names for profile, u_profile (if applicable), and
# id.  The choices file should be choices_id.

# profiles.py -u <file> <id> Create universal resource on the basis of profile <file>
#                            and string list string_list.id
# profiles.py -g <file> <id> Create language-specific resource on the basis of
#                            universal resource <file> and choices file choices.<id>

# ERB 2006-11-28 For debugging purposes, disable the following unless
# this script is being run as the main program.  (i.e., this should allow
# me to import this file into the interpreter without running anything.)

if __name__ == '__main__':

    (options, args) = getopt(sys.argv[1:],'iug')

    i_flag = ''
    u_flag = ''
    g_flag = ''

    for o in options:
        if re.search('i',o[0]):
            i_flag = 'true'
        if re.search('u',o[0]):
            u_flag = 'true'
        if re.search('g',o[0]):
            g_flag = 'true'


        #The intermediate resource doesn't involve any filtering, and
        # so we shouldn't recreate it with each run. Make the user specify
        # with the -i flag that the intermediate resource needs to be
        # recreated.

        #Okay, I kept confusing myself by not running the intermediate
        #resource part, and that part doesn't take so long.  So I'm flipping
        #the semantics of the -i flag, to make it mean *don't* make the
        #intermediate resource.

        #if not i_flag:
        #  print 'making intermediate resource...'
        #  make_intermediate_resource('string_list','profile','i_profile')

        # The universal resource takes a long time, so enable skipping it
        # for testing language-specific filters

        # Typical: python profiles.py -u coord_profile_all _coord

    if u_flag:
        string_list = 'string_list' + args[1]
        profile = args[0]
        i_profile = 'i_profile' + args[1]
        u_profile = 'u_profile' + args[1]

        make_intermediate_resource(string_list,profile,i_profile)
        make_universal_resource(string_list,i_profile,u_profile)

        # Typical: python profiles.py -g u_profile_coord _022
        # With choices_file_022 in the directory.

    if g_flag:
        # See if the user specified a choices_file to use
        #if len(args) > 0:
        #  choices_file = args[0]
        #else:
        #  choices_file = 'rand_choices'
        #  random_validated_grammar(choices_file,True)

        #Assume now that user specified a choices file.

        choices_file = 'choices_file' + args[1]
        u_profile = args[0]
        g_profile = 'g_profile' + args[1]
        choices = read_choices(choices_file)

        make_gold_standard(u_profile, g_profile)

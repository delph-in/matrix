##################
### VALIDATION ###
##################

def validate(ch, vr):
  if ch.get('infl-neg', default=False):  
    for aux in ch.get('aux', []):
      for cf in aux.get('compfeature', []):  
        if cf.get('name') == 'negation':    
          mess = 'If inflectional negation is selected ' +\
                 'then [negation +] should only be found on bound morphemes.'
          vr.err(cf.full_key, mess)
  validate_sentential_negation(ch, vr)

def validate_sentential_negation(ch, vr):
  
  neginfltype = ch.get('neg-infl-type')
  negseladv = ch.get('neg-sel-adv')

  # ERB 2009-01-23 Commenting out the following because infl-neg is
  # now handled with customize_inflection.  We should eventually give
  # a warning if infl-neg is selected but no lexical rules actually
  # use it.  I think it would make sense for that warning to go
  # on the negation page.

  # If affix is indicated, must select prefix/suffix and
  # main/aux/either and give form.
#   if (ch.get('infl-neg') == 'on'):
#     if (not ch.get('neg-infl-type')):
#       mess = 'If sentential negation is expressed through affixation, you must specify what the affix attaches to.'
#       vr.err('neg-infl-type', mess)
#     if (not ch.get('neg-aff')):
#       mess = 'If sentential negation is expressed through affixation, you must specify whether its a prefix or a suffix'
#       vr.err('neg-aff', mess)
#     if (not ch.get('neg-aff-orth')):
#       mess = 'If sentential negation is expressed through affixation, you must specify the form of the affix'
#       vr.err('neg-aff-orth', mess)
#     # If aux is selected then has-aux = 'yes' must be chosen in word order section
#     if ((neginfltype == 'aux' or neginfltype == 'aux-main') and ch.get('has-aux') != 'yes'):
#         mess = 'You have not indicated on the word order page that your language has auxiliaries.'
#         vr.err('neg-infl-type', mess)

  # If adverb is indicated, must lexical entry, what it modifies, and
  # ind/selected modifier
  if (ch.get('adv-neg') == 'on'):
#    if (not ch.get('neg-adv')):
#      mess = 'If sentential negation is expressed through an adverb, you must specify whether the adverb is a selected complement or an independent modifier.'
#      vr.err('neg-adv', mess)
#    if (ch.get('neg-adv') == 'ind-adv'):
    if (not ch.get('neg-mod')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what type of constituent the adverb modifies.'
      vr.err('neg-mod', mess)
    if (not ch.get('neg-order')):
      mess = 'If sentential negaton is expressed through an adverb, ' +\
             'you must specify what side of its host the adverb attaches to.'
      vr.err('neg-order', mess)
    if (not ch.get('neg-adv-orth')):
      mess = 'If sentential negation is expressed through an adverb, ' +\
             'you must specify the form of the adverb.'
      vr.err('neg-adv-orth', mess)

   # If aux is selected then has-aux = 'yes' must be chosen in word
   # order section
    if ((negseladv == 'aux' or negseladv == 'main-aux') and
        ch.get('has-aux') != 'yes'):
        mess = 'You have not indicated on the word order page ' +\
               'that your language has auxiliaries.'
        vr.err('neg-sel-adv', mess)

   # ERB 2009-01-23 Currently not possible to say how they combine.

#   # If both strategies are checked, then they must say how they combine:
#   if ((ch.get('infl-neg') == 'on') and (ch.get('adv-neg') == 'on')):
#     if (not ch.get('multi-neg')):
#       mess = 'If you have selected both affix and adverb realizations of sentential negation, you must specify how they interact.'
#       vr.err('multi-neg', mess)

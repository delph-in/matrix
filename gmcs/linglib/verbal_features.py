from gmcs.lib import TDLHierarchy


######################################################################
# customize_tense()
# Create tense feature value hierarchies per the user's choices

def init_tense_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('tense')

  tdefn = ch.get('tense-definition')
  if tdefn:
    if tdefn == 'choose':
      ppflist = []
      for ten in ('nonfuture', 'nonpast', 'past', 'present', 'future' ):

        if ten in ch:
          if ten not in ppflist:
            hier.add(ten, 'tense')

          for subtype in ch.get(ten + '-subtype',[]):
            st = subtype.get('name','')
            hier.add(st, ten)

          if ten == 'nonfuture':
            for moreten in ('past', 'present'):
              if moreten in ch:
                hier.add(moreten, ten)
                ppflist.append(moreten)

          if ten == 'nonpast':
            for moreten in ('present', 'future'):
              if moreten in ch:
                hier.add(moreten, ten)
                ppflist.append(moreten)

    elif tdefn == 'build':

      for tense in ch.get('tense',[]):
        name = tense.get('name')

        for supertype in tense.get('supertype',[]):
          supername = supertype.get('name')
          hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_tense(mylang, hierarchies):
  if 'tense' in hierarchies:
    hierarchies['tense'].save(mylang, False)


######################################################################
# customize_aspect()
# Create viewpoint aspect feature value definitions per the user's choices

def init_aspect_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('aspect')

  for aspect in ch.get('aspect',[]):
    name = aspect.get('name')
    for supertype in aspect.get('supertype', []):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier
  elif ch.get('perimper'):
    for asp in ('perfective', 'imperfective'):
      name = asp
      supername = 'aspect'
      hier.add(name, supername)
      hierarchies[hier.name] = hier

def customize_aspect(mylang, hierarchies):
  if 'aspect' in hierarchies:
    hierarchies['aspect'].save(mylang, False)

# customize_situation()
# Create situation aspect feature value definitions per the user's choices

def init_situation_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('situation')

  for situation in ch.get('situation',[]):
    name = situation.get('name')
    for supertype in situation.get('supertype',[]):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier

def customize_situation(mylang, hierarchies):
  if 'situation' in hierarchies:
    mylang.set_section('features')
    mylang.add('situation := sort.')
    mylang.add('tam :+ [SITUATION situation].', section='addenda')
    hierarchies['situation'].save(mylang, False)

######################################################################
# customize_mood()
# Create mood feature value definitions per the user's choices

def init_mood_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('mood')

  for mood in ch.get('mood',[]):
    name = mood.get('name')
    for supertype in mood.get('supertype',[]):
      supername = supertype.get('name')
      hier.add(name, supername)

  if not hier.is_empty():
    hierarchies[hier.name] = hier
  elif ch.get('subjind'):
    for md in ('subjunctive', 'indicative'):
      name = md
      supername = 'mood'
      hier.add(name, supername)
      hierarchies[hier.name] = hier

def customize_mood(mylang, hierarchies):
  if 'mood' in hierarchies:
    hierarchies['mood'].save(mylang, False)


###############################################################
# customize_form()

def init_form_hierarchy(ch, hierarchies):
  """
  Create the FORM hierarchies associated with the user's choices
  about verb forms
  Adds FORM finite and nonfinte values if there are auxiliaries
  or if user specified
  """
  hier = TDLHierarchy('form')

  if ch.get('has-aux') == 'yes' or 'noaux-fin-nf' in ch:

    hier.add('nonfinite', 'form')
###special case if auxiliary selection takes place
    auxsel = auxiliary_selection(ch)
    if not auxsel:
      hier.add('finite', 'form')
    else:
      auxs = auxsel[1]
      st = ''
      t = ''
      for aux in auxs:
        auxf = aux + '-only'
        hier.add(auxf, 'form')
        if st:
          st += ' &'
        st += auxf
        if t:
          t += '-or-'
        t += aux 
      hier.add(t, st)
      hier.add('finite', t)
      auxs_forms = auxsel[0]

    for p in ('nf', 'fin'):

      for subform in ch.get(p + '-subform',[]):
        if p == 'nf':
          sup = 'nonfinite'
        elif p == 'fin':
          sup = 'finite'

        sub = subform.get('name')
        hier.add(sub, sup)
###additional 
        if auxsel:
          if not sub in auxs_forms:
            hier.add(sub, t)
          else:
            for aux in auxs:
              hier.add(sub+'-'+aux, sub)
              hier.add(sub+'-'+aux, aux + '-only')

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def auxiliary_selection(ch):
  auxsel = []
  if ch.get('aux-select') == 'yes':
    asel_form = ch.get('aux-sel-form')
    my_forms = []
    if ',' in asel_form:
      my_forms = asel_form.split(',')
    else:
      my_forms.append(asel_form)
    sel_auxs = ch.get('sel_aux',[])
    my_auxs = []
    for aux in sel_auxs:
      my_auxs.append(aux.get('value'))
    auxsel.append(my_forms)
    auxsel.append(my_auxs)
  return auxsel

def customize_form(mylang, hierarchies):
  if 'form' in hierarchies:
    mylang.add('head :+ [FORM form].', section='addenda')
    hierarchies['form'].save(mylang)

def init_verbal_hierarchies(ch, hierarchies):
  init_tense_hierarchy(ch, hierarchies)
  init_aspect_hierarchy(ch, hierarchies)
  init_situation_hierarchy(ch, hierarchies)
  init_mood_hierarchy(ch, hierarchies)
  init_form_hierarchy(ch, hierarchies)

def customize_verbal_features(mylang, hierarchies):
  customize_form(mylang, hierarchies)
  customize_tense(mylang, hierarchies)
  customize_aspect(mylang, hierarchies)
  customize_situation(mylang, hierarchies)
  customize_mood(mylang, hierarchies)


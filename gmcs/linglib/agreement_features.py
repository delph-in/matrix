from gmcs.lib import TDLHierarchy

######################################################################
# customize_person_and_number()
#   Create the type definitions associated with the user's choices
#   about person and number.

def init_person_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('person')

  for p in ch.persons():
    for st in p[1].split(';'):
      hier.add(p[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def init_number_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('number')

  for n in ch.numbers():
    for st in n[1].split(';'):
      hier.add(n[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def init_pernum_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('pernum')

  for pn in ch.pernums():
    for st in pn[1].split(';'):
      hier.add(pn[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_person_and_number(mylang, climb_agree, hierarchies):
  if 'pernum' in hierarchies:
    mylang.add('png :+ [ PERNUM pernum ].', section='addenda')
    hierarchies['pernum'].save(mylang)
    climb_agree.add('png :+ [ PERNUM pernum ].', comment='section=addenda')
    hierarchies['pernum'].save(climb_agree)
  else:
    if 'person' in hierarchies:
      mylang.add('png :+ [ PER person ].', section='addenda')
      hierarchies['person'].save(mylang)
      climb_agree.add('png :+ [ PER person ].', comment='section=addenda')
      hierarchies['person'].save(climb_agree)

    if 'number' in hierarchies:
      mylang.add('png :+ [ NUM number ].', section='addenda')
      hierarchies['number'].save(mylang)
      climb_agree.add('png :+ [ NUM number ].', comment='section=addenda')
      hierarchies['number'].save(climb_agree)


######################################################################
# customize_gender()
#   Create the type definitions associated with the user's choices
#   about gender.

def init_gender_hierarchy(ch, hierarchies):
  hier = TDLHierarchy('gender')

  for g in ch.genders():
    for st in g[1].split(';'):
      hier.add(g[0], st)

  if not hier.is_empty():
    hierarchies[hier.name] = hier


def customize_gender(mylang, climb_agree, hierarchies):
  if 'gender' in hierarchies:
    mylang.add('png :+ [ GEND gender ].', section='addenda')
    hierarchies['gender'].save(mylang)
    climb_agree.add('png :+ [ GEND gender ].', comment='section=addenda')
    hierarchies['gender'].save(climb_agree)


######################################################################
# customize_other_features()
#   Create the type definitions associated with the user's choices
#   about other features.

def init_other_hierarchies(ch, hierarchies):
  for feature in ch.get('feature',[]):
    feat = feature.get('name','')
    type = feature.get('type','')
    hier = TDLHierarchy(feat, type)

    for value in feature.get('value', []):
      val = value.get('name')
      for supertype in value.get('supertype', []):
        stype = supertype.get('name')
        hier.add(val, stype)

    if not hier.is_empty():
      hierarchies[hier.name] = hier


def customize_other_features(mylang, climb_agree, hierarchies):
  for name in hierarchies:
    h = hierarchies[name]
    feat = h.name
    type = h.type
    # if this hierarchy isn't handled elsewhere, handle it here
    if feat not in ['case', 'person', 'number', 'pernum', 'gender',
                    'form', 'tense', 'aspect', 'situation', 'mood']:
      if type == 'head':
        mylang.add('head :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   section='addenda')
        climb_agree.add('head :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   comment='section=addenda')
      else:
        mylang.add('png :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   section='addenda')
        climb_agree.add('png :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   comment='section=addenda')

      # sfd: If it's an 'index' feature, we should make sure to strip it
      # out in the VPM

      h.save(mylang)


def init_agreement_hierarchies(ch, hierarchies):
  init_person_hierarchy(ch, hierarchies)
  init_number_hierarchy(ch, hierarchies)
  init_pernum_hierarchy(ch, hierarchies)
  init_gender_hierarchy(ch, hierarchies)
  init_other_hierarchies(ch, hierarchies)


def customize_agreement_features(mylang, climb_files, hierarchies):
  climb_agree = climb_files.get('agreement')
  customize_person_and_number(mylang, climb_agree, hierarchies)
  customize_gender(mylang, climb_agree, hierarchies)
  customize_other_features(mylang, climb_agree, hierarchies)

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

def customize_person_and_number(mylang, hierarchies):
  if 'pernum' in hierarchies:
    mylang.add('png :+ [ PERNUM pernum ].', section='addenda')
    hierarchies['pernum'].save(mylang)
  else:
    if 'person' in hierarchies:
      mylang.add('png :+ [ PER person ].', section='addenda')
      hierarchies['person'].save(mylang)
    if 'number' in hierarchies:
      mylang.add('png :+ [ NUM number ].', section='addenda')
      hierarchies['number'].save(mylang)



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


def customize_gender(mylang, hierarchies):
  if 'gender' in hierarchies:
    mylang.add('png :+ [ GEND gender ].', section='addenda')
    hierarchies['gender'].save(mylang)


######################################################################
# customize_other_features()
#   Create the type definitions associated with the user's choices
#   about other features.

def init_other_hierarchies(ch, mylang, hierarchies):
  for feature in ch.get('feature',[]):
    feat = feature.get('name','')
    type = feature.get('type','')
    hier = TDLHierarchy(feat, type)

    if feature.get('new', '') == 'yes':
      for value in feature.get('value', []):
        val = value.get('name')
        for supertype in value.get('supertype', []):
          stype = supertype.get('name')
          hier.add(val, stype) 
    else:
      if type == 'head':
        mylang.add('head :+ [ ' + feat.upper() + ' ' + feature.get('existing', '') + ' ].',
                   section='addenda')
      else:
        mylang.add('png :+ [ ' + feat.upper() + ' ' + feature.get('existing', '') + ' ].',
                   section='addenda')      

    if not hier.is_empty():
      hierarchies[hier.name] = hier

def customize_other_features(mylang, hierarchies):
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
      else:
        mylang.add('png :+ [ ' + feat.upper() + ' ' + feat + ' ].',
                   section='addenda')

      # sfd: If it's an 'index' feature, we should make sure to strip it
      # out in the VPM

      h.save(mylang)


def init_agreement_hierarchies(ch, mylang, hierarchies):
  init_person_hierarchy(ch, hierarchies)
  init_number_hierarchy(ch, hierarchies)
  init_pernum_hierarchy(ch, hierarchies)
  init_gender_hierarchy(ch, hierarchies)
  init_other_hierarchies(ch, mylang, hierarchies)


def customize_agreement_features(mylang, hierarchies):
  customize_person_and_number(mylang, hierarchies)
  customize_gender(mylang, hierarchies)
  customize_other_features(mylang, hierarchies)


def create_vpm_person(ch, vpm):
  literal = ''
  for p in ch.persons():
    literal += '  ' + p[0] + ' <> ' + p[0] + '\n'	

  if literal != '':
    vpm.add_literal('PNG.PER : PNG.PER\n' + literal + '  * <> !')


def create_vpm_number(ch, vpm):
  literal = ''
  for n in ch.numbers():
    literal += '  ' + n[0] + ' <> ' + n[0] + '\n'	

  if literal != '':
    vpm.add_literal('PNG.NUM : PNG.NUM\n' + literal + '  * <> !')


def create_vpm_pernum(ch, vpm):
  literal = ''

  for pn in ch.pernums():
    pos = pn[1].find(';')
    if pos != -1:
      literal += '  ' + pn[0] + ' <> ' + pn[1][:pos] + ' ' + pn[1][pos+1:] + '\n'	

  for p in ch.persons():
    literal += '  ' + p[0] + ' <> ' + p[0] + ' !\n'
    literal += '  ' + p[0] + ' << ' + p[0] + ' *\n'

  for n in ch.numbers():
    literal += '  ' + n[0] + ' <> ! ' + n[0] + '\n'	
    literal += '  ' + n[0] + ' << * ' + n[0] + '\n'

  if literal != '':
    vpm.add_literal('PNG.PERNUM : PNG.PER PNG.NUM\n' + literal + '  * >> ! !\n  ! << * *')


def create_vpm_gender(ch, vpm):
  literal = ''
  for g in ch.genders():
    literal += '  ' + g[0] + ' <> ' + g[0] + '\n'	

  if literal != '':
    vpm.add_literal('PNG.GEND : PNG.GEND\n' + literal + '  * <> !')


def create_vpm_others(ch, vpm):
  literal = ''
  for feature in ch.get('feature',[]):
    type = feature.get('type','')
    if type != 'index':
      continue

    name = feature.get('name','').upper()
    for value in feature.get('value', []):
      val = value.get('name')
      literal += '  ' + val + ' <> ' + val + '\n' 

  if literal != '':
    vpm.add_literal(name + ' : ' + name + '\n' + literal + '  * <> !')


def create_vpm_blocks(ch, vpm, hierarchies):
  if 'pernum' in hierarchies:
      create_vpm_pernum(ch, vpm)
  else:
    create_vpm_person(ch, vpm)
    create_vpm_number(ch, vpm)
  
  create_vpm_gender(ch, vpm)
  create_vpm_others(ch, vpm)



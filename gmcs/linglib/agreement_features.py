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
        if feat not in ('case', 'person', 'number', 'pernum', 'gender',
                        'form', 'tense', 'aspect', 'situation', 'mood'):
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

def make_vpm_order(name, h, o):
    if name not in h.keys(): return []
    for n in h[name]:
        make_vpm_order(n, h, o)
    o.append(name)
    return o

def create_vpm_person(ch, vpm):
    literal = ''
    person = []
    for p in ch.persons():
        if p[0] not in person:
            person.append(p[0])
    person.reverse()
    for p in person:
        literal += '  ' + p + ' <> ' + p + '\n'

    if literal != '':
        vpm.add_literal('PNG.PER : PNG.PER\n' + literal + '  * <> *')
        #vpm.add_literal('PNG.PER : PNG.PER\n' + literal + '  * <> !')
    else:
        vpm.add_literal('PNG.PER : PNG.PER\n  * <> *')

def create_vpm_number(ch, vpm):
    literal = ''
    number = []
    for n in ch.numbers():
        if n[0] not in number:
            number.append(n[0])
    number.reverse()
    for n in number:
        literal += '  ' + n + ' <> ' + n + '\n'

    if literal != '':
        vpm.add_literal('PNG.NUM : PNG.NUM\n' + literal + '  * <> *')
        #vpm.add_literal('PNG.NUM : PNG.NUM\n' + literal + '  * <> !')
    else:
        vpm.add_literal('PNG.NUM : PNG.NUM\n  * <> *')


def create_vpm_pernum(ch, vpm):
    literal = ''

    # person = []
    # for p in ch.persons():
    #   if p[0] not in person:
    #     person.append(p[0])
    # person.reverse()
    # number = []
    # for n in ch.numbers():
    #   if n[0] not in number:
    #     number.append(n[0])
    # number.reverse()

    pernum_key = []
    for pn in ch.pernums():
        if pn[0] not in pernum_key: pernum_key.append(pn[0])
    pernum_key.reverse()

    for pn in pernum_key:
        literal += '  ' + pn + ' <> ' + pn + '\n'
        # if pn not in person and pn not in number:
        #   literal += '  ' + pn + ' <> ' + pernum[pn][:pos] + ' ' + pernum[pn][pos+1:] + '\n'
        # pos = pernum[pn].find(';')
        # if pos != -1:
        #   literal += '  ' + pn + ' <> ' + pernum[pn][:pos] + ' ' + pernum[pn][pos+1:] + '\n'

    # for p in person:
    #   literal += '  ' + p + ' <> ' + p + ' *\n'
    # for n in number:
    #   literal += '  ' + n + ' <> * ' + n + '\n'

    if literal != '':
        vpm.add_literal('PNG.PERNUM : PNG.PERNUM\n' + literal + '  * <> *')
        #vpm.add_literal('PNG.PERNUM : PNG.PER PNG.NUM\n' + literal + '  * >> ! !\n  ! << * *')
    else:
        vpm.add_literal('PNG.PERNUM : PNG.PERNUM\n  * <> *')
        #vpm.add_literal('PNG.PERNUM : PNG.PER PNG.NUM\n  * <> * *')

def create_vpm_gender(ch, vpm):
    literal = ''
    gender = []
    for g in ch.genders():
        if g[0] not in gender:
            gender.append(g[0])
    gender.reverse()
    for g in gender:
        literal += '  ' + g + ' <> ' + g + '\n'

    if literal != '':
        vpm.add_literal('PNG.GEND : PNG.GEND\n' + literal + '  * <> *')
        #vpm.add_literal('PNG.GEND : PNG.GEND\n' + literal + '  * <> !')
    else:
        vpm.add_literal('PNG.GEND : PNG.GEND\n  * <> *')

def create_vpm_others(ch, vpm):

    for feature in ch.get('feature',[]):
        type = feature.get('type','')
        if type != 'index': continue
        literal = ''
        _hier = {}

        name = feature.get('name','').upper()
        for value in feature.get('value', []):
            val = value.get('name')
            if val not in _hier.keys():
                _hier[val] = []
            for supertype in value.get('supertype', []):
                supername = supertype.get('name')
                if supername not in _hier.keys():
                    _hier[supername] = [val]
                else:
                    _hier[supername].append(val)

        order = make_vpm_order(name.lower(), _hier, [])
        for val in order:
            if val == name.lower(): continue
            literal += '  ' + val + ' <> ' + val + '\n'

        if literal != '':
            vpm.add_literal('PNG.' + name + ' : ' + 'PNG.' + name + '\n' + literal + '  * <> *')
            #vpm.add_literal(name + ' : ' + name + '\n' + literal + '  * <> !')

def create_vpm_blocks(ch, vpm, hierarchies):
    create_vpm_others(ch, vpm)
    if len(ch.persons()) == 0 and len(ch.numbers()) == 0 and len(ch.genders()) == 0:
        vpm.add_literal('PNG : PNG\n  * <> *')
    else:
        if 'pernum' in hierarchies:
            create_vpm_pernum(ch, vpm)
        else:
            create_vpm_person(ch, vpm)
            create_vpm_number(ch, vpm)
        create_vpm_gender(ch, vpm)


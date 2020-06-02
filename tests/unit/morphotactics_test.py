import unittest
from gmcs import choices
from gmcs.linglib import morphotactics

class TestMorphotactics(unittest.TestCase):
    def test_is_slot(self):
        # for brevity
        i_s = morphotactics.is_slot
        self.assertEqual(i_s(''), False)
        self.assertEqual(i_s('verb-slot1'), True)
        self.assertEqual(i_s('verb-slot1_name'), False)
        self.assertEqual(i_s('verb'), False)
        self.assertEqual(i_s('slot1'), False)
        self.assertEqual(i_s('verb-slot42'), True)

    #def test_slot_inputs(self):
    #  s_i = morphotactics.slot_inputs
    #  ch = customize.choices.ChoicesFile()
    #  ch.load_choices(lextypes + sequential_verb_slots)
    #  # single input
    #  self.assertEqual(s_i(ch['verb-slot3']), ('verb-slot2',))
    #  # two inputs
    #  self.assertEqual(s_i(ch['verb-slot1']), ('verb1','verb2'))

    def test_create_preceeds_dict(self):
        # no slots
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes)
        self.assertEqual(morphotactics.create_preceeds_dict(choices), {})
        # single slot
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes + simple_no_orth)
        self.assertEqual(morphotactics.create_preceeds_dict(choices),
                         {'noun-slot1':set(['noun1'])})
        # two morphemes (no different)
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes + two_morphemes)
        self.assertEqual(morphotactics.create_preceeds_dict(choices),
                         {'noun-slot1':set(['noun1'])})
        # competing slots
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes + competing_slots)
        self.assertEqual(morphotactics.create_preceeds_dict(choices),
                         {'noun-slot1':set(['noun1']),
                          'noun-slot2':set(['noun1'])})
        # sequential slots
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes + sequential_slots)
        self.assertEqual(morphotactics.create_preceeds_dict(choices),
                         {'noun-slot1':set(['noun1']),
                          'noun-slot2':set(['noun-slot1', 'noun1'])})
        # multiple basetypes, multiple inputs
        choices = customize.choices.ChoicesFile()
        choices.load_choices(lextypes + sequential_verb_slots)
        self.assertEqual(morphotactics.create_preceeds_dict(choices),
                         {'verb-slot1':set(['verb1', 'verb2']),
                          'verb-slot2':set(['verb-slot1', 'verb1', 'verb2']),
                          'verb-slot3':set(['verb-slot1', 'verb-slot2',
                                            'verb1', 'verb2'])})

    #def test_disjunctive_typename(self):
    #  # for brevity
    #  dtname = morphotactics.disjunctive_typename
    #  ch = customize.choices.ChoicesFile()
    #  ch['a1_name'] = 'a'
    #  ch['b1_name'] = 'b'
    #  ch['c1_name'] = 'c'

    #  self.assertEqual(dtname([], ch), '')
    #  self.assertEqual(dtname(sorted(['a1']), ch), 'a')
    #  self.assertEqual(dtname(sorted(['a1','b1']), ch), 'a-or-b')
    #  self.assertEqual(dtname(sorted(['a1','b1','c1']), ch), 'a-or-b-or-c')

    def test_all_inputs(self):
        # for brevity
        ai = morphotactics.all_inputs
        s = sorted
        def prep_lrs(choice_lines):
            ch = customize.choices.ChoicesFile()
            ch.load_choices(choice_lines)
            preceeds = morphotactics.create_preceeds_dict(ch)
            lrs = morphotactics.create_lexical_rules(ch, preceeds)
            return lrs, preceeds, ch
        # no inputs
        lrs, preceeds, ch = prep_lrs(lextypes)
        self.assertEqual(ai('noun1',lrs,preceeds), [])
        self.assertEqual(ai('NOSUCH',lrs,preceeds), [])
        # one input
        lrs, preceeds, ch = prep_lrs(lextypes + simple_no_orth)
        self.assertEqual(ai('noun1',lrs,preceeds), [])
        self.assertEqual(ai('noun-slot1',lrs,preceeds), ['noun1'])
        # multiple inputs
        lrs, preceeds, ch = prep_lrs(lextypes + multiple_basetypes)
        self.assertEqual(s(ai('verb-slot1',lrs,preceeds)), ['verb1','verb2'])
        # multiple slots
        lrs, preceeds, ch = prep_lrs(lextypes + competing_slots)
        self.assertEqual(ai('noun-slot1',lrs,preceeds), ['noun1'])
        self.assertEqual(ai('noun-slot2',lrs,preceeds), ['noun1'])
        # sequential
        lrs, preceeds, ch = prep_lrs(lextypes + sequential_verb_slots)
        self.assertEqual(s(ai('verb-slot1',lrs,preceeds)), ['verb1','verb2'])
        self.assertEqual(s(ai('verb-slot2',lrs,preceeds)), s(['verb1','verb2',
                                                              'verb-slot1']))

    def test_sequential(self):
        # for brevity
        seq = morphotactics.sequential
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + con_dis_junctive)
        preceeds = morphotactics.create_preceeds_dict(ch)
        self.assertEqual(seq('verb-slot1','verb-slot2',preceeds), False)
        self.assertEqual(seq('verb-slot2','verb-slot1',preceeds), False)
        self.assertEqual(seq('verb-slot1','verb-slot3',preceeds), True)
        self.assertEqual(seq('verb-slot3','verb-slot1',preceeds), True)
        self.assertEqual(seq('verb-slot2','verb-slot3',preceeds), True)
        self.assertEqual(seq('verb-slot3','verb-slot2',preceeds), True)
        self.assertEqual(seq('verb-slot3','verb-slot4',preceeds), True)
        self.assertEqual(seq('verb-slot4','verb-slot3',preceeds), True)
        self.assertEqual(seq('verb-slot1','verb-slot4',preceeds), True)
        self.assertEqual(seq('verb-slot4','verb-slot1',preceeds), True)
        self.assertEqual(seq('verb-slot4','verb-slot5',preceeds), False)
        self.assertEqual(seq('verb-slot5','verb-slot4',preceeds), False)
        self.assertEqual(seq('verb-slot1','verb-slot1',preceeds), False)
        self.assertEqual(seq('verb-slot42','verb-slot99',preceeds), False)

    def test_ordered_constraints(self):
        # for brevity
        oc = morphotactics.ordered_constraints
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + extended_con_dis_junctive2)
        preceeds = morphotactics.create_preceeds_dict(ch)
        # one slot
        self.assertEqual(oc(['verb-slot1'], preceeds), ['verb-slot1'])
        # nonsequential, ordered by name
        self.assertEqual(oc(['verb-slot1','verb-slot2'], preceeds),
                         ['verb-slot1','verb-slot2'])
        # nonsequential, not ordered by name
        self.assertEqual(oc(['verb-slot2','verb-slot1'], preceeds),
                         ['verb-slot1','verb-slot2'])
        # sequential
        self.assertEqual(oc(['verb-slot4','verb-slot1'], preceeds),
                         ['verb-slot1','verb-slot4'])
        # sequential and nonsequential
        self.assertEqual(oc(['verb-slot7','verb-slot5','verb-slot4','verb-slot6'],
                            preceeds),
                         ['verb-slot4','verb-slot5','verb-slot6','verb-slot7'])

    def test_minimal_flag_set(self):
        # for brevity
        s = sorted
        mfs = morphotactics.minimal_flag_set
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + con_dis_junctive)
        preceeds = morphotactics.create_preceeds_dict(ch)
        # no constraints
        self.assertEqual(mfs({}, preceeds), [])
        # single constraint (a or b forces c)
        self.assertEqual(mfs(('verb-slot3',), preceeds), [('verb-slot3',)])
        # two constraints, sequential
        self.assertEqual(s(mfs(('verb-slot3', 'verb-slot4'), preceeds)),
                         [('verb-slot3',),('verb-slot4',)])
        # two constraints, non-sequential, same value
        self.assertEqual(s(mfs(('verb-slot4', 'verb-slot5'), preceeds)),
                         [('verb-slot4','verb-slot5',)])
        # four constraints, two non-seq, same val, other two non seq, same val
        self.assertEqual(s(mfs(('verb-slot1', 'verb-slot2',
                                'verb-slot4', 'verb-slot5'), preceeds)),
                         [('verb-slot1','verb-slot2',),
                          ('verb-slot4','verb-slot5',)])
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + extended_con_dis_junctive1)
        preceeds = morphotactics.create_preceeds_dict(ch)
        # disjunctive and sequential constrainted, all same value
        self.assertEqual(s(mfs(('verb-slot4', 'verb-slot5', 'verb-slot6'),
                               preceeds)),
                         [('verb-slot4','verb-slot5',),
                          ('verb-slot5','verb-slot6',)])
        ch.load_choices(lextypes + extended_con_dis_junctive2)
        preceeds = morphotactics.create_preceeds_dict(ch)
        # disjunctive and sequential constrainted, all same value
        self.assertEqual(s(mfs(('verb-slot4', 'verb-slot5',
                                'verb-slot6', 'verb-slot7'), preceeds)),
                         [('verb-slot4','verb-slot5',),
                          ('verb-slot6','verb-slot7',)])

    def test_Morpheme(self):
        # empty
        self.assertRaises(TypeError, morphotactics.Morpheme)
        # name only
        self.assertRaises(TypeError, morphotactics.Morpheme, 'name')
        # minimal
        m = morphotactics.Morpheme('name','slot_key','supertype')
        self.assertEqual(m.name, 'name')
        self.assertEqual(m.supertype, 'supertype')
        self.assertEqual(m.parents, set())
        self.assertEqual(m.orthography, '')
        self.assertEqual(m.features, {})
        # orth
        m = morphotactics.Morpheme('name','slot_key','supertype','orth')
        self.assertEqual(m.orthography, 'orth')

    def test_create_morpheme(self):
        cm = morphotactics.create_morpheme
        Slot = morphotactics.Slot
        # no-orth noun
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + simple_no_orth)
        s = Slot(ch['noun-slot1_name'],'noun-slot1')
        m = cm(ch['noun-slot1_morph1'], s.slot_key, ch)
        self.assertEqual(m.name, 'singular')
        self.assertEqual(m.parents, set(['num-lex-rule','const-lex-rule']))
        self.assertEqual(m.orthography, '')
        self.assertEqual(m.features, {'number':{'value':'sg','head':None}})
        # orth noun
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + simple_with_orth)
        s = Slot(ch['noun-slot1_name'],'noun-slot1')
        m = cm(ch['noun-slot1_morph1'], s.slot_key, ch)
        self.assertEqual(m.name, 'singular')
        self.assertEqual(m.parents, set(['num-lex-rule','infl-lex-rule']))
        self.assertEqual(m.orthography, 'g')
        self.assertEqual(m.features, {'number':{'value':'sg','head':None}})
        # verb
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + multiple_basetypes)
        s = Slot(ch['verb-slot1_name'],'verb-slot1')
        m = cm(ch['verb-slot1_morph1'], s.slot_key, ch)
        self.assertEqual(m.name, '3sg')
        self.assertEqual(m.parents, set(['person-lex-rule','infl-lex-rule']))
        self.assertEqual(m.orthography, 's')
        self.assertEqual(m.features, {'person':{'value':'3','head':'subj'},
                                      'number':{'value':'sg','head':'subj'}})

    def test_create_lexical_rules(self):
        clrs = morphotactics.create_lexical_rules
        # no rules
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes)
        preceeds = morphotactics.create_preceeds_dict(ch)
        self.assertEqual(clrs(ch, preceeds), {})
        # no-orth noun
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + simple_no_orth)
        preceeds = morphotactics.create_preceeds_dict(ch)
        lrs = clrs(ch, preceeds)
        self.assertEqual(len(lrs),2)
        self.assertEqual(lrs['noun-slot1'].name, 'num')
        self.assertEqual(lrs['noun-slot1'].slot_key, 'noun-slot1')
        self.assertEqual(lrs['noun-slot1'].order, 'after')
        self.assertEqual(len(lrs['noun-slot1'].morphs), 1)
        self.assertEqual(lrs['noun-slot1'].morphs[0].name, 'singular')
        self.assertEqual(lrs['noun-slot1'].morphs[0].features,
                         {'number':{'value':'sg','head':None}})
        self.assertEqual(lrs['noun1'].name, 'testnoun-noun')
        self.assertEqual(lrs['noun1'].slot_key, 'noun1')
        # multiple basetypes

        # sequential verb slots
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + sequential_verb_slots)
        preceeds = morphotactics.create_preceeds_dict(ch)
        lrs = clrs(ch, preceeds)
        self.assertEqual(len(lrs),5)
        self.assertEqual(lrs['verb-slot1'].name, 'person')
        self.assertEqual(lrs['verb-slot1'].slot_key, 'verb-slot1')
        self.assertEqual(lrs['verb-slot1'].order, 'after')
        self.assertEqual(len(lrs['verb-slot1'].morphs), 1)
        self.assertEqual(lrs['verb-slot1'].morphs[0].name, '3sg')
        self.assertEqual(len(lrs['verb-slot1'].morphs), 1)
        self.assertEqual(len(lrs['verb-slot2'].morphs), 2)
        self.assertEqual(len(lrs['verb-slot3'].morphs), 1)

    def test_create_input_dict(self):
        cid = morphotactics.create_input_dict
        def prep_lrs(choice_lines):
            ch = customize.choices.ChoicesFile()
            ch.load_choices(choice_lines)
            preceeds = morphotactics.create_preceeds_dict(ch)
            lrs = morphotactics.create_lexical_rules(ch, preceeds)
            return lrs, preceeds, ch
        # single input
        lrs, preceeds, ch = prep_lrs(lextypes + simple_no_orth)
        self.assertEqual(cid(lrs, preceeds), {('noun1',):set(['noun-slot1'])})
        # two slots one input
        lrs, preceeds, ch = prep_lrs(lextypes + competing_slots)
        self.assertEqual(cid(lrs, preceeds),
                         {('noun1',):set(['noun-slot1','noun-slot2'])})
        # one slot two inputs
        lrs, preceeds, ch = prep_lrs(lextypes + multiple_basetypes)
        self.assertEqual(cid(lrs, preceeds),
                         {('verb1','verb2'):set(['verb-slot1'])})
        # more complicated things are not yet handled, but should work ok
        # because of flags. For example, A>B>C>D, A requires B and C, then
        # D's inputs should just be C, even though B doesn't require C
        # extended con-dis-junctive
        #lrs, preceeds, ch = prep_lrs(lextypes + extended_con_dis_junctive2)
        #self.assertEqual(cid(lrs, preceeds),
        #                 {('verb1',):set(['verb-slot1','verb-slot2']),
        #                  ('verb-slot1','verb-slot2'):set(['verb-slot3']),
        #                  ('verb-slot3',):set(['verb-slot4','verb-slot5']),
        #                  ('verb-slot4',):set(['verb-slot6']),
        #                  ('verb-slot5',):set(['verb-slot7'])})

    def test_customize_lexical_rules(self):
        clrs = morphotactics.customize_lexical_rules
        # no rules
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes)
        self.assertEqual(clrs(ch), {})
        # no-orth noun
        ch = customize.choices.ChoicesFile()
        ch.load_choices(lextypes + simple_no_orth)
        lrs = clrs(ch)
        self.assertEqual(len(lrs), 2)
        self.assertEqual(lrs['noun-slot1'].name, 'num')
        self.assertEqual(lrs['noun-slot1'].slot_key, 'noun-slot1')
        self.assertEqual(lrs['noun-slot1'].order, 'after')
        # extended con-dis-junctive
        #ch = customize.choices.ChoicesFile()
        #ch.load_choices(lextypes + extended_con_dis_junctive2)
        #lrs = clrs(ch)


##############################################################################
#### Choices File Strings

lextypes = '''version=21

section=lexicon
  noun1_name=testnoun
  noun1_det=imp
    noun1_stem1_orth=test
    noun1_stem1_pred=test_n_1_rel
  verb1_name=testverb
  verb1_valence=intrans
    verb1_stem1_orth=test
    verb1_stem1_pred=test_v_1_rel
  verb2_name=testverb2
  verb2_valence=trans
    verb2_stem1_orth=test
    verb2_stem1_pred=test_v_2_rel'''.splitlines()

simple_no_orth = '''  noun-slot1_name=num
  noun-slot1_order=after
    noun-slot1_input1_type=noun1
    noun-slot1_morph1_name=singular
      noun-slot1_morph1_feat1_name=number
      noun-slot1_morph1_feat1_value=sg'''.splitlines()

simple_with_orth = '''  noun-slot1_name=num
  noun-slot1_order=after
    noun-slot1_input1_type=noun1
    noun-slot1_morph1_name=singular
    noun-slot1_morph1_orth=g
      noun-slot1_morph1_feat1_name=number
      noun-slot1_morph1_feat1_value=sg'''.splitlines()

two_morphemes = simple_no_orth + \
                '''    noun-slot1_morph2_name=plural
                    noun-slot1_morph2_orth=s
                      noun-slot1_morph2_feat1_name=number
                      noun-slot1_morph2_feat1_value=pl'''.splitlines()

competing_slots = simple_no_orth + \
                  '''  noun-slot2_name=num2
                    noun-slot2_order=after
                      noun-slot2_input1_type=noun1
                      noun-slot2_morph1_name=plural
                        noun-slot2_morph1_feat1_name=number
                        noun-slot2_morph1_feat1_value=pl'''.splitlines()

sequential_slots = simple_no_orth + \
                   '''  noun-slot2_name=num2
                     noun-slot2_order=after
                       noun-slot2_input1_type=noun-slot1
                       noun-slot2_morph1_name=3rd
                         noun-slot2_morph1_feat1_name=person
                         noun-slot2_morph1_feat1_value=pl'''.splitlines()

multiple_basetypes = '''  verb-slot1_name=person
  verb-slot1_order=after
    verb-slot1_input1_type=verb1
    verb-slot1_input2_type=verb2
    verb-slot1_opt=on
    verb-slot1_morph1_name=3sg
    verb-slot1_morph1_orth=s
      verb-slot1_morph1_feat1_name=person
      verb-slot1_morph1_feat1_value=3
      verb-slot1_morph1_feat1_head=subj
      verb-slot1_morph1_feat2_name=number
      verb-slot1_morph1_feat2_value=sg
      verb-slot1_morph1_feat2_head=subj'''.splitlines()

sequential_verb_slots = multiple_basetypes + \
                        '''  verb-slot2_name=tense
                          verb-slot2_order=after
                            verb-slot2_input1_type=verb-slot1
                            verb-slot2_input2_type=verb2
                            verb-slot2_morph1_name=past
                            verb-slot2_morph1_orth=ed
                              verb-slot2_morph1_feat1_name=tense
                              verb-slot2_morph1_feat1_value=past
                              verb-slot2_morph1_feat1_head=verb
                            verb-slot2_morph2_name=nonpast
                            verb-slot2_morph2_orth=
                              verb-slot2_morph2_feat1_name=tense
                              verb-slot2_morph2_feat1_value=nonpast
                              verb-slot2_morph2_feat1_head=verb
                          verb-slot3_name=aspect
                          verb-slot3_order=after
                            verb-slot3_input1_type=verb-slot2
                            verb-slot3_morph1_name=prg
                            verb-slot3_morph1_orth=ing
                              verb-slot3_morph1_feat1_name=aspect
                              verb-slot3_morph1_feat1_value=progressive
                              verb-slot3_morph1_feat1_head=verb
                              '''.splitlines()

con_dis_junctive = '''  verb-slot1_name=a
  verb-slot1_order=after
    verb-slot1_input1_type=verb1
    verb-slot1_morph1_name=am
  verb-slot2_name=b
  verb-slot2_order=after
    verb-slot2_input1_type=verb1
    verb_slot2_morph1_name=bm
  verb-slot3_name=c
  verb-slot3_order=after
    verb-slot3_input1_type=verb-slot1
    verb-slot3_input2_type=verb-slot2
    verb_slot3_morph1_name=cm
  verb-slot4_name=d
  verb-slot4_order=after
    verb-slot4_input1_type=verb-slot3
    verb_slot4_morph1_name=dm
  verb-slot5_name=e
  verb-slot5_order=after
    verb-slot5_input1_type=verb-slot3
    verb_slot5_morph1_name=em'''.splitlines()

extended_con_dis_junctive1 = con_dis_junctive + \
                             '''  verb-slot6_name=f
                               verb-slot6_order=after
                                 verb-slot6_input1_type=verb-slot4
                                 verb_slot6_morph1_name=fm'''.splitlines()

extended_con_dis_junctive2 = extended_con_dis_junctive1 + \
                             '''  verb-slot7_name=g
                               verb-slot7_order=after
                                 verb-slot7_input1_type=verb-slot5
                                 verb_slot7_morph1_name=gm'''.splitlines()

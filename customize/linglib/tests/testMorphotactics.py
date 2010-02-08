import unittest
import customize.choices
from customize.linglib import morphotactics

class TestMorphotactics(unittest.TestCase):
  def test_find_basetypes_for_slot(self):
    # single basetype
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + simple_no_orth)
    slot = choices['noun-slot1']
    self.assertEqual(morphotactics.find_basetypes_for_slot(choices, slot),
                     set(['noun1']))

    # multiple basetypes
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + multiple_basetypes)
    slot = choices['verb-slot1']
    self.assertEqual(morphotactics.find_basetypes_for_slot(choices, slot),
                     set(['verb1','verb2']))

  def test_create_basetype_dict(self):
    # single slot, single basetype
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + simple_no_orth)
    self.assertEqual(morphotactics.create_basetype_dict(choices),
                     {'noun-slot1': set(['noun1'])})

    # multiple slots, single basetype
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + competing_slots)
    self.assertEqual(morphotactics.create_basetype_dict(choices),
                     {'noun-slot1': set(['noun1']),
                      'noun-slot2': set(['noun1'])})

    # single slot, multiple basetypes
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + multiple_basetypes)
    self.assertEqual(morphotactics.create_basetype_dict(choices),
                     {'verb-slot1': set(['verb1','verb2'])})

    # multiple slots, multiple basetypes
    choices = customize.choices.ChoicesFile()
    choices.load_choices(lextypes + simple_no_orth + multiple_basetypes)
    self.assertEqual(morphotactics.create_basetype_dict(choices),
                     {'verb-slot1': set(['verb1','verb2']),
                      'noun-slot1': set(['noun1'])})

    def test_create_restriction_dict(self):
      # no restrictions
      # non-opt to requires restriction
      # overt restrictions

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

two_morphemes = simple_no_orth +\
'''    noun-slot1_morph2_name=plural
    noun-slot1_morph2_orth=s
      noun-slot1_morph2_feat1_name=number
      noun-slot1_morph2_feat1_value=pl'''.splitlines()

competing_slots = simple_no_orth +\
'''  noun-slot2_name=num2
  noun-slot2_order=after
    noun-slot2_input1_type=noun1
    noun-slot2_morph1_name=plural
      noun-slot2_morph1_feat1_name=number
      noun-slot2_morph1_feat1_value=pl'''.splitlines()

multiple_basetypes = '''  verb-slot1_name=person
  verb-slot1_order=after
    verb-slot1_input1_type=verb1
    verb-slot1_input2_type=verb2
    verb-slot1_morph1_name=3sg
    verb-slot1_morph1_orth=s
      verb-slot1_morph1_feat1_name=person
      verb-slot1_morph1_feat1_value=3
      verb-slot1_morph1_feat2_name=number
      verb-slot1_morph1_feat2_value=sg'''.splitlines()

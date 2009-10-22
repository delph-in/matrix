import unittest
import customize.choices

class TestChoicesFileParsingFunctions(unittest.TestCase):
    def test_get(self):
        c = customize.choices.ChoicesFile() # no file loaded
        c.choices = c.parse_choices(simple_choices_file)
        self.assertEqual(c.get('NO_SUCH'), [])
        self.assertEqual(c.get('language'), 'Simple')
        self.assertEqual(c.get('iso-code'), 'smp')
        self.assertEqual(c.get('verb1_name'), 'testverb')
        self.assertEqual(c.get('verb2_name'), 'testverb2')
        self.assertEqual(c.get('verb1_stem1_orth'), 'test')
        self.assertEqual(c.get('verb1_stem1_pred'), 'test_v_1_rel')
        self.assertEqual(c.get('verb2_stem1_orth'), 'test')
        self.assertEqual(c.get('verb2_stem1_pred'), 'test_v_2_rel')

    def test_split_choice_attribute(self):
        c = customize.choices.ChoicesFile() # no file loaded
        self.assertEqual(c.split_choice_attribute(''), [])
        self.assertEqual(c.split_choice_attribute('abc'),
                         ['abc'])
        self.assertEqual(c.split_choice_attribute('abc_def'),
                         ['abc', 'def'])
        self.assertEqual(c.split_choice_attribute('abc_def1_ghi'),
                         ['abc', 'def', '1', 'ghi'])
        self.assertEqual(c.split_choice_attribute('abc_def1'),
                         ['abc', 'def1'])
        self.assertEqual(c.split_choice_attribute('abc32_def1_ghi'),
                         ['abc', '32', 'def', '1', 'ghi'])

    def test_parse_choice(self):
        c = customize.choices.ChoicesFile() # no file loaded
        self.assertEqual(c.parse_choice([], 'def'), 'def')
        self.assertEqual(c.parse_choice(['abc'], 'def'), {'abc': 'def'})
        self.assertEqual(c.parse_choice([1], 'def'), ['def'])
        self.assertEqual(c.parse_choice([2], 'def'), [None, 'def'])
        self.assertEqual(c.parse_choice(['abc', 2, 'def'], 'ghi'),
                         {'abc': [None, {'def': 'ghi'}]})

    def test_merge_choices(self):
        c = customize.choices.ChoicesFile() # no file loaded
        self.assertEqual(c.merge_choices({}, {'abc': 'def'}),
                         {'abc': 'def'})
        self.assertEqual(c.merge_choices({'abc': 'def'}, {'ghi': 'jkl'}),
                         {'abc': 'def', 'ghi': 'jkl'})
        self.assertEqual(c.merge_choices(['abc'], [None, 'def']),
                         ['abc', 'def'])
        self.assertEqual(c.merge_choices(['abc'], [None, None, 'def']),
                         ['abc', None, 'def'])
        self.assertEqual(c.merge_choices([None, 'abc'], ['def']),
                         ['def', 'abc'])
        self.assertEqual(c.merge_choices({'abc': [{'def': 'ghi'}, None]},
                                         {'abc': [None, {'jkl': 'mno'}]}),
                         {'abc': [{'def': 'ghi'}, {'jkl': 'mno'}]})
        self.assertRaises(customize.choices.ChoicesFileParseError,
                          c.merge_choices, {'abc': 'def'}, {'abc': 'ghi'})
        self.assertRaises(customize.choices.ChoicesFileParseError,
                          c.merge_choices, ['abc'], ['def'])

    def test_parse_choices(self):
        c = customize.choices.ChoicesFile() # no file loaded
        self.assertEqual(c.parse_choices([]), {})
        self.assertEqual(c.parse_choices(['abc=ABC', 'def=DEF']),
                         {'abc': 'ABC', 'def': 'DEF'})
        self.assertEqual(c.parse_choices(['abc1_def=DEF', 'abc1_ghi=GHI']),
                         {'abc': [{'def': 'DEF', 'ghi': 'GHI'}]})
        self.assertEqual(c.parse_choices(['abc1_def=DEF1', 'abc2_def=DEF2']),
                         {'abc': [{'def':'DEF1'}, {'def':'DEF2'}]})
        self.assertEqual(c.parse_choices(['abc2_def=DEF2', 'abc1_def=DEF1']),
                         {'abc': [{'def':'DEF1'}, {'def':'DEF2'}]})

class TestChoicesDerivedValueFunctions(unittest.TestCase):
    def setUp(self):
        self.c = customize.choices.ChoicesFile()

    def test_has_case(self):
        feat = {'name':'case', 'value':'abc'}
        self.assertTrue(self.c.has_case(feat, 'abc'))
        self.assertTrue(self.c.has_case(feat, ''))
        self.assertFalse(self.c.has_case(feat, 'def'))

    def test_has_noun_case(self):
        pass
    def test_has_adp_case(self):
        pass
    def test_has_optadp_case(self):
        pass
    def test_has_mixed_case(self):
        pass
    def test_case_head(self):
        pass
    def test_has_dirinv(self):
        pass
    def test_has_SCARGS(self):
        pass
    def test_cases(self):
        pass
    def test_patterns(self):
        pass
    def test_numbers(self):
        pass
    def test_persons(self):
        pass
    def test_pernums(self):
        pass
    def test_genders(self):
        pass
    def test_forms(self):
        pass
    def test_tenses(self):
        pass
    def test_aspects(self):
        pass
    def test_situations(self):
        pass
    def test_types(self):
        pass
    def test_features(self):
        pass

class TestExampleChoicesFiles(unittest.TestCase):
    def setUp(self):
#        self.ch = customize.choices.ChoicesFile()
        pass

##############################################################################
#### Choices File Strings

empty_choices_file = ''

minimal_choices_file = '''version=18

section=general
language=Minimal
archive=no'''

simple_choices_file = '''version=18

section=general
language=Simple
iso-code=smp
archive=no

section=word-order
word-order=free
has-dets=no
has-aux=no

section=number

section=person
person=none

section=gender

section=case
case-marking=none

section=direct-inverse

section=tense-aspect

section=other-features

section=sentential-negation

section=coordination

section=matrix-yes-no

section=arg-opt

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
    verb2_stem1_pred=test_v_2_rel

section=test-sentences'''

if __name__ == '__main__':
    unittest.main()

# -*- coding: utf-8 -*-
"""
Run these tests with `python -m tests.unit.validate_test` from the matrix directory.
"""

import unittest
from gmcs.choices import ChoicesFile, ChoiceList, ChoiceDict
from gmcs.validate import validate

class TestValidate(unittest.TestCase):

    def assertErrorsOrWarnings(self, c, variables, errors):
        """
        Methods for asserting the presence of errors and warnings when
        # a ChoicesFile is put through validation.
        """
        vr = validate(c)
        for v in variables:
            if errors:
                self.assertTrue(v in vr.errors)
            else:
                self.assertTrue(v in vr.warnings)

    def assertWarning(self, c, warning):
        self.assertErrorsOrWarnings(c, [warning], False)

    def assertWarnings(self, c, warnings):
        self.assertErrorsOrWarnings(c, warnings, False)

    def assertError(self, c, error):
        self.assertErrorsOrWarnings(c, [error], True)

    def assertErrors(self, c, errors):
        self.assertErrorsOrWarnings(c, errors, True)

    # Tests

    def test_names(self):
        # Pairs of [variable prefix, value suffix].  Appending 'name' to the
        # prefix gives the variable name (e.g., 'case1_name').  The suffix
        # tells what the system will append to the end of the *value* of
        # that variable.
        variables = [['nom-acc-nom-case-', ''],
                     ['case1_', ''],
                     ['number1_', ''],
                     ['gender1_', ''],
                     ['feature1_', ''],
                     ['feature1_value1_', ''],
                     ['tense1_', ''],
                     ['aspect1_', ''],
                     ['situation1_', ''],
                     ['form-subtype1_', ''],
                     ['noun1_', '-noun-lex'],
                     ['noun2_', '-noun-lex'],
                     ['det1_', '-determiner-lex'],
                     ['det2_', '-determiner-lex'],
                     ['verb1_', '-verb-lex'],
                     ['verb2_', '-verb-lex'],
                     ['noun-pc1_', '-lex-rule'],
                     ['noun-pc1_lrt1_', '-lex-rule'],
                     ['noun-pc2_', '-lex-rule'],
                     ['noun-pc2_lrt1_', '-lex-rule']]

        # Name for causing collisions, made up of the lower-case letter 'a' in:
        #   Latin, accented Latin, accented Latin, Greek, Cyrillic, Armenian
        value = 'aáāαаա'

        for v1 in variables:
            # first try colliding with an existing name (head-comp-phrase)
            if not v1[1]:  # won't collide if there's a suffix
                name = v1[0] + 'name'
                c = ChoicesFile()
                c[name] = 'head-comp-phrase'
                self.assertError(c, name)

            # next try colliding pairs of names
            for v2 in [x for x in variables if x[0] != v1[0]]:
                if v1[1] == v2[1]:  # won't collide with different suffixes
                    name1 = v1[0] + 'name'
                    name2 = v2[0] + 'name'

                    c = ChoicesFile()

                    # same case
                    c[name1] = value
                    c[name2] = value

                    # different case (should still collide)
                    c[name1] = value.lower()
                    c[name2] = value.upper()
                    self.assertErrors(c, [name1, name2])

    def test_general(self):
        c = ChoicesFile()

        # no language
        vr = validate(c)
        self.assertError(c, 'language')
        self.assertWarning(c, 'archive')

        # illegal characters in language
        for bad_ch in '.~':
            c['language'] = bad_ch + 'x'
            self.assertError(c, 'language')

            # ...but these characters are OK in non-initial position
            c['language'] = 'x' + bad_ch + 'x'
            vr = validate(c)
            self.assertTrue('language' not in vr.errors)

        for bad_ch in chr(20) + '?*:<>|/\\"^':
            c['language'] = bad_ch + 'x'
            self.assertError(c, 'language')

            c['language'] = 'x' + bad_ch + 'x'
            self.assertError(c, 'language')

    def test_case(self):
        c = ChoicesFile()

        # no answer for case
        self.assertError(c, 'case-marking')

        # for each case pattern, make sure case names are required
        for cm in ['nom-acc', 'split-n', 'split-v']:
            c['case-marking'] = cm
            self.assertErrors(
                c, [cm + '-nom-case-name', cm + '-acc-case-name'])

        for cm in ['erg-abs', 'split-n', 'split-v']:
            c['case-marking'] = cm
            self.assertErrors(
                c, [cm + '-erg-case-name', cm + '-abs-case-name'])

        for cm in ['tripartite', 'split-s', 'fluid-s', 'focus']:
            c['case-marking'] = cm
            self.assertErrors(c, [cm + '-a-case-name', cm + '-o-case-name'])

        for cm in ['tripartite']:
            c['case-marking'] = cm
            self.assertError(c, cm + '-s-case-name')

        for cm in ['focus']:
            c['case-marking'] = cm
            self.assertError(c, cm + '-focus-case-name')

        # if there's no case, additional cases can't be specified
        c['case-marking'] = 'none'
        c['case1_name'] = 'dummy'
        self.assertError(c, 'case1_name')

        # if a dir-inv scale is defined, scale-equal must be too
        c['scale1_feat1_name'] = 'dummy'
        self.assertError(c, 'scale-equal')

    def test_person(self):
        c = ChoicesFile()

        # no answer for person
        self.assertError(c, 'person')

        # no answer for first-person in a language with a first person
        for person in ['1-2-3', '1-2-3-4', '1-non-1']:
            c['person'] = person
            self.assertError(c, 'first-person')

        # an answer for first-person in a language with no first person
        c['first-person'] = 'incl-excl'
        for person in ['none', '2-non-2', '3-non-3']:
            c['person'] = person
            self.assertError(c, 'first-person')
        
        # first person distinction, but no number specified    
        self.assertError(c, 'incl-excl-number')
        
        # number selected for first person distinction, but no distinction
        c['first-person'] = ""
        c['incl-excl-number'] = 'sg'
        self.assertWarning(c, 'incl-excl-number')

    def test_number(self):
        c = ChoicesFile()

        # a number defined without a name
        c['number1_supertype1_name'] = 'dummy'
        self.assertError(c, 'number1_name')

    def test_gender(self):
        c = ChoicesFile()

        # a gender defined without a name
        c['gender1_supertype1_name'] = 'dummy'
        self.assertError(c, 'gender1_name')

    def test_other_features(self):
        # a feature without a name, a type, or a name for its value
        c = ChoicesFile()
        c['feature1_value1_supertype1_name'] = 'dummy'
        self.assertErrors(c, ['feature1_name', 'feature1_type',
                              'feature1_value1_name'])

        # a feature without a name, a type, or a supertype for its value
        c = ChoicesFile()
        c['feature1_value1_name'] = 'dummy'
        self.assertErrors(c, ['feature1_name', 'feature1_type',
                              'feature1_value1_supertype1_name'])

    def test_word_order(self):
        # Determiners
        # no answer for word order or determiners
        c = ChoicesFile()
        self.assertErrors(c, ['word-order', 'has-dets'])

        # determiners but no order
        c['has-dets'] = 'yes'
        self.assertError(c, 'noun-det-order')

        # order but no determiners
        c = ChoicesFile()
        c['noun-det-order'] = 'noun-det'
        self.assertError(c, 'has-dets')

        # determiners=no but determiner in the lexicon
        c['has-dets'] = 'no'
        c['det1_name'] = 'dummy'
        self.assertError(c, 'has-dets')

        # Auxiliaries
        # no answer
        c = ChoicesFile()
        self.assertError(c, 'has-aux')

        # auxiliaries but no order
        c['has-aux'] = 'yes'
        self.assertError(c, 'aux-comp-order')

        # order but no auxiliaries
        c = ChoicesFile()
        c['aux-comp-order'] = 'before'
        self.assertError(c, 'has-aux')

        # aux but no answer for complement
        c['has-aux'] = 'yes'
        self.assertError(c, 'aux-comp')

        # aux but no answer for multiple aux in free word order
        c['word-order'] = 'free'
        self.assertError(c, 'multiple-aux')

        # Word Order
        c = ChoicesFile()
        c['word-order'] = 'vso'
        c['aux-comp'] = 'vp'
        c['aux-comp-order'] = 'after'
        self.assertError(c, 'aux-comp')

        c = ChoicesFile()
        c['word-order'] = 'osv'
        c['aux-comp'] = 'vp'
        c['aux-comp-order'] = 'before'
        self.assertError(c, 'aux-comp')

    def test_sentential_negation(self):
        c = ChoicesFile()

        # negation via adverb and simple negation
        # but no answers for other questions
        c['adv-neg'] = 'on'
        c['neg-exp'] = '1'
        self.assertErrors(c, ['neg-mod', 'neg-order', 'neg-adv-orth'])

        # auxiliary selects neg complement,
        # but no auxiliaries ('has-aux' != 'yes')
        c['comp-neg'] = 'on'
        c['comp-neg-head'] = 'aux'
        self.assertError(c, 'comp-neg-head')
    
        # negation adverb has same spelling as one already defined 
        c['neg-adv-orth'] = 'test'
        c['adv1_stem1_orth'] = 'test'
        self.assertError(c, 'neg-adv-orth')

    def test_coordination(self):
        # missing answers
        c = ChoicesFile()
        c['cs1_dummy'] = 'dummy'  # dummy to force there to be a cs1
        self.assertErrors(c, ['cs1_n', 'cs1_np', 'cs1_vp', 'cs1_s'])
        self.assertErrors(c, ['cs1_pat', 'cs1_mark', 'cs1_order', 'cs1_orth'])

        # asyndeton but with mark, order, and orth specified
        c = ChoicesFile()
        c['cs1_pat'] = 'a'
        c['cs1_mark'] = 'dummy'
        c['cs1_order'] = 'dummy'
        c['cs1_orth'] = 'dummy'
        self.assertErrors(c, ['cs1_mark', 'cs1_order', 'cs1_orth'])

        # marked by affixes on NP, VP, or S (not supported yet)
        for phrase in ['np', 'vp', 's']:
            c = ChoicesFile()
            c['cs1_mark'] = 'affix'
            c['cs1_' + phrase] = 'on'
            self.assertError(c, 'cs1_mark')

    def test_yesno_questions(self):
        # missing answers about particles
        c = ChoicesFile()
        c['q-part'] = 'on'
        # missing order and no specified particles
        self.assertErrors(c, ['q-part-order', 'q-particle'])
        # missing orth of specified particle
        c['q-particle1_main'] = 'on' 
        self.assertErrors(c, ['q-particle'])

        # missing or incompatible answers about inversion
        c = ChoicesFile()
        c['q-inv'] = 'on'
        for wo in ['v-final', 'v-initial', 'free']:
            for qiv in ['aux', 'aux-main']:
                c['word-order'] = wo
                c['q-inv-verb'] = qiv
                self.assertErrors(c, ['q-inv', 'q-inv-verb'])

        # inflection specified, but not defined in the lexicon
        c = ChoicesFile()
        c['q-infl'] = 'on'
        self.assertError(c, 'q-infl')
    
    def test_wh_questions(self):
        # missing question words in lexicon when question strategy defined
        for wh_q_strat in ['front-matrix', 'wh-q-infl', 'wh-q-part']:
            if wh_q_strat == 'front-matrix':
                # choices file instantiated on each iteration because of elif logic
                c = ChoicesFile()
                c[wh_q_strat] = 'multi'
            else:
                c = ChoicesFile()
                c[wh_q_strat] = 'on'
            self.assertError(c, wh_q_strat)
            
        # pied piping but no determiners defined 
        for p in ['pied-pip', 'pied-pip-adp']:
            c = ChoicesFile()
            c[p] = 'on'
            self.assertError(c, p)
            
        # pied piping with determiner, but not adposition
        c = ChoicesFile()
        c['pied-pip-adp'] = 'on'
        c['det'] = ChoiceList()
        self.assertError(c, 'pied-pip-adp')
        
        # pied piping with in-situ fronting option
        for p in ['pied-pip', 'pied-pip-adp']:
            c = ChoicesFile()
            c[p] = 'on'
            c['front-matrix'] = 'in-situ'
            self.assertError(c, p)
            
        # pied piping obligatory but not selected
        c = ChoicesFile()
        c['oblig-pied-pip-noun'] = 'on'
        self.assertError(c, 'pied-pip')
        
        c = ChoicesFile()
        c['oblig-pied-pip-adp'] = 'on'
        self.assertError(c, 'pied-pip-adp')
        
        # Multi question fronting but multiple questions in one clause not allowed
        c = ChoicesFile()
        c['no-multi-ques'] = 'on'
        c['front-matrix'] = 'multi'
        self.assertError(c, 'no-multi-ques')
        c = ChoicesFile()
        c['no-multi-ques'] = 'on'
        c['matrix-front-opt'] = 'all-oblig'
        self.assertError(c, 'no-multi-ques')
        
        # In-situ and obligatory fronting not compatible
        for opt in ['all-oblig', 'single-oblig']:
            c = ChoicesFile()
            c['matrix-front-opt'] = opt 
            c['front-matrix'] = 'in-situ'
            self.assertError(c, 'matrix-front-opt')
        
        # Embed in-situ only valid if fronting is optional
        c = ChoicesFile()
        c['matrix-front-opt'] = 'all-oblig'
        c['embed-insitu'] = 'on'
        self.assertError(c, 'embed-insitu')
        
        # Inversion not selected on Y/N page
        c = ChoicesFile()
        c['wh-inv-matrix'] = 'on'
        self.assertError(c, 'wh-inv-matrix')
        
        # Inversion in matrix clauses not selected, but desired in embedded
        c = ChoicesFile()
        c['wh-inv-embed'] = 'on'
        self.assertError(c, 'wh-inv-embed')
        
        # Inversion in matrix clauses not selected
        c = ChoicesFile()
        c['wh-inv-notsubj'] = 'on'
        self.assertError(c, 'wh-inv-notsubj')
        
        # No contrastive focus marker specified
        c = ChoicesFile()
        c['focus-marking'] = 'on'
        self.assertError(c, 'focus-marking')
        
        # No qverbs defined
        c = ChoicesFile()
        c['wh-q-inter-verbs'] = 'on'
        self.assertError(c, 'wh-q-inter-verbs')
        
        # Qverbs defined but not select on WH page 
        c = ChoicesFile()
        cl = ChoiceList()
        cl += [ChoiceDict(full_key='qverb')]
        c['qverb'] = cl
        self.assertError(c, 'wh-q-inter-verbs')
        
        # Verbal inflection not selected on Y/N page
        c = ChoicesFile()
        c['wh-q-infl'] = 'on'
        self.assertError(c, 'wh-q-infl')
        
    def test_tanda(self):
        tenses = ['past', 'present', 'future', 'nonpast', 'nonfuture']
        # Tense
        # subtypes defined for incorrect supertype
        for st in tenses:
            for t in [x for x in tenses if x != st]:
                c = ChoicesFile()
                c[st] = 'on'
                c[t + '-subtype1_name'] = 'dummy'
                self.assertError(c, t + '-subtype1_name')

        # added a hierarchy element but didn't answer yes to tense-definition
        c = ChoicesFile()
        c['past'] = 'on'
        self.assertWarning(c, 'tense-definition')

        # answered yes to tense-definition but then didn't define
        c = ChoicesFile()
        c['tense-definition'] = 'choose'
        self.assertErrors(c, tenses)

        # build-your-own hierarchy
        c = ChoicesFile()
        c['tense-definition'] = 'build'
        self.assertError(c, 'tense-definition')

        c['tense1_dummy'] = 'dummy'
        self.assertErrors(c, ['tense1_name', 'tense1_supertype1_name'])

        # Aspect
        c = ChoicesFile()
        c['aspect1_dummy'] = 'dummy'
        self.assertErrors(c, ['aspect1_name', 'aspect1_supertype1_name'])

        # Situation
        c = ChoicesFile()
        c['situation1_dummy'] = 'dummy'
        self.assertErrors(c, ['situation1_name', 'situation1_supertype1_name'])

        # Form
        c = ChoicesFile()
        c['has-aux'] = 'yes'
        c['form-fin-nf'] = ''
        self.assertError(c, 'form-fin-nf')

        c['has-aux'] = 'no'
        c['form-fin-nf'] = 'off'
        c['form-subtype1_name'] = 'finite'
        self.assertError(c, 'form-subtype')

    def test_lexicon(self):
        # not enough nouns and verbs
        c = ChoicesFile()
        self.assertWarnings(c, ['noun1_stem1_orth',
                                'verb1_valence',
                                'verb2_valence'])

        # Nouns
        # noun with no answer about determiners, and a stem with no orth or pred
        c['noun1_dummy'] = 'dummy'
        c['noun1_stem1_dummy'] = 'dummy'
        self.assertErrors(c, ['noun1_det',
                              'noun1_stem1_orth',
                              'noun1_stem1_pred'])

        # determiners obligatory, but not determiners
        c['noun1_det'] = 'obl'
        c['has-dets'] = 'no'
        self.assertErrors(c, ['has-dets', 'noun1_det'])

        # question pronoun, but no question constituent question selections
        c['noun1_inter'] = 'on'
        self.assertWarnings(c, ['noun1_inter'])
        
        # Verbs
        c = ChoicesFile()
        c['verb1_dummy'] = 'dummy'
        c['verb1_stem1_dummy'] = 'dummy'
        self.assertErrors(c, ['verb1_valence',
                              'verb1_stem1_orth',
                              'verb1_stem1_pred'])

        # Auxiliaries
        c = ChoicesFile()
        c['has-aux'] = 'yes'
        self.assertError(c, 'auxlabel')

        c['has-aux'] = 'no'
        c['aux-comp'] = 'vp'
        c['aux1_compfeature1_dummy'] = 'dummy'
        self.assertErrors(c, ['has-aux',
                              'aux1_sem',
                              'aux1_subj',
                              'aux1_complabel',
                              'aux1_stem1_orth'])

        c['aux1_sem'] = 'add-pred'
        c['aux1_stem1_dummy'] = 'dummy'
        self.assertError(c, 'aux1_stem1_pred')

        c['aux1_sem'] = ''
        c['aux1_stem1_pred'] = 'dummy'
        self.assertError(c, 'aux1_sem')

        # Adpositions
        c = ChoicesFile()
        c['adp1_feat1_name'] = 'poss-strat'
        self.assertError(c, 'adp1_feat1_name')
        
        # Adverbs 
        c = ChoicesFile()
        c['adv1_stem1_pred'] = '_pred_a_rel'
        c['adv1_inter'] = 'on'
        self.assertWarning(c, 'adv1_stem1_pred')

        c = ChoicesFile()
        c['neg-adv-orth'] = 'test'
        c['adv1_stem1_orth'] = 'test'
        self.assertError(c, 'adv1_stem1_orth')
        
        # Determiners
        c = ChoicesFile()
        c['det1_stem1_pred'] = 'x_q_rel'
        self.assertError(c, 'det1_stem1_orth')
        
        c = ChoicesFile()
        c['det1_stem1_orth'] = 'test'
        self.assertError(c, 'det1_stem1_pred')
        
        c = ChoicesFile()
        c['det1_stem1_pred'] = 'test'
        self.assertWarning(c, 'det1_stem1_pred')

        # Features
        for lt in ['noun', 'verb', 'aux', 'det', 'adp']:
            c = ChoicesFile()
            c[lt + '1_feat1_dummy'] = 'dummy'
            self.assertErrors(c, [lt + '1_feat1_name',
                                  lt + '1_feat1_value'])

            c[lt + '1_feat1_name'] = 'argument structure'
            self.assertError(c, lt + '1_feat1_name')

            if lt == 'verb':
                self.assertError(c, lt + '1_feat1_head')

            for head in ['higher', 'lower']:
                c[lt + '1_feat1_head'] = head
                self.assertError(c, lt + '1_feat1_head')

                # perhaps this should be refactored for morphotactics
                # Inflectional Slots
                # for pcprefix in ['noun', 'verb', 'det']:
                #  c = ChoicesFile()
                #  c[pcprefix + '-pc1_dummy'] = 'dummy'
                #  self.assertErrors(c, [pcprefix + '-pc1_order',
                #                        pcprefix + '-pc1_input1_type'])

                #  c[pcprefix + '-pc1_input1_type'] = 'dummy'
                #  self.assertError(c, pcprefix + '-pc1_input1_type')

                #  c[pcprefix + '-pc1_lrt1_feat1_dummy'] = 'dummy'
                #  self.assertErrors(c, [pcprefix + '-pc1_lrt1_feat1_name',
                #                        pcprefix + '-pc1_lrt1_feat1_value'])
                #  if pcprefix == 'verb':
                #    self.assertError(c, pcprefix + '-pc1_lrt1_feat1_head')             

    def test_features(self):
        # try a bad feature and value everywhere
        for p in ['scale',
                  'noun', 'verb', 'det', 'aux',
                  'noun-pc1_lrt', 'verb-pc1_lrt', 'det-pc1_lrt',
                  'context']:
            c = ChoicesFile()
            c[p + '1_feat1_name'] = 'dummy'
            c[p + '1_feat1_value'] = 'dummy'
            self.assertErrors(c, [p + '1_feat1_name', p + '1_feat1_value'])
            
        # test features whose categories do not match lrt feature head
        for lt in ['verb-pc1_lrt' , 'adj-pc1_lrt']: 
            c = ChoicesFile()
            feature_name = 'feat1'
            for head in ['subj', 'obj', 'noun']:
                c[lt + '1_feat1_name'] = feature_name
                c['feature1_name'] = feature_name
                c['feature1_cat'] = 'verb'
                c[lt + '1_feat1_head'] = head
                self.assertError(c, lt + '1_feat1_head') 
            for head in ['verb']:
                c[lt + '1_feat1_name'] = feature_name
                c['feature1_name'] = feature_name
                c['feature1_cat'] = 'noun'
                c[lt + '1_feat1_head'] = head
                self.assertError(c, lt + '1_feat1_head')   

    def test_argopt(self):
        c = ChoicesFile()
        c['subj-drop'] = 'on'
        c['obj-drop'] = 'on'
        c['context1_feat1_dummy'] = 'dummy'
        self.assertErrors(c, ['subj-mark-drop', 'subj-mark-no-drop',
                              'obj-mark-drop', 'obj-mark-no-drop',
                              'context1_feat1_head'])

if __name__ == '__main__':
    unittest.main()
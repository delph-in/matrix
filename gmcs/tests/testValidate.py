# -*- coding: utf-8 -*-

import unittest
from gmcs.choices import ChoicesFile
from gmcs.validate import validate

class TestValidate(unittest.TestCase):

    ## Methods for asserting the presence of errors and warnings when
    ## a ChoicesFile is put through validation

    def assertErrorsOrWarnings(self, c, variables, errors):
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


    ## Tests

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
                     ['nf-subform1_', ''],
                     ['fin-subform1_', ''],
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
        value = u'aáāαаա'

        for v1 in variables:
            # first try colliding with an existing name (head-comp-phrase)
            if not v1[1]:  # won't collide if there's a suffix
                name = v1[0] + 'name'
                c = ChoicesFile()
                c[name] = 'head-comp-phrase'
                self.assertError(c, name)

            # next try colliding pairs of names
            for v2 in [x for x in variables if x[0] != v1[0]]:
                if v1[1] == v2[1]: # won't collide with different suffixes
                    name1 = v1[0] + 'name'
                    name2 = v2[0] + 'name'

                    c = ChoicesFile()

                    # same case
                    c[name1] = value.encode('utf-8')
                    c[name2] = value.encode('utf-8')

                    # different case (should still collide)
                    c[name1] = value.lower().encode('utf-8')
                    c[name2] = value.upper().encode('utf-8')
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
            self.assertErrors(c, [cm + '-nom-case-name', cm + '-acc-case-name'])

        for cm in ['erg-abs', 'split-n', 'split-v']:
            c['case-marking'] = cm
            self.assertErrors(c, [cm + '-erg-case-name', cm + '-abs-case-name'])

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
        ## Determiners
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

        ## Auxiliaries
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

        ## Word Order
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
        self.assertErrors(c, ['q-part-order', 'q-part-orth'])

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


    def test_tanda(self):
        tenses = ['past', 'present', 'future', 'nonpast', 'nonfuture']

        ## Tense
        # subtypes defined for incorrect supertype
        for st in tenses:
            for t in [x for x in tenses if x != st]:
                c = ChoicesFile()
                c[st] = 'on'
                c[t + '-subtype1_name'] = 'dummy'
                self.assertError(c, t + '-subtype1_name')

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

        ## Aspect
        c = ChoicesFile()
        c['aspect1_dummy'] = 'dummy'
        self.assertErrors(c, ['aspect1_name', 'aspect1_supertype1_name'])

        ## Situation
        c = ChoicesFile()
        c['situation1_dummy'] = 'dummy'
        self.assertErrors(c, ['situation1_name', 'situation1_supertype1_name'])

        ## Form
        c = ChoicesFile()
        c['has-aux'] = 'yes'
        c['noaux-fin-nf'] = 'on'
        self.assertError(c, 'noaux-fin-nf')

        c['has-aux'] = 'no'
        c['noaux-fin-nf'] = ''
        c['nf-subform1_dummy'] = 'dummy'
        self.assertError(c, 'noaux-fin-nf')


    def test_lexicon(self):
        # not enough nouns and verbs
        c = ChoicesFile()
        self.assertWarnings(c, ['noun1_stem1_orth',
                                'verb1_valence',
                                'verb2_valence'])

        ## Nouns
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

        ## Verbs
        c = ChoicesFile()
        c['verb1_dummy'] = 'dummy'
        c['verb1_stem1_dummy'] = 'dummy'
        self.assertErrors(c, ['verb1_valence',
                              'verb1_stem1_orth',
                              'verb1_stem1_pred'])

        ## Auxiliaries
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

        ## Adpositions
        c = ChoicesFile()
        c['adp1_dummy'] = 'dummy'
        self.assertWarning(c, 'adp1_feat1_name')

        ## Adpositions
        c = ChoicesFile()
        c['adp1_dummy'] = 'dummy'
        self.assertWarning(c, 'adp1_feat1_name')

        ## Features
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
                ### Inflectional Slots
                #for pcprefix in ['noun', 'verb', 'det']:
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


    def test_argopt(self):
        c = ChoicesFile()
        c['subj-drop'] = 'on'
        c['obj-drop'] = 'on'
        c['context1_feat1_dummy'] = 'dummy'
        self.assertErrors(c, ['subj-mark-drop', 'subj-mark-no-drop',
                              'obj-mark-drop', 'obj-mark-no-drop',
                              'context1_feat1_head'])

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
#from selenium.webdriver.support.wait import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
import unittest, time, re, os

class AllPagesEmptyChoices(unittest.TestCase):
    '''Check that all the pages load with an empty choices file'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_all_pages_empty_choices(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "General Information").click()
        try: self.assertEqual("General Information", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Word Order").click()
        try: self.assertEqual("Word Order", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Number").click()
        try: self.assertEqual("Number", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Person").click()
        try: self.assertEqual("Person", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Gender").click()
        try: self.assertEqual("Gender", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Case").click()
        try: self.assertEqual("Case", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Direct-inverse").click()
        try: self.assertEqual("Direct-inverse", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Tense, Aspect and Mood").click()
        try: self.assertEqual("Tense, Aspect and Mood", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Other Features").click()
        try: self.assertEqual("Other Features", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Sentential Negation").click()
        try: self.assertEqual("Sentential Negation", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Coordination").click()
        try: self.assertEqual("Coordination", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Matrix Yes/No Questions").click()
        try: self.assertEqual("Matrix Yes/No Questions", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Argument Optionality").click()
        try: self.assertEqual("Argument Optionality", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Lexicon").click()
        try: self.assertEqual("Lexicon", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Morphology").click()
        try: self.assertEqual("Morphology", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Import Toolbox Lexicon").click()
        try: self.assertEqual("Import Toolbox Lexicon", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Test Sentences").click()
        try: self.assertEqual("Test Sentences", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Test by Generation Options").click()
        try: self.assertEqual("Test by Generation Options", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class AllPagesJamamadiChoices(unittest.TestCase):
    '''Check that all the pages load with a valid choices file'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_all_pages_jamamadi_choices(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.NAME, "choices").send_keys(os.path.abspath("./gmcs/web_tests/web_choices/Jamamadi_choices.txt"))
        driver.find_element(By.XPATH, "//form[2]/p/input").click()
        driver.find_element(By.LINK_TEXT, "General Information").click()
        try: self.assertEqual("General Information", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Number").click()
        try: self.assertEqual("Number", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Word Order").click()
        try: self.assertEqual("Word Order", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Person").click()
        try: self.assertEqual("Person", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Gender").click()
        try: self.assertEqual("Gender", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Case").click()
        try: self.assertEqual("Case", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Direct-inverse").click()
        try: self.assertEqual("Direct-inverse", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Tense, Aspect and Mood").click()
        try: self.assertEqual("Tense, Aspect and Mood", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Other Features").click()
        try: self.assertEqual("Other Features", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Sentential Negation").click()
        try: self.assertEqual("Sentential Negation", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Coordination").click()
        try: self.assertEqual("Coordination", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Matrix Yes/No Questions").click()
        try: self.assertEqual("Matrix Yes/No Questions", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Argument Optionality").click()
        try: self.assertEqual("Argument Optionality", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Lexicon").click()
        try: self.assertEqual("Lexicon", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Morphology").click()
        try: self.assertEqual("Morphology", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Import Toolbox Lexicon").click()
        try: self.assertEqual("Import Toolbox Lexicon", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Test Sentences").click()
        try: self.assertEqual("Test Sentences", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element(By.LINK_TEXT, "Test by Generation Options").click()
        try: self.assertEqual("Test by Generation Options", driver.find_element(By.CSS_SELECTOR, "h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class GeneralErrors(unittest.TestCase):
    '''Test Errors and Warnings for the General Information page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_general_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "General Information").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"language_error\"] > span.error[title=\"You must specify the name of your language\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"archive_warning\"] > span.error[title=\"Please answer whether you will allow your answers to be retained.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.NAME, "language").clear()
        driver.find_element(By.NAME, "language").send_keys("Invalid_name?")
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "span.error"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.NAME, "language").clear()
        driver.find_element(By.NAME, "language").send_keys("Valid_name")
        driver.find_element(By.XPATH, "//input[@name='archive' and @value='no']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class WordOrderErrors(unittest.TestCase):
    '''Check that errors and warnings appear correctly on the Word Order page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_word_order_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Word Order").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"word-order_error\"] > span.error[title=\"You must specify a choice for the basic word order.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-dets_error\"] > span.error[title=\"You must specify whether your language has determiners.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-aux_error\"] > span.error[title=\"You must specify whether your language has auxiliary verbs.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name='word-order'][value='svo']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='has-dets'][value='yes']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='has-aux'][value='yes']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"noun-det-order_error\"] > span.error[title^='If your language has determiners']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp-order_error\"] > span.error[title^='If your language has auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp_error\"] > span.error[title^='If your language has auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.LINK_TEXT, "Clear current subpage").click();
        driver.find_element(By.CSS_SELECTOR, "input[name='noun-det-order'][value='noun-det']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='aux-comp-order'][value='before']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-dets_error\"] > span.error[title*='You specified an order of nouns and dets']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-aux_error\"] > span.error[title*='You specified an order for auxiliaries and their complements']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.LINK_TEXT, "Clear current subpage").click();
        driver.find_element(By.CSS_SELECTOR, "input[name='word-order'][value='free']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='has-aux'][value='yes']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"multiple-aux_error\"] > span.error[title^='If your language has free word order and auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name='word-order'][value='vso']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='aux-comp-order'][value='after']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='aux-comp'][value='vp']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp_error\"] > span.error[title^='The general word order and aux-comp order are not compatible with vp complements.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class NumberErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Numbers page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_number_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Number").click()
        driver.find_element(By.CSS_SELECTOR, "input[name=''][value='Add a Number']").click()
#       The driver isn't recognizing the first click of the "Add supertype Button"
#        Hopefully this will eventually be corrected...
        driver.find_element(By.XPATH, "//div[2]/div/p/input").click()
        driver.find_element(By.XPATH, "//div[2]/div/p/input").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='number1_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='number1_supertype1_name'] > option[value='number1']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"number1_name_error\"] > span.error[title=\"You must specify a name for each number you define.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class PersonErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Person page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_person_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Person").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"person_error\"] > span.error[title=\"You must specify how many persons your language distinguishes.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name=person][value='1-2-3']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"first-person_error\"] > span.error[title^=\"If your language has the first person\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name=person][value='2-non-2']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='first-person'][value='incl-excl']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"first-person_error\"] > span.error[title^=\"If your language does not have the first person\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class CaseErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Case page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_case_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Case").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"case-marking_error\"] > span.error[title=\"You must specify if/how case is marked.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name='case-marking'][value='nom-acc']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"nom-acc-nom-case-name_error\"] > span.error[title=\"You must specify a name for every case.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"nom-acc-acc-case-name_error\"] > span.error[title=\"You must specify a name for every case.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name='case-marking'][value='none']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a Case']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='case1_name']").send_keys("Pointless-Case")
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"case1_name_error\"] > span.error[title=\"You may not specify additional cases if your language has no case marking.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class DirectInverseErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Direct-Inverse page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_direct_inverse_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Direct-inverse").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a Scale Entry']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='scale1_feat1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='scale1_feat1_name'] > option[value='information-structure meaning']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"scale1_feat1_value_error\"] > span.error[title=\"You have selected an invalid feature value.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "select[name='scale-equal'] > option[value='']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"scale-equal_error\"] > span.error[title^=\"If you define a direct-inverse scale\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class TenseAspectMoodErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Tense/Aspect page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://matrix.ling.washington.edu/test2/matrix.cgi"
        self.verificationErrors = []

    def test_tense_aspect_mood_errors(self):
        driver = self.driver
        driver.get("http://matrix.ling.washington.edu/test2/matrix.cgi")
        driver.find_element(By.LINK_TEXT, "Tense, Aspect and Mood").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='tense-definition'][value='choose']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a subtype']").click();
        driver.find_element(By.CSS_SELECTOR, "input[name='past-subtype1_name']").send_keys("pointless_tense")
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"past_error\"] > span.error[title=\"You have chosen to select among hierarchy elements. You need to select at least one tense element.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"past-subtype1_name_error\"] > span.error[title=\"You cannot add a subtype if the supertype is not selected.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[name='tense-definition'][value='build']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"tense-definition_error\"] > span.error[title=\"You have chosen to build your own tense hierarchy so you must enter at least one tense subtype.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a tense type']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='tense1_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='tense1_supertype1_name'] > option[value='tense']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a tense type']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='tense2_name']").send_keys("Pointless-Tense")
        driver.find_element(By.CSS_SELECTOR, "select[name='tense2_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='tense2_supertype1_name'] > option[value='Pointless-Tense']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add an aspect type']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='aspect1_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='aspect1_supertype1_name'] > option[value='aspect']").click() #THIS IS NOT WORKING
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add an aspect type']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='aspect2_name']").send_keys("Pointless-Aspect")
        driver.find_element(By.CSS_SELECTOR, "select[name='aspect2_supertype1_name'] > option[value='']").click() # new
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a situation type']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='situation1_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='situation1_supertype1_name'] > option[value='situation']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a situation type']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='situation2_name']").send_keys("Pointless-Situation")
        driver.find_element(By.CSS_SELECTOR, "select[name='situation2_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a mood type']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='mood1_supertype1_name'] > option[value='']").click()
        driver.find_element(By.CSS_SELECTOR, "select[name='mood1_supertype1_name'] > option[value='mood']").click()
        driver.find_element(By.CSS_SELECTOR, "input[type='button'][value='Add a mood type']").click()
        driver.find_element(By.CSS_SELECTOR, "input[name='mood2_name']").send_keys("Pointless-Mood")
        driver.find_element(By.CSS_SELECTOR, "select[name='mood2_supertype1_name'] > option[value='']").click()
        driver.find_element(By.LINK_TEXT, "Save & stay").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='tense1_name_error'] > span.error[title='You must specify a name for each tense subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='tense2_supertype1_name_error'] > span.error[title='You must specify a supertype for each tense subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='aspect1_name_error'] > span.error[title='You must specify a name for each viewpoint aspect subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='aspect2_supertype1_name_error'] > span.error[title='You must specify at least one supertype for each viewpoint aspect subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='situation1_name_error'] > span.error[title='You must specify a name for each situation aspect subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='situation2_supertype1_name_error'] > span.error[title='You must specify at least one supertype for each situation aspect subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='mood1_name_error'] > span.error[title='You must specify a name for each mood subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name='mood2_supertype1_name_error'] > span.error[title='You must specify at least one supertype for each mood subtype you define.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True

    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)


if __name__ == "__main__":
    unittest.main()

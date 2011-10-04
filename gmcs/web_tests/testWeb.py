from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
import unittest, time, re, os

class AllPagesEmptyChoices(unittest.TestCase):
    '''Check that all the pages load with an empty choices file'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_all_pages_empty_choices(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("General Information").click()
        try: self.assertEqual("General Information", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Number").click()
        self.assertEqual("Number", driver.find_element_by_css_selector("h2").text)
        driver.back()
        driver.find_element_by_link_text("Word Order").click()
        try: self.assertEqual("Word Order", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Person").click()
        try: self.assertEqual("Person", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Gender").click()
        try: self.assertEqual("Gender", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Case").click()
        try: self.assertEqual("Case", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Direct-inverse").click()
        try: self.assertEqual("Direct-inverse", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Tense, Aspect and Mood").click()
        try: self.assertEqual("Tense, Aspect and Mood", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Other Features").click()
        try: self.assertEqual("Other Features", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Sentential Negation").click()
        try: self.assertEqual("Sentential Negation", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Coordination").click()
        try: self.assertEqual("Coordination", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Matrix Yes/No Questions").click()
        try: self.assertEqual("Matrix Yes/No Questions", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Argument Optionality").click()
        try: self.assertEqual("Argument Optionality", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Lexicon").click()
        try: self.assertEqual("Lexicon", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Test Sentences").click()
        try: self.assertEqual("Test Sentences", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Test by Generation Options").click()
        try: self.assertEqual("Test by Generation Options", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class AllPagesJamamadiChoices(unittest.TestCase):
    '''Check thatt all the pages load with a valid choices file'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_all_pages_jamamadi_choices(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_name("choices").clear()
        driver.find_element_by_name("choices").send_keys(os.path.abspath("./gmcs/web_tests/web_choices/Jamamadi_choices.txt"))
        driver.find_element_by_css_selector("form[name=\"choices_form\"] > p > input[type=\"submit\"]").click()
        driver.find_element_by_link_text("General Information").click()
        try: self.assertEqual("General Information", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Word Order").click()
        try: self.assertEqual("Word Order", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Number").click()
        try: self.assertEqual("Number", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Person").click()
        try: self.assertEqual("Person", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Gender").click()
        try: self.assertEqual("Gender", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Case").click()
        try: self.assertEqual("Case", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Direct-inverse").click()
        try: self.assertEqual("Direct-inverse", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Tense, Aspect and Mood").click()
        try: self.assertEqual("Tense, Aspect and Mood", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Other Features").click()
        try: self.assertEqual("Other Features", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Sentential Negation").click()
        try: self.assertEqual("Sentential Negation", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Coordination").click()
        try: self.assertEqual("Coordination", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Matrix Yes/No Questions").click()
        try: self.assertEqual("Matrix Yes/No Questions", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Argument Optionality").click()
        try: self.assertEqual("Argument Optionality", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Lexicon").click()
        try: self.assertEqual("Lexicon", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Test Sentences").click()
        try: self.assertEqual("Test Sentences", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.back()
        driver.find_element_by_link_text("Test by Generation Options").click()
        try: self.assertEqual("Test by Generation Options", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class GeneralErrors(unittest.TestCase):
    '''Test Errors and Warnings for the General Information page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_general_errors(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("General Information").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"language_error\"] > span.error[title=\"You must specify the name of your language\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"archive_warning\"] > span.error[title=\"Please answer whether you will allow your answers to be retained.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_name("language").clear()
        driver.find_element_by_name("language").send_keys("Invalid_name?")
        driver.find_element_by_css_selector("input[type=\"button\"]").click()
        driver.find_element_by_link_text("General Information").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "span.error"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_name("language").clear()
        driver.find_element_by_name("language").send_keys("Valid_name")
        driver.find_element_by_xpath("//input[@name='archive' and @value='no']").click()
        driver.find_element_by_css_selector("input[type=\"submit\"]").click()
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class WordOrderErrors(unittest.TestCase):
    '''Check that errors and warnings appear correctly on the Word Order page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_word_order_errors(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("Word Order").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"word-order_error\"] > span.error[title=\"You must specify a choice for the basic word order.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-dets_error\"] > span.error[title=\"You must specify whether your language has determiners.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-aux_error\"] > span.error[title=\"You must specify whether your language has auxiliary verbs.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[name='word-order'][value='svo']").click()
        driver.find_element_by_css_selector("input[name='has-dets'][value='yes']").click()
        driver.find_element_by_css_selector("input[name='has-aux'][value='yes']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"noun-det-order_error\"] > span.error[title^='If your language has determiners']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp-order_error\"] > span.error[title^='If your language has auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp_error\"] > span.error[title^='If your language has auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[value='Clear']").click()
        driver.find_element_by_css_selector("input[name='noun-det-order'][value='noun-det']").click()
        driver.find_element_by_css_selector("input[name='aux-comp-order'][value='before']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-dets_error\"] > span.error[title*='You specified an order of nouns and dets']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"has-aux_error\"] > span.error[title*='You specified an order for auxiliaries and their complements']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[value='Clear']").click()
        driver.find_element_by_css_selector("input[name='word-order'][value='free']").click()
        driver.find_element_by_css_selector("input[name='has-aux'][value='yes']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"multiple-aux_error\"] > span.error[title^='If your language has free word order and auxiliaries']"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[name='word-order'][value='vso']").click()
        driver.find_element_by_css_selector("input[name='aux-comp-order'][value='after']").click()
        driver.find_element_by_css_selector("input[name='aux-comp'][value='vp']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"aux-comp_error\"] > span.error[title^='The general word order and aux-comp order are not compatible with vp complements.']"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class NumberErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Numbers page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_number_errors(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("Number").click()
        driver.find_element_by_css_selector("input[name=''][value='Add a Number']").click()
        driver.find_element_by_css_selector("#number1 > div.iterframe > p > input[type=\"button\"]").click()
        driver.find_element_by_css_selector("select[name='number1_supertype1_name']").click()
        driver.find_element_by_css_selector("select[name='number1_supertype1_name'] > option[value='number1']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"number1_name_error\"] > span.error[title=\"You must specify a name for each number you define.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class PersonErrors(unittest.TestCase):
    '''Test Errors and Warnings for the Person page.'''
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_person_errors(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("Person").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"person_error\"] > span.error[title=\"You must specify how many persons your language distinguishes.\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[name=person][value='1-2-3']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"first-person_error\"] > span.error[title^=\"If your language has the first person\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        driver.find_element_by_css_selector("input[name=person][value='2-non-2']").click()
        driver.find_element_by_css_selector("input[name='first-person'][value='incl-excl']").click()
        driver.find_element_by_css_selector("input[type='submit']").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "a[name=\"first-person_error\"] > span.error[title^=\"If your language does not have the first person\"]"))
        except AssertionError as e: self.verificationErrors.append(str(e))

    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

if __name__ == "__main__":
    unittest.main()

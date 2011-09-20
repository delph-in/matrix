from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
import unittest, time, re

class Test1(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_1(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("Lexicon").click()
        try: self.assertTrue(self.is_element_present(By.CSS_SELECTOR, "div.iterframe > a[name=\"noun1_stem1_orth_warning\"] > span.error"))
        except AssertionError as e: self.verificationErrors.append(str(e))
        try: self.assertEqual("Lexicon", driver.find_element_by_css_selector("h2").text)
        except AssertionError as e: self.verificationErrors.append(str(e))
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException, e: return False
        return True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

class Test2(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://uakari.ling.washington.edu/matrix/test/matrix.cgi"
        self.verificationErrors = []
    
    def test_2(self):
        driver = self.driver
        driver.get("http://uakari.ling.washington.edu/matrix/test/matrix.cgi")
        driver.find_element_by_link_text("General Information").click()
        try: self.assertEqual("General Information", driver.find_element_by_css_selector("h2").text)
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

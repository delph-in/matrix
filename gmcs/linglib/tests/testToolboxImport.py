# -*- coding: utf-8 -*-
import unittest
import tempfile
from gmcs.choices import ChoicesFile
from gmcs.choices import FormData
from gmcs.choices import FormInfo
from gmcs.linglib import toolboximport
from gmcs.deffile import MatrixDefFile

class TestToolboxImport(unittest.TestCase):
  def setUp(self):
    try:
      for toolbox in self.toolbox_files:
        toolbox.close()
    except AttributeError:
      pass
    self.toolbox_files = [];
    self.c = ChoicesFile()
    self.gold = ChoicesFile()
    self.matrixdef = MatrixDefFile('gmcs/matrixdef')

  def create_temp(self, string):
    '''This creates a temporary file with the contents of string
    and returns a file handle of the temp file.'''
    cfout = tempfile.NamedTemporaryFile(dir="./", delete=False)
    cfout.write(string);
    return cfout;

  def save_choices(self, choices):
    '''This writes a choices file using matrix def.'''
    tmp = tempfile.NamedTemporaryFile(dir='./', delete=False)
    tmp.write(str(choices))
    formdata = FormData()
    formdata["section"] = FormInfo("section", "None")
    self.matrixdef.save_choices(formdata, tmp.name)
    return tmp

  def assert_choices_equal(self, choices1, choices2):
    file1 = self.save_choices(choices1)
    file2 = self.save_choices(choices2)
    file1.seek(0)
    file2.seek(0)
    file_data1 = file1.readlines()
    file_data2 = file2.readlines()
#    print file_data1;
#    print file_data2;
    file1.close()
    file2.close()
    self.assertEqual(file_data1, file_data2)

  def add_toolbox_files(self, choices, tb_strings):
    for i in range(len(tb_strings)):
      for j in range(len(tb_strings[i])):
        fout = tempfile.NamedTemporaryFile()
        fout.write(tb_strings[i][j])
        self.toolbox_files.append(fout);
        choices["toolboximportconfig"+str(i+1)+"_toolboxfile"+str(j+1)+"_tbfilename"] = fout.name
    return self.create_temp(str(choices))

  def test_empty(self):
    choices = self.create_temp(empty_choices_file)
    toolboximport.import_toolbox_lexicon(choices.name)
    self.c = ChoicesFile(choices.name)
    choices.close()
    self.gold.load_choices(empty_choices_file.splitlines())
    self.assert_choices_equal(self.c, self.gold)

  def test_import(self):
#    choices = self.create_temp(test_choices_file)
#    self.c = ChoicesFile(choices.name)
#    choices = self.add_toolbox_files(self.c, [[test_toolbox_file]])
#    toolboximport.import_toolbox_lexicon(choices.name)
#    self.c = ChoicesFile(choices.name)
#    choices.close()

    self.c.load_choices(test_gold_file.splitlines())
    self.gold.load_choices(test_choices_file.splitlines())
    self.save_choices(self.c)
#    self.assert_choices_equal(self.c, self.gold)


##############################################################################
#### Toolbox File Strings

empty_toolbox_file = ''

test_toolbox_file = '''
'''


##############################################################################
#### Choices File Strings

empty_choices_file = ''

test_choices_file = '''
'''

test_gold_file = '''
'''


if __name__ == '__main__':
    unittest.main()

class UnitTestIndex:
  # initialize by passing either a file name or ???file handle
  def __init__(self, index_file):
    self.file_name = index_file
    self.tests = {}

    f = open(index_file,'r')
    lines = f.readlines()

    for l in lines:
        l = l.strip()
        if l:
            (testname, comment) = l.split('=')
            self.set(testname, comment)

  # Set the value of 'key' to 'value'
  def set(self, testname, comment):
      self.tests[testname] = comment

  # Check whether a testname is there
  def exists(self, testname):
      return testname in self.tests

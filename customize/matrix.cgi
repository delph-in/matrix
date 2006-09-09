#!/usr/local/bin/python

######################################################################
# imports

import sys
write = sys.stdout.write
import os


######################################################################
# globals

form_data = {}

HTTP_header = 'Content-type: text/html\n'

HTML_pretitle = '''<html>
<head>
'''

HTML_posttitle = '''<script type="text/javascript">
function toggle_display(para_id, button_id)
{
  p = document.getElementById(para_id);
  b = document.getElementById(button_id);
  if (p.style.display == 'none') {
    p.style.display = 'block';
    b.innerHTML = '&#9660;';
  } else {
    p.style.display = 'none';
    b.innerHTML = '&#9658;';
  }
}
</script>
<link rel="stylesheet" href="matrix.css">
</head>
'''

HTML_mainprebody = '''<body>
<h1>LinGO Grammar Matrix</h1>
<h1>Matrix customization and download page</h1>
<h2>Version of 8/20/2006</h2>

<p>By filling out this form, you will produce an initial starter
package for your grammar, consisting of the language-independent core,
appropriate word order, negation, and polar question modules (if
available), some initial lexical types, and a tiny lexicon.  Note that
this small fragment will only treat matrix (main) clauses.

<p>Please note that this customization form can only accept lower
ascii alphanumerics (a-z, A-Z, 0-9, _).  You may wish to edit the file
<tt>lexicon.tdl</tt> in the output to fix the orthography of your
lexical items.

<p>Be advised that this system is highly experimental.  We are
interested in your feedback.  If you have questions or comments,
please email Emily Bender at: ebender at u dot washington dot
edu.

<p>[<a href="http://www.delph-in.net/matrix/">Back to Matrix main
page</a>]

'''

HTML_subprebody = '''<body>
'''

HTML_form = '''<form action="matrix.cgi" method="post">'''

HTML_postbody = '''</form>
</body>

</html>'''

HTML_submit = '<p><input type="submit" value="Submit">'

HTML_lipsum = \
'''Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat
non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.
'''


######################################################################
# tokenize(str)
#   split a string into words, treating double-quoted strings as
#   single words.

def tokenize(str):
  i = 0
  result = []

  while i < len(str):
    # skip whitespace
    while i < len(str) and str[i].isspace():
      i += 1
    # if it's quoted, read to the close quote, otherwise to a space
    if i < len(str) and str[i] == '"':
      i += 1
      a = i
      while i < len(str) and str[i] != '"':
        i += 1
      result.append(str[a:i])
      i += 1
    elif i < len(str):
      a = i
      while i < len(str) and not str[i].isspace():
        i += 1
      result.append(str[a:i])

  return result
  

######################################################################
# Class: VarNameMap, for mapping variables and friendly names

class VarNameMap:
  v2f = {}
  f2v = {}

  def __init__(self, namefile):
    f = open(namefile, 'r')
    line = f.readlines()
    f.close()
  
    for l in line:
      l = l.strip()
      if len(l):
        w = tokenize(l)
        if len(w) >= 3:
          ty = w[0]
          vn = w[1]
          fn = w[2]
          if ty == 'Text' or ty == 'Radio' or ty == '.' or ty == 'Check':
            self.v2f[vn] = fn
            self.f2v[fn] = vn

  def f(self, v):
    if self.v2f.has_key(v):
      return self.v2f[v]
    else:
      return v

  def v(self, f):
    if self.f2v.has_key(f):
      return self.f2v[f]
    else:
      return f


######################################################################
# mainpage(def_file, choices_file)
#   Create and print the main matrix page based on the arguments,
#   which are the name of a matrix definition file and an optional
#   choices file.

def mainpage(def_file, choices_file):
  #print HTTP_header
  print HTML_pretitle
  print '<title>The Matrix</title>'
  print HTML_posttitle
  print HTML_mainprebody
  print HTML_form

  choice = []
  try:
    f = open(choices_file, 'r')
    choice = f.readlines()
    f.close()
  except:
    pass

  f = open(def_file, 'r')
  line = f.readlines()
  f.close()
  
  i = 0
  while i < len(line):
    word = tokenize(line[i])
    if len(word) == 0:
      pass
    elif word[0] == 'Section':
      print '<div class="section"><span id="' + word[1] + 'button" onclick="toggle_display(\'' +\
            word[1] + '\',\'' + word[1] + 'button\')">&#9658;</span> ' +\
            '<a href="matrix.cgi?subpage=' + word[1] + '">' +\
            word[2] + '</a>'
      print '<div class="values" id="' + word[1] + '" style="display:none">'
      cur_sec = ''
      for l in choice:
        l = l.strip()
        (k, v) = l.split('=')
        if k == 'section':
          cur_sec = v.strip()
        elif cur_sec == word[1]:
          print namemap.f(k) + ' = ' + namemap.f(v) + '<br>'
      print '</div></div>'
    i += 1

  print HTML_postbody


######################################################################
# print_input(type, name, value, checked, before, after)
#   Write out an HTML <input> tag with the specified attributes and
#   surrounding text

def print_input(type, name, value, checked, before, after, size = ''):
  if value:
    value = ' value="' + value + '"'

  chkd = ''
  if checked:
    chkd = ' checked'

  if size:
    size = ' size="' + size + '"'
    
  print '%s<input type="%s" name="%s"%s%s%s>%s' % \
        (before, type, name, value, chkd, size, after)
  

######################################################################
# subpage(section, def_file, choices_file)
#   Create and print the matrix subpage for the specified section
#   based on the arguments, which are the name of a matrix definition
#   file and an optional choices file.

def subpage(section, def_file, choices_file):
  #print HTTP_header
  print HTML_pretitle

  form_data = {}
  try:
    f = open(choices_file, 'r')
    line = f.readlines()
    f.close()
    for l in line:
      (a, v) = l.split('=')
      v = v.strip()
      form_data[a] = v
  except:
    pass

  f = open(def_file, 'r')
  line = f.readlines()
  f.close()

  i = 0
  cur_sec = ''
  while i < len(line):
    word = tokenize(line[i])
    if len(word) == 0:
      pass
    elif word[0] == 'Section':
      cur_sec = word[1]
      if cur_sec == section:
        print '<title>' + word[2] + '</title>'
        print HTML_posttitle
        print HTML_subprebody
        print '<h2>' + word[2] + '</h2>'
        print HTML_form
        print_input('hidden', 'section', section, 0, '', '\n');
    elif cur_sec == section:
      if word[0] == 'Label':
        write('<p>\n')
        for w in word[1:]:
          write(w)
        write('</p>\n')
      elif word[0] == 'Separator':
        print '<hr>'
      elif word[0] == 'Check':
        (vn, fn, bf, af) = word[1:]
        checked = form_data.has_key(vn)
        print_input('checkbox', vn, '', checked, bf, af);
      elif word[0] == 'Radio':
        (vn, fn, bf, af) = word[1:]
        print bf
        i += 1
        while line[i] != '\n':
          word = tokenize(line[i])
          (rval, rfrn, rbef, raft) = word[1:]
          checked = 0
          if form_data.has_key(vn) and form_data[vn] == word[0]:
            checked = 1
          print_input('radio', vn, rval, checked, rbef, raft)
          i += 1
        print af
      elif word[0] == 'Text':
        (vn, fn, bf, af, sz) = word[1:]
        value = ''
        if form_data.has_key(vn):
          value = form_data[vn]
        print_input('text', vn, value, 0, bf, af, sz)
    i += 1

  print HTML_submit
  print HTML_postbody


######################################################################
# savechoices(form_data, choices_file)
#   Read the choices_file, stripping out the section associated with
#   the 'section' member of form_data, and replacing it with all the
#   values in form_data.

def save_choices(form_data, choices_file):
  section = form_data['section']
  del form_data['section']

  try:
    f = open(choices_file, 'r')
    line = f.readlines()
    f.close()
  except:
    line = []

  f = open(choices_file, 'w')

  wrote_values = 0
  cur_sec = ''
  i = 0
  while i < len(line):
    # split into attr and value around the equals sign
    (a, v) = line[i].split('=')
    v = v.strip()
    if a == 'section':
      cur_sec = v
      # if this is the section in question, write out the new values
      if cur_sec == section:
        wrote_values = 1
        f.write('section=' + section + '\n')
        for k in form_data.keys():
          f.write(k + '=' + form_data[k] + '\n')
      else:
        f.write(a + '=' + v + '\n')
    # only pass through sections *besides* the one in question
    elif cur_sec != section:
      f.write(a + '=' + v + '\n')
    i += 1

  # If the values haven't been written yet (because 'section' is not
  # yet in the choices file, write it out now.
  if not wrote_values:
    f.write('section=' + section + '\n')
    for k in form_data.keys():
      f.write(k + '=' + form_data[k] + '\n')

  f.close()


######################################################################
# beginning of main program

print HTTP_header

namemap = VarNameMap('matrixdef')

# get the CGI arguments
query = ''
method = os.getenv('REQUEST_METHOD')
if method == 'GET':
  query = os.getenv('QUERY_STRING')
elif method == 'POST':
  query = sys.stdin.read()

for q in query.split('&'):
  if q and q.find('=') != -1:
    pair = q.split('=')
    form_data[pair[0]] = pair[1]

# if the 'section' field is defined, we have submitted values to save
if form_data.has_key('section'):
  save_choices(form_data, 'choices')
  
if form_data.has_key('subpage'):
  subpage(form_data['subpage'], 'matrixdef', 'choices')
else:
  mainpage('matrixdef', 'choices')

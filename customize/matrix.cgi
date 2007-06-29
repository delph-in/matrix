#!/usr/local/bin/python2.4

######################################################################
# imports

import sys
write = sys.stdout.write
import os
getenv = os.getenv
import random
randint = random.randint
import cgi
import cgitb; cgitb.enable()
import time
import glob
from distutils.dir_util import remove_tree

import utils
tokenize_def = utils.tokenize_def
import customize
customize_matrix = customize.customize_matrix
import validate
validate_choices = validate.validate_choices
from choices import ChoicesFile

def dummy():
  pass # let emacs know the indentation is 2 spaces


######################################################################
# globals

wrong = {}

HTTP_header = 'Content-type: text/html'

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

function clear_form()
{
  elements = document.getElementsByTagName('input');
  for (i = 0; elements.item(i); i++) {
    elm = elements.item(i)
    if (elm.type == 'text') {
      elm.value = '';
    } else if (elm.type == 'radio' || elm.type == 'checkbox') {
      elm.checked = ''
    }
  }
}
</script>
<link rel="stylesheet" href="matrix.css">
</head>
'''

HTML_mainprebody = '''<body>
<h1>LinGO Grammar Matrix</h1>
<h1>Matrix customization and download page</h1>
<h2>Version of 6/29/2007</h2>

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

HTML_customprebody = '''<h3>Customized Matrix</h3>

<p>A customized copy of the Matrix has been created for you.  
Please download it <a href="%s">here</a>.

<p>This file will be removed from the system in 15 minutes.

<h3>Instructions</h3>

<p>To unpack the archive, if your browswer hasn't already done it for
you, first try saving it on your desktop and double clicking it.  If
that doesn't work, and you're using Linux or Mac OS X, from a command
prompt, type <tt>tar xzf matrix.tar.gz</tt> or <tt>unzip matrix.zip</tt>.

<p>Once you've unpacked the archive you should find a directory called
<tt>matrix</tt>.  Inside the directory are several files.  Here is
an explanation of some:

<ul>
<li><tt>matrix.tdl</tt>: Language independent type and constraint
definitions.  You should not need to modify this file.
<li><tt>my_language.tdl</tt>: Types and constraints specific to your
language.  (Actual file name depends on the name of your language.)
<li><tt>lexicon.tdl</tt>: Lexical entries for your language.
<li><tt>rules.tdl</tt>: Phrase structure rule instance entries for
your language.
<li><tt>irules.tdl</tt>: Spelling-changing lexical rule instance
entries for your language.
<li><tt>lrules.tdl</tt>: Non-spelling-changing lexical rule instance
entries for your language.
<li><tt>lkb/script</tt>: The script file for loading your grammar into
the LKB.
<li><tt>choices</tt>: A record of the information you
provided in the matrix configuration form.
</ul>

<p>To run this grammar, start the LKB, and the load it by selected
"Load > Complete grammar..." from the LKB menu.  You can then parse a
sentence by selecting "Parse > Parse input..." from the LKB menu.  The
dialogue box that pops up should include the sentences you filled into
the form.  When a sentence parses successfully, you can try generating
from the associated semantics by selecting "Generate" or "Generate
from edge" from the pop-up menu.  For more on using the LKB, see the
<a href="http://www.delph-in.net/lkb">LKB page</a> and/or Copestake
2002 <a
href="http://cslipublications.stanford.edu/lkb.html"><i>Implementing
Typed Feature Structure Grammars</i></a>.

<hr>
<a href="matrix.cgi">Back to form</a><br>
<a href="http://www.delph-in.net/matrix/">Back to Matrix main page</a><br>
<a href="http://www.delph-in.net/lkb">To the LKB page</a>
'''

HTML_prebody = '''<body>
'''

HTML_method = 'post'
HTML_preform = '<form action="matrix.cgi" method="' + HTML_method + '">'

HTML_postform = '</form>'

HTML_uploadpreform = '''
<form action="matrix.cgi" method="post" enctype="multipart/form-data">
'''

HTML_uploadpostform = '</form>'

HTML_postbody = '''</body>

</html>'''


######################################################################
# Stupid: The Python syntax coloring in Emacs doesn't seem properly
# handle single-quotes inside of triple-quoted strings, so, as
# necessary to turn syntax-coloring back on for the rest of the file,
# include (or not) an extra apostrophe here:


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
        w = tokenize_def(l)
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
# print_input(type, name, value, checked, before, after, size, onclick)
#   Write out an HTML <input> tag with the specified attributes and
#   surrounding text

def print_input(type, name, value, checked, before, after, \
                size = '', onclick = ''):
  if value:
    value = ' value="' + value + '"'

  chkd = ''
  if checked:
    chkd = ' checked'

  if size:
    size = ' size="' + size + '"'

  if onclick:
    onclick = ' onclick="' + onclick + '"'
    
  print '%s<input type="%s" name="%s"%s%s%s%s>%s' % \
        (before, type, name, value, chkd, size, onclick, after)
  

######################################################################
# main_page(def_file, cookie)
#   Create and print the main matrix page based on the arguments,
#   which are the name of a matrix definition file and a cookie that
#   determines where to look for the choices file

def main_page(def_file, cookie):
  print HTTP_header
  print 'Set-cookie: session=' + cookie + '\n'
  print HTML_pretitle
  print '<title>The Matrix</title>'
  print HTML_posttitle
  print HTML_mainprebody

  choices_file = 'sessions/' + cookie + '/choices'

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
  
  for l in line:
    word = tokenize_def(l)
    if len(word) == 0:
      pass
    elif word[0] == 'Section':
      print '<div class="section"><span id="' + word[1] + 'button" onclick="toggle_display(\'' +\
            word[1] + '\',\'' + word[1] + 'button\')">&#9658;</span> ' +\
            '<a href="matrix.cgi?subpage=' + word[1] + '">' +\
            word[2] + '</a>'
      print '<div class="values" id="' + word[1] + '" style="display:none">'
      cur_sec = ''
      for c in choice:
        c = c.strip()
        if c:
          (a, v) = c.split('=')
          if a == 'section':
            cur_sec = v.strip()
          elif cur_sec == word[1]:
            print namemap.f(a) + ' = ' + namemap.f(v) + '<br>'
      print '</div></div>'

  print HTML_preform

  tgz_checked = False
  zip_checked = False
  if os.name == 'nt':
    zip_checked = True
  else:
    tgz_checked = True

  print_input('hidden', 'customize', 'customize', False, '', '');
  print_input('radio', 'delivery', 'tgz', tgz_checked, '<p class="submit">Archive type: ', ' .tar.gz')
  print_input('radio', 'delivery', 'zip', zip_checked, ' ', ' .zip<br>')
  print_input('submit', '', 'Create Grammar', False, '', '</p>')
  print_input('button', '', 'Download Choices File', False, '<p class="submit">', '</p>', '', 'window.location.href=\'' + choices_file + '\'')
  print HTML_postform
  print HTML_uploadpreform
  print_input('submit', '', 'Upload Choices File:', False, '<p class="submit">', '')
  print_input('file', 'choices', '', False, '', '</p>', '20')
  print HTML_uploadpostform
  print HTML_postbody


######################################################################
# sub_page(section, def_file, cookie)
#   Create and print the matrix subpage for the specified section
#   based on the arguments, which are the name of a matrix definition
#   file and a cookie that determines where to look for the choices
#   file

def sub_page(section, def_file, cookie):
  print HTTP_header + '\n'
  print HTML_pretitle

  choices = ChoicesFile('sessions/' + cookie + '/choices')

  f = open(def_file, 'r')
  line = f.readlines()
  f.close()

  i = 0
  cur_sec = ''
  while i < len(line):
    word = tokenize_def(line[i])
    if len(word) == 0:
      pass
    elif word[0] == 'Section':
      cur_sec = word[1]
      if cur_sec == section:
        print '<title>' + word[2] + '</title>'
        print HTML_posttitle
        print HTML_prebody
        print '<h2>' + word[2] + '</h2>'
        print HTML_preform
        print_input('hidden', 'section', section, False, '', '\n');
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
        checked = choices.is_set(vn)
        print_input('checkbox', vn, '', checked, bf, af);
      elif word[0] == 'Radio':
        (vn, fn, bf, af) = word[1:]
        print bf
        i += 1
        while line[i] != '\n':
          word = tokenize_def(line[i])
          (rval, rfrn, rbef, raft) = word[1:]
          checked = False
          if choices.is_set(vn) and choices.get(vn) == rval:
            checked = True
          print_input('radio', vn, rval, checked, rbef, raft)
          i += 1
        print af
      elif word[0] == 'Text':
        (vn, fn, bf, af, sz) = word[1:]
        value = choices.get(vn)
        print_input('text', vn, value, False, bf, af, sz)
    i += 1

  print_input('submit', '', 'Submit', False, '<p>', '')
  print_input('button', '', 'Clear', False, '', '</p>', '', 'clear_form()')
  print HTML_postform
  print HTML_postbody


######################################################################
# custom_page(session_path)
#   Create and print the "download your matrix here" page for the
#   customized matrix in the directory specified by session_path

def custom_page(session_path, arch_type):
  print HTTP_header + '\n'
  print HTML_pretitle
  print '<title>Matrix Customized</title>'
  if arch_type == 'tgz':
    arch_file = 'matrix.tar.gz'
  else:
    arch_file = 'matrix.zip'
  print HTML_customprebody % (session_path + '/' + arch_file)
  print HTML_postbody


######################################################################
# error_page(session_path)
#   Display errors that occurred during customization

def error_page():
  print HTTP_header + '\n'
  print HTML_pretitle
  print '<title>Matrix Customization Errors</title>'
  print HTML_prebody

  for w in wrong:
    print wrong[w] + '<br>'

  print HTML_postbody


######################################################################
# save_choices(form_data, def_file, choices_file)
#   Read the choices_file, stripping out the section associated with
#   the 'section' member of form_data, and replacing it with all the
#   values in form_data.  Use def_file to keep the choices file in
#   order.

def save_choices(form_data, def_file, choices_file):
  # The section isn't really a form field, but save it for later
  section = form_data['section'].value

  # Read the current choices file (if any) into old_choices
  old_choices = ChoicesFile(choices_file)

  # Open the def file and store it in line[]
  f = open(def_file, 'r')
  line = f.readlines()
  f.close()

  # Now pass through the def file, writing out either the old choices
  # for each section or, for the section we're saving, the new choices
  f = open(choices_file, 'w')
  f.write('version=' + str(old_choices.current_version()) + '\n\n')

  cur_sec = ''
  done = {}
  for l in line:
    word = tokenize_def(l)
    if len(word) == 0:
      pass
    elif word[0] == 'Section':
      if cur_sec:
        f.write('\n')
      cur_sec = word[1]
      f.write('section=' + cur_sec + '\n')
    elif word[0] == 'Check' or word[0] == 'Radio' or word[0] == 'Text':
      a = word[1]
      if not done.has_key(a):
        done[a] = ' '
        v = ''
        if cur_sec == section:
          if form_data.has_key(a):
            v = form_data[a].value
        else:
          if old_choices.is_set(a):
            v = old_choices.get(a)
        if a and v:
          f.write(a + '=' + v + '\n')
  f.close()


######################################################################
# beginning of main program

#print HTTP_header + '\n'

namemap = VarNameMap('matrixdef')

form_data = cgi.FieldStorage()

# Get the cookie.  If there's not one, make one.
http_cookie = getenv('HTTP_COOKIE')
if http_cookie:
  cookie = http_cookie.split('=')[1]
else:
  cookie = str(randint(1000,9999))
  while os.path.exists('sessions/' + cookie):
    cookie = str(randint(1000,9999))

# make the sessions directory if necessary
if not os.path.exists('sessions'):
  os.mkdir('sessions')

# look for any sessions older than an hour and delete them
now = time.time()
sessions = glob.glob('sessions/*')
for s in sessions:
  if now - os.path.getmtime(s) > 3600:
    remove_tree(s)

# figure out the path to the current session's directory, creating it
# if necessary
session_path = 'sessions/' + cookie
if cookie and not os.path.exists(session_path):
  os.mkdir(session_path)

# if the 'choices' field is defined, we have an uploaded choices file
# to replace the current one
if form_data.has_key('choices'):
  data = form_data['choices'].value
  if data:
    f = open(session_path + '/choices', 'w')
    f.write(data)
    f.close()

# if the 'section' field is defined, we have submitted values to save
if form_data.has_key('section'):
  save_choices(form_data, 'matrixdef', session_path + '/choices')

# if the 'customize' field is defined, create a customized copy of the matrix
# based on the current choices file
if form_data.has_key('customize'):
  wrong = validate_choices(session_path + '/choices')

# ERB 2006-10-03 Checking has_key here to enable local debugging.

  if form_data.has_key('delivery'):
    arch_type = form_data['delivery'].value
  else:
    arch_type = ''
  if arch_type != 'tgz' and arch_type != 'zip':
    wrong['delivery'] = "You must specify an archive type."

  if len(wrong):
    error_page()
  else:
    customize_matrix(session_path, arch_type)
    custom_page(session_path, arch_type)
elif form_data.has_key('subpage'):
  sub_page(form_data['subpage'].value, 'matrixdef', cookie)
else:
  main_page('matrixdef', cookie)

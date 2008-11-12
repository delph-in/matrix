### $Id: deffile.py,v 1.16 2008-09-30 23:50:02 lpoulson Exp $

######################################################################
# This module is currently a bit of a hybrid.  Most of the code is
# part of the MatrixDefFile class, which is used both to parse
# ./matrixdef and to emit HTML.  However, a couple of the methods
# on that class, while they do output HTML, don't have anything to
# do with the matrixdef file.  The HTML-generation code should
# probably be split out into a separate module/class.  Later.
#   - sfd 3/5/2008

######################################################################
# imports

import sys
import os
import glob
import re
from choices import ChoicesFile


######################################################################
# HTML blocks, used to create web pages

def dummy():
  pass # let emacs know the indentation is 2 spaces

HTTP_header = 'Content-type: text/html'

HTML_pretitle = '''<html>
<head>
'''

HTML_posttitle = '''<script type="text/javascript" src="matrix.js">
</script>

<script type="text/javascript">
// An array of strings, each of the form 'name:value|friendly value,...'
var features = [
%s
];

var verb_case_patterns = [
%s
];

var morph_case_patterns = [
%s
];
</script>

<link rel="stylesheet" href="matrix.css">
</head>
'''

HTML_mainprebody = '''<body>
<h1>LinGO Grammar Matrix</h1>
<h1>Matrix customization and download page</h1>
<h2>Version of %s</h2>

<p>Filling out this form will produce a starter grammar for a natural
language, consisting of a language-independent core and customized
support for the phenomena you describe below.  Note that this grammar
fragment will only treat matrix (main) clauses.

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

<p>This file will be removed from the system in 24 hours.

<h3>Instructions</h3>

<p>To unpack the archive, if your browser hasn't already done it for
you, first try saving it on your desktop and double-clicking it.  If
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
dialog box that pops up should include the sentences you filled into
the form.  When a sentence parses successfully, you can try generating
from the resulting semantic representation by selecting "Generate" or
"Generate from edge" from the pop-up menu.  For more on using the LKB,
see the <a href="http://www.delph-in.net/lkb">LKB page</a> and/or
Copestake 2002 <a
href="http://cslipublications.stanford.edu/lkb.html"><i>Implementing
Typed Feature Structure Grammars</i></a>.

<hr>
<a href="matrix.cgi">Back to form</a><br>
<a href="http://www.delph-in.net/matrix/">Back to Matrix main page</a><br>
<a href="http://www.delph-in.net/lkb">To the LKB page</a>
'''

HTML_prebody = '''<body onload="animate(); focus_all_fields(); multi_init();">
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
# Stupid: The Python syntax coloring in Emacs doesn't properly handle
# single-quotes inside of triple-quoted strings, so, as necessary to
# turn syntax-coloring back on for the rest of the file, include (or
# not) an extra apostrophe here:


######################################################################
# HTML creation functions

# Return an HTML <input> tag with the specified attributes and
# surrounding text
def html_input(errors, type, name, value, checked, before, after,
               size = '', onclick = '', disabled = False):
  if value:
    value = ' value="' + value + '"'

  chkd = ''
  if checked:
    chkd = ' checked'

  if size:
    size = ' size="' + size + '"'

  if onclick:
    onclick = ' onclick="' + onclick + '"'

  dsabld = ''
  if disabled:
    dsabld = ' disabled'

  asterisk = ''
  if name and errors.has_key(name):
    asterisk = '<span class="error" title="%s">*</span>' % \
               (errors[name])

  return '%s%s<input type="%s" name="%s"%s%s%s%s%s>%s' % \
         (before, asterisk, type, name, value, chkd, size, dsabld,
          onclick, after)


# Return an HTML <select> tag with the specified name
def html_select(errors, name, multi, onfocus = ''):
  asterisk = ''
  if name and errors.has_key(name):
    asterisk = '<span class="error" title="%s">*</span>' % \
               (errors[name])

  multi_attr = ''
  if multi:
    multi_attr = ' class="multi"'

  if onfocus:
    onfocus = ' onfocus="' + onfocus + '"'

  return '%s<select name="%s"%s%s>' % \
         (asterisk, name, multi_attr, onfocus)


# Return an HTML <option> tag with the specified attributes and
# surrounding text
def html_option(errors, name, selected, html, temp=False):
  sld = ''
  if selected:
    sld = ' selected'

  if temp:
    temp = ' class="temp"'
  else:
    temp = ''

  return '<option value="%s"%s%s>%s</option>' % \
         (name, sld, temp, html)


# split a string into words, treating double-quoted strings as
# single words.
def tokenize_def(str):
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
      while i < len(str) and not (str[i] == '"' and str[i-1] != '\\'):
        i += 1
      result.append(str[a:i].replace('\\"', '"'))
      i += 1
    elif i < len(str):
      a = i
      while i < len(str) and not str[i].isspace():
        i += 1
      result.append(str[a:i])

  return result


# Replace variables of the form {name} in word using the dict iter_vars
def replace_vars(word, iter_vars):
  for k in iter_vars.keys():
    word = re.sub('\\{' + k + '\\}', str(iter_vars[k]), word)
  return word


# From a list of triples of strings [string1, string2, ...], return
# a string containing a JavaScript-formatted list of strings of the
# form 'string1:string2'.
def js_array(list):
  val = ''
  for l in list:
    val += '\'' + l[0] + ':' + l[1] + '\',\n'
  val = val[:-2]  # trim off the last ,\n
  return val


######################################################################
# MatrixDefFile class
# This class and its methods are used to parse Matrix definition
# formatted files (currently just the file ./matrixdef), and based
# on the contents, to produce HTML pages and save choices files.

class MatrixDefFile:
  def_file = ''
  v2f = {}
  f2v = {}

  def __init__(self, def_file):
    self.def_file = def_file
    self.make_name_map()

  ######################################################################
  # Variable/friendly name mapping
  
  # initialize the v2f and f2v dicts
  def make_name_map(self):
    f = open(self.def_file, 'r')
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
          if ty in ['Text', 'Check', 'Radio', 'Select', 'MultiSelect', '.']:
            self.v2f[vn] = fn
            self.f2v[fn] = vn

  # return the friendly name for a variable, or the variable if none
  # is defined
  def f(self, v):
    if self.v2f.has_key(v):
      return self.v2f[v]
    else:
      return v

  # return the variablefor a friendly name, or the friendly name if
  # none is defined
  def v(self, f):
    if self.f2v.has_key(f):
      return self.f2v[f]
    else:
      return f


  # Create and print the main matrix page.  The argument is a cookie
  # that determines where to look for the choices file.
  def main_page(self, cookie, errors):
    print HTTP_header
    print 'Set-cookie: session=' + cookie + '\n'
    print HTML_pretitle
    print '<title>The Matrix</title>'
    print HTML_posttitle % ('', '', '')

    try:
      f = open('datestamp', 'r')
      datestamp = f.readlines()[0].strip()
      f.close()
    except:
      pass

    print HTML_mainprebody % (datestamp)
    print '<div class="indented">'

    choices_file = 'sessions/' + cookie + '/choices'

    choice = []
    try:
      f = open(choices_file, 'r')
      choice = f.readlines()
      f.close()
    except:
      pass

    f = open(self.def_file, 'r')
    line = f.readlines()
    f.close()

    # pass through the definition file once, augmenting the list of validation
    # errors with section names so that we can put red asterisks on the links
    # to the assocated sub-pages on the main page.
    prefix = ''
    for l in line:
      word = tokenize_def(l)
      if len(word) < 2:
        pass
      elif word[0] == 'Section':
        cur_sec = word[1]
      elif word[0] == 'BeginIter':
        if prefix:
          prefix += '_'
        prefix += re.sub('\\{.*\\}', '.*', word[1])
      elif word[0] == 'EndIter':
        prefix = re.sub('_?' + word[1] + '[^_]*$', '', prefix)
      elif word[0] != 'Label' and word[0][0] != '#' and \
           not errors.has_key(cur_sec):
        pat = '^' + prefix
        if prefix:
          pat += '_'
        pat += word[1] + '$'
        for k in errors.keys():
          if re.search(pat, k):
            errors[cur_sec] = 'error in section'
            break

    # now pass through again to actually emit the page
    for l in line:
      word = tokenize_def(l)
      if len(word) == 0:
        pass
      elif word[0] == 'Section':
        print '<div class="section"><span id="' + word[1] + \
              'button" onclick="toggle_display(\'' + \
              word[1] + '\',\'' + word[1] + 'button\')">&#9658;</span> '
        if errors.has_key(word[1]):
          print '<span class="error">* </span>'
        print '<a href="matrix.cgi?subpage=' + word[1] + '">' + \
              word[2] + '</a>'
        print '<div class="values" id="' + word[1] + '" style="display:none">'
        cur_sec = ''
        for c in choice:
          c = c.strip()
          if c:
            (a, v) = c.split('=', 1)
            if a == 'section':
              cur_sec = v.strip()
            elif cur_sec == word[1]:
              print self.f(a) + ' = ' + self.f(v) + '<br>'
        print '</div></div>'

    print HTML_preform

    tgz_checked = False
    zip_checked = False
    if os.name == 'nt':
      zip_checked = True
    else:
      tgz_checked = True

    # the buttons after the subpages
    print html_input(errors, 'hidden', 'customize', 'customize', False, '', '')
    print html_input(errors, 'radio', 'delivery', 'tgz', tgz_checked,
                     '<p>Archive type: ', ' .tar.gz')
    print html_input(errors, 'radio', 'delivery', 'zip', zip_checked,
                     ' ', ' .zip<br>')
    print html_input(errors, 'submit', '', 'Create Grammar', False, '', '</p>',
                     '', '', len(errors) > 0)

    print '<hr>\n'
    print html_input(errors, 'button', '', 'Download Choices File', False,
                     '<p>', '</p>', '',
                     'window.location.href=\'' + choices_file + '\'')
    
    print HTML_postform

    # the FORM for uploading choices files
    print HTML_uploadpreform
    print html_input(errors, 'submit', '', 'Upload Choices File:', False,
                     '<p>', '')
    print html_input(errors, 'file', 'choices', '', False, '', '</p>', '20')
    print HTML_uploadpostform

    # the list of sample choices files
    if os.path.exists('sample-choices'):
      print '<h3>Sample Grammars:</h3>\n' + \
            '<p>Click a link below to have the questionnaire ' + \
            'filled out automatically.</p>'
      print '<p>'
      globlist = glob.glob('sample-choices/*')
      globlist.sort()
      for f in globlist:
        f = f.replace('\\', '/')
        choices = ChoicesFile(f)
        lang = choices.get('language')
        if not lang:
          lang = '[empty questionnaire]'
        print '<a href="matrix.cgi?choices=' + f + '">' + \
              lang + '</a><br>\n'
      print '</p>'

    print '</div>'
    print HTML_postbody

  # Turn a list of lines containing matrix definitions into a string
  # containing HTML.
  def defs_to_html(self, lines, choices, errors, prefix, vars):
    html = ''
    i = 0
    
    while i < len(lines):
      word = tokenize_def(replace_vars(lines[i], vars))
      if len(word) == 0:
        pass
      elif word[0] == 'Label':
        for w in word[1:]:
          html += w
        html += '\n'
      elif word[0] == 'Separator':
        html += '<hr>'
      elif word[0] == 'Check':
        (vn, fn, bf, af) = word[1:]
        vn = prefix + vn
        checked = choices.is_set_full(vn)
        html += html_input(errors, 'checkbox', vn, '', checked,
                           bf, af) + '\n'
      elif word[0] == 'Radio':
        (vn, fn, bf, af) = word[1:]
        vn = prefix + vn
        html += bf + '\n'
        i += 1
        while lines[i] != '\n':
          word = tokenize_def(replace_vars(lines[i], vars))
          (rval, rfrn, rbef, raft) = word[1:]
          checked = False
          if choices.is_set_full(vn) and choices.get_full(vn) == rval:
            checked = True
          html += html_input(errors, 'radio', vn, rval, checked,
                             rbef, raft) + '\n'
          i += 1
        html += af + '\n'
      elif word[0] in ['Select', 'MultiSelect']:
        multi = (word[0] == 'MultiSelect')
        (vn, fn, bf, af) = word[1:]
        vn = prefix + vn

        html += bf + '\n'

        # look ahead and see if we have an auto-filled drop-down
        i += 1
        fill_type = ''
        fill_arg1 = ''
        fill_arg2 = ''
        if lines[i] != '\n':
          word = tokenize_def(replace_vars(lines[i], vars))
          fill_type = word[0]
          if len(word) > 1:
            fill_arg1 = word[1]
          if len(word) > 2:
            fill_arg2 = word[2]

        if fill_type[0:4] == 'fill':
          if fill_type == 'fillregex':
            if fill_arg2:
              html += html_select(errors, vn, multi,
                                  'fill_regex(\'' + vn + \
                                  '\', \'' + fill_arg1 + '\', true)') + '\n'
            else:
              html += html_select(errors, vn, multi,
                                  'fill_regex(\'' + vn + \
                                  '\', \'' + fill_arg1 + '\')') + '\n'
          elif fill_type == 'fillnames':
            html += html_select(errors, vn, multi,
                                'fill_feature_names(\'' + vn + '\')') + '\n'
          elif fill_type == 'fillvalues':
            if fill_arg2:
              html += html_select(errors, vn, multi,
                                  'fill_feature_values(\'' + vn + \
                                  '\', \'' + fill_arg1 + '\', true)') + '\n'
            else:
              html += html_select(errors, vn, multi,
                                  'fill_feature_values(\'' + vn + \
                                  '\', \'' + fill_arg1 + '\')') + '\n'
          elif fill_type == 'fillverbpat':
            html += html_select(errors, vn, multi,
                                'fill_case_patterns(\'' + vn + \
                                '\', false)') + '\n'
          elif fill_type == 'fillmorphpat':
            html += html_select(errors, vn, multi,
                                'fill_case_patterns(\'' + vn + \
                                '\', true)') + '\n'

          html += html_option(errors, '', False, '') + '\n'

          if choices.is_set_full(vn):
            sval = choices.get_full(vn)
            shtml = ''
            # If we're filling in a SELECT that shows friendly names,
            # we have to look it up.
            if fill_type in ['fillvalues']:
              for sv in sval.split(', '):
                for f in choices.features():
                  if f[0] == choices.get_full(fill_arg1):
                    for v in f[1].split(';'):
                      n = v.split('|')
                      if n[0] == sv:
                        if shtml:
                          shtml += ', '
                        shtml += n[1]
            elif fill_type in ['fillverbpat']:
              for sv in sval.split(', '):
                for p in choices.patterns():
                  if p[0] == sv:
                    if shtml:
                      shtml += ', '
                    shtml += p[1]
            else:
              shtml = sval
            html += html_option(errors, sval, True, shtml, True) + '\n'
          i += 1
        else:
          html += html_select(errors, vn, multi) + '\n'
          html += html_option(errors, '', False, '') + '\n'

        while lines[i] != '\n':
          word = tokenize_def(replace_vars(lines[i], vars))
          (sval, sfrn, shtml) = word[1:]
          selected = False
          if choices.is_set_full(vn) and choices.get_full(vn) == sval:
            selected = True
          html += html_option(errors, sval, selected, shtml) + '\n'
          i += 1

        html += '</select>'
        html += af + '\n'
      elif word[0] == 'Text':
        (vn, fn, bf, af, sz) = word[1:]
        vn = prefix + vn
        value = choices.get_full(vn)
        html += html_input(errors, 'text', vn, value, False,
                           bf, af, sz) + '\n'
      elif word[0] == 'BeginIter':
        iter_orig = word[1]
        (iter_name, iter_var) = word[1].replace('}', '').split('{', 1)
        label = word[2]
        iter_min = 0
        if len(word) > 3:
          iter_min = int(word[3])
        i += 1
        beg = i
        while True:
          word = tokenize_def(lines[i])
          if len(word) == 0:
            pass
          elif word[0] == 'EndIter' and word[1] == iter_name:
            break
          i += 1
        end = i

        html += '<div class="iterator" style="display: none" id="' + \
                prefix + iter_name + '_TEMPLATE">\n'
        html += self.defs_to_html(lines[beg:end], choices, errors,
                                  prefix + iter_orig + '_', vars)
        html += '</div>\n\n'

        choices.iter_begin(iter_name)
        cur = 1
        while choices.iter_valid() or cur <= iter_min:
          pre = prefix + iter_name + str(cur) + '_'
          vars[iter_var] = cur

          # pre[:-1] trims the trailing '_'
          html += '<div class="iterator" id="' + pre[:-1] + '">\n'
          html += self.defs_to_html(lines[beg:end], choices, errors, pre, vars)
          html += '</div>\n'

          choices.iter_next()
          del vars[iter_var]
          cur += 1
        choices.iter_end()

        html += '<div class="anchor" id="' + \
                prefix + iter_name + '_ANCHOR"></div>\n'
        html += '<p><input type="button" name="" ' + \
                'value="Add ' + label + '" ' + \
                'onclick="clone_region(\'' + \
                prefix + iter_name + '\', \'' + \
                iter_var + '\')">'
        html += '<input type="button" name="" ' + \
                'value="Remove ' + label + '" ' + \
                'onclick="remove_region(\'' + \
                prefix + iter_name + '\')"></p>\n'

      i += 1

    return html


  # Create and print the matrix subpage for the specified section
  # based on the arguments, which are the name of the section and
  # a cookie that determines where to look for the choices file
  def sub_page(self, section, cookie, errors):
    print HTTP_header + '\n'
    print HTML_pretitle

    choices_file = 'sessions/' + cookie + '/choices'
    choices = ChoicesFile(choices_file)

    f = open(self.def_file, 'r')
    lines = f.readlines()
    f.close()

    section_begin = -1
    section_end = -1
    section_friendly = ''

    i = 0
    while i < len(lines):
      word = tokenize_def(lines[i])
      if len(word) == 0:
        pass
      elif word[0] == 'Section':
        if section_begin != -1:
          section_end = i
          break
        if word[1] == section:
          section_begin = i + 1
          section_friendly = word[2]
        cur_sec = word[1]
        cur_sec_friendly = word[2]
        cur_sec_begin = i + 1
      i += 1

    if section_begin != -1:
      if section_end == -1:
        section_end = i

      print '<title>' + section_friendly + '</title>'
      print HTML_posttitle % \
            (js_array(choices.features()),
             js_array([c for c in choices.patterns() if not c[2]]),
             js_array([c for c in choices.patterns() if c[2]]))
      print HTML_prebody
      print '<h2>' + section_friendly + '</h2>'
      print HTML_preform
      print html_input(errors, 'hidden', 'section', section,
                       False, '', '\n')
      print self.defs_to_html(lines[section_begin:section_end],
                              choices, errors, '', {})

    print html_input(errors, 'submit', '', 'Submit', False, '<p>', '')
    print html_input(errors, 'button', '', 'Clear', False, '', '</p>', '',
                     'clear_form()')

    print HTML_postform
    print HTML_postbody


  # Create and print the "download your matrix here" page for the
  # customized matrix in the directory specified by session_path
  def custom_page(self, session_path, arch_type):
    print HTTP_header + '\n'
    print HTML_pretitle
    print '<title>Matrix Customized</title>'
    if arch_type == 'tgz':
      arch_file = 'matrix.tar.gz'
    else:
      arch_file = 'matrix.zip'
    print HTML_customprebody % (session_path + '/' + arch_file)
    print HTML_postbody


  # Display errors that occurred during customization
  def error_page(self, errors):
    print HTTP_header + '\n'
    print HTML_pretitle
    print '<title>Matrix Customization Errors</title>'
    print HTML_prebody

    for e in errors:
      print errors[e] + '<br>'

    print HTML_postbody


  # Inform the user that cookies must be enabled
  def cookie_error_page(self):
    print HTTP_header + '\n'
    print HTML_pretitle
    print '<title>Cookies Required</title>'
    print HTML_prebody

    print '<div style="position:absolute; top:45%; width:100%">\n' + \
          '<p style="color:red; text-align:center; font-size:16pt">' + \
          'Cookies must be enabled for this site in your browser in order ' + \
          'to fill out the questionnaire.</p>\n'

    print HTML_postbody


  # Based on a section of a matrix definition file in lines, save the
  # values from choices into the file handle f.  The section in lines
  # need not correspond to a whole named section (e.g. "Language"), but
  # can be any part of the file not containing a section line.
  def save_choices_section(self, lines, f, choices):
    already_saved = {}  # don't save a variable more than once
    i = 0
    while i < len(lines):
      word = tokenize_def(lines[i])
      if len(word) == 0:
        pass
      elif word[0] in ['Check', 'Text', 'Radio', 'Select', 'MultiSelect']:
        a = choices.iter_prefix() + word[1]
        if not already_saved.has_key(a):
          already_saved[a] = True
          v = ''
          if choices.is_set_full(a):
            v = choices.get_full(a)
          if a and v:
            f.write(a + '=' + v + '\n')
      elif word[0] == 'BeginIter':
        iter_orig = word[1]
        (iter_name, iter_var) = word[1].replace('}', '').split('{', 1)
        i += 1
        beg = i
        while True:
          word = tokenize_def(lines[i])
          if len(word) == 0:
            pass
          elif word[0] == 'EndIter' and word[1] == iter_name:
            break
          i += 1
        end = i

        choices.iter_begin(iter_name)
        while choices.iter_valid():
          self.save_choices_section(lines[beg:end], f, choices)
          choices.iter_next()
        choices.iter_end()

      i += 1


  # Read the choices_file, stripping out the section associated with
  # the 'section' member of form_data, and replacing it with all the
  # values in form_data.  Use self.def_file to keep the choices file
  # in order.
  def save_choices(self, form_data, choices_file):
    # The section isn't really a form field, but save it for later
    section = form_data['section'].value

    # Copy the form_data into a choices object
    new_choices = ChoicesFile('')
    for k in form_data.keys():
      if k:
        new_choices.set(k, form_data[k].value)

    # Read the current choices file (if any) into old_choices
    old_choices = ChoicesFile(choices_file)

    # Open the def file and store it in line[]
    f = open(self.def_file, 'r')
    lines = f.readlines()
    f.close()

    # Now pass through the def file, writing out either the old choices
    # for each section or, for the section we're saving, the new choices
    f = open(choices_file, 'w')
    f.write('version=' + str(old_choices.current_version()) + '\n\n')

    cur_sec = ''
    cur_sec_begin = 0
    i = 0
    while i < len(lines):
      word = tokenize_def(lines[i])
      if len(word) == 0:
        pass
      elif word[0] == 'Section':
        if cur_sec:
          self.save_choices_section(lines[cur_sec_begin:i], f, choices)
          f.write('\n')
        cur_sec = word[1]
        cur_sec_begin = i + 1
        f.write('section=' + cur_sec + '\n')
        if cur_sec == section:
          choices = new_choices
        else:
          choices = old_choices
      i += 1

    # Make sure to save the last section
    if cur_sec_begin:
      self.save_choices_section(lines[cur_sec_begin:i], f, choices)

    f.close()

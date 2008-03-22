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
  elements = document.getElementsByTagName('select');
  for (i = 0; elements.item(i); i++) {
    elm = elements.item(i)
    elm.value = '';
  }
}

animations = [];
an_max = 0;
function animate()
{
  for (i = an_max - 1; i >= 0; i--) {
    a = animations[i];
    n = document.getElementById(a.id);
    if (a.ticks) {
      v = Number(n.style[a.property].replace(/px/, ''));
      if (a.factor > 0) {
        v *= a.factor;
      } else {
        v += a.step;
      }
      n.style[a.property] = v + 'px';
      a.ticks--;
    } else {
      n.style[a.property] = '';
      //n.style.overflow = '';
      animations =
        animations.slice(0,i).concat(animations.slice(i + 1, an_max));
      an_max--;
    }
  }

  setTimeout('animate()', 10);
}

function expand_region(id)
{
  n = document.getElementById(id);

  //n.style.overflow = 'hidden';

  a = { id: id, property: 'maxHeight', factor: 2, ticks: 10 };
  n.style[a.property] = '1px';

  animations[an_max++] = a;
}

function prev_div(n, name)
{
  p = n.previousSibling;
  while (p && p.tagName != 'DIV') {
    p = p.previousSibling;
  }

  if (p.id == name + '_TEMPLATE' || p.id == name + '_ANCHOR')
    return null;
  else
    return p;
}

function do_clone_region(name, iter_var, bAnim)
{
  d = document.getElementById(name + '_TEMPLATE');
  a = document.getElementById(name + '_ANCHOR');
  p = prev_div(a, name);

  cur = 1;
  if (p && p.id) {
    pid = p.id;
    if (pid.indexOf(name) == 0) {
      pid = pid.slice(name.length);
      i = pid.search(/[0-9]+/);
      if (i != -1) {
        cur = Number(pid.slice(i));
        cur++;
      }
    }
  }

  n = d.cloneNode(true);

  re = new RegExp(name + '{' + iter_var + '}', 'g');
  n.innerHTML = n.innerHTML.replace(re, name + cur);

  //capName = name.charAt(0).toUpperCase() + name.slice(1);
  //re = new RegExp(capName + ' N', 'g');
  //n.innerHTML = n.innerHTML.replace(re, capName + ' ' + cur);

  n.id = name + cur;
  n.style.display = '';

  a.parentNode.insertBefore(n, a);

  if (bAnim) {
    expand_region(n.id);
  }
}

function clone_region(name, iter_var)
{
  do_clone_region(name, iter_var, true);
}

function clone_region_noanim(name, iter_var)
{
  do_clone_region(name, iter_var, false);
}

function remove_region(name)
{
  a = document.getElementById(name + '_ANCHOR');
  p = prev_div(a, name);
  
  if (p && p.id) {
    if (p.id.indexOf(name) == 0) {
      p.parentNode.removeChild(p);
    }
  }
}

function disp(on, off)
{
  for (i in on) {
    n = document.getElementById(on[i]);
    n.style.display = 'block';
    expand_region(n.id);
  }

  for (i in off) {
    n = document.getElementById(off[i]);
    n.style.display = 'none';
  }
}
</script>
<link rel="stylesheet" href="matrix.css">
</head>
'''

HTML_mainprebody = '''<body>
<h1>LinGO Grammar Matrix</h1>
<h1>Matrix customization and download page</h1>
<h2>Version of 1/25/2008</h2>

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

<p>This file will be removed from the system in 24 hours.

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

HTML_prebody = '''<body onload="animate()">
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
def html_select(errors, name):
  asterisk = ''
  if name and errors.has_key(name):
    asterisk = '<span class="error">*</span>'

  return '%s<select name="%s">' % \
         (asterisk, name)


# Return an HTML <option> tag with the specified attributes and
# surrounding text
def html_option(errors, name, selected, html):
  sld = ''
  if selected:
    sld = ' selected'

  return '<option value="%s"%s>%s</option>' % \
         (name, sld, html)


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


######################################################################
# MatrixDefFile class
# This class and its methods are used to parse Matrix definition
# formatted files (currently just the file ./matrixdef), and based
# on the contents, to produce HTML pages and save choices files.

class MatrixDefFile:
  def_file = ""
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
          if ty in ['Text', 'Check', 'Radio', 'Select', '.']:
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

    f = open(self.def_file, 'r')
    line = f.readlines()
    f.close()

    # pass through the definition file once, augmenting the list of validation
    # errors with section names so that we can put red asterisks on the links
    # to the assocated sub-pages on the main page.
    for l in line:
      word = tokenize_def(l)
      if len(word) == 0:
        pass
      elif word[0] == 'Section':
        cur_sec = word[1]
      elif errors.has_key(word[1]) and not errors.has_key(cur_sec):
        errors[cur_sec] = 'error in section'

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
            (a, v) = c.split('=')
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

    print html_input(errors, 'hidden', 'customize', 'customize', False, '', '')
    print html_input(errors, 'radio', 'delivery', 'tgz', tgz_checked,
                     '<p class="submit">Archive type: ', ' .tar.gz')
    print html_input(errors, 'radio', 'delivery', 'zip', zip_checked,
                     ' ', ' .zip<br>')
    print html_input(errors, 'submit', '', 'Create Grammar', False, '', '</p>',
                     '', '', len(errors) > 0)
    print html_input(errors, 'button', '', 'Download Choices File', False,
                     '<p class="submit">', '</p>', '',
                     'window.location.href=\'' + choices_file + '\'')
    
    print HTML_postform
    print HTML_uploadpreform
    
    print html_input(errors, 'submit', '', 'Upload Choices File:', False,
                     '<p class="submit">', '')
    print html_input(errors, 'file', 'choices', '', False, '', '</p>', '20')
    
    print HTML_uploadpostform
    print HTML_postbody


  # Turn a list of lines containing matrix definitions into a string
  # containing HTML.
  def defs_to_html(self, lines, choices, errors, prefix):
    html = ''
    i = 0
    while i < len(lines):
      word = tokenize_def(lines[i])
      if len(word) == 0:
        pass
      elif word[0] == 'Label':
        html += '<p>'
        for w in word[1:]:
          html += w
        html += '</p>\n'
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
          word = tokenize_def(lines[i])
          (rval, rfrn, rbef, raft) = word[1:]
          checked = False
          if choices.is_set_full(vn) and choices.get_full(vn) == rval:
            checked = True
          html += html_input(errors, 'radio', vn, rval, checked,
                             rbef, raft) + '\n'
          i += 1
        html += af + '\n'
      elif word[0] == 'Select':
        (vn, fn, bf, af) = word[1:]
        vn = prefix + vn
        html += bf + '\n'
        html += html_select(errors, vn) + '\n'
        html += html_option(errors, '', False, '') + '\n'
        i += 1
        while lines[i] != '\n':
          word = tokenize_def(lines[i])
          (sval, sfrn, shtml) = word[1:]
          selected = False
          if choices.is_set_full(vn) and choices.get_full(vn) == sval:
            selected = True
          html += html_option(errors, sval, selected, shtml) + '\n'
          i += 1
        html += '</select>\n'
        html += af + '\n'
      elif word[0] == 'Text':
        (vn, fn, bf, af, sz) = word[1:]
        vn = prefix + vn
        value = choices.get_full(vn)
        html += html_input(errors, 'text', vn, value, False,
                           bf, af, sz) + '\n'
      elif word[0] == 'BeginIter':
        iter_orig = word[1]
        (iter_name, iter_var) = word[1].replace('}', '').split('{')
        label = word[2]
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
                                  prefix + iter_orig + '_')
        html += '</div>\n\n'

        choices.iter_begin(iter_name)
        while choices.iter_valid():
          pre = choices.iter_prefix()
          html += '<div id="' + pre[0:-1] + '">\n'  # trim the trailing '_'
          html += self.defs_to_html(lines[beg:end], choices, errors, pre)
          html += '</div>\n'
          choices.iter_next()
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

    cur_sec = ''
    cur_sec_friendly = ''
    cur_sec_begin = 0
    i = 0
    while i < len(lines):
      word = tokenize_def(lines[i])
      if len(word) == 0:
        pass
      elif word[0] == 'Section':
        if cur_sec and cur_sec == section:
          print '<title>' + cur_sec_friendly + '</title>'
          print HTML_posttitle
          print HTML_prebody
          print '<h2>' + cur_sec_friendly + '</h2>'
          print HTML_preform
          print html_input(errors, 'hidden', 'section', section,
                             False, '', '\n')
          print self.defs_to_html(lines[cur_sec_begin:i], choices, errors, '')
          break
        cur_sec = word[1]
        cur_sec_friendly = word[2]
        cur_sec_begin = i + 1
      i += 1

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
      elif word[0] in ['Check', 'Text', 'Radio', 'Select']:
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
        (iter_name, iter_var) = word[1].replace('}', '').split('{')
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

    f.close()

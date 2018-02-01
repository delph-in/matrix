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

import os
import cgitb
import glob
import re
import tarfile
import gzip
import zipfile
import cgi

from gmcs import choices
from gmcs.choices import ChoicesFile
from gmcs.utils import tokenize_def, get_name
from gmcs import generate
from gmcs.validate import ValidationMessage

from collections import defaultdict

######################################################################
# HTML blocks, used to create web pages

def dummy():
    pass # let emacs know the indentation is 2 spaces 

HTTP_header = 'Content-type: text/html;charset=UTF-8'

HTML_pretitle = '''<!doctype html>
<html>
<head><meta charset="utf-8"/>
'''

HTML_posttitle = '''<script type="text/javascript" src="web/matrix.js?cachebuster=103">
</script>

<script type="text/javascript">
// An array of strings, each of the form 'name:value|friendly value,...'
var features = [
%s
];

var verb_case_patterns = [
%s
];

var numbers = [
%s
];

var forms = [
%s
];

</script>

<link rel="stylesheet" href="web/matrix.css">
</head>
'''

HTML_jscache = '''<script type="text/javascript">
// A cache of choices from other subpages
var %s = [
%s
];
</script>'''

# toggle_visible provided by
#   http://blog.movalog.com/a/javascript-toggle-visibility/

HTML_toggle_visible_js = '''<script type="text/javascript">
<!--
    function toggle_visible(id) {
       var e = document.getElementById(id);
       if(e.style.display == 'block')
          e.style.display = 'none';
       else
          e.style.display = 'block';
    }
//-->
</script>
'''

HTML_mainprebody = '''<body onload="animate()">
<h1>LinGO Grammar Matrix</h1>
<h1 style="display:inline">Matrix customization and download page</h1>
<span class="tt">[<a href="http://moin.delph-in.net/MatrixDocTop" target="matrixdoc">documentation</a>]</span>
<h2>Version of %s</h2>

<p>The <a href="http://www.delph-in.net/matrix">LinGO Grammar
Matrix</a> is developed at the University of Washington in the context
of the <a href="http://www.delph-in.net/">DELPH-IN Consortium</a>,
by <a  href="http://faculty.washington.edu/ebender">Emily M. Bender</a>
and colleagues.  This material is based up work supported by
the National Science Foundation under Grant No. BCS-0644097.
Additional support for Grammar Matrix development came from a gift
to the Turing Center from the Utilika Foundation.  Any opinions,
findings, and conclusions or recommendations expressed in this
material are those of the author(s) and do not necessarily
reflect the views of the National Science Foundation.
The Grammar Matrix customization system is hosted by the University
of Washington.

<p>[<a href="http://www.washington.edu/online/terms">University of
Washington Website Terms and Conditions of Use</a>]<br>
[<a href="http://www.washington.edu/online/privacy">University of
Washington Online Privacy Statement</a>]


<p>Publications reporting on work based on grammars derived from this
system should cite <a
href="http://faculty.washington.edu/ebender/papers/gee03.pdf">Bender,
Flickinger and Oepen 2002</a> <a
href="http://faculty.washington.edu/ebender/bibtex/BenFliOep02.bib.txt">[.bib]</a>
and <a href="http://www.springerlink.com/content/767771152h331808/">Bender et al 2010</a> <a href="http://faculty.washington.edu/ebender/bibtex/BenDreFokPouSal10.bib.txt">[.bib]</a>.  Further publications from the project are available
on the <a href="http://www.delph-in.net/matrix/index.html#pubs">project website</a>.

<p>Filling out this form will produce a starter grammar for a natural
language, consisting of a language-independent core and customized
support for the phenomena you describe below.  Note that this grammar
fragment will only treat matrix (main) clauses. Be advised that this
system is highly experimental.  We are interested in your feedback.
If you have questions or comments, please email Emily Bender at:
<strong>ebender at u dot washington dot edu</strong>.

<p>[<a href="http://www.delph-in.net/matrix/">Back to Matrix main
page</a>]

<p>NOTE: Throughout the questionnaire, questions or subpages that lack
a required answer or contain an incorrect answer are marked with a red
asterisk:

<span class="error" title="This asterisk is an exception. It has a
tooltip even though it does not mark an error.">*</span>.

Questions or subpages that contain answers that might be problematic,
but are not outright incorrect, are marked with a red question mark:

<span class="error" title="This question mark is similarly
exceptional. It has a tooltip even though it does not mark a
warning.">?</span>.

Hovering the mouse cursor over a red asterisk or question mark will
show a tooltip describing the error. Clicking on a red asterisk or
question mark that is on the main page will link to the error or warning
on the appropriate subpage.</p>

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

HTML_sentencesprebody = '''
<script type="text/javascript">
<!--
function toggle_visibility(ids) {
  for(i in ids) {
    var e = document.getElementById(ids[i]);
    if(e.style.display == 'none')
      e.style.display = 'block';
    else
  e.style.display = 'none';
  }
}
//-->
</script>

<h3>Generated Sentences</h3>
Click on a sentence to view its parse tree and the mrs associated to that parse.  Capitalized lexical entries indicate that the lexical entry has undergone a spelling-changing lexical rule<br><br>
'''

HTML_sentencespostbody = '''
<hr>
<a href="matrix.cgi">Back to form</a><br>
<a href="http://www.delph-in.net/matrix/">Back to Matrix main page</a><br>
<a href="http://www.delph-in.net/lkb">To the LKB page</a>
'''

HTML_prebody = '''<body onload="animate(); focus_all_fields(); multi_init(); fill_hidden_errors(); scalenav();">
'''

HTML_prebody_sn = '''<body onload="animate(); focus_all_fields(); multi_init(); fill_hidden_errors();display_neg_form();scalenav();">'''

HTML_method = 'post'

HTML_preform = '<form action="matrix.cgi" method="' + HTML_method + '" enctype="multipart/form-data" name="choices_form">'

HTML_postform = '</form>'

HTML_uploadpreform = '''
<form action="matrix.cgi" method="post" enctype="multipart/form-data"
 name="choices_form">
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

def html_mark(mark, vm):
    if vm.href:
        if mark == '#':
            return '<a href="%s" style="text-decoration:none"><span class="info" title="%s">%s</span></a>' % \
                   (vm.href, vm.message.replace('"', '&quot;'), mark)
        else:
            return '<a href="%s" style="text-decoration:none"><span class="error" title="%s">%s</span></a>' % \
                   (vm.href, vm.message.replace('"', '&quot;'), mark)
    else:
        if mark == '#':
            return '<a href="%s" style="text-decoration:none"><span class="info" title="%s">%s</span></a>' % \
                   (vm.name, vm.message.replace('"', '&quot;'), mark)
        else:
            return '<a name="%s" style="text-decoration:none"><span class="error" title="%s">%s</span></a>' % \
                   (vm.name, vm.message.replace('"', '&quot;'), mark)

def html_error_mark(vm):
    return html_mark('*', vm)

def html_warning_mark(vm):
    return html_mark('?', vm)

def html_info_mark(vm):
    return html_mark('#', vm)

# Return an HTML <input> tag with the specified attributes and
# surrounding text
# TJT 2014-05-07 Adding randid to html_input to pass random number
# matching radio buttons to their labels along
# TJT 2014-09-05 Getting rid of randid to wrap entire radio option in label
def html_input(vr, type, name, value, checked, before = '', after = '',
               size = '', onclick = '', disabled = False, onchange = ''):
    chkd = ''
    if checked:
        chkd = ' checked="checked"'

    if size:
        if 'x' in size:
            size = ' cols="%s" rows="%s"' % tuple(size.split('x'))
        else:
            size = ' size="' + size + '"'

    if onclick:
        onclick = ' onclick="' + onclick + '"'

    if onchange:
        onchange = ' onchange="' + onchange + '"'

    dsabld = ''
    if disabled:
        dsabld = ' disabled="disabled"'

    mark = ''
    if type != 'radio':
        if vr:
            if name in vr.errors:
                mark = html_error_mark(vr.errors[name])
            elif name in vr.warnings:
                mark = html_warning_mark(vr.warnings[name])
            if name in vr.infos:
                mark = html_info_mark(vr.infos[name])
        if name in vr.infos:
            mark = html_info_mark(vr.infos[name])

    if type == 'textarea':
        value = value.replace('\\n','\n')
        return '%s%s<TextArea name="%s"%s>%s</TextArea>%s' % \
               (before, mark, name, size, value, after)

    else:
        if value:
            value = ' value="' + value + '"'
        if name:
            name = ' name="' + name + '"'
        output = '%s%s<input type="%s" %s%s%s%s%s%s%s>%s' % \
                 (before, mark, type, name, value, chkd, size, dsabld,
                  onclick, onchange, after)
        # TJT 2014-09-05: If checkbox
        if type in ('checkbox','radio'):
            return "<label>%s</label>" % output
        return output


# Return an HTML <select> tag with the specified name
# TJT 2014-08-26: Adding onchange
def html_select(vr, name, multi=False, onfocus='', onchange=''):
    mark = ''
    if name in vr.errors:
        mark = html_error_mark(vr.errors[name])
    elif name in vr.warnings:
        mark = html_warning_mark(vr.warnings[name])
    if name in vr.infos:
        mark = html_info_mark(vr.infos[name])

    multi_attr = ''
    if multi:
        multi_attr = ' class="multi"  multiple="multiple" '

    if onfocus:
        onfocus = ' onfocus="' + onfocus + '"'

    if onchange:
        onchange = ' onchange="' + onchange + '"'

    return '%s<select name="%s"%s%s%s>' % \
           (mark, name, multi_attr, onfocus, onchange)


# Return an HTML <option> tag with the specified attributes and
# surrounding text
def html_option(vr, name, selected, html, temp=False, strike=False):
    if selected:
        selected = ' selected'
    else: selected = ''

    # TJT 2014-03-19: adding disabled option for always-disabled "future work"
    # TODO: javascript cuts this out, need to change javascript
    if strike:
        strike = ' disabled'
        html = '<p style="display:inline;color:#ADADAD"> %s</p>' % html
    else: strike = ''

    if temp:
        temp = ' class="temp"'
    else: temp = ''

    return '<option value="%s"%s%s%s>%s</option>' % \
           (name, selected, temp, strike, html)


def html_delbutton(id):
    """
    return the HTML for an iterator delete button that will delete
    iterator "id"
    """
    # Various Unicode exxes:
    # xD7:   Multiplication Sign (old)
    # x2169: Roman Numeral Ten
    # x2179: Small Roman Numeral Ten (good)
    # x2573: Box Drawings Light Diagonal Cross
    # x2613: Saltire
    # x2715: Multiplication X
    # x2716: Heavy Multiplication X
    # x2717: Ballot X
    # x2718: Heavy Ballot X
    return '<input type="button" class="delbutton" ' + \
           'value="X" name="" title="Delete" ' + \
           'onclick="remove_element(\'' + id + '\')">\n'
    # TJT 2014-5-27 Regular capital X looks the best + most compliant
    #'value="&#x2169;" name="" title="Delete" ' + \


# given a list of lines of text, some of which may contain
# unterminated double-quoted strings, merge some lines as necessary so
# that every quoted string is contained on a single line, and return
# the merged list
def merge_quoted_strings(line):
    i = 0
    while i < len(line):
        j = 0
        in_quotes = False
        while j < len(line[i]):
            if line[i][j] == '"' and (j == 0 or line[i][j-1] != '\\'):
                in_quotes = not in_quotes
            j += 1

        # if we reach the end of a line inside a quoted string, merge with
        # the next line and reprocess the newly-merged line
        if in_quotes:
            line[i] += line[i+1] # crash here implies an unbalanced '"'
            del line[i+1]
        else:
            i += 1

    return line

######################################################################
# Archive helper functions
#   make_tgz(dir) and make_zip(dir) create an archive called
#   dir.(tar.gz|zip) that contains the contents of dir

def make_tgz(dir):

    # ERB First get rid of existing file because gzip won't
    # overwrite existing .tgz meaning you can only customize
    # grammar once per session.

    if os.path.exists('matrix.tar.gz'):
        os.remove('matrix.tar.gz')

    archive = dir + '.tar'
    t = tarfile.open(archive, 'w')
    t.add(dir)
    t.close()

    g = gzip.open(archive + '.gz', 'wb')
    f = open(archive, 'rb')
    g.write(f.read())
    f.close()
    g.close()

    os.remove(archive)


def add_zip_files(z, dir):
    files = os.listdir(dir)
    for f in files:
        cur = os.path.join(dir, f)
        if os.path.isdir(cur):
            add_zip_files(z, cur)
        else:
            z.write(cur, cur)


def make_zip(dir):
    z = zipfile.ZipFile(dir + '.zip', 'w')
    add_zip_files(z, dir)
    z.close()


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

# From a list of triples of strings [string1, string2, ...], return
# a string containing a JavaScript-formatted list of strings of the
# form 'string1:string2:string3'. This is used to convey features,
# values and category (category of feature).
def js_array3(list):
    val = ''
    for l in list:
        val += '\'' + l[0] + ':' + l[1] + ':' + l[3] + '\',\n'
    val = val[:-2]  # trim off the last ,\n
    return val

# From a list of triples of strings [string1, string2, ...], return
# a string containing a JavaScript-formatted list of strings of the
# form 'string1:string2:string3:string4'. This is used to convey features,
# values, category (category of feature), a flag feature 'customized'.
def js_array4(list):
    val = ''
    for l in list:
        val += '\'' + l[0] + ':' + l[1] + ':' + l[3] + ':' + l[4] + '\',\n'
    val = val[:-2]  # trim off the last ,\n
    return val

######################################################################
# MatrixDefFile class
# This class and its methods are used to parse Matrix definition
# formatted files (currently just the file ./matrixdef), and based
# on the contents, to produce HTML pages and save choices files.

class MatrixDefFile:
    # links between names and friendly names for
    # use in links on html navigation menu
    sections = { 'general':'General Information',
                 'word-order':'Word Order', 'number':'Number',
                 'person':'Person', 'gender':'Gender', 'case':'Case', 
                 'adnom-poss':'Adnominal Possession', 'direct-inverse':'Direct-inverse', 
                 'tense-aspect-mood':'Tense, Aspect and Mood', 'evidentials': 'Evidentials',
                 'other-features':'Other Features', 'sentential-negation':'Sentential Negation',
                 'coordination':'Coordination', 'matrix-yes-no':'Matrix Yes/No Questions',
                 'info-str':'Information Structure',
                 'arg-opt':'Argument Optionality',
                 'clausal-comp':'Clausal Complements',
                 'nominalclause':'Nominalized Clauses',
                 'clausalmods':'Clausal Modifiers', 'lexicon':'Lexicon',
                 'morphology':'Morphology','toolbox-import':'Toolbox Import',
                 'test-sentences':'Test Sentences','gen-options':'TbG Options',
                 'ToolboxLexicon':'Toolbox Lexicon'}
    # used to link section names to their documentation
    # page name in the delph-in wiki
    doclinks = { 'general':'GeneralInfo',
                 'word-order':'WordOrder', 'number':'Number',
                 'person':'Person', 'gender':'Gender', 'case':'Case',
                 'adnom-poss':'AdnominalPossession',
                 'direct-inverse':'DirectInverse', 'tense-aspect-mood':'TenseAspectMood', 
                 'evidentials': 'Evidentials',
                 'other-features':'OtherFeatures', 'sentential-negation':'SententialNegation',
                 'coordination':'Coordination', 'matrix-yes-no':'YesNoQ',
                 'info-str':'InformationStructure',
                 'arg-opt':'ArgumentOptionality',
                 'clausal-comp':'ClausalComplements',
                 'nominalclause':'Nominalized Clauses',
                 'clausalmods':'Clausal Modifiers', 'lexicon':'Lexicon',
                 'morphology':'Morphology','toolbox-import':'ImportToolboxLexicon',
                 'test-sentences':'TestSentences','gen-options':'TestByGeneration',
                 'ToolboxLexicon':'ImportToolboxLexicon'}
    def_file = ''
    v2f = {}
    f2v = {}

    def __init__(self, def_file):
        self.def_file = def_file
        f = open(self.def_file)
        self.def_lines = merge_quoted_strings(f.readlines())
        f.close()
        self.make_name_map()

    ######################################################################
    # Variable/friendly name mapping

    # initialize the v2f and f2v dicts
    def make_name_map(self):
        for l in self.def_lines:
            l = l.strip()
            if len(l):
                w = tokenize_def(l)
                if len(w) >= 3:
                    ty = w[0]
                    vn = w[1]
                    fn = w[2]
                    if ty in ('Text', 'TextArea', 'Check', 'Radio',
                              'Select', 'MultiSelect', '.'):
                        self.v2f[vn] = fn
                        self.f2v[fn] = vn

    # return the friendly name for a variable, or the variable if none
    # is defined
    def f(self, v):
        if v in self.v2f:
            return self.v2f[v]
        else:
            return v

    # return the variable for a friendly name, or the friendly name if
    # none is defined
    def v(self, f):
        if f in self.f2v:
            return self.f2v[f]
        else:
            return f


    ######################################################################
    # Utility methods

    # TJT 2014-08-28: Adding method to find if some choice exists
    # in order to enable skipping UI elements based on this choice
    def check_choice_switch(self, switch, choices):
        """
        This method checks to see if a given string or regex (the switch)
        is in a given choices file. This method is used to see if a given
        UI element should be skipped. As such, the boolean return value
        is a little bit backwards. The function returns True if the switch
        is not found, and false if it is found.
        """
        if not switch:
            return True
        # Switch can have multiple variable names
        switches = switch.strip('"').split('|')
        # Switch contains choice name and value
        # split on the rightmost "=" just in case...
        switches = [switch.rsplit('=',1) if "=" in switch else switch
                    for switch in switches]
        # Default to true
        #skip_it = {switch[0] if isinstance(switch, list) else switch: True
        #           for switch in switches}
        # TJT: 12-19-14 changing this to loop instead of comprehension
        skip_it = {}
        for switch in switches:
            if isinstance(switch, list):
                skip_it[switch[0]] = True
            else:
                skip_it[switch] = True
        for instance in switches:
            if isinstance(instance, list):
                switch = instance[0].strip()
                values = instance[1].strip().strip("()").split(',') # Get multiple values
            else:
                switch = instance
                values = False # set default
            results = choices.get_regex(switch)
            if results:
                # Found a match
                if not values:
                    # If no value, then switching on a key,
                    # which was found, so don't skip it!
                    skip_it[switch] = False
                else:
                    for item in results:
                        if item[1] in values:
                            skip_it[switch] = False
                            break
        # if all false, don't skip it
        # else, skip it
        return any(skip_it.values())

    ######################################################################
    # HTML output methods

    # Create and print the main matrix page.  The argument is a cookie
    # that determines where to look for the choices file.
    def main_page(self, cookie, vr):
        print HTTP_header
        print 'Set-cookie: session=' + cookie + '\n'
        print HTML_pretitle
        print '<title>The Matrix</title>'
        print HTML_posttitle % ('', '', '','')

        try:
            f = open('datestamp', 'r')
            datestamp = f.readlines()[0].strip()
            f.close()
        except:
            datestamp = "[date unknown]"

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

        # pass through the definition file once, augmenting the list of validation
        # results with section names so that we can put red asterisks on the links
        # to the assocated sub-pages on the main page.
        prefix = ''
        for l in self.def_lines:
            word = tokenize_def(l)
            if len(word) < 2 or word[0][0] == '#':
                pass
            elif word[0] == 'Section':
                cur_sec = word[1]
            elif word[0] == 'BeginIter':
                if prefix:
                    prefix += '_'
                prefix += re.sub('\\{.*\\}', '[0-9]+', word[1])
            elif word[0] == 'EndIter':
                prefix = re.sub('_?' + word[1] + '[^_]*$', '', prefix)
            elif not (word[0] == 'Label' and len(word) < 3):
                pat = '^' + prefix
                if prefix:
                    pat += '_'
                pat += word[1] + '$'
                for k in vr.errors.keys():
                    if re.search(pat, k):
                        anchor = "matrix.cgi?subpage="+cur_sec+"#"+k
                        vr.err(cur_sec, "This section contains one or more errors. \nClicking this error will link to the error on the subpage.", anchor+"_error", False)
                        break
                for k in vr.warnings.keys():
                    if re.search(pat, k):
                        anchor = "matrix.cgi?subpage="+cur_sec+"#"+k
                        vr.warn(cur_sec, "This section contains one or more warnings. \nClicking this warning will link to the warning on the subpage.", anchor+"_warning", False)
                        break

        # now pass through again to actually emit the page
        for l in self.def_lines:
            word = tokenize_def(l)
            if len(word) == 0:
                pass
            elif word[0] == 'Section' and (len(word) != 4 or word[3] != '0'):
                print '<div class="section"><span id="' + word[1] + 'button" ' + \
                      'onclick="toggle_display(\'' + \
                      word[1] + '\',\'' + word[1] + 'button\')"' + \
                      '>&#9658;</span> '
                if word[1] in vr.errors:
                    print html_error_mark(vr.errors[word[1]])
                elif word[1] in vr.warnings:
                    print html_warning_mark(vr.warnings[word[1]])
                print '<a href="matrix.cgi?subpage=' + word[1] + '">' + \
                      word[2] + '</a>'
                print '<div class="values" id="' + word[1] + '" style="display:none">'
                cur_sec = ''
                printed_something = False
                for c in choice:
                    try:
                        c = c.strip()
                        if c:
                            (a, v) = c.split('=', 1)
                            if a == 'section':
                                cur_sec = v.strip()
                            elif cur_sec == word[1]:
                                print self.f(a) + ' = ' + self.f(v) + '<br>'
                                printed_something = True
                    except ValueError:
                        if cur_sec == word[1]:
                            print '(<i>Bad line in choices file: </i>"<tt>' + \
                                  c + '</tt>")<br>'
                            printed_something = True
                if not printed_something:
                    print '&nbsp;'
                print '</div></div>'

        print HTML_preform

        tgz_checked = True
        zip_checked = False

        # the buttons after the subpages
        print "<p>" # TJT 2014-09-18: Converting these radios to new set up
        print html_input(vr, 'hidden', 'customize', 'customize', False, '', '')
        print html_input(vr, 'radio', 'delivery', 'tgz', tgz_checked,
                         'Archive type: ', ' .tar.gz')
        print html_input(vr, 'radio', 'delivery', 'zip', zip_checked,
                         ' ', ' .zip<br>')
        print html_input(vr, 'submit', 'create_grammar_submit', 'Create Grammar',
                         False, '', '', '', '', vr.has_errors())

        print html_input(vr, 'submit', 'sentences', 'Test by Generation', False,
                         '', '', '', '', vr.has_errors())
        print "</p>"

        print '<hr>\n'

        # the button for downloading the choices file
        print '<p><a href="' + choices_file + '">View Choices File</a> ',
        print '(right-click to download)</p>'
        print HTML_postform

        # the FORM for uploading a choices file
        print HTML_uploadpreform
        print html_input(vr, 'submit', '', 'Upload Choices File:', False,
                         '<p>', '')
        print html_input(vr, 'file', 'choices', '', False, '', '</p>', '')
        print HTML_uploadpostform

        print '<hr>\n'

        # the list of sample choices files
        if os.path.exists('web/sample-choices'):
            print '<h3>Sample Grammars:</h3>\n' + \
                  '<p>Click a link below to have the questionnaire ' + \
                  'filled out automatically.</p>'
            print '<p>'

            globlist = glob.glob('web/sample-choices/*')
            linklist = {}

            for f in globlist:
                f = f.replace('\\', '/')
                lang = choices.get_choice('language',f) or '(empty questionnaire)'
                if lang == 'minimal-grammar':
                    lang = '(minimal grammar)'
                linklist[lang] = f

            for k in sorted(linklist.keys(), lambda x, y: cmp(x.lower(), y.lower())):
                print '<a href="matrix.cgi?choices=' + linklist[k] + '">' + \
                      k + '</a><br>\n'

            print '</p>'

        print '</div>'
        print HTML_postbody


    # Turn a list of lines containing matrix definitions into a string
    # containing HTML.
    def defs_to_html(self, lines, choices, vr, prefix, vars):

        html = ''

        http_cookie = os.getenv('HTTP_COOKIE')

        cookie = {}
        for c in http_cookie.split(';'):
            (name, value) = c.split('=', 1)
            cookie[name.strip()] = value

        i = 0
        while i < len(lines):
            word = tokenize_def(replace_vars(lines[i], vars))
            if len(word) == 0:
                pass
            elif word[0] == 'Cache':
                cache_name = word[1]
                items = choices.get_regex(word[2])
                if len(word) > 3:
                    items = [(k, v.get(word[3])) for (k, v) in items]
                html += HTML_jscache % (cache_name,
                                        '\n'.join(["\"" + ':'.join((v, k)) + "\","
                                                   for (k, v) in items]))
            elif word[0] == 'Label':
                if len(word) > 2:
                    if prefix + word[1] in vr.errors:
                        html += html_error_mark(vr.errors[prefix + word[1]])
                    elif prefix + word[1] in vr.warnings:
                        html += html_warning_mark(vr.warnings[prefix + word[1]])
                html += word[-1] + '\n'
            elif word[0] == 'Separator':
                html += '<hr>'
            elif word[0] == 'Check':
                if len(word) < 5: continue # TJT 2014-08-28: Syntax error!
                js = ''
                if len(word) >= 5:
                    (vn, fn, bf, af) = word[1:5]
                if len(word) >= 6:
                    js = word[5]
                # TJT 2014-08-28: Adding switch here to ignore entire check definition
                # based on some other choice
                skip_this_check = False
                if len(word) >= 7:
                    # matrixdef contains name of choice to switch on
                    switch = word[6]
                    skip_this_check = self.check_choice_switch(switch, choices)
                if not skip_this_check:
                    vn = prefix + vn
                    checked = choices.get(vn)
                    html += html_input(vr, 'checkbox', vn, '', checked,
                                       bf, af, onclick=js) + '\n'
            elif word[0] == 'Radio':
                # TJT 2014-03-19: Removed disabled flag that was on the entire radio
                # definition instead of on individual choices. See below
                if len(word) >= 5:
                    (vn, fn, bf, af) = word[1:5]
                else: continue # TJT 2014-08-28: Syntax error
                vn = prefix + vn
                # TJT 2014-08-28: Adding switch here to ignore entire radio definition
                # based on some other choice
                skip_this_radio = False
                if len(word) >= 6:
                    # matrixdef contains name of choice to switch on
                    switch = word[5]
                    skip_this_radio = self.check_choice_switch(switch, choices)
                # it's nicer to put vrs for radio buttons on the entire
                # collection of inputs, rather than one for each button
                if not skip_this_radio:
                    mark =''
                    if vn in vr.errors:
                        mark = html_error_mark(vr.errors[vn])
                    elif vn in vr.warnings:
                        mark = html_warning_mark(vr.warnings[vn])
                    if vn in vr.infos:
                        mark = html_info_mark(vr.infos[vn])

                    html += bf + mark + '\n'
                    i += 1
                    # TJT 2014-08-28: changing this to "startswith" to enforce syntax
                    while lines[i].strip().startswith('.'):
                        # Reset flags on each item
                        dis, js = '', ''
                        checked = False
                        word = tokenize_def(replace_vars(lines[i], vars))
                        # TJT 2014-05-07 Rearranged this logic (hoping for speed)
                        rval, rfrn, rbef, raft = word[1:5]
                        # Format choice name
                        if choices.get(vn) == rval: # If previously marked, mark as checked again
                            checked = True
                        if len(word) >= 6:
                            js = word[5]
                        if len(word) >= 7: # TJT 2014-03-19: option for disabled radio buttons
                            if word[6]: # If anything here...
                                dis = True
                        html += html_input(vr, 'radio', vn, rval, checked, rbef, raft,
                                           onclick=js, disabled=dis) + '\n'
                        i += 1
                    html += af + '\n'
                else:
                    # TJT 2014-08-28: skipping radio buttons,
                    # so skip the button definitions
                    while lines[i].strip().startswith('.'):
                        i += 1

            elif word[0] in ('Select', 'MultiSelect'):
                multi = (word[0] == 'MultiSelect')
                (vn, fn, bf, af) = word[1:5]

                onfocus, onchange = '', ''
                if len(word) > 5: onfocus = word[5]
                if len(word) > 6: onchange = word[6]

                vn = prefix + vn

                html += bf + '\n'

                fillers=[]

                # TJT 2014-08-26: Moving this out of while loop for efficiency's sake
                fillstrings = {'fillregex':'fill_regex(%(args)s)',
                               'fillnames':'fill_feature_names(%(args)s)',
                               'fillnames2':'fill_feature_names_only_customized(%(args)s)',
                               'fillvalues':'fill_feature_values(%(args)s)',
                               'fillverbpat':'fill_case_patterns(false)',
                               'fillnumbers':'fill_numbers()',
                               'fillcache':'fill_cache(%(args)s)',
                               'fillforms':'fill_forms()'}
                # look ahead and see if we have an auto-filled drop-down
                i += 1
                # OZ 2017-12-05 Adding check that i is not out of array bounds; managed to break it otherwise,
                # probably by calling it incorrectly, but the check should not hurt.
                while i < len(lines)-1 and lines[i].strip().startswith('fill'):
                    word = tokenize_def(replace_vars(lines[i], vars))
                    #note: possible cat values are "noun", "verb" or "both"
                    argstring = ','.join(['true' if a in ('n', 'l') else "'%s'" % x
                                          for (a, x) in [w.split('=') for w in word[1:]]])
                    fillers += [fillstrings[word[0]] % {'args':argstring}]
                    i += 1

                # Section variables:
                # SVAL: selected option variable name, e.g. "verb", "subj",
                #     i.e. the variable name of value previously selected
                # VN: variable name, e.g. "verb1_feat1_head"
                # self.f(VN): friendly name, e.g. "The verb", "The subject"
                # OFRN: option friendly name, e.g. "The verb", "The subject"
                # OVAL: option variable name, e.g. "verb", "subj"
                # OHTML: option HTML after, e.g. "the verb", some javascript, etc.

                # Get previously selected item
                # TJT 2014-05-08 always get selected value, even if not using fillers
                sval = choices.get(vn)
                if fillers:
                    fillcmd = "fill('%s', [].concat(%s));" % (vn, ','.join(fillers))
                    html += html_select(vr, vn, multi, fillcmd+onfocus, onchange=onchange) + '\n'
                    # Mark previously selected filled item as selected
                    # This is necessary because the value is not in the deffile
                    if sval:
                        html += html_option(vr, sval, True, self.f(sval), True) + '\n'
                else:
                    # If not using fillers, previously selected value
                    # will be marked during option processing below
                    html += html_select(vr, vn, multi, onchange=onchange) + '\n'
                # Add individual items, if applicable
                while lines[i].strip().startswith('.'):
                    sstrike = False # Reset variable
                    word = tokenize_def(replace_vars(lines[i], vars))
                    # select/multiselect options
                    oval, ofrn, ohtml = word[1:4]
                    # TJT 2014-03-19: add disabled option to allow for always-disabled
                    # If there's anything in this slot, disable option
                    if len(word) >= 5: sstrike = True
                    # Add option and mark "selected" if previously selected
                    html += html_option(vr, oval, (sval == oval), ofrn, strike=sstrike) + '\n'
                    i += 1
                # add empty option
                html += html_option(vr, '', False, '') + '\n'
                html += '</select>'
                html += af + '\n'

            elif word[0] in ('Text', 'TextArea'):
                if len(word) > 6:
                    (vn, fn, bf, af, sz, oc) = word[1:]
                else:
                    (vn, fn, bf, af, sz) = word[1:]
                    oc = ''
                # TJT 2014-08-27: Prepend auto onchange events (instead of assinging)
                if vn == "name":
                    oc = "fill_display_name('"+prefix[:-1]+"');" + oc
                # TJT 2014-08-26: Adding auto check radio button
                # on morphology page affixes
                elif vn == "orth":
                    # Previous line usually empty; find previous non-empty line
                    checker = int(i-1)
                    while lines[checker] == "\n":
                        checker -= 1
                    # If previous non-empty line a radio definition, add check radio
                    # button function to onChange
                    if lines[checker].strip().startswith("."):
                        oc = "check_radio_button('"+prefix[:-1]+"_inflecting', 'yes'); " + oc
                vn = prefix + vn
                value = choices.get(vn)
                html += html_input(vr, word[0].lower(), vn, value, False,
                                   bf, af, sz, onchange=oc) + '\n'
            elif word[0] == ('Hidden'):
                (vn, fn) = word[1:]
                value = choices.get(vn)
                html += html_input(vr, word[0].lower(), vn, value, False,
                                   '', '', 0) + '\n'
            elif word[0] == 'File':
                (vn, fn, bf, af) = word[1:]
                vn = prefix + vn
                value = choices.get(vn)
                html += html_input(vr, word[0].lower(), vn, value, False,
                                   bf, af) + '\n'
            elif word[0] == 'Button':
                (vn, bf, af, oc) = word[1:]
                html += html_input(vr, word[0].lower(), '', vn, False,
                                   bf, af, onclick=oc) + '\n'
            elif word[0] == 'BeginIter':
                iter_orig = word[1]
                (iter_name, iter_var) = word[1].replace('}', '').split('{', 1)
                label = word[2]
                show_hide = 0
                if len(word) > 3:
                    show_hide = int(word[3])
                iter_min = 0
                if len(word) > 4:
                    iter_min = int(word[4])
                    # TJT 2014-08-20: adding option to only do iter based on other choice
                skip_this_iter = False
                if len(word) > 5:
                    # matrixdef contains name of choice to switch on
                    switch = word[5]
                    skip_this_iter = self.check_choice_switch(switch, choices)

                i += 1

                # collect the lines that are between BeginIter and EndIter
                beg = i
                while True:
                    word = tokenize_def(lines[i])
                    if len(word) == 0:
                        pass
                    elif word[0] == 'EndIter' and word[1] == iter_name:
                        break
                    i += 1
                end = i

                # TJT 2014-08-20: if skipping iter, skip this whole section
                if not skip_this_iter:

                    # write out the (invisible) template for the iterator
                    # (this will be copied by JavaScript on the client side when
                    # the user clicks the "Add" button)
                    html += '<div class="iterator" style="display: none" id="' + \
                            prefix + iter_name + '_TEMPLATE">\n'
                    html += html_delbutton(prefix + iter_name + '{' + iter_var + '}')
                    html += '<div class="iterframe">'
                    html += self.defs_to_html(lines[beg:end],
                                              choices, vr,
                                              prefix + iter_orig + '_', vars)
                    html += '</div>\n'
                    html += '</div>\n\n'

                    # write out as many copies of the iterator as called for by
                    # the current choices file OR iter_min copies, whichever is
                    # greater
                    c = 0
                    iter_num = 0
                    chlist = [x for x in choices.get(prefix + iter_name) if x]
                    while (chlist and c < len(chlist)) or c < iter_min:
                        show_name = ""
                        if c < len(chlist):
                            iter_num = str(chlist[c].iter_num())
                            show_name = chlist[c]["name"]
                        else:
                            iter_num = str(int(iter_num)+1)
                        new_prefix = prefix + iter_name + iter_num + '_'
                        vars[iter_var] = iter_num

                        # new_prefix[:-1] trims the trailing '_'

                        # the show/hide button gets placed before each iterator
                        # as long as it's not a stem/feature/forbid/require/lri iterator
                        if show_hide:
                            #          if new_prefix[:-1].find('feat')==-1 and \
                            #                  new_prefix[:-1].find('stem')==-1 and \
                            #                  new_prefix[:-1].find('require')==-1 and \
                            #                  new_prefix[:-1].find('forbid')==-1 and \
                            #                  new_prefix[:-1].find('lri')==-1:
                            #            html += ''
                            if show_name:
                                name = show_name+" ("+new_prefix[:-1]+")"
                            else:
                                name = new_prefix[:-1]
                            html += '<span id="'+new_prefix[:-1]+'_errors" class="error" '
                            if cookie.get(new_prefix[:-1]+'button','block') != 'none':
                                html += 'style="display: none"'
                            html += '></span>'+'<a id="' + new_prefix[:-1] + 'button" ' + \
                                    'onclick="toggle_display_lex(\'' + \
                                    new_prefix[:-1] + '\',\'' + new_prefix[:-1] + 'button\')">'

                            if cookie.get(new_prefix[:-1]+'button','block') == 'none':
                                html += '&#9658; '+name+'<br /></a>'
                            else:
                                html += '&#9660; '+name+'</a>'
                        if cookie.get(new_prefix[:-1], 'block') == 'block':
                            html += '<div class="iterator" id="' + new_prefix[:-1] + '">\n'
                        else:
                            html += '<div class="iterator" style="display: none" id="' + new_prefix[:-1] + '">\n'
                        html += html_delbutton(new_prefix[:-1])
                        html += '<div class="iterframe">'
                        html += self.defs_to_html(lines[beg:end],
                                                  choices, vr,
                                                  new_prefix, vars)
                        html += '</div>\n'
                        html += '</div>\n'

                        del vars[iter_var]
                        c += 1

                    # write out the "anchor" marking the end of the iterator and
                    # the "Add" button
                    html += '<div class="anchor" id="' + \
                            prefix + iter_name + '_ANCHOR"></div>\n<p>'
                    # add any iterator-nonspecific errors here
                    if prefix + iter_name in vr.errors:
                        html += html_error_mark(vr.errors[prefix + iter_name])
                    elif prefix + iter_name in vr.warnings:
                        html += html_warning_mark(vr.warnings[prefix + iter_name])
                    elif prefix + iter_name in vr.infos:
                        html += html_info_mark(vr.infos[prefix + iter_name])
                    # finally add the button
                    html += '<input type="button" name="" ' + \
                            'value="Add ' + label + '" ' + \
                            'onclick="clone_region(\'' + \
                            prefix + iter_name + '\', \'' + \
                            iter_var + '\','
                    if show_hide:
                        html += 'true'
                    else:
                        html += 'false'
                    html += ')">'

            i += 1

        return html

    def verification(self):
        # Note, In previous parts of this site the html bits are configured outside of the
        # function and pipped in as global variables. I don't like this. So i'm confining variables
        # locally. But I don't want to leave things messy, so I've divided it into sections to keep
        # things more "clean" looking.
        ### Setup ###
        verify_form = cgi.FieldStorage()
        HTML_google_api = "<script src='https://www.google.com/recaptcha/api.js'></script> \n"
        HTML_verify_form_prebody = """
<html>
  <head>
    <title>Grammar Matrix: Session Verification Page</title>
     <script src="https://www.google.com/recaptcha/api.js" async defer></script>
     <script>
       function onSubmit(token) {
         document.getElementById("verify-form").submit();
       }
     </script>
  </head>
"""
        HTML_verify_form_body = """
<body>
	We were unable to detect a valid session for you. In order to create a new session,<br>
	please use the verify button below to confirm you are not a bot. Once this is done you will be<br>
	redirected to the main page where you can begin using the grammar matrix.<br><br>
</body>
"""
        HTML_verify_form_postbody = """
      <form id='verify-form' action="matrix.cgi" method="POST">
      <button class="g-recaptcha" data-sitekey="6LfEeisUAAAAAGdbbNlfjxKjkRxcSWhSovyq9oik" data-callback='onSubmit'>Verify</button>
      <br/>
    </form>
</html>


   """

        ### Execution ###
        print HTTP_header + '\n'
        print HTML_pretitle
        print HTML_verify_form_prebody
        print HTML_verify_form_body
        print HTML_verify_form_postbody
        print HTML_postbody

    # Create and print the matrix subpage for the specified section
    # based on the arguments, which are the name of the section and
    # a cookie that determines where to look for the choices file
    def sub_page(self, section, cookie, vr):
        print HTTP_header + '\n'
        print HTML_pretitle
        if section == 'lexicon':
            print "<script type='text/javascript' src='web/draw.js'></script>"

        choices_file = 'sessions/' + cookie + '/choices'
        choices = ChoicesFile(choices_file)

        section_begin = -1
        section_end = -1
        section_friendly = ''


        i = 0
        while i < len(self.def_lines):
            word = tokenize_def(self.def_lines[i])
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
                  (js_array4(choices.features()),
                   js_array([c for c in choices.patterns() if not c[2]]),
                   js_array([n for n in choices.numbers()]),
                   js_array([n for n in choices.forms()]))

            if section == 'sentential-negation':
                print HTML_prebody_sn
            else:
                print HTML_prebody

            print '<h2 style="display:inline">' + section_friendly + '</h2>'
            doclink = '<a href="http://moin.delph-in.net/MatrixDoc/' + \
                      self.doclinks[section] + '" target="matrixdoc">documentation</a>'
            print '<span class="tt">['+doclink+']</span><br />'


            print '<div id="navmenu"><br />'
            # pass through the definition file once, augmenting the list of validation
            # results with section names so that we can put red asterisks on the links
            # to the assocated sub-pages on the nav menu.
            prefix = ''
            sec_links = []
            n = -1
            printed = False
            for l in self.def_lines:
                word = tokenize_def(l)
                cur_sec = ''
                if len(word) < 2 or word[0][0] == '#':
                    pass
                elif len(word) == 4 and word[3] == '0':
                    # don't print links to sections that are marked 0
                    pass
                elif word[0] == 'Section':
                    printed = False
                    cur_sec = word[1]
                    # disable the link if this is the page we're on
                    if cur_sec == section:
                        sec_links.append('</span><span class="navlinks">'+self.sections[cur_sec]+'</span>')
                    else:
                        sec_links.append('</span><a class="navlinks" href="#" onclick="submit_go(\''+cur_sec+'\')">'+self.sections[cur_sec]+'</a>')
                    n+=1
                elif word[0] == 'BeginIter':
                    if prefix:
                        prefix += '_'
                    prefix += re.sub('\\{.*\\}', '[0-9]+', word[1])
                elif word[0] == 'EndIter':
                    prefix = re.sub('_?' + word[1] + '[^_]*$', '', prefix)
                elif not (word[0] == 'Label' and len(word) < 3):
                    pat = '^' + prefix
                    if prefix:
                        pat += '_'
                    pat += word[1] + '$'
                    if not printed:
                        for k in vr.errors.keys():
                            if re.search(pat, k):
                                sec_links[n] = '*'+sec_links[n]
                                printed = True
                                break
                    if not printed:
                        for k in vr.warnings.keys():
                            if re.search(pat, k):
                                sec_links[n] = '?'+sec_links[n]
                                printed = True
                                break

            print '<a href="." onclick="submit_main()" class="navleft">Main page</a><br />'
            print '<hr />'
            for l in sec_links:
                print '<span style="color:#ff0000;" class="navleft">'+l+'<br />'

            print '<hr />'
            print '<a href="' + choices_file + '" class="navleft">Choices file</a><br /><div class="navleft" style="margin-bottom:0;padding-bottom:0">(right-click to download)</div>'
            print '<a href="#stay" onclick="document.forms[0].submit()" class="navleft">Save &amp; stay</a><br />'
            # TJT 2014-05-28: Not sure why the following doesn't work -- need to do more investigation
            #print '<a href="?subpage=%s" onclick="document.forms[0].submit()" class="navleft">Save &amp; stay</a><br />' % section
            print '<a href="#clear" onclick="clear_form()" class="navleft">Clear current subpage</a><br />'

            ## if there are errors, then we print the links in red and
            ## unclickable
            if not vr.has_errors() == 0:
                print '<span class="navleft">Create grammar:'
                print html_info_mark(
                    ValidationMessage('','Resolve validation errors to enable '+
                                      'grammar customization.',''))
                print '</span><br />'
                print '<span class="navleft" style="padding-left:15px">tgz</span>, <span class="navleft">zip</span>'
            else:
                print '<span class="navleft">Create grammar:</span><br />'
                print '<a href="#" onclick="nav_customize(\'tgz\')" class="navleft" style="padding-left:15px">tgz</a>, <a href="#customize" onclick="nav_customize(\'zip\')" class="navleft">zip</a>'
            print '</div>'


            print '<div id="form_holder">'
            print HTML_preform
            print html_input(vr, 'hidden', 'section', section, False, '', '\n')
            print html_input(vr, 'hidden', 'subpage', section, False, '', '\n')
            print self.defs_to_html(self.def_lines[section_begin:section_end],
                                    choices, vr,
                                    prefix='', vars={})

        # these buttons are now in the navmenu
        #    print html_input(vr, 'button', '', 'Submit', False, '<p>', '', onclick='submit_main()')
        #    print html_input(vr, 'submit', '', 'Save', False)
        #    print html_input(vr, 'button', '', 'Clear', False, '', '</p>', '',
        #                     'clear_form()')

        print HTML_postform
        print '</div>'
        print HTML_postbody


    # Create and print the "download your matrix here" page for the
    # customized matrix in the directory specified by session_path
    def custom_page(self, session_path, grammar_path, arch_type):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>Matrix Customized</title>'
        # we don't want the contents of the archive to be something like
        # sessions/7149/..., so we remove session_path from grammar_path
        grammar_dir = grammar_path.replace(session_path, '').lstrip('/')
        if arch_type == 'tgz':
            arch_file = grammar_dir + '.tar.gz'
        else:
            arch_file = grammar_dir + '.zip'
        cwd = os.getcwd()
        os.chdir(session_path)
        if arch_type == 'tgz':
            make_tgz(grammar_dir)
        else:
            make_zip(grammar_dir)
        os.chdir(cwd)
        print HTML_customprebody % (os.path.join(session_path, arch_file))
        print HTML_postbody

    # Generate and print sample sentences from the customized grammar
    def sentences_page(self, session_path, grammar_dir, session):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>Matrix Sample Sentences</title>'
        print HTML_posttitle
        delphin_dir = os.path.join(os.getcwd(), 'delphin')
        sentences = generate.get_sentences(grammar_dir, delphin_dir, session)
        print HTML_sentencesprebody
        for i in range(len(sentences)):
            long = False
            print "<b>" + sentences[i][0][0][4:]+"</b> " + sentences[i][0][2] + ", with predication: " + ", ".join(sentences[i][0][1].values()) +"<br>"
            if len(sentences[i][1]) > 0 and sentences[i][1][0] == '#EDGE-ERROR#':
                print 'This grammar combined with this input semantics results in too large of a seach space<br>'
            elif len(sentences[i][1]) > 0 and sentences[i][1][0] == '#NO-SENTENCES#':
                print 'This combination of verb, pattern, and feature specification did not result in any generated sentences with the nouns that the system chose.<br>'
                print HTML_preform
                print '<input type="hidden" name="verbpred" value="%s">' % sentences[i][0][1]
                print '<input type="hidden" name="template" value="%s">' % sentences[i][0][3]
                print '<input type="hidden" name="grammar" value="%s">' % grammar_dir
                print '<input type="submit" name="" value="Try this verb and pattern with all possible nouns">'
                print HTML_postform
            else:
                for j in range(len(sentences[i][1])):
                    if j == 10:
                        print '<div id="%s_extra" style=display:none;>' % (i+1)
                        long = True
                    print '<div onclick=toggle_visibility(["%s_%s_parsemrs"])>%s. %s</div>' % (i+1,j+1,j+1,sentences[i][1][j])
                    print '<div id="%s_%s_parsemrs" style=display:none;>' % (i+1,j+1)
                    print '&nbsp&nbsp Parse tree:<br>' + sentences[i][2][j]
                    print '&nbsp&nbsp MRS:<br>' + sentences[i][3][j]
                    print '</div>'
                if long:
                    print '</div>'
                    print '<div id="%s_dots" style=display:block;>...</div>' % (i+1)
                    print '<input type="button" id="%s_show" value="Show Remainder" onclick=toggle_visibility(["%s_extra","%s_dots","%s_show","%s_hide"]) style=display:block;>' % (i+1,i+1,i+1,i+1,i+1)
                    print '<input type="button" id="%s_hide" value="Hide Remainder" onclick=toggle_visibility(["%s_extra","%s_dots","%s_show","%s_hide"]) style=display:none;>' % (i+1,i+1,i+1,i+1,i+1)

                print '<br>'
                print HTML_preform
                print '<input type="hidden" name="verbpred" value="%s">' % sentences[i][0][1]
                print '<input type="hidden" name="template" value="%s">' % sentences[i][0][3]
                print '<input type="hidden" name="grammar" value="%s">' % grammar_dir
                print '<input type="submit" name="" value="More sentences with this verb and pattern">'
                print HTML_postform
            print '<br>'
        print HTML_sentencespostbody
        print HTML_postbody

    # Display page with additional sentences
    def more_sentences_page(self, session_path, grammar_dir, verbpred, template_file, session):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>More Sentences</title>'
        print HTML_sentencesprebody
        delphin_dir = os.path.join(os.getcwd(), 'delphin')
        sentences,trees,mrss = generate.get_additional_sentences(grammar_dir,
                                                                 delphin_dir,
                                                                 verbpred,
                                                                 template_file,
                                                                 session)
        if len(sentences) > 0:
            if sentences[0] == "#EDGE-ERROR#":
                print 'This grammar combined with this input semantics results in too large of a seach space<br>'
            if sentences[0] == "#NO-SENTENCES#":
                print 'This combination of verb, pattern, and feature specification did not result in any generated sentences.<br>'
            else:
                for j in range(len(sentences)):
                    #print str(j+1) + '. <span title="' + trees[j] + '">' + sentences[j] + "</span><br>"
                    print '<div onclick=toggle_visibility(["%s_parsemrs"])>%s. %s</div>' % (j+1,j+1,sentences[j])
                    print '<div id="%s_parsemrs" style=display:none;>' % (j+1)
                    print '&nbsp&nbsp Parse tree:<br>' + trees[j]
                    print '&nbsp&nbsp MRS:<br>' + mrss[j]
                    print '</div>'
        print '<br><input type="button" name="" value="Back to sentences" onclick="history.go(-1)">'
        print HTML_sentencespostbody
        print HTML_postbody

    # Display errors and warnings that occurred during customization
    def error_page(self, vr):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>Matrix Customization Errors</title>'
        print HTML_prebody

        if vr.has_errors():
            print '<h2>Errors</h2>'
            print '<dl>'
            for k in vr.errors:
                print '<dt><b>' + k + ':</b></dt>'
                print '<dd>' + vr.errors[k].message + '</dd>'
            print '</dl>'

        if vr.has_warnings():
            print '<h2>Warnings</h2>'
            print '<dl>'
            for k in vr.warnings:
                print '<dt><b>' + k + ':</b></dt>'
                print '<dd>' + vr.warnings[k].message + '</dd>'
            print '</dl>'

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
        print '<p style="text-align:center; font-size:16pt">'
        print 'If cookies are enabled, try reloading an '
        print '<a href="matrix.cgi?choices=empty">empty questionnaire</a>. '
        print 'Note that any existing changes will be lost.</p>'
        print '</div>'

        print HTML_postbody


    # Based on a section of a matrix definition file in lines, save the
    # values from choices into the file handle f.  The section in lines
    # need not correspond to a whole named section (e.g. "Language"), but
    # can be any part of the file not containing a section line.
    def save_choices_section(self, lines, f, choices,
                             iter_level = 0, prefix = ''):
        already_saved = {}  # don't save a variable more than once
        i = 0
        while i < len(lines):
            word = tokenize_def(lines[i])
            if len(word) == 0:
                pass
            # TJT 2014-5-27: changing this from list to tuple
            elif word[0] in ('Check', 'Text', 'TextArea',
                             'Radio', 'Select', 'MultiSelect', 'File','Hidden'):
                vn = word[1]
                if prefix + vn not in already_saved:
                    already_saved[prefix + vn] = True
                    val = ''
                    if choices.get(prefix + vn):
                        val = choices.get(prefix + vn)
                        if word[0] == 'TextArea':
                            val = '\\n'.join(val.splitlines())
                    if vn and val:
                        f.write('  '*iter_level) # TJT 2014-09-01: Changing this to one write from loop
                        f.write(prefix + vn + '=' + val + '\n')
            elif word[0] == 'BeginIter':
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

                for var in choices.get(prefix + iter_name):
                    self.save_choices_section(lines[beg:end], f, choices,
                                              iter_level = iter_level + 1,
                                              prefix =
                                              prefix + iter_name + \
                                              str(var.iter_num()) + '_')

            i += 1


    # Read the choices_file, stripping out the section associated with
    # the 'section' member of form_data, and replacing it with all the
    # values in form_data.  Use self.def_file to keep the choices file
    # in order.
    def save_choices(self, form_data, choices_file):
        # The section isn't really a form field, but save it for later
        # section is page user is leaving (or clicking "save and stay" on)
        section = form_data['section'].value

        ## New choices vs Old choices
        # old_choices is saved to pages other than "section" variable above
        # new_choices is saved to the "section" variable above's page

        # Copy the form_data into a choices object
        new_choices = ChoicesFile('')
        for k in form_data.keys():
            if k:
                # on sentential negation page, some choices are hidden in
                # more than one place, so the FieldStorage object at [k] can
                # be a list, but in these cases only one item on the list
                # should ever have a value
                if type(form_data[k]) == list:
                    for l in form_data[k]:
                        if l.value:
                            new_choices[k] = l.value
                else:
                    new_choices[k] = form_data[k].value

        # Read the current choices file (if any) into old_choices
        old_choices = ChoicesFile(choices_file)

        # Keep track of features
        if section in ('lexicon', 'morphology'): feats_to_add = defaultdict(list)

        # TJT: 2014-08-26: If optionally copula complement,
        # add zero rules to choices
        if section == 'lexicon':
            for adj in new_choices.get('adj', []): # check NEW values
                if adj.get('predcop') == "opt":
                    atype = get_name(adj)
                    pc_name = "%s_opt_cop" % atype
                    # Skip if already added
                    if not any(pc.get('name','') == pc_name
                               for pc in old_choices['adj-pc']):
                        # TODO: make this a function
                        switching_pc = "adj-pc%d" % (old_choices['adj-pc'].next_iter_num()
                                                     if old_choices['adj-pc'] else 1)
                        # Set up position class
                        old_choices[switching_pc+"_name"] = pc_name
                        old_choices[switching_pc+"_obligatory"] = "on"
                        old_choices[switching_pc+"_inputs"] = atype
                        # Set up new position class as a "switching" pc
                        old_choices[switching_pc+'_switching'] = "on"
                        # Define lexical rule types
                        aToA = switching_pc+'_lrt1'
                        aToV = switching_pc+'_lrt2'
                        # Write adjective to adjective rule
                        old_choices[aToA+"_name"] = "%s_cop_comp" % atype
                        old_choices[aToA+"_predcop"] = "on"
                        old_choices[aToA+"_mod"] = "pred"
                        # Write adjective to verb rule
                        old_choices[aToV+"_name"] = "%s_stative_pred" % atype
                        #old_choices[aToV+"_predcop"] = "off" # Unchecked is off
                        old_choices[aToV+"_mod"] = "pred"


                        # TJT: 2014-08-26: If adjective agrees only with one argument,
                        # add zero rules to choices
                        #if section == 'lexicon': # already in lexicon
            # Check lexicon for argument agreement
            for adj in new_choices.get('adj',[]): # check NEW values
                for feat in adj.get('feat',[]):
                    if feat.get('head','') in ('subj','mod'):
                        atype = get_name(adj)
                        pc_name = "%s_argument_agreement" % atype
                        # Skip if already added
                        if not any(pc.get('name','') == pc_name for pc in old_choices['adj-pc']):
                            feats_to_add[atype].append(feat)

        elif section == 'morphology':
            # Check morphology for argument agreement
            for adj_pc in new_choices.get('adj-pc',[]): # check NEW values
                for lrt in adj_pc.get('lrt',[]):
                    for feat in lrt.get('feat',[]):
                        if feat.get('head','') in ('subj','mod'):
                            apc = adj_pc.full_key
                            pc_name = "%s_argument_agreement" % apc
                            # Skip if already added
                            if not any(pc.get('name','') == pc_name for pc in old_choices['adj-pc']):
                                feats_to_add[apc].append(feat)

        if section in ('lexicon', 'morphology'):
            # With features collected, add them to choices dict
            target_page_choices = old_choices if section == "lexicon" else new_choices
            for adj in feats_to_add:
                # Add zero rules
                argument_agreement_pc = "adj-pc%d" % (old_choices['adj-pc'].next_iter_num()
                                                      if old_choices['adj-pc'] else 1)
                # Set up position class
                target_page_choices[argument_agreement_pc+'_name'] = "%s_argument_agreement" % adj
                target_page_choices[argument_agreement_pc+'_obligatory'] = 'on'
                target_page_choices[argument_agreement_pc+'_inputs'] = adj
                # Set up new position class as a "switching" pc
                target_page_choices[argument_agreement_pc+'_switching'] = 'on'
                # Define lexical rule types
                subj_only = argument_agreement_pc+'_lrt1'
                mod_only = argument_agreement_pc+'_lrt2'
                # Write subject agreement rule
                target_page_choices[subj_only+'_name'] = "%s_subj_agr" % adj
                target_page_choices[subj_only+'_mod'] = 'pred'
                # Write modificand agreement rule
                target_page_choices[mod_only+"_name"] = "%s_mod_agr" % adj
                target_page_choices[mod_only+"_mod"] = "attr"
                # Add features from lexicon page to morphology page
                feat_count = 1
                for feat in feats_to_add[adj]:
                    head = feat.get('head','').lower()
                    if head in ('subj', 'mod'):
                        if head == 'subj':
                            # Copy subject agreement features
                            feature_vn = subj_only+"_feat%d" % feat_count
                        elif head == 'mod':
                            # Copy object agreement features
                            feature_vn = mod_only+"_feat%d" % feat_count
                        target_page_choices[feature_vn+'_name'] = feat.get('name')
                        target_page_choices[feature_vn+'_value'] = feat.get('value')
                        # Adjectives' MOD, XARG, and SUBJ identified
                        # so just agree with the XARG
                        target_page_choices[feature_vn+'_head'] = 'xarg'
                        # Delete this feature from current page
                        new_choices.delete(feat.full_key+'_name')
                        new_choices.delete(feat.full_key+'_value')
                        new_choices.delete(feat.full_key+'_head', prune=True)
                        feat_count += 1

        # if neg-aux=on exists, create side-effect in lexicon.
        if section == 'sentential-negation' \
                and ('neg-aux' in form_data.keys() \
                             or ('bineg-type' in form_data.keys() \
                                         and form_data['bineg-type'].value =='infl-head')):
            # see if we're already storing an index number
            if 'neg-aux-index' in old_choices.keys():
                # we have an index for a neg-aux, see if it's still around
                if not old_choices['aux%s' % old_choices['neg-aux-index']]:
                    # it's not so we make a new neg-aux and store the index
                    old_choices, neg_aux_index = self.create_neg_aux_choices(old_choices)
                    new_choices["neg-aux-index"] = str(neg_aux_index) if neg_aux_index > 0 else str(1)
            else: #we don't have any neg aux index stored, so make a new one
                old_choices, neg_aux_index = self.create_neg_aux_choices(old_choices,form_data)
                new_choices["neg-aux-index"] = str(neg_aux_index) if neg_aux_index > 0 else str(1)

        # create a zero-neg lri in choices
        if section == 'sentential-negation' and 'neg-exp' in form_data \
                and form_data['neg-exp'].value == '0' \
                and 'vpc-0-neg' in form_data.keys():
            if form_data['vpc-0-neg'].value != "":
                # infl-neg should be on for zero-neg to work
                new_choices['infl-neg'] = 'on'
                old_choices, new_choices = self.create_infl_neg_choices(old_choices, new_choices)

        # add FORM subtype for neg1b-neg2b analysis
        # also add it for infl-head neg analysis
        if section == 'sentential-negation':
            keys = form_data.keys()
            if 'neg1b-neg2b' in keys or \
                    ('neg1-type' in keys and 'neg2-type' in keys and form_data['neg1-type'].value == 'fh' and form_data['neg2-type'].value == 'b') or \
                    ('neg1-type' in keys and 'neg2-type' in keys and form_data['neg2-type'].value == 'fh' and form_data['neg1-type'].value == 'b'):
                try:
                    next_n = old_choices['form-subtype'].next_iter_num() if 'form-subtype' in old_choices else 1
                    found_negform = False
                    if next_n > 1:
                        nfss = old_choices.get('form-subtype')
                        for nfs in nfss:
                            if nfs['name'] == 'negform':
                                found_negform = True
                    if not found_negform:
                        old_choices['form-subtype%d_name' % next_n ] = 'negform' #TODO update this to new form: form-subtype name=negform with supertype nonfinite
                        old_choices['form-subtype%d_supertype' % next_n ] = 'nonfinite' #OZ 2017-12-09 I have not tested this line and don't know if it works
                except:
                    print 'Go look in deffile.py save_choices; probably nf-subform needs to be updated to form-subtype; see choices.py up-rev 28-29 function.'

        # Now pass through the def file, writing out either the old choices
        # for each section or, for the section we're saving, the new choices
        f = open(choices_file, 'w')
        f.write('\n') # blank line in case an editor inserts a BOM
        f.write('version=' + str(old_choices.current_version()) + '\n\n')

        cur_sec = ''
        cur_sec_begin = 0
        i = 0
        while i < len(self.def_lines):
            word = tokenize_def(self.def_lines[i])
            if len(word) == 0:
                pass
            elif word[0] == 'Section':
                if cur_sec:
                    self.save_choices_section(self.def_lines[cur_sec_begin:i], f, choices)
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
            self.save_choices_section(self.def_lines[cur_sec_begin:i], f, choices)

        f.close()


    def create_neg_aux_choices(self, choices,form_data):
        '''this is a side effect of the existence of neg-aux
        in the form data, it puts some lines pertaining to a neg-aux
        lexical item into the choices file object unless they are
        already there. returns a ChoicesFile instance, and an int
        which is the index number of the created neg-aux'''

        # get the next aux number
        next_n = choices['aux'].next_iter_num() if 'aux' in choices else 1

        # create a new aux with that number
        choices['aux%d_name' % next_n] = 'neg'

        # copy that to nli for adding further information
        nli = choices['aux'].get_last()
        nli['sem']='add-pred'
        nli['stem1_pred'] = 'neg_rel'

        if 'bineg-type' in form_data.keys() and \
                        form_data['bineg-type'].value =='infl-head':
            nli['compfeature1_name']='form'
            nli['compfeature1_value']='negform'

        # if auxiliaries are off, turn them on
        choices['has-aux'] = 'yes'
        return choices, next_n

    def create_infl_neg_choices(self, old_choices, new_choices):
        vpc = new_choices['vpc-0-neg']
        lrt = ''
        if vpc == 'create':
            next_n = old_choices['verb-pc'].next_iter_num() if old_choices['verb-pc'] else 1
            old_choices['verb-pc%d_name' % next_n] = 'negpc'
            vpc = old_choices['verb-pc'].get_last()
            vpc['lrt1_name'] = 'neg'
            lrt = old_choices['verb-pc'].get_last()['lrt'].get_last()
            new_choices['vpc-0-neg'] = 'verb-pc'+str(next_n)

        else:
            next_n = old_choices[vpc]['lrt'].next_iter_num() if old_choices[vpc]['lrt'] else 1
            # create new lrt in this position class
            old_choices[vpc]['lrt%d_name' % next_n] = 'neg'
            # add some features for negation and empty PHON
            lrt = old_choices[vpc]['lrt'].get_last()

        lrt['feat1_name']= 'negation'
        lrt['feat1_value'] = 'plus'
        lrt['feat1_head'] = 'verb'
        lrt['lri1_inflecting'] = 'no'
        return old_choices, new_choices

    def choices_error_page(self, choices_file, exc=None):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>Invalid Choices File</title>'
        print HTML_posttitle % ('', '', '','')
        print HTML_toggle_visible_js
        print HTML_prebody

        print '<div style="position:absolute; top:15%; width:60%">\n' + \
              '<p style="color:red; text-align:center; font-size:12pt">' + \
              'The provided choices file is invalid. If you have edited the ' + \
              'file by hand, please review the changes you made to make sure ' + \
              'they follow the choices file file format. If you did not make ' + \
              'any manual changes, please email the choices file to the Matrix ' + \
              'developers. You may download the choices file to try and fix ' + \
              'any errors.</p>\n'

        print '<p style="text-align:center"><a href="' + choices_file + '">' + \
              'View Choices File</a> (right-click to download)</p>'

        print '<p style="text-align:center">In most cases, you can go back ' + \
              'in your browser and fix the problems, but if not you may ' + \
              '<a href="matrix.cgi?choices=empty">reload an empty ' + \
              'questionnaire</a> (this will erase your changes, so be sure to ' + \
              'save your choices (above) first).'
        exception_html(exc)
        print HTML_postbody

    def customize_error_page(self, choices_file, exc=None):
        print HTTP_header + '\n'
        print HTML_pretitle
        print '<title>Problem Customizing Grammar</title>'
        print HTML_posttitle % ('', '', '','')
        print HTML_toggle_visible_js
        print HTML_prebody

        print '<div style="position:absolute; top:15%; width:60%">\n' + \
              '<p style="color:red; text-align:center; font-size:12pt">' + \
              'The Grammar Matrix Customization System was unable to create ' + \
              'a grammar with the provided choices file. You may go back in ' + \
              'your browser to try and fix the problem, or if you think ' + \
              'there is a bug in the system you may email the choices file ' + \
              'to the developers</p>\n'

        print '<p style="text-align:center"><a href="' + choices_file + '">' + \
              'View Choices File</a> (right-click to download)</p>'

        print '<p style="text-align:center">In most cases, you can go back ' + \
              'in your browser and fix the problems, but if not you may ' + \
              '<a href="matrix.cgi?choices=empty">reload an empty ' + \
              'questionnaire</a> (this will erase your changes, so be sure to ' + \
              'save your choices (above) first).'
        exception_html(exc)
        print HTML_postbody

def exception_html(exc):
    if exc and exc != (None, None, None):
        print '<p style="text-align:center">You may also wish to ' + \
              '<a href="#" onclick="toggle_visible(\'error\');">' + \
              'see the Python error</a> ' + \
              '(note: it is very technical, and possibly not useful).</p>'
        print "<div id=\"error\" style=\"display:none\">"
        cgitb.handler(exc)
        print "</div>"

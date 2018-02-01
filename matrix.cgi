#!/usr/bin/env python2

### $Id: matrix.cgi,v 1.27 2008-09-09 08:37:52 sfd Exp $

######################################################################
# imports

import sys
import os
import glob
import shutil
import cgi
import cgitb; cgitb.enable()
import time
import glob
import tempfile
from random import randint
from distutils.dir_util import remove_tree

from gmcs.deffile import MatrixDefFile
from gmcs.customize import customize_matrix
from gmcs.validate import validate_choices
from gmcs.choices import ChoicesFile
from gmcs.linglib.toolboximport import import_toolbox_lexicon

from gmcs.deffile import HTTP_header
import requests


#Production Check
disable_captcha = True

# Safety Check - There was a problem with the choices file ending up in a weird place and breaking things.
# this should hopefully delete it before the rest of the script can break. Still unsure what is causing the file
# to end up there in the first place.
if os.path.exists('sessions/choices'):
	os.remove('sessions/choices')

######################################################################
# beginning of main program

# Uncomment this to see the output from print in the HTML page
 #print HTTP_header + '\n'


matrixdef = MatrixDefFile('web/matrixdef')

form_data = cgi.FieldStorage()

# see if we are in debug mode
debug = 'debug' in form_data and form_data['debug'].value in ('true','True')

# Get the cookie.  If there's not one, make one.
http_cookie = os.getenv('HTTP_COOKIE')
browser_cookie = False
cookie = ''
need_verify=False
if http_cookie:
  for c in http_cookie.split(';'):
    (name, value) = c.split('=', 1)
    if name == 'session' and len(value) == 4 and value.isdigit():
      browser_cookie = True
      cookie = value
      need_verify=False
      break

# This area has been modified to support captcha code
def create_cookie():
    need_verify=False
    cookie = str(randint(1000,9999))
    while os.path.exists('sessions/' + cookie):
      cookie = str(randint(1000,9999))
    return cookie

if not cookie:
  if form_data.has_key('g-recaptcha-response'):
     public  = "6LfEeisUAAAAAGdbbNlfjxKjkRxcSWhSovyq9oik"
     private = "6LfEeisUAAAAAKb8ODRb6c06-TER8MhdmJ3Rkx9u"
     response = form_data['g-recaptcha-response'].value
     clientIP = os.environ["REMOTE_ADDR"]
     check_url = "https://www.google.com/recaptcha/api/siteverify"
     req = requests.get(check_url, params={"secret" : private, "response": response, "remoteip" : clientIP}) 
     if req.json()['success']:
       cookie = create_cookie()
#        need_verify=False
#        cookie = str(randint(1000,9999))
#        while os.path.exists('sessions/' + cookie):
#          cookie = str(randint(1000,9999))
  elif disable_captcha:
    cookie = create_cookie()
  	
  else:
     need_verify=True

# make the sessions directory if necessary
if not os.path.exists('sessions'):
  os.mkdir('sessions')

# look for any sessions older than 24 hours and delete them
now = time.time()
sessions = glob.glob('sessions/*')
for s in sessions:
  if now - os.path.getmtime(s) > 86400:
    remove_tree(s)

# figure out the path to the current session's directory, creating it
# if necessary
session_path = 'sessions/' + cookie
if cookie and not os.path.exists(session_path):
  os.mkdir(session_path)
  # create a blank choices file
  open(os.path.join(session_path, 'choices'), 'w').close()

# if the 'choices' field is defined, we have either the contents of an
# uploaded choices file or the name of a sample choices file (which
# will begin with 'sample-choices/') to replace the current choices.
# TJT 2014-09-18: Get choices files from Language CoLLAGE links
if form_data.has_key('choices'):
  choices = form_data['choices'].value
  if choices:
    data = ''
    if choices.startswith('web/sample-choices/'):
      f = open(choices, 'r')
      data = f.read()
      f.close()
    elif choices.startswith('collage/'):
      # Get choices files from CoLLAGE
      # should be 3 or 7 letter keys... doesn't work for other length keys
      if len(choices) in ((len('collage/') + 3), (len('collage/') + 7)):
	import urllib2, tarfile, StringIO
        choices = 'http://www.delph-in.net/matrix/language-'+choices+'/choices-final.tgz'
        try:
          tar = urllib2.urlopen(choices)
          tar = tarfile.open(fileobj=StringIO.StringIO(tar.read()), mode='r|*')
          for tarinfo in tar:
            if tarinfo.isreg() and tarinfo.name[-len('choices'):] == 'choices':
              choicesData = tar.extractfile(tarinfo)
              data = choicesData.read()
              choicesData.close()
              break # Found the choices file...
        except (urllib2.HTTPError, urllib2.URLError, tarfile.TarError):
          data = ''
	finally:
 	  tar.close()
    else: # Uploaded choices data
      data = choices
    if data or choices.endswith('/empty'):
      f = open(os.path.join(session_path, 'choices'), 'w')
      f.write(data)
      f.close()

# if the 'section' field is defined, we have submitted values to save
if form_data.has_key('section'):
  matrixdef.save_choices(form_data, os.path.join(session_path, 'choices'))

# if we have recieved toolbox files, then we want to add these lexical items after saving the toolbox configuration (done above).
if form_data.has_key('import_toolbox'):
  toolbox_files = []
  for key in form_data.keys():
    if key[-10:] == 'tbfilename' and form_data[key].value != "":
      fout = tempfile.NamedTemporaryFile(dir=session_path)
      fout.write(form_data[key].value)
      toolbox_files.append(fout)
      form_data[key].value = fout.name
  matrixdef.save_choices(form_data, os.path.join(session_path, 'choices'))
  import_toolbox_lexicon(os.path.join(session_path, 'choices'))
  for tbfile in toolbox_files:
    tbfile.close()

# If the 'verbpred' field is defined, then the user wishes to generate more sentences with that predication
if form_data.has_key('verbpred'):
  matrixdef.more_sentences_page(session_path,form_data['grammar'].value, form_data['verbpred'].value, form_data['template'].value, cookie)
  sys.exit()

# Get a list of error messages, determined by validating the current
# choices.  If the current choices are valid, the list will be empty.
# --
# no longer true, there can now be validation info messages.
# nothing seems to depend on the list being empty #14 feb 2012
try:
  vr = validate_choices(os.path.join(session_path, 'choices'))
except:
  exc = sys.exc_info()
  matrixdef.choices_error_page(os.path.join(session_path, 'choices'), exc)
  sys.exit()

# modified to support captcha
if need_verify:
  matrixdef.verification()
elif form_data.has_key('customize'):
	# if the 'customize' field is defined, create a customized copy of the matrix
	# based on the current choices file
  # ERB 2006-10-03 Checking has_key here to enable local debugging.
  if form_data.has_key('delivery'):
    arch_type = form_data['delivery'].value
  else:
    arch_type = ''
  if arch_type not in ( 'tgz', 'zip' ):
    vr.err('delivery', 'You must specify an archive type.')

  if vr.has_errors():
    matrixdef.error_page(vr)
  else:
    # If the user said it's OK, archive the choices file
    choices = ChoicesFile(os.path.join(session_path, 'choices'))
    if choices.get('archive') == 'yes':
      # create the saved-choices directory
      if not os.path.exists('saved-choices'):
        os.mkdir('saved-choices')

      # look at the files in saved-choices, which will have names like
      # choices.N, figure out the next serial number, and copy the current
      # choices file to saved-choices/choices.N+1
      serial = 1
      for f in glob.glob('saved-choices/choices.*'):
        i = f.rfind('.')
        if i != -1:
          num = f[i + 1:]
          if num.isdigit():
            serial = max(serial, int(num) + 1)
      shutil.copy(os.path.join(session_path, 'choices'),
                  'saved-choices/choices.' + str(serial))

    # Create the customized grammar
    try:
      grammar_dir = customize_matrix(session_path, arch_type)
    except:
      exc = sys.exc_info()
      matrixdef.customize_error_page(os.path.join(session_path, 'choices'),
                                     exc)
      sys.exit()

    if form_data.has_key('sentences'):
      matrixdef.sentences_page(session_path, grammar_dir, cookie)
    else:
      matrixdef.custom_page(session_path, grammar_dir, arch_type)
elif form_data.has_key('subpage'):
  if browser_cookie:
    matrixdef.sub_page(form_data['subpage'].value, cookie, vr)
  else:
    matrixdef.cookie_error_page()
else:
  matrixdef.main_page(cookie, vr)

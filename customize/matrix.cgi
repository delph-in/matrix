#!/usr/local/bin/python2.5

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
from random import randint
from distutils.dir_util import remove_tree

from deffile import MatrixDefFile
from customize import customize_matrix
from validate import validate_choices
from choices import ChoicesFile

from deffile import HTTP_header


######################################################################
# beginning of main program

# Uncomment this to see the output from print in the HTML page
#print HTTP_header + '\n'

matrixdef = MatrixDefFile('matrixdef')

form_data = cgi.FieldStorage()

# Get the cookie.  If there's not one, make one.
http_cookie = os.getenv('HTTP_COOKIE')
browser_cookie = False
if http_cookie:
  browser_cookie = True
  cookie = http_cookie.split('=')[1]
else:
  cookie = str(randint(1000,9999))
  while os.path.exists('sessions/' + cookie):
    cookie = str(randint(1000,9999))

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

# if the 'choices' field is defined, we have either the contents of an
# uploaded choices file or the name of a sample choices file (which
# will begin with 'sample-choices/') to replace the current choices.
if form_data.has_key('choices'):
  choices = form_data['choices'].value
  if choices:
    if choices[:15] == 'sample-choices/':
      f = open(choices, 'r')
      data = f.read()
      f.close()
    else:
      data = choices
    f = open(session_path + '/choices', 'w')
    f.write(data)
    f.close()

# if the 'section' field is defined, we have submitted values to save
if form_data.has_key('section'):
  matrixdef.save_choices(form_data, session_path + '/choices')


# Get a list of error messages, determined by validating the current
# choices.  If the current choices are valid, the list will be empty.
errors = validate_choices(session_path + '/choices')

# if the 'customize' field is defined, create a customized copy of the matrix
# based on the current choices file
if form_data.has_key('customize'):
  # ERB 2006-10-03 Checking has_key here to enable local debugging.
  if form_data.has_key('delivery'):
    arch_type = form_data['delivery'].value
  else:
    arch_type = ''
  if arch_type != 'tgz' and arch_type != 'zip':
    errors['delivery'] = 'You must specify an archive type.'

  if len(errors):
    matrixdef.error_page(errors)
  else:
    # If the user said it's OK, archive the choices file
    choices = ChoicesFile(session_path + '/choices')
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
      shutil.copy(session_path + '/choices',
                  'saved-choices/choices.' + str(serial))

    # Create the customized grammar
    customize_matrix(session_path, arch_type)
    matrixdef.custom_page(session_path, arch_type)
elif form_data.has_key('subpage'):
  if browser_cookie:
    matrixdef.sub_page(form_data['subpage'].value, cookie, errors)
  else:
    matrixdef.cookie_error_page()
else:
  matrixdef.main_page(cookie, errors)

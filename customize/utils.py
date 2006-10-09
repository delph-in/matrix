#!/usr/local/bin/python2.4

######################################################################
# tokenize_def(str)
#   split a string into words, treating double-quoted strings as
#   single words.

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
# load_choices(choices_file)
#   Load the choices in choices_file and return them in a dict

def load_choices(choices_file):
  choices = {}

  try:
    f = open(choices_file, 'r')
    lines = f.readlines()
    f.close()
    for l in lines:
      l = l.strip()
      if l:
        (a, v) = l.split('=')
        choices[a] = v
  except:
    pass

  return choices

### $Id: utils.py,v 1.8 2008-05-28 21:08:12 sfd Exp $

def tokenize_def(str):
  """
  Split a string into words, treating double-quoted strings as
  single words.
  """
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


def TDLencode(string):
  """
  Encode a string in such a way as to make it a legal TDL type name
  """
  val = ''
  for c in string:
    if not (c.isalnum() or ord(c) > 127 or c in ['_', '-', '+', '*']):
      val += '%' + '%2X' % (ord(c))
    else:
      val += c

  return val


def get_name(item):
  return item.get('name', None) or item.full_key

def format_comment_block(comment_string, max_chars=70, prefix=';;;'):
  lines = []
  comment_lines = comment_string.split('\\n')
  for s in comment_lines:
    lines += [prefix]
    toks = s.split(' ')
    for tok in toks:
        if len(lines[-1]) + len(tok) > max_chars:
            lines += [prefix + ' ' + tok]
        else:
            lines[-1] += ' ' + tok
  return '\n'.join(lines)

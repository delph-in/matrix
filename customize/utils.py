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

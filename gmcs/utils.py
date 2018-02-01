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

def orth_encode(orthin):
    """
    prepare an orth string in a way that
    words with spaces are treated properly.
    """
    orthlist = orthin.split(' ')
    orthout = ''
    if len(orthlist) > 1:
        orthout = '","'.join(orthlist)
    else:
        orthout = orthlist[0]
    return orthout


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

def verify():
    return raw_input("  Do you want to continue? (y/n): ").lower() in ('y','yes')

'''
2017-12-08 OZ: An attempt to start modularizing the customization code better.
A function
(with lots of params, so, not sure this is going to work well, this is just a first take on it)
that will generally merge in constraints into a type, assuming a two-tier choice structure.
Example:
        ccs = ch.get('comps')[0] # First clausal complement strategy
        path = 'SYNSEM.LOCAL.CAT.VAL.COMPS.FIRST.LOCAL.CAT.HEAD.FORM'
        merge_constraints(choice=ccs, mylang=mylang, typename='comp1-comp-lex-item',
                               path=path,key1='feat',key2='name',val='form')

This will merge in constraints on FORM into a complementizer lexical type.
The idea is that this and similar functions can be used throughout,
though like I said above I am not sure this is actually better.

'''
def merge_constraints(choicedict, mylang, typename, path, key1, key2, val):
    for ch in choicedict[key1]:
        if ch[key2] == val:
            mylang.add(typename + ' := [ ' + path + ' ' + ch['value'] + ' ].',
                       merge=True)

def nonempty_ccomp_nmz(ch):
    for cs in ch['comps']:
        for f in cs['feat']:
            if f['name'] == 'nominalization':
                for ns in ch['ns']:
                    if ns['name'] == f['value']:
                        if ns['nmzRel'] == 'yes':
                            return True
    return False

def has_nmz_ccomp(ch):
    for cs in ch['comps']:
        for f in cs['feat']:
            if f['name'] == 'nominalization':
                return True
    return False

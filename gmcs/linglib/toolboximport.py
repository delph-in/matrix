######################################################################
# imports

import os
from gmcs.utils import TDLencode
from gmcs.choices import ChoicesFile

# ERB 2011-04-25
# These functions enable the import of information from
# Toolbox lexicons into a choices file.  Because Toolbox
# does not constrain the tags that are used, the user must
# specify which Toolbox tags the system will attend to,
# both to find lexical items in general and to map entries
# in the Toolbox lexicon to lexical classes in the Toolbox
# file.  The result of calling import_toolbox_lexicon()
# is that any previously imported entries are dropped, and
# new entries are recorded in the choices file.  These are
# stored in a separate part of the choices file to a) enable
# that dropping and b) keep the Lexicon page from becoming
# unmanageable.  Ideally, the UI will eventually let the
# user explore the imported entries on demand (through clicking
# a link and bringing up a separate window, for example).
# The result of running the script should also provide the user
# with feedback about about the number of entries that were
# imported to each lexical class and the number of entries
# that matched no lexical classes.

def make_pred(tbentry,stemtag,glosstag,predchoice,lextype):
    '''
    Construct pred value from toolbox entry according
    to the predchoice specified.
    '''
    # Figure out if we're doing a noun or a verb
    lex_cat = lextype.rstrip('0123456789')
    # FIXME: What should I actually be doing with these errors?
    if lex_cat == 'verb':
        rel = '_v_rel'
    elif lex_cat == 'noun':
        rel = '_n_rel'
    else:
        print "Error: lex cat isn't verb or noun."
    # Construct stem
    if predchoice == 'stem':
        pred = TDLencode('_' + tbentry[stemtag] + rel) 
    elif predchoice == 'gloss':
        if tbentry[glosstag]:
            pred = TDLencode('_' + tbentry[glosstag] + rel)
        else:
            pred = TDLencode('_' + tbentry[stemtag] + rel)
    elif predchoice == 'glossfw':
        if tbentry[glosstag]:
            pred = TDLencode('_' + tbentry[glosstag].split()[0] + rel)
        else:
            pred = TDLencode('_' + tbentry[stemtag] + rel)
    else:
        print "Error: bad predchoice."

    return pred
  

def process_tb_entry(tbentry,lexclasses,idtag,stemtag,
                     bistemtag,glosstag,predchoice,choices):
    '''
    Figure out which lexclass this entry should belong
    to then add information to the choices file."
    '''

    for lexclass in lexclasses:
        match = False
        lextype = lexclass.get('importlextype')
        for tagvaluepair in lexclass.get('toolboxtag'):
            if tagvaluepair.get('tbtag') in tbentry.keys() \
                    and tbentry[tagvaluepair.get('tbtag')] == tagvaluepair.get('tbvalue'):
                match = True
        if match:
            if choices['imported-entry']:
                n = choices['imported-entry'].next_iter_num() 
            else:
                n = 1
            prefix = 'imported-entry' + str(n)
            pred = make_pred(tbentry,stemtag,glosstag,predchoice,lextype)
            if stemtag in tbentry.keys():
                choices[prefix + '_orth'] = tbentry[stemtag]
                if bistemtag in tbentry.keys():
                    choices[prefix + '_aff'] = tbentry[bistemtag]
                choices[prefix + '_pred'] = pred
                choices[prefix + '_lextype'] = lextype
            else:
                # Throw an error or warning here?
                continue
 

def import_toolbox_lexicon(choicesfile):
    '''
    Add choices to the choices file on the basis of
    information in a Toolbox lexicon file guided by
    the specifications in toolboximportconfig in choices file.
    This function is called separately from and before
    customize, being invoked by the user by clicking
    the Import Toolbox Lexicon button on the web page.
    The result is a new choices file that can then
    be customized.
    '''

    # input choices
    choices = ChoicesFile(choicesfile)
    # output choices
    imported_choices = ChoicesFile
    for config in choices['toolboximportconfig']:
        idtag = config.get('idtag')
        stemtag = config.get('stemtag')
        bistemtag = config.get('bistemtag')
        glosstag = config.get('glosstag')
        predchoice = config.get('tbpredvalues')
        lexclasses = config.get('importclass')
        #FIXME: Surely need a path here.  Also, the current
        #questionnaire allows multiple Toolbox files, need
        #to iterate trhough them.
        if not config.get('tbfilename'):
            continue
        tblex = open(config.get('tbfilename'),'r')

        #Go through lexicon file only once, as it could
        #be quite large.  For each entry in the lexicon,
        #iterate through the lexclasses to see if it matches
        #any of them, and if so, import.

        tbentry = {}
        for line in tblex.readlines():
            # Assume that the Toolbox tags may occur in any order
            # within an entry, but that they never repeat within
            # an entry.  In other words, when we see the same tag
            # again, that means we've hit a new entry, and we
            # should process the previous one then reset tbentry.
            words = line.split()
            if words:
                if words[0] in tbentry.keys():
                    process_tb_entry(tbentry,lexclasses,idtag,stemtag,bistemtag,glosstag,predchoice,choices)
                    tbentry = {}
                tbentry[words[0]] = ' '.join(words[1:])

    # Print new choices file by concatenating input choices
    # with output choices, and adding a section= line between
    
    #print choices['verb-pc2_lrt2_feat1_value']
    #print choices['verb-pc2_name']
    print choices


def integrate_imported_entries(choices):
    '''
    Take ChoicesFile object and check for imported-entry
    choices.  If present, add stem or bistem choices to
    relevant lexical classes on the basis of imported-entry
    information.  This function should be called early in
    the customization process, before anything else that relies
    on lexicon-related choices.
    '''

    for imported_entry in choices['imported-entry']:
        lextype = imported_entry['lextype']
        orth = imported_entry['orth']
        pred = imported_entry['pred']
        aff = imported_entry['aff']

        if aff:
            prefix = lextype + '_bistem'
        else:
            prefix = lextype + '_stem'

        if choices[prefix]:
            n = choices[prefix].next_iter_num()
        else: 
            n = 1

        prefix = prefix + str(n)
        choices[prefix + '_orth'] = orth
        choices[prefix + '_pred'] = pred
        if aff:
            choices[prefix + '_aff'] = aff

    # FIXME: This is just a hack to keep things working now.
    # What I really want to do is print out a version number
    # that matches whatever the last uprev was (since someone
    # might walk away with one of these output choices files
    # and then come back some time later).  Also, the printed version
    # should probably include the section = lines, right?
    choices['version'] = str(9999)
    choices.delete('imported-entry')


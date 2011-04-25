######################################################################
# imports

import os
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

def process_tb_entry(tbentry,lexclasses,idtag,stemtag,
                     bistemtag,glosstag,predchoice,choices):
    '''
    Figure out which lexclass this entry should belong
    to then add information to the choices file."
    '''

    for lexclass in lexclasses:
        match = False
        for tagvaluepair in lexclass.get('toolboxtag'):
            if tagvaluepair.get('tbtag') in tbentry.keys \
                    and tbentry(tagvaluepair.get('tbtag')) == tagvaluepair.get('tbvalue'):
                match = True
        if match:
            # Figure out prefix: prefix should be imported_<lexclass>N
            # that is, create a prefix like imported_verb1, and then
            # would get things like imported_verb1_stem1_pred.
            # Figure out if this is a bistem
            # Figure out what pred would be
            # Add lexclass, pred, stem and bistem info to choices


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
    choices = ChoicesFile(path)
    # output choices
    imported_choices = ChoicesFile
    for config in choices.get('toolboximportconfig'):
        idtag = config.get('idtag')
        stemtag = config.get('stemtag')
        bistemtag = config.get('bistemtag')
        glosstag = config.get('glosstag')
        predchoice = config.get('tbpredvalues')
        lexclasses = config.get('importclass')
        #FIXME: Surely need a path here.  Also, the current
        #questionnaire allows multiple Toolbox files, need
        #to iterate trhough them.
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
            if words[0] in tbentry.keys():
                process_tb_entry(tbentry,lexclasses,idtag,stemtag,bistemtag,glosstag,predchoice,imported_choices)
                tbentry = {}
            tbentry(words[0]) = ' '.join(words[1:])

    # Print new choices file by concatenating input choices
    # with output choices, and adding a section= line between

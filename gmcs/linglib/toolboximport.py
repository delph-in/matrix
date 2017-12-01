######################################################################
# imports

import os
import re
from gmcs.utils import TDLencode
from gmcs.choices import ChoicesFile
from gmcs.choices import FormData
from gmcs.deffile import MatrixDefFile

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


def process_tb_entry(tbentry,lexclasses,stemtag,
                     bistemtag,glosstag,predchoice,choices,affixes,
                     formdata, n):
    '''
    Figure out which lexclass this entry should belong
    to then add information to the choices file.
    Along the way, if there are any bistems, add the value
    of that field to the list of affixes.
    '''

    for lexclass in lexclasses:
        match = 0
        lextype = lexclass.get('importlextype')
        tvps = lexclass.get('toolboxtag')
        for tagvaluepair in tvps:
            if tagvaluepair.get('tbtag') in tbentry.keys() \
                    and tbentry[tagvaluepair.get('tbtag')] == tagvaluepair.get('tbvalue'):
                match += 1

        if match == len(tvps):
            #            if choices['imported-entry']:
            #                n = choices['imported-entry'].next_iter_num()
            #            else:
            #                n = 1
            prefix = 'imported-entry' + str(n)
            pred = make_pred(tbentry,stemtag,glosstag,predchoice,lextype)
            if stemtag in tbentry.keys():
                #                choices[prefix + '_orth'] = tbentry[stemtag]
                formdata[prefix + '_orth'].value = tbentry[stemtag]
                if bistemtag in tbentry.keys():
                    #                    choices[prefix + '_aff'] = tbentry[bistemtag]
                    formdata[prefix + '_aff'].value = tbentry[bistemtag]
                    affixes.append(tbentry[bistemtag])
                #                choices[prefix + '_pred'] = pred
                #                choices[prefix + '_lextype'] = lextype
                formdata[prefix + '_pred'].value = pred
                formdata[prefix + '_lextype'].value = lextype
            else:
                # Throw an error or warning here?
                continue

    return affixes


def get_affix_from_entry(tbentry,idtag,stemtag,affixes,affix_strings):
    ''' 
    Given a toolbox entry see if it is an entry for a bistem
    affix, If so, find the orthography of the affix and store it in
    the affix_strings dictionary.
    '''
    if idtag not in tbentry.keys():
        #print tbentry
        #print "Error: tbentry without tbid"
        tbid = 0
    else:
        tbid = tbentry[idtag]
    for affix in affixes:
        if affix == tbid:
            affixes.remove(tbid)
            affix_strings[tbid] = tbentry[stemtag]
            break
    return [affixes, affix_strings]

def insert_affixes(form_data, affix_strings, number):
    '''
    Given a dictionary mapping affix ids to affix forms,
    update the imported-entry choices to replace the orthography
    of bistem affixes.
    '''
    for entry in range(1,number):
        affix_id = form_data['imported-entry'+str(entry)+'_aff'].value
        #        full_key = entry.full_key
        if affix_id in affix_strings.keys():
            form_data['imported-entry'+str(entry)+'_aff'].value = affix_strings[affix_id]

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
    # Counters
    tbentries = 0
    form_data_entries = 1;

    form_data = FormData();
    form_data['section'].value = 'ToolboxLexicon'

    for config in choices['toolboximportconfig']:
        idtag = config.get('idtag')
        stemtag = config.get('stemtag')
        bistemtag = config.get('bistemtag')
        glosstag = config.get('glosstag')
        predchoice = config.get('tbpredvalues')
        lexclasses = config.get('importclass')
        starttag = config.get('starttag')
        #FIXME: Surely need a path here.  Also, the current
        #questionnaire allows multiple Toolbox files, need
        #to iterate trhough them.

        for tbfile in config.get('toolboxfile'):
            if not tbfile.get('tbfilename'):
                continue
            tb_lines = None
            tblex = open(tbfile.get('tbfilename'),'r')
            tbentry = {}
            # List of values of the bistemtag field.
            affixes = []

            #Go through lexicon file only once, as it could
            #be quite large.  For each entry in the lexicon,
            #iterate through the lexclasses to see if it matches
            #any of them, and if so, import.

            for line in tblex.readlines():
                # Assume that the Toolbox tags may occur in any order
                # within an entry, but that they never repeat within
                # an entry.  In other words, when we see the same tag
                # again, that means we've hit a new entry, and we
                # should process the previous one then reset tbentry.
                words = line.split()
                if words:
                    if words[0] == starttag:
                        affixes = process_tb_entry(tbentry,lexclasses,stemtag,bistemtag,glosstag,predchoice,choices,affixes,form_data,form_data_entries)
                        if form_data.has_key('imported-entry'+str(form_data_entries)+'_orth'):
                            form_data_entries += 1
                        tbentry = {}
                        tbentries += 1
                    tbentry[words[0]] = ' '.join(words[1:])

            tblex.close()

            # Go through the list of affixes for bistems.
            # If any of the bistems is non-numeric, assume that the
            # value of that field was a lexid, and go get the actual
            # orthographic material for each bistem.

            affixids = True
            if affixes:
                for affix in affixes:
                    if not re.search(r'^[0-9]+$',affix):
                        affixids = False

            if affixids and affixes:
                tblex = open(tbfile.get('tbfilename'),'r')
                affix_strings = {}
                for line in tblex.readlines():
                    words = line.rstrip().split()
                    if words:
                        if words[0] == starttag and affixes:
                            [affixes, affix_strings] = get_affix_from_entry(tbentry,idtag,stemtag,affixes,affix_strings)
                            tbentry = {}
                        tbentry[words[0]] = ' '.join(words[1:])
                insert_affixes(form_data, affix_strings, form_data_entries)
                # FIXME:  Put a break statement here so that we 
                # don't keep reading the file if we've found all the
                # affixes (i.e., if affixes == []).


    # Print new choices file by concatenating input choices
    # with output choices.  FIXME: What about section=?
    matrixdef = MatrixDefFile('web/matrixdef')
    matrixdef.save_choices(form_data, choicesfile)
#    fout = open(choicesfile+"new", 'w')
#    choices['version'] = str(choices.current_version())
#    fout.write(str(choices))
#    fout.write('Toolbox entries processed: ' + str(tbentries))
#    fout.write('Total entries imported: ' + str(choices['imported-entry'].next_iter_num() - 1))

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
    choices.delete('imported-entry')


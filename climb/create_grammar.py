import shutil
import os
import tdl
import re
import sys
from tdl import TDLparse_compl
from tdl import TDLmergeable
from tdl import TDLmerge
from extract_feature_geometry import build_defined_types_hierarchy
from extract_feature_geometry import is_subtype
from abbreviate_paths_in_climb import create_feature_geom_objects

#output files

mylang = None
rules = None
irules = None
lrules = None
lexicon = None
roots = None

thierarchy = {}
fg_dicts = []

def create_back_ups(file_names):
    base_dir = '../'
    bak_dir = base_dir + 'back-up/'
    if not os.path.exists(bak_dir):
        os.makedirs(bak_dir)
    for f in file_names:
        shutil.copy(base_dir + f, bak_dir + f)



def create_tdl_files(my_language):
    file_names = [my_language + '.tdl', 'rules.tdl', 'irules.tdl', 'lrules.tdl' , 'lexicon.tdl', 'roots.tdl']
    create_back_ups(file_names)
    grammar_path = '../'
    global mylang, rules, irules, lrules, lexicon, roots

    mylang =  tdl.TDLfile(os.path.join(grammar_path, my_language.lower() + '.tdl'))
    mylang.define_sections([['addenda', 'Matrix Type Addenda', True, False],
                          ['features', 'Features', True, False],
                          ['dirinv', 'Direct-Inverse', True, False],
                          ['lextypes', 'Lexical Types', True, True],
                          ['nounlex', 'Nouns', False, False],
                          ['verblex', 'Verbs', False, False],
                          ['auxlex', 'Auxiliaries', False, False],
                          ['otherlex', 'Others', False, False],
                          ['lexrules', 'Lexical Rules', True, False],
                          ['phrases', 'Phrasal Types', True, False],
                          ['coord', 'Coordination', True, False]])
    rules =   tdl.TDLfile(os.path.join(grammar_path, 'rules.tdl'))
    irules =  tdl.TDLfile(os.path.join(grammar_path, 'irules.tdl'))
    lrules =  tdl.TDLfile(os.path.join(grammar_path, 'lrules.tdl'))
    lexicon = tdl.TDLfile(os.path.join(grammar_path, 'lexicon.tdl'))
    roots =   tdl.TDLfile(os.path.join(grammar_path, 'roots.tdl'))


'''reads choices file and retrieves language name'''
def retrieve_language_name():

    choices = open('../choices','r')
    for line in choices:
        if 'language=' in line:
            return line.split('=')[1].rstrip()


def selected_type(begin_type, excl_analysis):
    for excl_an in excl_analysis:
        if begin_type.endswith(excl_an):
            return False
    return True


'''returns true if current location is part of comment'''
def partOfComment(line, flag):
  if re.match('\s*\#\|', line):
            #multiline comment starts
    flag = 1
  elif re.match('.*\|\#', line):
    flag = 0

  return flag


def retrieve_begin_type(description):
    type_name = description.split(':')[0].rstrip()
    type_name = type_name.lstrip()
    if '+' in type_name:
        type_name = 'head'
    t2val = fg_dicts[1]
    if type_name in t2val:
        return type_name
    else:
        for t in t2val.keys():
            if type_name in t:
                return t
    if type_name in thierarchy:
        my_t = thierarchy[type_name]
        for tl in t2val:
            if not '|' in tl:
                if tl in thierarchy and not tl == 'avm':
                    my_tl = thierarchy[tl]
                    if is_subtype(my_t, my_tl, thierarchy) or is_subtype(my_tl, my_t, thierarchy):
                        return tl
            elif type_name in tl:
                return tl
            else:
                for t in tl.split('|'):
                    if not t == 'avm' and t in thierarchy:
                        my_t2 = thierarchy[t]
                        if is_subtype(my_t, my_t2, thierarchy) or is_subtype(my_t2, my_t, thierarchy):
                            return tl
                    
    return 'sign'


def process_file(f, tdl_files, exclusions):
    #always start with mylang
    current_tdl = mylang
#go through file, if not 'utf stuff '
    my_climb_file = open(f, 'r')

    section = ''
    flag = 0
    temp_descr = ''
    comment = ''
    irules = False
    irule = ''
    include = True
    included_lib = True
    for line in my_climb_file:
        if 'Library=' in line:
            libname = line.split('=')[1].rstrip()
            if libname in exclusions('libraries'):
                included_lib = False
                include = False
            else:
                included_lib = True
                include = True
        elif 'Begin=' in line:
            chunkname = line.split('=')[1].rstrip()
            if chunkname in exclusions('chunks'):
                include = False
            else:
                if included_lib:
                    include = True
        elif 'End=' in line:
            if not include and include_lib:
                include = True
        elif 'File=' in line:
            new_fname = line.split('=')[1].rstrip()
            current_tdl = tdl_files[new_fname]
            if 'File=irules' in line:
                irules = True
            else:
                irules = False
        elif irule and line == '\n' and include:
            current_tdl.add_literal(irule)
            irule = ''
        elif irules:
            irule += line
        elif 'section=' in line:
            section = line.split('=')[1].rstrip()
            section = section.lstrip('\'').rstrip('\'')
        elif not ((re.match('\s*;', line)) or (flag == 1) or (re.match('.*\|\#', line))):

#TODO in the following line I ignore comments 
#appearing in a description itself
#INSTEAD: use groups before ';' and after
#before is line, after is stored as comment
            if comment and include:
                current_tdl.add_literal(comment)
                comment = ''
            line = re.sub( r';.*', '', line )
            temp_descr += line
#search for potential end of description
            if re.search('\.\s*\n',  line):
                description = temp_descr
                temp_descr = ''
                begin_type = retrieve_begin_type(description)
                if include and selected_type(begin_type, exclusions['analysis']):
                    tdl_object = TDLparse_compl(description, fg_dicts, begin_type)
                    handled = False
                    for i in range(len(current_tdl.typedefs) - 1, -1, -1):
                        if TDLmergeable(current_tdl.typedefs[i], tdl_object):
                            current_tdl.typedefs[i] = TDLmerge(current_tdl.typedefs[i], tdl_object)
                            handled = True
                            break
                        if not handled:
                            current_tdl.typedefs.append(tdl_object)
        else:
            if not ';;; -*- Mode: TDL; Coding: utf-8 -*-' in line:
                comment += line
            else:
                comment += '\n'
    if comment:
        current_tdl.add_literal(comment)
#1. determine output (if contains 'File=', determine new file, ignore comment line)
#2. determine section
#else: build comment
#or build types
#print to output file or section

def process_climb_files(exclusions):
    climb_files = [f for f in os.listdir('.') if os.path.isfile(f) and f.endswith("tdl")]
    tdl_files = {'Language Specific': mylang, 'rules':rules, 'irules':irules, 'lexicon':lexicon, 'lrules': lrules, 'roots':roots }
    for f in climb_files:
        if not f in exclusions['tdl-files']:
            process_file(f, tdl_files, exclusions)
    #go through files in current directory
    
    for tdl_f in tdl_files.values():
        tdl_f.save()


def process_thierarchy(my_language):
    tdl_files = [my_language + '.tdl','matrix.tdl','head-types.tdl']
    global thierarchy
    thierarchy = build_defined_types_hierarchy('../', tdl_files)



def interpret_choices(choices):
    exclusions = {'tdl-files': [], 'libraries': [], 'chunks': [], 'analysis': []}
    if choices:
        c_file = open(choices, 'r')
        excl_cat = ''
        for line in c_file:
            if 'category=' in line:
                excl_cat = line.split('=')[1].rstrip()
            elif 'exclude=' in line:
                exclusion = line.split('=')[1].rstrip()
                if excl_cat:
                    exclusions[excl_cat].append(exclusion)
    return exclusions

def create_grammar(choices = None):
    
    feat_geom_file = 'feature_geometry'
    global fg_dicts
    fg_dicts = create_feature_geom_objects(feat_geom_file)
    my_language = retrieve_language_name()
    
    process_thierarchy(my_language)
    create_tdl_files(my_language)
    exclusions = interpret_choices(choices)
    process_climb_files(exclusions)
    #3. go through climb files, add types to correct objects




def main(argv=None):

    if argv = None:
        argv = sys.argv
    if len(argv) < 2:
        create_grammar()
    else:
        create_grammar(argv[1])



if __name__ == "__main__":
    main()
  #main -> always create back-up files

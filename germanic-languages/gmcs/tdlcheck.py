###checks well-formedness of tdl hierarchy
# has additional functions
# 1. only add instantiated types
import re
import shutil

from gmcs import tdl
from gmcs.tdl import TDLparse
from gmcs.tdl import TDLelem_type
from gmcs.tdl import TDLset_file



class TDLdefined_type(object):
  def __init__(self, type, op):
    self.comment = ''
    self.type = type
    self.op = op
    self.supertypes = []
    self.subtypes = []
    self.val_constr = {}
    self.coind_constr = {}
    
  def set_comment(self, comment):
    self.comment = comment

  def get_comment(self):
    return self.comment

  def set_type(self, type_name):
    self.type = type_name

  def get_type(self):
    return self.type

  def add_supertype(self, type_name):
    self.supertypes.append(type_name)

  def add_subtype(self, type_name):
    self.subtypes.append(type_name)


def create_type_dict_from_tdl(file, type_dict):
  print file.name
  lines = file.readlines()
  file.close()
  temp_descr = ''
  flag = 0
  for line in lines:
    if re.match('\s*\#\|', line):
            #multiline comment starts
      flag = 1
    elif re.match('.*\|\#', line):
      flag = 0

    if not ((re.match('\s*;', line)) or (flag == 1)):

#TODO in the following line I ignore comments 
#appearing in a description itself
#I do not see any good way to store and 
#reproduce them later 
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object = TDLparse(description)
        type_dict[tdl_object.type] = tdl_object      
  

def identify_inst_types(itypes, deftypes):
  #set of elementary types requested by instantiated types
  req_inst_types = set()
  #adding 'null': not often specified, but generally required
  req_inst_types.add('null')
  req_inst_types.add('label')
  #set of identified elementary types requested by instantiated
  id_inst_types = set()

  #retrieve all requested elem types from instantiated types
  for k, v in itypes.iteritems():
    identify_objects(v.child, req_inst_types)
  
  walk_through_instantiated(req_inst_types, id_inst_types, deftypes)
  

  return id_inst_types


def walk_through_instantiated(requested, identified, defined):
  new_rit = set()
 #dangerous: operating on set that is looped through...
  for rit in requested:
    if rit in defined:
      identified.add(rit)
      temp_rit = set()
      identify_objects(defined[rit].child, temp_rit)
      for nt in temp_rit:
        if not (nt in identified or nt in requested):
          new_rit.add(nt)
      temp_rit.clear()
    else:
      print "Problem: somehow the following type is requested but not defined:"
      print rit
  if len(new_rit) > 0:
    walk_through_instantiated(new_rit, identified, defined)

def identify_objects(tdl_e_c_c, itypes):
  for ch in tdl_e_c_c:
    if isinstance(ch, TDLelem_type):
      process_elementary_type(ch, itypes)
    identify_objects(ch.child, itypes)



def process_elementary_type(t, tset):
  out = open('check_file.txt', 'a')
  if not re.match('\"*\"',t.type):
    if not t.type == 'null':
      tset.add(t.type)
      out.write(t.type + '\n')
  for ch in t.child:
    print "type child: "
    print ch
    identify_objects(ch)



def separate_instantiated_non_instantiated(file, instset, sfset):
  print file.name
  copied = file.name + '-old'
  unused_types = file.name + '-removed-types'
  check = open('3check.txt', 'a')
  shutil.copy(file.name, copied)
  lines = file.readlines()
  file.close()
  temp_descr = ''
  flag = 0
  output = open(file.name, 'w')
  dump_output = open(unused_types, 'w')
  for line in lines:
    if re.match('\s*\#\|', line):
            #multiline comment starts
      flag = 1
    elif re.match('.*\|\#', line):
      flag = 0

    if not ((re.match('\s*;', line)) or (flag == 1)):

#TODO in the following line I ignore comments 
#appearing in a description itself
#I do not see any good way to store and 
#reproduce them later 
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object = TDLparse(description)
        check.write(tdl_object.type + ';\n')
        if tdl_object.type in instset:   
          output.write(description)
        elif tdl_object.type in sfset:
          dump_output.write(description)
        else:
          output.write(description)
#          dump_output.write(description)
#          check.write(tdl_object.type + ';\n')
    else:
      output.write(line)
 
  output.close()
  dump_output.close()
  check.close()

def process_instantiation_files(path, inst, type_defs):
  itypes = {}
  for f in inst:
    file = open( path + '/' + f )
    create_type_dict_from_tdl(file, itypes) 
    file.close

  deftypes = {}
  for tf in type_defs:
    file = open( path + '/' + tf )
    create_type_dict_from_tdl(file, deftypes)
    file.close()

  
  instantiatedset = identify_inst_types(itypes, deftypes)
  superflset = set()
  for k, v in deftypes.iteritems():
    if not k in instantiatedset:
      superflset.add(k)

  print "identified: " 
  print len(superflset)
  print " unused types."

  check = open('2check.txt', 'a')
  for ins in instantiatedset:
    check.write(ins + ';')
    check.write('\n')
  check.close()

  for dtf in type_defs:
    file = open ( path + '/' + dtf)
    separate_instantiated_non_instantiated(file, instantiatedset, superflset)
    file.close()

  output_file = open( 'output.txt' , 'w')
  for it in instantiatedset:
    output_file.write(it)

  output_file.close()
#    output_file = open( 'test.tdl' , 'w')
  


'''take script file as input and create an array of instantiating tdl files'''
def identify_instantiation_files(script):
  script_lines = script.readlines()
  instantiated = []
  for line in script_lines:
    if re.match('\(read-tdl-lex-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        instantiated.append(parts[1])
    elif re.match('\(read-tdl-grammar-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        instantiated.append(parts[1])
    elif re.match('\(read-tdl-lex-rule-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        instantiated.append(parts[1])
    elif re.match('\(read-morph-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        instantiated.append(parts[1])
    elif re.match('\(read-tdl-psort-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        instantiated.append(parts[1])
  return instantiated


'''take script file as input and create array of type defining tdl files'''
def identify_type_def_files(script):
  script_lines = script.readlines()
  defined_types = []
  td = False
  for line in script_lines:
    if td:    
      parts = re.split('\"', line)
      if len(parts) > 1:
        defined_types.append(parts[1])
      elif not re.match('\(list', line):
        td = False
    elif re.match('\(read-tdl-type-files-aux', line):
      td = True 
  return defined_types

def process_script(path):
  script = open( path + '/lkb/script' )   
  inst = identify_instantiation_files(script)
  script.close
  script = open( path + '/lkb/script' )
  type_defs = identify_type_def_files(script)
  script.close
  process_instantiation_files(path, inst, type_defs)

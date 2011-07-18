###checks well-formedness of tdl hierarchy
# has additional functions
# 1. only add instantiated types
import re
import shutil

from gmcs import tdl
from gmcs.tdl import TDLparse
from gmcs.tdl import TDLelem_type
from gmcs.tdl import TDLelem_av
from gmcs.tdl import TDLelem_conj
from gmcs.tdl import TDLelem_feat
from gmcs.tdl import TDLelem_coref
from gmcs.tdl import TDLelem_dlist
from gmcs.tdl import TDLset_file


####global for sisters & cousins same generation in hierarchy
current_generation = set()


class TDLdefined_type(object):
  def __init__(self, type, op):
    self.comment = ''
    self.type = type
    self.op = op
    self.supertypes = []
    self.subtypes = []
    self.values = []
    self.attributes = []
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

  def add_value(self, val_name):
    if not val_name == None:
      if not val_name in self.values:
        self.values.append(val_name)   

  def add_attribute(self, attr_name):
    if not attr_name in self.attributes:
      self.attributes.append(attr_name)

  def add_att_val_pair(self, att, v):
    if att in self.val_constr:
      new_val = self.val_constr[att] + " & " + v
      self.val_constr.update([[att, new_val]])
    else:
      self.val_constr.update([[att, v]])

  #p is set of paths leading to marked up co-reference
  #coref is symbol used to mark coreference
  def add_coref_constraint(self, coref, p):
    if coref in self.coind_constr:
      old_p = self.coind_constr[coref]
      my_ps = old_p | p
    else:
      my_ps = p
    self.coind_constr.update([[coref, my_ps]])

def partOfComment(line, flag):
  if re.match('\s*\#\|', line):
            #multiline comment starts
    flag = 1
  elif re.match('.*\|\#', line):
    flag = 0

  return flag


def create_type_dict_from_tdl(file, type_dict, att):
  lines = file.readlines()
  file.close()
  temp_descr = ''
  flag = 0
  for line in lines:
    flag = partOfComment(line, flag)

    if not ((re.match('\s*;', line)) or (flag == 1)):
#Ignoring comments, okay at this stage: just processing the types
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object = TDLparse(description)
        type_dict[tdl_object.type] = tdl_object      
        collect_attributes(tdl_object, att)

def identify_inst_types(itypes, deftypes, attributes, id_inst_types = set()):
  #set of elementary types requested by instantiated types
  req_inst_types = set()
  #adding 'null': not often specified, but generally required
  if len(id_inst_types) == 0:
    req_inst_types.add('null')
  #adding label for labels.tdl
    req_inst_types.add('label')
  #in update round, itypes need to be added themselves as well..
  else:
    for it in itypes:
      req_inst_types.add(it)
  #set of identified elementary types requested by instantiated

  #retrieve all requested elem types from instantiated types

  for v in itypes.itervalues():
    for st in v.supertypes:
      req_inst_types.add(st)
    for val in v.values:
      req_inst_types.add(val)
    for a in v.attributes:
      attributes.add(a)
  
  walk_through_instantiated(req_inst_types, id_inst_types, deftypes, attributes)
  

  return id_inst_types


###TO DO: add subtype values at correct position
###plan: define attribute set of values on TDLdefined_type
###use typhierarchy for defined stuff
###collect supertypes for id-ing new subtypes
###should be done with complete hierarchy: else old problem will occur: types with relevant definitions 
###will not be marked as subtypes...

def walk_through_instantiated(requested, identified, defined, atts):
  new_rit = set()

  for rit in requested:
    if rit in defined:
      if not rit in identified:
        identified.add(rit)
#attributes in defined type:
        for a in defined[rit].attributes:
          atts.add(a)
#create list of defined type's value
        my_types = defined[rit].values
#for supertypes of defined type: add current type as subtype
#add to my_types list
        for s in defined[rit].supertypes:
          if s == '*top*':
            current_generation.add(rit)
          elif not s in defined:
            print s + ' listed as supertype not found in hierarchy'
          my_types.append(s)

        temp_rit = set(my_types)
         
  #    identify_objects(defined[rit].child, temp_rit, attributes)
        for nt in temp_rit:
          if not (nt in identified or nt in requested):
            new_rit.add(nt)
        temp_rit.clear()
    elif not rit == '*top*':
      print "Problem: somehow the following type is requested but not defined:"
      print rit
  if len(new_rit) > 0:
    walk_through_instantiated(new_rit, identified, defined, atts)

'interprets children of TDLelem_typedef and adds them to object of type TDLdefined_type'
def convert_elem_type_to_type_def(elem, typedef):
  for ch in elem.child:
#check for not embedded elementary types: these are supertypes
    if isinstance(ch, TDLelem_type):
      st = process_elementary_type(ch)
      typedef.add_supertype(st)
#call recursive function for all other children
    else:
      interpret_elemtype_children(ch, typedef)

def interpret_elemtype_children(elem, typedef):
  if isinstance(elem, TDLelem_av):
    path = ''
    process_att_val_type(elem, path, typedef)
  elif isinstance(elem, TDLelem_feat) or isinstance(elem, TDLelem_conj):
    for ch in elem.child:
      interpret_elemtype_children(ch, typedef)    
  elif not elem == None:  #TDLelem_feat and conj are covered by this...always has children of type av, no own attributes
    print "New case scenario: following type occurring unembedded: "
    print elem

def identify_objects(tdl_e_c_c, itypes, attrs):
  for ch in tdl_e_c_c:
    if isinstance(ch, TDLelem_type):
      my_type = process_elementary_type(ch)
      if not (my_type == None):
        itypes.add(my_type)
    elif isinstance(ch, TDLelem_av):
      attrs.add(ch.attr)
    identify_objects(ch.child, itypes, attrs)

def collect_attributes(t, att):
  for ch in t.child:
    if isinstance(ch, TDLelem_av):
      att.add(ch.attr)
    if not ch == None:
      collect_attributes(ch, att)


def process_att_val_type(t, p, td):
  td.add_attribute(t.attr)
  p += t.attr
  if len(t.child) == 0:
    print "this attribute has no children...something is wrong"
  elif len(t.child) == 1 and isinstance(t.child[0], TDLelem_conj):
    t = t.child[0]
    if len(t.child) == 1:
      t = t.child[0]
      interpret_av_emb_child(t, p, td)
    else:
      for c in t.child:
        temp_p = p
        interpret_av_emb_child(c, temp_p, td)
  else:
    print "av type has unusual child or number of children: "
    print t.child[0]
    print len(t.child)
    

def interpret_av_emb_child(t, p, td):
  if isinstance(t, TDLelem_feat):
    interpret_embedded_feat(t, p, td)
      
  elif isinstance(t, TDLelem_type):
    my_val = process_elementary_type(t)
    td.add_value(my_val)
    td.add_att_val_pair(p, my_val)
  elif isinstance(t, TDLelem_coref):
    if len(t.child) > 0:
      print "Problem: coreference type occurring with children"
    else:
      my_path = set([p])
      td.add_coref_constraint(t, my_path)
  elif isinstance(t, TDLelem_dlist):
    interpret_dlist(t, p, td)
  else:
    if not t is None:
      print "New case scenario: av_type has child:"
      print t

def interpret_embedded_feat(t, p, td):
  if len(t.child) == 1:
    t = t.child[0]
    if isinstance(t, TDLelem_av):
      p += '.'
      process_att_val_type(t, p, td)
    else:
      print 'Problem: feature has non-attribute value child...'
  elif len(t.child) > 1:
    for ch in t.child:  
      temp_p = p    
      if isinstance(ch, TDLelem_av):
        temp_p += '.'
        process_att_val_type(ch, temp_p, td)
      else:
        print 'Problem: feature has non-av child'

###to do: not completely done yet: does not interpret complex a_v within dlist value
###not too relevant for current purposes, so leaving this for now
###Also todo: deal with complex structures
def interpret_dlist(t, p, td):
  val = ''
  if len(t.child) == 0:
    val = '<! !>'
  elif len(t.child) == 1:
    t = t.child[0]
    for tch in t.child:
      if isinstance(tch, TDLelem_type):
        temp_val = process_elementary_type(tch)
        td.add_value(temp_val)
        val = '<! ' + temp_val + ' !>'
      elif isinstance(tch, TDLelem_feat):
        if len(tch.child) == 1:
          t = tch.child[0]
          if isinstance(t, TDLelem_av):
            td.add_attribute(t.attr)
    #ToDo: make complete (but is a detail for now...)
            val = '<! [ ' + t.attr + ' ]!>'
        else:
            val = '<! [ ] !>'
  #        for myc in tch.child:
  #          print myc.attr
  else:
    val = '<! [ ] !>'
  if val:
    td.add_att_val_pair(p, val)


def process_elementary_type(t):
#for loop is safety check: elementary types should not have children
  for ch in t.child:
    print "elementary type child: "
    print ch
    if isinstance(ch, TDLelem_type):
      process_elementary_type(ch)
    else:
      print "more work needed here...."
  if not re.match('\"*\"',t.type):
    if not t.type == 'null':
      return t.type


def add_subtype_values(thierarchy):
  for t in thierarchy.itervalues():
    suptypes = t.supertypes
    for s in suptypes:
      if s in thierarchy:
        my_suptype = thierarchy[s]
        my_suptype.add_subtype(t.type)
      elif not s == "*top*":
        print s 
        print " is identified as supertype but not defined in the hierarchy" 



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
    
    flag = partOfComment(line, flag)

    if not ((re.match('\s*;', line)) or (flag == 1)):

#TODO in the following line I ignore comments 
#appearing in a description itself
#INSTEAD: use groups before ';' and after
#before is line, after is stored as comment

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


def create_type_inventory(file, deftypes):

  lines = file.readlines()
  file.close()
  temp_descr = ''
  flag = 0
  for line in lines:
    flag = partOfComment(line, flag)

    if not ((re.match('\s*;', line)) or (flag == 1)):
#Ignoring comments, okay at this stage: just processing the types
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object = TDLparse(description)
        tdldef = TDLdefined_type(tdl_object.type, tdl_object.op)
        #parse tdl_object to obtain further info on type
        if len(tdl_object.child) == 1:
          t = tdl_object.child[0]
          convert_elem_type_to_type_def(t, tdldef)
          deftypes[tdldef.type] = tdldef   
        else:
          print "Problem: general typedef has more than one child"


def build_defined_types_hierarchy(path, type_defs):
  deftypes = {}
  for f in type_defs:
    file = open( path + '/' + f)
    create_type_inventory(file, deftypes)
    file.close()

# only taking path length one (probably too strict...)
# use this as type identifier
# then relax: all ends of path can be introduced
# if necessary: integrate check
def find_introduced_attribute(path):
  ats = path.split('.')
  if len(ats) <= 1:
    return path



def identify_attribute_intro_types(identified, typehierarchy, attrs, new_requests):
  global current_generation
  count_down_attrs = attrs
  new_requests.clear()
  i = 0

###exception for HEAD-DTR introduced by headed-phrase
  if 'headed-phrase' in identified:
    if 'HEAD-DTR' in count_down_attrs:
      count_down_attrs.remove('HEAD-DTR')

  while True:
    if len(current_generation) < 1:
      return False
    if len(count_down_attrs) == 0:
      return False
    if i >= 100:
      return False
    i += 1
    next_generation = set()
    for t in current_generation:
#to do: check whether type is in hierarchy (should not happen, but security check would be good)
      my_t = typehierarchy[t] 
      my_atts = set()
      my_avs = my_t.val_constr
      if len(my_avs) > 0:
        for at in my_avs.iterkeys():
          int_att = find_introduced_attribute(at)
          my_atts.add(int_att)
      if len(my_atts) > 0:
        for a in my_atts:
          if a in count_down_attrs:
            if t not in identified and t not in new_requests:
              new_requests[t] = typehierarchy[t]
            count_down_attrs.remove(a)
      for st in my_t.subtypes:
        next_generation.add(st)
    current_generation.clear()
    current_generation = next_generation
  ##do while count_down_attrs > 0 and current_generation > 0
 
###
  
def update_req_types_based_on_atts(instantiated_updated, new_reqs, typehierarchy, attributes):

  check_a = len(attributes)
  while True:
    if len(new_reqs) < 1:
      return False
    instantiated_updated = identify_inst_types(new_reqs, typehierarchy, attributes, instantiated_updated)
    if len(attributes) > check_a:
      identify_attribute_intro_types(instantiated_updated, typehierarchy, attributes, new_reqs)
    else:
      new_reqs.clear()


####depending on structure: change name of function...
def process_instantiation_files(path, inst, type_defs):

  typehierarchy = {}
  for dtf in type_defs:
    file = open ( path + '/' + dtf)
#    separate_instantiated_non_instantiated(file, instantiatedset, superflset)
    create_type_inventory(file, typehierarchy)
    file.close()  

  itypes = {}
  attributes = set()
  for f in inst:
    file = open( path + '/' + f )
    create_type_inventory(file, itypes) 
    file.close
  
  global current_generation
  current_generation.clear()
  instantiatedset = identify_inst_types(itypes, typehierarchy, attributes)

  add_subtype_values(typehierarchy)
  ###checking if instantiated types contain attributes that are not introduced
  #call function that starts at current_generation, checks for ATTR adds subtypes to new generation
  new_reqs = {}
  identify_attribute_intro_types(instantiatedset, typehierarchy, attributes, new_reqs)
  
  instantiated_updated = instantiatedset

  update_req_types_based_on_atts(instantiated_updated, new_reqs, typehierarchy, attributes)

  

  superflset = set()
  for k in typehierarchy.iterkeys():
    if not k in instantiated_updated:
      superflset.add(k)

  for tf in type_defs:
    file = open ( path + '/' + tf)
    separate_instantiated_non_instantiated(file, instantiated_updated, superflset)
    file.close()  


  
  print "identified: " 
  print len(superflset)
  print " unused types."
 
  typehierarchy.clear()
  superflset.clear()
  instantiatedset.clear()
  attributes.clear()
  itypes.clear()

#  heck = open('2check.txt', 'a')
#  for ins in instantiatedset:
#    check.write(ins + ';')
#    check.write('\n')
#  check.close()


#  output_file = open( 'output.txt' , 'w')
#  for it in instantiatedset:
#    output_file.write(it)
#
#  output_file.close()
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
  script.close()
  script = open( path + '/lkb/script' )
  type_defs = identify_type_def_files(script)
  script.close()
  process_instantiation_files(path, inst, type_defs)



#####NOTES##########
#
# 1. Heuristics are used for the attribute introduction problem:
# problem: some types are not specifically called as values by other types
# but they are needed to intoduce attributes
# Current strategy:
# - top-down walk-through type-hierarchy (per generation)
# - check path of length one -> assume this introduces an attribute
# - if attribute is required and not identified :
#       a) check if type is on instantiated required list
#       b) if not, add to new requested
#       c) remove attribute from to be identified list
# - Exception for HEAD-DTR: introduced by phrase at beginning of path
#
# 2. dlist interpretation is not finished: no interpretation of complex values
# - must do similar things as for av_interpretation, without directly modifying typehierarchy
# - larger operation: see if code can be shared between av for type and av for dlist...      

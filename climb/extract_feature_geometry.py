
import re
import sys
import getopt

import tdl
from tdl import TDLparse
from tdl import TDLelem_type
from tdl import TDLelem_av
from tdl import TDLelem_conj
from tdl import TDLelem_feat
from tdl import TDLelem_coref
from tdl import TDLelem_dlist
from tdl import TDLset_file

from abbreviate_paths_in_climb import create_feature_geom_objects

top_subtypes = set()
atts_and_intro_type = {}

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
    self.compl_val_constr = {}
    self.compl_coind_constr = {}
    
  def set_comment(self, comment):
    self.comment = comment

  def get_comment(self):
    return self.comment

  def set_type(self, type_name):
    self.type = type_name.lower()

  def get_type(self):
    return self.type

  def add_supertype(self, type_name):
    self.supertypes.append(type_name.lower())

  def add_subtype(self, type_name):
    self.subtypes.append(type_name.lower())

  def add_value(self, val_name):
    if not val_name == None:
      if not val_name in self.values:
        self.values.append(val_name.lower())   

  def add_attribute(self, attr_name):
    if not attr_name in self.attributes:
      self.attributes.append(attr_name)

  def add_att_val_pair(self, att, v):
    self.val_constr.update([[att, v]])

  def add_av_pair_tocl(self, att, v):
      self.compl_val_constr.update([[att, v]])
  #p is set of paths leading to marked up co-reference
  #coref is symbol used to mark coreference
  def add_coref_constraint(self, coref, p):
    if coref in self.coind_constr:
      old_p = self.coind_constr[coref]
      my_ps = old_p | p
    else:
      my_ps = p
    self.coind_constr.update([[coref, my_ps]])




'''Function dealing with TDLelem_feat
t = TDLelem_feat
p = string (path of attributes)
td = TDLdefined_type to be updated
Children of elem_feat should be TDLelem_av: process_att_val_type is called
Warning printed (and nothing done, if other child occurs...'''
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
#boolean property empty list indicates an empty list as feature value
  elif t.empty_list:
    td.add_value('< >')
    td.add_att_val_pair(p, '< >')



'''returns type name of elementary type, ignoring null, or values starting with quotes (which are not part of the hierarchy)'''
def process_elementary_type(t):
#for loop is safety check: elementary types should not have children
  for ch in t.child:
    print "elementary type child: "
    print ch
    if isinstance(ch, TDLelem_type):
      mytype = process_elementary_type(ch)
      return mytype
    else:
      print "more work needed here...."
  if not re.match('\"*\"',t.type):
    if not t.type == 'null':
      return t.type
    else:
      return '< >'

'''identifies supertypes TDLelem_typedef and calls recursive function interpreting other children of TDLelem_typedef which are used to define TDLdefined_type'''
def convert_elem_type_to_type_def(elem, typedef):
  for ch in elem.child:
#check for not embedded elementary types: these are supertypes
    if isinstance(ch, TDLelem_type):
      st = process_elementary_type(ch)
      typedef.add_supertype(st)
#top_subtypes collects types directly inheriting from *top*
#processes operating on complete tree are started with them
      if st == "*top*":
        top_subtypes.add(typedef.type)
#call recursive interpretation function for all other children
    else:
      interpret_elemtype_children(ch, typedef)
    
    if len(typedef.supertypes) == 0:
      print "FOLLOWING TYPE HAS NO SUPERTYPES: " + typedef.type


'''recursive function interpreting TDLelem children changing them into objects of TDLdefined_type, elem=TDLelem_typedef, typedef instantiated object of class TDLdefined_type'''
def interpret_elemtype_children(elem, typedef):
#TDLelem_av introduces attributes and a complex structure of further paths,
#values and coind: calling function to deal with this
  if isinstance(elem, TDLelem_av):
    path = ''
    process_att_val_type(elem, path, typedef)
#conj types only introduce new children (no proper info)
#feature types may also introduce < > as value, but not on highest level (at least one attribute must be found first)
  elif isinstance(elem, TDLelem_feat) or isinstance(elem, TDLelem_conj):
    for ch in elem.child:
      interpret_elemtype_children(ch, typedef) 
#Other TDLelem objects are either embedded into TDLelem_av or do not occur
#print warning if this turns out to be false...   
  elif not elem == None:  
    print "New case scenario: following type occurring unembedded: "
    print elem



'''Initiates interpretations of av_element, takes 
TDLelem_av (t), 
string of attributes (p),
TDLdefined_type (td) to be updated
interprets t, adding its attribute value to the path, TDLelem_av children are always TDLelem_conj: calling function to interpret further values of av'''
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

'''Function interpreting children of TDLelem_av object
t = TDLelem: child of elem_conj (child of av)
p = string: path made up of currently identified attributes
td = TDLdefined_type to be updated according to findings.'''
def interpret_av_emb_child(t, p, td):
  if isinstance(t, TDLelem_feat):
    interpret_embedded_feat(t, p, td)
#if elem_type is found, we reached a value definition      
  elif isinstance(t, TDLelem_type):
    my_val = process_elementary_type(t)
    td.add_value(my_val)
    td.add_att_val_pair(p, my_val)
  elif isinstance(t, TDLelem_coref):
    if len(t.child) > 0:
      print "Problem: coreference type occurring with children"
    else:
      my_path = set([p])
      #coind_constr is a dictionary with coref as key, and a set of paths as
      #value, add_coref_constraints checks existence of coref and makes value
      #union of set
      td.add_coref_constraint(t.coref, my_path)
  elif isinstance(t, TDLelem_dlist):
    interpret_dlist(t, p, td)
  else:
    if not t is None:
      print "New case scenario: av_type has child:"
      print t


'''Function interpreting dlists
t = TDLelem_dlist
p = string (path of attributes
td = TDLdefined_type for updating
NOTE: This function is underdeveloped at present:
the exact values of dlists are ignored for now, this must change if
1. We start manipulating types on a more detailed level
2. We want to print types based on TDLdefined_type'''

def interpret_dlist(t, p, td):
  val = ''
  if len(t.child) == 0:
    val = '<! !>'
  elif len(t.child) == 1:
    t = t.child[0]
    p += '.LIST.FIRST'
    for tch in t.child:
      if isinstance(tch, TDLelem_type):
        temp_val = process_elementary_type(tch)
        td.add_value(temp_val)
        val = temp_val 
      elif isinstance(tch, TDLelem_feat):
        interpret_embedded_feat(tch, p, td)
      elif isinstance(tch, TDLelem_coref):
        my_p = set([p])
        td.add_coref_constraint(tch.coref, my_p)
  else:
    for c in t.child:
      for gc in c.child:
        if isinstance(gc, TDLelem_type):
          td.add_value(gc.type)
        elif isinstance(gc, TDLelem_feat):
          interpret_embedded_feat(gc, p, td)
        elif isinstance(gc, TDLelem_coref):
          my_p = set([p])
          td.add_coref_constraint(gc.coref, my_p)
  if val:
    td.add_att_val_pair(p, val)


'''goes through file, interprets types and adds them to dictionary'''
def create_type_inventory(file, deftypes):
 
  lines = file.readlines()
  file.close()
  temp_descr = ''
  flag = 0
  for line in lines:
    flag = partOfComment(line, flag)
    if not ((re.match('\s*;', line)) or (flag == 1) or (re.match('.*\|\#', line))):
#Ignoring comments, okay at this stage: just processing the types
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object = TDLparse(description)
#special case 'null' interpreted as object None by python:
        if tdl_object.type == 'null':
          tdl_object.type = '< >'
#some types are processed more than once...
        if tdl_object.type in deftypes:
          tdldef = deftypes[tdl_object.type]
        else:
          tdldef = TDLdefined_type(tdl_object.type, tdl_object.op)
          
        #parse tdl_object to obtain further info on type
        if len(tdl_object.child) == 1:
          t = tdl_object.child[0]
          convert_elem_type_to_type_def(t, tdldef)
          deftypes[tdldef.type.lower()] = tdldef
            
        else:
          print "Problem: general typedef has more than one child"

  return deftypes



'''goes through set of files and builds dictionary of types based on that'''
def build_defined_types_hierarchy(path, type_defs):
  deftypes = {}
  for f in type_defs:
    file = open( path + f)
    create_type_inventory(file, deftypes)
    file.close()
  return deftypes
 

'''Returns true if sub is subtype of sup in thierarchy'''
def is_subtype(sub, sup, thierarchy):
  sts = sub.supertypes
  if sup.type in sts:
    return True
  elif "*top*" in sts:
    return False    
  else:
    for s in sts:
      if s in thierarchy:
        new_s = thierarchy[s]
        if is_subtype(new_s, sup, thierarchy):
          return True
      else:
        print 'Error: ' + s + ' is defined as supertype, but not included in thierarchy'
        return False  




# Function below may be too restrictive, but no counter examples found so far
'''Function that returns first attribute in path of feature declaration'''
def find_introduced_attribute(path):
  ats = path.split('.')
  if len(ats) < 1:
    return path
  else:
    return ats[0]

'''Function that identifies attributes that may be introduced by type (t),
if not introduced already, adds them to set of attributes with their introducer (global atts_and_intro_type)'''
def add_attribute_related_constraints(t, thierarchy):
  global atts_and_intro_type
  atts = set()
  intro_atts = set()
  for av in t.val_constr.iterkeys():
    p = find_introduced_attribute(av)
    atts.add(p)
  for acs in t.coind_constr.itervalues():
    for ac in acs:
      p = find_introduced_attribute(ac)
      atts.add(p)
  for p in atts:
    if not p in atts_and_intro_type:
      atts_and_intro_type[p] = t.type
      intro_atts.add(p)
    else:
      ided_t_name = atts_and_intro_type[p]
      ided_t = thierarchy[ided_t_name]
      if not ided_t.type == t.type:
        stypes = ided_t.supertypes
###TODO: no safety check if types are in sub-super type relation
###nor what to do in that case (has not occurred in grammars so far) 
        if is_subtype(ided_t, t, thierarchy):      
          atts_and_intro_type[p] = t.type
          intro_atts.add(p)
  return intro_atts


'''function that goes 'generation-wise' through type hierarchy, top-down and identifies where attributes are introduced,
i.e. by which type'''
def identify_attribute_introducing_types(thierarchy):
  global top_subtypes
  
  ati = set()
  current_gen = set(top_subtypes)
  old_gen = set()
  i = 0  
  while True:
    if len(current_gen) < 1:
      return False
#security to make walk through not too long
    if i >= 100:
      print "Had to break off search for attributes before going through hierarchy"
      return False
    i += 1 
    next_gen = set()
    for t in current_gen:
      old_gen.add(t)
      my_t = thierarchy[t]
      found_atts = set()
      found_atts = add_attribute_related_constraints(my_t, thierarchy)
      for st in my_t.subtypes:
        if not st in old_gen:
          next_gen.add(st)
    current_gen.clear()
    current_gen = next_gen 


def retrieve_val_from_path_dot(m_av, attr):
  val = ''
  parts = m_av.split('.')
  if attr in parts and len(parts) > 1:
    if parts[0] == attr:
      n_attr = parts[1]
      val = atts_and_intro_type[n_attr]
    else:
      print 'unexpected location for attribute'
  return val


def retrieve_basic_val_from_coref(attr, my_t):
  val = ''
  my_at_cor = my_t.coind_constr
  for my_a in my_at_cor.itervalues():
    for my_p in my_a:
      if '.' in my_p and not val:
        val = retrieve_val_from_path_dot(my_p, attr)
  return val

'''function that retrieves basic value of an attribute'''
def retrieve_basic_val(attr, my_t):
  #get all at_val_pairs
  val = ''
  my_at_val = my_t.val_constr
  if attr in my_at_val:
    val = my_at_val[attr]
  else:
    for m_av in my_at_val.iterkeys():
      while not val:
        val = retrieve_val_from_path_dot(m_av, attr)
###cut function out, call it from here
  if not val: 
    val = retrieve_basic_val_from_coref(attr, my_t)
  if not val:
    print 'Was not able to establish the value: ' + attr
  return val  

'''Function that goes through attributes and retrieves their basic value'''
def retrieve_attributes_basic_values(thierarchy):
  global atts_and_intro_type
  atts_intro_val = {}

  for a in atts_and_intro_type.iterkeys():
    tname = atts_and_intro_type[a]
    my_t = thierarchy[tname]
    val = retrieve_basic_val(a, my_t)
    intro_val = [tname, val]
    atts_intro_val[a] = intro_val

  return atts_intro_val


'''returns true if current location is part of comment'''
def partOfComment(line, flag):
  if re.match('\s*\#\|', line):
            #multiline comment starts
    flag = 1
  elif re.match('.*\|\#', line):
    flag = 0

  return flag


'''function that goes through the type hierarchy and adds information on a types subtype'''
def add_subtype_values(thierarchy):
  for t in thierarchy.itervalues():
    suptypes = t.supertypes
    for s in suptypes:
      if s in thierarchy:
        my_suptype = thierarchy[s]
        my_suptype.add_subtype(t.type)
###note: 'null' as a supertype gets converted to None...
      elif not s == "*top*" and not s == None:
        print s 
        print " is identified as supertype but not defined in the hierarchy" 



def contains_mergeable_chains(chains):
  check_chains = chains[:]
  for my_chain in chains:
    check_chains.remove(my_chain)
    for my_type in my_chain.split('|'):
      print my_type
      for comp_chain in check_chains:
        if my_type in comp_chain:
          return True
  return False



def merge_chains(chains): 
  check_chains = chains[:]
  for my_chain in chains:
    check_chains.remove(my_chain)
    for my_type in my_chain.split('|'):
      for comp_chain in check_chains:
        if my_type in comp_chain:
          all_types = set(my_chain.split('|') + comp_chain.split('|'))
          merged_chain_types = list(all_types)
          merged_chain = ''
          for t in merged_chain_types:
            merged_chain += t + '|'
          merged_chain.rsplit('|')
          return [merged_chain, my_chain, comp_chain]
  return False

def merge_all_related_chains(st_chains):
  merged_chains = st_chains[:]
  
  while contains_mergeable_chains(merged_chains):
    new_chain_and_olds = merge_chains(merged_chains)
    if not new_chain_and_olds:
      break
    else:
      merged_chains.remove(new_chain_and_olds[1])
      merged_chains.remove(new_chain_and_olds[2])
      merged_chains.append(new_chain_and_olds[0])

  return merged_chains
###redefine
###compare chain to all chains in chains_count
###if any overlap, new chain should be combined chains
def merge_corresponding_chain(st_chains):
  toremove_chains = []
  new_chains = st_chains[:]
  chains_count = st_chains[:]
  for chain in st_chains:
    chains_count.remove(chain)
    chparts = chain.split('|')
    chains_to_merge = []
    for my_ch in chains_count:
      mergeable = False
      for my_t in chparts:
        if my_t in my_ch:
          mergeable = True
          break
      if mergeable:
        chains_to_merge.append(my_ch)
    new_chain = ''
    if len(chains_to_merge) > 0:
      print 'blabla'
 
  for ch in toremove_chains:
    new_chains.remove(ch)

  return new_chains




def retrieve_itype(att, line, att_itypes):
  itype = ''
  if att + '.' in line:
    my_atts = line.split('.')
    found = False
    for a in my_atts:
      if a == att:
        found = True
      if found:
        my_new_att = a
        if ' ' in a:
          new_att = a.split(' ')[0]
        else:
          new_att = a
        if '.' in new_att:
          new_att = new_att.split('.')[0]
        itype = att_itypes[new_att]
        return itype
  elif '<' in line:
    my_chunks = line.split('<')
    rel_chunk = my_chunks[1]
    if '[' in rel_chunk:
      parts = rel_chunk.split('[')
      if ']' in parts[1]:
        new_att = parts[1].split(']')[0]
      else:
        new_att = parts[1]
      if ' ' in new_att:
        new_att = new_att.split(' ')[0] 
      if not (new_att == '>' or new_att == '!>'):
        if '.' in new_att:
          new_att = new_att.split('.')[0]
        if new_att:
          itype = att_itypes[new_att]
          return itype
    elif ' ' in rel_chunk:  
      parts = rel_chunk.split(' ')
      if not parts[1] == '>' or parts[1] == '!>':
        itype = parts[1]
      return itype
   

def find_common_supertype(t1, t2, thierarchy):
  t1_supertypes = []
  t2_supertypes = []
  t1_start_sts = t1.supertypes
  t2_start_sts = t2.supertypes

###first check hierarchy at location 1
  for t1_st in t1_start_sts:
    if t1_st in t2_start_sts and not t1_st == '*top*':
      return t1_st

  for t1_st in t1_start_sts:
    if not t1_st == '*top*':
      my_st = thierarchy[t1_st]
      the_com_st = find_common_supertype(my_st, t2, thierarchy)
      if the_com_st:
        return the_com_st
      else:
        t1_supertypes.append(my_st.supertypes)

  for t2_st in t2_start_sts:
    if not t2_st == '*top*':
      my_st = thierarchy[t2_st]
      the_com_st = find_common_supertype(my_st, t1, thierarchy)
      if the_com_st:
        return the_com_st
      else:
        t2_supertypes.append(my_st.supertypes)

  for t2_stlist in t2_supertypes:
    for t2_st in t2_stlist:
      for t1_stlist in t1_supertypes:
        if t2_st in t1_stlist:
          if not t2_st == '*top*':
            return t2_st 

def create_dict_of_begin_types(thierarchy, fg_dicts):
  type_btype = {}
  # print len(fg_dicts)
  # print fg_dicts
  #find out what fg_dicts is as an object and create it from intro val
  for t in thierarchy.keys():
    btype = retrieve_begin_type(t, fg_dicts, thierarchy)
    type_btype[t] = btype

  return type_btype


def retrieve_begin_type(type_name, fg_dicts, thierarchy):
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




def update_supertype_list(chain_parts, t2, thierarchy):
  chain = chain_parts[0]
  found = False
  for x in range(1, len(chain_parts)):
    new_t = chain_parts[x]
    my_new_t = thierarchy[new_t]
    my_t2 = thierarchy[t2]
    if found or is_subtype(my_new_t, my_t2, thierarchy):
      chain += '|' + new_t
    else:
      chain += '|' + t2
      found = True
  return chain

def retrieve_feature_geometry(input_files, output_file_fg, output_file_bt):

  my_typedef_files = input_files[0]
  gram_dir = input_files[1]

  thierarchy = build_defined_types_hierarchy(gram_dir, my_typedef_files)

  #create set of attributes:
  attributes = set()
  for t in thierarchy.itervalues():
    for a in t.attributes:
      attributes.add(a)

  ###creates set of attributes with the type that introduces them
  ###fix so that it actually does this...
  add_subtype_values(thierarchy)
  identify_attribute_introducing_types(thierarchy)
  

  atts_intro_val = retrieve_attributes_basic_values(thierarchy)
 
  fg_file = gram_dir + output_file_fg
  feat_geom = open(fg_file, 'w')
  
  att_rel_types = set()
  for a, v in atts_intro_val.iteritems():
    if '+' in v[0]:
      v[0] = 'head'
    if '+' in v[1]:
      v[1] = 'head'
    feat_geom.write(a + '\t' + v[0] + '\t' + v[1] + '\n')
    att_rel_types.add(v[0])
    att_rel_types.add(v[1])
  
  my_reltypes = list(att_rel_types)
  supert_chains = create_supertype_chains(my_reltypes, thierarchy)
  supert_chains =  remove_sublists(supert_chains)

  print_out_chains = []
  for st_chain in supert_chains:
    print_out_chain = ''
    for type_inc in st_chain:
      print_out_chain += type_inc + '|'
    print_out_chain.rstrip('|')
    print_out_chains.append(print_out_chain)

  for my_c in print_out_chains:
    feat_geom.write(my_c + '\n')

  feat_geom.close()
  
 # feat_geom_file = open(fg_file, 'r')
  fg_dicts = create_feature_geom_objects(fg_file)
  ###create dictionary that identifies begin_type of each type
  begin_types = create_dict_of_begin_types(thierarchy, fg_dicts)

  btypes_f = gram_dir + output_file_bt
  bt_file = open(btypes_f, 'w')

  for t, bt in begin_types.items():
    bt_file.write(t + ':' + bt + '\n')

  bt_file.close()



  #####TODO
  #3. improve functionality for lists (make defaults work)
  
####


def insert_t_if_applicable(my_tt, st_l, thierarchy):
  new_stl = st_l[:]
  for i, st in enumerate(st_l):
    st_t = thierarchy[st]
    if is_subtype(my_tt, st_t, thierarchy):
      new_stl.insert(i, my_tt.type)
      break
    elif not is_subtype(st_t, my_tt, thierarchy):
      break
  return new_stl


def remove_sublists(supert_list):
  red_slist = supert_list[:]
  
  for sl in supert_list:
    for sl2 in supert_list:
      if set(sl).issubset(set(sl2)) and len(sl) != len(sl2):
        if sl in red_slist:
          red_slist.remove(sl)
  return red_slist


def create_supertype_chains(t_list, thierarchy):
  t_list.remove('*top*')
  ts_to_check = t_list[:]
  st_lists = []
  for my_t in t_list:
    ts_to_check.remove(my_t)
    my_tt = thierarchy[my_t]
    updated_stl = []
    no_check = []
    for st_l in st_lists:
      upd_stl = insert_t_if_applicable(my_tt, st_l, thierarchy)
      if len(upd_stl) > len(st_l):
        no_check += st_l
      updated_stl.append(upd_stl)
    st_lists = updated_stl
    for t in ts_to_check:
      if not t in no_check:
        ttype = thierarchy[t]
        if is_subtype(ttype, my_tt, thierarchy):
          st_lists.append([t, my_t])
        elif is_subtype(my_tt, ttype, thierarchy):
          st_lists.append([my_t, t])
  return st_lists
####chains for supertypes

# go through types, create binary supertype pairs

# go through chains recursively, creating longer chains, until no new created






'''Processes pet-input file to identify files that define instances and typefiles'''
def retrieve_input_files(flop_file):
  typedef_files = []

  flop = open(flop_file,'r')
  
  type_flag = False
  instance_flag = False

  for line in flop:
    if ':begin' in line:
      if ':type' in line:
        type_flag = True
      elif ':instance' in line:
        instance_flag = True
    elif ':end' in line:
      type_flag = False
      instance_flag = False
    elif ':include' in line and not ';:include' in line:
      file_name = line.split('"')[1] 
      if not '.tdl' in file_name:
        file_name += '.tdl'
      if type_flag:
        typedef_files.append(file_name)
      elif not instance_flag:
        print 'Warning: possible problem: ' + file_name + ' is defined included, but specified to be part of the type hierarchy or instances.'

  path_parts = flop_file.split('/')
  gram_dir = ''
  for x in range(0, len(path_parts) - 1):
    gram_dir += path_parts[x] + '/'

  input_files = [typedef_files, gram_dir]

  return input_files

#2. create typehierarchy, including attribute introduction


#3. for each introducing feature, each value, retrieve super-type list

def extract_feature_geometry(flop_file):

  input_files = retrieve_input_files(flop_file)
  outfile = 'climb/feature_geometry'
  outfile_bt = 'climb/begin_types'
  retrieve_feature_geometry(input_files, outfile, outfile_bt)




#4. print out in file
def main():

  try:
    opts, args = getopt.getopt(sys.argv[1:], 'n',['nodefaults'])
  except getopt.GetoptError, err:
    print str(err)
    usage()

  if len(args) < 1:
    usage()

  flop_file = args[0]
  # list containing two elements: list of files, grammar_directory
  input_files = retrieve_input_files(flop_file)
  retrieve_feature_geometry(input_files, 'feature_geometry', 'basic_types')

'''Print an appropriate usage message and exit.'''
def usage():

  indent = 0

  something_printed = False

  examples = []
  def p(msg, nobreak=False):

    if nobreak:
      print " " * indent + msg,
    else:
      print " " * indent + msg

  p("Usage: extract_feature_geometry.py INPUT (flop_input_file_name)", nobreak=True)


#process_script("../../../jacy-spring-cleaned")
if __name__ == '__main__':
  main()

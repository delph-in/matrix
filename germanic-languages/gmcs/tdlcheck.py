###checks well-formedness of tdl hierarchy
# has additional functions
# 1. only add instantiated types

##
# to do:
# 1. add values to all co-indexed paths... (#x & y -> y needs to be added to all other paths as value)
# function going through tree at the end (similar to adding subtypes...): before subtypes...
# 2. Go through hierarchy, for each supertype, collect constraints for its subtype
# -> other way around step 1 would have to be taken again after 2...


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
top_subtypes = set()
atts_and_intro_type = {}
supts_comm_subts = {}

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

def partOfComment(line, flag):
  if re.match('\s*\#\|', line):
            #multiline comment starts
    flag = 1
  elif re.match('.*\|\#', line):
    flag = 0

  return flag


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
        for v in defined[rit].compl_val_constr.itervalues():
          if v not in my_types:
            my_types.append(v)
#for supertypes of defined type: add current type as subtype
#add to my_types list
        for s in defined[rit].supertypes:
          if not s in defined and not s == "*top*":
            print s + ' listed as supertype not found in hierarchy'
          my_types.append(s)

        temp_rit = set(my_types)
         
  #    identify_objects(defined[rit].child, temp_rit, attributes)
        for nt in temp_rit:
          if not (nt in identified or nt in requested):
            new_rit.add(nt)
        temp_rit.clear()
    elif not rit == '*top*' and not re.match('<!*', rit):
      print "Problem: somehow the following type is requested but not defined:"
      print rit
  if len(new_rit) > 0:
    walk_through_instantiated(new_rit, identified, defined, atts)


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
    if not t.type == 'null':
      my_val = process_elementary_type(t)
      td.add_value(my_val)
      td.add_att_val_pair(p, my_val)
  elif isinstance(t, TDLelem_coref):
    if len(t.child) > 0:
      print "Problem: coreference type occurring with children"
    else:
      my_path = set([p])
      td.add_coref_constraint(t.coref, my_path)
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
###note: 'null' as a supertype gets converted to None...
      elif not s == "*top*" and not s == None:
        print s 
        print " is identified as supertype but not defined in the hierarchy" 

def add_inherited_constraints(thierarchy):
  global top_subtypes

  for t in top_subtypes:
    set_complete_constraints(t, thierarchy)

def is_difflist(t):
  if re.match('<!*', t):
    return True
  else:
    return False



def set_complete_constraints(t, thierarchy):
  global supts_comm_subts
  my_t = thierarchy[t]
  for sbt in my_t.subtypes:
    ct = thierarchy[sbt]
    if len(ct.compl_val_constr) == 0 and len(ct.compl_coind_constr) == 0:
#call attribute function here (is to be used once...)
      add_avs_introduced_by_atts(ct, thierarchy)
      ct.compl_val_constr = dict(ct.val_constr)
      ct.compl_coind_constr = dict(ct.coind_constr)
    for vc in my_t.compl_val_constr.iterkeys():
      if vc in ct.compl_val_constr:
        n_val = ct.compl_val_constr[vc]
        o_val = my_t.compl_val_constr[vc]
        if not o_val == n_val:
          if not is_difflist(n_val) and not o_val == "*top*" and not is_difflist(o_val):
            n_t = thierarchy[n_val]
            old_t = thierarchy[o_val]
            if not is_subtype(n_t, old_t, thierarchy):
              if is_subtype(old_t, n_t, thierarchy):
                if vc in ct.val_constr:
                  if vc in ct.values:
                    print "more specific constraint was found on supertype of: " + ct.type + ":\n"
                    print n_val + " introduced on lower type than " + o_val + "\n"
                else:
                  ct.compl_val_constr[vc] = o_val
              else:
                jointtype = n_val + o_val
                if not jointtype in supts_comm_subts:
                  assign_subt_seem_confl_vals(n_t, old_t, thierarchy)
                val = supts_comm_subts[jointtype]
                ct.add_av_pair_tocl(vc, val)
       #   else:
       #     print "Diff-list issue for: " + sbt + " old value: " + o_val
      else:
        val = my_t.compl_val_constr[vc]
        ct.add_av_pair_tocl(vc, val)
        #ct.compl_val_constr[vc] = my_t.val_constr[vc]

    set_complete_constraints(sbt, thierarchy)


def assign_subt_seem_confl_vals(t1, t2, thierarchy):
  search = t1.type + t2.type
  if not search in supts_comm_subts:
    comm_sts = find_common_subtype(t1, t2, thierarchy)
    if len(comm_sts) > 1:
      found1 = t1.type + t2.type
      found2 = t2.type + t1.type
###################to do: must determine somehow which type to take....HACK we know this occurs in infl-left-coord-rule, where it must be the second
      supts_comm_subts[found1] = comm_sts[1]
      supts_comm_subts[found2] = comm_sts[1]
    elif len(comm_sts) ==1:
      found1 = t1.type + t2.type
      found2 = t1.type + t2.type
      supts_comm_subts[found1] = comm_sts[0]
      supts_comm_subts[found2] = comm_sts[0]
###########HACK2: now just randomly assigning one of the two types, when nothing
#found
    else:
      supts_comm_subst[found1] = t1.type
      supts_comm_subst[found2] = t2.type

def find_common_subtype(t1, t2, thierarchy):
  commontypes = []
  for sbt in t1.subtypes:
    st = thierarchy[sbt]
    if t2.type in st.supertypes:
      commontypes.append(sbt)
  if len(commontypes) > 0:
    return commontypes
  else:
    ct1 = search_hierarchy_for_common_st(t1, t2, thierarchy)
    ct2 = search_hierarchy_for_common_st(t2, t1, thierarchy)
    same = True
    for e in ct1:
      if e in ct2:
        ct2.remove(e)
      else:
        same = False
    if len(ct2) > 0:
      same = False
    if same:
      return ct1
    else:
      print "Method leads to different results depending on order..."
      return commontypes


def search_hierarchy_for_common_st(t1, t2, thierarchy):
  c_subts = []
  for sbt in t1.subtypes:
    st = thierarchy[sbt]
    if is_subtype(st, t2, thierarchy):
      c_subts.append(sbt)
  if len(c_subts) > 0:
    return c_subts
  else:
    for sbt in t1.subtypes:
      st = thierarchy[sbt]
      c_subts = search_hierarchy_for_common_st(st, t2, thierarchy)
    if len(c_subts) > 0:
      return c_subts

def add_avs_introduced_by_atts(t, thierarchy):
  paths = collect_paths(t)
  for p in paths: 
    determine_type_based_on_att(p, t, thierarchy)

def collect_paths(t):
  paths = set()
  for p in t.val_constr.iterkeys():
    paths.add(p)
  for v in t.coind_constr.itervalues():
    for p in v:
      paths.add(p)
  return paths

def determine_type_based_on_att(path, t, thierarchy):
  global atts_and_intro_type
  atts = path.split('.')
  if len(atts) > 1:
    new_path = ""
    for i in range(len(atts) - 1):
      new_path += atts[i]
      val = atts_and_intro_type[atts[i+1]]
      if not new_path in t.val_constr:
        t.add_att_val_pair(new_path, val)
      elif not val == t.val_constr[new_path]:
        old_val = t.val_constr[new_path]
        if not is_difflist(old_val):
          nv = thierarchy[val]
          ov = thierarchy[old_val]
          if is_subtype(nv, ov, thierarchy):
            t.val_constr[new_path] = val
          elif not is_subtype(ov, nv, thierarchy): 
            jointtype = val + old_val
            if not jointtype in supts_comm_subts:
              assign_subt_seem_confl_vals(nv, ov, thierarchy)
            val = supts_comm_subts[jointtype]
            t.val_constr[new_path] = val

      new_path += "."


def determine_sub_sup_type(t1, t2, th):
  if is_subtype(t1, t2, th):
    return [t1, t2]
  elif is_subtype(t2, t1, th):
    return [t1, t2]
  else:
    print t1 + " " + t2 + " are assigned to the same type"
    print "but do not have a subtype-supertype relation..."
    return []
 

def is_subtype(sub, sup, thierarchy):  
  sts = sub.supertypes
  if sup.type in sts:
    return True
  elif "*top*" in sts:
    return False    
  else:
    for s in sts:
      new_s = thierarchy[s]
      if is_subtype(new_s, sup, thierarchy):
        return True



def identify_attribute_introducing_types(thierarchy):
  global top_subtypes
  ati = set()
  current_gen = set(top_subtypes)
  old_gen = set()
  i = 0

  
  while True:
    if len(current_gen) < 1:
      return False
    if i >= 100:
      return False
    i += 1 
    next_gen = set()
    for t in current_gen:
      old_gen.add(t)
      my_t = thierarchy[t]
      found_atts = set()
      found_atts = add_attribute_related_constraints(my_t, ati, thierarchy)
      for st in my_t.subtypes:
        if not st in old_gen:
          next_gen.add(st)
    current_gen.clear()
    current_gen = next_gen 

def introduce_attributes(t, thierarchy, atts_to_ignore): 
  found_atts = set() 
  found_atts = add_attribute_related_constraints(t, atts_to_ignore, thierarchy)
  for fa in found_atts:
    atts_to_ignore.add(fa)
  for st in t.subtypes:
    my_st = thierarchy[st]
    introduce_attributes(my_st, thierarchy, atts_to_ignore)
  for a in found_atts:
    if a in atts_to_ignore:
      atts_to_ignore.remove(a)

def add_attribute_related_constraints(t, a_to_ignore, thierarchy):
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
        if is_subtype(ided_t, t, thierarchy):      
          atts_and_intro_type[p] = t.type
          intro_atts.add(p)
  return intro_atts



def add_values_of_coindexed_constr(thierarchy):
  for t in thierarchy.itervalues():
    if len(t.compl_val_constr) > 0 and len(t.compl_coind_constr) > 0:
      coinds = t.compl_coind_constr
      for c in coinds.itervalues():
        for p in c:
          if p in t.compl_val_constr:
            add_path_value_pairs(p, c, t.compl_val_constr, thierarchy)


def add_path_value_pairs(p, c, compl_val_constr, th):
  myval = compl_val_constr[p]
  for ap in c:
    if not ap == p:
      if not ap in compl_val_constr:
        compl_val_constr[ap] = myval
      else:
        pres_val = compl_val_constr[ap]
        if not pres_val == myval:
          pv = th[pres_val]
          mv = th[myval]
          if is_subtype(mv, pv, th):
            compl_val_constr[ap] = myval
          elif not is_subtype(pv, mv, th):
            print "To fix: found values assigned by coindeces that are not in a sub-supertype-relation"

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
  if len(ats) < 1:
    return path
  else:
    return ats[0]
    

def identify_attribute_intro_types(identified, typehierarchy, attrs, new_requests):
  global top_subtypes
  current_generation = top_subtypes
  count_down_attrs = attrs
  new_requests.clear()
  i = 0

###exception for HEAD-DTR introduced by headed-phrase
#  if 'headed-phrase' in identified:
#    if 'HEAD-DTR' in count_down_attrs:
#      count_down_attrs.remove('HEAD-DTR')

  while True:
    if len(current_generation) < 1:
      return False
    if len(count_down_attrs) == 0:
      return False
    #temporary security if this takes too long (attributes of this kind are typically high up in the hierarchy)
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


def remove_redundant_labels(file, iset, alist):

  lines = file.readlines()
  file.close()
  output = open(file.name, 'w')
  temp_descr = ''
  flag = 0
  for line in lines:
    
    flag = partOfComment(line, flag)

    if not ((re.match('\s*;', line)) or (flag == 1)):

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
        keep = True
        for a in tdldef.attributes:
          if not a in alist:
            keep = False
        for v in tdldef.values:
          if not v in iset:
            keep = False
        if keep:
          output.write(description + '\n')
  output.close()

####depending on structure: change name of function...
def process_instantiation_files(path, inst, type_defs):

  global top_subtypes
  top_subtypes.clear()
  global atts_and_intro_type
  atts_and_intro_type.clear()
  typehierarchy = {}
  for dtf in type_defs:
    file = open ( path + '/' + dtf)
#    separate_instantiated_non_instantiated(file, instantiatedset, superflset)
    create_type_inventory(file, typehierarchy)
    file.close()  
   

  attributes = set()
  itypes = {}
  for f in inst:
    file = open( path + '/' + f )
    create_type_inventory(file, itypes) 
    file.close


  add_subtype_values(typehierarchy)
  identify_attribute_introducing_types(typehierarchy)
  add_inherited_constraints(typehierarchy)
  add_values_of_coindexed_constr(typehierarchy)
  
  ###checking if instantiated types contain attributes that are not introduced
  #call function that starts at current_generation, checks for ATTR adds subtypes to new generation
  instantiatedset = identify_inst_types(itypes, typehierarchy, attributes)
  new_reqs = {}
#  identify_attribute_intro_types(instantiatedset, typehierarchy, attributes, new_reqs)
#  print len(attributes)
#  print "identify attribute_intro_types"  

  output = open("check_a_intro.txt", 'w')
  for k, v in atts_and_intro_type.iteritems():
    output.write(k + " " + v + "\n")

  output.close()
#  transverb = typehierarchy['transitive-verb-lex']

  superflset = set()
  for k in typehierarchy.iterkeys():
    if not k in instantiatedset:
      superflset.add(k)

  for tf in type_defs:
    file = open ( path + '/' + tf)
    separate_instantiated_non_instantiated(file, instantiatedset, superflset)
    file.close()  
    

  script = open(path + "/lkb/script")
  labelsfile = identify_label_file(script) 
  file = open (path + '/' + labelsfile)
  remove_redundant_labels(file, instantiatedset, attributes)

  
  print "identified: " 
  print len(superflset)
  print " unused types."
 
  typehierarchy.clear()
  superflset.clear()
  instantiatedset.clear()
  attributes.clear()
  itypes.clear()


def process_script(path):
  script = open( path + '/lkb/script' )   
  inst = identify_instantiation_files(script)
  script.close()
  script = open( path + '/lkb/script' )
  type_defs = identify_type_def_files(script)
  script.close()
  process_instantiation_files(path, inst, type_defs)



##########################################################
#
# Functions that are handling the files
#

#
# Retrieving information from files
#

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


#
# Identifying files based on scripts
#

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


def identify_label_file(script):
  script_lines = script.readlines()
  for line in script_lines:
    if re.match('\(read-tdl-parse-node-file-aux', line):
      parts = re.split('\"', line)
      if len(parts) > 1:
        return parts[1]
  return ""

###########################################################
#
# Fuctions that retrieve information from TDLtypedef
#



###########################################################
#
# Functions that interpret objects from tdl.py
#


'interprets children of TDLelem_typedef and adds them to object of type TDLdefined_type'
def convert_elem_type_to_type_def(elem, typedef):
  for ch in elem.child:
#check for not embedded elementary types: these are supertypes
    if isinstance(ch, TDLelem_type):
      st = process_elementary_type(ch)
      typedef.add_supertype(st)
      if st == "*top*":
        top_subtypes.add(typedef.type)
#call recursive function for all other children
    else:
      interpret_elemtype_children(ch, typedef)


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



#####################################################################
#
# Function that are currently not used
#

'''Takes input file, identifies type definitions and adds them to a provided dictionary, where the type-name is the key and the TDLelem as defined in tdl.py is its value. 'atts' registers attribute names found in the type definitions'''
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


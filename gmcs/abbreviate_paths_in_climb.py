
import os
import re
import tdl
import copy
import shutil
from tdl import TDLparse_abbr
from tdl import TDLwrite
from tdl import TDLmergeable
from tdl import TDLmerge

'''creates dictionary with feature-values as keys and potential attributes as value '''
def process_for_indexing_types(feat_file):

  feat_geom = open(feat_file, 'r')
  def_feats = feat_geom.readlines()
  my_ind_types = {}
  my_chains = []
  for f_def in def_feats:
    my_parts = f_def.split()
    if len(my_parts) > 2:    
      key = my_parts[2]
      val = my_parts[0]
      if key in my_ind_types:
        my_val = my_ind_types.get(key)
        if not ',' + val in my_val or val + ',' in my_val:
           my_val += ',' + val
      else:
        my_val = val
      my_ind_types[key] = my_val
    else:
      my_chains.append(my_parts[0])
  if not 'supertype-chain' in my_ind_types:
    my_ind_types['supertype-chain'] = my_chains
  else:
    print 'Error: some attribute called \'supertype-chain\' in grammar, cannot create supertype chain for path abbreviation' 
  feat_geom.close()
  return my_ind_types






'''creates dictionary with attributes as KEY and list of intro type and feature-value as its value '''
def process_for_feature_names(feat_file):
 # print feat_file
 # print '++++++++++++++++++++++++'
  feat_geom = open(feat_file, 'r')
  def_feats = feat_geom.readlines()
  my_feats = {}
  for f_def in def_feats:
    my_parts = f_def.split()
    if len(my_parts) > 2:
      key = my_parts[0]
      val = []
      val.append(my_parts[1]) 
      val.append(my_parts[2])
      my_feats[key] = val
  feat_geom.close()
  return my_feats



def create_feature_geom_objects(feat_file):
  my_att_val_dict = process_for_feature_names(feat_file)
  my_val_att_list_dict = process_for_indexing_types(feat_file)
  my_chains = my_val_att_list_dict.pop('supertype-chain')

 # for k, vals in my_val_att_list_dict.items():
 #   print k
 #   print vals

####TAKE COMMAS INTO ACCOUNT AND CHECK IF IS CHAIN ALREADY
  
 #replace values attribute-look-up by their supertype chains
  for ts in my_att_val_dict.itervalues():
    for ch in my_chains:
      if ts[0] in ch:
        ts[0] = ch

    if not ',' in ts[1]:
      for ch in my_chains:
        if ts[1] in ch:
          ts[1] = ch
    elif not '|' in ts[1]:
      new_ts1 = ''
      ts_parts = ts[1].split(',')
      for t in ts_parts:
        if t in ch:
          new_ts1 += ch + ','
        else:
          new_ts1 += t + ','
      new_ts1.rstrip(',') 
      ts[1] = new_ts1

  #replace keys type look-up by their supertype chains
  cleaned_dict = {}
  for t in my_val_att_list_dict.iterkeys():
    in_chain = False
    if not t == 'avm':
      for ch in my_chains:
        if t in ch:
          my_val = my_val_att_list_dict[t]
          if not ch in cleaned_dict:
            cleaned_dict[ch] = my_val
          else:
            cleaned_dict[ch] += ',' + my_val
          in_chain = True
    if not in_chain:
      cleaned_dict[t] = my_val_att_list_dict[t]


  for ts in my_att_val_dict.values():
    if not ts[1] in cleaned_dict:
      ts_imp = ts[1]
      ts_imp = ts_imp.replace('avm|','')
      new_val = retrieve_partial_values(ts_imp, my_val_att_list_dict)
      cleaned_dict[ts[1]] = new_val 
    if not ts[0] in cleaned_dict:
      ts_imp = ts[0]
      ts_imp = ts_imp.replace('avm|','')
      new_val = retrieve_partial_values(ts_imp, my_val_att_list_dict)
      cleaned_dict[ts[0]] = new_val

  return [my_att_val_dict, cleaned_dict]

  #replace all types by their chains, if they are in one


def retrieve_partial_values(ts_imp, my_val_att_list_dict):
  my_types = ts_imp.split('|')
  new_val = ''
  for t in my_types:
    if t in my_val_att_list_dict:
      new_val += my_val_att_list_dict[t] + ','
  new_val.rstrip(',')
  return new_val

#function processing types (take from spring cleaning)


'''returns true if current location is part of comment'''
def partOfComment(line, flag):
  if re.match('\s*\#\|', line):
            #multiline comment starts
    flag = 1
  elif re.match('.*\|\#', line):
    flag = 0

  return flag


def inIrulesSection(line, irules):
  if irules:
    if 'File=lexicon' in line or 'File=roots' in line:
      return False
  else:
    if 'File=irules' in line:
      return True
  return irules  



def reduce_paths_in_file(tdl_file, fg_dicts):
  my_tdl = open(tdl_file, 'r')
  lines = my_tdl.readlines()
  my_tdl.close()
  feat_2_type = fg_dicts[0]
  type_2_feat = fg_dicts[1]
  grammar_path = tdl_file.split('/climb/')[0]
 ####CREATE TDL OBJECT, ADD TYPES AND OTHER STUFF AS LITERALS
 ####SAVE TDL OBJECT


  my_new_tdl =  tdl.TDLfile(grammar_path + '/climb/temp_tdl.tdl')
  flag = 0
  i = 0
  temp_descr = ''
  comment = ''
  irules = False
  irule = ''
  for line in lines:
    
    flag = partOfComment(line, flag)
    irules = inIrulesSection(line, irules)
    

    if irule and line == '\n':  
      my_new_tdl.add_literal(irule)
      irule = ''

    if irules:
      irule += line
    elif not ((re.match('\s*;', line)) or (flag == 1) or (re.match('.*\|\#', line))):

#TODO in the following line I ignore comments 
#appearing in a description itself
#INSTEAD: use groups before ';' and after
#before is line, after is stored as comment
      if comment:
        my_new_tdl.add_literal(comment)
        comment = ''
      line = re.sub( r';.*', '', line )
      temp_descr += line
#search for potential end of description
      if re.search('\.\s*\n',  line):
        description = temp_descr
        temp_descr = ''
        tdl_object_fg_dicts = TDLparse_abbr(description, fg_dicts)
        tdl_object = tdl_object_fg_dicts[0]
        fg_dicts = tdl_object_fg_dicts[1] 
        handled = False
        for i in range(len(my_new_tdl.typedefs) - 1, -1, -1):
          if TDLmergeable(my_new_tdl.typedefs[i], tdl_object):
            my_new_tdl.typedefs[i] = TDLmerge(my_new_tdl.typedefs[i], tdl_object)
            handled = True
            break
        if not handled:
          my_new_tdl.typedefs.append(tdl_object)

        #my_new_tdl.typedefs.append(tdl_object)
        #retrieve paths of tdl_object and reduce them
        #print newly defined type PRINT TYPE
    else:
      if not ';;; -*- Mode: TDL; Coding: utf-8 -*-' in line:
        comment += line
      else:
        comment += '\n'
  #else, get type, process type, retrieve paths, reduce paths, print type
  #move new_file to old file (overwrite old file)
  if comment:
    my_new_tdl.add_literal(comment)
  #in case last line is part of irule
  if irule:  
    my_new_tdl.add_literal(irule)
  my_new_tdl.save()
  shutil.move(grammar_path + '/climb/temp_tdl.tdl',tdl_file)
  return fg_dicts

def get_tdl_files(my_dir):
  extension = ".tdl"
  list_of_files = [file for file in os.listdir(my_dir) if file.lower().endswith(extension)]
  return list_of_files


def update_feat_geom_file(feat_geom_file, fg_dicts):
  defaults = set()
#create list of atts that have default
  for k, v in fg_dicts[1].items():
    if ';' in v:
      atts_to_update = v.split(';')[1:]
      for a in atts_to_update:
        defaults.add(a)

  fg_in = open(feat_geom_file, 'r')
  lines = fg_in.readlines()
  fg_in.close()
  fg_out = open(feat_geom_file, 'w')

  for line in lines:
    parts = line.split()
    if parts[0] in defaults:
      fg_out.write(parts[0] + '\t' + parts[1] + '\t' + fg_dicts[0][parts[0]][1] + '\n')
    else:
      fg_out.write(line)
  fg_out.close()



def abbreviate_paths(climb_dir):

  feat_geom_file = climb_dir + 'feature_geometry'
  fg_dicts = create_feature_geom_objects(feat_geom_file)
  tdl_files = get_tdl_files(climb_dir)
  for tdl_file in tdl_files:
    fg_dicts = reduce_paths_in_file(climb_dir + tdl_file, fg_dicts)
  update_feat_geom_file(feat_geom_file, fg_dicts)  


if __name__ == "__main__":
  abbreviate_paths('/home/antske/test/climb/')

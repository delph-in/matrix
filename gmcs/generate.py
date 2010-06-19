##########################################################
# This file contains functions for extracting predications
# from grammar files, generating MRSs, and generating
# sentences from those MRSs

import re
import os

# MRS templates that will be filled in before generation
MRS_itv = '''
[ LTOP: h1
INDEX: e2 [ e SF: PROP-OR-QUES SORT: SEMSORT ]
RELS: <
  [ "#DET1#"
  LBL: h3
  ARG0: x4 [ x SORT: SEMSORT COG-ST: COG-ST SPECI: BOOL ]
  RSTR: h5
  BODY: h6 ]
  [ "#NOUN1#"
  LBL: h7
  ARG0: x4 ]
  [ "#VERB#"
  LBL: h1
  ARG0: e2
  ARG1: x4 ] >
HCONS: < h5 qeq h7 > ]
'''

MRS_stv = '''
[ LTOP: h1
INDEX: e2 [ e SF: PROP-OR-QUES SORT: SEMSORT ]
RELS: <
  [ "#DET1#"
  LBL: h3
  ARG0: x4 [ x SORT: SEMSORT SPECI: BOOL COG-ST: COG-ST  ]
  RSTR: h5
  BODY: h6 ]
  [ "#NOUN1#"
  LBL: h7
  ARG0: x4 ]
  [ "#VERB#"
  LBL: h1
  ARG0: e2
  ARG1: x4
  ARG2: x8 [ x SORT: SEMSORT COG-ST: COG-ST SPECI: BOOL ] ]
  [ "#DET2#"
  LBL: h9
  ARG0: x8
  RSTR: h10
  BODY: h11 ]
  [ "#NOUN2#"
  LBL: h12
  ARG0: x8 ] >
HCONS: < h5 qeq h7 h10 qeq h12 > ] 
'''

#Generate sentences from mrs files
def generate_sentences(grammar, mrs_files, verb_preds, delphin_dir):
  lkb_input = open('lkb_input','w')
  lkb_input.write('(read-script-file-aux "%s/lkb/script")' % (grammar))
  lkb_input.write('(setf *maximum-number-of-edges* 10000)')
  for file in mrs_files:
    lkb_input.write('(null (print (generate-from-mrs (mrs::read-mrs-from-file "%s"))))' % (file))
  lkb_input.flush()
  output = os.popen('cat lkb_input | %s | sed -n "/^LKB/,/EOF$/p"' % (delphin_dir + '/bin/lkb'))
  lkb_input.close()
  os.remove('lkb_input')
  sentences = []
  index = 0
  string = ""
  in_output = False
  lkb_re = re.compile(r'LKB\(([0-9]+)\)')
  for line in output:
    #print line+"<br>"
    m = lkb_re.match(line)
    if m:
      i = int(m.group(1)) - 3
      if i >= 0 and line.find("EOF") == -1:
        sentences.append([[mrs_files[i]+":",verb_preds[i][0],verb_preds[i][1]],[]])
    if line.find('*maximum-number-of-edges*') > -1:
      sentences[index][1].append('#EDGE-ERROR#')
      index += 1
    if line.find('((') >= 0:
      in_output = True
    if in_output:
      string += line
      if re.compile(r'(\)([. ]*)\)\s*)|(\.\.\.\))').search(line):
        in_output = False
        new_lines = re.split(r'\)\s*\(',string)
        string = ""
        for sent in new_lines:
          sentences[index][1].append(unicode(sent.lstrip('( \n').rstrip(') .\n').replace('"',''), 'utf-8').lower().encode('utf-8'))
        index += 1
  return sentences

def remove_duplicates(list):
  new_list = {}
  for k in list.iterkeys():
    insert = True
    for v in new_list.itervalues():
      if v[0] == list[k][0]: insert = False
    if insert: new_list[k] = list[k]
  return new_list

# Extract predications from the grammar
def get_n_predications(grammar_dir):
  lexicon = open(grammar_dir+'/lexicon.tdl','r')
  choices = open(grammar_dir+'/choices','r')
  lang = None
  pred_re = re.compile(r'noun([0-9]+)_stem[0-9]+_pred')
  det_re = re.compile(r'noun([0-9]+)_det')
  det_rel_re = re.compile(r'det[0-9]+_stem[0-9]+_pred')
  det_rel = None;
  noun_rels_dets = {}
  for line in choices:
    pline = line.lstrip().split('=')
    m1 = pred_re.match(pline[0])
    m2 = det_re.match(pline[0])
    m3 = det_rel_re.match(pline[0])
    if pline[0] == 'language':
      lang = grammar_dir+'/'+pline[1].lower().rstrip()+'.tdl'
    if m1:
      if m1.group(1) in noun_rels_dets:
        noun_rels_dets[m1.group(1)][0] = pline[1].rstrip()
      else:
        noun_rels_dets[m1.group(1)] = [pline[1].rstrip(),None]        
    if m2:
      if m2.group(1) in noun_rels_dets:
        noun_rels_dets[m2.group(1)][1] = pline[1].rstrip()
      else:
        noun_rels_dets[m2.group(1)]  = [None,pline[1].rstrip()]
    if m3:
      det_rel = pline[1].rstrip()
  map_noun_rels_dets = remove_duplicates(noun_rels_dets)
  noun_rels_dets = []
  for v in map_noun_rels_dets.itervalues(): noun_rels_dets.append(v)
  noun_rels = []
  det_rels = []
  for pair in noun_rels_dets:
    noun_rels.append(pair[0])   
    if pair[1] == 'obl' and not det_rel == None:
      det_rels.append(det_rel)
    else:
      det_rels.append('exist_q_rel')
  lexicon.close()
  choices.close()
  return(noun_rels,det_rels,lang)

def get_v_predications(grammar_dir,lang):
  itv_rels = []
  stv_rels = []
  verbs = {}
  cur_type = ""
  used_types = []
  p1 = re.compile(r'(\S*verb-lex)')
  p2 = re.compile(r'PRED \"(\S*)\"')
  lexicon = open(grammar_dir+'/lexicon.tdl','r')
  for line in lexicon:
    m1 = p1.search(line)
    m2 = p2.search(line)
    if m1:
      cur_type = m1.group(1)
    if m2 and cur_type != "" and cur_type not in used_types:
      verbs[m2.group(1)] = [cur_type]
      used_types.append(cur_type)
      cur_type = ""
  p = re.compile(r'(\S*) := (.*)')
  while verbs != {}:
    language = open(lang,'r')
    for line in language:
      if line.find(":=") > -1:
        for verb in verbs.keys():
          for type in verbs[verb]:
            if line.find(type) == 0:
              verbs[verb].remove(type)
              new = p.match(line).group(2).split('&')
              new = [x.lstrip().rstrip('. \t\n') for x in new]
              if '' in new:
                new.remove('')
              verbs[verb] = verbs[verb] + new
              if 'intransitive-lex-item' in verbs[verb]:
                itv_rels.append(verb)
                del verbs[verb]
              elif 'transitive-lex-item' in verbs[verb]:
                stv_rels.append(verb)
                del verbs[verb]
    language.close()
  lexicon.close()
  return (itv_rels,stv_rels)

# Output an mrs file from a template, replacing the appropriate predications
def process_mrs_file(mrs, outfile, noun1_rel, det1_rel, noun2_rel, det2_rel, verb_rel):
  output = mrs.replace("#NOUN1#",noun1_rel).replace("#NOUN2#",noun2_rel).replace("#VERB#",verb_rel).replace("#DET1#",det1_rel).replace("#DET2#",det2_rel)
  #print output+"<br><br>"
  f = open(outfile,'w')
  f.write(output)
  f.close()

# Wrap up all of the components involved in generation, and return the results
def get_sentences(grammar_dir,delphin_dir,session):
  (noun_rels,det_rels,language) = get_n_predications(grammar_dir)
  (itvs,stvs) = get_v_predications(grammar_dir,language)
  if len(noun_rels) < 2:
    noun_rels.append(noun_rels[0])
    det_rels.append(det_rels[0])
  i = 0
  mrs_files = []
  for verb_rel in itvs:
    i += 1
    mrs_files.append(session+'Verb ' + str(i))
    process_mrs_file(MRS_itv,session+'Verb '+str(i),noun_rels[0],det_rels[0],noun_rels[1],det_rels[1],verb_rel)
  for verb_rel in stvs:
    i += 1
    mrs_files.append(session+'Verb ' + str(i))
    process_mrs_file(MRS_stv,session+'Verb '+str(i),noun_rels[0],det_rels[0],noun_rels[1],det_rels[1],verb_rel)
  sentences = generate_sentences(grammar_dir, mrs_files, [[x,"Intrasitive verb phrase"] for x in itvs]+[[x,"Transitive verb phrase"] for x in stvs], delphin_dir)
  for file in mrs_files:
    os.remove(file)
  return sentences

# Same as get_sentences, but for the additional sentences page
def get_additional_sentences(grammar_dir,delphin_dir,verb_rel,session):
  (noun_rels,det_rels,language) = get_n_predications(grammar_dir)
  (itvs,stvs) = get_v_predications(grammar_dir,language)
  if verb_rel in itvs:
    mrs = MRS_itv
  else:
    mrs = MRS_stv
  mrs_files = []
  if verb_rel in itvs:
    for i in range(len(noun_rels)):
      mrs_files.append(session+'noun' + str(i))
      process_mrs_file(mrs,session+'noun'+str(i),noun_rels[i],det_rels[i],"","",verb_rel)
  else:
    for i in range(len(noun_rels)):
      for j in range(len(noun_rels)):
        mrs_files.append(session+'noun' + str(i) + "_" + str(j))
        process_mrs_file(mrs,session+'noun'+str(i) + "_" + str(j),noun_rels[i],det_rels[i],noun_rels[j],det_rels[j],verb_rel)
  sentences_with_info = generate_sentences(grammar_dir, mrs_files, [[verb_rel,""]] * len(mrs_files), delphin_dir)
  sentences = []
  for i in range(len(sentences_with_info)):
    sentences.extend(sentences_with_info[i][1])
  for file in mrs_files:
    os.remove(file)
  return sentences

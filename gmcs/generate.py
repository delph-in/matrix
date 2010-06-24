##########################################################
# This file contains functions for extracting predications
# from grammar files, generating MRSs, and generating
# sentences from those MRSs

import re
import os
import shutil

#(tsdb:tsdb :cpu :temp :file t :count 1 :error :exit :wait 1200)
#(setf target
#  (format
#    nil "~a~a"
#    (tsdb::suggest-test-run-directory "target" :absolute nil)
#    "_foo"))



tsdb = '''
(setf (system:getenv "DISPLAY") nil)
(tsdb::tsdb :home "/dw22/d50/dpmills/home")
(setf *tsdb-skeleton-directory* "/dw22/d50/dpmills/skeletons")
(tsdb::tsdb :skeletons "/dw22/d50/dpmills/skeletons")
(tsdb:tsdb :create "target" :skeleton "skeleton")
(tsdb::tsdb-do-process "target" :type :translate :overwrite t :gold "profile")
'''

display_gen_results_fn = '''
(defun TbG-gen-results nil
  (format t "~&new-pattern")
  (if *gen-record*
      (loop for edge in *gen-record*
	  do
	    (format t "~&start-entry")
	    (pprint (edge-string edge))
            (format t "~&parse")
	    (pprint (parse-tree-structure edge))
            (format t "~&mrs")
            (mrs::output-mrs1 (mrs::extract-mrs edge) 'mrs::indexed t)
	    (format t "~&end-entry")
	  finally
	    (force-output)
	    (terpri))
    (format t "~&No strings generated")))
'''

#Generate sentences from mrs files
#DEPRECATED
def old_generate_sentences(grammar, mrs_files, verb_preds, delphin_dir):
  lkb_input = open('lkb_input','w')
  #lkb_input.write('(load \"%s/lkb/src/general/defsystem.lisp\")(load \"%s/lkb/src/general/loadup.lisp\")(load-system \"tsdb\")' % (delphin_dir,delphin_dir))
  lkb_input.write('(read-script-file-aux "%s/lkb/script")' % (grammar))
  lkb_input.write('(setf *maximum-number-of-edges* 10000)')
  #lkb_input.write(tsdb)
  for file in mrs_files:
    lkb_input.write('(null (print (generate-from-mrs (mrs::read-mrs-from-file "%s"))))' % (file))
  lkb_input.flush()
  output = os.popen('cat lkb_input | %s | sed -n "/^LKB/,/EOF$/p"' % (delphin_dir + '/bin/lkb'))
  lkb_input.close()
  os.remove('lkb_input')
  #shutil.rmtree('/dw22/d50/dpmills/home/target')
  sentences = []
  index = 0
  string = ""
  in_output = False
  lkb_re = re.compile(r'LKB\(([0-9]+)\)')
  for line in output:
    #print line+"<br>"
    #continue
    m = lkb_re.match(line)
    if m:
      i = int(m.group(1)) - 3
      if i >= 0 and line.find("EOF") == -1:
        sentences.append([[mrs_files[i]+":",verb_preds[i][0],verb_preds[i][1],verb_preds[i][2]],[]])
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

def generate_sentences(grammar, mrs_files, verb_preds, delphin_dir):
  lkb_input = open('lkb_input','w')
  lkb_input.write('(read-script-file-aux "%s/lkb/script")' % (grammar))
  lkb_input.write('(setf *maximum-number-of-edges* 10000)')
  lkb_input.write(display_gen_results_fn)
  for file in mrs_files:
    lkb_input.write('(null (generate-from-mrs (mrs::read-mrs-from-file "%s")))' % (file))
    lkb_input.write('(TbG-gen-results)')
  lkb_input.flush()
  output = os.popen('cat lkb_input | %s | sed -n "/^LKB/,/EOF$/p"' % (delphin_dir + '/bin/lkb'))
  lkb_input.close()
  os.remove('lkb_input')
  sentences = []
  index = -1
  sentence = ""
  parse = ""
  mrs = ""
  state = ""
  in_output = False
  sentence_found = False
  lkb_re = re.compile(r'LKB\(([0-9]+)\)')
  for line in output:
    #print line+"<br>"
    #continue
    if line.find("new-pattern") == 0:
      index += 1
      sentences.append([[mrs_files[index]+":",verb_preds[index][0],verb_preds[index][1],verb_preds[index][2]],[],[],[]])
    elif line.find("start-entry") == 0:
      state = "sentence"
    elif line.find("parse") == 0:
      state = "parse"
    elif line.find("mrs") == 0:
      state = "mrs"
    elif line.find("end-entry") == 0:
      if sentence != "":
        sentences[index][1].append(sentence)
        sentences[index][2].append(parse+"<br>")
        sentences[index][3].append(mrs)
        sentence = parse = mrs = ""
      state = ""
    elif line.find("No strings generated") == 0:
      sentences[index][1].append('#NO-SENTENCES#')
      sentences[index][2].append('')
      sentences[index][3].append('')
    elif line.find('*maximum-number-of-edges*') > -1:
      sentences[index][1].append('#EDGE-ERROR#')
      sentences[index][2].append('')
    elif state == "sentence":
      sentence = unicode(line.lstrip('( \n').rstrip(') .\n').replace('"',''), 'utf-8').lower().encode('utf-8')
    elif state == "parse":
      parse += "&nbsp&nbsp " + line.strip()
    elif state == "mrs":
      mrs += "&nbsp&nbsp " + line.strip().replace("<","&lt ").replace(">","&gt") + "<br>"

  for entry in sentences:
    entry[2] = [clean_tree(s) for s in entry[2]]
  #print sentences
  return sentences

#returns a clean version of a tree outputted by the lkb
def clean_tree(tree):
  return re.sub(r'\("([^()]+)"\)',r'(@\1@)',tree).replace('"','').replace('@',"'")
  

def remove_duplicates(list):
  new_list = []
  while(list != []):
    new_list.append(list[0])
    list = filter((lambda x:x[0] != list[0][0]),list)
  return new_list

# Extract predications from the grammar
def get_n_predications(grammar_dir):
  lexicon = open(grammar_dir+'/lexicon.tdl','r')
  choices = open(grammar_dir+'/choices','r')
  lang = None
  pred_re = re.compile(r'noun([0-9]+)_stem1_pred')
  det_re = re.compile(r'noun([0-9]+)_det')
  det2_re = re.compile(r'det[0-9]+_stem1_pred')
  noun_rels_dets = []
  det_list = set([])
  for line in choices:
    pline = line.lstrip().split('=')
    m1 = pred_re.match(pline[0])
    m2 = det_re.match(pline[0])
    m3 = det2_re.match(pline[0])
    if pline[0] == 'language':
      lang = grammar_dir+'/'+pline[1].lower().rstrip()+'.tdl'
    if m1:
      if int(m1.group(1)) <= len(noun_rels_dets):
        noun_rels_dets[int(m1.group(1))-1][0] = pline[1].rstrip()
      else:
        noun_rels_dets.append([pline[1].rstrip(),None])        
    if m2:
      if int(m2.group(1)) <= len(noun_rels_dets):
        noun_rels_dets[int(m2.group(1))-1][1] = pline[1].rstrip()
      else:
        noun_rels_dets.append([None,pline[1].rstrip()])
    if m3:
      det_list.add(pline[1].rstrip())
  noun_rels_dets = remove_duplicates(noun_rels_dets)
#  noun_rels = []
#  det_rels = []
#  for pair in noun_rels_dets:
#    noun_rels.append(pair[0])   
#    if pair[1] == 'obl':
#      det_rels.append(det_rel)
#    else:
#      det_rels.append('exist_q_rel')
  det_rels = {"imp":["exist_q_rel"],"obl":list(det_list),"opt":list(det_list)+["exist_q_rel"]}
  lexicon.close()
  choices.close()
  return(noun_rels_dets,det_rels,lang)

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

#Class for storing templates with methods to facilitate predicate and feature replacement
class Template:
  def __init__(self, file=None):
    if file:
      pred_re = re.compile(r'#(.*?)#')
      feat_re = re.compile(r'@(.*?)@')
      label_re = re.compile(r'^label=(.*)')
      self.preds = set([])
      self.feats = set([])
      self.string = ""
      self.label = ""
      self.name = file
      f = open("templates/"+file,'r')
      for line in f:
        m = label_re.match(line)
        if m:
          self.label = m.group(1)
        elif line.lstrip().find(";") != 0:
          self.string += line
          (m1,m2) = pred_re.search(line),feat_re.search(line)
          if m1:
            self.preds.add(m1.group(1))
          if m2:
            self.feats.add(m2.group(1))
      f.close()
           
  def replace_pred(self,flag,pred):
    self.string = self.string.replace('#'+flag+'#',pred)
    self.preds.remove(flag)
    return self

  def replace_feat(self,flag,feat):
    self.string = self.string.replace('@'+flag+'@',feat)
    self.feats.remove(flag)
    return self
    
  def copy(self):
    t = Template()
    t.preds = self.preds.copy()
    t.feats = self.feats.copy()
    t.string = self.string
    t.label = self.label
    t.name = self.name
    return t

  def replace_features_from_grammar(self,repl_feats):
    if self.name in repl_feats:
      repl_feats = repl_feats[self.name]
      for replacement in repl_feats.values():
        self.replace_feat(replacement[0],var(replacement[1])+": "+replacement[2]+" @"+replacement[0]+"@")
        self.feats.add(replacement[0])
    for feat in self.feats.copy():
      self.replace_feat(feat,"")

def var(string):
  if string == "number":
    string = "num"
  elif string == "person":
    string = "per"
  elif string == "gender":
    string = "gend"
  if string.lower() in [ "sf","cog-st","speci","sort" ]:
    pass
  elif string.lower() in ["situation","tense","aspect" ]:
    string = "e." + string
  else:
    string = "png." + string
  return string

def get_replacement_features_from_grammar(grammar_dir):
  choices = open(grammar_dir+'/choices','r')
  in_options = False
  section_re = re.compile(r'section=(.*)')
  choice_re = re.compile(r'(.+)-feat([0-9]+)_(.+)=(.*)')
  temp_re = re.compile(r'(.*)=on')
  result = {}
  pos = {"name":1,"value":2,"location":0}
  for line in choices:
    m1 = section_re.match(line)
    if m1:
      if m1.group(1) == 'gen-options':
        in_options = True
      else:
        in_options = False
    elif in_options:
      m2,m3 = choice_re.match(line),temp_re.match(line)
      if m3:
        result[m3.group(1)] = {}
      elif m2 and m2.group(1).strip() in result.keys():
        name = m2.group(1).strip()
        if m2.group(2) not in result[name]:
          result[name][m2.group(2)] = ["","",""]
        result[name][m2.group(2)][pos[m2.group(3)]] = result[name][m2.group(2)][pos[m2.group(3)]] = m2.group(4)
  return result
    
# Extract templates from the grammar
def get_templates(grammar_dir):
  choices = open(grammar_dir+'/choices','r')
  choices_present = False
  in_options = False
  template_files = []
  templates = []
  section_re = re.compile(r'section=(.*)')
  choice_re = re.compile(r'(.*)=on')
  for line in choices:
    m1 = section_re.match(line)
    m2 = choice_re.match(line)
    if m1:
      if m1.group(1)=="gen-options":
        in_options = True
      else:
        in_options = False
    elif in_options and m2:
      template_files.append(m2.group(1))
      choices_present = True
  if not choices_present:
    template_files = ['itv','stv']
  for f in template_files:
    templates.append(Template(f))
  return templates    

# Output an mrs file from a template, replacing the appropriate predications
def process_mrs_file(mrs, outfile, noun1_rel, det1_rel, noun2_rel, det2_rel, verb_rel):
  output = mrs.replace("#NOUN1#",noun1_rel).replace("#NOUN2#",noun2_rel).replace("#VERB#",verb_rel).replace("#DET1#",det1_rel).replace("#DET2#",det2_rel)
  #print output+"<br><br>"
  f = open(outfile,'w')
  f.write(output)
  f.close()

#  for verb_rel in itvs:
#    i += 1
#    mrs_files.append(session+'Pattern ' + str(i))
#    process_mrs_file(MRS_itv,session+'Pattern '+str(i),noun_rels_dets[0][0],det_rels[noun_rels_dets[0][1]][0],noun_rels_dets[1][0],det_rels[noun_rels_dets[1][1]][0],verb_rel)
#  for verb_rel in stvs:
#    i += 1
#    mrs_files.append(session+'Pattern ' + str(i))
#    process_mrs_file(MRS_stv,session+'Pattern '+str(i),noun_rels_dets[0][0],det_rels[noun_rels_dets[0][1]][0],noun_rels_dets[1][0],det_rels[noun_rels_dets[1][1]][0],verb_rel)


# Wrap up all of the components involved in generation, and return the results
def get_sentences(grammar_dir,delphin_dir,session):
  (noun_rels_dets,det_rels,language) = get_n_predications(grammar_dir)
  (itvs,stvs) = get_v_predications(grammar_dir,language)
  templates = get_templates(grammar_dir)
  i = 0
  mrs_files = []
  info_list = []
  itr_verb_re,tr_verb_re,noun_re,det_re = re.compile(r'ITR-VERB([0-9]*)'),re.compile(r'TR-VERB([0-9]*)'),re.compile(r'NOUN([0-9]*)'),re.compile(r'DET([0-9]*)')
  repl_feats = get_replacement_features_from_grammar(grammar_dir)
  for template in templates:
    verb_rels = {}
    i += 1
    template.replace_features_from_grammar(repl_feats)
    for pred in template.preds.copy():
      m1,m2,m3,m4 = itr_verb_re.match(pred),tr_verb_re.match(pred),noun_re.match(pred),det_re.match(pred)
      if m1:
        template.replace_pred(pred,itvs[int(m1.group(1)) % len(itvs)])
        verb_rels[pred] = itvs[int(m1.group(1)) % len(itvs)]
      elif m2:
        template.replace_pred(pred,stvs[int(m2.group(1)) % len(stvs)])
        verb_rels[pred] = stvs[int(m2.group(1)) % len(stvs)]
      elif m3:
        template.replace_pred(pred,noun_rels_dets[int(m3.group(1)) % len(noun_rels_dets)][0])
      elif m4:
        det_list = det_rels[noun_rels_dets[int(m4.group(1)) % len(noun_rels_dets)][1]]
        if len(det_list) == 0:
          det_list.append("")
        template.replace_pred(pred,det_list[0])
    output = session+'Pattern '+str(i)
    mrs_files.append(output)
    f = open(output,'w')
    f.write(template.string)
    f.close()
    info_list.append([verb_rels,template.label,template.name])
  sentences = generate_sentences(grammar_dir, mrs_files, info_list, delphin_dir)
  for file in mrs_files:
    try:
      os.remove(file)
    except OSError:
      pass
  return sentences


# Same as get_sentences, but for the additional sentences page
def get_additional_sentences(grammar_dir,delphin_dir,verb_rels,template_file,session):
  (noun_rels_dets,det_rels,language) = get_n_predications(grammar_dir)
  itr_verb_re,tr_verb_re,noun_re,det_re = re.compile(r'ITR-VERB([0-9]*)'),re.compile(r'TR-VERB([0-9]*)'),re.compile(r'NOUN([0-9]*)'),re.compile(r'DET([0-9]*)')
#  (itvs,stvs) = get_v_predications(grammar_dir,language)
  exec("verb_rels = "+verb_rels)
  mrs_files = []
  t = Template(template_file)
  templates = [t]
  #  if verb_rel in itvs:
  #    for i in range(len(noun_rels_dets)):
  #      for det_rel in det_rels[noun_rels_dets[i][1]]:
  #        mrs_files.append(session+'noun' + str(i) + det_rel)
  #        process_mrs_file(mrs,session+'noun'+str(i) + det_rel,noun_rels_dets[i][0],det_rel,"","",verb_rel)
  #  else:
  #    for i in range(len(noun_rels_dets)):
  #      for j in range(len(noun_rels_dets)):
  #        for det_rel1 in det_rels[noun_rels_dets[i][1]]:
  #          for det_rel2 in det_rels[noun_rels_dets[j][1]]:
  #            mrs_files.append(session+'noun' + str(i) + det_rel1 + "_" + str(j) + det_rel2)
  #            process_mrs_file(mrs,session+'noun' + str(i) + det_rel1 + "_" + str(j) + det_rel2,noun_rels_dets[i][0],det_rel1,noun_rels_dets[j][0],det_rel2,verb_rel)
  repl_feats = get_replacement_features_from_grammar(grammar_dir)
  t.replace_features_from_grammar(repl_feats)
  for pred in t.preds:
    m1,m2,m3,m4 = itr_verb_re.match(pred),tr_verb_re.match(pred),noun_re.match(pred),det_re.match(pred)
    if m1:
      for temp in templates:
        temp.replace_pred(pred,verb_rels[pred])
    elif m2:
      for temp in templates:
        temp.replace_pred(pred,verb_rels[pred])
    elif m3:
      templates = [temp.copy().replace_pred(pred,noun[0]) for temp in templates for noun in noun_rels_dets]
    elif m4:
      templates = [temp.copy().replace_pred(pred,det) for temp in templates for det in det_rels['opt']]
  i = 0
  for temp in templates:
    i += 1
    output = session+'pattern'+str(i)
    mrs_files.append(output)
    f = open(output,'w')
    f.write(temp.string)
    #print temp.string
    #print "<br>"
    f.close()
  sentences_with_info = generate_sentences(grammar_dir, mrs_files, [[verb_rels,"",""]] * len(mrs_files), delphin_dir)
  sentences = []
  trees = []
  mrss = []
  for i in range(len(sentences_with_info)):
    if sentences_with_info[i][1][0] != "#NO-SENTENCES#":
      sentences.extend(sentences_with_info[i][1])
      trees.extend(sentences_with_info[i][2])
      mrss.extend(sentences_with_info[i][3])
  if len(sentences) == 0:
    sentences = ["#NO-SENTENCES#"]
    trees = [""]
  for file in mrs_files:
    try:
      os.remove(file)
    except OSError:
      pass
  return sentences,trees,mrss

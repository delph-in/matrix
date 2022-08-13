##########################################################
# This file contains functions for extracting predications
# from grammar files, generating MRSs, and generating
# sentences from those MRSs

import re
import os
import shutil
import subprocess
import json
from delphin import ace
from delphin.codecs import simplemrs

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


def generate_sentences_lkb(grammar, mrs_files, verb_preds, delphin_dir, session):

    # open a file called lkb_input+session for writing
    # this file will contain commands to be sent to the lkb
    lkb_input = open('lkb_input'+session, 'w')

    # command to load the grammar's script file
    lkb_input.write('(read-script-file-aux "%s/lkb/script")' % (grammar))

    # command to set the max number of edges on parsing and generation
    lkb_input.write('(setf *maximum-number-of-edges* 10000)')

    # display_gen_results_fn is a string with some lkb functions for printing
    # generation results
    lkb_input.write(display_gen_results_fn)

    # here, for each of our pattern files, we put a command to generate from that
    # file, and then call the print results function for it
    for file in mrs_files:
        lkb_input.write(
            '(null (generate-from-mrs (mrs::read-mrs-from-file "%s")))' % (file))
        lkb_input.write('(TbG-gen-results)')

    # okay, close the file for writing
    lkb_input.flush()
    lkb_input.close()

    # reopen the file in read-only mode
    lkb_input = open('lkb_input'+session, 'r')

    # open another file to catch the output
    output = open('lkb_output'+session, 'w')
    subprocess.call([os.path.join(delphin_dir, 'bin/lkb')],
                    stdin=lkb_input, stdout=output)
    lkb_input.close()
    os.remove('lkb_input'+session)
    output.close()

    sentences = get_sentences_from_lkb_output('lkb_output'+session, mrs_files, verb_preds)
    # TODO: This complicated list structure could be cleaner. 
    # The structure of this sentences list is
    # [ 
    #   [
    #       [ <mrs_file_name>:, <dictionary_with_verb_predicate_names>, <mrs_template_label>, <mrs_template_file_name> ],
    #       [ <sentence_generated> ], 
    #       [ <tree_structure_for_sentences> ], 
    #       [ <values_for_tags_and_features_in_MRS> ]
    #   ]
    # ]
    # Eg. [[['8098Pattern 1:', {'ITR-VERB1': '_sleep_v_rel'}, 'Intransitive verb phrase', 'itv'], [b'i sleep'], ["&nbsp&nbsp&nbsp&nbsp (S (NP (N ('I'))) (VP ('sleep')))<br>"], ['&nbsp&nbsp&nbsp&nbsp &lt h1,e2:PROP-OR-QUES:TENSE:ASPECT:MOOD,...]
    return sentences

def get_sentences_from_lkb_output(output_file,  mrs_files, verb_preds):
    '''parses the output file from lkb after generating sentences to extract the sentence information.'''
    output = open(output_file, 'r')
    sentences = []

     # create an entry in sentences for every mrs pattern in mrs_files
    for i in range(len(mrs_files)):
        sentences.append([[mrs_files[i]+":", verb_preds[i][0],
                               verb_preds[i][1], verb_preds[i][2]], [], [], []])

    index = -1
    sentence = parse = mrs = state = ""
    in_output = False
    sentence_found = False
    lkb_re = re.compile(r'LKB\(([0-9]+)\)')
    for line in output:
        # print line+"<br>"
        # continue
        if line.find("new-pattern") == 0:
            index += 1
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
            try:
                sentences[index][1].append('#NO-SENTENCES#')
                sentences[index][2].append('')
                sentences[index][3].append('')
            except IndexError:
                print("#NO-SENTENCES#<br />")
        elif line.find('*maximum-number-of-edges*') > -1:
            try:
                sentences[index][1].append('#EDGE-ERROR#')
                sentences[index][2].append('')
                sentences[index][3].append('')
            except IndexError:
                print("#EDGE-ERROR#<br />")
        elif line.find('Stack overflow') > -1:
            try:
                sentences[index][1].append('#EDGE-ERROR#')
                sentences[index][2].append('')
                sentences[index][3].append('')
            except IndexError:
                print("#Stack overflow#<br />")

        elif state == "sentence":
            sentence = line.lstrip('( \n').rstrip(') .\n').replace(
                '"', '').lower()
        elif state == "parse":
            parse += "&nbsp&nbsp&nbsp&nbsp " + line.strip()
        elif state == "mrs":
            mrs += "&nbsp&nbsp&nbsp&nbsp " + \
                line.strip().replace("<", "&lt ").replace(">", "&gt") + "<br>"
    output.close()
    os.remove(output_file)
    for entry in sentences:
        entry[2] = [clean_tree(s) for s in entry[2]]
    return sentences

# returns a clean version of a tree outputted by the lkb
def clean_tree(tree):
    return re.sub(r'\("([^()]+)"\)', r'(@\1@)', tree).replace('"', '').replace('@', "'")

def generate_sentences_ace(grammar_dir, mrs_files, verb_preds, delphin_dir, session):
    iso = os.path.basename(grammar_dir)

    # open a file to catch the ace output
    output = open('ace_output'+session, 'w')
    output.write('Compiling grammar...\n')
    output.flush()

    ace_error = open('ace_error'+session, 'w')
    ace_error.write('Compiling grammar...\n')
    ace_error.flush()

    # compile the grammar
    ace.compile('%(grammar_dir)s/ace/config.tdl' % {'grammar_dir': grammar_dir}, 
                '%(grammar_dir)s/%(iso)s.dat' % {'grammar_dir':grammar_dir, 'iso':iso},
                executable='/usr/local/bin/ace',
                stdout=output, stderr=ace_error)

    # rewrite the multiple mrs files to be one mrs per line
    ace_mrs_iterable = []
    for mrs_file in mrs_files:
        text = collapse_mrs_to_one_line(mrs_file)
        ace_mrs_iterable.append(text)

    output.write('\nGenerating Sentences...\n')
    output.flush()
    ace_error.write('\nGenerating Sentences...\n')
    ace_error.flush()

    # generate sentences with ace
    # TODO: add -n [count] to enumerate only the top [count] results.
    response_iter = ace.generate_from_iterable(
        '%(grammar_dir)s/%(iso)s.dat' % {'grammar_dir':grammar_dir, 'iso':iso},
        ace_mrs_iterable,
        cmdargs = ['--disable-subsumption-test'],
        executable='/usr/local/bin/ace',
        stderr=ace_error
    )

    sentences = get_sentences_from_ace_response(response_iter, mrs_files, verb_preds)
    # get_sentences_from_ace_output('ace_output'+session, mrs_files, verb_preds)
    # the structure of this sentences list is
    # [ 
    #   [
    #       [ <mrs_file_name>:, <dictionary_with_verb_predicate_names>, <mrs_template_label>, <mrs_template_file_name> ],
    #       [ <sentence_generated> ], 
    #       [ <tree_structure_for_sentences> ], 
    #       [ <values_for_tags_and_features_in_MRS> ]
    #   ]
    # ]
    # Eg. [[['8098Pattern 1:', {'ITR-VERB1': '_sleep_v_rel'}, 'Intransitive verb phrase', 'itv'], [b'i sleep'], ["&nbsp&nbsp&nbsp&nbsp (S (NP (N ('I'))) (VP ('sleep')))<br>"], ['&nbsp&nbsp&nbsp&nbsp &lt h1,e2:PROP-OR-QUES:TENSE:ASPECT:MOOD,...]
    ace_error.close()
    output.close()
    return sentences

def get_sentences_from_ace_response(response_iter, mrs_files, verb_preds):
    sentences = []

    # create an entry in sentences for every mrs pattern in mrs_files
    for i in range(len(mrs_files)):
        sentences.append([[mrs_files[i]+":", verb_preds[i][0],
                               verb_preds[i][1], verb_preds[i][2]], [], [], []])
    
    mrs_index = 0
    for response in response_iter:
        results = len(response.results())
        if results == 0:
            sentences[mrs_index][1].append("#NO-SENTENCES#")
            sentences[mrs_index][2].append("")
            sentences[mrs_index][3].append("")
        else:
            for i in range(results):
                result = response.result(i)
                sentences[mrs_index][1].append(result['surface'])
                sentences[mrs_index][2].append("&nbsp&nbsp&nbsp&nbsp " + result['tree'].strip() + '<br>')
                mrs = simplemrs.decode(result['mrs'].strip())
                mrs_string = simplemrs.encode(mrs, indent=True).replace('\n', '<br>&nbsp&nbsp&nbsp&nbsp&nbsp')
                sentences[mrs_index][3].append("&nbsp&nbsp&nbsp&nbsp " + mrs_string)
        mrs_index += 1
    return sentences

            

def collapse_mrs_to_one_line(mrs_file):
    '''Takes a multiline mrs file, splits the white sapce and puts everything together into one line.'''
    file = open(mrs_file, 'r')
    text = file.read()
    text = " ".join(text.split())
    return text


def remove_duplicates(input_list):
    '''Takes an input list that has the form [[a1,b1],[a2,b2], ... [a{n}, b{n}]] ie. a list of pairs.
    It removes any pair in the list where the "a" value appeared earlier in the list. 
    
    For example if a2 == a1 in the example list above then [a2, b2] would be removed from the list.
    '''
    new_list = []
    while(input_list != []):
        new_list.append(input_list[0])
        input_list = list(filter((lambda x: x[0] != input_list[0][0]), input_list))
    return new_list

# Extract predications from the grammar


def get_n_predications(grammar_dir):
    lexicon = open(os.path.join(grammar_dir, 'lexicon.tdl'), 'r')
    choices = open(os.path.join(grammar_dir, 'choices'), 'r')
    lang = None

    # Worth noting that since the stem must explicitly be [0-9] only the first 10 predicates
    # Are ever stored for each noun type.
    pred_re = re.compile(r'noun([0-9]+)_stem[0-9]_pred')

    # det_re is searching for whether dets are opt, obl or imp
    det_re = re.compile(r'noun([0-9]+)_det')

    # det2_re is searching for determiner predications
    det2_re = re.compile(r'det[0-9]+_stem[0-9]_pred')

    # Every noun type in the choices file should have a number eg.noun23
    # And every noun type should define exactly one determiner type (opt, obl, imp)
    # This dictionary maps the noun number to the det type.
    noun_det_dict = {}
    # noun_rels_dets is a list of pairs, where the first element is a noun_rel
    # the second element is whether a determiner is 'obl','opt' or 'imp'
    # for that noun_rel
    noun_rels_dets = []
    # det_list is the set of determiner rels found in the choices
    det_list = set([])
    for line in choices:
        pline = line.strip().split('=')
        if len(pline) != 2:
            continue
        feature, value = pline
        m1 = pred_re.match(feature)
        m2 = det_re.match(feature)
        m3 = det2_re.match(feature)
        if feature == 'language':
            lang = os.path.join(grammar_dir, value.lower()+'.tdl')
        if m1:  # m1 is results of looking for noun predications
            noun_number = m1.group(1)
            if noun_number in noun_det_dict:
                noun_rels_dets.append([value, noun_det_dict[noun_number]])
        if m2:
            noun_number = m2.group(1)
            noun_det_dict[noun_number] = value
        if m3:
            det_list.add(value)

    noun_rels_dets = remove_duplicates(noun_rels_dets)
    # print "noun_rels_dets: <<",noun_rels_dets,">><br />"

    # okay, here in det_rels, we're building key value pairs where the
    # keys are relationship types between nouns and determiners
    # imp: is mapped to "exist_q_rel"
    # obl: is mapped to the list of determiner relations found in the choices
    # opt: is mapped to that list of determiners found in the choices + an exist_q_rel
    det_rels = {"imp": ["exist_q_rel"], "obl": list(
        det_list), "opt": list(det_list)+["exist_q_rel"]}
    # print "det_rels: <<",det_rels,">><br />"
    lexicon.close()
    choices.close()
    return(noun_rels_dets, det_rels, lang)

def get_section(text:str, section_start_symbol:str, section_end_symbol:str):
    start = text.find(section_start_symbol)
    end = text.find(section_end_symbol, start+1)
    return text[start:end]

def get_v_predications(grammar_dir, lang):
    itv_rels = []
    stv_rels = []
    # verbs has the structure {"verb_v_rel": "some-verb-lex"}
    verbs = {}
    cur_type = ""
    # Not sure what the rationality of used_types is but the way its used
    # is there can only be one predicate per verb-lex-type in the dict
    # so if multiple verbs are trans-verb-lex, only one goes into the dict 
    # which seems wrong.
    used_types = []
    p1 = re.compile(r'(\S*-verb-lex)')
    p2 = re.compile(r'PRED \"(\S*)\"')
    lexicon = open(os.path.join(grammar_dir, 'lexicon.tdl'), 'r')
    verb_section = get_section(lexicon.read(), ";;; Verbs", ";;; ")
    verb_lines = verb_section.split('\n')
    # iterates through lexicon.tdl
    for line in verb_lines:
        m1 = p1.search(line)
        m2 = p2.search(line)
        if m1:
            cur_type = m1.group(1)
        if m2 and cur_type != "" and cur_type not in used_types:
            verbs[m2.group(1)] = [cur_type]
            used_types.append(cur_type)
            cur_type = ""
    lexicon.close()
    language = open(lang, 'r')
    verb_section = get_section(language.read(), ";;; Verbs", ";;; ")
    # For each verb in verbs, we need to find out if it is a 
    # descendant of transitive-lex-item or intransitive-lex-item
    for verb in verbs.keys():
        lex_types = verbs[verb]
        while len(lex_types) > 0:
            for lex_type in list(lex_types):
                type_regex = re.compile(r'\s{} := (.*)\n'.format(lex_type))
                type_match = type_regex.search(verb_section)
                if type_match:
                    new_types = type_match.group(1).split('&')
                    new_types = [item.strip().rstrip('.') for item in new_types if len(item) > 0]
                    if 'intransitive-lex-item' in new_types:
                        itv_rels.append(verb)
                        lex_types = []
                    elif 'transitive-lex-item' in new_types:
                        stv_rels.append(verb)
                        lex_types = []
                    else:
                        lex_types = lex_types + new_types
                # Always remove the current type from the lex_types list
                # after we finished "searching" this type.
                if lex_type in lex_types:
                    lex_types.remove(lex_type)
    language.close()
    return (itv_rels, stv_rels)

# Class for storing templates with methods to facilitate predicate and feature replacement


class Template:
    def __init__(self, file=None):
        # The file name is expected to be a file listed in web/templates/
        if file:
            pred_re = re.compile(r'#(.*?)#')
            feat_re = re.compile(r'@(.*?)@')
            label_re = re.compile(r'^label=(.*)')
            self.preds = set([])
            self.feats = set([])
            self.string = ""
            self.label = ""
            self.name = file
            f = open("web/templates/"+file, 'r')
            for line in f:
                m = label_re.match(line)
                if m:
                    self.label = m.group(1)
                elif line.lstrip().find(";") != 0:
                    self.string += line
                    (m1, m2) = pred_re.search(line), feat_re.search(line)
                    if m1:
                        self.preds.add(m1.group(1))
                    if m2:
                        self.feats.add(m2.group(1))
            f.close()

    def replace_pred(self, flag, pred):
        self.string = self.string.replace('#'+flag+'#', pred)
        self.preds.remove(flag)
        return self

    def replace_feat(self, flag, feat):
        self.string = self.string.replace('@'+flag+'@', feat)
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

    def replace_features_from_grammar(self, repl_feats):
        if self.name in repl_feats:
            repl_feats = repl_feats[self.name]
            for replacement in list(repl_feats.values()):
                self.replace_feat(replacement[0], var(
                    replacement[1])+": "+replacement[2]+" @"+replacement[0]+"@")
                self.feats.add(replacement[0])
        for feat in self.feats.copy():
            self.replace_feat(feat, "")


def var(string):
    if string == "number":
        string = "num"
    elif string == "person":
        string = "per"
    elif string == "gender":
        string = "gend"
    if string.lower() in ["sf", "cog-st", "speci", "sort"]:
        pass
    elif string.lower() in ["mood", "situation", "tense", "aspect"]:
        string = "e." + string
    else:
        string = "png." + string
    return string


def get_replacement_features_from_grammar(grammar_dir):
    choices = open(os.path.join(grammar_dir, 'choices'), 'r')
    in_options = False
    section_re = re.compile(r'section=(.*)')
    choice_re = re.compile(r'(.+)-feat([0-9]+)_(.+)=(.*)')
    temp_re = re.compile(r'(.*)=on')
    result = {}
    pos = {"name": 1, "value": 2, "location": 0}
    for line in choices:
        m1 = section_re.match(line)
        if m1:
            if m1.group(1) == 'gen-options':
                in_options = True
            else:
                in_options = False
        elif in_options:
            m2, m3 = choice_re.match(line), temp_re.match(line)
            if m3:
                result[m3.group(1)] = {}
            elif m2 and m2.group(1).strip() in list(result.keys()):
                name = m2.group(1).strip()
                if m2.group(2) not in result[name]:
                    result[name][m2.group(2)] = ["", "", ""]
                result[name][m2.group(2)][pos[m2.group(3)]] = result[name][m2.group(
                    2)][pos[m2.group(3)]] = m2.group(4)
    return result

# Extract templates from the grammar
# more specifically, this subprocess (seems to) be checking
# to see if the TbG options section is present in the choices
# file, and if it's not, we just make basic stv and itv
# templates and return them


def get_templates(grammar_dir):
    choices = open(os.path.join(grammar_dir, 'choices'), 'r')
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
            if m1.group(1) == "gen-options":
                in_options = True
            else:
                in_options = False
        elif in_options and m2:
            template_files.append(m2.group(1))
            choices_present = True
    if not choices_present:
        template_files = ['itv', 'stv']
    for f in template_files:
        templates.append(Template(f))
    return templates

# Output an mrs file from a template, replacing the appropriate predications


def process_mrs_file(mrs, outfile, noun1_rel, det1_rel, noun2_rel, det2_rel, verb_rel):
    output = mrs.replace("#NOUN1#", noun1_rel).replace("#NOUN2#", noun2_rel).replace(
        "#VERB#", verb_rel).replace("#DET1#", det1_rel).replace("#DET2#", det2_rel)
    # print output+"<br><br>"
    f = open(outfile, 'w')
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
def get_sentences(grammar_dir, delphin_dir, session, with_lkb=False):
    (noun_rels_dets, det_rels, language) = get_n_predications(grammar_dir)
    (itvs, stvs) = get_v_predications(grammar_dir, language)
    templates = get_templates(grammar_dir)
    i = 0
    mrs_files = []
    info_list = []
    itr_verb_re, tr_verb_re, noun_re, det_re = re.compile(r'ITR-VERB([0-9]*)'), re.compile(
        r'TR-VERB([0-9]*)'), re.compile(r'NOUN([0-9]*)'), re.compile(r'DET([0-9]*)')
    repl_feats = get_replacement_features_from_grammar(grammar_dir)
    for template in templates:
        verb_rels = {}
        i += 1
        template.replace_features_from_grammar(repl_feats)
        for pred in template.preds.copy():
            m1, m2, m3, m4 = itr_verb_re.match(pred), tr_verb_re.match(
                pred), noun_re.match(pred), det_re.match(pred)
            if m1:
                template.replace_pred(pred, itvs[int(m1.group(1)) % len(itvs)])
                verb_rels[pred] = itvs[int(m1.group(1)) % len(itvs)]
            elif m2:
                template.replace_pred(pred, stvs[int(m2.group(1)) % len(stvs)])
                verb_rels[pred] = stvs[int(m2.group(1)) % len(stvs)]
            elif m3:
                template.replace_pred(pred, noun_rels_dets[int(
                    m3.group(1)) % len(noun_rels_dets)][0])
            elif m4:
                # print "det_list = det_rels[noun_rels_dets[int(m4.group(1)) % len(noun_rels_dets)][1]]<br />"
                # print "m4.group(1): ",m4.group(1),"<br />"
                # print "int(m4.group(1)): ",int(m4.group(1)),"<br />"
                # print "len(noun_rels_dets): ",len(noun_rels_dets),"<br />"
                # print int(m4.group(1)),"%",len(noun_rels_dets),": ",int(m4.group(1)) % len(noun_rels_dets),"<br />"
                # print "noun_rels_dets[",int(m4.group(1)) % len(noun_rels_dets),"]: ", noun_rels_dets[int(m4.group(1)) % len(noun_rels_dets)],"<br />"
                det_list = det_rels[noun_rels_dets[int(
                    m4.group(1)) % len(noun_rels_dets)][1]]
                # print"det_list: ",det_list,"<br />"
                if len(det_list) == 0:
                    det_list.append("")
                template.replace_pred(pred, det_list[0])
        output = session+'Pattern '+str(i)
        mrs_files.append(output)
        f = open(output, 'w')
        # print "generated template: ",template.string
        f.write(template.string)
        f.close()
        info_list.append([verb_rels, template.label, template.name])
    if with_lkb:
        sentences = generate_sentences_lkb(
            grammar_dir, mrs_files, info_list, delphin_dir, session)
    else:
        sentences = generate_sentences_ace(
            grammar_dir, mrs_files, info_list, delphin_dir, session)

    for file in mrs_files:
        try:
            os.remove(file)
        except OSError:
            pass
    return sentences


# Same as get_sentences, but for the additional sentences page
def get_additional_sentences(grammar_dir, delphin_dir, verb_rels, template_file, session):
    (noun_rels_dets, det_rels, language) = get_n_predications(grammar_dir)
    itr_verb_re, tr_verb_re, noun_re, det_re = re.compile(r'ITR-VERB([0-9]*)'), re.compile(
        r'TR-VERB([0-9]*)'), re.compile(r'NOUN([0-9]*)'), re.compile(r'DET([0-9]*)')
    # verb_rels is a string dictionary with the same structure as verbs dictionary in get_v_predications
    verb_rels = json.loads(verb_rels.replace("'", '"'))
    mrs_files = []
    t = Template(template_file)
    templates = [t]
    repl_feats = get_replacement_features_from_grammar(grammar_dir)
    t.replace_features_from_grammar(repl_feats)
    for pred in t.preds.copy():
        m1, m2, m3, m4 = itr_verb_re.match(pred), tr_verb_re.match(
            pred), noun_re.match(pred), det_re.match(pred)
        if m1:
            for temp in templates:
                temp.replace_pred(pred, verb_rels[pred])
        elif m2:
            for temp in templates:
                temp.replace_pred(pred, verb_rels[pred])
        elif m3:
            templates = [temp.copy().replace_pred(pred, noun[0])
                         for temp in templates for noun in noun_rels_dets]
        elif m4:
            templates = [temp.copy().replace_pred(pred, det)
                         for temp in templates for det in det_rels['opt']]
    i = 0
    for temp in templates:
        i += 1
        output = session+'pattern'+str(i)
        mrs_files.append(output)
        f = open(output, 'w')
        f.write(temp.string)
        # print temp.string
        # print "<br>"
        f.close()
    sentences_with_info = generate_sentences_ace(grammar_dir, mrs_files, [
                                             [verb_rels, "", ""]] * len(mrs_files), delphin_dir, session)
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
    return sentences, trees, mrss

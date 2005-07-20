#!/usr/local/bin/perl
#
#First pass at html-based program for customizing
#the matrix.  Contains word order modules, rudimentary
#lexicon support, and (potentially) sentential negation.

print "Content-type: text/html", "\n\n";
print "<html>\n<head>\n";
print "<title>Matrix Customized</title>\n";
print "</head><body>";

&remove_old_files;

#Some stuff copied from TWiki, in the hopes of making homer happy.  Turns
#out it was irrelevant to making homer happy, but useful anyway.

use strict;
use CGI::Carp qw(fatalsToBrowser);
use CGI;

#Parse the data from the form, and store it in matrix_form.
#Make sure to check all text fields and rejecct anything that
#contains characters other than [a-Z0-9_].  No spaces!  No back ticks!
#This will help with security.  Especially since I'm going to be 
#using the language name value to create a file, which requires a
#system call, which leads to that value being interpreted.

my(%matrix_form);
#&parse_form_data (*matrix_form);

%matrix_form = &parse_form_data;

#Some error checking

my($delivery) = $matrix_form{"delivery"};
my($tvsubj) = $matrix_form{"tverbSubj"};
my($ivsubj) = $matrix_form{"iverbSubj"};
my($tvobj) = $matrix_form{"tverbObj"};
my($subjadpform) = $matrix_form{"subjAdpForm"};
my($subjadp) = $matrix_form{"subjAdp"};
my($objadpform) = $matrix_form{"objAdpForm"};
my($objadp) = $matrix_form{"objAdp"};
my($hasdets) = $matrix_form{"hasDets"};
my($noundetorder) = $matrix_form{"NounDetOrder"};

unless ($delivery) {
    return_error (500, "Internal Server Error", "You must specify an archive type (.tar.gz or .zip).");
}

if (($tvsubj =~ /pp/ || $ivsubj =~ /pp/) && !$subjadpform) {
    return_error (500, "Internal Server Error", "If one or both of your verbs take PP subjects, you must provide a subject-marking adposition.");
}

if ($tvobj =~ /pp/ && !$objadpform) {
    return_error (500, "Internal Server Error", "If your transitive verb takes PP objects, you must provide an object-marking adposition.");
}

if (($subjadpform && !$subjadp) || ($objadpform && !$objadp)) {
    return_error (500, "Internal Server Error", "All adpositions must be specified as either prepositions or postpositions.");
}

if ($subjadp && $objadp && !($subjadp eq $objadp)) {
    return_error (500, "Internal Server Error", "Systems with inconsistent adposition placement not currently supported.");
}

unless ($hasdets) {
    return_error (500, "Internal Server Error", "You must specify whether your language has determiners as independent words.");
}

if ($hasdets =~ /t/) {
    unless ($noundetorder) {
	return_error (500, "Internal Server Error", "If your language has independent determiners, you must specify which side of the noun they appear on.");
    }
}

#Create a copy of the core-matrix files.

my($user,@home,$home,$matrix,$core_matrix,$modules_home);

$user = &make_user_id;

#open and close don't interpret ~.  Need to get shell to
#interprest it for me.
#$home = "~uwcl/matrix/";

$home = `ls -ld ~uwcl`;
@home = split(' ',$home);
$home = $home[8]."/matrix/";

$matrix = "$home"."tmp/"."matrix"."\.$user";
$core_matrix = "$home"."matrix-core";
$modules_home = "$home"."modules/";

#Note that $core_matrix and $matrix are not user supplied
#fields, so even though this way does pass its arguments to /bin/sh
#for interpretation, this should be safe.  Note that cp also
#passes its arguments to /bin/sh, so even using "system" wouldn't
#be better.

`cp -r $core_matrix $matrix`;

#Create a customized script file, and store it in matrix.user/lkb/
#Consider adding if time: A field in the form for two sample sentences,
#to put in script file.

my($my_language) = $matrix_form{"language"}.".tdl";
my($sentence1) = $matrix_form{"sentence1"};
my($sentence2) = $matrix_form{"sentence2"};

open (LKBSCRIPT, ">$matrix"."/lkb/script") || return_error (500,"Internal Server Error","Cannot open necessary output file.");

open (BASESCRIPT, "$modules_home"."script") || return_error (500,"Internal Server Error","Cannot open necessary input file: script.");

while (<BASESCRIPT>) {

    if (/^\;\;\; Modules: LOAD my_language.tdl/) {
	print LKBSCRIPT "   (lkb-pathname (parent-directory) \"$my_language\")\n";
    } elsif ($_ =~ /^\;\;\; Modules: Default sentences/ && defined($sentence1)){
        print LKBSCRIPT "(if (eq (length *last-parses*) 1)\n";
	print LKBSCRIPT "   (setf *last-parses* '(\"$sentence1\" \"$sentence2\")))\n";
    } else {
	print LKBSCRIPT;
    }
}

close (LKBSCRIPT);
close (BASESCRIPT);

#Create a customized my_language.tdl file, and store in matrix.user/

#Word order

#First, if there are adpositions, is their placement consistent
#with the O/V order?

my($ruletypesinput,$wordorder,$adp);
$wordorder = $matrix_form{"wordorder"};
my($consistentorder) = "easy";

if ($subjadp || $objadp) {

    $adp = ($subjadp || $objadp);

    if ($wordorder =~ /free/) {
	$consistentorder = "free";
    } elsif (($wordorder =~ /(sov)|(osv)|(ovs)|v-final/) && ($adp =~ /pre/)) {
	$consistentorder = "ov-prep";
    } elsif (($wordorder =~ /(svo)|(vos)|(vso)|v-initial/) && ($adp =~ /post/)) {
	$consistentorder = "vo-postp";
    }
}

open (MYLANGUAGE, ">$matrix/"."$my_language") || return_error (500,"Internal Server Error","Cannot open necessary output file.");

if ($wordorder =~ /ovs/) {
    print "<p>Word order matches ovs.\n";
} else {
    print "<p>Word order does not match ovs.\n";
}

if ($wordorder =~ /sov/) {
    $ruletypesinput = "SOV.tdl";
} elsif ($wordorder =~  /svo/) {
    $ruletypesinput = "SVO.tdl";
} elsif ($wordorder =~  /vso/) {
    $ruletypesinput = "VSO.tdl";
} elsif ($wordorder =~  /osv/) {
    $ruletypesinput = "OSV.tdl";
} elsif ($wordorder =~  /ovs/) {
    $ruletypesinput = "OVS.tdl";
} elsif ($wordorder =~  /vos/) {
    $ruletypesinput = "VOS.tdl";
} elsif ($wordorder =~  /v-final/) {
    $ruletypesinput = "V-final.tdl";
} elsif ($wordorder =~  /v-initial/) {
    $ruletypesinput = "V-initial.tdl";
} elsif ($wordorder =~  /free/) {
    $ruletypesinput = "free-order.tdl";
} else {
    return_error (500,"Internal Server Error","No value specified for word order")
}

open (RULETYPES, "$modules_home"."$ruletypesinput") || return_error (500,"Internal Server Error","Cannot open necessary input file: $ruletypesinput.");

print MYLANGUAGE ";;; -*- Mode: TDL; Package: LKB -*-\n";
print MYLANGUAGE ";;;\n";
print MYLANGUAGE ";;; Language-specific types and constraints for ",$matrix_form{"language"},"\n\n";
print MYLANGUAGE ";;; Phrase structure rule types\n\n";

while (<RULETYPES>) {
    unless (/\;\;\; -\*- Mode: TDL/) {
	print MYLANGUAGE;
    }
}

close (RULETYPES);

#Now some add ons for languages which have adpositions
#(in particular, adp-marked subjects or objects) which
#don't follow the general ordering convenstions of the language.

if ($consistentorder =~ /ov-prep/) {
   
    print MYLANGUAGE "\;\;\; The following type addendum statment restricts the head-final\n";
    print MYLANGUAGE "\;\;\; head-complement rule from taking adpositions as itshead.\n\n";
    print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";

    print MYLANGUAGE "head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD adp ].\n\n";

}

if ($consistentorder =~ /vo-postp/) {
   
    print MYLANGUAGE "\;\;\; The following type addendum statment restricts the head-initial\n";
    print MYLANGUAGE "\;\;\; head-complement rule from taking adpositions as itshead.\n\n";
    print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";

    print MYLANGUAGE "comp-head-phrase := basic-head-1st-comp-phrase & head-initial &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD adp ].\n\n";
    
}

if ($consistentorder =~ /free/) {

    if ($adp =~ /pre/) {

	print MYLANGUAGE "\;\;\; The following type addendum statment restricts the head-final\n";
	print MYLANGUAGE "\;\;\; head-complement rule from taking adpositions as itshead.\n\n";
	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";

    } else {
	
	print MYLANGUAGE "\;\;\; The following type addendum statment restricts the head-initial\n";
	print MYLANGUAGE "\;\;\; head-complement rule from taking adpositions as itshead.\n\n";
	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";

    }
}

#Now the rules for NPs: spec-head or head-spec, plus also the
#opt-det rule.

print MYLANGUAGE "\;\;\; Rules for building NPs.  Note that the Matrix uses SPR for\n";
print MYLANGUAGE "\;\;\; the specifier of nouns and SUBJ for the subject (specifier) of verbs.\n\n";

if ($hasdets =~ /t/) {
    if ($noundetorder =~ /SpecHead/) {

	print MYLANGUAGE "spec-head-phrase := basic-head-spec-phrase & head-final.\n\n";

    } else {

	print MYLANGUAGE "head-spec-phrase := basic-head-spec-phrase & head-initial.\n\n";

    }
}

print MYLANGUAGE "\;\;\; Bare NP phrase.  Consider modifying the PRED value of the quantifier relation\n";
print MYLANGUAGE "\;\;\; introduced to match the semantic effect of bare NPs in your language.\n\n";

print MYLANGUAGE "bare-np-phrase := basic-bare-np-phrase &\n";
print MYLANGUAGE "   [ C-CONT.RELS <! [ PRED \"unspec_q_rel\" ] !> ].\n\n";


#Lexical types

print MYLANGUAGE "\n\;\;\; Lexical types\n\n";

#There ought to be some way to clean this up, but doing it
#like this for now.

#Lexical types for nouns

print MYLANGUAGE "\;\;\; Nouns\n\n";

my($noun1spr) = $matrix_form{"noun1spr"};
my($noun2spr) = $matrix_form{"noun2spr"};
my($singlentype) = 1;

if ($noun1spr) {

    if ($noun2spr && ($noun1spr ne $noun2spr)) {
	$singlentype = 0;
    }
} else {
   
    return_error (500, "Internal Server Error", "You must create at least one noun entry.");
}

# Playing fast and loose with the meaning of OPT on SPR.  Using
# OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
# weird), and leaving OPT unspecified for truly optional.  Hoping
# this will work at least for LSA111 lab.  
# Funny leading white space is to make the tdl look good.

if ($singlentype) {
    
    print MYLANGUAGE "noun-lex := basic-noun-lex & basic-one-arg &\n";
    print MYLANGUAGE "           [ SYNSEM.LOCAL [ CAT.VAL [ SPR < \#spr & [ LOCAL.CAT.HEAD det";
    
    if ($noun1spr =~ /obl/) {
	print MYLANGUAGE ",\n";
        print MYLANGUAGE "                                                     OPT - ] >,\n";
    } elsif ($noun1spr =~ /nil/) {
	print MYLANGUAGE ",\n";
        print MYLANGUAGE "                                                     OPT + ] >,\n";
    } else {
	print MYLANGUAGE " ] >,\n";
    }

    print MYLANGUAGE "			                    COMPS < >,\n";
    print MYLANGUAGE "			                    SUBJ < >,\n";
    print MYLANGUAGE "			                    SPEC < > ]],\n";
    print MYLANGUAGE "    ARG-ST < \#spr > ].\n\n";
	
} else {

    print MYLANGUAGE "noun-lex := basic-noun-lex & basic-one-arg &\n";
    print MYLANGUAGE "  [ SYNSEM.LOCAL [ CAT.VAL [ SPR < \#spr & [ LOCAL.CAT.HEAD det ] >,\n";
    print MYLANGUAGE "			     COMPS < >,\n";
    print MYLANGUAGE "			     SUBJ < >,\n";
    print MYLANGUAGE "			     SPEC < > ]],\n";
    print MYLANGUAGE "    ARG-ST < \#spr > ].\n\n";

    if ($noun1spr eq "obl" || $noun2spr eq "obl") {
	
	print MYLANGUAGE "obl-spr-noun-lex := noun-lex &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].\n\n";

    }

    if ($noun1spr eq "nil" || $noun2spr eq "nil") {
	
	print MYLANGUAGE "no-spr-noun-lex := noun-lex &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].\n\n";

    }

}
    
#Lexical types for verbs

print MYLANGUAGE "\;\;\; Verbs\n\n";

my($iverbsubj) = $matrix_form{"iverbSubj"};
my($tverbsubj) = $matrix_form{"tverbSubj"};
my($tverbobj) = $matrix_form{"tverbObj"};
my($singlevtype) = 1;

if ($iverbsubj) {

    if ($tverbsubj && ($iverbsubj ne $tverbsubj)) {
	$singlevtype = 0;
    }
} else {
   
    return_error (500, "Internal Server Error", "You must create an intransitive verb entry.");
}

print MYLANGUAGE "verb-lex := basic-verb-lex &\n";
print MYLANGUAGE "  [ SYNSEM.LOCAL.CAT.VAL [ SPR < >,\n";
print MYLANGUAGE " 			   SPEC < >,\n";
print MYLANGUAGE "			   SUBJ < \#subj > ],\n";
print MYLANGUAGE "    ARG-ST < \#subj &\n";

if ($singlevtype) {

    if ($iverbsubj =~ /np/) {

	print MYLANGUAGE "             [ LOCAL.CAT [ HEAD noun,\n";

    } else {

	print MYLANGUAGE "             [ LOCAL.CAT [ HEAD adp,\n";	
	
    }

    print MYLANGUAGE "                           VAL [ SPR < >,\n";
    print MYLANGUAGE "                                 COMPS < > ]]], ... > ].\n\n";

} else {

    print MYLANGUAGE "                 [ LOCAL.CAT.VAL [ SPR < >,\n";
    print MYLANGUAGE "                                  COMPS < > ]], ... > ].\n\n";
}

print MYLANGUAGE "intransitive-verb-lex := verb-lex & intransitive-lex-item &\n";
print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.COMPS < >";

if ($singlevtype) {

    print MYLANGUAGE " ].\n\n";

} else {

    print MYLANGUAGE ",\n";
    
    if ($iverbsubj =~ /np/) {
	
	print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD noun ] > ].\n\n";
	
    } else {
	
	print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD adp ] > ].\n\n";
    }
}

print MYLANGUAGE "transitive-verb-lex := verb-lex & transitive-lex-item &\n";
print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.COMPS < \#comps >,\n";

if ($singlevtype) {

    print MYLANGUAGE "    ARG-ST < [ ],\n";

} else {

    if ($tverbsubj =~ /np/) {
	print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD noun ],\n";
    } else {
	print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD adp ],\n";
    }

}

print MYLANGUAGE "              \#comps &\n";

if ($tverbobj =~ /np/) {
    print MYLANGUAGE "              [ LOCAL.CAT.HEAD noun ] > ].\n\n";
} else {
    print MYLANGUAGE "              [ LOCAL.CAT.HEAD adp ] > ].\n\n";
}

#Lexical types for adpositions (if present):

if ($subjadp || $objadp) {

    print MYLANGUAGE "\;\;\; Case-marking adpositions\n\n";
    print MYLANGUAGE "case-marker-p-lex := basic-one-arg & no-hcons-lex-item &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL [ CAT [ HEAD adp,\n";
    print MYLANGUAGE "                          VAL [ SPR < >,\n";
    print MYLANGUAGE "                                SUBJ < >,\n";
    print MYLANGUAGE "                                COMPS < \#comps >,\n";
    print MYLANGUAGE "                                SPEC < > ]],\n";
    print MYLANGUAGE "                    CONT [ HOOK \#hook,\n";
    print MYLANGUAGE "                           RELS <! !> ]],\n";
    print MYLANGUAGE "     ARG-ST < \#comps & [ LOCAL [ CAT [ HEAD noun,\n";
    print MYLANGUAGE "                                        VAL.SPR < > ],\n";
    print MYLANGUAGE "                                  CONT.HOOK \#hook ]] > ].\n\n";

}

#Lexical type for determiners, if the language has any:

my($hasdets) = $matrix_form{"hasDets"};

if ($hasdets =~ /t/) {

    print MYLANGUAGE "\;\;\; Determiners\n";
    print MYLANGUAGE "\;\;\; SPEC is non-empty, and already specified by basic-determiner-lex.\n\n";

    print MYLANGUAGE "determiner-lex := basic-determiner-lex & basic-zero-arg &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL [ SPR < >,\n";
    print MYLANGUAGE "                            COMPS < >,\n";
    print MYLANGUAGE "                            SUBJ < > ]].\n\n";

}

close (MYLANGUAGE);

#Create a customized rules.tdl file, and store in matrix.user/
#FIXME: Need to add optdet and head-spec to the rules taken
#from the basic order files.

my($rulesfile,$rulesdest);

if ($wordorder =~ /(sov)|(osv)|(v-final)/) {
    $rulesfile = "V-final-rules.tdl";} 
elsif ($wordorder =~  /svo/) {
    $rulesfile = "SVO-rules.tdl";}
elsif ($wordorder =~  /(vso)|(vos)|(v-initial)/) {
    $rulesfile = "V-initial-rules.tdl";}
elsif ($wordorder =~  /ovs/) {
    $rulesfile = "OVS-rules.tdl";}
elsif ($wordorder =~  /free/) {
    $rulesfile = "free-order-rules.tdl";}
else {
    return_error (500,"Internal Server Error","No value specified for word order")
}

$rulesfile = "$modules_home"."$rulesfile";
$rulesdest = "$matrix"."/rules.tdl";
`cp $rulesfile $rulesdest`;

#Add to rules file if $consistentorder is vo-postp or ov-prep

open (RULESINST, ">>$matrix/"."rules.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: rules.tdl");

if ($consistentorder =~ /(vo)|(ov)/ ) {

    print RULESINST "\;\;\; Additional head-complement rule for adpositions.\n";

    if ($consistentorder =~ /vo/) {

	print RULESINST "comp-head := comp-head-phrase.\n\n";

    } else {

	print RULESINST "head-comp := head-comp-phrase.\n\n";

    }
}

print RULESINST "\;\;\; Rule(s) for building NPs.\n\n";

if ($noundetorder =~ /SpecHead/) {

    print RULESINST "spec-head := spec-head-phrase.\n";

} else {

    print RULESINST "head-spec := head-spec-phrase.\n";

}

print RULESINST "bare-np := bare-np-phrase.\n\n";

close (RULESINST);

#Create a customized irules.tdl file, and store in matrix.user/.
#If no irules are used, substitute standard empty file.

my($irulesfile,$irulesdest);
$irulesfile = "$modules_home"."irules.tdl";
$irulesdest = "$matrix"."/irules.tdl";
`cp $irulesfile $irulesdest`;

#Create a customized lrules.tdl file, and store in matrix.user/.
#If no lrules are used, substitute standard empty file.

my($lrulesfile,$lrulesdest);
$lrulesfile = "$modules_home"."lrules.tdl";
$lrulesdest = "$matrix"."/lrules.tdl";
`cp $lrulesfile $lrulesdest`;

#Create a customized lexicon.tdl file, and store in matrix.user/.

open (LEXICON, ">$matrix"."/lexicon.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: lexicon.tdl.");

print LEXICON ";;; -*- Mode: TDL; Package: LKB -*-\n\n";
print LEXICON ";;; Nouns\n\n";

my($noun,$nounpred,$nounspr);

#Since I call the following twice, it should probably be separated
#out into a subroutine.

$noun = $matrix_form{"noun1"};
$nounpred = $matrix_form{"noun1pred"};
$nounspr = $matrix_form{"noun1spr"};

if ($noun) {

    unless ($nounpred){
	return_error (500,"Internal Server Error","You must provide a pred name for each noun you specify.");
    }

    unless ($nounspr){
	return_error (500,"Internal Server Error","You must indicate the optionality of the specified for each noun you provide.");
    }

    print LEXICON "$noun := ";
    
    if ($singlentype || $nounspr =~ /opt/) {
	print LEXICON "noun-lex &\n";
    } elsif ($nounspr =~ /obl/) {
	print LEXICON "obl-spr-noun-lex &\n";
    } else {
	print LEXICON "no-spr-noun-lex &\n";
    }

    print LEXICON "   [ STEM < \"$noun\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$nounpred\" ].\n\n";

}

$noun = $matrix_form{"noun2"};
$nounpred = $matrix_form{"noun2pred"};
$nounspr = $matrix_form{"noun2spr"};

if ($noun) {

    unless ($nounpred){
	return_error (500,"Internal Server Error","You must provide a pred name for each noun you specify.");
    }

    unless ($nounspr){
	return_error (500,"Internal Server Error","You must indicate the optionality of the specifier for each noun you provide.");
    }

    print LEXICON "$noun := ";
    
    if ($singlentype || $nounspr =~ /opt/) {
	print LEXICON "noun-lex &\n";
    } elsif ($nounspr =~ /obl/) {
	print LEXICON "obl-spr-noun-lex &\n";
    } else {
	print LEXICON "no-spr-noun-lex &\n";
    }

    print LEXICON "   [ STEM < \"$noun\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$nounpred\" ].\n\n";

}

print LEXICON "\;\;\; Verbs\n\n";

my($iverb) = $matrix_form{"iverb"};
my($iverbpred) = $matrix_form{"ivpred"};
my($tverb) = $matrix_form{"tverb"};
my($tverbpred) = $matrix_form{"tvpred"};
   
if ($iverb) {

    unless ($iverbpred) {
	return_error (500, "Internal Server Error", "You must specify a predicate name for the intransitive verb.");
    }

    print LEXICON "$iverb := intransitive-verb-lex &\n";
    print LEXICON "   [ STEM < \"$iverb\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$iverbpred\" ].\n\n";

}
   
if ($tverb) {

    unless ($tverbpred) {
	return_error (500, "Internal Server Error", "You must specify a predicate name for the transitive verb.");
    }

    print LEXICON "$tverb := transitive-verb-lex &\n";
    print LEXICON "   [ STEM < \"$tverb\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$tverbpred\" ].\n\n";

}

if ($subjadpform || $objadpform) {
    print LEXICON "\;\;\; Case-marking adpositions\n\n";
}

if ($subjadpform) {

    print LEXICON "subj-marker := case-marker-p-lex &\n";
    print LEXICON "   [ STEM < \"$subjadpform\" > ].\n\n";

}

if ($objadpform) {

    print LEXICON "obj-marker := case-marker-p-lex &\n";
    print LEXICON "   [ STEM < \"$objadpform\" > ].\n\n";

}

my($det1) = $matrix_form{"det1"};
my($det1pred) = $matrix_form{"det1pred"};
my($det2) = $matrix_form{"det2"};
my($det2pred) = $matrix_form{"det2pred"};

if ($det1) {

    unless ($det1pred) {
	return_error (500, "Internal Server Error", "You must supply a predicate name for each determiner you define." );
    }

    print LEXICON "\;\;\; Determiners\n\n";
    print LEXICON "$det1 := determiner-lex &\n";
    print LEXICON "   [ STEM < \"$det1\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$det1pred\" ].\n\n";

}

if ($det2) {

    unless ($det2pred) {
	return_error (500, "Internal Server Error", "You must supply a predicate name for each determiner you define." );
    }

    print LEXICON "$det2 := determiner-lex &\n";
    print LEXICON "   [ STEM < \"$det2\" >,\n";
    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$det2pred\" ].\n\n";

}

#Package up the matrix in an archive, and print out instructions
#for downloading it.

my($tarmatrix) = "matrix.$user.tar";
my($tgzmatrix) = "$tarmatrix.gz";
my($usermatrix) = "matrix.$user";
my($tmpdir) = "$home"."tmp";
my($zipmatrix) = "matrix.$user.zip";
my($download);

#Change working directory to the tmp directory.

chdir $tmpdir;

if ($delivery =~ /tgz/) {

    `tar -cf $tarmatrix $usermatrix`;
    `gzip $tarmatrix`;
    $download = "$tarmatrix".".gz";

} else {

    `zip -r $zipmatrix $usermatrix`;
    $download = $zipmatrix;

}

print "<h3>Customized Matrix</h3>\n";
print "<p>A customized copy of the Matrix has been created for you.  Please download it <a href=\"http://www.delph-in.net/matrix/tmp/$download\">here</a>\n";
print "<p>This file will be removed from the system in 15 minutes.  If you wish to recreate it, please fill out <a href=\"www.delph-in.net/matrix/modules.html\">the form</a> again.\n";
print "<h3>Instructions</h3>\n";
print "<p>To unpack the archive (if your browswer hasn't already done it for you), first try saving it on your desktop and double clicking it.  If that doesn't work, and you're using Linux of Mac OS X, from a command prompt, type <tt>\"tar xzf matrix.$user.tar.gz\"</tt>.";
print "<p>Once you've unpacked the archive you should find a directory called <tt>matrix.$user</tt>. Inside the directory are several files.  Here is an explanation of some:\n";
print "<ul>\n";
print "<li><tt>matrix.tdl</tt>: Language independent type and constraint definitions.  You should not need to modify this file.\n";
print "<li><tt>$my_language</tt>: Types and constraints specific to your language.  This is where you will add additional constraints.\n";
print "<li><tt>lexicon.tdl</tt>: Lexical entries for your language.\n";
print "<li><tt>rules.tdl</tt>: Phrase structure rule instance entries for your language.\n";
print "<li><tt>irules.tdl</tt>: Spelling-changing lexical rule instance entries for your language.\n";
print "<li><tt>lrules.tdl</tt>: Non-spelling-changing lexical rule instance entries for your language.\n";
print "<li><tt>lkb/script</tt>: The script file for loading your grammar into the LKB.\n";
print "</ul>\n";

print "<hr>\n";
print "<a href=\"http://www.delph-in.net/matrix/modules.html\">Back to form</a><br>\n";
print "<a href=\"http://hpsg.stanford.edu/05inst/\">Back to course page</a><br>\n";
print "<a href=\"http://catalyst.washington.edu/webtools/epost/register.cgi?owner=ebender&id=12192\">To course bulletin board</a><br>\n";


print "</body></html>\n";

   
# -------------------------------------------------------------
# Subroutines

sub make_user_id 
{
    my($id,@id,$file);
    $id = rand(1000);
    @id = split(/\./,$id);
    $id = $id[0];

    return($id);
}
	
sub remove_old_files
{

    my(@tmpls,$tmpls,$date,@date,$now,@now,$today,$thisminute,$thishour,$fileday,$filetime,@filetime,$filehour,$fileminute,$home,$matrix,@matrix,$filename,$torm,$hourdiff);
    $date = `date`;
    @date = split(' ',$date);
    $today = $date[2];
    $now = $date[3];
    @now = split(':',$now);
    $thishour = $now[0];
    $thisminute = $now[1];

    $home = `ls -ld ~uwcl`;
    @home = split(' ',$home);
    $home = $home[8]."/matrix/tmp";

    $tmpls = `ls -l $home`;
    @tmpls = split('\n',$tmpls);

    for ($matrix=1; $matrix<@tmpls; $matrix++) {

	@matrix = split(' ',$tmpls[$matrix]);
	$fileday = $matrix[6];
	$filetime = $matrix [7];
	@filetime = split(':',$filetime);
	$filehour = $filetime[0];
	$fileminute = $filetime[1];
	$filename = $matrix[8];
	$hourdiff = $thishour - $filehour;

	if ($fileday ne $today) {

	    $torm = "$home"."/$filename";
	    `rm -rf $torm`;

	} elsif (($thishour - $filehour) > 1 ) {

	    $torm = "$home"."/$filename";
	    `rm -rf $torm`;

	} elsif ($hourdiff == 1) { 

	    if (($thisminute > 15) || ($fileminute < 45)) {

		$torm = "$home"."/$filename";
		`rm -rf $torm`;
	    }
	
	} elsif (($thisminute - $fileminute) > 15) {

	    $torm = "$home"."/$filename";
	    `rm -rf $torm`;
	}
    }
}    

sub parse_form_data
{
    my(%FORM_DATA,$request_method,$query_string,@key_value_pairs,$key_value,$key,$value);

    *FORM_DATA = @_;

#    local (*FORM_DATA) = @_;
#    local ( $request_method, $query_string, @key_value_pairs, 
#	    $key_value, $key, $value);

    $request_method = $ENV{'REQUEST_METHOD'};
    
    if ($request_method eq "GET") {
	$query_string = $ENV{'QUERY_STRING'};
    } elsif ($request_method eq "POST") {
	read (STDIN, $query_string, $ENV{'CONTENT_LENGTH'});
    } #else {
#	&return_error (500, "Server Error", 
#		       "Server uses unsupported method");
#    }

    @key_value_pairs = split (/&/, $query_string);

    foreach $key_value (@key_value_pairs) {
	($key, $value) = split (/=/, $key_value);
	$value =~ tr/+/ /;
	$value =~ s/%([\dA-Fa-f][\dA-Fa-f])/pack ("C", hex ($1))/eg;

	if (defined($FORM_DATA{$key})) {
	    $FORM_DATA{$key} = join ("\0", $FORM_DATA{$key}, $value);
	} else {
	    $FORM_DATA{$key} = $value;
	}

	if ($key =~ /sentence/) {
	    unless ($value =~ /[a-zA-Z0-9_ ]*/) {
		&return_error (500, "Internal Server Error",
			       "Value of $key is invalid. Please use only letters, numbers, spaces and _.");
	    }
	} else {
	    unless ($value =~ /[a-zA-Z0-9_]*/) {
		&return_error (500, "Internal Server Error",
			       "Value of $key is invalid. Please use only letters, numbers and _.");
	    }
	}
    }
    return %FORM_DATA;
}

sub return_error 
{
#    local ($status, $keyword, $message) = @_;

    my($status, $keyword, $message) = @_;
    
    print "Content-type: text/html", "\n";
    print "Status: ", $status, " ", $keyword, "\n\n";

    print <<End_of_Error;

<title>CGI Program - Unexpected Error</title>
<h1>$keyword</h1>
<hr>$message</hr>

End_of_Error

    exit(1);
}


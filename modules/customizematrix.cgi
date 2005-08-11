#!/usr/local/bin/perl
#
#First pass at html-based program for customizing
#the matrix.  Contains word order modules, rudimentary
#lexicon support, and (potentially) sentential negation.

#########################################################################
#TODO:
#
#0. Move more stuff off into subroutines.
#
#1. Consolidate word order things in order to
#   take adpositions into consideration from the start (to
#   avoid postiting addenda to types defined in my_language.tdl.
#
#2. Consider moving particularly ugly subroutines off into different
#   files.
#
#3. Add yes-no questions. 
#
#4. Figure out what's wrong .tar.gz output.
#   Test/fix negation (esp. independent adv): add head-modifier rules!
##########################################################################

#Declare the package of this file, mm for "matrix modules"

package mm;

#Clean up any files that are hanging around from previous
#downloads, so the file space doesn't get filled up.

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

#This subroutine now also extracts all of the values returned
#by the form and stores them in global variables of package mm

&parse_form_data;

#Some error checking: Do we have all of the values we need?

&check_for_form_errors;

#Create a copy of the core-matrix files.

&copy_core_matrix;

#Create a customized script file, and store it in matrix.user/lkb/

&customize_script;

#Create a customized my_language.tdl file, and store in matrix.user/

open (MYLANGUAGE, ">$mm::matrix/"."$mm::my_language") || return_error (500,"Internal Server Error","Cannot open necessary output file.");

&print_mylanguage_headers;
&print_head_type_addenda_tdl;
&print_value_types_tdl;
&print_word_order_tdl;
&print_lex_rule_types_tdl;
&print_lex_types_tdl;

close (MYLANGUAGE);

#Create a customized rules.tdl file, and store in matrix.user/

&create_rules_tdl;

#Create a customized irules.tdl file, and store in matrix.user/.
#If no irules are used, substitute standard empty file.

&create_irules_tdl;

#Create a customized lrules.tdl file, and store in matrix.user/.
#If no lrules are used, substitute standard empty file.

&create_lrules_tdl;

#Create a customized lexicon.tdl file, and store in matrix.user/.

&create_lexicon_tdl;


#Package up the matrix in an archive, and print out instructions
#for downloading it.

&output_matrix;

#Print html output file.

&print_html;

   
#######################################################################################################
# Subroutines
#
# These are presently organized according to which tdl file is
# being modified.  This might not be the ideal way to do it.

#-------------------------------------------------------------------------

sub make_user_id 
{
    my($id,@id,$file);
    $id = rand(1000);
    @id = split(/\./,$id);
    $id = $id[0];

    return($id);
}
	
#-------------------------------------------------------------------------

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
    @mm::home = split(' ',$home);
    $home = $mm::home[8]."/matrix/tmp";

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
#-------------------------------------------------------------------------

sub parse_form_data
{
    my(%FORM_DATA,$request_method,$query_string,@key_value_pairs,$key_value,$key,$value);

    *FORM_DATA = @_;

    $request_method = $ENV{'REQUEST_METHOD'};
    
    if ($request_method eq "GET") {
	$query_string = $ENV{'QUERY_STRING'};
    } elsif ($request_method eq "POST") {
	read (STDIN, $query_string, $ENV{'CONTENT_LENGTH'});
#    } #else {
#	&return_error (500, "Server Error", 
#		       "Server uses unsupported method");
    }

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


    #Now, extract each piece of information and store it in a global variable
    #in package mm.  Some of these may be empty, leading to undefined variables.
    #That's fine: The error checking (&check_for_form_errors) should handle it.

    $mm::auxcomp = $FORM_DATA{"auxcomp"};
    $mm::auxorder = $FORM_DATA{"auxorder"};
    $mm::auxsem = $FORM_DATA{"auxsem"};
    $mm::auxsubj = $FORM_DATA{"auxsubj"};
    $mm::auxverbform = $FORM_DATA{"auxverb"};
    $mm::delivery = $FORM_DATA{"delivery"};
    $mm::det1 = $FORM_DATA{"det1"};
    $mm::det1pred = $FORM_DATA{"det1pred"};
    $mm::det2 = $FORM_DATA{"det2"};
    $mm::det2pred = $FORM_DATA{"det2pred"};
    $mm::hasdets = $FORM_DATA{"hasDets"};
    $mm::hasdets = $FORM_DATA{"hasDets"};
    $mm::iverb = $FORM_DATA{"iverb"};
    $mm::iverbnf = $FORM_DATA{"iverb-nonfinite"};
    $mm::iverbpred = $FORM_DATA{"ivpred"};
    $mm::iverbsubj = $FORM_DATA{"iverbSubj"};
    $mm::ivsubj = $FORM_DATA{"iverbSubj"};
    $mm::language = $FORM_DATA{"language"};
    $mm::multineg = $FORM_DATA{"multineg"};
    $mm::my_language = $FORM_DATA{"language"}.".tdl";
    $mm::neg = $FORM_DATA{"neg"};
    $mm::negadv = $FORM_DATA{"neg-adv"};
    $mm::negadvform = $FORM_DATA{"negadvform"};
    $mm::negaff = $FORM_DATA{"neg-aff"};
    $mm::negaffform = $FORM_DATA{"neg-aff-form"};
    $mm::neginfltype = $FORM_DATA{"neg-infl-type"};
    $mm::negmod = $FORM_DATA{"negmod"};
    $mm::negprepostmod = $FORM_DATA{"negprepostmod"};
    $mm::negseladv = $FORM_DATA{"neg-sel-adv"};
    $mm::noun1 = $FORM_DATA{"noun1"};
    $mm::noun1pred = $FORM_DATA{"noun1pred"};
    $mm::noun1spr = $FORM_DATA{"noun1spr"};
    $mm::noun1spr = $FORM_DATA{"noun1spr"};
    $mm::noun2 = $FORM_DATA{"noun2"};
    $mm::noun2pred = $FORM_DATA{"noun2pred"};
    $mm::noun2spr = $FORM_DATA{"noun2spr"};
    $mm::noun2spr = $FORM_DATA{"noun2spr"};
    $mm::noundetorder = $FORM_DATA{"NounDetOrder"};
    $mm::objadp = $FORM_DATA{"objAdp"};
    $mm::objadpform = $FORM_DATA{"objAdpForm"};
    $mm::ques = $FORM_DATA{"ques"};
    $mm::qinvverb = $FORM_DATA{"qinvverb"};
    $mm::qpartposthead = $FORM_DATA{"qpartposthead"};
    $mm::qpartform = $FORM_DATA{"qpartform"};
    $mm::sentence1 = $FORM_DATA{"sentence1"};
    $mm::sentence2 = $FORM_DATA{"sentence2"};
    $mm::subjadp = $FORM_DATA{"subjAdp"};
    $mm::subjadpform = $FORM_DATA{"subjAdpForm"};
    $mm::tverb = $FORM_DATA{"tverb"};
    $mm::tverbnf = $FORM_DATA{"tverb-nonfinite"};
    $mm::tverbobj = $FORM_DATA{"tverbObj"};
    $mm::tverbpred = $FORM_DATA{"tvpred"};
    $mm::tverbsubj = $FORM_DATA{"tverbSubj"};
    $mm::tvobj = $FORM_DATA{"tverbObj"};
    $mm::tvsubj = $FORM_DATA{"tverbSubj"};
    $mm::wordorder = $FORM_DATA{"wordorder"};
#   $mm:: = $FORM_DATA{""};

#Some secondary information, calculated on the basis of the above:

#Is the ordering of adpositions consistent with the order of 
#V and O?

    $mm::consistentorder = "easy";

    if ($mm::subjadp || $mm::objadp) {
	
	$mm::adp = ($mm::subjadp || $mm::objadp);
	
	if ($mm::wordorder =~ /free/) {
	    $mm::consistentorder = "free";
	} elsif (($mm::wordorder =~ /(sov)|(osv)|(ovs)|v-final/) && ($mm::adp =~ /pre/)) {
	    $mm::consistentorder = "ov-prep";
	} elsif (($mm::wordorder =~ /(svo)|(vos)|(vso)|v-initial/) && ($mm::adp =~ /post/)) {
	    $mm::consistentorder = "vo-postp";
	}
    }


#Is the ordering of auxiliaries and their complements consistent
#with the order of other verbs?

    $mm::auxconsistentorder = "easy";

    if ($mm::auxverbform) {

	if ($mm::wordorder =~ /free/) {
	    if ($mm::auxorder =~ /left/) {
		$mm::auxconsistentorder = "free-aux-left";
	    } elsif ($mm::auxorder =~ /right/) {
		$mm::auxconsistentorder = "free-aux-right";
	    }

	} elsif (($mm::wordorder =~ /(sov)|(osv)|(ovs)|v-final/) && ($mm::auxorder =~ /left/)) {
	    $mm::auxconsistentorder = "ov-auxv";
	} elsif (($mm::wordorder =~ /(svo)|(vos)|(vso)|v-initial/) && ($mm::auxorder =~ /right/)) {
	    $mm::auxconsistentorder = "vo-vaux";
	}
    }

#Do the noun entries have the same behavior wrt to overt determiners?

    $mm::singlentype = 1;

    if ($mm::noun1spr) {

	if ($mm::noun2spr && ($mm::noun1spr ne $mm::noun2spr)) {
	    $mm::singlentype = 0;
	}
    } else {
	
	return_error (500, "Internal Server Error", "Something's wrong with the noun entries.  Please contact developers.");
    }

#Do the verbs take the same category (NP or PP) for their subjects?

    if ($mm::iverbsubj) {
	
	if ($mm::tverbsubj && ($mm::iverbsubj ne $mm::tverbsubj)) {
	    $mm::singlevtype = 0;
	} else {
	    $mm::singlevtype = 1;
	}
    } else {
	
	return_error (500, "Internal Server Error", "Something's wrong with the intransitive verb. Please contact developers.");
    }
    

#What's going on with the distribution of multiple negation strategies?

    if (($mm::neq eq "adv") || ($mm::multineg)) {
	if (($mm::neg eq "adv") || ($mm::multineg =~ /comp/)) {
	    $mm::advalone = "always";
	} elsif ($mm::multineg =~ /bothopt|advobl/) {
	    $mm::advalone = "sometimes";
	} elsif ($mm::multineg =~ /bothobl|inflobl/) {
	    $mm::advalone = "never"; 
	} else {
	    return_error (500, "Internal Server Error", "There's something wrong with adverbial negation.  Please contact the developers.");
	}
    }

#For the independent adverb negation strategy, are the adverbs
#prehead or posthead?

    if ($mm::negadv =~ /ind-adv/) {

	if ($mm::negprepostmod =~ /pre/) {
	    $mm::posthead = "-";
	} elsif ($mm::negprepostmod =~ /post/) {
	    $mm::posthead = "+";
	} elsif ($mm::negprepostmod =~ /either/) {
	    $mm::prepostmod = "";
	} else {
	    return_error (500, "Internal Sever Error", "Something's wrong with the mod order for neg adv.  Please contact developers.");
	}
    }



#Do we need a feature AUX?  So far, could be using it in either of the
#negation strategies or one of the question strategies.  
#Note that if the value for either is "main-aux"
#we don't yet need a main verb/auxiliary distinction.  (Though one might
#expect to find one elsewhere, since the terms are used.)

    if (($mm::neginfltype eq "aux") || ($mm::negseladv eq "aux")
	|| ($mm::qinvverb eq "aux")) {
	$mm::hasaux = 1;
    } else {
	$mm::hasaux = 0;
    }

#Do we need head-modifier rules?

    if (($mm::negadv =~ /ind-adv/) &&
	(!$mm::advalone ||
	 $mm::advalone =~ /always|sometimes/)) {

	$mm::modrules = "t";

    } else {

	$mm::modrules = "nil";

    }

#Do we need to constrain HC-LIGHT on verbs, to distinguish V from VP?

    if (($mm::negadv =~ /ind-adv/ && $mm::negmod eq "V") ||
	($mm::auxcomp eq "V")) {

	$mm::hclight = "t";

    } else {

	$mm::hclight = "nil";
    }

#Which type of auxiliary, if any, are we using?  Store the type
#name as $mm::auxtypename.

    if ($mm::hasaux) {
	if ($mm::auxcomp =~ /VP/) {
	    if ($mm::auxsem =~ /pred/) {
		$mm::auxtypename = "subj-raise-aux-with-pred";
	    } elsif ($mm::auxsem =~ /tma/) {
		$mm::auxtypename = "subj-raise-aux-no-sem";
	    } else {
		return_error (500, "Internal Server Error", "Something's wrong with the aux type.  Please contact developers.");
	    }
	} elsif ($mm::auxcomp =~ /V/) {
	    if ($mm::auxsem =~ /pred/) {
		$mm::auxtypename = "arg-comp-aux-with-pred";
	    } elsif ($mm::auxsem =~ /tma/) {
		$mm::auxtypename = "arg-comp-aux-no-sem";
	    } else {
		return_error (500, "Internal Server Error", "Something's wrong with the aux type.  Please contact developers.");
	    }
	} elsif ($mm::auxcomp =~ /S/) {
	    if ($mm::auxsem =~ /pred/) {
		$mm::auxtypename = "s-comp-aux-with-pred";
	    } elsif ($mm::auxsem =~ /tma/) {
		$mm::auxtypename = "s-comp-aux-no-sem";
	    } else {
		return_error (500, "Internal Server Error", "Something's wrong with the aux type.  Please contact developers.");
	    }
	} else {
	    return_error (500, "Internal Server Error", "Something's wrong with the aux type.  Please contact developers.");
	    
	}
    }
   
}

#-------------------------------------------------------------------------

sub check_for_form_errors {

#Did they specify a word order?

    unless ($mm::wordorder) {
	return_error (500,"Internal Server Error","No value specified for word order");
    }


#Did they specify an archive type?
  
    unless ($mm::delivery) {
	return_error (500, "Internal Server Error", "You must specify an archive type (.tar.gz or .zip).");
    }

#If they want a PP subject, did they supply an adposition?

    if (($mm::tvsubj =~ /pp/ || $mm::ivsubj =~ /pp/) && !$mm::subjadpform) {
	return_error (500, "Internal Server Error", "If one or both of your verbs take PP subjects, you must provide a subject-marking adposition.");
    }

#If they want a PP object, did they supply an adposition?
    
    if ($mm::tvobj =~ /pp/ && !$mm::objadpform) {
	return_error (500, "Internal Server Error", "If your transitive verb takes PP objects, you must provide an object-marking adposition.");
    }

#If they want and adposition, did they specify its ordering possibilities?

    if (($mm::subjadpform && !$mm::subjadp) || ($mm::objadpform && !$mm::objadp)) {
	return_error (500, "Internal Server Error", "All adpositions must be specified as either prepositions or postpositions.");
    }

#If they have two adpositions, do they have the same ordering possibilities?

    if ($mm::subjadp && $mm::objadp && !($mm::subjadp eq $mm::objadp)) {
	return_error (500, "Internal Server Error", "Systems with inconsistent adposition placement not currently supported.");
    }

#Did they specify whether the language has determiners?

    unless ($mm::hasdets) {
	return_error (500, "Internal Server Error", "You must specify whether your language has determiners as independent words.");
    }

#If they want determiners, did they specify the noun-det order?

    if ($mm::hasdets =~ /t/) {
	unless ($mm::noundetorder) {
	    return_error (500, "Internal Server Error", "If your language has independent determiners, you must specify which side of the noun they appear on.");
	}
    }

#Did they answer all of the questions about the negation strategy they chose?
#This one is buggy right now, so commented out.  FIX_ME and put back in.

#    if (($mm::neg =~ /adv/ && !($mm::negmod && $mm::negprepostmod)) || (($mm::neg =~ /infl/) && !($mm::neginfltype && $mm::negaff && $mm::negaffform))) {
#	return_error (500, "Internal Server Error", "You must answer all questions for each negation strategy you choose.");
#    }

#Check that if both inflectional and adverbial negation are selected, one of the options for combining them is too.

    if ($mm::neg =~ /adv/ && $mm::neg =~ /infl/) {
	unless ($mm::multineg) {
	    return_error (500, "Internal Server Error", "If you select both inflectional and adverbial negation, you must specify how they may or may not cooccur.");
	}
    }
    
#If they chose adverbial negation, did they give a form for the adverb?

    if ($mm::negadvform) {
	unless ($mm::neg =~ /adv/) {
	    return_error (500, "Internal Server Error", "Do not specify a form for a negative adverb, unless you also choose the adverbial sentential negation strategy.");
	}
    }

#If they specify a form for the adverb, did they choose adverbial negation?

    if ($mm::neg =~ /adv/) {
	unless ($mm::negadvform) {
	    return_error (500, "Internal Server Error", "If you select adverbial negation, you must specify a form for the negative adverb in the lexicon section.");
	}
    }

#Check that if anything requiring and auxiliary is selected, an auxiliary is specified.
#This one does negation so far.  Revisit when I add question formation.
 
    if ($mm::neginfltype eq "aux" || $mm::negseladv =~ /aux/) {
	unless ($mm::auxverbform) {
	    return_error (500, "Internal Server Error", "You have indicated that sentential negation requires the presence of an auxiliary, but you have not specified a lexical entry for the auxiliary.");
	}
    }

#Did they specify enough lexical entries?

    unless ($mm::noun1 && $mm::iverb && $mm::tverb) {
	return_error (500, "Internal Server Error", "You must create an intransitive verb entry, a transitive verb entry, and at least one noun entry.");
    }

#Did they give pred names?
   
    unless (($mm::noun1pred || !$mm::noun1) &&
	    ($mm::noun2pred || !$mm::noun2) &&
	    ($mm::det1pred || !$mm::det1) &&
	    ($mm::det2pred || !$mm::det2) &&
	    $mm::iverbpred && $mm::tverbpred) {
	return_error (500, "Internal Server Error", "You must specify a predicate value for each noun, (main) verb, and determiner you specify.");
    }

#Did they answer all of the questions about lexical entries?

    unless (($mm::noun1spr || !$mm::noun1) &&
	    ($mm::noun2spr || !$mm::noun2) &&
	    ($mm::iverbsubj || !$mm::iverb) &&
	    ($mm::tverbsubj || !$mm::tverb) &&
	    ($mm::tverbobj || !$mm::iverb) &&
	    ($mm::objadpform || !$mm::objadp) &&
	    ($mm::subjadpform || !$mm::subjadp) &&
	    (($mm::auxsem && $mm::auxcomp && $mm::auxorder) || !$mm::auxverbform)) {
	return_error (500, "Internal Server Error", "You must answer all questions for each lexical entry you specify.");
    }

#Did they give us the same form for both finite and nonfinite verbs?

    if (($mm::tverb eq $mm::tverbnf) || ($mm::iverb eq $mm::iverbf)) {

	return_error (500, "Internal Server Error", "If you provide a form for a verb when it cooccurs with an auxiliary, it must be different from the other (finite) form.");

    }

#If they're specifying an auxiliary, and they say it takes a VP or V complement,
#did they tell us what type of subject?

    if ($mm::auxverbform) {
	if (($mm::auxcomp =~ /V/) & !$mm::auxsubj) {
	    return_error (500, "Internal Server Error", "If your auxiliary takes a V or VP complement, you must specify whether its subject is an NP or a PP.");
	}
    }

#If they selected the question particle strategy, did they specify 
#the order and the form?

    if ($mm::ques =~ /qpart/) {
	unless ($mm::qpartposthead) {
	    return_error (500, "Internal Server Error", "If you chose the question particle strategy for yes-no questions, you must specify where the question particle appears.");
	}
	unless ($mm::qpartform) {
	    return_error (500, "Internal Server Error", "If you chose the question particle strategy for yes-no questions, you must specify the form of the question particle.");
	}
    }
}

#-------------------------------------------------------------------------

sub return_error 
{
    my($status, $keyword, $message) = @_;
    
    print "Content-type: text/html", "\n";
    print "Status: ", $status, " ", $keyword, "\n\n";
    print <<End_of_Error;

<title>CGI Program - Unexpected Error</title>
<h1>$keyword</h1>
<hr>$message
<p>Use the back button on your browser to correct your entries and resubmit the form.
<hr>

End_of_Error

exit(1);
}

#-------------------------------------------------------------------------

sub copy_core_matrix
{
#Make these package-global variables, since multiple subroutines might want them.
#my($user,@mm::home,$home,$matrix,$core_matrix,$modules_home);

    $mm::user = &make_user_id;
    
#open and close don't interpret ~.  Need to get shell to
#interprest it for me.
#$home = "~uwcl/matrix/";

    $mm::home = `ls -ld ~uwcl`;
    @mm::home = split(' ',$mm::home);
    $mm::home = $mm::home[8]."/matrix/";
    
    $mm::matrix = "$mm::home"."tmp/"."matrix"."\.$mm::user";
    $mm::core_matrix = "$mm::home"."matrix-core";
    $mm::modules_home = "$mm::home"."modules/";
    
#Note that $mm::core_matrix and $mm::matrix are not user supplied
#fields, so even though this way does pass its arguments to /bin/sh
#for interpretation, this should be safe.  Note that cp also
#passes its arguments to /bin/sh, so even using "system" wouldn't
#be better.

    `cp -r $mm::core_matrix $mm::matrix`;

}

#-------------------------------------------------------------------------

sub customize_script 
{

#This subroutine fills in the appropriate file for my_language.tdl and
#also fills in the default sentences for the parse dialogue, if they
#are supplied by the user.  Note that in this version of the modules
#customization, the script file does not need any further tweaking.
#All module-generated tdl is included in the `standard' files (my_language.tdl,
#lexicon.tdl, irules.tdl, etc).

    open (LKBSCRIPT, ">$mm::matrix"."/lkb/script") || return_error (500,"Internal Server Error","Cannot open necessary output file.");
    open (BASESCRIPT, "$mm::modules_home"."script") || return_error (500,"Internal Server Error","Cannot open necessary input file: script.");

    while (<BASESCRIPT>) {

	if (/^\;\;\; Modules: LOAD my_language.tdl/) {
	    print LKBSCRIPT "   (lkb-pathname (parent-directory) \"$mm::my_language\")\n";
	} elsif ($_ =~ /^\;\;\; Modules: Default sentences/ && defined($mm::sentence1)){
	    print LKBSCRIPT "(if (eq (length *last-parses*) 1)\n";
	    print LKBSCRIPT "   (setf *last-parses* '(\"$mm::sentence1\" \"$mm::sentence2\")))\n";
	} else {
	    print LKBSCRIPT;
	}
    }
    
    close (LKBSCRIPT);
    close (BASESCRIPT);
}

#-------------------------------------------------------------------------

sub print_mylanguage_headers
{
    print MYLANGUAGE ";;; -*- Mode: TDL; Package: LKB -*-\n";
    print MYLANGUAGE ";;;\n";
    print MYLANGUAGE ";;; Language-specific types and constraints for ",$mm::language,"\n\n";
}

#-------------------------------------------------------------------------

sub print_head_type_addenda_tdl
{
#Gather all things we want to add to the head types and print them here.

    print MYLANGUAGE ";;; Type addenda adding constraints to head types\n\n";

    #Add the features AUX, FORM, and INV, if necessary.

    if ($mm::hasaux == 1) {
	print MYLANGUAGE ";;; Your grammar has auxiliaries, so we are adding the features AUX and FORM\n";
	print MYLANGUAGE ";;; to the type verb.  We are assuming that auxiliaries select non-finite verbal\n";
	print MYLANGUAGE ";;; projections for their complements.\n\n";

	print MYLANGUAGE ";;; To allow for a simpler statement of word order rules (in some grammars)\n";
	print MYLANGUAGE ";;; we add the feature AUX to the type head, rather than verb.\n\n";

	print MYLANGUAGE ";;; For the analysis of inverted yes-no questions, we add the feature INV.\n";

	print MYLANGUAGE "head :+ [ AUX bool ].\n\n";
	print MYLANGUAGE "verb :+ [ FORM form";

	if ($mm::ques =~ /inv/) {
	    print MYLANGUAGE ",\n INV bool ].\n\n";
	} else {
	    print MYLANGUAGE " ].\n\n";
	}
    }

    #Constrain the MOD values of non-modifiers if we're adding head-adj rules

    if ($mm::modrules =~ /t/) {

	print MYLANGUAGE ";;; This grammar includes head-modifier rules.  To keep\n";
	print MYLANGUAGE ";;; out extraneous parses, constrain the value of MOD on\n";
	print MYLANGUAGE ";;; various subtypes of head.  This may need to be loosened later.\n";
	print MYLANGUAGE ";;; This constraint says that only adverbs, adjectives,\n";
	print MYLANGUAGE ";;; and adpositions can be modifiers.\n\n";

	print MYLANGUAGE "+nvcdmo :+ [ MOD < > ].\n\n";

    }

}

#-------------------------------------------------------------------------

sub print_value_types_tdl
{
#Types for values of things like FORM

    print MYLANGUAGE ";;; Types for values of additional features.\n\n";

    if ($mm::hasaux == 1) {
	
	print MYLANGUAGE ";;; Value of the feature FORM, used to distinguish\n;;; which verbs appear after auxiliaries.\n\n";

	print MYLANGUAGE "form := avm.\n";
	print MYLANGUAGE "fin := form.\n";
	print MYLANGUAGE "inf := form.\n\n";

    }
}

#-------------------------------------------------------------------------

sub print_word_order_tdl
{
#First, if there are adpositions, is their placement consistent
#with the O/V order?

    my($ruletypesinput);

    if ($mm::wordorder =~ /sov/) {
	$ruletypesinput = "SOV.tdl";
    } elsif ($mm::wordorder =~  /svo/) {
	$ruletypesinput = "SVO.tdl";
    } elsif ($mm::wordorder =~  /vso/) {
	$ruletypesinput = "VSO.tdl";
    } elsif ($mm::wordorder =~  /osv/) {
	$ruletypesinput = "OSV.tdl";
    } elsif ($mm::wordorder =~  /ovs/) {
	$ruletypesinput = "OVS.tdl";
    } elsif ($mm::wordorder =~  /vos/) {
	$ruletypesinput = "VOS.tdl";
    } elsif ($mm::wordorder =~  /v-final/) {
	$ruletypesinput = "V-final.tdl";
    } elsif ($mm::wordorder =~  /v-initial/) {
	$ruletypesinput = "V-initial.tdl";
    } elsif ($mm::wordorder =~  /free/) {
	$ruletypesinput = "free-order.tdl";
    } else {
	return_error (500, "Internal Server Error", "Something's wrong with the word order.  Please contact developers.");
    }

    open (RULETYPES, "$mm::modules_home"."$ruletypesinput") || return_error (500,"Internal Server Error","Cannot open necessary input file: $ruletypesinput.");

    print MYLANGUAGE ";;; Phrase structure rule types\n\n";

    while (<RULETYPES>) {
	unless (/\;\;\; -\*- Mode: TDL/) {
	    print MYLANGUAGE;
	}
    }
    
    close (RULETYPES);
    
#Now some add-ons for those cases where the word order is
#sensitive to the head type (adpositions, auxiliaries).
#In general, we might also find word order sensitive to
#clause type (matrix v. subordinate) and dependent type.

# aux		 adp		head-comp		comp-head		#rules    add to rules.tdl
# ---		 ---		---------		---------		------    ----------------
# ov-auxv 	 ov-prep	v:AUX + | adp		~adp:AUX -		2         head-comp: both
# vo-vaux	 vo-post	~adp:AUX - 		v:AUX + | adp		2         comp-head: both
# free-aux-left	 free-prep	unrestricted		~adp:AUX - 		2         --
# free-aux-right free-post	~adp:AUX -		unrestricted		2         --
# free-aux-left	 free-post	~adp			AUX -			2         --
# free-aux-right free-prep	AUX -			~adp			2         --
# easy		 ov-prep	adp			~adp			2         head-comp: adp
# easy		 vo-post	~adp			adp			2         comp-head: adp
# easy		 free-prep	unrestricted		~adp			2         --
# easy		 free-post	~adp			unrestricted		2         --
# ov-auxv	 easy		v:AUX +			AUX -			2         head-comp: aux
# vo-vaux	 easy		AUX -			v:AUX +			2         comp-head: aux
#
# Not bothering with the case where adpositions aren't fixed in their order,
# since I don't believe it exists.
    
    if (($mm::consistentorder =~ /ov-prep/) && ($mm::auxconsistentorder =~ /ov-auxv/)) {

	print MYLANGUAGE ";;; The following type addendum statment restricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions or auxiliaries as its head.\n\n";

	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo &\n";
	print MYLANGUAGE "                                            [ AUX - ]].\n\n";

	print MYLANGUAGE ";;; Head-initial head-comp phrases for auxiliaries and prepositions.\n";
	print MYLANGUAGE ";;; Using the disjunctive type for verbs and prepositions to state\n";
	print MYLANGUAGE ";;; this with one rule. Note that AUX is appropriate for all head types\n";
	print MYLANGUAGE ";;; and prepositions are underspecified for AUX.\n\n";

	print MYLANGUAGE "head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ]].\n";

    }
    
    if (($mm::consistentorder =~ /vo-post/) && ($mm::auxconsistentorder =~ /vo-vaux/)) {

	print MYLANGUAGE ";;; The following type addendum statment restricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions or auxiliaries as its head.\n\n";

	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo &\n";
	print MYLANGUAGE "                                            [ AUX - ]].\n\n";

	print MYLANGUAGE ";;; Head-final head-comp phrases for auxiliaries and prepositions.\n";
	print MYLANGUAGE ";;; Using the disjunctive type for verbs and prepositions to state\n";
	print MYLANGUAGE ";;; this with one rule. Note that AUX is appropriate for all head types\n";
	print MYLANGUAGE ";;; and prepositions are underspecified for AUX.\n\n";

	print MYLANGUAGE "comp-head-phrase := basic-head-1st-comp-phrase & head-final &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ]].\n\n";

    }

    if (($mm::auxconsistentorder =~ /free-aux-left/) && 
	($mm::consistentorder =~ /free/) && ($mm::adp =~ /pre/)) {

	print MYLANGUAGE ";;; The following type addendum statement resricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions or auxiliaries as its head.\n\n";

	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo &\n";
	print MYLANGUAGE "                                            [ AUX - ]].\n\n";
	    
    }
    
    if (($mm::auxconsistentorder =~ /free-aux-right/) && 
	($mm::consistentorder =~ /free/) && ($mm::adp =~ /post/)) {

	print MYLANGUAGE ";;; The following type addendum statement resricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions or auxiliaries as its head.\n\n";

	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo &\n";
	print MYLANGUAGE "                                            [ AUX - ]].\n\n";
	
    }
    
    if (($mm::auxconsistentorder =~ /free-aux-left/) && 
	($mm::consistentorder =~ /free/) && ($mm::adp =~ /post/)) {

	print MYLANGUAGE ";;; The following type addendum statement resricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as its head.\n\n";

	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	    
	print MYLANGUAGE ";;; The following type addendum statement resricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking auxiliaries as its head.\n\n";

	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].\n\n";

    }

    if (($mm::auxconsistentorder =~ /free-aux-right/) && 
	($mm::consistentorder =~ /free/) && ($mm::adp =~ /pre/)) {

	print MYLANGUAGE ";;; The following type addendum statement resricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as its head.\n\n";

	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	    
	print MYLANGUAGE ";;; The following type addendum statement resricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking auxiliaries as its head.\n\n";

	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].\n\n";

    }
    

    if (($mm::consistentorder =~ /ov-prep/) && ($mm::auxconsistentorder =~ /easy/)) {
	
	print MYLANGUAGE ";;; The following type addendum statment restricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as its head.\n\n";
	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	
	print MYLANGUAGE "head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD adp ].\n\n";
	
    }
    
    if (($mm::consistentorder =~ /vo-postp/) && ($mm::auxconsistentorder =~ /easy/)) {
	
	print MYLANGUAGE ";;; The following type addendum statment restricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as itshead.\n\n";
	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	
	print MYLANGUAGE "comp-head-phrase := basic-head-1st-comp-phrase & head-initial &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD adp ].\n\n";
	
    }

    if (($mm::consistentorder =~ /free/) && ($mm::auxconsistentorder =~ /easy/)) {

	if ($mm::adp =~ /pre/) {
	    
	    print MYLANGUAGE ";;; The following type addendum statment restricts the head-final\n";
	    print MYLANGUAGE ";;; head-complement rule from taking adpositions as itshead.\n\n";
	    print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	    
	} else {
	    
	    print MYLANGUAGE ";;; The following type addendum statment restricts the head-initial\n";
	    print MYLANGUAGE ";;; head-complement rule from taking adpositions as itshead.\n\n";
	    print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].\n\n";
	    
	}
    }

    if (($mm::consistentorder =~ /easy/) && ($mm::auxconsistentorder =~ /ov-auxv/)) {

	print MYLANGUAGE ";;; The following type addendum statment restricts the head-final\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as its head.\n\n";
	print MYLANGUAGE "comp-head-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].\n\n";
	
	print MYLANGUAGE "head-comp-phrase := basic-head-1st-comp-phrase & head-initial &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].\n\n";

    }

    if (($mm::consistentorder =~ /easy/) && ($mm::auxconsistentorder =~ /vo-vaux/)) {

	print MYLANGUAGE ";;; The following type addendum statment restricts the head-initial\n";
	print MYLANGUAGE ";;; head-complement rule from taking adpositions as its head.\n\n";
	print MYLANGUAGE "head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].\n\n";
	
	print MYLANGUAGE "comp-head-phrase := basic-head-1st-comp-phrase & head-initial &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].\n\n";

    }


#Now the rules for NPs: spec-head or head-spec, plus also the
#opt-det rule.
    
    print MYLANGUAGE ";;; Rules for building NPs.  Note that the Matrix uses SPR for\n";
    print MYLANGUAGE ";;; the specifier of nouns and SUBJ for the subject (specifier) of verbs.\n\n";
    
    if ($mm::hasdets =~ /t/) {
	if ($mm::noundetorder =~ /SpecHead/) {
	    
	    print MYLANGUAGE "spec-head-phrase := basic-head-spec-phrase & head-final.\n\n";
	    
	} else {
	    
	    print MYLANGUAGE "head-spec-phrase := basic-head-spec-phrase & head-initial.\n\n";
	    
	}
    }

    print MYLANGUAGE ";;; Bare NP phrase.  Consider modifying the PRED value of the quantifier relation\n";
    print MYLANGUAGE ";;; introduced to match the semantic effect of bare NPs in your language.\n\n";
    
    print MYLANGUAGE "bare-np-phrase := basic-bare-np-phrase &\n";
    print MYLANGUAGE "   [ C-CONT.RELS <! [ PRED \"unspec_q_rel\" ] !> ].\n\n";
    
}

#-------------------------------------------------------------------------

sub print_lex_rule_types_tdl
{
#sub routine for printing lex rule types

#Since we're only positing lexical rules for negation (and maybe
#question formation) for now, I'm not bothering with the feature
#INFLECTED.  There's a large issue here concerning modularity,
#however: what to do about ordering of lexical rules?

#A related question is how to keep the lexical rules from spinning
#or the multiple negation lexical rules from feeding each other.
#For now, requiring daughter to be a lexical item, but that might
#not even scale to the questions stuff.

#Actually, it works for the English case at least.  We want the
#sai rule to apply after the negation rule, and that rule won't
#spin because of differing SUBJ values.  But clearly, a more
#general solution is required.

#Types for negation: 

#Selected adverb, with or without inflection.  The same rule type
#is used in both cases, but the corresponding rule instance may
#vary.  Indeed, some languages may have two instances of this rule,
#one in irules.tdl (for inflection+adv) and one in lrules.tdl (adv only).

    if (($mm::neg =~ /adv/) && ($mm::negadv =~ /sel-adv/)) {

	print MYLANGUAGE ";;; This lexical rule adds a selected negative\n";
	print MYLANGUAGE ";;; adverb to the beginning of the COMPS list.\n";

    #Decide first how many instances we need, and whether we're talking
    #spelling-changing or not, so we can inherit from the appropriate
    #supertypes.

	if ($mm::advalone =~ /always/ ) {
	    print MYLANGUAGE ";;; This type is instantiated in lrules.tdl\n\n";
	} elsif ($mm::advalone =~ /sometimes/) {
	    print MYLANGUAGE ";;; This type has subtypes instantiated by instances in\n";
	    print MYLANGUAGE ";;; both irules.tdl and lrules.tdl\n\n";
	} elsif ($mm::advalone =~ /never/ ) { 
	    print MYLANGUAGE ";;; This type is instantiated in irules.tdl\n\n";
	}

	print MYLANGUAGE "neg-add-lex-rule := local-change-only-lex-rule &\n";
	print MYLANGUAGE "                    same-ctxt-lex-rule &\n";
	print MYLANGUAGE "                    same-agr-lex-rule &\n";
	print MYLANGUAGE "                    same-head-lex-rule &\n";
	print MYLANGUAGE "                    same-hc-light-lex-rule &\n";
	print MYLANGUAGE "                    same-posthead-lex-rule &\n";

	if ($mm::advalone =~ /always/) {
	    print MYLANGUAGE "                    constant-lex-rule &\n";
	} elsif ($mm::advalone =~ /never/) {
	    print MYLANGUAGE "                    inflecting-lex-rule &\n";
	}

        print MYLANGUAGE "   [ SYNSEM.LOCAL [ CAT.VAL [ SUBJ \#subj,\n";
	print MYLANGUAGE "                              SPR \#spr,\n";
	print MYLANGUAGE "                              SPEC \#spec,\n";
	print MYLANGUAGE "                              COMPS < [ LOCAL [ CONT [ HOOK [ INDEX \#negind,\n";
	print MYLANGUAGE "                                                               LTOP \#negltop ],\n";
	print MYLANGUAGE "                                                       HCONS <! [ LARG \#larg ] !> ]],\n";
	print MYLANGUAGE "                                                LKEYS.KEYREL.PRED \"_neg_r_rel\" ] . \#comps > ],\n";
	print MYLANGUAGE "                    CONT [ HOOK [ INDEX \#negind,\n";
	print MYLANGUAGE "                                  LTOP \#negltop,\n";
	print MYLANGUAGE "                                  XARG \#xarg ],\n";
	print MYLANGUAGE "                           MSG \#msg ]],\n";
	print MYLANGUAGE "     DTR lex-item & [ SYNSEM.LOCAL [ CAT [ VAL [ SUBJ \#subj,\n";
	print MYLANGUAGE "                                                 SPR \#spr,\n";
	print MYLANGUAGE "                                                 SPEC \#spec,\n";
	print MYLANGUAGE "                                                 COMPS \#comps ],\n";
	print MYLANGUAGE "                                           HEAD verb";

	#_FIX_ME_: In theory, one could have a langauge where one type of verb takes
	#inflection + adverb, and the other only inflection.  Not going there for now...

	if (($mm::negseladv =~ /main-aux/) ||
	    ($mm::hasaux == 0)) {
	    print MYLANGUAGE " ],\n";
	} elsif ($mm::negseladv =~ /aux/) {
	    print MYLANGUAGE "& [ AUX + ]],\n";
	} elsif ($mm::negseladv =~ /main/ && $mm::hasaux == 1) {
	    print MYLANGUAGE "& [ AUX - ]],\n";
	} else {
	    return_error (500, "Internal Server Error", "There's something wrong with selected adverb neagation.  Please contact developers.");
	}

	print MYLANGUAGE "                                     CONT [ HOOK [ LTOP \#larg,\n";
	print MYLANGUAGE "                                                   XARG \#xarg ],\n";
	print MYLANGUAGE "                                            MSG \#msg ]]]].\n\n";

	if ($mm::advalone =~ /sometimes/) {
	    print MYLANGUAGE "infl-neg-add-lex-rule := neg-add-lex-rule & inflecting-lex-rule\n";
	    print MYLANGUAGE "const-neg-add-lex-rule := neg-add-lex-rule & constant-lex-rule\n\n";
	}

    }

     #Inflection without selected adverb
     #This one adds the '_neg_r_rel, and as such is only used
     #when inflection appears alone (infl strategy only, both
     #strategies with multineg = comp, bothopt, inflobl).

    #Spell _neg_r_rel with leading _ even though it is introduced
    #by the lexical rule so that "the cat didn't sleep" and "the
    #cat did not sleep" have the same representation.

    #Copying up LKEYS here because I use the KEYREL.PRED to select
    #the neg adv in the neg-add-lex-rule.  We don't want the output
    #of this rule to be a possible first complement to a neg-add aux.
    #If we find another way to select the neg adv, something will
    #probably need to be changed here.

    if (($mm::neg =~ /infl/) && ($mm::multineg ne "bothobl")
	&& ($mm::multineg ne "advobl")) {

	print MYLANGUAGE ";;; This lexical rule adds the neg_r_rel to the verb's\n";
	print MYLANGUAGE ";;; RELS list.  It is instantiated by a spelling-changing\n";
	print MYLANGUAGE ";;; rule as specified in irules.tdl.\n\n";

	print MYLANGUAGE "neg-infl-lex-rule := cont-change-only-lex-rule &\n";
	print MYLANGUAGE "                     inflecting-lex-rule &\n";
	print MYLANGUAGE "   [ C-CONT [ MSG \#msg,\n";
	print MYLANGUAGE "              HOOK [ XARG \#xarg,\n";
	print MYLANGUAGE "                     LTOP \#ltop,\n";
	print MYLANGUAGE "                     INDEX \#ind ],\n";
	print MYLANGUAGE "              RELS <! event-relation &\n";
	print MYLANGUAGE "                      [ PRED \"_neg_r_rel\",\n";
	print MYLANGUAGE "                        LBL \#ltop,\n";
	print MYLANGUAGE "                        ARG0 \#ind,\n";
	print MYLANGUAGE "                        ARG1 \#harg ] !>,\n";
	print MYLANGUAGE "              HCONS <! qeq &\n";
	print MYLANGUAGE "                       [ HARG \#harg,\n";
	print MYLANGUAGE "                         LARG \#larg ] !> ],\n";
	print MYLANGUAGE "     SYNSEM.LKEYS \#lkeys,\n";
	print MYLANGUAGE "     DTR lex-item & \n";
	print MYLANGUAGE "         [ SYNSEM [ LKEYS \#lkeys,\n";
	print MYLANGUAGE "                    LOCAL [ CONT [ MSG \#msg,\n";
	print MYLANGUAGE "                                 HOOK [ XARG \#xarg,\n";
	print MYLANGUAGE "                                        LTOP \#larg ]],\n";
	print MYLANGUAGE "                          CAT.HEAD verb";

	if (($mm::neginfltype =~ /main-aux/) ||
	    ($mm::hasaux == 0)) {
	    print MYLANGUAGE " ]]]].\n\n";
	} elsif ($mm::neginfltype =~ /aux/) {
	    print MYLANGUAGE "& [ AUX + ]]]]].\n\n";
	} elsif ($mm::neginfltype =~ /main/ && $mm::hasaux == 1) {
	    print MYLANGUAGE "& [ AUX - ]]]]].\n\n";
	} else {
	    return_error (500, "Internal Server Error", "There's something wrong with inflectional neagation.  Please contact developers.");
	}
    }

    #Types for subject-verb and subject-aux inversion question strategies

    if ($mm::ques =~ /inv/) {

	print MYLANGUAGE ";;; Rule for inverted subject verb order in questions.\n\n";
	print MYLANGUAGE ";;; The incompatible SUBJ values on SYNSEM and DTR are\n";
	print MYLANGUAGE ";;; what keeps this one from spinning.\n";

	print MYLANGUAGE "subj-v-inv-lrule := val-change-only-lex-rule &\n";
	print MYLANGUAGE "                    constant-lex-rule &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ HEAD verb & [ INV +";

	if ($mm::qinvverb eq "aux") {
	    print MYLANGUAGE ",\n                                       AUX + ],\n";
	} elsif ($mm::qinvverb eq "main") {
	    print MYLANGUAGE ",\n                                       AUX - ],\n";
	} elsif ($mm::qinvverb eq "main-aux") {
	    print MYLANGUAGE " ],\n";
	} else {
	    return_error (500, "Internal Server Error", "There's something wrong with the verb type (main/aux) for inverted questions.  Please contact developers.");
	}

	print MYLANGUAGE "                        VAL [ COMPS < \#subj . \#comps >,\n";
	print MYLANGUAGE "                              SUBJ < >,\n";
	print MYLANGUAGE "                              SPR \#spr,\n";
	print MYLANGUAGE "                              SPEC \#spec ]],\n";
	print MYLANGUAGE "     DTR.SYNSEM.LOCAL.CAT.VAL [ SUBJ < \#subj >,\n";
	print MYLANGUAGE "                                COMPS \#comps,\n";
	print MYLANGUAGE "                                SPR \#spr,\n";
	print MYLANGUAGE "                                SPEC \#spec ]].\n\n";
     
    }

}

#-------------------------------------------------------------------------

sub print_lex_types_tdl
{
    print MYLANGUAGE "\n\;\;\; Lexical types\n\n";

#There ought to be some way to clean this up, but doing it
#like this for now.

#Lexical types for nouns

    print MYLANGUAGE ";;; Nouns\n\n";
    
# Playing fast and loose with the meaning of OPT on SPR.  Using
# OPT - to mean obligatory (as usual), OPT + to mean impossible (that's
# weird), and leaving OPT unspecified for truly optional.  Hoping
# this will work at least for LSA111 lab.  
# Funny leading white space is to make the tdl look good.

    if ($mm::singlentype) {
    
	print MYLANGUAGE "noun-lex := basic-noun-lex & basic-one-arg &\n";
	print MYLANGUAGE "           [ SYNSEM.LOCAL [ CAT.VAL [ SPR < \#spr & [ LOCAL.CAT.HEAD det";
	
	if ($mm::noun1spr =~ /obl/) {
	    print MYLANGUAGE ",\n";
	    print MYLANGUAGE "                                                     OPT - ] >,\n";
	} elsif ($mm::noun1spr =~ /nil/) {
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
	
	if ($mm::noun1spr eq "obl" || $mm::noun2spr eq "obl") {
	    
	    print MYLANGUAGE "obl-spr-noun-lex := noun-lex &\n";
	    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT - ] > ].\n\n";

	}

	if ($mm::noun1spr eq "nil" || $mm::noun2spr eq "nil") {
	    
	    print MYLANGUAGE "no-spr-noun-lex := noun-lex &\n";
	    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.SPR < [ OPT + ] > ].\n\n";
	    
	}
	
    }
    
#Lexical types for verbs
#I'm adding the constraint to associate XARG with the
#first ARG-ST element here (so raising auxiliaries work),
#but perhaps this belongs in matrix.tdl?  Or maybe this
#is another module/parameter (like, the external argument
#might not be the first one?

    print MYLANGUAGE ";;; Verbs\n\n";
    
    print MYLANGUAGE "verb-lex := basic-verb-lex &\n";
    print MYLANGUAGE "  [ SYNSEM.LOCAL [ CAT [ VAL [ SPR < >,\n";
    print MYLANGUAGE " 			             SPEC < >,\n";
    print MYLANGUAGE "			             SUBJ < \#subj > ]";

    if ($mm::hasaux) {
	print MYLANGUAGE ",\n                         HEAD.AUX - ],\n";
    } else {
	print MYLANGUAGE "],\n";
    }

    print MYLANGUAGE "                   CONT.HOOK.XARG \#xarg ],\n";
    print MYLANGUAGE "    ARG-ST < \#subj &\n";
    
    if ($mm::singlevtype) {
	
	if ($mm::iverbsubj =~ /np/) {
	    
	    print MYLANGUAGE "             [ LOCAL [ CAT [ HEAD noun,\n";
	    
	} else {
	    
	    print MYLANGUAGE "             [ LOCAL [ CAT [ HEAD adp,\n";	
	    
	}
	
	print MYLANGUAGE "                             VAL [ SPR < >,\n";
	print MYLANGUAGE "                                   COMPS < > ]],\n";
	print MYLANGUAGE "                       CONT.HOOK.INDEX \#xarg ]], ... > ].\n\n";
	
    } else {
	
	print MYLANGUAGE "                 [ LOCAL [ CAT.VAL [ SPR < >,\n";
	print MYLANGUAGE "                                     COMPS < > ],\n";
        print MYLANGUAGE "                           CONT.HOOK.INDEX \#xarg ]], ... > ].\n\n";
    }
    
    if ($mm::hclight =~ /t/) {
	

	print MYLANGUAGE ";;; If there are aspects of the syntax which pick out\n";
	print MYLANGUAGE ";;; lexical Vs (transitive or intransitive) such as V-attachment\n";
	print MYLANGUAGE ";;; of adverbs or argument composition auxiliaries which take V\n";
	print MYLANGUAGE ";;; complements, we need to distinguish (intranstive) V and VP.\n";
	print MYLANGUAGE ";;; To do so, we make use of a feature LIGHT.  Phrases are generally [LIGHT -]\n";
	print MYLANGUAGE ";;; with the exception of head-complement phrases, which take their\n";
	print MYLANGUAGE ";;; value for LIGHT from the head's HC-LIGHT feature.  To make this\n";
	print MYLANGUAGE ";;; work for us here, constraint HC-LIGHT on verbs to be -.\n\n";
	
	print MYLANGUAGE "verb-lex :+ [ SYNSEM.LOCAL.CAT.HC-LIGHT - ].\n\n";

    }

    print MYLANGUAGE "intransitive-verb-lex := verb-lex & intransitive-lex-item &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.COMPS < >";
    
    if ($mm::singlevtype) {
	
	print MYLANGUAGE " ].\n\n";
	
    } else {
	
	print MYLANGUAGE ",\n";
	
	if ($mm::iverbsubj =~ /np/) {
	    
	    print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD noun ] > ].\n\n";
	    
	} else {
	    
	    print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD adp ] > ].\n\n";
	}
    }
    
    print MYLANGUAGE "transitive-verb-lex := verb-lex & transitive-lex-item &\n";
    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL.COMPS < \#comps >,\n";
    
    if ($mm::singlevtype) {
	
	print MYLANGUAGE "    ARG-ST < [ ],\n";
	
    } else {
	
	if ($mm::tverbsubj =~ /np/) {
	    print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD noun ],\n";
	} else {
	    print MYLANGUAGE "     ARG-ST < [ LOCAL.CAT.HEAD adp ],\n";
	}
	
    }

    print MYLANGUAGE "              \#comps &\n";
    print MYLANGUAGE "              [ LOCAL.CAT [ VAL [ SPR < >,\n";
    print MYLANGUAGE "                                  COMPS < > ],\n";
    
    if ($mm::tverbobj =~ /np/) {
	print MYLANGUAGE "                            HEAD noun ]] > ].\n\n";
    } else {
	print MYLANGUAGE "                            HEAD adp ]] > ].\n\n";
    }
    
#If there's auxiliaries, define finite and non-finite verb types,
#cross-classify with trans and intrans.

    if ($mm::hasaux == 1) {
	print MYLANGUAGE ";;; Types for finite and non-finite verbs.  These will\n";
	print MYLANGUAGE ";;; most likely need to be replaced with lexical rules\n";
	print MYLANGUAGE ";;; deriving the finite and non-finite forms from verb stems.\n\n";

	print MYLANGUAGE "finite-verb-lex := verb-lex &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD.FORM fin ].\n\n";

	print MYLANGUAGE "non-finite-verb-lex := verb-lex &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.HEAD.FORM inf ].\n\n";

	print MYLANGUAGE "finite-trans-verb-lex := finite-verb-lex & transitive-verb-lex.\n";
	print MYLANGUAGE "non-finite-trans-verb-lex := non-finite-verb-lex & transitive-verb-lex.\n";
	print MYLANGUAGE "finite-intrans-verb-lex := finite-verb-lex & intransitive-verb-lex.\n";
	print MYLANGUAGE "non-finite-intrans-verb-lex := non-finite-verb-lex & intransitive-verb-lex.\n";

    }

#Lexical types for adpositions (if present):
    
    if ($mm::subjadp || $mm::objadp) {
	
	print MYLANGUAGE ";;; Case-marking adpositions\n";
	print MYLANGUAGE ";;; Case marking adpositions are constrained not to\n";
	print MYLANGUAGE ";;; be modifiers.\n\n";
	print MYLANGUAGE "case-marker-p-lex := basic-one-arg & raise-sem-lex-item &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ HEAD adp & [ MOD < > ],\n";
	print MYLANGUAGE "                        VAL [ SPR < >,\n";
	print MYLANGUAGE "                              SUBJ < >,\n";
	print MYLANGUAGE "                              COMPS < \#comps >,\n";
	print MYLANGUAGE "                              SPEC < > ]],\n";
	print MYLANGUAGE "     ARG-ST < \#comps & [ LOCAL.CAT [ HEAD noun,\n";
	print MYLANGUAGE "                                      VAL.SPR < > ]] > ].\n\n";
	
    }
    
#Lexical type for determiners, if the language has any:
    
    if ($mm::hasdets =~ /t/) {
	
	print MYLANGUAGE ";;; Determiners\n";
	print MYLANGUAGE ";;; SPEC is non-empty, and already specified by basic-determiner-lex.\n\n";
	
	print MYLANGUAGE "determiner-lex := basic-determiner-lex & basic-zero-arg &\n";
	print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT.VAL [ SPR < >,\n";
	print MYLANGUAGE "                            COMPS < >,\n";
	print MYLANGUAGE "                            SUBJ < > ]].\n\n";
	
    }

#Lexical type for auxiliaries.  There's probably more we can give them
#here, if we ask more questions (are there auxiliaries with both independent
#preds and those which just contribute tense/aspect information?)...

    if ($mm::hasaux) {

	print MYLANGUAGE ";;; Auxiliaries\n\n";

	if ($mm::auxcomp =~ /VP/) {

	    print MYLANGUAGE "subj-raise-aux := trans-first-arg-raising-lex-item &\n";
	    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ VAL [ SUBJ < \#subj >,\n";
	    print MYLANGUAGE "                              COMPS < \#comps >,\n";
	    print MYLANGUAGE "                              SPR < >,\n";
	    print MYLANGUAGE "                              SPEC < > ],\n";
	    print MYLANGUAGE "                        HEAD verb & [ AUX + ]],\n";
	    print MYLANGUAGE "     ARG-ST < \#subj &\n";
	    print MYLANGUAGE "              [ LOCAL.CAT [ VAL  [ SPR < >,\n";
	    print MYLANGUAGE "                                   COMPS < > ],\n";
	    print MYLANGUAGE "                            HEAD";

	    if ($mm::auxsubj =~ /noun/) {

		print MYLANGUAGE " noun ]],\n";

	    } elsif ($mm::auxsubj =~ /adp/) {

		print MYLANGUAGE " adp ]],\n";

	    } else {

		return_error (500, "Internal Server Error", "There's something wrong with the subject type for your auxiliary.  Please contact developers.");
	    }

	    print MYLANGUAGE "              \#comps &\n";
	    print MYLANGUAGE "              [ LOCAL.CAT [ VAL [ SUBJ < [ ] >,\n";
	    print MYLANGUAGE "                                  COMPS < > ],\n";
	    print MYLANGUAGE "                            HEAD verb &\n";
	    print MYLANGUAGE "                                 [ FORM inf ]]] > ].\n\n";

	    if ($mm::auxsem =~ /pred/) {

		print MYLANGUAGE "subj-raise-aux-with-pred := subj-raise-aux &\n";
                print MYLANGUAGE "                            trans-first-arg-raising-lex-item-1.\n\n";

	    } elsif ($mm::auxsem =~ /tma/) {

		print MYLANGUAGE "; To keep the semantically empty ones from spinning on\n";
		print MYLANGUAGE "; generation, require complement to be [AUX -].  The\n";
		print MYLANGUAGE "; FORM feature might be enough in the starter grammars,\n";
		print MYLANGUAGE "; but I don't want to rely on this.  Then again, [ AUX - ]\n";
		print MYLANGUAGE "; might not be true.  Be sure to put in a comment.\n\n";

		print MYLANGUAGE "subj-raise-aux-no-sem := subj-raise-aux &\n";
		print MYLANGUAGE "                         trans-first-arg-raising-lex-item-2 &\n"; 
		print MYLANGUAGE "   [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].\n\n";

	    } else {

		return_error (500, "Internal Server Error", "There's something wrong with the semantics of the auxiliary.  Please contact developers.");

	    }

	} elsif ($mm::auxcomp =~ /V/) {

	    print MYLANGUAGE "; Somewhat surprisingly, this inherits from basic-two-arg, so that\n";
	    print MYLANGUAGE "; the non-local features are amalgamated from subj, the lexical\n";
	    print MYLANGUAGE "; verb complement, but not the other complements, if any.\n\n";
	    
	    print MYLANGUAGE "arg-comp-aux := basic-two-arg &\n";
	    print MYLANGUAGE "  [ SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ],\n";
	    print MYLANGUAGE "                       VAL [ SUBJ < \#subj >,\n";
	    print MYLANGUAGE "                             COMPS < \#comps . \#vcomps >,\n";
	    print MYLANGUAGE "                             SPR < >,\n";
	    print MYLANGUAGE "                             SPEC < > ]],\n";
	    print MYLANGUAGE "    ARG-ST < \#subj &\n";
	    print MYLANGUAGE "             [ LOCAL [ CAT [ VAL [ SPR < >,\n";
	    print MYLANGUAGE "                                   COMPS < > ],\n";
	    print MYLANGUAGE "                             HEAD";
	    
	    if ($mm::auxsubj =~ /NP/) {
		
		print MYLANGUAGE " noun ]],\n";
		
	    } elsif ($mm::auxsubj =~ /PP/) {
		
		print MYLANGUAGE " adp ]],\n";
		
	    } else {
		
		return_error (500, "Internal Server Error", "There's something wrong with the subject type for your auxiliary.  Please contact developers.");
	    }

	    print MYLANGUAGE "                       CONT.HOOK.INDEX \#xarg ]],\n";
	    print MYLANGUAGE "             \#comps &\n";
	    print MYLANGUAGE "             [ LIGHT +,\n";
	    print MYLANGUAGE "               LOCAL [ CAT [ VAL [ SUBJ <[ ]>,\n";
	    print MYLANGUAGE "                                   COMPS \#vcomps ],\n";
	    print MYLANGUAGE "                             HEAD verb & [ FORM inf ]],\n";
	    print MYLANGUAGE "                       CONT.HOOK.XARG \#xarg ]] > ].\n\n";

	    if ($mm::auxsem =~ /pred/) {

		print MYLANGUAGE "; Not inheriting from basic-verb-lex, so need to put in event-relation\n";
		print MYLANGUAGE "; by hand here.\n\n";

		print MYLANGUAGE "arg-comp-aux-with-pred := arg-comp-aux & hcons-lex-item &\n";
		print MYLANGUAGE "   [ SYNSEM [ LOCAL [ CONT.HCONS <! qeq &\n";
		print MYLANGUAGE "                                    [ HARG \#harg,\n";
		print MYLANGUAGE "                                      LARG \#larg ] !> ],\n";
		print MYLANGUAGE "              LKEYS.KEYREL event-relation &\n";
		print MYLANGUAGE "                           [ ARG1 \#harg ]],\n";
		print MYLANGUAGE "     ARG-ST < [ ], \n";
		print MYLANGUAGE "              [ LOCAL.CONT.HOOK [ XARG \#xarg,\n";
		print MYLANGUAGE "              LTOP \#larg ]] > ].\n\n";
		    
	    } elsif ($mm::auxsem =~ /tma/) {

		print MYLANGUAGE "; Note that raise-sem-lex-item assumes the first complement is\n";
		print MYLANGUAGE "; where the HOOK comes from.  It's not clear to me how you'd tell\n";
		print MYLANGUAGE "; that you had an argument composition auxiliary if it wasn't appearing\n";
		print MYLANGUAGE "; adjacent to the verb.\n\n";
		
		print MYLANGUAGE "arg-comp-aux-no-sem := arg-comp-aux  & raise-sem-lex-item & \n";
		print MYLANGUAGE "  [ ARG-ST < [ ], [ LOCAL.CAT.HEAD.AUX - ] > ].\n\n";
		
	    } else {

		return_error (500, "Internal Server Error", "There's something wrong with the semantics of the auxiliary.  Please contact developers.");
		
	    }

	} elsif ($mm::auxcomp =~ /S/) {

	    print MYLANGUAGE "s-comp-aux := basic-one-arg & \n";
	    print MYLANGUAGE "  [ SYNSEM.LOCAL.CAT [ HEAD verb & [ AUX + ],\n";
	    print MYLANGUAGE "                       VAL [ SUBJ < >,\n";
	    print MYLANGUAGE "                             COMPS < \#comps >,\n";
	    print MYLANGUAGE "                             SPR < >,\n";
	    print MYLANGUAGE "                             SPEC < > ]],\n";
	    print MYLANGUAGE "    ARG-ST < \#comps &\n";
	    print MYLANGUAGE "             [ LOCAL.CAT [ VAL [ SUBJ < >,\n";
	    print MYLANGUAGE "                                 COMPS < > ],\n";
	    print MYLANGUAGE "                           HEAD verb & [ FORM inf ]]] > ].\n\n";
		       
	    if ($mm::auxsem =~ /pred/) {
	    
		print MYLANGUAGE "; S comp aux, with pred\n\n";

		print MYLANGUAGE "s-comp-aux-with-pred := s-comp-aux & hcons-lex-item &\n";
		print MYLANGUAGE "   [ SYNSEM [ LOCAL.CONT.HCONS <! qeq &\n";
		print MYLANGUAGE "                                  [ HARG \#harg,\n";
		print MYLANGUAGE "                                    LARG \#larg ] !>,\n";
		print MYLANGUAGE "              LKEYS.KEYREL event-relation &\n";
		print MYLANGUAGE "                           [ ARG1 \#harg ]],\n";
		print MYLANGUAGE "     ARG-ST < [ LOCAL.CONT.HOOK.LTOP \#larg ] > ].\n\n";

	    } elsif ($mm::auxsem =~ /tma/) {

		print MYLANGUAGE "; S comp aux, no sem\n\n";

		print MYLANGUAGE "; Better say [ AUX - ] on complement here, or we'll spin\n";
		print MYLANGUAGE "; on generation.\n\n";
		
		print MYLANGUAGE "s-comp-aux-no-sem := s-comp-aux & raise-sem-lex-item &\n";
		print MYLANGUAGE "  [ ARG-ST < [ LOCAL.CAT.HEAD.AUX - ] > ].\n\n";
		
	    } else {

		return_error (500, "Internal Server Error", "There's something wrong with the semantics of the auxiliary.  Please contact developers.");
		
	    }
	}
    }


#Lexical type for negative adverb, if appropriate.  Note that $mm::neg will
#match the string "adv" if it's just adverbs or if both adverbial and 
#inflectional strategies are used. 

    if ($mm::neg =~ /adv/) {

	print MYLANGUAGE ";;; Adverbs\n\n";
	print MYLANGUAGE ";;; Negative adverb\n\n";
	
	if ($mm::negadv =~ /ind-adv/) {

	    print MYLANGUAGE "neg-adv-lex := basic-scopal-adverb-lex &\n";

	    #Perhaps it would be cleaner to just store all 2x3 possibilities
	    #for the tdl (pre/post head x V/VP/S modifier)?  Hard to maintain
	    #that, though...

	    if ($mm::posthead) {
		print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ POSTHEAD $mm::posthead,\n";
		print MYLANGUAGE "                        VAL [ SPR < >,\n";
	    } else {
		print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,\n";
	    }

	    print MYLANGUAGE "                              COMPS < >,\n";
	    print MYLANGUAGE "                              SUBJ < > ],\n";
	    print MYLANGUAGE "                        HEAD.MOD < [ LOCAL.CAT [ HEAD verb,\n";

	    if ($mm::negmod =~ /S/) { 
		print MYLANGUAGE "                                                 VAL [ SUBJ null\n";
                print MYLANGUAGE "                                                       COMPS null ]]] > ]].\n\n";
	    } elsif ($mm::negmod =~ /VP/) { #Careful, "VP" will also match "V"
                print MYLANGUAGE "                                                 VAL [ SUBJ cons\n";
                print MYLANGUAGE "                                                       COMPS null ]]] > ]].\n\n";
	    } elsif ($mm::negmod =~ /V/) {
		print MYLANGUAGE "                                                 VAL.SUBJ cons ],\n";
		print MYLANGUAGE "                                     LIGHT + ] > ]].\n\n";
	    } else {
		return_error (500,"Internal Server Error", "There's something wrong with the barlevel attachment of negative adverbs.  Please contact developers.");
	    }

	} elsif ($mm::negadv =~ /sel-adv/) {

	    print MYLANGUAGE ";;; Constrain the MOD value of this adverb to keep\n";
	    print MYLANGUAGE ";;; it from modifying the kind of verbs which can select it\n";
	    print MYLANGUAGE ";;; To keep spurious parses down, as a starting point, we have\n";
	    print MYLANGUAGE ";;; assumed that it only modifies verbs (e.g., non-finite verbs).\n\n";

	    print MYLANGUAGE "neg-adv-lex := basic-scopal-adverb-lex &\n";
	    print MYLANGUAGE "   [ SYNSEM.LOCAL.CAT [ VAL [ SPR < >,\n";
	    print MYLANGUAGE "                              COMPS < >,\n";
	    print MYLANGUAGE "                              SUBJ < > ],\n";
	    print MYLANGUAGE "                        HEAD.MOD < [ LOCAL.CAT.HEAD verb ] > ]].\n\n";
	
	} else {
	    return_error (500,"Internal Server Error", "There's something wrong with the negative adverb.  Please contact developers.");
	}
    }

#Lexical type for question particle.

    if ($mm::ques =~ /qpart/) {
	
	print MYLANGUAGE "qpart-le := scopal-adverb-lex &\n";
	print MYLANGUAGE "  [ SYNSEM [ LOCAL.CAT [ HEAD.MOD < [ LOCAL.CAT [ HEAD verb,\n";
	print MYLANGUAGE "                                                  VAL [ SUBJ < >,\n";
	print MYLANGUAGE "                                                        COMPS < > ]]>],\n";
	print MYLANGUAGE "                         VAL [ SUBJ < >,\n";
	print MYLANGUAGE "                               SPR < >,\n";
	print MYLANGUAGE "                               COMPS < > ],\n";
	print MYLANGUAGE "                         POSTHEAD $mm::qpartposthead ],\n";
	print MYLANGUAGE "             LKEYS.KEYREL message &\n";
	print MYLANGUAGE "                          [ PRED question_m_rel ]]].\n\n";

    }
}

#-------------------------------------------------------------------------

sub create_rules_tdl
{
    my($rulesfile,$rulesdest);

    if ($mm::wordorder =~ /(sov)|(osv)|(v-final)/) {
	$rulesfile = "V-final-rules.tdl";} 
    elsif ($mm::wordorder =~  /svo/) {
	$rulesfile = "SVO-rules.tdl";}
    elsif ($mm::wordorder =~  /(vso)|(vos)|(v-initial)/) {
	$rulesfile = "V-initial-rules.tdl";}
    elsif ($mm::wordorder =~  /ovs/) {
	$rulesfile = "OVS-rules.tdl";}
    elsif ($mm::wordorder =~  /free/) {
	$rulesfile = "free-order-rules.tdl";}
    else {
	return_error (500,"Internal Server Error","Something's wrong with the word order.  Please contact developers.")
	}

    $rulesfile = "$mm::modules_home"."$rulesfile";
    $rulesdest = "$mm::matrix"."/rules.tdl";
    `cp $rulesfile $rulesdest`;
    
#Add to rules file if $mm::consistentorder is vo-postp or ov-prep
    
    open (RULESINST, ">>$mm::matrix/"."rules.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: rules.tdl");

    if (($mm::auxconsistentorder =~ /(vo)|(ov)/) &&
	($mm::consistentorder =~ /(vo)|(ov)/)) {

	print RULESINST ";;; Additional head-complement rule for auxiliaries and adpositions.\n\n";

    } elsif (($mm::auxconsistentorder =~ /easy/) &&
	     ($mm::consistentorder =~ /(vo)|(ov)/)) {

	print RULESINST ";;; Additional head-complement rule for adpositions.\n\n";
	
    } elsif ($mm::auxconsistentorder =~ /(vo)|(ov)/) {

	print RULESINST ";;; Additional head-complement rule for auxiliaries.\n";
 
    }

    if (($mm::consistentorder =~ /vo/) ||
	($mm::auxconsistentorder =~ /vo/)) {
	
	print RULESINST "comp-head := comp-head-phrase.\n\n";
	    
    } 

    if (($mm::consistentorder =~ /ov/) ||
	($mm::auxconsistentorder =~ /ov/)) {
	    
	print RULESINST "head-comp := head-comp-phrase.\n\n";
	    
    }
    
    print RULESINST ";;; Rule(s) for building NPs.\n\n";

    if ($mm::noundetorder =~ /SpecHead/) {
	
	print RULESINST "spec-head := spec-head-phrase.\n";
	
    } else {
	
	print RULESINST "head-spec := head-spec-phrase.\n";
	
    }
    
    print RULESINST "bare-np := bare-np-phrase.\n\n";
    
#Add head-modifier rules to rules file if necessary.  Just
#create them all at this point, even though we're only using
#the scopal rules for negation and probably only one of the two
#at that.

    if ($mm::modrules =~ /t/) {

	print RULESINST ";;; Rule instances for head-modifier structures. Corresponding types\n";
	print RULESINST ";;; are defined in matrix.tdl.  The matrix customization script did\n";
	print RULESINST ";;; not need to add any further constraints, so no corresponding tyes \n";
	print RULESINST ";;; appear in $mm::my_language\n\n";
	    
	print RULESINST "head-adj-int := head-adj-int-phrase.\n";
	print RULESINST "adj-head-int := adj-head-int-phrase.\n";
	print RULESINST "head-adj-scop := head-adj-scop-phrase.\n";
	print RULESINST "adj-head-scop := adj-head-scop-phrase.\n";

    }

    close (RULESINST);

}

#-------------------------------------------------------------------------

sub create_irules_tdl
{
# Copy over standard empty file.
    my($irulesfile,$irulesdest);
    $irulesfile = "$mm::modules_home"."irules.tdl";
    $irulesdest = "$mm::matrix"."/irules.tdl";
    `cp $irulesfile $irulesdest`;

# Then add to it if need be.

    open (IRULES, ">>$mm::matrix/"."irules.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: irules.tdl");

# Cases where we have an instance of neg-infl-lex-rule:

    if (($mm::neg =~ /infl/) && ($mm::multineg ne "bothobl")
	&& ($mm::multineg ne "advobl")) {

# Value of $mm::negaff is conveniently either the string "prefix" or
# "suffix" already from the form.

	print IRULES "neg-infl-lr :=\n";
	print IRULES "%"."$mm::negaff (* $mm::negaffform)\n";
	print IRULES "neg-infl-lex-rule.\n\n";

    }

# Cases where we have an instance of neg-add-lex-rule
# (not subtypes of same):

    if ($mm::advalone =~ /never/) {

# Value of $mm::negaff is conveniently either the string "prefix" or
# "suffix" already from the form.

	print IRULES "neg-add-lr :=\n";
	print IRULES "%"."$mm::negaff (* $mm::negaffform)\n";
	print IRULES "neg-add-lex-rule.\n\n";

    }

# Cases where we have instances of two subtypes of
# neg-add-lex-rule:

    if ($mm::advalone =~ /sometimes/) {

	print IRULES "neg-add-ir :=\n";
	print IRULES "%"."$mm::negaff (* $mm::negaffform)\n";
	print IRULES "infl-neg-add-lex-rule.\n\n";
    }

    close (IRULES);

}

#-------------------------------------------------------------------------

sub create_lrules_tdl
{
# Copy over standard empty file.
    my($lrulesfile,$lrulesdest);
    $lrulesfile = "$mm::modules_home"."lrules.tdl";
    $lrulesdest = "$mm::matrix"."/lrules.tdl";
    `cp $lrulesfile $lrulesdest`;

# Then add to it if need be.

    open (LRULES, ">>$mm::matrix/"."lrules.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: lrules.tdl");

# Cases where we have an instance of neg-add-lex-rule
# (not subtypes of same):

    if ($mm::advalone =~ /always/) {
	print LRULES "neg-add-lr := neg-add-lex-rule.\n\n";
    }

# Cases where we have instances of two subtypes of
# neg-add-lex-rule:

    if ($mm::advalone =~ /sometimes/) {
	print LRULES "neg-add-lr := const-neg-add-lex-rule.\n\n";
    }

#Lexical rule for subject-verb/subject-aux inversion.

    if ($mm::ques =~ /inv/) {
	print LRULES "inv-lr := subj-v-inv-lrule.\n\n";
    }

    close (LRULES);
}


#-------------------------------------------------------------------------

sub create_lexicon_tdl
{

    open (LEXICON, ">$mm::matrix"."/lexicon.tdl") || return_error (500,"Internal Server Error","Cannot open necessary output file: lexicon.tdl.");

    print LEXICON ";;; -*- Mode: TDL; Package: LKB -*-\n\n";
    print LEXICON ";;; Nouns\n\n";

#Since I call the following twice, it should probably be separated
#out into a subroutine.

    if ($mm::noun1) {

	print LEXICON "$mm::noun1 := ";
    
	if ($mm::singlentype || $mm::noun1spr =~ /opt/) {
	    print LEXICON "noun-lex &\n";
	} elsif ($mm::noun1spr =~ /obl/) {
	    print LEXICON "obl-spr-noun-lex &\n";
	} else {
	    print LEXICON "no-spr-noun-lex &\n";
	}
	
	print LEXICON "   [ STEM < \"$mm::noun1\" >,\n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::noun1pred\" ].\n\n";
	
    }
    
    if ($mm::noun2) {

	print LEXICON "$mm::noun2 := ";
    
	if ($mm::singlentype || $mm::noun2spr =~ /opt/) {
	    print LEXICON "noun-lex &\n";
	} elsif ($mm::noun2spr =~ /obl/) {
	    print LEXICON "obl-spr-noun-lex &\n";
	} else {
	    print LEXICON "no-spr-noun-lex &\n";
	}
	
	print LEXICON "   [ STEM < \"$mm::noun2\" >,\n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::noun2pred\" ].\n\n";
	
    }
    
    print LEXICON ";;; Verbs\n\n";
    
    if ($mm::iverb) {
	
	if ($mm::hasaux) {

	    print LEXICON "$mm::iverb := finite-intrans-verb-lex &\n";
	    print LEXICON "   [ STEM < \"$mm::iverb\">,\n";
	    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::iverbpred\" ].\n\n";
	    
	    if ($mm::iverbnf) {
		print LEXICON "$mm::iverbnf := non-finite-intrans-verb-lex &\n";
		print LEXICON "   [ STEM < \"$mm::iverbnf\" >,\n";
		print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::iverbpred\" ].\n\n";
		
	    } else {
		
		print LEXICON "$mm::iverb","2 := non-finite-intrans-verb-lex &\n";
		print LEXICON "   [ STEM < \"$mm::iverb\">,\n";
		print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::iverbpred\" ].\n\n";

	    } 

	} else {

	    print LEXICON "$mm::iverb := intransitive-verb-lex &\n";
	    print LEXICON "   [ STEM < \"$mm::iverb\" >,\n";
	    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::iverbpred\" ].\n\n";
	
	}
    }

    if ($mm::tverb) {
	
	if ($mm::hasaux) {

	    print LEXICON "$mm::tverb := finite-trans-verb-lex &\n";
	    print LEXICON "   [ STEM < \"$mm::tverb\">,\n";
	    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::tverbpred\" ].\n\n";

	    if ($mm::tverbnf) {
		print LEXICON "$mm::tverbnf := non-finite-trans-verb-lex &\n";
		print LEXICON "   [ STEM < \"$mm::tverbnf\" >,\n";
		print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::tverbpred\" ].\n\n";

	    } else {

		print LEXICON "$mm::tverb","2 := non-finite-trans-verb-lex &\n";
		print LEXICON "   [ STEM < \"$mm::tverb\">,\n";
		print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::tverbpred\" ].\n\n";

	    } 

	} else {

	    print LEXICON "$mm::tverb := transitive-verb-lex &\n";
	    print LEXICON "   [ STEM < \"$mm::tverb\" >,\n";
	    print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::tverbpred\" ].\n\n";
	
	}
    }    

    if ($mm::hasaux) {

	print LEXICON ";;; Auxiliaries\n\n";

	print LEXICON "$mm::auxverbform := $mm::auxtypename &\n";
	print LEXICON "   [ STEM < \"$mm::auxverbform\" >,\n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED ";

	if ($mm::auxsem =~ /pred/) {
	    print LEXICON "\"$mm::auxpred\" ].\n\n";
	} else {
	    print LEXICON "no-pred ].\n\n";
	}
	
    }

    print LEXICON ";;; Other\n\n";

    if ($mm::subjadpform || $mm::objadpform) {
	print LEXICON ";;; Case-marking adpositions\n\n";
    }
    
    if ($mm::subjadpform) {
	
	print LEXICON "subj-marker := case-marker-p-lex &\n";
	print LEXICON "   [ STEM < \"$mm::subjadpform\" > ].\n\n";
	
    }
    
    if ($mm::objadpform) {
	
	print LEXICON "obj-marker := case-marker-p-lex &\n";
	print LEXICON "   [ STEM < \"$mm::objadpform\" > ].\n\n";
	
    }
    
    if ($mm::det1) {

	print LEXICON ";;; Determiners\n\n";
	print LEXICON "$mm::det1 := determiner-lex &\n";
	print LEXICON "   [ STEM < \"$mm::det1\" >,\n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::det1pred\" ].\n\n";
	
    }
    
    if ($mm::det2) {
	
	print LEXICON "$mm::det2 := determiner-lex &\n";
	print LEXICON "   [ STEM < \"$mm::det2\" >,\n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"$mm::det2pred\" ].\n\n";
	
    }

    if ($mm::negadvform) {

	print LEXICON "$mm::negadvform := neg-adv-lex &\n";
	print LEXICON "   [ STEM < \"$mm::negadvform\" >, \n";
	print LEXICON "     SYNSEM.LKEYS.KEYREL.PRED \"_neg_r_rel\" ].\n\n";

    }

    if ($mm::qpartform) {
	
	print LEXICON "$mm::qpartform := qpart-lex &\n";
	print LEXICON "   [ STEM < \"$mm::qpartform\" > ].\n\n";

    }
}


#------------------------------------------------------------

sub output_matrix
{
    my($tarmatrix) = "matrix.$mm::user.tar";
    my($tgzmatrix) = "$tarmatrix.gz";
    my($usermatrix) = "matrix.$mm::user";
    my($tmpdir) = "$mm::home"."tmp";
    my($zipmatrix) = "matrix.$mm::user.zip";

#Change working directory to the tmp directory.

    chdir $tmpdir;

    if ($mm::delivery =~ /tgz/) {
	
	`tar -cf $tarmatrix $usermatrix`;
	`gzip $tarmatrix`;
	$mm::download = "$tarmatrix".".gz";
	
    } else {
	
	`zip -r $zipmatrix $usermatrix`;
	$mm::download = $zipmatrix;
	
    }
}

#------------------------------------------------------------

sub print_html 
{
#Print headers for html output file.

    print "Content-type: text/html", "\n\n";
    print "<html>\n<head>\n";
    print "<title>Matrix Customized</title>\n";
    print "</head><body>";

#Print content of html output file.
    print <<End_of_Html;
  
<h3>Customized Matrix</h3>

<p>A customized copy of the Matrix has been created for you.  
Please download it <a href="http://www.delph-in.net/matrix/tmp/$mm::download">here</a>

<p>This file will be removed from the system in 15 minutes.  If you wish to recreate it, 
please fill out <a href="http://www.delph-in.net/matrix/modules.html">the form</a> again.

<h3>Instructions</h3>

<p>To unpack the archive, if your browswer hasn't already done it for you, first try saving it 
on your desktop and double clicking it.  If that doesn't work, and you're using Linux or 
Mac OS X, from a command prompt, type <tt>&quot;tar xzf matrix.$mm::user.tar.gz&quot;</tt>.

<p>Once you've unpacked the archive you should find a directory called <tt>matrix.$mm::user</tt>. 
Inside the directory are several files.  Here is an explanation of some:

<ul>
<li><tt>matrix.tdl</tt>: Language independent type and constraint definitions.  You should not need to modify this file.
<li><tt>$mm::my_language</tt>: Types and constraints specific to your language.  This is where you will add additional constraints.
<li><tt>lexicon.tdl</tt>: Lexical entries for your language.
<li><tt>rules.tdl</tt>: Phrase structure rule instance entries for your language.
<li><tt>irules.tdl</tt>: Spelling-changing lexical rule instance entries for your language.
<li><tt>lrules.tdl</tt>: Non-spelling-changing lexical rule instance entries for your language.
<li><tt>lkb/script</tt>: The script file for loading your grammar into the LKB.
</ul>

<hr>
<a href="http://www.delph-in.net/matrix/modules.html">Back to form</a><br>
<a href="http://www.delph-in.net/matrix/">Back to Matrix main page</a><br>
</body></html>

End_of_Html
}


#!/usr/bin/perl

# packages to include

use Switch;

# Perl script for automatically generating a grammar prototype
# including the core Matrix as well as appropriate modules for
# word order, negation, and yes-no questions.  This will instantiate
# files as needed in matrix/modules and create a useful script
# in lkb/script (saving whatever was there to script.old).

# usage: perl modules.pl

# Initialize the arrays that will be used to store the names
# of files we're going to want to make sure are loaded in the
# script.

@typesfiles = ();
@lexfiles = ();
@irulesfiles = ();
@rulesfiles = ();

# Find out where files are stored.  I've coded in the path
# to my directory for ease of use here.  Change the line setting
# marked "CHANGE HERE" below to a local directory for convenience.
# _FIX_ME_: Figure out how to get this to work with ~ as an
# abbreviation for the home directory.

print "What is the pathname for your Matrix directory?\n\n";
$answer = <STDIN>;
chomp($answer);
$matrix_dir = $answer;

# Check whether they've really given us a matrix directory.
# Doing this by looking for the lkb/script file.

unless (-e $matrix_dir."/lkb/script") {
#    die "There's something wrong with the Matrix stored in $matrix_dir.\n";
    print "There's something wrong with the Matrix stored in $matrix_dir.\n";
# CHANGE HERE
    $matrix_dir = "/home/bender/lingo/grammars/matrix";
#    $matrix_dir = "/Users/erb/lingo/grammars/matrix";
    print "I'm trying $matrix_dir\n";
   
    unless (-e $matrix_dir."/lkb/script") {
	die "$matrix_dir doesn't work either.  Exiting...\n";
    } else {
	print "Okay, that seems to work.\n\n";
    }
}

print "This questionnaire will help you build a prototype grammar.\n\n";

print "Let's start with basic word order.  Is your language best described as:\n\n";
print "\t a) SOV\n";
print "\t b) SVO\n";
print "\t c) VSO\n";
print "\t d) OSV\n";
print "\t e) OVS\n";
print "\t f) VOS\n";
print "\t g) V-final\n";
print "\t h) V-initial\n";
print "\t i) free (pragmatically determined) word order\n";
print "\t j) other\n\n";

$answer = &getanswer("A","a","B","b","C","c","D","d","E","e","F","f","G","g","H","h","I","i","J","j");

# Depending on the answer, select types and rules files to have the
# script load.

switch ($answer) {
    
    case /^[Aa]/ { push(@typesfiles, "SOV.tdl");
		   push(@rulesfiles, "V-final-rules.tdl"); }
    case /^[Bb]/ { push(@typesfiles, "SVO.tdl");
		   push(@rulesfiles, "SVO-rules.tdl"); }
    case /^[Cc]/ { push(@typesfiles, "VSO.tdl");
		   push(@rulesfiles, "V-initial-rules.tdl"); }
    case /^[Dd]/ { push(@typesfiles, "OSV.tdl");
		   push(@rulesfiles, "V-final-rules.tdl"); }
    case /^[Ee]/ { push(@typesfiles, "OVS.tdl");
		   push(@rulesfiles, "OVS-rules.tdl"); }
    case /^[Ff]/ { push(@typesfiles, "VOS.tdl");
		   push(@rulesfiles, "V-initial-rules.tdl"); }
    case /^[Gg]/ { push(@typesfiles, "V-final.tdl");
		   push(@rulesfiles, "V-final-rules.tdl"); }
    case /^[Hh]/ { push(@typesfiles, "V-initial.tdl");
		   push(@rulesfiles, "V-initial-rules.tdl"); }
    case /^[Ii]/ { push(@typesfiles, "free-order.tdl");
		   push(@rulesfiles, "free-order-rules.tdl"); }
    case /^[Jj]/ { print "Sorry I can't help you with word order today.  Without some word\n order module you won't be able to parse anything.  Exiting now. Please start\n again.\n";
		   exit(0); }
}

print "Okay, now let's work on matrix  yes-no questions.\n\n";

# Prompt user for input to find out which mechanism is
# relevant for their language.

print "Does your language handle yes-no questions by means of:\n\n";
print "\t a) inverting the order of the subject and the main verb\n";
print "\t b) inverting the order of the subject and auxiliary verbs only\n";
print "\t c) a separate question particle\n";
print "\t d) none of the above\n\n";

$answer = &getanswer("A","a","B","b","C","c","D","d");

# Move to appropriate subroutine on the basis of the answer.
# D just prints the "sorry can't help message."
# A and B go straight to subroutines.
# C looks for subcases, and then calls subroutines.

switch ($answer) {

    case /^[Aa]/  { &generate_mainv_yesno; }
    case /^[Bb]/  { &generate_aux_yesno; }
    case /^[Cc]/  { &generate_particle_yesno; }
    case /^[Dd]/  {
        print "Sorry, I can't help you with yes-no questions today.\n";
    }
}


print "Okay, now let's work on sentential negation.\n\n";

# Prompt user for input to find out which kind of negation is
# relevant for their language.

print "Does your language handle sentential negation by means of:\n\n";
print "\t a) inflection on the main verb\n";
print "\t b) an adverb\n";
print "\t c) both\n";
print "\t d) neither\n\n";

$answer = &getanswer("A","a","B","b","C","c","D","d");

# Move to appropriate subroutine on the basis of the answer.
# D just prints the "sorry can't help message."
# A and B go straight to subroutines.
# C looks for subcases, and then calls subroutines.

switch ($answer) {

    case /^[Aa]/ { &generate_infl_neg; }
    case /^[Bb]/ { &generate_adv_neg; }
    case /^[Dd]/ {
	print "Sorry, I can't help you with negation today.\n";
    }
    case /^[Cc]/ {

# Here is where we need more input from the user, to find
# out which kind of "both" it is.

	print "You've said your language has both inflectional sentential\n";
	print "negation and adverbial sentential negation.  Which scenario\n";
	print "best describes your language?\n\n";

	print "\t a) inflectional negation and adverbial negation are in\n\t\t"."complementary distribution\n";
	print "\t b) the inflectional negation and the adverb can appear\n\t\t"."independently or together\n";
	print "\t c) sentential negation requires both the inflection and\n\t\t"."the adverb\n";
	print "\t d) none of the above\n\n";
      
	$answer = &getanswer("A","a","B","b","C","c","D","d");

# Call appropriate subroutine(s) on the basis of the answer.
# D just prints the "sorry can't help message."
# A and B call multiple subroutines, C just one.
	
	switch ($answer) {
       
	    case /^[Aa]/ { &generate_infl_neg;
			   &generate_adv_neg; }
	    case /^[Bb]/ { &generate_infl_neg;
			   &generate_infl_neg;
			   &generate_both_neg; }
	    case /^[Cc]/ { &generate_both_neg; }
	    case /^[Dd]/ { 
		print "Sorry, I can't help you with negation today.\n";
	    }
	}
    }
}

&generatescript;

#################################################################
# Subroutines

# This one creates the tdl files for the subject-main-verb-inversion
# case.

sub generate_mainv_yesno {

    print "Generating tdl files for subject-verb inversion.\n";
    print "Be sure to load subj-v-inv-lrule.tdl and svi-lrule.tdl in your script.\n\n";
}

# This one creates the tdl files for the subject-aux-inversion
# case.

sub generate_aux_yesno {

    print "Generating tdl files for subject-aux inversion.\n";
    print "Be sure to load subj-v-inv-lrule.tdl, subj-aux-inv-lrule.tdl and sai-lrule.tdl in your script.\n\n";
}


# This one will generate the yes-no particle version.

sub generate_particle_yesno {

# Find out the spelling of the particle.  We'll assume for now that
# it has constant form.

    print "How is the question particle spelled?\n";
    print "Suggested form (we'll use this as a default): QPART \n";

    $spelling = <STDIN>;
    chomp($spelling);

    if ($spelling =~ /^$/) {
	$spelling = "QPART";
    }

    &check_clobber("qpart-lex.tdl");

# Check for write permissions.  If we can't open the file for
# output in the specified directory, exit. 

    open(OUTPUT,">$matrix_dir"."/modules/qpart-lex.tdl") || 
	die "Cannot create matrix/modules/qpart-lex.tdl.\n";

# Output tdl header.

    print OUTPUT "\;\;\; -*- Mode: TDL; Package: LKB -*-\n\n";
    print OUTPUT "\;\;\; Autogenerated sentence particle question module\n";

# Output lexical entry definition. $spelling stores the
# spelling of the particle.

    print OUTPUT "qpart-1 := qpart-le &\n";
    print OUTPUT "[ STEM < \"$spelling\" > ].\n\n";

# Hint about what to do to the script file.  In the long run, we
# should be modifying the script files, too.

#    print "Be sure to load qpart-type.tdl and qpart-lex.tdl in your script.\n\n";


# Add those file to the arrays which store the ones we're going
# to mention in the autogenerated script:

	    push(@typesfiles, "qpart-type.tdl");
	    push(@lexfiles, "qpart-lex.tdl");

}


# This one creates the tdl files for the negation as inflection
# case.

sub generate_infl_neg {

    print "Alright, let's work on inflectional negation.\n";

# Find out whether it's a prefix or a suffix.

    print "Is the negation affix a prefix or a suffix?\n";

    $answer = &getanswer("P","p","S","s"); 

# Set the value of $affix based on the answer.  We need
# to do this to get a normalized spelling, since we're going
# to use the value of $affix as part of the generated tdl code.

    if ($answer =~ /^[Pp]/) {

	$affix = "prefix";

    } else {

	$affix = "suffix";

    }

# Now find out the spelling of the negation affix.  Assuming
# that there is only concatenation involved here.  This would be
# the case if the morphophonology is handled by a morphological
# analyzer which produces "underlying forms" to be interpreted by
# the morphosyntax.  Cf Bender & Good 2005.

    print "How is the negation $affix spelled in the underlying form?\n";
    print "Suggested form (we'll use this as a default): NEG\n";

    $spelling = <STDIN>;
    chomp($spelling);

    if ($spelling =~ /^$/) {

	$spelling = "NEG";

    }


# Check whether the file already exists, and if so whether the user
# wants to overwrite.

    &check_clobber("infl-neg-irule.tdl");

# Check for write permissions.  If we can't open the file for
# output in the specified directory, exit. 

    open(OUTPUT,">$matrix_dir"."/modules/infl-neg-irule.tdl") || 
	die "Cannot create matrix/modules/infl-neg-irule.tdl.\n";

# Output tdl header.

    print OUTPUT "\;\;\; -*- Mode: TDL; Package: LKB -*-\n\n";
    print OUTPUT "\;\;\; Autogenerated inflection negation module\n";
    print OUTPUT "\;\;\; Use in conjunction with infl-neg.tdl\n\n";

# Output lexical rule definition.  $affix stores either "prefix" or
# "suffix", depending on the user's answer.  $spelling stores the
# spelling of the affix.

    print OUTPUT "neg-lr :=\n";
    print OUTPUT "%"."$affix (* $spelling)\n";
    print OUTPUT "negation-lex-rule.\n\n";

# Hint about what to do to the script file.  In the long run, we
# should be modifying the script files, too.

#    print "Be sure to load infl-neg.tdl and infl-neg-irule.tdl in your script.\n\n";

# Add those file to the arrays which store the ones we're going
# to mention in the autogenerated script:

    push(@typesfiles, "infl-neg.tdl");
    push(@irulesfiles, "infl-neg-irule.tdl");

}

# This one will generate the negation adverb version.

sub generate_adv_neg {

#Assume that we're going to instantiate scopal and intersective
#head-modifier rules for both orders for every grammar.  We might
#one day find a language that only uses a subset, but that's a
#topic for another time.

    print "Generating tdl files for adverbial sentential negation.\n";

    print "You've said your language handles sentential negation\n";
    print "via a negative adverb.\n";

    print "How is the negative adverb spelled? (default: negadv)\n\n";

    $spelling = <STDIN>;
    chomp($spelling);
    if ($spelling =~ /^$/) {
	$spelling = "negadv";
    }

    print "Is this element best described as:\n\n";
    print "\t a) an independent modifier\n";
    print "\t b) selected by a verb (auxiliary verb or main verb)\n\n";

    $answer = &getanswer("a","b","A","B");

    switch ($answer) {

	case /^[Aa]/ { 

#For independent modifiers, determiner order of attachment (pre or posthead)
#and bar-level of the thing it attaches to.  Assume for now that order
#of attachment won't be free (this isn't hard to handle if we need it).
#Need a better solution for forcing V-level attachment.

	    print "Does it attach to the left or to the right of the head it modifies?\n";

	    $order = &getanswer("L","l","R","r");

	    if ($order =~ /^[Ll]/) {
		$posthead = "-";
	    } else {
		$posthead = "+";
	    }

	    print "Does it attach to:\n\n";
	    print "\t a) V\n";
	    print "\t b) VP\n";
	    print "\t c) S\n";
            print "\t d) other\n\n";
	 
	    $barlevel = &getanswer("A","a","B","b","C","c","D","d");

	    if ($barlevel =~ /^[Dd]/) {
		print "Sorry, I can't help you with negation today.\n";
		exit(0);
	    }

# That's all we need to know to write some tdl.  Start with type file.
# Check for write permissions.  If we can't open the file for
# output in the specified directory, exit. 

	    &check_clobber("adv-neg.tdl");
	    open(OUTPUT,">$matrix_dir"."/modules/adv-neg.tdl") || 
		die "Cannot create matrix/modules/adv-neg.tdl.\n";
	    
	    print OUTPUT "\;\;\; -*- Mode: TDL; Package: LKB -*-\n\n";
	    print OUTPUT "\;\;\; Autogenerated adverbial negation module\n";
	    print OUTPUT "\;\;\; Use in conjunction with adv-neg-lex.tdl\n\n";

	    print OUTPUT "neg-adv-lex := basic-scopal-adverb-lex &\n";
	    print OUTPUT "   [ SYNSEM.LOCAL.CAT [ POSTHEAD $posthead,\n";
	    print OUTPUT "                        VAL [ SPR < >,\n";
	    print OUTPUT "                              COMPS < >,\n";
            print OUTPUT "                              SUBJ < > ],\n";
	    print OUTPUT "                        HEAD.MOD < [ LOCAL.CAT [ HEAD verb,\n";

	    switch ($barlevel) {

#Actually, this won't work.  We need some sort of LEX feature to force
#attachment to V.

		case /^[Aa]/ {
		    print OUTPUT "                                                 VAL [ SUBJ cons,\n";
		    print OUTPUT "                                                       COMPS cons ]]] > ]].\n\n";
		}

		case /^[Bb]/ {
		    print OUTPUT "                                                 VAL [ SUBJ cons,\n";
		    print OUTPUT "                                                       COMPS null ]]] > ]].\n\n";
		}
		 
		case /^[Cc]/ {
		    print OUTPUT "                                                 VAL [ SUBJ null,\n";
                    print OUTPUT "                                                       COMPS null ]]] > ]].\n\n";
		}
	    }

# Now generate lexical item file.

	    &check_clobber("adv-neg-lex.tdl");
	    open(OUTPUT,">$matrix_dir"."/modules/adv-neg-lex.tdl") || 
		die "Cannot create matrix/modules/adv-neg-lex.tdl.\n";
	    
	    print OUTPUT "\;\;\; -*- Mode: TDL; Package: LKB -*-\n\n";
	    print OUTPUT "\;\;\; Autogenerated adverbial negation module\n";
	    print OUTPUT "\;\;\; Use in conjunction with adv-neg.tdl\n\n";

	    print OUTPUT "neg-adv := neg-adv-lex &\n";
            print OUTPUT "  [ SYNSEM.LKEYS.KEYREL.PRED \"_neg_r_rel\",\n";
            print OUTPUT "    STEM < \"$spelling\" > ].\n\n"; 


#	    print "Be sure to load adv-neg.tdl and adv-neg-lex.tdl in your script.\n\n";

# Add those file to the arrays which store the ones we're going
# to mention in the autogenerated script:

	    push(@typesfiles, "adv-neg.tdl");
	    push(@lexfiles, "adv-neg-lex.tdl");

	}

	case /^[Bb]/ {

	    print "Selected adverb case.  Needs lexical rules.\n";

	}
	    
    }
}

# This will generate the tdl for two part negation.

sub generate_both_neg {

    print "Generating tdl files for two-part negation (inflection and adverb in the same sentence.\n";
    print "Be sure to load two-part-neg.tdl, two-part-neg-lex.tdl, two-part-neg-irule.tdl and two-part-neg-rules.tdl in your script.\n\n";

}

# This subroutine checks whether the string passed as its
# argument corresponds to a file in the matrix/modules directory.
# If it does, it will alert the user, and prompt to find out whether
# to overwrite the file or bail.

sub check_clobber {

# $file is a local variable
# stores the full pathname to the file we're worried about,
# based on the matrix directory decided on at the beginning of
# the script and the file name passed in when the subroutine
# was called.  These will be hard coded into this script, not
# supplied by the user.

    my($file) = $matrix_dir."/modules/".$_[0];
    my($answer);

#    print "Calling check clobber on $file\n";

    if (-e $file) {

	print "$_[0] already exists. Continue anyway? [yn]";
	$answer = &getanswer("Y","y","N","n");
	if ($answer =~ /^[Yy]$/) {
	    
	    print "Okay, continuing and overwriting output file...\n";
	    
	} else {
	    
	    print "Okay, quitting...\n";
	    exit (0);

	}
    }
}

# Subroutine for answers to multiple choice questionnaire items.
# Takes as arguments the set of valid choices, and then checks
# whether user input matches.  As long as user input doesn't match,
# keep prompting user.

sub getanswer {

# Read the answer from standard in.

    $answer = <STDIN>;
    chomp($answer);

# Make sure the answer was something appropriate.  If not,
# prompt for a appropriate answer.

    while (&notinarray($answer,@_)) {
	print "Please type";

	$last = scalar(@_) - 1;

	for ($i=0;$i<$last;$i++) {

	    print " $_[$i]";
	    
	}

	print " or $_[$last].\n";

	$answer = <STDIN>;
	chomp($answer);
    }

    return $answer;

}

# Subroutine used by getanswer to test whether or not an element
# (first argument) is in an array (remaining arguments).  Surely
# there's a standard form for this in Perl, but oh well.

sub notinarray {

    for($i=1;$i<scalar(@_);$i++) {
	
	if ($_[0] =~ /^$_[$i]/) {
	    return 0;
	}

    }

    return 1;

}

# Subroutine which takes the basic script file we'll start with
# and the modifications stored in @typesfiles, @lexfiles, @irulesfiles
# and @rulesfiles and makes the appropriate script file.

sub generatescript {

# I'm assuming that lkb/script exists, since that's how we're verifying
# the matrix directory at the beginning.  This will need to be fixed
# if we go with a different check eventually.

    print "I'm moving your existing script file from lkb/script to lkb/script.old.\n";

# This next bit assumes that we're running unix/linux.  Actually
# some of the -e stuff used before might assume that too.  We'll
# need to generalize at some point, but as soon as we're dealing with
# actual files, I think we need to know at least a little bit about
# the OS.

    `mv $matrix_dir/lkb/script $matrix_dir/lkb/script.old`;

    open (INPUT,"$matrix_dir"."/modules/basic_script");
    open (OUTPUT,">$matrix_dir"."/lkb/script") || 
	die "Could not open $matrix_dir/lkb/script.\n";

    while (<INPUT>) {
	print OUTPUT;
	last if /^\;\;\; Modules:/;
	    
    }

#First time we see ";;; Modules: in the basic_script file, 
#it's time to load the types files.  _FIX_ME_: Find some more
#robust, maintainable way to do this.


    while (@typesfiles) {
	$file = pop(@typesfiles);
	print OUTPUT "(lkb-pathname (parent-directory) \"modules/$file\")\n";
    }

#Now keep going until we see the next ;;; Modules comment

    while (<INPUT>) {
	print OUTPUT;
	last if /^\;\;\; Modules:/;
	    
    }

#Time for the lex files

    while (@lexfiles) {
	$file = pop(@lexfiles);
	print OUTPUT "(lkb-pathname (parent-directory) \"modules/$file\")\n";
    }

#Keep going until we see the next ;;; Modules comment

    while (<INPUT>) {
	print OUTPUT;
	last if /^\;\;\; Modules:/;
	    
    }

#Time for the lexical rules files

    while (@irulesfiles) {
	$file = pop(@irulesfiles);
	print OUTPUT "(read-morph-file-aux (lkb-pathname (parent-directory) \"modules/$file\"))\n";
    }

#And again

    while (<INPUT>) {
	print OUTPUT;
	last if /^\;\;\; Modules:/;
	    
    }

#Finally the grammar rules files.

    while (@rulesfiles) {
	$file = pop(@rulesfiles);
	print OUTPUT "(read-tdl-grammar-file-aux (lkb-pathname \n";
	print OUTPUT "                           (parent-directory)\n";
	print OUTPUT "                           \"modules/$file\"))\n";
    }

#And then the rest of the script file.

    while (<INPUT>) {
	print OUTPUT;
    }

}


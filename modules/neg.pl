#!/usr/bin/perl

# Perl script for customizing module handling sentential
# negation.  We'll eventually probably want just one big
# perl/cgi thing which handles all of the modules.  Negation
# seemed like a good place to start, since there are too
# many choices to just build separate .tdl files in this case.

# usage: perl neg.pl

# Find out where files are stored.  I've coded in the path
# to my directory for ease of use here.  Change the line setting
# marked "CHANGE HERE" below to a local directory for convenience.
# _FIX_ME_: Figure out how to get this to work with ~ as an
# abbreviation for the home directory.

print "What is the pathname for your Matrix directory?\n\n";
$answer = <STDIN>;
chomp($answer);
$matrix_dir = $answer;

# Check whether they're really given us a matrix directory.
# Doing this by looking for the lkb/script file.

unless (-e $matrix_dir."/lkb/script") {
#    die "There's something wrong with the Matrix stored in $matrix_dir.\n";
    print "There's something wrong with the Matrix stored in $matrix_dir.\n";
    print "I'm trying /home/bender/lingo/grammars/matrix\n\n";
# CHANGE HERE
    $matrix_dir = "/home/bender/lingo/grammars/matrix";
}

print "This questionnaire will help you build a prototype\n analysis of negation for your grammar.\n\n";

# Prompt user for input to find out which kind of negation is
# relevant for their language.

print "Does your language handle sentential negation by means of:\n\n";
print "\t a) inflection on the main verb\n";
print "\t b) an adverb\n";
print "\t c) both\n";
print "\t d) neither\n\n";

# Read the answer from standard in.

$answer = <STDIN>;
chomp($answer);

# Make sure the answer was something appropriate.  If not,
# prompt for a appropriate answer.

while ($answer =~ /^[^ABCDabcd]/) {
    print "Please type A B C or D.\n";
    $answer = <STDIN>;
    chomp($answer);
}

# Move to appropriate subroutine on the basis of the answer.
# D just prints the "sorry can't help message."
# A and B go straight to subroutines.
# C looks for subcases, and then calls subroutines.

if ($answer =~ /^[Aa]/) {

    &generate_infl_neg;

} elsif ($answer =~ /^[Bb]/) {
    
    &generate_adv_neg;

} elsif ($answer =~ /^[Dd]/) {
    
    print "Sorry, I can't help you with negation today.\n";

} elsif ($answer =~ /^[Cc]/) {

# Here is where we need more input from the user, to find
# out which kind of "both" it is.

    print "You've said your language has both inflectional sentential\n";
    print "negation and adverbial sentential negation.  Which scenario\n";
    print "best describes your language?\n\n";

    print "\t a) inflectional negation and adverbial negation are in\n\t\t"."complementary distribution\n";
    print "\t b) the inflectional negation and the adverb can appear\n\t\t"."independently or together\n";
    print "\t c) sentential negation requires both the inflection and\n\t\t"."the adverb\n";
    print "\t d) none of the above\n\n";
      
# Read in the answer.

    $answer = <STDIN>;
    chomp($answer);

# Check that it's appropriate.  If not, prompt again.

    while ($answer =~ /^[^ABCDabcd]/) {
	print "Please type A B C or D.\n";
	$answer = <STDIN>;
	chomp($answer);
    }

# Call appropriate subroutine(s) on the basis of the answer.
# D just prints the "sorry can't help message."
# A and B call multiple subroutines, C just one.

    if ($answer =~ /^[Aa]/) {

	&generate_infl_neg;
	&generate_adv_neg; 

    } elsif ($answer =~ /^[Bb]/) {

	&generate_infl_neg;
	&generate_adv_neg; 
	&generate_both_neg;

    } elsif ($answer =~ /^[Cc]/) {

	&generate_both_neg;

    } elsif ($answer =~ /^[Dd]/) {

	print "Sorry, I can't help you with negation today.\n";
	
    } 
}

#################################################################
# Subroutines

# This one creates the tdl files for the negation as inflection
# case.

sub generate_infl_neg {

    print "Alright, let's work on inflectional negation.\n";

# Find out whether it's a prefix or a suffix.

    print "Is the negation affix a prefix or a suffix?\n";
    $answer = <STDIN>;
    chomp($answer);

# Check for an appropriate answer.

    while ($answer =~ /^[^PpSs]/) {
	print "Please type P or S.\n";
	$answer = <STDIN>;
	chomp($answer);
    }
   
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
    print "Suggested form: NEG\n";

    $spelling = <STDIN>;
    chomp($spelling);

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

    print "Be sure to load infl-neg.tdl and infl-neg-irule.tdl in your script.\n\n";

}

# This one will generate the negation adverb version.

sub generate_adv_neg {

    #Here we're going to have worry about whether or not the
    #scopal head-adj rules are already instantiated.  Hmmm...

    print "Generating tdl files for adverbial sentential negation.\n";
    print "Be sure to load adv-neg.tdl, adv-neg-lex.tdl, and adv-neg-rules.tdl in your script.\n\n";

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

#    print "Calling check clobber on $file\n";

    if (-e $file) {

	print "$_[0] already exists. Continue anyway? [yn]";
	$answer = <STDIN>;
	chomp($answer);
	if ($answer =~ /^[Yy]$/) {
	    
	    print "Okay, continuing and overwriting output file...\n";
	    
	} else {
	    
	    print "Okay, quitting...\n";
	    exit (0);

	}
    }
}

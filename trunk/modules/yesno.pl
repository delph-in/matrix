#!/usr/bin/perl

# Perl script for customizing module handling yes-no questions
# (Cannibalized from neg.pl)

# usage: perl yesno.pl

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

print "This questionnaire will help you build a prototype\n analysis of yes-no questions for your grammar.\n\n";

# Prompt user for input to find out which mechanism is
# relevant for their language.

print "Does your language handle yes-no questions by means of:\n\n";
print "\t a) inverting the order of the subject and the main verb\n";
print "\t b) inverting the order of the subject and auxiliary verbs only\n";
print "\t c) a separate question particle\n";
print "\t d) none of the above\n\n";

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

    &generate_mainv_yesno;

} elsif ($answer =~ /^[Bb]/) {
    
    &generate_aux_yesno;

} elsif ($answer =~ /^[Cc]/) {
    
    &generate_particle_yesno;

} elsif ($answer =~ /^[Dd]/) {
    
    print "Sorry, I can't help you with yes-no questions today.\n";

}

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
    print "Suggested form: QPART\n";

    $spelling = <STDIN>;
    chomp($spelling);

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

    print "Be sure to load qpart-type.tdl and qpart-lex.tdl in your script.\n\n";

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

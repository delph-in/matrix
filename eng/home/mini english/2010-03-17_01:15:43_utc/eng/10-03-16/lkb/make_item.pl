#!/usr/bin/perl

# Script to take test suite file formatted per Ling 567 Lab 2
# directions and create an items file for [incr tsdb()] use.
#
# Assume for now that each source cited in the headers is on a single line.
#
# Emily M. Bender 4/6/06
# Modified Shauna Eggers 4/17/06:
# - unicode handling, normalizing phenomena to short forms, page numbers
# - some subroutines and reformatting
# Modified Emily M. Bender 1/30/09:
# - Allow user-specified i-ids.

# ------------ Main ----------------------------

$#ARGV < 0 and die "Usage:  $0 <source_file>\n";

$three_lines = 0;
$opt = "";
%sources = {};
%phenomena_codes = {};


# Set up input and output
if (-e $ARGV[0].".item") {

    print "$ARGV[0].item already exists. Continue anyway? [yn] ";
    if (<STDIN> =~ /^[Yy]/) {
	print "Okay, continuing and overwriting output file...\n";
    } else {
	print "Okay, quitting...\n";
	exit (0);
    }
}

open(INPUT, "<:utf8", $ARGV[0]) || die "Cannot open $ARGV[0]";
open(OUTPUT, ">:utf8", "$ARGV[0].item") || die "Cannot create $ARGV[0].item.\n";

# For debugging

#open(LOG, ">:utf8", "make_item.log") || die "Cannot create log.\n";

# Get user input options and init some tables
initialize();


# Read testsuite header
$line = &parse_header();


# Read examples and print to output
$i_id = 1;
@i_ids = ();
parse_example($line);
while ($line = &next_line()) {
    parse_example($line);
}

# Done!
close INPUT;
close OUTPUT;


# ------------ Subroutines ---------------------

# Read in header information for test_suite file
sub parse_header {

    while($_ = &next_line() and ($_ !~ /^[Ss]ource:/ and $_ !~ /^[Ii]d:/)) {
	
	if (/[Ll]anguage:\s*(.*\S*)\s*/) {
	    print "Language is $1.\n";
	}
	if (/[Aa]uthor:\s*(.*\S*)\s*/) {
	    $author = $1;
	    print "Author is $1.\n";
	}
	if (/[Dd]ate:\s*(.*\S*)\s*/) {
	    $date = $1;
	    print "Date is $1.\n";
	}
	
	if (/^[Ss]ource ([a-z]):\s*(.*\S*)\s*$/) {
	    $sources{$1} = $2;
	    print "Source $1 is $2.\n";
	}

    }
    

    return $_;  # Will have consumed first line of non-header; give it back!
}


# Read in the next example with its header, and print in one-line itsdb format
sub parse_example {

    my $line = $_[0];

    ### i-id
    # Allow for optional user-specified i-id:
    if ($line =~ /^[Ii]d:\s*.*/) {

	($foo,$i_id) = split(/\:\s?/,$line);
	if (&id_used($i_id)) {
	    die "item id $i_id is not unique.\n";
	}
	$line = &next_line();
    }

    push(@i_ids,$i_id);

    ### Source
    # first line given to this subroutine should be the source line.
    ($line =~ /^[Ss]ource:\s*(.*\S*)\s*/) or die "Expected line to start with \"Source\".  Actual line: $line\n";
    ($source_code, $page) = split(/\:/,$1);

    # use actual source instead of source code, if alternative exists
    $source = $sources{$source_code};
    $i_origin = ($source ne undef)? $source : $source_code;

    # append page back on source; change from : to @?
    if ($page ne undef and $page ne "") {
	$i_origin .= ":".$page;
    }


    ### Vetted
    # next line says whether or not example vetted.  Not needed for output, just check formatting
    $line = &next_line();
    ($line =~ /^[Vv]etted/) or die "Expected line to start with \"Vetted\".  Actual line: $line\n";


    ### Judgment
    $line = &next_line();
    ($line =~ /^[Jj]udgment:\s*(.*\S*)\s*/) or die "Expected line to start with \"Judgment\".  Actual line: $line\n";

    # input should be U or G, but output 0 or 1, respectively
    $judgment = $1;
    ($judgment =~ /^[UuGg]$/) or die "Bad value for Judgment, should be U or u or G or g: $judgment\n";
    $i_wf = ($judgment =~ /[Uu]/)? 0 : 1;


    ### Phenomena
    $line = &next_line();
    ($line =~ /^[Pp]henom.*:\s*(.*\S*)\s*/) or die "Expected line to start with \"Phenom\".  Actual line: $line\n";
    $i_category = $1;

    # normalize phenomena to their short form (in phenomena_codes table)
    $i_category =~ s/\s+//g;
    @phenomena = split /\,/, $i_category;
    for $i (0..@phenomena) {
	$code = $phenomena_codes{lc($phenomena[$i])};
	if ($code ne undef) {
	    $phenomena[$i] = $code;
	}
    }
    $i_category = join(",",@phenomena);

    
    ### Input
    # figure out which line from the forms to use for input, and consume the others
    $one = &next_line();
    $two = &next_line();
    $three_lines and $three = &next_line();

    if ($opt eq 'A' and $three_lines) {
	$i_input = $three;
    } elsif ($opt eq 'B') {
	$i_input = $one;
    } else {
	$i_input = $two;
    }


    ### Line length:  number of words, based on whitespace
    $i_length = split(/\s+/,$i_input);


    ### Morpheme-by-morpheme gloss
    next_line();  # not needed in output, just consume it


    ### Comments
    # this would be the English translation, e.g.
    $i_comment = &next_line();


    print OUTPUT "$i_id\@$i_origin\@\@\@1\@$i_category\@$i_input\@$i_wf\@$i_length\@$i_comment\@$author\@$date\n";

    $i_id++;
}


# Initialize tables and user input options
sub initialize {

    # User input:  find out how the examples are displayed,
    # and which line the user would like in their test suite.

    # opt A means use the morpheme segmented line (third in case of three;
    #         second in case of two)
    # opt B means use the first line (standard orthography in case of
    #         all three, whichever is included in case just two are)
    # opt C means use the second line (transliteration in case of all three)

    print "Did you include both a standard orthography and a transliteration line? [yn] ";

    $ans = <STDIN>;
    chomp($ans);
    
    while ($ans =~ /^[^YyNn]/) {
	
	print "Please type \'Y\' or \'N\'\n";
	$ans = <STDIN>;
	chomp($ans);
	
    }
    
    
    if ($ans =~ /^[Yy]/) {
	
	$three_lines = "true";
	
	print "Should your test suite items be in:\n\n";
	print "\t a) morpheme segmented format\n";
	print "\t b) standard orthography\n";
	print "\t c) transliteration\n";
	
	
	$ans2 = <STDIN>;
	chomp($ans2);
	
	while ($ans2 =~ /^[^ABCabc]/) {
	    
	    print "Please type \'A\', '\B\' or \'C\'\n";
	    $ans2 = <STDIN>;
	    chomp($ans2);
	}
	
    } else {
	
	print "Should your test suite items be in:\n\n";
	print "\t a) morpheme segmented format\n";
	print "\t b) standard orthography\/transliteration\n";
	
	$ans2 = <STDIN>;
	chomp($ans2);
	
	while ($ans2 =~ /^[^ABCabc]/) {
	    
	    print "Please type \'A\' or \'B\'\n";
	    $ans2 = <STDIN>;
	    chomp($ans2);
	}    
	
    }

    $opt = uc($ans2); # keep track of option for input line

    # Phenomena:  map possible forms for phenomena (lc, no whitespace) to short forms
    %phenomena_codes = (
			'wordorder' => 'wo',
			'pronouns' => 'pn',
			'determiners' => 'det',
			'case-markingadpositions' => 'adp',
			'casemarkingadpositions' => 'adp',
			'argumentoptionality' => 'pro-d',
			'prod' => 'pro-d',
			'agreement' => 'agr',
			'case' => 'c',
			'coordination' => 'crd',
			'matrixyes-noquestions' => 'q',
			'matrixyes/noquestions' => 'q',
			'matrixyesnoquestions' => 'q',
			'yes-noquestions' => 'q',
			'yesnoquestions' => 'q',
			'yes-no' => 'q',
			'yesno' => 'q',
			'modals' => 'm',
			'negation' => 'neg',
			'imperatives' => 'imp',
			'embeddeddeclaratives' => 'emb-d',
			'embeddedquestions' => 'emb-q',
			'adjectives' => 'adj',
			'relativeclauses' => 'rel',
			'serial verb constructions' => 'svc',
			'cognitive status' => 'cogst',
			'non-verbal predicates' => 'cop',
			'numeral classifiers' => 'numcl'
			);
}


# Get next line from INPUT file, ignoring comments and empty lines, and chomping off newlines
sub next_line {

    my $line = <INPUT>;
    while ($line ne undef and
	   ($line =~ /^#/ or $line =~ /^\s*$/)) {
	$line = <INPUT>;
    }
    chomp $line;
    $line =~ s/\r$//; # remove windows carriage return, if exists

    return $line;
}

# Check whether $i_id is on the array @i_ids.  Surely there's already
# a perl function for this, but I couldn't find it.

sub id_used { 
  my $id = $_[0];
  $j = 0;
  while ($i = $i_ids[$j]) {
    if ($i == $id) {
      return 1;
    }
    $j++;
  }
  return 0;
}


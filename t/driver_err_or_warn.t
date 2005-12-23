use strict;
#use Gift;

our @errfiles;
our $numtests;
our $debugging = 0;

BEGIN { 
  chdir "t/" if -e "t/"; # go to the test directory
  @errfiles = glob("*_err.gift *_warn.gift"); 
  $numtests = @errfiles + 1;
} 

use Test::More tests => $numtests;

BEGIN { use_ok('Gift') };

our $warnings = "";

sub warning_handler {
  $warnings .= $_[0] if $_[0] !~ m{34_not_defined_pre_err};
}

#########################

for my $file (@errfiles) {

  $file =~ m{((\d+)_(\w+)(_err|_warn))\.gift};
  my ($filename, $order, $name) = ($1, $2, $3);
  my $answerfile = "$filename.ok";
  local $/ = undef;

  open FILE, $answerfile;
  my $ok = <FILE>;

  eval {
    $warnings = ""; # reset warnings
    $SIG{__WARN__} = \&warning_handler;
    Gift->GiftFromFile($file);
    delete($SIG{__WARN__});
  } ;

  $ok = substr($ok,0,length$warnings); # warnings is a prefix of $ok

  is($warnings, $ok, $name);

if ($debugging) {
  print <<"EOI";
FILE: $filename
WARNINGS:
$warnings;
OK:
$ok
******************************************
EOI
} # debugging

} # for my $file


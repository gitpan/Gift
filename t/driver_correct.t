#use strict;
#use Gift;

our @corfiles;
our $numtests;
our $debugging = 0;

BEGIN { 
  chdir "t/" if -e "t/"; # go to the test directory
  @corfiles = glob("*_cor.gift"); 
  $numtests = @corfiles + 1;
} 

use Test::More tests => $numtests;

BEGIN { use_ok('Gift') };

my ($result, $VAR1);

for my $file (@corfiles) {

  $file =~ m{((\d+)_(\w+)(_cor))\.gift};
  my ($filename, $order, $name) = ($1, $2, $3);
  my $answerfile = "$filename.ok";
  my $corfile = "$filename.cor";

  local $/ = undef;

  open FILE, $answerfile;
  my $ok = <FILE>;
  
  eval $ok;

  eval {
    $result = Gift->GiftFromFile($file);
  };

  is_deeply($VAR1, $result, $name);


} # for my $file


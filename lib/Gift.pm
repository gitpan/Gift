####################################################################
#
#    This file was generated using Parse::Yapp version 1.05.
#
#        Don't edit this file, use source file instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
####################################################################
package Gift;
use vars qw ( @ISA );
use strict;

@ISA= qw ( Parse::Yapp::Driver );
#Included Parse/Yapp/Driver.pm file----------------------------------------
{
#
# Module Parse::Yapp::Driver
#
# This module is part of the Parse::Yapp package available on your
# nearest CPAN
#
# Any use of this module in a standalone parser make the included
# text under the same copyright as the Parse::Yapp module itself.
#
# This notice should remain unchanged.
#
# (c) Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (see the pod text in Parse::Yapp module for use and distribution rights)
#

package Parse::Yapp::Driver;

require 5.004;

use strict;

use vars qw ( $VERSION $COMPATIBLE $FILENAME );

$VERSION = '1.05';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
			 YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '');
#Mandatory parameters
my(@params)=('LEX','RULES','STATES');

sub new {
    my($class)=shift;
	my($errst,$nberr,$token,$value,$check,$dotpos);
    my($self)={ ERROR => \&_Error,
				ERRST => \$errst,
                NBERR => \$nberr,
				TOKEN => \$token,
				VALUE => \$value,
				DOTPOS => \$dotpos,
				STACK => [],
				DEBUG => 0,
				CHECK => \$check };

	_CheckParams( [], \%params, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Yapp driver version $VERSION ".
			  "incompatible with version $$self{VERSION}:\n".
			  "Please recompile parser module.";

        ref($class)
    and $class=ref($class);

    bless($self,$class);
}

sub YYParse {
    my($self)=shift;
    my($retval);

	_CheckParams( \@params, \%params, \@_, $self );

	if($$self{DEBUG}) {
		_DBLoad();
		$retval = eval '$self->_DBParse()';#Do not create stab entry on compile
        $@ and die $@;
	}
	else {
		$retval = $self->_Parse();
	}
    $retval
}

sub YYData {
	my($self)=shift;

		exists($$self{USER})
	or	$$self{USER}={};

	$$self{USER};
	
}

sub YYErrok {
	my($self)=shift;

	${$$self{ERRST}}=0;
    undef;
}

sub YYNberr {
	my($self)=shift;

	${$$self{NBERR}};
}

sub YYRecovering {
	my($self)=shift;

	${$$self{ERRST}} != 0;
}

sub YYAbort {
	my($self)=shift;

	${$$self{CHECK}}='ABORT';
    undef;
}

sub YYAccept {
	my($self)=shift;

	${$$self{CHECK}}='ACCEPT';
    undef;
}

sub YYError {
	my($self)=shift;

	${$$self{CHECK}}='ERROR';
    undef;
}

sub YYSemval {
	my($self)=shift;
	my($index)= $_[0] - ${$$self{DOTPOS}} - 1;

		$index < 0
	and	-$index <= @{$$self{STACK}}
	and	return $$self{STACK}[$index][1];

	undef;	#Invalid index
}

sub YYCurtok {
	my($self)=shift;

        @_
    and ${$$self{TOKEN}}=$_[0];
    ${$$self{TOKEN}};
}

sub YYCurval {
	my($self)=shift;

        @_
    and ${$$self{VALUE}}=$_[0];
    ${$$self{VALUE}};
}

sub YYExpect {
    my($self)=shift;

    keys %{$self->{STATES}[$self->{STACK}[-1][0]]{ACTIONS}}
}

sub YYLexer {
    my($self)=shift;

	$$self{LEX};
}


#################
# Private stuff #
#################


sub _CheckParams {
	my($mandatory,$checklist,$inarray,$outhash)=@_;
	my($prm,$value);
	my($prmlst)={};

	while(($prm,$value)=splice(@$inarray,0,2)) {
        $prm=uc($prm);
			exists($$checklist{$prm})
		or	croak("Unknow parameter '$prm'");
			ref($value) eq $$checklist{$prm}
		or	croak("Invalid value for parameter '$prm'");
        $prm=unpack('@2A*',$prm);
		$$outhash{$prm}=$value;
	}
	for (@$mandatory) {
			exists($$outhash{$_})
		or	croak("Missing mandatory parameter '".lc($_)."'");
	}
}

sub _Error {
	print "Parse error.\n";
}

sub _DBLoad {
	{
		no strict 'refs';

			exists(${__PACKAGE__.'::'}{_DBParse})#Already loaded ?
		and	return;
	}
	my($fname)=__FILE__;
	my(@drv);
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
	while(<DRV>) {
                	/^\s*sub\s+_Parse\s*{\s*$/ .. /^\s*}\s*#\s*_Parse\s*$/
        	and     do {
                	s/^#DBG>//;
                	push(@drv,$_);
        	}
	}
	close(DRV);

	$drv[0]=~s/_P/_DBP/;
	eval join('',@drv);
}

#Note that for loading debugging version of the driver,
#this file will be parsed from 'sub _Parse' up to '}#_Parse' inclusive.
#So, DO NOT remove comment at end of sub !!!
sub _Parse {
    my($self)=shift;

	my($rules,$states,$lex,$error)
     = @$self{ 'RULES', 'STATES', 'LEX', 'ERROR' };
	my($errstatus,$nberror,$token,$value,$stack,$check,$dotpos)
     = @$self{ 'ERRST', 'NBERR', 'TOKEN', 'VALUE', 'STACK', 'CHECK', 'DOTPOS' };

#DBG>	my($debug)=$$self{DEBUG};
#DBG>	my($dbgerror)=0;

#DBG>	my($ShowCurToken) = sub {
#DBG>		my($tok)='>';
#DBG>		for (split('',$$token)) {
#DBG>			$tok.=		(ord($_) < 32 or ord($_) > 126)
#DBG>					?	sprintf('<%02X>',ord($_))
#DBG>					:	$_;
#DBG>		}
#DBG>		$tok.='<';
#DBG>	};

	$$errstatus=0;
	$$nberror=0;
	($$token,$$value)=(undef,undef);
	@$stack=( [ 0, undef ] );
	$$check='';

    while(1) {
        my($actions,$act,$stateno);

        $stateno=$$stack[-1][0];
        $actions=$$states[$stateno];

#DBG>	print STDERR ('-' x 40),"\n";
#DBG>		$debug & 0x2
#DBG>	and	print STDERR "In state $stateno:\n";
#DBG>		$debug & 0x08
#DBG>	and	print STDERR "Stack:[".
#DBG>					 join(',',map { $$_[0] } @$stack).
#DBG>					 "]\n";


        if  (exists($$actions{ACTIONS})) {

				defined($$token)
            or	do {
				($$token,$$value)=&$lex($self);
#DBG>				$debug & 0x01
#DBG>			and	print STDERR "Need token. Got ".&$ShowCurToken."\n";
			};

            $act=   exists($$actions{ACTIONS}{$$token})
                    ?   $$actions{ACTIONS}{$$token}
                    :   exists($$actions{DEFAULT})
                        ?   $$actions{DEFAULT}
                        :   undef;
        }
        else {
            $act=$$actions{DEFAULT};
#DBG>			$debug & 0x01
#DBG>		and	print STDERR "Don't need token.\n";
        }

            defined($act)
        and do {

                $act > 0
            and do {        #shift

#DBG>				$debug & 0x04
#DBG>			and	print STDERR "Shift and go to state $act.\n";

					$$errstatus
				and	do {
					--$$errstatus;

#DBG>					$debug & 0x10
#DBG>				and	$dbgerror
#DBG>				and	$$errstatus == 0
#DBG>				and	do {
#DBG>					print STDERR "**End of Error recovery.\n";
#DBG>					$dbgerror=0;
#DBG>				};
				};


                push(@$stack,[ $act, $$value ]);

					$$token ne ''	#Don't eat the eof
				and	$$token=$$value=undef;
                next;
            };

            #reduce
            my($lhs,$len,$code,@sempar,$semval);
            ($lhs,$len,$code)=@{$$rules[-$act]};

#DBG>			$debug & 0x04
#DBG>		and	$act
#DBG>		and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): ";

                $act
            or  $self->YYAccept();

            $$dotpos=$len;

                unpack('A1',$lhs) eq '@'    #In line rule
            and do {
                    $lhs =~ /^\@[0-9]+\-([0-9]+)$/
                or  die "In line rule name '$lhs' ill formed: ".
                        "report it as a BUG.\n";
                $$dotpos = $1;
            };

            @sempar =       $$dotpos
                        ?   map { $$_[1] } @$stack[ -$$dotpos .. -1 ]
                        :   ();

            $semval = $code ? &$code( $self, @sempar )
                            : @sempar ? $sempar[0] : undef;

            splice(@$stack,-$len,$len);

                $$check eq 'ACCEPT'
            and do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Accept.\n";

				return($semval);
			};

                $$check eq 'ABORT'
            and	do {

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Abort.\n";

				return(undef);

			};

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Back to state $$stack[-1][0], then ";

                $$check eq 'ERROR'
            or  do {
#DBG>				$debug & 0x04
#DBG>			and	print STDERR 
#DBG>				    "go to state $$states[$$stack[-1][0]]{GOTOS}{$lhs}.\n";

#DBG>				$debug & 0x10
#DBG>			and	$dbgerror
#DBG>			and	$$errstatus == 0
#DBG>			and	do {
#DBG>				print STDERR "**End of Error recovery.\n";
#DBG>				$dbgerror=0;
#DBG>			};

			    push(@$stack,
                     [ $$states[$$stack[-1][0]]{GOTOS}{$lhs}, $semval ]);
                $$check='';
                next;
            };

#DBG>			$debug & 0x04
#DBG>		and	print STDERR "Forced Error recovery.\n";

            $$check='';

        };

        #Error
            $$errstatus
        or   do {

            $$errstatus = 1;
            &$error($self);
                $$errstatus # if 0, then YYErrok has been called
            or  next;       # so continue parsing

#DBG>			$debug & 0x10
#DBG>		and	do {
#DBG>			print STDERR "**Entering Error recovery.\n";
#DBG>			++$dbgerror;
#DBG>		};

            ++$$nberror;

        };

			$$errstatus == 3	#The next token is not valid: discard it
		and	do {
				$$token eq ''	# End of input: no hope
			and	do {
#DBG>				$debug & 0x10
#DBG>			and	print STDERR "**At eof: aborting.\n";
				return(undef);
			};

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Dicard invalid token ".&$ShowCurToken.".\n";

			$$token=$$value=undef;
		};

        $$errstatus=3;

		while(	  @$stack
			  and (		not exists($$states[$$stack[-1][0]]{ACTIONS})
			        or  not exists($$states[$$stack[-1][0]]{ACTIONS}{error})
					or	$$states[$$stack[-1][0]]{ACTIONS}{error} <= 0)) {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Pop state $$stack[-1][0].\n";

			pop(@$stack);
		}

			@$stack
		or	do {

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**No state left on stack: aborting.\n";

			return(undef);
		};

		#shift the error token

#DBG>			$debug & 0x10
#DBG>		and	print STDERR "**Shift \$error token and go to state ".
#DBG>						 $$states[$$stack[-1][0]]{ACTIONS}{error}.
#DBG>						 ".\n";

		push(@$stack, [ $$states[$$stack[-1][0]]{ACTIONS}{error}, undef ]);

    }

    #never reached
	croak("Error in driver logic. Please, report it as a BUG");

}#_Parse
#DO NOT remove comment

1;

}
#End of include--------------------------------------------------


#line 1 "Gift.yp"


#
#Note for the Authors: Look at: /home/webs/pcgull/moodle/datos/12/cuestionario in zion

# The standalone parser in this module was built using the Parse::Yapp 
# distribution (available from CPAN). The author of Parse::Yapp is Francois Desarmenien.

my $input; # input stream

# State variables: we divide the question in three parts
# prefix-statement { answer section } post-statement
# The state variables indicate what sort of part we are
my $inside_answers = 0; # true iff inside answer section { ... }
my $inside_match = 0;   # true iff inside a match answer section
my $inside_numeric = 0; # true iff inside a numeric answer section
my $inside_truefalse = 0; # true iff inside a truefalse answer section
my $post_state = 0;     # true iff in the post-statement part
my $newquestion = 1;    # true iff we expect a new question

my $lineno = 1;
my $answerno = 0; # number of answers in current question 
my $numright = 0; # number of answers of type = (right or correct answer) 
my $is_shortanswer = 0;
my $is_multipleanswer = 0;
my $num_positive_weights = 0;
my $weightsum = 0;
my $numwarningslimit = 3;
my $numwarnings = 0;
my $prestate; 

sub trim_end {
  defined($_) and s/\s*$// for (@_);
}

sub set_error {
  my ($parser, $message) = @_;
  $parser->YYData->{ERRMSG} = $message;
}

sub build_question { 
  my $parser = shift;
  my ($prestate, $answers, $posstate) = @_;
  my @answers = @$answers;
  
  my $problem = { PRESTATE => $prestate, ANSWERS => $answers, POSTSTATE => $posstate };
  # Compute problem class 
  my $answer = $answers[0];

  # warning! the order is important in the following statements
  return bless $problem, 'Gift::MATCH' if exists($answer->{FIRST}); 
  if ($answer->{TYPE} eq 'TRUEFALSE') {
    delete($answer->{TYPE}); # now is redundant
    return bless $problem, 'Gift::TRUEFALSE' 
  }
  return bless $problem, 'Gift::NUMERIC' if ($answer->{TYPE} eq 'NUMERIC') or ($answer->{TYPE} eq 'NUMERICRANGE'); 
  if ($is_shortanswer) {

    for $answer (@answers) {
      delete($answer->{TYPE});
    }
    return bless $problem, 'Gift::SHORTANSWER' 
  }
  if ($is_multipleanswer) {
    for $answer (@answers) {
      delete($answer->{TYPE});
    }
    return bless $problem, 'Gift::MULTIPLEANSWER' 
  }
  return bless $problem, 'Gift::MULTIPLECHOICE' if ($answer->{TYPE} =~ m{RIGHT|WRONG}); 
  die 'Fatal Internal Error. Contact the authors.',"/n";
}



sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    my($self)=$class->SUPER::new( yyversion => '1.05',
                                  yystates =>
[
	{#State 0
		ACTIONS => {
			'PRESTATE' => 3
		},
		DEFAULT => -4,
		GOTOS => {
			'question' => 1,
			'gift' => 2,
			'questions' => 4
		}
	},
	{#State 1
		DEFAULT => -3
	},
	{#State 2
		ACTIONS => {
			'' => 5
		}
	},
	{#State 3
		ACTIONS => {
			"{" => 6
		}
	},
	{#State 4
		ACTIONS => {
			'QUESTIONSEP' => 7
		},
		DEFAULT => -1
	},
	{#State 5
		DEFAULT => 0
	},
	{#State 6
		ACTIONS => {
			'ANSWER' => 8
		},
		GOTOS => {
			'answers' => 9
		}
	},
	{#State 7
		ACTIONS => {
			'PRESTATE' => 3
		},
		DEFAULT => -4,
		GOTOS => {
			'question' => 10
		}
	},
	{#State 8
		DEFAULT => -7
	},
	{#State 9
		ACTIONS => {
			"}" => 11,
			'ANSWER' => 12
		}
	},
	{#State 10
		DEFAULT => -2
	},
	{#State 11
		ACTIONS => {
			'POSTSTATE' => 14
		},
		DEFAULT => -8,
		GOTOS => {
			'poststate' => 13
		}
	},
	{#State 12
		DEFAULT => -6
	},
	{#State 13
		DEFAULT => -5
	},
	{#State 14
		DEFAULT => -9
	}
],
                                  yyrules  =>
[
	[#Rule 0
		 '$start', 2, undef
	],
	[#Rule 1
		 'gift', 1,
sub
#line 86 "Gift.yp"
{ $_[1]; }
	],
	[#Rule 2
		 'questions', 3,
sub
#line 91 "Gift.yp"
{ push(@{$_[1]}, $_[3]) if defined($_[3]); $_[1] }
	],
	[#Rule 3
		 'questions', 1,
sub
#line 92 "Gift.yp"
{ defined($_[1])? [ $_[1] ] : [] }
	],
	[#Rule 4
		 'question', 0,
sub
#line 94 "Gift.yp"
{ undef }
	],
	[#Rule 5
		 'question', 5,
sub
#line 99 "Gift.yp"
{ $_[0]->build_question($_[1], $_[3], $_[5]); }
	],
	[#Rule 6
		 'answers', 2,
sub
#line 102 "Gift.yp"
{ 
                          push @{$_[1]}, $_[2]; $_[1]  
                        }
	],
	[#Rule 7
		 'answers', 1,
sub
#line 105 "Gift.yp"
{ 
                          [ $_[1] ] 
                        }
	],
	[#Rule 8
		 'poststate', 0,
sub
#line 110 "Gift.yp"
{ '' }
	],
	[#Rule 9
		 'poststate', 1, undef
	]
],
                                  @_);
    bless($self,$class);
}

#line 113 "Gift.yp"


sub Warning {
  my $parser = shift;

  my ($Err, $message) = @_;
  $parser->set_error($message);

  $numwarnings++;
  return if $numwarnings >= $numwarningslimit;

  if ($lineno > 1) {
    warn "$Err around lines ",$lineno-1," and $lineno!\n";
  }
  else {
    warn "$Err around $lineno!\n";
  }

  exists $parser->YYData->{ERRMSG}
    and do {
        warn $parser->YYData->{ERRMSG}."\n";
        delete $parser->YYData->{ERRMSG};
    };

  if (defined($prestate)) {
    warn "\nLast question processed:\n$prestate\n";
  }
  $input =~ m{^\s*(.{3,50}\S*)}sg;

  if (defined($1)) {
    my $text = $1;
    warn "$Err is probably before or around:\n\n$text\n";
  }
  else {
    warn "at end of input\n";
  }
}

sub Error {
  my ($parser, $message) = @_;

  $parser->Warning('Error', $message);
  die "\n";
}

sub countlines {
  my $match = shift;

  return ($match =~ tr/\n/\n/);
}

sub Lex_newquestion {
  my $parser = shift;

  unless ($input =~ s/\A\s*
             ((\\.|[^{])*) # No { or escaped character
             {   # Everything up to the curly bracket constitutes the pre-statement
             /{/x)  {
    $prestate = $1;
    defined($prestate) and 
      $input = "$prestate$input";
    $parser->Error("Curly bracket ('{') expected. ");
  };
  $lineno += &countlines($&);
  $prestate = $1;
  # Get the attributes
  $prestate =~ m{(::
                 ((\\.|[^:])+) # question name
                 ::)?
                 (\[(\w+)\])?  # question type: html, plain, etc.
                 (.*)          # prefix statement
               }sx;
  $newquestion = 0;
  return ('PRESTATE', { NAME => $2, FORMAT => $5, PREFIX => $6 });
}

sub parse_end_of_answer {
  my ($parser, $match) = @_;

  $lineno += &countlines($match);
  $parser->Error("A match question must have at least three pairs") 
                            if ($inside_match and ($answerno <= 2));

  $parser->Error("Sum of weights is ${weightsum}% > 100%.") 
                                      if ($weightsum > 100);

  $parser->Error("A TRUE-FALSE question can have only one answer.") 
                         if ($inside_truefalse and ($answerno > 1));

  if (!$inside_numeric and !$inside_match) {
    $is_shortanswer = ($answerno == $numright);
    $is_multipleanswer = (($numright == 0) and $num_positive_weights);
    
  }

  $inside_answers       = $inside_match     = $inside_numeric = 
  $answerno             = $numright         = $weightsum      = 
  $num_positive_weights = $inside_truefalse = 0;
  $post_state = 1;

  return ('}', '}');
}

# Inside the answer section white spaces between answers can be skipped 
sub skip_whites {
  $input =~ s/\A\s+//; 
  $lineno += &countlines($&) if defined($&);
}

sub parse_numeric {
  my $parser = shift;

  if ($input =~ s/\A          # range type {#3.141..3.142}
                  =?
                  (%\s*(-?\d+)\s*%)? # weight
                  (\d+(\.\d+)?)(e[-+]?\d+)?
                  \s*\.\.\s*
                  (\d+(\.\d+)?)(e[-+]?\d+)?
                  (\s*\#((\\.|[^=~}])*))?
                  //xi) {  # range
    my ($weight, $firstnumber, $secondnumber, $comment) = ($2, $3, $6, $10); 
    $lineno += &countlines($&);
    $weightsum += $weight if defined($weight);
    &trim_end($comment); # trim final spaces in comment
    return ('ANSWER', 
            { TYPE  => 'NUMERICRANGE',
              ANSWER => [ $firstnumber, $secondnumber ], 
              COMMENT => $comment
            }
           );
  }

  if ($input =~ s/\A
                     =?
                     (%\s*(-?\d+)\s*%)?
                     (\d+(\.\d+)?)(e[-+]?\d+)?
                     (:
                       (\d+(\.\d+)?)(e[-+]?\d+)? # error threshold
                     )? 
                     (\s*\#((\\.|[^=~}])*))?
                     //xi) { 
    my ($weight, $number, $error, $comment) = ($2, $3, $7, $11);  
    $lineno += &countlines($&);
    $weightsum += $weight if defined($weight);
    &trim_end($comment);
    return ('ANSWER', 
            { TYPE  => 'NUMERIC',
              WEIGHT => $weight,
              ANSWER => [$number, $error], 
              COMMENT => $comment
            }
           );
  }

  $parser->Error("Expecting a numeric answer or }");

}

sub parse_truefalse {
  my $parser = shift;
  my ($match, $token, $commenttrue, $commentfalse) = @_;

  $lineno += &countlines($match);
  &trim_end($commenttrue, $commentfalse);
  $answerno++;
  $inside_truefalse = 1;
  if ($answerno > 1) {
    $parser->Error("A True-False question admits only one answer.");
  }
  return ($token =~ m{T}i)? 
     ('ANSWER', { TYPE => 'TRUEFALSE',
                ANSWER => 'TRUE', 
                COMMENT_TRUE => $commenttrue, 
                COMMENT_FALSE => $commentfalse
              }) : 
     ('ANSWER', { TYPE => 'TRUEFALSE',
                ANSWER => 'FALSE', 
                COMMENT_TRUE => $commenttrue, 
                COMMENT_FALSE => $commentfalse
               });
}

sub parse_single_short_answer {
  my $parser = shift;
  my ($match, $weight, $answer, $comment) = @_ ; # ($2, $3, $6);

  # Single Answer: Short answer
  # ... If there is only one correct Short Answer, it may be written without
  # the equal sign prefix, as long as it cannot be confused as True-False.
  $lineno += &countlines($match);
  if (defined($weight)) {
    $weightsum += $weight;
    $num_positive_weights++ if $weight > 0;
  }
  &trim_end($answer, $comment);
  $answerno++;
  $numright++;
  return ('ANSWER', 
          { TYPE => 'RIGHT',
            WEIGHT => $weight, 
            ANSWER => $answer, 
            COMMENT => $comment 
          }
         );
}

sub parse_match {
  my $parser = shift;
  my ($weight, $comment) = @_;

  ((!$inside_match) and ($answerno > 1)) and
    $parser->Error(
     "Inside a Matching question all the answers have to match the format:\n".
     "a -> b\n".
     "An answer previous to ${answerno}th answer does not have an arrow."
    ); 

  $inside_match = 1; 
  # Warn if weights 
  $parser->Warning(
    'Warning', 
    "Matching questions do not support feedback or percentage answer weights."
  ) if (defined($weight) or defined($comment));

  return ('ANSWER', { FIRST => $1, SECOND => $2, });
}

sub update_weights {
  my $weight = shift;

  $weightsum += $weight;
  $num_positive_weights++ if $weight > 0;
}

sub parse_right_or_wrong {
  my $parser = shift;
  my ($match, $type, $weight, $answer, $comment) = @_;

  $lineno += &countlines($match);
  &update_weights($weight) if (defined($weight));
  &trim_end($answer, $comment);
  $answerno++;

  $parser->Error("Illformed matching question. Expected ~ or ->.") 
    if ($inside_match and (($type eq '~') or ($answer !~ m{(.+?)\s*->\s*(.+)})));

  if ($type eq '=') {

   # lazy operator +?: first arrow appearance. Spaces are required?
   return $parser->parse_match($weight, $comment) if ($answer =~ m{(.+?)\s*->\s*(.+)});

   $numright++;
   return ('ANSWER', 
           { TYPE => 'RIGHT',
             WEIGHT => $weight, 
             ANSWER => $answer, 
             COMMENT => $comment 
           }
          );
  } # end of ($type eq '=')

  return ('ANSWER', 
          { TYPE => 'WRONG',
            WEIGHT => $weight, 
            ANSWER => $answer, 
            COMMENT => $comment 
          }
         );
}

sub Lex_inside_answers {
  my $parser = shift;

  &skip_whites();

  return $parser->parse_end_of_answer($&) if ($input =~ s/\A\s*}//);  # CLOSING }

  return $parser->parse_numeric() if ($inside_numeric);
  
  return $parser->parse_truefalse($&, $1, $5, $7) if 
                  ($input =~ s{\A(
                       (TR?U?E?)|(FA?L?S?E?)) # answer
                       (\#([^\}]*))?          # comment for the first answer
                       \s*
                       (\#([^\}]*))?}         # comment for the second answer
                  {}ix);

  return $parser->parse_single_short_answer($&, $2, $3, $6) if 
       ($input =~ s/\A\s*       # single short answer
                      (%\s*(-?\d+)\s*%)?   # weight
                      ((\\.|[^~=\}\#])+)   # the answer
                      (\#((\\.|[^=~}])*))? # feedback
                      \s*}                 # the end of the answer: }
                  /}/x                     # give back the }
       );

  return $parser->parse_right_or_wrong($&, $1, $3, $4, $7) if 
         ($input =~ s/\A\s*
                      ([~=])               # right or wrong answer
                      (%\s*(-?\d+)\s*%)?   # weight
                      ((\\.|[^~=\}\#])+)   # the answer
                      (\#((\\.|[^=~}])*))? # feedback
                      \s*                  # the end of the answer
                     //x            
         ); 
  $parser->Error("Expected a correct answer. ");
}

sub Lex {
  my $parser = shift;

  while ($input) {

    $lineno += &countlines($&) while ($input =~ s{\A//.*\n}{}); # skip comments

    return $parser->Lex_newquestion() if ($newquestion);

    if ($input =~ s/\A{\s*//) { # Curly bracket "{": going to the "answer" section
      $lineno += &countlines($&);
      $inside_answers = 1;
      if ($input =~ s{\A#\s*}{}){
        $lineno += &countlines($&);
        $inside_numeric = 1 
      }
      return ('{', '{');
    }

    if ($post_state and ($input =~ s/\A(.*?)((\n\s*\n)|\Z)/$2/s)) { 
      $lineno += &countlines($1);
      $post_state = 0;
      return ('POSTSTATE', $1 );
    }

    if ($input =~ s/\A\n\s*\n(\S)/$1/) { # There must be at least one character 
      $newquestion = 1;                  # after \n\n to have a new question
      $lineno += &countlines($&);
      $lineno += &countlines($&) while ($input =~ s{\A//.*\n}{} or $input =~ s{\A\s+}{}); 
      return 'QUESTIONSEP', 'QUESTIONSEP';
    }

    if ($input =~ s/\A\s*\Z//) {
      $lineno += &countlines($&);
      return ('',undef); # END OF INPUT
    }

    return $parser->Lex_inside_answers() if ($inside_answers);

    $parser->Error("New question or new answer or end of input expected. ");
  } # while $input ...
}

sub ParseGift {
  my $class = shift;

  $inside_answers = 0; # true iff inside answer section { ... }
  $inside_match = 0;   # true iff inside a match answer section
  $inside_numeric = 0; # true iff inside a numeric answer section
  $inside_truefalse = 0; # true iff inside a truefalse answer section
  $post_state = 0;     # true iff in the post-statement part
  $newquestion = 1;    # true iff we expect a new question

  $lineno = 1;
  $answerno = 0; # number of answers in current question 
  $numright = 0; # number of answers of type = (right or correct answer) 
  $is_shortanswer = 0;
  $is_multipleanswer = 0;
  $num_positive_weights = 0;
  $weightsum = 0;
  $numwarningslimit = 3;
  $numwarnings = 0;
  $prestate = ""; 

  my $parser = new Gift();
  # initial blanks and comments
  while (($input =~ s{\A\s+}{}) or ($input =~ s{\A//.*\n}{})) {
    $lineno += &countlines($&) 
  }
  my $result = $parser->YYParse(yylex => \&Lex, yyerror => \&Error, yydebug => 0x0);
  bless $result, $class; 
}

# Receives a string and does the parsing returning
# the gift data structure
sub GiftFromString {
  my ($class, $string) = @_;

  die "An input string must be provided\n" unless defined($string);

  $input = $string; # dont' destroy $string

  return $class->ParseGift();
}

# Receives a file name and does the parsing returning
# the gift data structure
sub GiftFromFile {
  my ($class, $file) = @_;

  die "The name of a gift file must be provided\n" unless defined($file);
  open FILE, $file or die "Can't open file $file\n";
  {
    local $/ = undef;
    $input = <FILE>;
  }
  close(FILE);

  return $class->ParseGift();
}

package Gift::Question;

sub is_a_MISSINGWORD {
 my $self = shift;

 length($self->{POSTSTATE})
}

sub number_of_answers {
  my $self = shift;

  scalar (@{$self->{ANSWERS}});
}

sub PRESTATE {
  my $self = shift;

  $self->{PRESTATE} = $_[0] if defined($_[0]);
  return $self->{PRESTATE}
}

sub PREFIX {
  my $self = shift;

  $self->PRESTATE->{PREFIX} = $_[0] if defined($_[0]);
  return $self->{PREFIX};
}

sub FORMAT {
  my $self = shift;

  $self->PRESTATE->{FORMAT} = $_[0] if defined($_[0]);
  return $self->{FORMAT};
}

sub NAME {
  my $self = shift;

  $self->PRESTATE->{NAME} = $_[0] if defined($_[0]);
  return $self->{NAME};
}

sub ANSWERS {
  my $self = shift;

  $self->{ANSWERS} = $_[0] if defined($_[0]);
  return $self->{ANSWERS}
}

sub POSTSTATE {
  my $self = shift;

  $self->{POSTSTATE} = $_[0] if defined($_[0]);
  return $self->{POSTSTATE}
}

package Gift::TRUEFALSE;
our @ISA = ('Gift::Question');


package Gift::MULTIPLECHOICE;
our @ISA = ('Gift::Question');


package Gift::SHORTANSWER;
our @ISA = ('Gift::Question');


package Gift::MATCH;
our @ISA = ('Gift::Question');


package Gift::NUMERIC;
our @ISA = ('Gift::Question');


package Gift::MULTIPLEANSWER;
our @ISA = ('Gift::Question');

######################################################

=head1 NAME

Gift - Parser for Moodle Gift format 

=head1 SYNOPSIS

    use Gift;

    my $result = Gift->GiftFromFile($filename);

    my $result = Gift->GiftFromString($input);

=head1 DESCRIPTION

Moodle is an Open Source Learning Management System. It
uses GIFT (which stands for General Import Format Technology) 
to save and recover quiz questions to and from text files.

This module provides a parser for the GIFT format.

The idea which moved us to write it
was that Perl programmers writing 
translators from GIFT format to other formats (most commonly to other
course management system formats but also to 
edition languages like LaTeX 
or to produce a standalone
CGI for the quizs) can benefit of having 
the parser and concentrate their efforts in 
writing the back-end phase of generating
the target format.

=head2 Methods in the Gift class: C<GiftFromFile> and C<GiftFromString>

The method C<GiftFromFile> receives as 
its only parameter the name of a file containing a questionnaire
written in Moodle GIFT format.  It returns a Gift object describing
the questionnaire. 

The method C<GiftFromString> is similar but receives the input string
containing the questions in GIFT format.


The following script C<gift> enclosed with this distribution
illustrates the use of the method:

    $ cat gift
    #!/usr/bin/perl -I../lib -w
    use strict;
    use Gift;
    use Data::Dumper;

    die "Usage:\n$0 giftfile\n" unless (@ARGV == 1);

    my $result = Gift->GiftFromFile(@ARGV);
    print Dumper($result);


Let us feed the script with the following C<numeric1.gift> file as input:

  $ cat numeric1.gift

  When was Ulysses S. Grant born? {#
      =1822:0
      =%50%1822:2}

When running it, we get this output that describes the
generated data structure:

$ gift numeric1.gift

  $VAR1 = bless( [
     bless( {
        'PRESTATE' => {
           'FORMAT' => undef, 'NAME' => undef, 
           'PREFIX' => 'When was Ulysses S. Grant born? '
        },
        'ANSWERS' => [
           { 'WEIGHT' => undef, 'COMMENT' => undef, 
             'TYPE' => 'NUMERIC',
             'ANSWER' => [ '1822', '0' ]
           },
           { 'WEIGHT' => 50, 'COMMENT' => undef, 
             'TYPE' => 'NUMERIC',
             'ANSWER' => [ '1822', '2' ]
           },
        'POSTSTATE' => '',
        ]
      }, 'Gift::NUMERIC' )
   ], 'Gift' );


A Gift object is an array of questions. Each question is an object
blessed in its class. The following classes of questions 
are supported:

=over

=item * C<Gift::MATCH> for Matching questions

=item * C<Gift::MULTIPLEANSWER> for  multiple choice questions where two or more answers
                          must be selected in order to obtain full credit

=item * C<Gift::MULTIPLECHOICE> for Multiple Choice questions

=item * C<Gift::NUMERIC> for the two types of numeric questions (range and threshold)

=item * C<Gift::SHORTANSWER> for Short Answer questions

=item * C<Gift::TRUEFALSE> for True-false questions

=back

A question is a hash with 3 keys:
C<PRESTATE>, C<POSTSTATE> and C<ANSWERS>.
These keys correspond 
to divide a gift question in three parts

  prefix-statement { answer section } post-statement

The hash entry C<PRESTATE> is a reference to a hash with
keys:

=over

=item *  C<FORMAT> describing the format in which it is 
written the question: html, plain, etc., 

=item * C<NAME> 
the optional name for the question and 

=item * C<PREFIX>
containing the text of the question before
the answer section.

=back

The hash entry C<POSTSTATE> is a string containing the text of the question
after the answer section.

The hash entry C<ANSWERS> is a reference to an array
of hashes describing the list of answers for this
question. The fields in these answer hashes depend
on the class of question and are described below.

=head2 The C<Gift::Question> class

All the question classes inherit from the C<Gift::Question> class.
The C<Gift::Question> class provides the methods

=over

=item C<is_a_MISSINGWORD> 

Which returns TRUE
if the question matches the Missing Word format, i.e.
has a non empty postfix.

When displaying a Missing Word, the Moodle quiz engine inserts a fill-in-the-blank line (like
this _____) in the middle of the sentence. To use the Missing Word format,
place the answer section before the end of the sentence.
All question types can be written in the Missing Word format.


=item C<number_of_answers>

The C<Gift::Question> class has also the method

          number_of_answers

which returns the number of answers in the question.

Follows an example of use:

  my $result = Gift->GiftFromString($input);

  for (@$result) {
    print Dumper($_) if $_->is_a_MISSINGWORD;
    print $_->number_of_answers()."\n";
  }

=item The following accesor/mutators (getter-setters) for the 
C<Gift::Question> object:

=over

=item C<PRESTATE> 

A reference to a hash with keys C<PREFIX>, C<FORMAT> and C<NAME>

=item C<PREFIX>

A string. The text of the question before the answer section.

=item C<FORMAT> 

A string. Set/Returns the format used for the question prefix: html, plain, etc.

=item C<NAME>

A string. The name of the question.

=item C<ANSWERS>

A reference to the array of answers. Each element is a reference to a hash
describing the answer.

=item C<POSTSTATE>

A string. The text of the question after the answer section.

=back

=back

=head2 The Gift::MATCH Class

Matching answers always begin with an equal sign (C<=>) and are separated by an arrow 
C<-E<gt>>. There must be at least three matching pairs.
Matching questions do not support feedback or percentage answer weights,
this parser will issue a warning (but not a fatal error) if they are there.
The Matching question:

  Match the following countries with their corresponding capitals. {
    =Canada -> Ottawa
    =Italy  -> Rome
    =Japan  -> Tokyo
    =India  -> New Delhi
    }

produces the object:

  bless( {
    'PRESTATE' => {
      'PREFIX' => 'Match the following countries with their corresponding capitals. ',
      'FORMAT' => undef,
      'NAME' => undef
    },
    'ANSWERS' => [
       { 'FIRST' => 'Canada', 'SECOND' => 'Ottawa', },
       { 'FIRST' => 'Italy', 'SECOND' => 'Rome', },
       { 'FIRST' => 'Japan', 'SECOND' => 'Tokyo', },
       { 'FIRST' => 'India', 'SECOND' => 'New Delhi', }
     ],
    'POSTSTATE' => '',
  }, 'Gift::MATCH' )

=head2 The Gift::MULTIPLEANSWER Class

The Multiple Answers option is used for multiple choice questions when two or
more answers must be selected in order to obtain full credit. The multiple
answers option is enabled by assigning partial answer weights to multiple
answers.
All the answers have to start with the tilde sign (C<~>) and the weights should add
no more than 100%, otherwise the parser will return an error. To avoid the problem
of students automatically getting 100% by simply checking all of the answers,
it is best to include negative answer weights for wrong answers.

For this question:

     What two people are entombed in Grant's tomb? {
          ~%-50%No one
          ~%50%Grant
          ~%50%Grant's wife
          ~%-50%Grant's father }

the parser produces:

  bless( {
    'PRESTATE' => {
      'PREFIX' => 'What two people are entombed in Grant\'s tomb? ',
      'FORMAT' => undef,
      'NAME' => undef
    },
    'ANSWERS' => [
                   {
                     'COMMENT' => undef,
                     'WEIGHT' => undef,
                     'ANSWER' => 'No one'
                   },
                   {
                     'COMMENT' => undef,
                     'WEIGHT' => '50',
                     'ANSWER' => 'Grant'
                   },
                   {
                     'COMMENT' => undef,
                     'WEIGHT' => '50',
                     'ANSWER' => 'Grant\'s wife'
                   },
                   {
                     'COMMENT' => undef,
                     'WEIGHT' => undef,
                     'ANSWER' => 'Grant\'s father'
                   }
                 ],
    'POSTSTATE' => '',
  }, 'Gift::MULTIPLEANSWER' )


=head2 The Gift::MULTIPLECHOICE Class

In the GIFT format, inside multiple choice questions, 
wrong answers are prefixed with a tilde (~) and
the correct answer is prefixed with an equal sign (=).

     Grant is {~buried =entombed ~living} in Grant's tomb.

This is also an example of Missing Word format question 
since there is text after the answers.

The former question produces the object:

  $x = bless( { 
     'PRESTATE' => { 'PREFIX' => 'Grant is ', '
        FORMAT' => undef, 'NAME' => undef },
     'ANSWERS' => [
      { 'TYPE' => 'WRONG', 'COMMENT' => undef, 
        'WEIGHT' => undef, 'ANSWER' => 'buried' },
      { 'TYPE' => 'RIGHT', 'COMMENT' => undef,
        'WEIGHT' => undef, 'ANSWER' => 'entombed' },
      { 'TYPE' => 'WRONG', 'COMMENT' => undef, 
        'WEIGHT' => undef, 'ANSWER' => 'living' }
      ],
     'POSTSTATE' => ' in Grant\'s tomb.',
   }, 'Gift::MULTIPLECHOICE' );

The answer key C<TYPE> indicates what kind of answer is: right or wrong.
Optionally an answer may have a C<WEIGHT> percentage saying the contribution
of the answer to the total. The field C<COMMENT> holds the feedback 
comment that will be displayed when the student chooses that answer.

=head2 The Gift::NUMERIC Class

The answer section for Numerical questions must start with a number sign (C<#>).
Numerical answers can include an error margin, which is written following the
correct answer, separated by a colon. 
Multiple Numerical Answers can be combined
to specify numerical multiple spans.
If multiple answers are used, they must be separated by an equal
sign.

The C<Gift::NUMERIC> question:

     When was Ulysses S. Grant born? {#
         =1822:0
         =%50%1822:2}

produces:

  bless( {
    'ANSWERS' => [
                   {
                     'TYPE' => 'NUMERIC',
                     'COMMENT' => undef,
                     'WEIGHT' => undef,
                     'ANSWER' => [ '1822', '0' ]
                   },
                   {
                     'TYPE' => 'NUMERIC',
                     'COMMENT' => undef,
                     'WEIGHT' => '50',
                     'ANSWER' => [ '1822', '2' ]
                   }
                 ],
    'PRESTATE' => {
                    'PREFIX' => 'When was Ulysses S. Grant born? ',
                    'FORMAT' => undef,
                    'NAME' => undef
                  },
    'POSTSTATE' => '',
  }, 'Gift::NUMERIC' )

Optionally, numerical answers can be written as a span in the following format
C<{#MinimumValue..MaximumValue}>.

  What is the value of pi (to 3 decimal places)? {#
   =3.1415 =%50%3.141..3.142}

  bless( {
    'POSTSTATE' => '.',
    'ANSWERS' => [
       {
         'TYPE' => 'NUMERIC',
         'COMMENT' => undef,
         'WEIGHT' => undef,
         'ANSWER' => [ '3.1415', undef ]
       },
       {
         'TYPE' => 'NUMERICRANGE',
         'COMMENT' => undef,
         'ANSWER' => [ '3.141', '3.142' ]
       }
     ],
    'PRESTATE' => {
        'PREFIX' => 'What is the value of pi (to 3 decimal places)? ',
        'FORMAT' => undef,
        'NAME' => undef
      }
  }, 'Gift::NUMERIC' )

=head2 The Gift::SHORTANSWER Class

In the GIFT format, answers in Short Answer question-type are all prefixed by an equal sign (=),
indicating that they are all correct answers. The answers must not contain a
tilde. The short answer question:

  Who's buried in Grant's tomb?{=no one =nobody}

the parser translates this question to:

   bless( {
            'POSTSTATE' => '',
            'ANSWERS' => [
                           {
                             'COMMENT' => undef,
                             'WEIGHT' => undef,
                             'ANSWER' => 'no one'
                           },
                           {
                             'COMMENT' => undef,
                             'WEIGHT' => undef,
                             'ANSWER' => 'nobody'
                           }
                         ],
            'PRESTATE' => 
              {
                'PREFIX' => 'Who\'s buried in Grant\'s tomb?',
                'FORMAT' => undef,
                'NAME' => undef
              }
          }, 'Gift::SHORTANSWER' )

When there is only one correct Short Answer, the question may be written without the equal
sign prefix:

  What is the charge on a CH<sub>3</sub>COO ion.{1-#correct}

produces:

  bless( {
    'PRESTATE' => {
      'PREFIX' => 'What is the charge on a CH<sub>3</sub>COO ion.',
      'FORMAT' => undef,
      'NAME' => undef
    },
    'ANSWERS' => [ {
                     'COMMENT' => 'correcto',
                     'WEIGHT' => undef,
                     'ANSWER' => '1-'
                   }
                 ],
    'POSTSTATE' => '',
  }, 'Gift::SHORTANSWER' )


=head2 The C<Gift::TRUEFALSE> Class

In this question-type the answer indicates whether the statement is true or
false. The answer should be written as C<{TRUE}> or C<{FALSE}>, or abbreviated to C<{T}>
or C<{F}>. The following True-False question:

  The sun rises in the east.{T}

is translated into:

  bless( {
    'PRESTATE' => {
      'PREFIX' => 'The sun rises in the east.',
      'FORMAT' => undef, 'NAME' => undef
    },
    'ANSWERS' => [
       {
         'COMMENT_FALSE' => undef,
         'COMMENT_TRUE' => undef,
         'ANSWER' => 'TRUE'
       }
     ],
    'POSTSTATE' => '',
  }, 'Gift::TRUEFALSE' )

 The fields C<COMMENT_TRUE> and  C<COMMENT_FALSE> hold the feedback 
comment that will be displayed by Moodle when the student chooses the
corresponding answer.


=head1 BUGS

We haven't found a formal definition of the GIFT language
and so we have based the building of this parser 
on the description given by the Moodle
help for the GIFT format. If you find any bugs,
please let us know to the first author address
E<lt>casiano@ull.esE<gt>

There are a few limits in the way the version of Moodle 
manages the GIFT format. 
Some of them are due to the way some "gift metasymbols",
(namely %, [, ] and  -> ) are not escaped
(all the experiences refer to the "plain" format):

=over 

=item * Clozed and Computed questions aren't supported by this parser. The version 
of Moodle we have used (1.5.2) has no gift handler to export Computed questions.

=item * The version we used of Moodle couldn't also import the clozed questions 
it previously exported.

=item * After exporting matching problems containing arrows (->,
   the metasymbol used to set up the pairs)
   inside the answer section,
   Moodle is not able to import them back correctly.
   We haven't found in which way arrows must be escaped inside 
   an answer to differentiate them from the arrow metasymbol.
   It seems that when dealing with several arrows the interpret chooses
   as metasymbol the first one.


=item * If you insert brackets ([, the metasymbol to indicate the type: html, plain, etc.)
        inside  the question, the Moodle interpreter 
        goes in trouble.

=back

=head2 EXPORT

The module does not export any symbols

=head1 SEE ALSO

See the help in Moodle about the GIFT format. To get it, go to questionnaire, 
create one if needed, then click on the 
help icon next to the import link. Paul Tsuchido Shew (http://ac.shew.jp)
wrote the php Moodle GIFT filter and the documentation.

=head1 ACKNOWLEDGEMENTS

Thanks to Universidad de La Laguna,
and National TIC project TIC2002-04498-C05-05 (TRACER).

=head1 AUTHOR

This is a join work by
Casiano Rodriguez Leon E<lt>casiano@ull.esE<gt>,
Coromoto Leon Hernandez E<lt>cleon@ull.esE<gt>,
and Luis Garcia Forte E<lt>lgforte@ull.esE<gt>.
Universidad de La Laguna.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Casiano Rodriguez Leon, Coromoto Leon Hernandez
and Luis Garcia Forte. 

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.


=cut

1;

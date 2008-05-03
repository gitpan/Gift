###################################################################################
#
#    This file was generated using Parse::Eyapp version 1.109.
#
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien.
# (c) Parse::Eyapp Copyright 2006-2007 Casiano Rodriguez-Leon. Universidad de La Laguna.
#        Don't edit this file, use source file "Gift.yp" instead.
#
#             ANY CHANGE MADE HERE WILL BE LOST !
#
###################################################################################
package Gift;
use strict;

push @Gift::ISA, 'Parse::Eyapp::Driver';


{ ###########Included /home/pl/LEyapp/lib//Parse/Eyapp/Driver.pm file
#
# Module Parse::Eyapp::Driver
#
# This module is part of the Parse::Eyapp package available on your
# nearest CPAN
#
# This module is based on Francois Desarmenien Parse::Yapp module
# (c) Parse::Yapp Copyright 1998-2001 Francois Desarmenien, all rights reserved.
# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.

package Parse::Eyapp::Driver;

require 5.006;

use strict;

our ( $VERSION, $COMPATIBLE, $FILENAME );

$VERSION = '1.109';
$COMPATIBLE = '0.07';
$FILENAME=__FILE__;

use Carp;

#Known parameters, all starting with YY (leading YY will be discarded)
my(%params)=(YYLEX => 'CODE', 'YYERROR' => 'CODE', YYVERSION => '',
	     YYRULES => 'ARRAY', YYSTATES => 'ARRAY', YYDEBUG => '', 
	     # added by Casiano
	     #YYPREFIX  => '',  # Not allowed at YYParse time but in new
	     YYFILENAME => '', 
       YYBYPASS   => '',
	     YYGRAMMAR  => 'ARRAY', 
	     YYTERMS    => 'HASH',
	     YYBUILDINGTREE  => '',
	     ); 
my (%newparams) = (%params, YYPREFIX => '',);

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
				PREFIX => "",
				CHECK => \$check };

	_CheckParams( [], \%newparams, \@_, $self );

		exists($$self{VERSION})
	and	$$self{VERSION} < $COMPATIBLE
	and	croak "Eyapp driver version $VERSION ".
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
    return $retval;
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

### Casiano methods

sub YYLhs { 
  # returns the syntax variable on
  # the left hand side of the current production
  my $self = shift;

  return $self->{CURRENT_LHS}
}

sub YYRuleindex { 
  # returns the index of the rule
  # counting the super rule as rule 0
  my $self = shift;

  return $self->{CURRENT_RULE}
}

sub YYRightside { 
  # returns the rule
  # counting the super rule as rule 0
  my $self = shift;

  return @{$self->{GRAMMAR}->[$self->{CURRENT_RULE}]->[2]};
}

sub YYIsterm {
  my $self = shift;
  my $symbol = shift;

  return exists ($self->{TERMS}->{$symbol});
}

sub YYIssemantic {
  my $self = shift;
  my $symbol = shift;

  $self->{TERMS}->{$symbol} = shift if @_;
  return ($self->{TERMS}->{$symbol});
}


sub YYName {
  my $self = shift;

  my $current_rule = $self->{GRAMMAR}->[$self->{CURRENT_RULE}];
  $current_rule->[0] = shift if @_;
  return $current_rule->[0];
}

sub YYPrefix {
  my $self = shift;

  $self->{PREFIX} = $_[0] if @_;
  #$self->{PREFIX} .= '::' unless  $self->{PREFIX} =~ /::$/;
  $self->{PREFIX};
}

sub YYFilename {
  my $self = shift;

  $self->{FILENAME} = $_[0] if @_;
  $self->{FILENAME};
}

sub YYBypass {
  my $self = shift;

  $self->{BYPASS} = $_[0] if @_;
  $self->{BYPASS};
}

sub YYBypassrule {
  my $self = shift;

  $self->{GRAMMAR}->[$self->{CURRENT_RULE}][3] = $_[0] if @_;
  return $self->{GRAMMAR}->[$self->{CURRENT_RULE}][3];
}

sub YYFirstline {
  my $self = shift;

  $self->{FIRSTLINE} = $_[0] if @_;
  $self->{FIRSTLINE};
}

# Influences the behavior of YYActionforT_X1X2
# YYActionforT_single and YYActionforT_empty
# If true these methods will build simple lists of attributes 
# for the lists operators X*, X+ and X? and parenthesis (X Y)
# Otherwise the classic node construction for the
# syntax tree is used
sub YYBuildingTree {
  my $self = shift;

  $self->{BUILDINGTREE} = $_[0] if @_;
  $self->{BUILDINGTREE};
}

sub BeANode {
  my $class = shift;

    no strict 'refs';
    push @{$class."::ISA"}, "Parse::Eyapp::Node" unless $class->isa("Parse::Eyapp::Node");
}

#sub BeATranslationScheme {
#  my $class = shift;
#
#    no strict 'refs';
#    push @{$class."::ISA"}, "Parse::Eyapp::TranslationScheme" unless $class->isa("Parse::Eyapp::TranslationScheme");
#}

{
  my $attr =  sub { 
      $_[0]{attr} = $_[1] if @_ > 1;
      $_[0]{attr}
    };

  sub make_node_classes {
    my $self = shift;
    my $prefix = $self->YYPrefix() || '';

    { no strict 'refs';
      *{$prefix."TERMINAL::attr"} = $attr;
    }

    for (@_) {
       BeANode("$prefix$_"); 
    }
  }
}

####################################################################
# Usage      : ????
# Purpose    : Responsible for the %tree directive 
#              On each production the default action becomes:
#              sub { goto &Parse::Eyapp::Driver::YYBuildAST }
#
# Returns    : ????
# Parameters : ????
# Throws     : no exceptions
# Comments   : none
# See Also   : n/a
# To Do      : many things: Optimize this!!!!
sub YYBuildAST { 
  my $self = shift;
  my $PREFIX = $self->YYPrefix();
  my @right = $self->YYRightside(); # Symbols on the right hand side of the production
  my $lhs = $self->YYLhs;
  my $name = $self->YYName();
  my $bypass = $self->YYBypassrule; # Boolean: shall we do bypassing of lonely nodes?
  my $class = "$PREFIX$name";
  my @children;

  my $node = bless {}, $class;

  for(my $i = 0; $i < @right; $i++) {
    $_ = $right[$i]; # The symbol
    my $ch = $_[$i]; # The attribute/reference
    if ($self->YYIssemantic($_)) {
      my $class = $PREFIX.'TERMINAL';
      my $node = bless { token => $_, attr => $ch, children => [] }, $class;
      push @children, $node;
      next;
    }

    if ($self->YYIsterm($_)) {
      TERMINAL::save_attributes($ch, $node) if UNIVERSAL::can($PREFIX."TERMINAL", "save_attributes");
      next;
    }

    if (UNIVERSAL::isa($ch, $PREFIX."_PAREN")) { # Warning: weak code!!!
      push @children, @{$ch->{children}};
      next;
    }

    # If it is an intermediate semantic action skip it
    next if $_ =~ qr{@}; # intermediate rule
    next unless ref($ch);
    push @children, $ch;
  }

  
  if ($bypass and @children == 1) {
    $node = $children[0]; 
    # Re-bless unless is "an automatically named node", but the characterization of this is 
    bless $node, $class unless $name =~ /${lhs}_\d+$/; # lazy, weak (and wicked).
    return $node;
  }
  $node->{children} = \@children; 
  return $node;
}

sub YYBuildTS { 
  my $self = shift;
  my $PREFIX = $self->YYPrefix();
  my @right = $self->YYRightside(); # Symbols on the right hand side of the production
  my $lhs = $self->YYLhs;
  my $name = $self->YYName();
  my $class;
  my @children;

  for(my $i = 0; $i < @right; $i++) {
    $_ = $right[$i]; # The symbol
    my $ch = $_[$i]; # The attribute/reference

    if ($self->YYIsterm($_)) { 
      $class = $PREFIX.'TERMINAL';
      push @children, bless { token => $_, attr => $ch, children => [] }, $class;
      next;
    }

    if (UNIVERSAL::isa($ch, $PREFIX."_PAREN")) { # Warning: weak code!!!
      push @children, @{$ch->{children}};
      next;
    }

    # Substitute intermediate code node _CODE(CODE()) by CODE()
    if (UNIVERSAL::isa($ch, $PREFIX."_CODE")) { # Warning: weak code!!!
      push @children, $ch->child(0);
      next;
    }

    next unless ref($ch);
    push @children, $ch;
  }

  if (unpack('A1',$lhs) eq '@') { # class has to be _CODE check
          $lhs =~ /^\@[0-9]+\-([0-9]+)$/
      or  croak "In line rule name '$lhs' ill formed: report it as a BUG.\n";
      my $dotpos = $1;
 
      croak "Fatal error building metatree when processing  $lhs -> @right" 
      unless exists($_[$dotpos]) and UNIVERSAL::isa($_[$dotpos], 'CODE') ; 
      push @children, $_[$dotpos];
  }
  else {
    my $code = $_[@right];
    if (UNIVERSAL::isa($code, 'CODE')) {
      push @children, $code; 
    }
    else {
      croak "Fatal error building translation scheme. Code or undef expected" if (defined($code));
    }
  }

  $class = "$PREFIX$name";
  my $node = bless { children => \@children }, $class; 
  $node;
}

sub YYActionforT_TX1X2_tree {
  my $self = shift;
  my $head = shift;
  my $PREFIX = $self->YYPrefix();
  my @right = $self->YYRightside();
  my $class;

  for(my $i = 1; $i < @right; $i++) {
    $_ = $right[$i];
    my $ch = $_[$i-1];
    if ($self->YYIssemantic($_)) {
      $class = $PREFIX.'TERMINAL';
      push @{$head->{children}}, bless { token => $_, attr => $ch, children => [] }, $class;
      
      next;
    }
    next if $self->YYIsterm($_);
    if (ref($ch) eq  $PREFIX."_PAREN") { # Warning: weak code!!!
      push @{$head->{children}}, @{$ch->{children}};
      next;
    }
    next unless ref($ch);
    push @{$head->{children}}, $ch;
  }

  return $head;
}

# For * and + lists 
# S2 -> S2 X         { push @$_[1] the node associated with X; $_[1] }
# S2 -> /* empty */  { a node with empty children }
sub YYActionforT_TX1X2 {
  goto &YYActionforT_TX1X2_tree if $_[0]->YYBuildingTree;

  my $self = shift;
  my $head = shift;

  push @$head, @_;
  return $head;
}

sub YYActionforParenthesis {
  goto &YYBuildAST if $_[0]->YYBuildingTree;

  my $self = shift;

  return [ @_ ];
}


sub YYActionforT_empty_tree {
  my $self = shift;
  my $PREFIX = $self->YYPrefix();
  my $name = $self->YYName();

  # Allow use of %name
  my $class = $PREFIX.$name;
  my $node = bless { children => [] }, $class;
  #BeANode($class);
  $node;
}

sub YYActionforT_empty {
  goto &YYActionforT_empty_tree  if $_[0]->YYBuildingTree;

  [];
}

sub YYActionforT_single_tree {
  my $self = shift;
  my $PREFIX = $self->YYPrefix();
  my $name = $self->YYName();
  my @right = $self->YYRightside();
  my $class;

  # Allow use of %name
  my @t;
  for(my $i = 0; $i < @right; $i++) {
    $_ = $right[$i];
    my $ch = $_[$i];
    if ($self->YYIssemantic($_)) {
      $class = $PREFIX.'TERMINAL';
      push @t, bless { token => $_, attr => $ch, children => [] }, $class;
      #BeANode($class);
      next;
    }
    next if $self->YYIsterm($_);
    if (ref($ch) eq  $PREFIX."_PAREN") { # Warning: weak code!!!
      push @t, @{$ch->{children}};
      next;
    }
    next unless ref($ch);
    push @t, $ch;
  }
  $class = $PREFIX.$name;
  my $node = bless { children => \@t }, $class;
  #BeANode($class);
  $node;
}

sub YYActionforT_single {
  goto &YYActionforT_single_tree  if $_[0]->YYBuildingTree;

  my $self = shift;
  [ @_ ];
}

### end Casiano methods

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
	local $/ = "\n";
	open(DRV,"<$fname") or die "Report this as a BUG: Cannot open $fname";
  local $_;
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
#DBG>			and	do { 
#DBG>       print STDERR "Need token. Got ".&$ShowCurToken."\n";
#DBG>     };
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
#DBG>		#and	print STDERR "Reduce using rule ".-$act." ($lhs,$len): "; # old Parse::Yapp line
#DBG>		and	do { my @rhs = @{$self->{GRAMMAR}->[-$act]->[2]};
#DBG>            @rhs = ( '/* empty */' ) unless @rhs;
#DBG>            my $rhs = "@rhs";
#DBG>            $rhs = substr($rhs, 0, 30).'...' if length($rhs) > 30; # chomp if too large
#DBG>            print STDERR "Reduce using rule ".-$act." ($lhs --> $rhs): "; 
#DBG>          };

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

            $self->{CURRENT_LHS} = $lhs;
            $self->{CURRENT_RULE} = -$act; # count the super-rule?
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
#DBG>			{ 
#DBG>       local $" = ", "; 
#DBG>       my @expect = map { ">$_<" } $self->YYExpect();
#DBG>       print STDERR "Expecting one of: @expect\n";
#DBG>     };
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
#DBG>		and	print STDERR "**Discard invalid token ".&$ShowCurToken.".\n";

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


} ###########End of include /home/pl/LEyapp/lib//Parse/Eyapp/Driver.pm file

{ ###########Included /home/pl/LEyapp/lib//Parse/Eyapp/Node.pm file
# (c) Parse::Eyapp Copyright 2006-2007 Casiano Rodriguez-Leon, all rights reserved.
package Parse::Eyapp::Node;
use strict;
use Carp;#use base qw(Exporter);
use List::MoreUtils qw(firstval lastval);
use List::Util qw(first);
use Data::Dumper;

#our @EXPORT_OK = qw(new);

our $FILENAME=__FILE__;

####################################################################
# Usage      : 
# line: %name PROG
#        exp <%name EXP + ';'>
#                 { @{$lhs->{t}} = map { $_->{t}} ($lhs->child(0)->children()); }
# ;
# Returns    : The array of children of the node. When the tree is a
#              translation scheme the CODE references are also included
# Parameters : the node (method)
# See Also   : Children

sub children {
  my $self = CORE::shift;
  
  return () unless UNIVERSAL::can($self, 'children');
  @{$self->{children}} = @_ if @_;
  @{$self->{children}}
}

####################################################################
# Usage      :  line: %name PROG
#                        (exp) <%name EXP + ';'>
#                          { @{$lhs->{t}} = map { $_->{t}} ($_[1]->Children()); }
#
# Returns    : The true children of the node, excluding CODE CHILDREN
# Parameters : The Node object

sub Children {
  my $self = CORE::shift;
  
  return () unless UNIVERSAL::can($self, 'children');

  @{$self->{children}} = @_ if @_;
  grep { !UNIVERSAL::isa($_, 'CODE') } @{$self->{children}}
}

####################################################################
# Returns    : Last non CODE child
# Parameters : the node object

sub Last_child {
  my $self = CORE::shift;

  return unless UNIVERSAL::can($self, 'children') and @{$self->{children}};
  my $i = -1;
  $i-- while defined($self->{children}->[$i]) and UNIVERSAL::isa($self->{children}->[$i], 'CODE');
  return  $self->{children}->[$i];
}

sub last_child {
  my $self = CORE::shift;

  return unless UNIVERSAL::can($self, 'children') and @{$self->{children}};
  ${$self->{children}}[-1];
}

####################################################################
# Usage      :  $node->child($i)
#  my $transform = Parse::Eyapp::Treeregexp->new( STRING => q{
#     commutative_add: PLUS($x, ., $y, .)
#       => { my $t = $x; $_[0]->child(0, $y); $_[0]->child(2, $t)}
#  }
# Purpose    : Setter-getter to modify a specific child of a node
# Returns    : Child with index $i. Returns undef if the child does not exists
# Parameters : Method: the node and the index of the child. The new value is used 
#              as a setter.
# Throws     : Croaks if the index parameter is not provided
sub child {
  my ($self, $index, $value) = @_;
  
  #croak "$self is not a Parse::Eyapp::Node" unless $self->isa('Parse::Eyapp::Node');
  return undef unless  UNIVERSAL::can($self, 'child');
  croak "Index not provided" unless defined($index);
  $self->{children}[$index] = $value if defined($value);
  $self->{children}[$index];
}

sub descendant {
  my $self = shift;
  my $coord = shift;

  my @pos = split /\./, $coord;
  my $t = $self;
  my $x = shift(@pos); # discard the first empty dot
  for (@pos) {
      croak "Error computing descendant: $_ is not a number\n" 
    unless m{\d+} and $_ < $t->children;
    $t = $t->child($_);
  }
  return $t;
}

####################################################################
# Usage      : $node->s(@transformationlist);
# Example    : The following example simplifies arithmetic expressions
# using method "s":
# > cat Timeszero.trg
# /* Operator "and" has higher priority than comma "," */
# whatever_times_zero: TIMES(@b, NUM($x) and { $x->{attr} == 0 }) => { $_[0] = $NUM }
#
# > treereg Timeszero
# > cat arrays.pl
#  !/usr/bin/perl -w
#  use strict;
#  use Rule6;
#  use Parse::Eyapp::Treeregexp;
#  use Timeszero;
#
#  my $parser = new Rule6();
#  my $t = $parser->Run;
#  $t->s(@Timeszero::all);
#
#
# Returns    : Nothing
# Parameters : The object (is a method) and the list of transformations to apply.
#              The list may be a list of Parse::Eyapp:YATW objects and/or CODE
#              references
# Throws     : No exceptions
# Comments   : The set of transformations is repeatedly applied to the node
#              until there are no changes.
#              The function may hang if the set of transformations
#              matches forever.
# See Also   : The "s" method for Parse::Eyapp::YATW objects 
#              (i.e. transformation objects)

sub s {
  my @patterns = @_[1..$#_];

  # Make them Parse::Eyapp:YATW objects if they are CODE references
  @patterns = map { ref($_) eq 'CODE'? 
                      Parse::Eyapp::YATW->new(
                        PATTERN => $_,
                        #PATTERN_ARGS => [],
                      )
                      :
                      $_
                  } 
                  @patterns;
  my $changes; 
  do { 
    $changes = 0;
    foreach (@patterns) {
      $_->{CHANGES} = 0;
      $_->s($_[0]);
      $changes += $_->{CHANGES};
    }
  } while ($changes);
}


####################################################################
# Usage      : ????
# Purpose    : bud = Bottom Up Decoration: Decorates the tree with flowers :-)
#              The purpose is to decorate the AST with attributes during
#              the context-dependent analysis, mainly type-checking.
# Returns    : ????
# Parameters : The transformations.
# Throws     : no exceptions
# Comments   : The tree is traversed bottom-up. The set of
#              transformations is applied to each node in the order
#              supplied by the user. As soon as one succeeds
#              no more transformations are applied.
# See Also   : n/a
# To Do      : Avoid closure. Save @patterns inside the object
{
  my @patterns;

  sub bud {
    @patterns = @_[1..$#_];

    @patterns = map { ref($_) eq 'CODE'? 
                        Parse::Eyapp::YATW->new(
                          PATTERN => $_,
                          #PATTERN_ARGS => [],
                        )
                        :
                        $_
                    } 
                    @patterns;
    _bud($_[0], undef, undef);
  }

  sub _bud {
    my $node = $_[0];
    my $index = $_[2];

      # Is an odd leaf. Not actually a Parse::Eyapp::Node. Decorate it and leave
      if (!ref($node) or !UNIVERSAL::can($node, "children"))  {
        for my $p (@patterns) {
          return if $p->pattern->(
            $_[0],  # Node being visited  
            $_[1],  # Father of this node
            $index, # Index of this node in @Father->children
            $p,  # The YATW pattern object   
          );
        }
      };

      # Recursively decorate subtrees
      my $i = 0;
      for (@{$node->{children}}) {
        $_->_bud($_, $_[0], $i);
        $i++;
      }

      # Decorate the node
      #Change YATW object to be the  first argument?
      for my $p (@patterns) {
        return if $p->pattern->($_[0], $_[1], $index, $p); 
      }
  }
} # closure for @patterns

####################################################################
# Usage      : 
# @t = Parse::Eyapp::Node->new( q{TIMES(NUM(TERMINAL), NUM(TERMINAL))}, 
#      sub { 
#        our ($TIMES, @NUM, @TERMINAL);
#        $TIMES->{type}       = "binary operation"; 
#        $NUM[0]->{type}      = "int"; 
#        $NUM[1]->{type}      = "float"; 
#        $TERMINAL[1]->{attr} = 3.5; 
#      },
#    );
# Purpose    : Multi-Constructor
# Returns    : Array of pointers to the objects created
#              in scalar context a pointer to the first node
# Parameters : The class plus the string description and attribute handler

{

my %cache;

  sub m_bless {

    my $key = join "",@_;
    my $class = shift;
    return $cache{$key} if exists $cache{$key};

    my $b = bless { children => \@_}, $class;
    $cache{$key} = $b;

    return $b;
  }
}

sub _bless {
  my $class = shift;

  my $b = bless { children => \@_ }, $class;
  return $b;
}

sub hexpand {
  my $class = CORE::shift;

  my $handler = CORE::pop if ref($_[-1]) eq 'CODE';
  my $n = m_bless(@_);

  my $newnodeclass = CORE::shift;

  no strict 'refs';
  push @{$newnodeclass."::ISA"}, 'Parse::Eyapp::Node' unless $newnodeclass->isa('Parse::Eyapp::Node');

  if (defined($handler) and UNIVERSAL::isa($handler, "CODE")) {
    $handler->($n);
  }

  $n;
}

sub hnew {
  my $blesser = \&m_bless;

  return _new($blesser, @_);
}

# Regexp for a full Perl identifier
sub _new {
  my $blesser = CORE::shift;
  my $class = CORE::shift;
  local $_ = CORE::shift; # string: tree description
  my $handler = CORE::shift if ref($_[0]) eq 'CODE';


  my %classes;
  my $b;
  #TODO: Shall I receive a prefix?

  my (@stack, @index, @results, %results, @place, $open);
  while ($_) {
    #skip white spaces
    s{\A\s+}{};

    # If is a leaf is followed by parenthesis or comma or an ID
    s{\A([A-Za-z_][A-Za-z0-9_:]*)\s*([),])} 
     {$1()$2} # ... then add an empty pair of parenthesis
      and do { 
        next; 
       };

    # If is a leaf is followed by an ID
    s{\A([A-Za-z_][A-Za-z0-9_:]*)\s+([A-Za-z_])} 
     {$1()$2} # ... then add an empty pair of parenthesis
      and do { 
        next; 
       };

    # If is a leaf at the end
    s{\A([A-Za-z_][A-Za-z0-9_:]*)\s*$} 
     {$1()} # ... then add an empty pair of parenthesis
      and do { 
        $classes{$1} = 1;
        next; 
       };

    # Is an identifier
    s{\A([A-Za-z_][A-Za-z0-9_:]*)}{} 
      and do { 
        $classes{$1} = 1;
        CORE::push @stack, $1; 
        next; 
      };

    # Open parenthesis: mark the position for when parenthesis closes
    s{\A[(]}{} 
      and do { 
        my $pos = scalar(@stack);
        CORE::push @index, $pos; 
        $place[$pos] = $open++;

        # Warning! I don't know what I am doing
        next;
      };

    # Skip commas
    s{\A,}{} and next; 

    # Closing parenthesis: time to build a node
    s{\A[)]}{} and do { 
        croak "Syntax error! Closing parenthesis has no left partner!" unless @index;
        my $begin = pop @index; # check if empty!
        my @children = splice(@stack, $begin);
        my $class = pop @stack;
        croak "Syntax error! Any couple of parenthesis must be preceded by an identifier"
          unless (defined($class) and $class =~ m{^[a-zA-Z_][\w:]*$});

        $b = $blesser->($class, @children);

        CORE::push @stack, $b;
        $results[$place[$begin]] = $b;
        CORE::push @{$results{$class}}, $b;
        next; 
    }; 

    croak "Error building Parse::Eyapp::Node tree at '$_'." unless s{\A\s+}{};
  } # while
  croak "Syntax error! Open parenthesis has no right partner!" if @index;
  { 
    no strict 'refs';
    for (keys(%classes)) {
      push @{$_."::ISA"}, 'Parse::Eyapp::Node' unless $_->isa('Parse::Eyapp::Node');
    }
  }
  if (defined($handler) and UNIVERSAL::isa($handler, "CODE")) {
    $handler->(@results);
  }
  return wantarray? @results : $b;
}

sub new {
  my $blesser = \&_bless;

  _new($blesser, @_);
}

## Used by _subtree_list
#sub compute_hierarchy {
#  my @results = @{shift()};
#
#  # Compute the hierarchy
#  my $b;
#  my @r = @results;
#  while (@results) {
#    $b = pop @results;
#    my $d = $b->{depth};
#    my $f = lastval { $_->{depth} < $d} @results;
#    
#    $b->{father} = $f;
#    $b->{children} = [];
#    unshift @{$f->{children}}, $b;
#  }
#  $_->{father} = undef for @results;
#  bless $_, "Parse::Eyapp::Node::Match" for @r;
#  return  @r;
#}

# Matches

sub m {
  my $self = shift;
  my @patterns = @_ or croak "Expected a pattern!";
  croak "Error in method m of Parse::Eyapp::Node. Expected Parse::Eyapp:YATW patterns"
    unless $a = first { !UNIVERSAL::isa($_, "Parse::Eyapp:YATW") } @_;

  # array context: return all matches
  local $a = 0;
  my %index = map { ("$_", $a++) } @patterns;
  my @stack = (
    Parse::Eyapp::Node::Match->new( 
       node => $self, 
       depth => 0,  
       dewey => "", 
       patterns =>[] 
    ) 
  );
  my @results;
  do {
    my $mn = CORE::shift(@stack);
    my %n = %$mn;

    # See what patterns do match the current $node
    for my $pattern (@patterns) {
      push @{$mn->{patterns}}, $index{$pattern} if $pattern->{PATTERN}($n{node});
    } 
    my $dewey = $n{dewey};
    if (@{$mn->{patterns}}) {
      $mn->{family} = \@patterns;

      # Is at this time that I have to compute the father
      my $f = lastval { $dewey =~ m{^$_->{dewey}}} @results;
      $mn->{father} = $f;
      # ... and children
      push @{$f->{children}}, $mn if defined($f);
      CORE::push @results, $mn;
    }
    my $childdepth = $n{depth}+1;
    my $k = -1;
    CORE::unshift @stack, 
          map 
            { 
              $k++; 
              Parse::Eyapp::Node::Match->new(
                node => $_, 
                depth => $childdepth, 
                dewey => "$dewey.$k", 
                patterns => [] 
              ) 
            } $n{node}->children();
  } while (@stack);

  wantarray? @results : $results[0];
}

#sub _subtree_scalar {
#  # scalar context: return iterator
#  my $self = CORE::shift;
#  my @patterns = @_ or croak "Expected a pattern!";
#
#  # %index gives the index of $p in @patterns
#  local $a = 0;
#  my %index = map { ("$_", $a++) } @patterns;
#
#  my @stack = ();
#  my $mn = { node => $self, depth => 0, patterns =>[] };
#  my @results = ();
#
#  return sub {
#     do {
#       # See if current $node matches some patterns
#       my $d = $mn->{depth};
#       my $childdepth = $d+1;
#       # See what patterns do match the current $node
#       for my $pattern (@patterns) {
#         push @{$mn->{patterns}}, $index{$pattern} if $pattern->{PATTERN}($mn->{node});
#       } 
#
#       if (@{$mn->{patterns}}) { # matched
#         CORE::push @results, $mn;
#
#         # Compute the hierarchy
#         my $f = lastval { $_->{depth} < $d} @results;
#         $mn->{father} = $f;
#         $mn->{children} = [];
#         $mn->{family} = \@patterns;
#         unshift @{$f->{children}}, $mn if defined($f);
#         bless $mn, "Parse::Eyapp::Node::Match";
#
#         # push children in the stack
#         CORE::unshift @stack, 
#                   map { { node => $_, depth => $childdepth, patterns => [] } } 
#                                                       $mn->{node}->children();
#         $mn = CORE::shift(@stack);
#         return $results[-1];
#       }
#       # didn't match: push children in the stack
#       CORE::unshift @stack, 
#                  map { { node => $_, depth => $childdepth, patterns => [] } } 
#                                                      $mn->{node}->children();
#       $mn = CORE::shift(@stack);
#     } while ($mn); # May be the stack is empty now, but if $mn then there is a node to process
#     # reset iterator
#     my @stack = ();
#     my $mn = { node => $self, depth => 0, patterns =>[] };
#     return undef;
#   };
#}

# Factorize this!!!!!!!!!!!!!!
#sub m {
#  goto &_subtree_list if (wantarray()); 
#  goto &_subtree_scalar;
#}

####################################################################
# Usage      :   $BLOCK->delete($ASSIGN)
#                $BLOCK->delete(2)
# Purpose    : deletes the specified child of the node
# Returns    : The deleted child
# Parameters : The object plus the index or pointer to the child to be deleted
# Throws     : If the object can't do children or has no children
# See Also   : n/a

sub delete {
  my $self = CORE::shift; # The tree object
  my $child = CORE::shift; # index or pointer

  croak "Parse::Eyapp::Node::delete error, node:\n"
        .Parse::Eyapp::Node::str($self)."\ndoes not have children" 
    unless UNIVERSAL::can($self, 'children') and ($self->children()>0);
  if (ref($child)) {
    my $i = 0;
    for ($self->children()) {
      last if $_ == $child;
      $i++;
    }
    if ($i == $self->children()) {
      warn "Parse::Eyapp::Node::delete warning: node:\n".Parse::Eyapp::Node::str($self)
           ."\ndoes not have a child like:\n"
           .Parse::Eyapp::Node::str($child)
           ."\nThe node was not deleted!\n";
      return $child;
    }
    splice(@{$self->{children}}, $i, 1);
    return $child;
  }
  my $numchildren = $self->children();
  croak "Parse::Eyapp::Node::delete error: expected an index between 0 and ".
        ($numchildren-1).". Got $child" unless ($child =~ /\d+/ and $child < $numchildren);
  splice(@{$self->{children}}, $child, 1);
  return $child;
}

####################################################################
# Usage      : $BLOCK->shift
# Purpose    : deletes the first child of the node
# Returns    : The deleted child
# Parameters : The object 
# Throws     : If the object can't do children 
# See Also   : n/a

sub shift {
  my $self = CORE::shift; # The tree object

  croak "Parse::Eyapp::Node::shift error, node:\n"
       .Parse::Eyapp::Node->str($self)."\ndoes not have children" 
    unless UNIVERSAL::can($self, 'children');

  return CORE::shift(@{$self->{children}});
}

sub unshift {
  my $self = CORE::shift; # The tree object
  my $node = CORE::shift; # node to insert

  CORE::unshift @{$self->{children}}, $node;
}

sub push {
  my $self = CORE::shift; # The tree object
  #my $node = CORE::shift; # node to insert

  #CORE::push @{$self->{children}}, $node;
  CORE::push @{$self->{children}}, @_;
}

sub insert_before {
  my $self = CORE::shift; # The tree object
  my $child = CORE::shift; # index or pointer
  my $node = CORE::shift; # node to insert

  croak "Parse::Eyapp::Node::insert_before error, node:\n"
        .Parse::Eyapp::Node::str($self)."\ndoes not have children" 
    unless UNIVERSAL::can($self, 'children') and ($self->children()>0);

  if (ref($child)) {
    my $i = 0;
    for ($self->children()) {
      last if $_ == $child;
      $i++;
    }
    if ($i == $self->children()) {
      warn "Parse::Eyapp::Node::insert_before warning: node:\n"
           .Parse::Eyapp::Node::str($self)
           ."\ndoes not have a child like:\n"
           .Parse::Eyapp::Node::str($child)."\nThe node was not inserted!\n";
      return $child;
    }
    splice(@{$self->{children}}, $i, 0, $node);
    return $node;
  }
  my $numchildren = $self->children();
  croak "Parse::Eyapp::Node::insert_before error: expected an index between 0 and ".
        ($numchildren-1).". Got $child" unless ($child =~ /\d+/ and $child < $numchildren);
  splice(@{$self->{children}}, $child, 0, $node);
  return $child;
}

sub insert_after {
  my $self = CORE::shift; # The tree object
  my $child = CORE::shift; # index or pointer
  my $node = CORE::shift; # node to insert

  croak "Parse::Eyapp::Node::insert_after error, node:\n"
        .Parse::Eyapp::Node::str($self)."\ndoes not have children" 
    unless UNIVERSAL::can($self, 'children') and ($self->children()>0);

  if (ref($child)) {
    my $i = 0;
    for ($self->children()) {
      last if $_ == $child;
      $i++;
    }
    if ($i == $self->children()) {
      warn "Parse::Eyapp::Node::insert_after warning: node:\n"
           .Parse::Eyapp::Node::str($self).
           "\ndoes not have a child like:\n"
           .Parse::Eyapp::Node::str($child)."\nThe node was not inserted!\n";
      return $child;
    }
    splice(@{$self->{children}}, $i+1, 0, $node);
    return $node;
  }
  my $numchildren = $self->children();
  croak "Parse::Eyapp::Node::insert_after error: expected an index between 0 and ".
        ($numchildren-1).". Got $child" unless ($child =~ /\d+/ and $child < $numchildren);
  splice(@{$self->{children}}, $child+1, 0, $node);
  return $child;
}

{ # $match closure

  my $match;

  sub clean_tree {
    $match = pop;
    croak "clean tree: a node and code reference expected" unless (ref($match) eq 'CODE') and (@_ > 0);
    $_[0]->_clean_tree();
  }

  sub _clean_tree {
    my @children;
    
    for ($_[0]->children()) {
      next if (!defined($_) or $match->($_));
      
      $_->_clean_tree();
      CORE::push @children, $_;
    }
    $_[0]->{children} = \@children; # Bad code
  }
} # $match closure

####################################################################
# Usage      : $t->str 
# Returns    : Returns a string describing the Parse::Eyapp::Node as a term
#              i.e., s.t. like: 'PROGRAM(FUNCTION(RETURN(TERMINAL,VAR(TERMINAL))))'
our @PREFIXES = qw(Parse::Eyapp::Node::);
our $INDENT = 0; # -1 new 0 = compact, 1 = indent, 2 = indent and include Types in closing parenthesis
our $STRSEP = ',';
our $DELIMITER = '[';
our $FOOTNOTE_HEADER = "\n---------------------------\n";
our $FOOTNOTE_SEP = ")\n";
our $FOOTNOTE_LEFT = '^{';
our $FOOTNOTE_RIGHT = '}';
our $LINESEP = 4;
our $CLASS_HANDLER = sub { type($_[0]) }; # What to print to identify the node

my %match_del = (
  '[' => ']',
  '{' => '}',
  '(' => ')',
  '<' => '>'
);

my $pair;
my $footnotes = '';
my $footnote_label;

sub str {

  my @terms;

  # Consume arg only if called as a class method Parse::Eyap::Node->str($node1, $node2, ...)
  CORE::shift unless ref($_[0]);

  for (@_) {
    $footnote_label = 0;
    $footnotes = '';
    # Set delimiters for semantic values
    if (defined($DELIMITER) and exists($match_del{$DELIMITER})) {
      $pair = $match_del{$DELIMITER};
    }
    else {
      $DELIMITER = $pair = '';
    }
    CORE::push @terms,  _str($_).$footnotes;
  }
  return wantarray? @terms : $terms[0];
}  

sub _str {
  my $self = CORE::shift;          # root of the subtree
  my $indent = (CORE::shift or 0); # current depth in spaces " "

  my @children = Parse::Eyapp::Node::children($self);
  my @t;

  my $res;
  my $fn = $footnote_label;
  if ($INDENT >= 0 && UNIVERSAL::can($self, 'footnote')) {
    $res = $self->footnote; 
    $footnotes .= $FOOTNOTE_HEADER.$footnote_label++.$FOOTNOTE_SEP.$res if $res;
  }

  # recursively visit nodes
  for (@children) {
    CORE::push @t, Parse::Eyapp::Node::_str($_, $indent+2) if defined($_);
  }
  local $" = $STRSEP;
  my $class = $CLASS_HANDLER->($self);
  $class =~ s/^$_// for @PREFIXES; 
  my $information;
  $information = $self->info if ($INDENT >= 0 && UNIVERSAL::can($self, 'info'));
  $class .= $DELIMITER.$information.$pair if $information;
  if ($INDENT >= 0 &&  $res) {
   $class .= $FOOTNOTE_LEFT.$fn.$FOOTNOTE_RIGHT;
  }

  if ($INDENT > 0) {
    my $w = " "x$indent;
    $class = "\n$w$class";
    $class .= "(@t\n$w)" if @children;
    $class .= " # ".$CLASS_HANDLER->($self) if ($INDENT > 1) and ($class =~ tr/\n/\n/>$LINESEP);
  }
  else {
    $class .= "(@t)" if @children;
  }
  return $class;
}

#use overload q{""} => \&stringify;

sub translation_scheme {
  my $self = CORE::shift; # root of the subtree
  my @children = $self->children();
  for (@children) {
    if (ref($_) eq 'CODE') {
      $_->($self, $self->Children);
    }
    elsif (defined($_)) {
      translation_scheme($_);
    }
  }
}

sub type {
 my $type = ref($_[0]);

 if ($type) {
   if (defined($_[1])) {
     $type = $_[1];
     Parse::Eyapp::Driver::BeANode($type);
     bless $_[0], $type;
   }
   return $type 
 }
 return 'Parse::Eyapp::Node::STRING';
}

{ # Tree "fuzzy" equality

####################################################################
# Usage      : $t1->equal($t2, n => sub { return $_[0] == $_[1] })
# Purpose    : Checks the equality between two AST
# Returns    : 1 if equal, 0 if not 'equal'
# Parameters : Two Parse::Eyapp:Node nodes and a hash of comparison handlers.
#              The keys of the hash are the attributes of the nodes. The value is
#              a comparator function. The comparator for key $k receives the attribute
#              for the nodes being visited and rmust return true if they are considered similar
# Throws     : exceptions if the parameters aren't Parse::Eyapp::Nodes

  my %handler;

  # True if the two trees look similar
  sub equal {
    croak "Parse::Eyapp::Node::equal error. Expected two syntax trees \n" unless (@_ > 1);

    %handler = splice(@_, 2);
    my $key = '';
    defined($key=firstval {!UNIVERSAL::isa($handler{$_},'CODE') } keys %handler) 
    and 
      croak "Parse::Eyapp::Node::equal error. Expected a CODE ref for attribute $key\n";
    goto &_equal;
  }

  sub _equal {
    my $tree1 = CORE::shift;
    my $tree2 = CORE::shift;

    # Same type
    return 0 unless ref($tree1) eq ref($tree2);

    # Check attributes via handlers
    for (keys %handler) {
      # Check for existence
      return 0 if (exists($tree1->{$_}) && !exists($tree2->{$_}));
      return 0 if (exists($tree2->{$_}) && !exists($tree1->{$_}));

      # Check for definition
      return 0 if (defined($tree1->{$_}) && !defined($tree2->{$_}));
      return 0 if (defined($tree2->{$_}) && !defined($tree1->{$_}));

      # Check for equality
      return 0 unless $handler{$_}->($tree1->{$_}, $tree2->{$_});
    }

    # Same number of children
    my @children1 = @{$tree1->{children}};
    my @children2 = @{$tree2->{children}};
    return 0 unless @children1 == @children2;

    # Children must be similar
    for (@children1) {
      my $ch2 = CORE::shift @children2;
      return 0 unless _equal($_, $ch2);
    }
    return 1;
  }
}

1;

package Parse::Eyapp::Node::Match;
our @ISA = qw(Parse::Eyapp::Node);

# A Parse::Eyapp::Node::Match object is a reference
# to a tree of Parse::Eyapp::Nodes that has been used
# in a tree matching regexp. You can think of them
# as the equivalent of $1 $2, ... in treeregexeps

# The depth of the Parse::Eyapp::Node being referenced

sub new {
  my $class = shift;

  my $matchnode = { @_ };
  $matchnode->{children} = [];
  bless $matchnode, $class;
}

sub depth {
  my $self = shift;

  return $self->{depth};
}

# The coordinates of the Parse::Eyapp::Node being referenced
sub coord {
  my $self = shift;

  return $self->{dewey};
}


# The Parse::Eyapp::Node being referenced
sub node {
  my $self = shift;

  return $self->{node};
}

# The Parse::Eyapp::Node:Match that references
# the nearest ancestor of $self->{node} that matched
sub father {
  my $self = shift;

  return $self->{father};
}
  
# The patterns that matched with $self->{node}
# Indexes
sub patterns {
  my $self = shift;

  @{$self->{patterns}} = @_ if @_;
  return @{$self->{patterns}};
}
  
# The original list of patterns that produced this match
sub family {
  my $self = shift;

  @{$self->{family}} = @_ if @_;
  return @{$self->{family}};
}
  
# The names of the patterns that matched
sub names {
  my $self = shift;

  my @indexes = $self->patterns;
  my @family = $self->family;

  return map { $_->{NAME} or "Unknown" } @family[@indexes];
}
  
sub info {
  my $self = shift;

  my $node = $self->node;
  my @names = $self->names;
  my $nodeinfo;
  if (UNIVERSAL::can($node, 'info')) {
    $nodeinfo = ":".$node->info;
  }
  else {
    $nodeinfo = "";
  }
  return "[".ref($self->node).":".$self->depth.":@names$nodeinfo]"
}

1;



} ###########End of include /home/pl/LEyapp/lib//Parse/Eyapp/Node.pm file

{ ###########Included /home/pl/LEyapp/lib//Parse/Eyapp/YATW.pm file
# (c) Parse::Eyapp Copyright 2006 Casiano Rodriguez-Leon, all rights reserved.
package Parse::Eyapp::YATW;
use strict;
use warnings;
use Carp;
use Data::Dumper;
use List::Util qw(first);
use List::MoreUtils qw(lastval);
#use Parse::Eyapp::Base qw( valid_keys invalid_keys );

sub valid_keys {
  my %valid_args = @_;

  my @valid_args = keys(%valid_args); 
  local $" = ", "; 
  return "@valid_args" 
}

sub invalid_keys {
  my $valid_args = shift;
  my $args = shift;

  return (first { !exists($valid_args->{$_}) } keys(%$args));
}


our $VERSION = $Parse::Eyapp::Driver::VERSION;

our $FILENAME=__FILE__;

# TODO: Check args. Typical args:
# 'CHANGES' => 0,
# 'PATTERN' => sub { "DUMMY" },
# 'NAME' => 'fold',
# 'PATTERN_ARGS' => [],
# 'PENDING_TASKS' => {},
# 'NODE' => []

my %_new_yatw = (
  PATTERN => 'CODE',
  NAME => 'STRING',
);

my $validkeys = valid_keys(%_new_yatw); 

sub new {
  my $class = shift;
  my %args = @_;

  croak "Error. Expected a code reference when building a tree walker. " unless (ref($args{PATTERN}) eq 'CODE');
  if (defined($a = invalid_keys(\%_new_yatw, \%args))) {
    croak("Parse::Eyapp::YATW::new Error!: unknown argument $a. Valid arguments are: $validkeys")
  }


  # obsolete, I have to delete this
  #$args{PATTERN_ARGS} = [] unless (ref($args{PATTERN_ARGS}) eq 'ARRAY'); 

  # Internal fields

  # Tell us if the node has changed after the visit
  $args{CHANGES} = 0;
  
  # PENDING_TASKS is a queue storing the tasks waiting for a "safe time/node" to do them 
  # Usually that time occurs when visiting the father of the node who generated the job 
  # (when asap criteria is applied).
  # Keys are node references. Values are array references. Each entry defines:
  #  [ the task kind, the node where to do the job, and info related to the particular job ]
  # Example: @{$self->{PENDING_TASKS}{$father}}, ['insert_before', $node, ${$self->{NODE}}[0] ];
  $args{PENDING_TASKS} = {};

  # NODE is a stack storing the ancestor of the node being visited
  # Example: my $ancestor = ${$self->{NODE}}[$k]; when k=1 is the father, k=2 the grandfather, etc.
  # Example: CORE::unshift @{$self->{NODE}}, $_[0]; Finished the visit so take it out
  $args{NODE} = [];

  bless \%args, $class;
}

sub buildpatterns {
  my $class = shift;
  
  my @family;
  while (my ($n, $p) = splice(@_, 0,2)) {
    push @family, Parse::Eyapp::YATW->new(NAME => $n, PATTERN => $p);
  }
  return wantarray? @family : $family[0];
}

####################################################################
# Usage      : @r = $b{$_}->m($t)
#              See Simple4.eyp and m_yatw.pl in the examples directory
# Returns    : Returns an array of nodes matching the treeregexp
#              The set of nodes is a Parse::Eyapp::Node::Match tree 
#              showing the relation between the matches
# Parameters : The tree (and the object of course)
# depth is no longer used: eliminate
sub m {
  my $p = shift(); # pattern YATW object
  my $t = shift;   # tree
  my $pattern = $p->{PATTERN}; # CODE ref

  # References to the found nodes are stored in @stack
  my @stack = ( Parse::Eyapp::Node::Match->new(node=>$t, depth=>0, dewey => "") ); 
  my @results;
  do {
    my $n = CORE::shift(@stack);
    my %n = %$n;

    my $dewey = $n->{dewey};
    my $d = $n->{depth};
    if ($pattern->($n{node})) {
      $n->{family} = [ $p ];
      $n->{patterns} = [ 0 ];

      # Is at this time that I have to compute the father
      my $f = lastval { $dewey =~ m{^$_->{dewey}}} @results;
      $n->{father} = $f;
      # ... and children
      push @{$f->{children}}, $n if defined($f);
      push @results, $n;
    }
    my $k = 0;
    CORE::unshift @stack, 
       map { 
              local $a;
              $a = Parse::Eyapp::Node::Match->new(node=>$_, depth=>$d+1, dewey=>"$dewey.$k" );
              $k++;
              $a;
           } $n{node}->children();
  } while (@stack);

  return wantarray? @results : $results[0];
}

######################### getter-setter for YATW objects ###########################

sub pattern {
  my $self = shift;
  $self->{PATTERN} = shift if (@_);
  return $self->{PATTERN};
}

sub name {
  my $self = shift;
  $self->{NAME} = shift if (@_);
  return $self->{NAME};
}

#sub pattern_args {
#  my $self = shift;
#
#  $self->{PATTERN_ARGS} = @_ if @_;
#  return @{$self->{PATTERN_ARGS}};
#}

########################## PENDING TASKS management ################################

# Purpose    : Deletes the node that matched from the list of children of its father. 
sub delete {
  my $self = shift;

  bless $self->{NODE}[0], 'Parse::Eyapp::Node::DELETE';
}
  
sub make_delete_effective {
  my $self = shift;
  my $node = shift;

  my $i = -1+$node->children;
  while ($i >= 0) {
    if (UNIVERSAL::isa($node->child($i), 'Parse::Eyapp::Node::DELETE')) {
      $self->{CHANGES}++ if splice @{$node->{children}}, $i, 1;
    }
    $i--;
  }
}

####################################################################
# Usage      :    my $b = Parse::Eyapp::Node->new( 'NUM(TERMINAL)', sub { $_[1]->{attr} = 4 });
#                 $yatw_pattern->unshift($b); 
# Parameters : YATW object, node to insert, 
#              ancestor offset: 0 = root of the tree that matched, 1 = father, 2 = granfather, etc.

sub unshift {
  my ($self, $node, $k) = @_;
  $k = 1 unless defined($k); # father by default

  my $ancestor = ${$self->{NODE}}[$k];
  croak "unshift: does not exist ancestor $k of node ".Dumper(${$self->{NODE}}[0]) unless defined($ancestor);

  # Stringification of $ancestor. Hope it works
                                            # operation, node to insert, 
  push @{$self->{PENDING_TASKS}{$ancestor}}, ['unshift', $node ];
}

sub insert_before {
  my ($self, $node) = @_;

  my $father = ${$self->{NODE}}[1];
  croak "insert_before: does not exist father of node ".Dumper(${$self->{NODE}}[0]) unless defined($father);

                                           # operation, node to insert, before this node 
  push @{$self->{PENDING_TASKS}{$father}}, ['insert_before', $node, ${$self->{NODE}}[0] ];
}

sub _delayed_insert_before {
  my ($father, $node, $before) = @_;

  my $i = 0;
  for ($father->children()) {
    last if ($_ == $before);
    $i++;
  }
  splice @{$father->{children}}, $i, 0, $node;
}

sub do_pending_tasks {
  my $self = shift;
  my $node = shift;

  my $mytasks = $self->{PENDING_TASKS}{$node};
  while ($mytasks and (my $job = shift @{$mytasks})) {
    my @args = @$job;
    my $task = shift @args;

    # change this for a jump table
    if ($task eq 'unshift') {
      CORE::unshift(@{$node->{children}}, @args);
      $self->{CHANGES}++;
    }
    elsif ($task eq 'insert_before') {
      _delayed_insert_before($node, @args);
      $self->{CHANGES}++;
    }
  }
}

####################################################################
# Parameters : pattern, node, father of the node, index of the child in the children array
# YATW object. Probably too many 
sub s {
  my $self = shift;
  my $node = $_[0] or croak("Error. Method __PACKAGE__::s requires a node");
  CORE::unshift @{$self->{NODE}}, $_[0];
  # father is $_[1]
  my $index = $_[2];

  # If is not a reference or can't children then simply check the matching and leave
  if (!ref($node) or !UNIVERSAL::can($node, "children"))  {
                                         
    $self->{CHANGES}++ if $self->pattern->(
      $_[0],  # Node being visited  
      $_[1],  # Father of this node
      $index, # Index of this node in @Father->children
      $self,  # The YATW pattern object   
    );
    return;
  };
  
  # Else, is not a leaf and is a regular Parse::Eyapp::Node
  # Recursively transform subtrees
  my $i = 0;
  for (@{$node->{children}}) {
    $self->s($_, $_[0], $i);
    $i++;
  }
  
  my $number_of_changes = $self->{CHANGES};
  # Now is safe to delete children nodes that are no longer needed
  $self->make_delete_effective($node);

  # Safely do pending jobs for this node
  $self->do_pending_tasks($node);

  #node , father, childindex, and ... 
  #Change YATW object to be the  first argument?
  if ($self->pattern->($_[0], $_[1], $index, $self)) {
    $self->{CHANGES}++;
  }
  shift @{$self->{NODE}};
}

1;


} ###########End of include /home/pl/LEyapp/lib//Parse/Eyapp/YATW.pm file


#line 1 "Gift.yp"


#
#Note for the Authors: Look at: /home/webs/pcgull/moodle/datos/12/cuestionario in zion

# The standalone parser in this module was built using the Parse::Eyapp 
# distribution (available from CPAN). 
#
# eyapp -s -m Gift Gift.yp
# perl -c Gift.pm

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


#line 2206 Gift.pm

my $warnmessage =<< "EOFWARN";
Warning!: Did you changed the \@Gift::ISA variable inside the header section of the eyapp program?
EOFWARN

sub new {
        my($class)=shift;
        ref($class)
    and $class=ref($class);

    warn $warnmessage unless __PACKAGE__->isa('Parse::Eyapp::Driver'); 
    my($self)=$class->SUPER::new( yyversion => '1.109',
                                  yyGRAMMAR  =>
[
  [ _SUPERSTART => '$start', [ 'gift', '$end' ], 0 ],
  [ gift_1 => 'gift', [ 'questions' ], 0 ],
  [ questions_2 => 'questions', [ 'questions', 'QUESTIONSEP', 'question' ], 0 ],
  [ questions_3 => 'questions', [ 'question' ], 0 ],
  [ question_4 => 'question', [  ], 0 ],
  [ question_5 => 'question', [ 'PRESTATE', '{', 'answers', '}', 'poststate' ], 0 ],
  [ answers_6 => 'answers', [ 'answers', 'ANSWER' ], 0 ],
  [ answers_7 => 'answers', [ 'ANSWER' ], 0 ],
  [ poststate_8 => 'poststate', [  ], 0 ],
  [ poststate_9 => 'poststate', [ 'POSTSTATE' ], 0 ],
],
                                  yyTERMS  =>
{ '$end' => 0, '{' => 0, '}' => 0, ANSWER => 1, POSTSTATE => 1, PRESTATE => 1, QUESTIONSEP => 1 },
                                  yyFILENAME  => "Gift.yp",
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
	[#Rule _SUPERSTART
		 '$start', 2, undef
#line 2322 Gift.pm
	],
	[#Rule gift_1
		 'gift', 1,
sub {
#line 89 "Gift.yp"
 $_[1]; }
#line 2329 Gift.pm
	],
	[#Rule questions_2
		 'questions', 3,
sub {
#line 94 "Gift.yp"
 push(@{$_[1]}, $_[3]) if defined($_[3]); $_[1] }
#line 2336 Gift.pm
	],
	[#Rule questions_3
		 'questions', 1,
sub {
#line 95 "Gift.yp"
 defined($_[1])? [ $_[1] ] : [] }
#line 2343 Gift.pm
	],
	[#Rule question_4
		 'question', 0,
sub {
#line 97 "Gift.yp"
 undef }
#line 2350 Gift.pm
	],
	[#Rule question_5
		 'question', 5,
sub {
#line 102 "Gift.yp"
 $_[0]->build_question($_[1], $_[3], $_[5]); }
#line 2357 Gift.pm
	],
	[#Rule answers_6
		 'answers', 2,
sub {
#line 105 "Gift.yp"
 
                          push @{$_[1]}, $_[2]; $_[1]  
                        }
#line 2366 Gift.pm
	],
	[#Rule answers_7
		 'answers', 1,
sub {
#line 108 "Gift.yp"
 
                          [ $_[1] ] 
                        }
#line 2375 Gift.pm
	],
	[#Rule poststate_8
		 'poststate', 0,
sub {
#line 113 "Gift.yp"
 '' }
#line 2382 Gift.pm
	],
	[#Rule poststate_9
		 'poststate', 1, undef
#line 2386 Gift.pm
	]
],
#line 2389 Gift.pm
                                  yybypass => 0,
                                  yybuildingtree => 0,
                                  @_,);
    bless($self,$class);

    $self->make_node_classes( qw{TERMINAL _OPTIONAL _STAR_LIST _PLUS_LIST 
         _SUPERSTART
         gift_1
         questions_2
         questions_3
         question_4
         question_5
         answers_6
         answers_7
         poststate_8
         poststate_9} );
    $self;
}

#line 116 "Gift.yp"


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
  my $prefix = Gift::Question->unescape_gift_metasymbols($6);
  return ('PRESTATE', { NAME => $2, FORMAT => $5, PREFIX => $prefix });
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
      my $suffix = Gift::Question->unescape_gift_metasymbols($1);
      $lineno += &countlines($suffix);
      &trim_end($suffix);
      $post_state = 0;
      return ('POSTSTATE', $suffix );
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

  die "Error: The name of a gift file must be provided\n" unless defined($file);
  open FILE, $file or die "Can't open file $file\n";
  {
    local $/ = undef;
    $input = <FILE>;
  }
  close(FILE);
  die "Error: Got nothing from $file\n" unless defined($input);

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
  return $self->PRESTATE->{PREFIX};
}

sub FORMAT {
  my $self = shift;

  $self->PRESTATE->{FORMAT} = $_[0] if defined($_[0]);
  return $self->PRESTATE->{FORMAT};
}

sub NAME {
  my $self = shift;

  $self->PRESTATE->PRESTATE->{NAME} = $_[0] if defined($_[0]);
  return $self->{NAME};
}

sub ANSWERS {
  my $self = shift;
  my @args = @_;

  if (defined($_[0]) and ref($_[0])) {
    $self->{ANSWERS} = $_[0] 
  }
  elsif (defined($_[0])) { # not a reference
    $self->{ANSWERS} = \@args 
  }
  return $self->{ANSWERS}
}

sub POSTSTATE {
  my $self = shift;

  $self->{POSTSTATE} = $_[0] if defined($_[0]);
  return $self->{POSTSTATE}
}

sub unescape_gift_metasymbols {
  my $self = shift;
  my $str = shift;

  $str =~ s/\\=/=/g;
  $str =~ s/\\~/~/g;
  $str =~ s/\\#/#/g;
  $str =~ s/\\{/{/g;
  $str =~ s/\\}/}/g;
  $str =~ s/\\\\}/\\/g;
  return $str;
}

sub Error {
  my $message = shift;

  die "$message\n";
}

package Gift::TRUEFALSE;
our @ISA = ('Gift::Question');

sub CORRECT_ANSWER {
  my $self = shift;
  $self->ANSWERS->[0]->{ANSWER} = $_[0] if defined($_[0]);
  return $self->ANSWERS->[0]->{ANSWER};
}

package Gift::MULTIPLECHOICE;
our @ISA = ('Gift::Question');

# returns a reference to the array of answer strings
sub ANSWERS {
  my $self = shift;

  my $answers = $self->SUPER::ANSWERS();
  my @ANSWERS = map { $_->{ANSWER} } @$answers;
  return \@ANSWERS;
}

sub CORRECT_ANSWER { # Returns the index of the correct answer
  my $self = shift;

  my $i = 0;
  for (@{$self->SUPER::ANSWERS}) {
    return $i if $_->{TYPE} eq 'RIGHT'; # there is only one right answer
    $i++;
  }
}

package Gift::SHORTANSWER;
our @ISA = ('Gift::Question');

# returns the array of answer strings
sub CORRECT_ANSWERS {
  my $self = shift;
  my @CORRECT_ANSWERS;
  my $answers = $self->SUPER::ANSWERS();

  for (@$answers) {
    my ($answer, $weight) = ($_->{ANSWER}, $_->{WEIGHT});

    next if (defined($_->{WEIGHT}) and ($_->{WEIGHT} < 100));
    push @CORRECT_ANSWERS, $answer;
  }
  return \@CORRECT_ANSWERS;
}

sub LENGTH_SHORTANSWER {
  my $self = shift;
  my $ml = 0;

  for  (@{$self->CORRECT_ANSWERS()}) {
    $ml = length if length > $ml;
  }
  return $ml;  
}

package Gift::MATCH;
our @ISA = ('Gift::Question');


sub RANDOM_ANSWERS {
  my $self = shift;

  my $answers = $self->SUPER::ANSWERS();
  my @FIRST = map { $_->{FIRST} } @$answers;
  my @SECOND = map { $_->{SECOND} } @$answers;
  my @RANDOM_ANSWERS = ();

  my $k = 0;
  while (@SECOND) {
    my $r = int(rand(@SECOND));
    my $s = splice @SECOND, $r, 1;
    push @RANDOM_ANSWERS, [$FIRST[$k++], $s];
  }
  return \@RANDOM_ANSWERS;
}

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
tilde. The following is a short answer question:

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

The class provide the method C<ANSWER> which gives you access to
get or set the C<ANSWER> entry to the only one hash item in the
C<ANSWERS> array.

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

#line 3556 Gift.pm

1;

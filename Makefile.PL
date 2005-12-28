use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME              => 'Gift',
    VERSION           => '0.6', # Not to crash with $Parse::Yapp::VERSION in the standalone driver
    PREREQ_PM         => {}, 
    #PREREQ_PM         => {Parse::Yapp => 0}, # not needed: parser driver is standalone 
    EXE_FILES         => [ 'script/gift' ],
    ($] >= 5.005 ?     ## Add these new keywords supported since 5.005
      (ABSTRACT       => 'Parser for Moodle GIFT format for quizs',
       AUTHOR         => 'Casiano Rodriguez Leon casiano@ull.es') : ()),
);

use ExtUtils::MakeMaker;

WriteMakefile(
    NAME              => 'Unix::PID',
    VERSION_FROM      => 'PID.pm', 
    PREREQ_PM         => { 
                             'Class::Std'           => 0,
                             'Class::Std::Utils'    => 0,
                             'version'              => 0,
                             'Math::Fibonacci::Phi' => 0,
                             'List::Cycle'          => 0,
                             'Time::HiRes'          => 0,
                         },
    ($] >= 5.005 ?    
      (ABSTRACT_FROM  => 'PID.pm',
       AUTHOR         => 'Daniel Muey <http://drmuey.com/cpan_contact.pl>') : ()),
);

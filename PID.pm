package Unix::PID;

use strict;
use warnings;
our $VERSION = '0.19';
our $AUTOLOAD;

use IPC::Open3;

sub import {
    shift;
    my $file = defined $_[0] && $_[0] !~ m{ \A \d+ \. \d+ \. \d+ \z }xms ? shift : '';

    #### handle use Mod '1.2.3'; here? make it play nice with version.pm ?? ##
    #    my $want = shift;
    #
    #    if(defined $want && $want !~ m{^\d+\.\d+\.\d+$}) {
    #        require Carp;
    #        Carp::croak "Unix::PID is version $VERSION, you requested $want"
    #            if Unix::PID->VERSION < version->new($want)->numify();
    #    }
    #### ???? ##

    if ( defined $file && $file ne '' ) {
        require Carp;
        Unix::PID->new()->pid_file($file)
          or Carp::croak "The PID in $file is still running.";
    }
}

sub new {
    my ( $class, $args_ref ) = @_;
    $args_ref = {} if ref($args_ref) ne 'HASH';
    my $self = bless(
        {
            'ps_path' => '',
            'errstr'  => '',
            'minimum_pid' => !exists $args_ref->{'minimum_pid'} || $args_ref->{'minimum_pid'} !~ m{\A\d+\z}ms ? 11 : $args_ref->{'minimum_pid'},
        },
        $class
    );

    $self->set_ps_path( $args_ref->{'ps_path'} ) if exists $args_ref->{'ps_path'};

    return $self;
}

sub get_ps_path {
    return $_[0]->{'ps_path'};
}

sub get_errstr {
    return $_[0]->{'errstr'};
}

sub non_blocking_wait {
    my ($self) = @_;
    while ( ( my $zombie = waitpid( -1, 1 ) ) > 0 ) { }
}

sub set_ps_path {
    my ( $self, $path ) = @_;
    $path = substr( $path, 0, ( length($path) - 1 ) )
      if substr( $path, -1, 1 ) eq '/';
    if ( ( -d $path && -x "$path/ps" ) || $path eq '' ) {
        $self->{'ps_path'} = $path;
        return 1;
    }
    else {
        return;
    }
}

sub get_pidof {
    my ( $self, $name, $exact ) = @_;
    my %map;
    for ( $self->_raw_ps( 'axo', 'pid,command' ) ) {
        $_ =~ s{ \A \s* | \s* \z }{}xmsg;
        my ( $pid, $cmd ) = $_ =~ m{ \A (\d+) \s+ (.*) \z }xmsg;
        $map{$pid} = $cmd if $pid && $pid ne $$ && $cmd;
    }
    my @pids =
      $exact
      ? grep { $map{$_} =~ m/^\Q$name\E$/ } keys %map
      : grep { $map{$_} =~ m/\Q$name\E/ } keys %map;

    return wantarray ? @pids : $pids[0];
}

sub kill {
    my ( $self, $pid ) = @_;
    $pid = int $pid;
    my $min = int $self->{'minimum_pid'};
    if ( $pid < $min ) {

        # prevent bad args from killing the process group (IE '0')
        # or general low level ones
        warn "kill() called with integer value less than $min";
        return;
    }
    
    # CORE::kill 0, $pid : may be false but still running, see `perldoc -f kill`
    if ( $self->is_pid_running($pid) ) {

        # RC from CORE::kill is not a boolean of if the PID was killed or not, only that it was signaled
        # so it is not an indicator of "success" in killing $pid
        CORE::kill( 15, $pid );    # TERM
        CORE::kill( 2,  $pid );    # INT
        CORE::kill( 1,  $pid );    # HUP
        CORE::kill( 9,  $pid );    # KILL
        return if $self->is_pid_running($pid);
    }
    return 1;
}

sub get_pid_from_pidfile {
    my ( $self, $pid_file ) = @_;
    
    return 0 if !-e $pid_file;
    
    open my $pid_fh, '<', $pid_file or return;
    chomp( my $pid = <$pid_fh> );
    close $pid_fh;
    
    return int(abs($pid));
}

sub is_pidfile_running {
    my ( $self, $pid_file ) = @_;
    my $pid = $self->get_pid_from_pidfile($pid_file) || return;
    return $pid if $self->is_pid_running($pid);
    return;
}

sub pid_file {
    my ( $self, $pid_file, $newpid, $retry_conf ) = @_;
    $newpid = $$ if !$newpid;

    my $rc = $self->pid_file_no_unlink( $pid_file, $newpid, $retry_conf );
    if ($rc && $newpid == $$) {
        eval 'END { unlink $pid_file; }';
    }

    return 1 if $rc == 1;
    return 0 if $rc == 0;
    return;
}

sub pid_file_no_unlink {
    my ( $self, $pid_file, $newpid, $retry_conf ) = @_;
    $newpid = $$ if !$newpid;

    if ( ref($retry_conf) eq 'ARRAY' ) {
        $retry_conf->[0] = int( abs( $retry_conf->[0] ) );
        for my $idx ( 1 .. scalar( @{$retry_conf} ) - 1 ) {
            next if ref $retry_conf->[$idx] eq 'CODE';
            $retry_conf->[$idx] = int( abs( $retry_conf->[$idx] ) );
        }
    }
    else {
        $retry_conf = [ 3, 1, 2 ];
    }

    my $passes = 0;
    require Fcntl;

  EXISTS:
    $passes++;
    if ( -e $pid_file ) {

        my $curpid = $self->get_pid_from_pidfile($pid_file);

        # TODO: narrow even more the race condition where $curpid stops running and a new PID is put in
        # the file between when we pull in $curpid above and check to see if it is running/unlink below

        return 1 if int $curpid == $$ && $newpid == $$;    # already setup
        return if int $curpid == $$;                       # can't change it while $$ is alive
        return if $self->is_pid_running( int $curpid );

        unlink $pid_file;                                  # must be a stale PID file, so try to remove it for sysopen()
    }

    # write only if it does not exist:
    sysopen( my $pid_fh, $pid_file, Fcntl::O_WRONLY() | Fcntl::O_EXCL() | Fcntl::O_CREAT() ) || do {
        return 0 if $passes >= $retry_conf->[0];
        if ( ref( $retry_conf->[$passes] ) eq 'CODE' ) {
            $retry_conf->[$passes]->( $self, $pid_file, $passes );
        }
        else {
            sleep( $retry_conf->[$passes] ) if $retry_conf->[$passes];
        }
        goto EXISTS;
    };

    print {$pid_fh} int( abs($newpid) );
    close $pid_fh;

    return 1;
}

sub kill_pid_file {
    my ( $self, $pidfile ) = @_;
    my $rc = $self->kill_pid_file_no_unlink($pidfile);
    if ( $rc && -e $pidfile ) {
        unlink $pidfile or return -1;
    }
    return $rc;
}

sub kill_pid_file_no_unlink {
    my ( $self, $pidfile ) = @_;
    if ( -e $pidfile ) {
        my $pid = $self->get_pid_from_pidfile($pidfile);
        $self->kill($pid) or return;
        return $pid;
    }
    return 1;
}

sub is_running {
    my ( $self, $check_this, $exact ) = @_;
    return $self->is_pid_running($check_this) if $check_this =~ m{ \A d+ \z }xms;
    return $self->is_command_running( $check_this, $exact );
}

sub pid_info {
    my ( $self, $pid ) = @_;
    my @outp = $self->_pid_info_raw($pid);
    return wantarray ? split( /\s+/, $outp[1], 11 ) : [ split( /\s+/, $outp[1], 11 ) ];
}

sub pid_info_hash {
    my ( $self, $pid ) = @_;
    my @outp = $self->_pid_info_raw($pid);
    my %info;
    @info{ split( /\s+/, $outp[0], 11 ) } = split( /\s+/, $outp[1], 11 );
    return wantarray ? %info : \%info;
}

sub _pid_info_raw {
    my ( $self, $pid ) = @_;
    my @info = $self->_raw_ps( 'u', '-p', $pid );
    chomp @info;
    return wantarray ? @info : \@info;
}

sub is_pid_running {
    my ( $self, $check_pid ) = @_;
    return 1 if $> == 0 && CORE::kill(0, $check_pid); # if we are superuser we can avoid the the system call. For details see `perldoc -f kill`

    # even if we are superuser, go ahead and call ps just in case CORE::kill 0's false RC was erroneous
    my $info = ( $self->_pid_info_raw($check_pid) )[1];
    return 1 if defined $info;
    return;
}

sub is_command_running {
    my ( $self, $check_command, $exact ) = @_;
    return scalar $self->get_pidof( $check_command, $exact ) ? 1 : 0;
}

sub wait_for_pidsof {
    my ( $self, $wait_ref ) = @_;

    $wait_ref->{'get_pidof'} = $self->get_command($$) if !$wait_ref->{'get_pidof'};
    $wait_ref->{'max_loops'} = 5
      if !defined $wait_ref->{'max_loops'}
          || $wait_ref->{'max_loops'} !~ m{ \A \d+ \z }xms;

    $wait_ref->{'hit_max_loops'} = sub {
        die 'Hit max loops in wait_for_pidsof()';
      }
      if ref $wait_ref->{'hit_max_loops'} ne 'CODE';

    my @got_pids;
    if ( ref $wait_ref->{'pid_list'} eq 'ARRAY' ) {
        @got_pids = grep { defined } map { $self->is_pid_running($_) ? $_ : undef } @{ $wait_ref->{'pid_list'} };
    }
    else {
        @got_pids = $self->get_pidof( $wait_ref->{'get_pidof'} );
    }

    if ( $wait_ref->{'use_hires_usleep'} || $wait_ref->{'use_hires_nanosleep'} ) {
        require Time::HiRes;
    }

    my $lcy = '';
    my $fib = '';
    if ( ref $wait_ref->{'sleep_for'} ) {
        if ( ref $wait_ref->{'sleep_for'} eq 'ARRAY' ) {
            require List::Cycle;
            $lcy = List::Cycle->new( { 'values' => $wait_ref->{'sleep_for'} } );
        }
        if ( $wait_ref->{'sleep_for'} eq 'HASH' ) {
            if ( exists $wait_ref->{'sleep_for'}->{'fibonacci'} ) {
                require Math::Fibonacci::Phi;
                $fib = 1;
            }
        }
    }
    $wait_ref->{'sleep_for'} = 60 if !defined $wait_ref->{'sleep_for'};

    my $loop_cnt = 0;

    while ( scalar @got_pids ) {
        $loop_cnt++;

        $wait_ref->{'pre_sleep'}->( $loop_cnt, \@got_pids )
          if ref $wait_ref->{'pre_sleep'} eq 'CODE';

        my $period =
            $lcy ? $lcy->next()
          : $fib ? Math::Fibonacci::term($loop_cnt)
          :        $wait_ref->{'sleep_for'};

        if ( $wait_ref->{'use_hires_nanosleep'} ) {
            Time::HiRes::nanosleep($period);
        }
        elsif ( $wait_ref->{'use_hires_usleep'} ) {
            Time::HiRes::usleep($period);
        }
        else {
            sleep $period;
        }

        if ( ref $wait_ref->{'pid_list'} eq 'ARRAY' ) {
            @got_pids = grep { defined } map { $self->is_pid_running($_) ? $_ : undef } @{ $wait_ref->{'pid_list'} };
        }
        else {
            @got_pids = $self->get_pidof( $wait_ref->{'get_pidof'} );
        }

        if ( $loop_cnt >= $wait_ref->{'max_loops'} ) {
            $wait_ref->{'hit_max_loops'}->( $loop_cnt, \@got_pids );
            last;
        }
    }
}

sub _raw_ps {
    my ( $self, @ps_args ) = @_;
    my $path = $self->get_ps_path();
    $self->{'errstr'} = '';

    if ( !$path ) {
        for (
            qw( /usr/local/bin /usr/local/sbin
            /usr/bin /usr/sbin
            /bin      /sbin
            )
          ) {
            if ( -x "$_/ps" ) {
                $self->set_ps_path($_);
                $path = $self->get_ps_path();
                last;
            }
        }
    }

    my $ps = $path ? "$path/ps" : 'ps';
    local $SIG{'CHLD'} = 'IGNORE';
    my $pid = open3( my $in_fh, my $out_fh, my $err_fh, $ps, @ps_args );
    my @out = <$out_fh>;
    $self->{'errstr'} = join '', <$err_fh> if defined $err_fh;
    close $in_fh;
    close $out_fh;
    close $err_fh if defined $err_fh;
    waitpid( $pid, 0 );

    return wantarray ? @out : join '', @out;
}

sub AUTOLOAD {
    my ( $self, $pid ) = @_;

    my $subname = $AUTOLOAD . '=';
    $subname =~ s/.*:://;
    $subname =~ s{\A get\_ }{}xms;

    my $data = $self->_raw_ps( '-p', $pid, '-o', $subname );
    $data =~ s{ \A \s* | \s* \z }{}xmsg;
    return $data;
}

1;

__END__

=head1 NAME

Unix::PID - Perl extension for getting PID info.

=head1 SYNOPSIS

   use Unix::PID;
   my $pid = Unix::PID->new();

or specify where ps is at:

   my $pid = Unix::PID->new({ 'ps_path' => '/usr/util/bin' });

or simplify pid file use by:

   use Unix::PID '/var/run/this.pid';

Aside from the obvious run time vs. compile time factor, this is *exactly* the same as doing

   use Unix::PID;
   Unix::PID->new()->pid_file('/var/run/this.pid') or die 'The PID in /var/run/this.pid is still running.';

So the "use Unix::PID 'pidfile';" will simplify 99% of the times you'd use $pid->pid_file();

=head1 METHODS

=head2 Unix::PID->new()

Get a Unix::PID object. 

It takes an optional hashref with the following, optional, keys:

=over 4

=item 'minimum_pid'

The minimum PID that can be kill()ed. If not given or not all digits then it defaults to 11.

=item 'ps_path'

The path to the 'ps' binary you want to use. The value gets passed to the object's set_ps_path().

=back

=head2 $pid->set_ps_path()

Set the path where ps is at. If not set via this method or in new() or previously then _raw_ps() looks for it in several common places and sets it to that if it finds it.
returns true if what you specify is ok and false otherwise.

    $pid->set_ps_path('/usr/util/bin');

=head2 $pid->get_ps_path()

Get the path that the object thinks ps is at
   
    print 'I am using ps at ' . $pid->get_ps_path();

=head2 $pid->get_pidof()

Gets the pid(s) matching the given command. In scalar context returns the first, in array returns all.

The first arg is a string to match against running PIDs, the second, option arg, if true makes it match exactly as given.

    $pid->get_pidof("waldo --foo");

returns pids whose commands contains "waldo --foo" (IE would match 'bin/waldo --foo')

    $pids->get_pidof('waldo --foo', 1);

returns pids whose commands are *entirely* 'waldo --foo' (IE would not match 'bin/waldo --foo')

So for instance to see all processes that are exactly the same as the current:

   $pids->get_pidof( $pids->get_command($$), 1 );

To see in script.pl how many others of itself are running under force:

   $pids->get_pidof('script.pl --force');

The current script's PID (IE $$) is never included in this output.

=head2 $pid->pid_info( $pid )

Get an array (or array ref in scalar context ) of $pid's USER, PID, %CPU, %MEM, VSZ, RSS, TT, STAT, STARTED, TIME, COMMAND

This may vary on your system so check the header of 'ps u -p NUMERIC_PID_HERE' on your system.

=head2 $pid->pid_info_hash()

Same info as pid_info except you get a hash (or hashref in scalar context) with the keys: USER, PID, %CPU, %MEM, VSZ, RSS, TT, STAT, STARTED, TIME, COMMAND and values that correspond to each one.

This may vary on your system so check the header of 'ps u -p NUMERIC_PID_HERE' on your system.

=head2 $pid->is_*running

Check if a pid or command is running or not, returns 1 or 0

=head3 $pid->is_running()

If the first argument is all digits then this it calls $pid->is_pid_running for you. Otherwise it calls $pid->is_command_running() for you.

=head3 $pid->is_pid_running()

    if($pid->is_pid_running($miscpid)) {
        warn "PID $miscpid is running, you had better go catch it";
    } 

=head3 $pid->is_pidfile_running()

Takes one argument, the pid file whose PID you're interested in.

Returns the numeric pid stored in the given pid file if it is running, otherwise it return;s

=head3 $pid->is_command_running()

    if($pid->is_comand_running($cmd)) {
        warn "$cmd is still going strong";
    }

If the second argument is true it acts just like get_pidof()

=head2 $pid->pid_file()

Takes three arguments, the first is the pid file, the second, optional, argument is the pid to write to the file (defaults to $$), the third, also optional, argument is "retry" configuration described below.

If the pid file exists it checks to see if the pid in it is running and if so it returns undef, if not it writes the second argument (or $$) to the file and returns 1.

It returns 0 if the pid file read or write open() fails. (IE you could use $! in your "or whatever")

    # make sure this only runs one at a time 
    Unix::PID->new()->pid_file('/var/run/this.pid') or die 'This is already running';

Upon success it also sets up and END block to remove the file if the PID we setup was our PID.

The "retry" configuration mentioned above is a reference to an array. The first item is the number of times to "retry" processing of an existing pid file. The additonal arguments are what to do after each pass (except the last pass which returns false afterward). The index corresponds to the pass number. e.g. $ar->[1] is what to do after the first pass, $ar->[2] is what to do after the second pass, and so on.

The value can be a number, in which case it sleep()s that many seconds, or a code ref. The code ref is passed the Unix::PID object as thre first argument, pid file in question as the second argument and the number of passes thus far as the third.

The default "retry" configuration is [3,1,2].

=head2 $pid->pid_file_no_unlink()

Just like $pid->pid_file() except no END block cleanup is setup. Useful for doing pid files for a sporking daemon.

=head2 $pid->get_pid_from_pidfile()

Takes one argument, the pid file whose PID you want. 

Returns the pid stored in the given pid file, 0 if the pid file does not exist or the contents are not numeric. return;s on failure to open the existing pid file.

=head2 $pid->kill_pid_file()

Takes one argument, the pid file whose PID you want kill()ed. It unlinks the pid file after its successful run.

It returns true (if the file exists, the pid. otherwise 1) if all is well, 0 if it exists but could not be opened, undef if the pid could not be killed, and -1 if it could not be cleaned up after it was successfully killed.

=head2 $pid->kill_pid_file_no_unlink();

Just like $pid->kill_pid_file() but the pid file is not unlink()ed. (and it likewise does not return -1)

=head2 $pid->kill()

Takes one argument, the PID to kill. If its running it first tries kill 1 and if that fails it tries kill 9.

Returns undef if the PID was running and could not be killed, true if its not running or was killed successfully.

    $pid->kill( $mypid ) or warn "Could not kill PID $mypid: $!";

=head2 $pid->non_blocking_wait()

Does a non-blocking wait for all pending zombie processes

=head2 $pid->wait_for_pidsof()

This function waits for processes matching your criteria to finish before going on.

Its single argument is a hash ref whose keys are the following:

=over

=item pid_list

An array ref of numeric PIDs to wait on. If this exists and is an array ref it will be used instead of get_pidof

=item get_pidof

The value is the same as you'd pass to $pid->get_pidof, defaults to $pid->get_command($$) to wait for process that have the exact same command to stop.

=item sleep_for

Number of seconds to sleep between checking on the pids. defaults to 60

If an array ref is passed the sleep time cycles through this list.

If a hashref is sent, and it has a key of the value 'fibonacci' each cycle uses the next fibonnaci number as the time to sleep, starting with the first.

See L<Math::Fibonacci::Phi>

=item use_hires_usleep

If true Time::HiRes::usleep() is used instead of sleep() so that you can have it sleep (via "sleep_for") for fractions of seconds in microseconds.

See L<Time::HiRes>

=item use_hires_nanosleep

If true Time::HiRes::nanosleep() is used instead of sleep() so that you can have it sleep (via "sleep_for") for fractions of seconds in nanoseconds.

See L<Time::HiRes>

If both use_hires_nanosleep and use_hires_usleep are true use_hires_nanosleep is used.

=item max_loops

number of times to check before giving up, defaults to 5

=item pre_sleep

An optional code reference to do before it sleeps, this would be useful to let everyone know whats going on.

@_ contains the number of the loop you're in and an array ref of the pids you're currently waiting on

=item hit_max_loops

A code reference to do once you've looped 'max_loops' times. By default it die()'s. I purposefully die()ed instead of croak to encourage you to specify it so you can handle it properly according to your needs.

@_ contains the number of the loop you're in and an array ref of the pids you're currently waiting on

=back

For clarity and maintainability I highly recommend specifying each option so you (and the poor souls who have to maintain your code later) will have some sort of idea what you were trying for instead having to guess and hack away deeper into code, 

Here is a complete example:

    # do some initialization stuff

    $pid->wait_for_pidsof(
        {
            'get_pidof'     => 'deepthought --get-ultimate-answer',
            'sleep_for'     => 31_556_926, # check once a year
            'max_loops'     => 7_500_000,  # for 7.5 million years
            'pre_sleep'     => sub {
                my($we_are_in_loop_number, $waiting_on_these_pids_ref) = @_;
                print "Currently in year $we_are_in_loop_number waiting for deepthought: " 
                      . join ',', @{ $waiting_on_these_pids_ref };
            },
            'hit_max_loops' => sub {
                my($we_are_in_loop_number, $waiting_on_these_pids_ref) = @_;
                croak "Sorry mice, even after $we_are_in_loop_number years deepthought is still just watching TV: "
                      . join ',', @{ $waiting_on_these_pids_ref };
            },
        }
    );

    # continue on now that the answer has been had :)

Obviously its not efficient to have it sleep for a year between checks 7.5 million times just to calculate 42, its just an example :)

=head2 $pid->get_errstr();

Any errors from _raw_ps can be fetched from $pid->get_errstr()

The "or die $pid->get_errstr()" paradigm doesn't work because any undef or otherwise "false" values do not necessarily indicate an error.

So you can do:

   die $pid->get_errstr() if $pid->get_errstr();

=head2 $pid->get_*

There are get_ functions for each \w+ that can be passed to ps's -o option, 

Each one takes a pid as the only argument.

    my $i_am_command = $pid->get_command($$);

=head2 $pid->_raw_ps(@ps_args);

Calls ps with the given args and returns an array of each line (or the first line in scalar context).

=head1 TO DO

I'd be happy to add additional functionality if it belongs here, just drop me a line :)

=head1 AUTHOR

Daniel Muey, L<http://drmuey.com/cpan_contact.pl>

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Daniel Muey

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself. 

=cut

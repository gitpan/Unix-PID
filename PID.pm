package Unix::PID;

use strict;
use warnings;
use version;our $VERSION = qv('0.0.9');

use IPC::Open3;
use Class::Std;
use Class::Std::Utils;

sub import {
    shift;
    my $file = defined $_[0] && $_[0] !~ m{^\d+\.\d+\.\d+$} ? shift : '';

    #### handle use Mod '1.2.3'; here? make it play nice with version.pm ?? ##
    #    my $want = shift;
    #
    #    if(defined $want && $want !~ m{^\d+\.\d+\.\d+$}) {
    #        require Carp;
    #        Carp::croak "Unix::PID is version $VERSION, you requested $want"
    #            if Unix::PID->VERSION < version->new($want)->numify();
    #    } 
    #### ???? ##

    if(defined $file && $file ne '') {
        require Carp;
        Unix::PID->new()->pid_file($file) 
            or Carp::croak "The PID in $file is still running.";
    }
}

{
    
    my %ps_path :ATTR('get' => 'ps_path', 'init_arg' => 'ps_path', 'default' => '');
    my %errstr  :ATTR('get' => 'errstr'); 

    sub set_ps_path {
        my($self, $path) = @_;
        $path = substr($path, 0, (length($path) - 1)) 
            if substr($path, -1, 1) eq '/';
        if( (-d $path && -x "$path/ps") || $path eq '') {
            $ps_path{ ident $self } = $path;
            return 1;
        }       
        else {
            return;
        }
    }

    sub get_pidof {
        my($self, $name, $exact) = @_;
        my %map;
        for( $self->_raw_ps('axo', 'pid,command') ) {
            $_ =~ s{ \A \s* | \s* \z }{}xmsg;
            my($pid, $cmd) = $_ =~ m{ \A (\d+)\s+(.*) \z }xmsg;
            $map{ $pid } = $cmd if $pid && $pid ne $$ && $cmd;
        }
        my @pids = $exact ? grep { $map{$_} =~ m/^\Q$name\E$/  } keys %map 
                          : grep { $map{$_} =~ m/\Q$name\E/  } keys %map;

        return wantarray ? @pids : $pids[0];
    }

    sub kill {
        my($self, $pid) = @_;
        $pid = int $pid;
        if(kill 0, $pid) {
            if(!kill 1, $pid) {
                kill 9, $pid or return;
            }
        }   
        return 1; 
    }
    
    sub pid_file {
        my($self, $pid_file, $newpid) = @_;
        eval 'END { unlink $pid_file; }';
        return $self->pid_file_no_unlink($pid_file, $newpid);
    }
    
    sub pid_file_no_unlink {
        my($self, $pid_file, $newpid) = @_;
        $newpid = $$ if !$newpid;
       
        if(-e $pid_file) {
            open my $oldpid_fh, '<', $pid_file or return 0;
            chomp(my $curpid = <$oldpid_fh>);
            close $oldpid_fh;
            return 1 if int $curpid == $$ && $newpid == $$; # already setup
            return if   int $curpid == $$; # can't change it while $$ is alive
            return if $self->is_pid_running(int $curpid); 
        }
        
        open my $pid_fh, '>', $pid_file or return 0;
        print {$pid_fh} int $newpid;
        close $pid_fh;

        return 1;
    }

    sub kill_pid_file {
        my($self, $pidfile) = @_;
        my $rc = $self->kill_pid_file_no_unlink($pidfile);
        if($rc) {
            unlink $pidfile or return -1;
        }
        return $rc;
    }
    
    sub kill_pid_file_no_unlink {
        my($self, $pidfile) = @_;
        if(-e $pidfile) {
            open my $pid_fh, '<', $pidfile or return 0;
            chomp(my $pid = <$pid_fh>);
            close $pid_fh;
            $self->kill($pid) or return;
            return $pid;
        }
        return 1;
    }
    
    sub is_running {
        my($self, $check_this, $exact) = @_;
        return $self->is_pid_running($check_this) if $check_this =~ m/^d+$/;
        return $self->is_command_running($check_this, $exact);
    }

    sub is_pid_running {
        my($self, $check_pid) = @_;
        my %pids;
        @pids{ $self->get_pidof( $self->get_command($check_pid) ) } = ();
        return exists $pids{ $check_pid } ? 1 : 0;
    }

    sub is_command_running {
        my($self, $check_command, $exact) = @_;
        return scalar $self->get_pidof($check_command, $exact) ? 1 : 0;
    }

    sub wait_for_pidsof {
        my ($self, $wait_ref) = @_;

        $wait_ref->{'get_pidof'} = $self->get_command($$) 
            if !$wait_ref->{'get_pidof'};
        $wait_ref->{'sleep_for'} = 60 if !defined $wait_ref->{'sleep_for'} 
                                   || $wait_ref->{'sleep_for'} !~ m/^\d+$/;
        $wait_ref->{'max_loops'} = 5  if !defined $wait_ref->{'max_loops'}
                                   || $wait_ref->{'max_loops'} !~ m/^\d+$/;

        $wait_ref->{'hit_max_loops'} = sub {
            die 'Hit max loops in wait_for_pidsof()';            
        } if ref $wait_ref->{'hit_max_loops'} ne 'CODE';
 
        my @got_pids = $self->get_pidof( $wait_ref->{'get_pidof'} );
        my $loop_cnt = 0;
       
        while(scalar @got_pids) {
            $loop_cnt++;

            $wait_ref->{'pre_sleep'}->($loop_cnt, \@got_pids) 
                if ref $wait_ref->{'pre_sleep'} eq 'CODE';

            sleep $wait_ref->{'sleep_for'};
            @got_pids = $self->get_pidof( $wait_ref->{'get_pidof'} );
 
            $wait_ref->{'hit_max_loops'}->($loop_cnt, \@got_pids)
                if $loop_cnt >= $wait_ref->{'max_loops'};
        }
    }

    sub _raw_ps {
        my($self, @ps_args) = @_;
        my $path = $self->get_ps_path();
        $errstr{ ident $self } = '';

        if(!$path) {
            for( qw( /bin      /sbin          /usr/bin 
                     /usr/sbin /usr/local/bin /usr/local/sbin) ) {
                if(-x "$_/ps") {
                    $self->set_ps_path($_);
                    $path = $self->get_ps_path();
                    last;
                }
            }
        }

        my $ps = $path ? "$path/ps" : 'ps';
        my $pid = open3(my $in_fh, my $out_fh, my $err_fh, $ps, @ps_args);
        my @out = <$out_fh>;
        $errstr{ ident $self } = join '', <$err_fh> if defined $err_fh; 
        close $in_fh;
        close $out_fh;
        close $err_fh if defined $err_fh;

        return wantarray ? @out : join '', @out;
    }

    sub AUTOMETHOD {
        my($self, $ident, $pid) = @_;

        my $subname = $_ . '=';
        $subname =~ s{\A get\_ }{}xms;

        my $data = $self->_raw_ps('-p', $pid, '-o', $subname);
        $data    =~ s{ \A \s* | \s* \z }{}xmsg;
        return sub { return $data };
    }
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

This is *exactly* the same as:

   use Unix::PID;
   Unix::PID->new()->pid_file('/var/run/this.pid') or die 'The PID in /var/run/this.pid is still running.';

So the "use Unix::PID 'pidfile';" will simplify 99% of the times you'd use $pid->pid_file();

=head1 METHODS

=head2 $pid->set_ps_path()

Set the path where ps is at. If not set here or in new() or previously then _raw_ps() looks for it in several common places and sets it to that if it finds it.
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

=head2 $pid->is_*running

Check if a pid or command is running or not, returns 1 or 0

=head3 $pid->is_running()

If the first argument is all digits then this it calls $pid->is_pid_running for you. Otherwise it calls $pid->is_command_running() for you.

=head3 $pid->is_pid_running()

    if($pid->is_pid_running($miscpid)) {
        warn "PID $miscpid is running, you had better go catch it";
    } 

=head3 $pid->is_command_running()

    if($pid->is_comand_running($cmd)) {
        warn "$cmd is still goign strong";
    }

If the second argument is true it acts just like get_pidof()

=head2 $pid->pid_file()

Takes two arguments, the first is the pid file, the second, optional, argument is the pid to write to the file: defaults to $$.

If the pidfile exists it checks to see if the pid in it is running and if so it returns undef, if not it writes the second argument (or $$) to the file and returns 1.

It returns 0 if the pid file read or write open() fails. (IE you could use $! in your "or whatever")

    # make sure this only runs one at a time 
    Unix::PID->new()->pid_file('/var/run/this.pid') or die 'This is already running';

Also sets up and END block to remove file.

=head2 $pid->pid_file_no_unlink()

Just like $pid->pid_file() except no END block cleanup is setup. Useful for doing pid files for a sporking daemon.

=head2 $pid->kill_pid_file()

Takes one argument, the pidfile whose PID you want kill()ed. It unlinks the pidfile after its successful run.

It returns true (if the file exists, the pid. otherwise 1) if all is well, 0 if it exists but could not be opened, undef if the pid could not be killed, and -1 if it could not be cleaned up after it was successfully killed.

=head2 $pid->kill_pid_file_no_unlink();

Just like $pid->kill_pid_file() but the pid file is not unlink()ed. (and it likewise does not return -1)

=head2 $pid->kill()

Takes one argument, the PID to kill. If its running it first tries kill 1 and if that fails it tries kill 9.

Returns undef if the PID was running and could not be killed, true if its not running or was killed successfully.

    $pid->kill( $mypid ) or warn "Could not kill PID $mypid: $!";

=head2 $pid->wait_for_pidsof()

This function waits for processes matching your criteria to finish before going on.

Its single argument is a hash ref whose keys are the following:

=over

=item get_pidof

The value is the same as you'd pass to $pid->get_pidof, defaults to $pid->get_command($$) to wait for process that have the exact same command to stop.

=item sleep_for

number of seconds to sleep between checking on the pids. defaults to 60

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

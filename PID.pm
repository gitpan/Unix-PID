package Unix::PID;

use strict;
use warnings;
use version;our $VERSION = qv('0.0.1');

use IPC::Open3;
use Class::Std;
use Class::Std::Utils;

{
    
    my %ps_path :ATTR('get' => 'ps_path', 'init_arg' => 'ps_path', 'default' => '');
    my %errstr  :ATTR('get' => 'errstr'); 

    sub set_ps_path {
        my($self, $path) = @_;
        $path = substr($path, 0, (length($path) - 1)) if substr($path, -1, 1) eq '/';
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
        my @pids = $exact ? grep { $map{$_} =~ m/^\Q$name\E$/  } keys %map : grep { $map{$_} =~ m/\Q$name\E/  } keys %map;
        return wantarray ? @pids : $pids[0];
    }

    sub AUTOMETHOD {
        my($self, $ident, $pid) = @_;
        my $subname = $_ . '=';
        $subname =~ s{\A get\_ }{}xms;
        my $data = $self->_raw_ps('-p', $pid, '-o', $subname);
        $data    =~ s{ \A \s* | \s* \z }{}xmsg;
        return sub { return $data };
    }

    sub _raw_ps {

        my($self, @ps_args) = @_;
        my $path = $self->get_ps_path();
        $errstr{ ident $self } = '';

        if(!$path) {
            for( qw( /bin /sbin /usr/bin /usr/sbin /usr/local/bin /usr/local/sbin) ) {
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

} 


1;

__END__

=head1 NAME

Unix::PID - Perl extension for getting PID info.

=head1 SYNOPSIS

   use Unix::PID;
   my $pid = Unix::PID->new();

or specify where ps is at:

   my $pid = unix::PID->new({ 'ps_path' => '/usr/util/bin' });

=head1 METHODS

=head2 $pid->set_ps_path()

Set the path where ps is at. If not set here or in new() or previously then _raw_ps() looks for it in several common places and sets it to that if it finds it.
returns true if what you specify is ok and false otherwise.

    $pid->set_ps_path('/usr/util/bin');
 
=head2 $pid->get_ps_path()

Get the path that the object thinks ps is at
   
    print 'I am using ps at ' . $pid->get_ps_path();

=head2 $pid->get_pidof()

Gets the pid(s) matching the given command. In scalar contaxt returns the first, in array returns all.

The first arg is a string to match against running PIDs, the second, option arg, if true makes it match exactly as given.

    $pid->get_pidof("httpd");

returns pids whose commands contains "httpd" (Except $$ of course ;p)

    $pids->get_pidof('waldo --foo', 1);

returns pids whose commands are *exactly* 'waldo --foo'

So for instance to see all process that are exactly the same as the current:

   $pids->get_pidof( $pids->get_command($$), 1 );

To see in script.pl how many others of itself are running under force:

   $pids->get_pidof('script.pl --force');

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

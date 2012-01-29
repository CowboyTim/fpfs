#!/usr/bin/perl

use strict; use warnings;

use Fuse qw(:all);
use Errno qw(:POSIX);
use File::Basename qw(basename dirname);
use Data::Dumper;
use POSIX qw(strftime);
use Getopt::Long;
use Pod::Usage;

# cmdline option parsing
GetOptions(
    my $opts = {},
    "debug|d!",
    "manpage|m!",
    "help|?!"
) or pod2usage(1);
{
    no warnings 'redefine';
    *{debug} = sub {} unless $opts->{debug};
}
$SIG{__WARN__} = sub {debug(@_)};
pod2usage(-verbose => 0) if $opts->{usage};
pod2usage(-verbose => 2) if $opts->{manpage};
my $mountpoint = shift @ARGV
    or pod2usage(
        verbose => 1,
        msg     => "Need mountpount\n"
    );

debug('CPAN Fuse version', $Fuse::VERSION);

# some needed constants
my $S_IFIFO = 1*(2**12);
my $S_IFCHR = 2*(2**12);
my $S_IFDIR = 4*(2**12);
my $S_IFBLK = 6*(2**12);
my $S_IFREG = 2**15;
my $S_IFLNK = $S_IFREG + $S_IFCHR;

my $S_WID   = 1;    #world
my $S_GID   = 2**3; #group
my $S_UID   = 2**6; #owner
my $S_SID   = 2**9; #sticky bits etc.

# our data..
my $inode_start = 1;
my $fs_meta = {'/' => _new_meta(_mk_mode(7,5,5) + $S_IFDIR, $<, (split m/ /, $()[0], time())};
$fs_meta->{'/'}{nlink} = 2;
$fs_meta->{'/'}{directorylist} = {};

sub f_getattr {
    my ($path) = @_;
    return -Errno::ENAMETOOLONG() if length($path) > 1024;
    my ($dir, $file) = (dirname($path), basename($path)); 
    return -Ernno::ENAMETOOLONG() if length($file) > 255;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    return 0, $r->{ino}, $r->{mode}, $r->{nlink}//1, $r->{uid}, $r->{gid}, 0, $r->{size}, $r->{atime}, $r->{mtime}, $r->{ctime}, 0, 0;
}

sub f_readlink {
}

sub f_mknod {
}

sub f_mkdir {
    my ($path, $mode, $cuid, $cgid, $ctime) = @_;
    return -Errno::ENAMETOOLONG() if length($path) > 1024;
    my ($parent, $subdir) = (dirname($path), basename($path)); 
    my $m = $fs_meta->{$path} = _new_meta($mode + $S_IFDIR, $cuid, $cgid, $ctime);
    $m->{directorylist} = {};
    $m->{nlink} = 2;
    my $p = $fs_meta->{$parent};
    $p->{nlink}++;
    $p->{mtime} = $p->{ctime} = $ctime;
    $p->{directorylist}{$subdir} = $m;
    debug("returning");
    return 0;
}

sub f_rmdir {
    my ($fn) = @_;
    return -Errno::ENOENT() unless defined (my $r = delete $fs_meta->{$fn});
    return 0;
}

sub f_getdir {
    my ($path) = @_;
    return ['.', '..', keys %{($fs_meta->{$path}//{})->{directorylist}}, 0];
}

sub f_opendir {
    my ($path) = @_;
}

sub f_readdir {
    my ($path, $offset, $dir_fh) = @_;
}

sub f_releasedir {
    my ($path, $dirent) = @_;
    return 0;
}

sub f_unlink {
    my ($fn) = @_;
    return -Errno::ENOENT() unless defined (my $r = delete $fs_meta->{$fn});
    return 0;
}


sub f_symlink {
}

sub f_rename {
}

sub f_link {
}

sub _mk_mode {
    my ($owner, $group, $world, $sticky) = @_;
    return $owner * $S_UID + $group * $S_GID + $world + ($sticky // 0) * $S_SID;
}

sub _new_meta {
    my ($mymode, $uid, $gid, $now) = @_;
    return {
        mode  => $mymode,
        ino   => $inode_start++,
        uid   => $uid,
        gid   => $gid,
        size  => 0,
        atime => $now,
        mtime => $now,
        ctime => $now
    }
}

sub _db {
    my ($abbr, $f) = @_;
    sub {
        debug($abbr, 'arguments', \@_);
        my @r = &$f(@_);
        debug($abbr, 'result', @r, $fs_meta);
        wantarray ? @r : $r[0];
    };
}

sub _ctx {
    my ($m) = @_;
    sub {
        my $f_ctx = fuse_get_context();
        debug($f_ctx);
        &$m(@_, $f_ctx->{uid}, $f_ctx->{gid}, time());
    };
}

Fuse::main(
    mountpoint => $mountpoint,
    mountopts  => "allow_other,default_permissions,hard_remove,use_ino,attr_timeout=0,readdir_ino",
    debug      => $opts->{debug},
    getattr    => _db('getattr',    \&f_getattr), 
    readlink   => _db('readlink',   \&f_readlink), 
    getdir     => _db('getdir',     \&f_getdir), 
    mknod      => _db('mknod',      \&f_mknod), 
    mkdir      => _ctx(_db('mkdir', \&f_mkdir)),
);


sub debug {
    $Data::Dumper::Indent   = 1;
    $Data::Dumper::Sortkeys = 1;
    my @c = caller(0);
    my $str = join(':', strftime('%d/%m %H:%M:%S', gmtime()), ' ', $c[1], $c[3], $c[2]);
    for (split m/\n/, join(' ', map {ref($_) and Dumper($_) or $_} map {$_//'<undef>'} @_)){
        print "$str: ", $_, "\n"
    }
}


=pod

=head1 NAME

=head1 SYNOPSIS

    perl tstfuse.pl [OPTIONS] <mountpoint>

=head1 OPTIONS

=over 4

=item --manpage

Shows manpage.

=item --help|-?

Shows usage information.

=item --debug|-d

Enables debugging.

=back

=cut

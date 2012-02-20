#!/usr/bin/perl

use strict; use warnings;

use Getopt::Long;
use Pod::Usage;

# cmdline option parsing
GetOptions(
    my $opts = {},
    "debug|d!",
    "manpage|m!",
    "help|?!"
) or pod2usage(1);
pod2usage(-verbose => 0) if $opts->{usage};
pod2usage(-verbose => 2) if $opts->{manpage};
my $mountpoint = shift @ARGV
    or pod2usage(
        verbose => 1,
        msg     => "Need mountpount\n"
    );

Fuse::POSIX
    ->new(
        mountpoint => $mountpoint,
        mountopts  => "allow_other,default_permissions,hard_remove,use_ino,attr_timeout=0,entry_timeout=0,readdir_ino",
        %{$opts}
    )
    ->run();

package Fuse::POSIX;

use strict; use warnings;

use Fuse qw(:all :xattr);
use Errno qw(:POSIX);
use File::Basename qw(basename dirname);
use Data::Dumper;
use POSIX qw(strftime floor);

# some needed constants
use constant S_IFIFO => 1*(2**12);
use constant S_IFCHR => 2*(2**12);
use constant S_IFDIR => 4*(2**12);
use constant S_IFBLK => 6*(2**12);
use constant S_IFREG => 2**15;
use constant S_IFLNK => S_IFREG + S_IFCHR;

use constant MAXINT  => 2**32 -1;

use constant S_WID   => 1;    #world
use constant S_GID   => 2**3; #group
use constant S_UID   => 2**6; #owner
use constant S_SID   => 2**9; #sticky bits etc.

# not really OO, but then again, not really needed?!
my $inode_start;
BEGIN {$inode_start = 1};

sub new {
    my ($class, %args) = @_;
    my $self = bless {-args => \%args}, ref($class)||$class;

    # our data..
    my $fs_meta = $self->{-data} = {};
    $fs_meta->{'/'} = _new_meta(_mk_mode(7,5,5)|S_IFDIR, $<, (split m/ /, $()[0], time());
    $fs_meta->{'/'}{nlink} = 2;
    $fs_meta->{'/'}{directorylist} = {};

    return $self;
}

sub run {
    my ($self) = @_;
    {
        no warnings 'redefine';
        *{debug} = sub {} unless defined $self->{-args}{debug};
    }

    $SIG{__WARN__} = sub {die @_};
    debug('CPAN Fuse version', $Fuse::VERSION);

    Fuse::main(
        %{$self->{-args}},
        getattr     => $self->_db('getattr',        \&f_getattr),
        fgetattr    => $self->_db('fgetattr',       \&f_getattr),
        readlink    => $self->_db('readlink',       \&f_readlink),
        chmod       => _ctx($self->_db('chmod',     \&f_chmod)),
        chown       => _ctx($self->_db('chown',     \&f_chown)),
        symlink     => _ctx($self->_db('symlink',   \&f_symlink)),
        link        => _ctx($self->_db('link',      \&f_link)),
        unlink      => _ctx($self->_db('unlink',    \&f_unlink)),
        rename      => _ctx($self->_db('rename',    \&f_rename)),
        mknod       => _ctx($self->_db('mknod',     \&f_mknod)),
        mkdir       => _ctx($self->_db('mkdir',     \&f_mkdir)),
        rmdir       => _ctx($self->_db('rmdir',     \&f_rmdir)),
        opendir     => $self->_db('opendir',        \&f_opendir),
        readdir     => $self->_db('readdir',        \&f_readdir),
        releasedir  => $self->_db('releasedir',     \&f_releasedir),
        fsyncdir    => $self->_db('fsyncdir',       \&f_fsyncdir),
        fsync       => $self->_db('fsync',          \&f_fsync),
        setxattr    => $self->_db('setxattr',       \&f_setxattr),
        getxattr    => $self->_db('getxattr',       \&f_getxattr),
        removexattr => $self->_db('removexattr',    \&f_removexattr),
        listxattr   => $self->_db('listxattr',      \&f_listxattr),
        utimens     => $self->_db('utimens',        \&f_utimes),
        statfs      => $self->_db('statfs',         \&f_statfs),
        access      => $self->_db('access',         \&f_access),
        open        => $self->_db('open',           \&f_open),
        release     => $self->_db('release',        \&f_release),
        read        => $self->_db('read',           \&f_read),
        write       => $self->_db('write',          \&f_write),
        truncate    => _ctx($self->_db('truncate',  \&f_truncate)),
        ftruncate   => _ctx($self->_db('ftruncate', \&f_ftruncate)),
        flush       => $self->_db('flush',          \&f_flush),
        create      => _ctx($self->_db('create',    \&f_create)),
        read        => $self->_db('read',           \&f_read),
        write       => $self->_db('write',          \&f_write),
    );
};

sub f_getattr {
    my ($fs_meta, $path) = @_;
    return -Errno::ENAMETOOLONG() if length($path) > 1024;
    my ($dir, $file) = _splitpath($path);
    return -Ernno::ENAMETOOLONG() if length($file) > 255;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    return 0, $r->{ino}, $r->{mode}, $r->{nlink}//1,
         $r->{uid}, $r->{gid}, $r->{dev}//0, $r->{size},
         $r->{atime}, $r->{mtime}, $r->{ctime}, 128*1024, 0;
}

sub f_symlink {
    my ($fs_meta, $from, $to, $cuid, $cgid, $ctime) = @_;
    my $r = _new_entry($fs_meta, $to, _mk_mode(7,7,7)|S_IFLNK, $cuid, $cgid, $ctime);
    $r->{target} = $from;
    return 0;
}

sub f_readlink {
    my ($fs_meta, $path) = @_;
    return -Errno::ENOENT() unless defined(my $entry = $fs_meta->{$path});
    return $entry->{target};
}

sub f_mknod {
    my ($fs_meta, $path, $mode, $rdev, $cuid, $cgid, $ctime) = @_;
    # only called for non-symlinks, non-directories, non-files and links as
    # those are handled by symlink, mkdir, create, link. This is called when
    # mkfifo is used to make a named pipe for instance.
    #
    # FIXME: support 'plain' mknod too: S_IFBLK and S_IFCHR
    my $entry = $fs_meta->{$path} = _new_meta($mode, $cuid, $cgid, $ctime);
    $entry->{dev} = $rdev;

    my ($parent, $file) = _splitpath($path);
    my $p = $fs_meta->{$parent};
    $p->{directorylist}{$file} = $entry;
    $p->{ctime} = $p->{mtime} = $ctime;
    return 0
}

sub f_mkdir {
    my ($fs_meta, $path, $mode, $cuid, $cgid, $ctime) = @_;
    return -Errno::ENAMETOOLONG() if length($path) > 1024;
    my ($parent, $subdir) = _splitpath($path);
    my $m = $fs_meta->{$path} = _new_meta($mode + S_IFDIR, $cuid, $cgid, $ctime);
    $m->{directorylist} = {};
    $m->{nlink} = 2;
    my $p = $fs_meta->{$parent};
    $p->{nlink}++;
    $p->{mtime} = $p->{ctime} = $ctime;
    $p->{directorylist}{$subdir} = $m;
    return 0;
}

sub f_rmdir {
    my ($fs_meta, $path, undef, undef, $ctime) = @_;
    return -Errno::EEXIST() if keys %{$fs_meta->{$path}{directorylist}};
    my ($parent, $dir) = _splitpath($path);
    my $p = $fs_meta->{$parent};
    $p->{nlink}--;
    $p->{ctime} = $p->{mtime} = $ctime;
    delete $p->{directorylist}{$dir};
    delete $fs_meta->{$path};
    return 0;
}

sub f_opendir {
    my ($fs_meta, $path) = @_;
    # each() is not intelligent enough, so we make a copy and do shift()...
    # *sigh*. We could use a double array (basically implement the hash
    # ourselves) and use an index iterator value.
    return 0, [keys %{($fs_meta->{$path}//{})->{directorylist}}];
}

sub f_readdir {
    my ($fs_meta, $path, $offset, $dir_fh) = @_;
    $path = ($path ne '/')?"$path/":$path;
    my ($dirent, $attr) = shift @{$dir_fh};
    return 0 unless defined $dirent;
    return [$offset+1, $dirent, [f_getattr($fs_meta, "$path$dirent")]], 0;
}

sub f_releasedir {
    my ($fs_meta, $path, $dir_fh) = @_;
    # eventually the last reference to it will disappear
    return 0;
}

sub f_fsyncdir {
    my ($fs_meta, $path, $dir_fh) = @_;
    return 0; # nothing to do for now
}

sub f_fsync {
    my ($fs_meta, $path, $fh) = @_;
    return 0; # nothing to do for now
}

sub f_unlink {
    my ($fs_meta, $path, undef, undef, $ctime) = @_;
    my $r = delete $fs_meta->{$path};
    ($r->{nlink} //= 1)--;
    $r->{ctime} = $ctime;

    my ($dir, $f) = _splitpath($path);
    my $p = $fs_meta->{$dir};
    delete $p->{directorylist}{$f};
    $p->{ctime} = $p->{mtime} = $ctime;

    # cleanup for real, file is gone.
    _unlink($fs_meta, $path, $r, $ctime) if $r->{nlink} == 0;
    return 0;
}

sub f_chown {
    my ($fs_meta, $path, $cuid, $cgid, undef, undef, $ctime) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});

    # Funny this is.. but this appears to be ext3 on linux behavior.
    # However, FUSE doesn't give me e.g. -1 -1 as user root, while it
    # wants the ctime to be adjusted. I think this is the nitty gritty
    # details that makes this code rather 'not needed' anywayz..
    #
    # That's the reason why tests 141, 145, 149 and 153 of pjd fail
    # btw...
    if ($cuid != 0){
        unless ($cuid == MAXINT and $cgid == MAXINT) {
            $r->{mode} &= ~S_SID;
        }
    }
    $r->{uid} = $cuid != MAXINT?$cuid:$r->{uid};
    $r->{gid} = $cgid != MAXINT?$cgid:$r->{gid};
    $r->{ctime} = $ctime;
    return 0
}

sub f_chmod {
    my ($fs_meta, $path, $mode, undef, undef, $ctime) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    $r->{mode}  = $mode;
    $r->{ctime} = $ctime;
    return 0;
}

sub f_rename {
    my ($fs_meta, $from, $to, undef, undef, $ctime) = @_;

    # FUSE handles paths, e.g. a file being moved to a directory: the 'to'
    # becomes that target directory + "/" + basename(from).

    my $r_to = \($fs_meta->{$to});

    if ($$r_to){

        # target is a non-empty directory? return ENOTEMPTY errno.h
        return -Errno::ENOTEMPTY() if keys %{$$r_to->{directorylist}//{}};

        # if the target still exists, e.g. when you move a file to another
        # file,first free the blocks: just add to the filesystem's freelist.
        # For this we can simply unlink it, just make sure we use the real
        # unlink, not the journalled one. Of course, we only unlink when it's a
        # file, not in other cases. unlink here also maintains the nlink
        # parameter.

        _unlink($fs_meta, $to, $$r_to, $ctime);
    }

    # rename main node
    $$r_to = delete $fs_meta->{$from};
    $$r_to->{ctime} = $ctime;

    # rename both parent's references to the renamed entity
    my ($p, $e, $fs_p);

    # 'to'
    ($p, $e) = _splitpath($to);
    $fs_p = $fs_meta->{$p};
    $fs_p->{directorylist}{$e} = $$r_to;
    $fs_p->{nlink}++;
    $fs_p->{ctime} = $fs_p->{mtime} = $ctime;

    # 'from'
    ($p, $e) = _splitpath($from);
    $fs_p = $fs_meta->{$p};
    delete $fs_p->{directorylist}{$e};
    $fs_p->{nlink}--;
    $fs_p->{ctime} = $fs_p->{mtime} = $ctime;

    # rename all decendants, maybe not such a good idea to use this
    # mechanism, but don't forget, how many times does one rename e.g.
    # /usr and such.. ;-). for a plain file (or empty subdir), this is for
    # isn't even executed (looped actually)
    #
    # NOTE: 'to' here is of course the freshly moved entry, the previous 'to'
    #       if any is gone, and will be garbage collected.
    #
    if (keys %{$$r_to->{directorylist}//{}}) {
        my ($ts, $fs) = ("$to/", "$from/");
        foreach my $sub (keys %{$$r_to->{directorylist}}){
            my ($tts, $tfs) = ($ts.$sub, $fs.$sub);
            $fs_meta->{$tts} = delete $fs_meta->{$tfs};
        }
    }

    return 0
}

sub f_link {
    my ($fs_meta, $from, $to, undef, undef, $ctime) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$from});

    # update meta
    $r->{ctime} = $ctime;
    ($r->{nlink} //= 1)++;

    # 'copy'
    $fs_meta->{$to} = $r;

    # update the TO parent: add entry + change meta
    my ($toparent, $e) = _splitpath($to);
    my $tp = $fs_meta->{$toparent};
    $tp->{directorylist}{$e} = $r;
    $tp->{ctime} = $tp->{mtime} = $ctime;
    return 0
}

sub f_utimes {
    my ($fs_meta, $path, $atime, $ctime) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    $r->{atime} = floor($atime);
    $r->{ctime} = floor($ctime);
    $r->{atimens} = $atime if $r->{atime} != $atime;
    $r->{ctimens} = $ctime if $r->{ctime} != $ctime;
    return 0;
}

sub f_setxattr {
    my ($fs_meta, $path, $name, $value) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    ($r->{xattr} //= {})->{$name} = $value;
    return 0;
}

sub f_getxattr {
    my ($fs_meta, $path, $name) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    return ($r->{xattr}//{})->{$name};
}

sub f_removexattr {
    my ($fs_meta, $path, $name) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    delete $r->{xattr}{$name};
    return 0;
}

sub f_listxattr {
    my ($fs_meta, $path) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    return values %{$r->{xattr}//{}}, 0;
}

sub f_statfs {
    my ($fs_meta, $path) = @_;
    return 1024, $inode_start, (MAXINT - $inode_start), 0, 0, 0;
}

sub f_access {
    my ($fs_meta, $path) = @_;
    # nothing to do for now, all access is granted (OS/linux kernel determines
    # this with the 'mode'
    return 0;
}

sub f_open {
    my ($fs_meta, $path) = @_;
    return -Errno::ENOENT() unless defined (my $r = $fs_meta->{$path});
    return 0, {f => $r};
}

sub f_flush {
    my ($fs_meta, $path, $fh) = @_;
    # nothing to do for the moment
    return $fh->{errorcode} if defined $fh and exists $fh->{errorcode};
    return 0;
}

sub f_release {
    my ($fs_meta, $path, $mask, $fh) = @_;
    # eventually the last reference to it will disappear
    delete $fh->{f} if defined $fh;
    return 0;
}

sub f_ftruncate {
    my ($fs_meta, $path, $size, $fh, @r) = @_;
    f_truncate($fs_meta, $path, $size, @r);
}

sub f_create {
    my ($fs_meta, $path, $mode, $mask, $flags, $cuid, $cgid, $ctime) = @_;
    $mode = _mk_mode(6,4,4) if $mode == 32768;
    my $r = _new_entry($fs_meta, $path, $mode|S_IFREG, $cuid, $cgid, $ctime);
    return 0, {f => $r};
}

sub _unlink {
    my ($fs_meta, $path, $r, $ctime) = @_;
    debug(\@_);
    return unless keys %{$r->{blockmap}};
    # TODO: implement this
}

sub f_truncate {
    my ($fs_meta, $path, $size, undef, undef, $ctime) = @_;
    return -Errno::EINVAL() if $size < 0;
    # TODO: implement this!
    return 0;
}

sub f_read {
    my ($fs_meta, $path, $size, $offset, $obj) = @_;
    # TODO: implement this!
    return '';
}

sub f_write {
    my ($fs_meta, $path, $size, $buf, $offset, $obj) = @_;
    # TODO: implement this!
    return length($buf);
}

sub _mk_mode {
    my ($owner, $group, $world, $sticky) = @_;
    return $owner * S_UID + $group * S_GID + $world + ($sticky // 0) * S_SID;
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

sub _new_entry {
    my ($fs_meta, $path, $mode, $cuid, $cgid, $ctime) = @_;
    my $r = $fs_meta->{$path} = _new_meta($mode, $cuid, $cgid, $ctime);
    my ($parent, $file) = _splitpath($path);
    my $p = $fs_meta->{$parent};
    $p->{directorylist}{$file} = $r;
    $p->{ctime} = $p->{mtime} = $ctime;
    return $r;
}

sub _db {
    my ($self, $abbr, $f) = @_;
    sub {
        local $SIG{__WARN__} = sub {
            die $abbr, ': ', @_;
        };
        debug($abbr, 'arguments', \@_);
        my @r = &$f($self->{-data}, @_);
        debug($abbr, 'result', @r, $self->{-data});
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

sub _splitpath {
    my ($path) = @_;
    return dirname($path), basename($path);
}

sub debug {
    $Data::Dumper::Indent   = 1;
    $Data::Dumper::Sortkeys = 1;
    my @c = caller(0);
    my $str = join(':', strftime('%d/%m %H:%M:%S', gmtime()), ' ', $c[1], $c[3], $c[2]);
    for (split m/\n/, join(' ', map {ref($_) and Dumper($_) or $_} map {$_//'<undef>'} @_)){
        print STDERR "$str: ", $_, "\n";
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

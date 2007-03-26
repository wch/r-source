# fixme: --dirfile option
# fixme: sort entries
# fixme: send to FSF ?

$version="1.4.1.13"; # This line modified by Makefile
sub version {
        print STDERR <<END;
Debian GNU/Linux install-info $version.  Copyright (C) 1994,1995
Ian Jackson.  This is free software; see the GNU General Public Licence
version 2 or later for copying conditions.  There is NO warranty.
END
}

sub usage {
    print STDERR <<END;
usage: install-info [--version] [--help] [--debug] [--maxwidth=nnn]
             [--section regexp title] [--infodir=xxx] [--align=nnn]
             [--calign=nnn] [--quiet] [--menuentry=xxx] [--info-dir=xxx]
             [--keep-old] [--description=xxx] [--test] [--remove] [--]
             filename
END
}

$infodir='/usr/info';
$maxwidth=79;
$align=27;
$calign=29;

undef $menuentry;
undef $quiet;
undef $nowrite;
undef $keepold;
undef $description;
undef $sectionre;
undef $sectiontitle;
$0 =~ m|[^/]+$|; $name= $&;

while ($ARGV[0] =~ m/^--/) {
    $_= shift(@ARGV);
    last if $_ eq '--';
    if ($_ eq '--version') {
        &version; exit 0;
    } elsif ($_ eq '--quiet') {
        $quiet=1;
    } elsif ($_ eq '--test') {
        $nowrite=1;
    } elsif ($_ eq '--keep-old') {
        $keepold=1;
    } elsif ($_ eq '--remove') {
        $remove=1;
    } elsif ($_ eq '--help') {
        &usage; exit 0;
    } elsif ($_ eq '--debug') {
        open(DEBUG,">&STDERR") || exit 1;
    } elsif ($_ eq '--section') {
        if (@ARGV < 2) {
            print STDERR "$name: --section needs two more args\n";
            &usage; exit 1;
        }
        $sectionre= shift(@ARGV);
        $sectiontitle= shift(@ARGV);
    } elsif (m/^--maxwidth=([0-9]+)$/) {
        $maxwidth= $1;
    } elsif (m/^--align=([0-9]+)$/) {
        $align= $1;
    } elsif (m/^--calign=([0-9]+)$/) {
        $calign= $1;
    } elsif (m/^--infodir=/) {
        $infodir=$';
    } elsif (m/^--menuentry=/) {
        $menuentry=$';
    } elsif (m/^--info-dir=/) {
        $infodir=$';
    } elsif (m/^--description=/) {
        $description=$';
    } else {
        print STDERR "$name: unknown option \`$_'\n"; &usage; exit 1;
    }
}

if (!@ARGV) { &version; print STDERR "\n"; &usage; exit 1; }

$filename= shift(@ARGV);
if (@ARGV) { print STDERR "$name: too many arguments\n"; &usage; exit 1; }

if ($remove) {
    print STDERR "$name: --section ignored with --remove\n" if length($sectiontitle);
    print STDERR "$name: --description ignored with --remove\n" if length($description);
}

print STDERR "$name: test mode - dir file will not be updated\n"
    if $nowrite && !$quiet;

umask(umask(0777) & ~0444);

$filename =~ m|[^/]+$|; $basename= $&; $basename =~ s/(\.info)?(\.gz)?$//;
print DEBUG <<END;
 infodir=\`$infodir'  filename=\`$filename'  maxwidth=\`$maxwidth'
 menuentry=\`$menuentry'  basename=\`$basename'
 description=\`$description' remove=$remove
END

if (!$remove) {

    if (!-f $filename && -f "$filename.gz" || $filename =~ s/\.gz$//) {
        $filename= "gzip -d <$filename.gz |";  $pipeit= 1;
    } else {
        $filename= "< $filename";
    }

    if (!length($description)) {
        
        open(IF,"$filename") || die "$name: read $filename: $!\n";
        $asread='';
        while(<IF>) {
	    m/^START-INFO-DIR-ENTRY$/ && last;
	    m/^INFO-DIR-SECTION (.+)$/ && do {
		$sectiontitle = $1		unless defined($sectiontitle);
		$sectionre = '^'.quotemeta($1)	unless defined($sectionre);
	    }
	}
        while(<IF>) { last if m/^END-INFO-DIR-ENTRY$/; $asread.= $_; }
        close(IF); &checkpipe;
        if ($asread =~ m/(\* *[^:]+: *\([^\)]+\).*\. *.*\n){2,}/) {
            $infoentry= $asread; $multiline= 1;
            print DEBUG <<END;
 multiline \`$asread'
END
        } elsif ($asread =~ m/^\* *([^:]+):( *\([^\)]+\)\.|:)\s*/) {
            $menuentry= $1; $description= $';
            print DEBUG <<END;
 infile menuentry \`$menuentry' description \`$description'
END
        } elsif (length($asread)) {
            print STDERR <<END;
$name: warning, ignoring confusing INFO-DIR-ENTRY in file.
END
        }
    }

    if (length($infoentry)) {

        $infoentry =~ m/\n/;
        print "$`\n" unless $quiet;
        $infoentry =~ m/^\* *([^:]+): *\(([^\)]+)\)/ || die; # internal error
        $sortby= $1;  $fileinentry= $2;
        
    } else {
        
        if (!length($description)) {
            open(IF,"$filename") || die "$name: read $filename: $!\n";
            $asread='';
            while(<IF>) {
                if (m/^\s*[Tt]his file documents/) {
                    $asread=$';
                    last;
                }
            }
            if (length($asread)) {
                while(<IF>) { last if m/^\s*$/; $asread.= $_; }
                $description= $asread;
            }
            close(IF); &checkpipe;
        }

        if (!length($description)) {
            print STDERR <<END;
No \`START-INFO-DIR-ENTRY' and no \`This file documents'.
$name: unable to determine description for \`dir' entry - giving up
END
            exit 1;
        }

        $description =~ s/^\s*(.)//;  $_=$1;  y/a-z/A-Z/;
        $description= $_ . $description;

        if (!length($menuentry)) {
            $menuentry= $basename;  $menuentry =~ s/\Winfo$//;
            $menuentry =~ s/^.//;  $_=$&;  y/a-z/A-Z/;
            $menuentry= $_ . $menuentry;
        }

        print DEBUG <<END;
 menuentry=\`$menuentry'  description=\`$description'
END

        $cprefix= sprintf("* %s: (%s).", $menuentry, $basename);
        $align--; $calign--;
        $lprefix= length($cprefix);
        if ($lprefix < $align) {
            $cprefix .= ' ' x ($align - $lprefix);
            $lprefix= $align;
        }
        $prefix= "\n". (' 'x $calign);
        $cwidth= $maxwidth+1;

        for $_ (split(/\s+/,$description)) {
            $l= length($_);
            $cwidth++; $cwidth += $l;
            if ($cwidth > $maxwidth) {
                $infoentry .= $cprefix;
                $cwidth= $lprefix+1+$l;
                $cprefix= $prefix; $lprefix= $calign;
            }
            $infoentry.= ' '; $infoentry .= $_;
        }

        $infoentry.= "\n";
        print $infoentry unless $quiet;
        $sortby= $menuentry;  $sortby =~ y/A-Z/a-z/;

    }
}

if (!$nowrite && !link("$infodir/dir","$infodir/dir.lock")) {
    die "$name: failed to lock dir for editing! $!\n".
        ($! =~ m/exists/i ? "try deleting $infodir/dir.lock ?\n" : '');
}

open(OLD,"$infodir/dir") || &ulquit("$name: open $infodir/dir: $!\n");
@work= <OLD>;
eof(OLD) || &ulquit("$name: read $infodir/dir: $!\n");
close(OLD) || &ulquit("$name: close $infodir/dir after read: $!\n");
while ($work[$#work] !~ m/\S/) { $#work--; }

do {
    last if !@work;
    $_= shift(@work);
    push(@head,$_);
} until (m/^\*\s*Menu:/i);

if (!$remove) {

    for ($i=0; $i<=$#work; $i++) {
        next unless $work[$i] =~ m/^\* *[^:]+: *\(([^\)]+)\).*\.\s/;
        last if $1 eq $basename || $1 eq "$basename.info";
    }
    for ($j=$i; $j<=$#work+1; $j++) {
        next if $work[$j] =~ m/^\s+\S/;
        last unless $work[$j] =~ m/^\* *[^:]+: *\(([^\)]+)\).*\.\s/;
        last unless $1 eq $basename || $1 eq "$basename.info";
    }

    if ($i < $j) {
        if ($keepold) {
            print "$name: existing entry for \`$basename' not replaced\n" unless $quiet;
            $nowrite=1;
        } else {
            print "$name: replacing existing dir entry for \`$basename'\n" unless $quiet;
        }
        $mss= $i;
        @work= (@work[0..$i-1], @work[$j..$#work]);
    } elsif (length($sectionre)) {
        $mss= -1;
        for ($i=0; $i<=$#work; $i++) {
            $_= $work[$i];
            next if m/^\*/;
            next unless m/$sectionre/io;
            $mss= $i+1; last;
        }
        if ($mss < 0) {
            print "$name: creating new section \`$sectiontitle'\n" unless $quiet;
            for ($i= $#work; $i>=0 && $work[$i] =~ m/\S/; $i--) { }
            if ($i <= 0) { # We ran off the top, make this section and Misc.
                print "$name: no sections yet, creating Miscellaneous section too.\n"
                    unless $quiet;
                @work= ("\n", "$sectiontitle\n", "\n", "Miscellaneous:\n", @work);
                $mss= 1;
            } else {
                @work= (@work[0..$i], "$sectiontitle\n", "\n", @work[$i+1..$#work]);
                $mss= $i+1;
            }
        }
        while ($mss <= $#work) {
            $work[$mss] =~ m/\S/ || last;
            $work[$mss] =~ m/^\* *([^:]+):/ || ($mss++, next);
            last if $multiline;
            $_=$1;  y/A-Z/a-z/;
            last if $_ gt $sortby;
            $mss++;
        }
    } else {
        print "$name: no section specified for new entry, placing at end\n"
            unless $quiet;
        $mss= $#work+1;
    }

    @work= (@work[0..$mss-1], $infoentry, @work[$mss..$#work]);
    
} else {

    for ($i=0; $i<=$#work; $i++) {
        next unless $work[$i] =~ m/^\* *([^:]+): *\((\w[^\)]*)\)/;
        $tme= $1; $tfile= $2; $match= $&;
        next unless $tfile eq $basename;
        last if !length($menuentry);
        $tme =~ y/A-Z/a-z/;
        last if $tme eq $menuentry;
    }
    for ($j=$i; $j<=$#work+1; $j++) {
        next if $work[$j] =~ m/^\s+\S/;
        last unless $work[$j] =~ m/^\* *([^:]+): *\((\w[^\)]*)\)/;
        $tme= $1; $tfile= $2;
        last unless $tfile eq $basename;
        next if !length($menuentry);
        $tme =~ y/A-Z/a-z/;
        last unless $tme eq $menuentry;
    }
        print DEBUG <<END;
 i=$i \$work[\$i]=\`$work[$i]' j=$j \$work[\$j]=\`$work[$j]'
END

    if ($i < $j) {
        print "$name: deleting entry \`$match ...'\n" unless $quiet;
        $_= $work[$i-1];
        unless (m/^\s/ || m/^\*/ || m/^$/ ||
                $j > $#work || $work[$j] !~ m/^\s*$/) {
            s/:?\s+$//;
            if ($keepold) {
                print "$name: empty section \`$_' not removed\n" unless $quiet;
            } else {
                $i--; $j++;
                print "$name: deleting empty section \`$_'\n" unless $quiet;
            }
        }
        @work= (@work[0..$i-1], @work[$j..$#work]);
    } else {
        print "$name: no entry for file \`$basename'".
              (length($menuentry) ? " and menu entry \`$menuentry'": '').
              ".\n"
            unless $quiet;
    }
}

if (!$nowrite) {
    open(NEW,"> $infodir/dir.new") || &ulquit("$name: create $infodir/dir.new: $!\n");
    print(NEW @head,@work) || &ulquit("$name: write $infodir/dir.new: $!\n");
    close(NEW) || &ulquit("$name: close $infodir/dir.new: $!\n");

    unlink("$infodir/dir.old");
    link("$infodir/dir","$infodir/dir.old") ||
        &ulquit("$name: cannot backup old $infodir/dir, giving up: $!\n");
    rename("$infodir/dir.new","$infodir/dir") ||
        &ulquit("$name: install new $infodir/dir: $!\n");
unlink("$infodir/dir.lock") || die "$name: unlock $infodir/dir: $!\n";
}

sub ulquit {
    unlink("$infodir/dir.lock") ||
        warn "$name: warning - unable to unlock $infodir/dir: $!\n";
    die $_[0];
}

sub checkpipe {
    return if !$pipeit || !$? || $?==0x8D00 || $?==0x0D;
    die "$name: read $filename: $?\n";
}

exit 0;

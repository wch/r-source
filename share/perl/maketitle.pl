# Perl script to create a TITLE file from the package DESCRIPTION.
#
# <FIXME>
#  This script was obtained by running a2p on an Awk script.
#  It could be considerably improved, using R::Dcf etc.
# </FIXME>

$[ = 1;			# set array base to 1
$, = ' ';		# set output field separator
$\ = "\n";		# set output record separator

$active = 0;
$n = 0;

while (<>) {
    chomp;	# strip record separator
    @Fld = split(' ', $_, 9999);

    if ($_ =~ /^Package:/) {
	$package = $Fld[2];
    }
    elsif ($_ =~ /^Title:/) {
	$active = 1;
	$n = $#Fld - 1;
	for ($i = 2; $i <= $#Fld; $i++) {
	    $a{$i - 1} = $Fld[$i];
	}
    }
    elsif ($_ =~ /^[ \t]/) {
	if ($active == 1) {
	    for ($i = 1; $i <= $#Fld; $i++) {
		$a{$n + $i} = $Fld[$i];
	    }
	    $n = $n + $#Fld;
	}
    }
    else {
	$active = 0;
    }
}

$S = '';
$lc = 16;
$rc = 72;
$old = $package;
$len = length($old);
if ($len > $lc - 3) {
    $S = $old;
    for ($j = 1; $j < $lc; $j++) {
	$old = $old . ' ';
    }
}
else {
    for ($j = 1; $j < $lc - $len; $j++) {
	$old = $old . ' ';
    }
}
for ($i = 1; $i <= $n; $i++) {
    $new = $old . ' ' . $a{$i};
    if (length($new) > $rc) {
	if (length($S) > 0) {
	    $S = $S . "\n" . $old;
	}
	else {
	    $S = $old;
	}
	$old = '';
	for ($j = 1; $j < $lc; $j++) {
	    $old = $old . ' ';
	}
	$new = $old . ' ' . $a{$i};
    }
    $old = $new;
}
if (length($S) > 0) {
    $S = $S . "\n" . $new;
}
else {
    $S = $old;
}
print $S;


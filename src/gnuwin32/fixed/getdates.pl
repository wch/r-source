$infile = $ARGV[0];
$confile = $ARGV[1];
$datefile = $ARGV[2];

open fd, "$datefile" || die "file $datefile not found\n";
while (<fd>) {
    chomp;
    $date = $_;
    break;
}
close(fd);
($year, $month, $day) = split "/", $date;
$day =~ s/^0//;

$done = 0;
open fd, "$confile" || die "file $confile not found\n";
while (<fd>) {
    chomp;
    if( /^R_MAJOR=/) {
	($junk, $rmajor) = split "=";
	$done++;
    }
    if( /^R_MINOR=/) {
	($junk, $rminor) = split "=";
	$done++;
    }
    if( /^R_STATUS=/) {
	($junk, $rstatus) = split "=";
	$done++;
    }
    if( /^R_STATUS_REV=/) {
	($junk, $rstatusrev) = split "=";
	$done++;
    }
    if ($done >= 4) { break; }
}

open fd, "<$infile" || die "file $infile not found\n";
while (<fd>) {
    s/\@R_MAJOR@/$rmajor/;
    s/\@R_MINOR@/$rminor/;
    s/\@R_STATUS@/$rstatus/;
    s/\@R_STATUS_REV@/$rstatusrev/;
    s/\@R_DAY@/"$day"/;
    s/\@R_MONTH@/"$month"/;
    s/\@R_YEAR@/"$year"/;
    print;
}

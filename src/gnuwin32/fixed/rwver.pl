while(<>) {
    s/([0-9]+)\.([0-9]+)\.([0-9]+)\s*//;
    my $major = $1;
    my $minor = $2;
    my $patch = $3;
    $patch++ if /Patched/;
    $minor = "0".$minor if $minor < 10;
    $ans = "rw$major$minor$patch\n";
}
print $ans;

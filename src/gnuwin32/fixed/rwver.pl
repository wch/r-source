while(<>) {
    s/([0-9]+)\.([0-9]+)\.([0-9]+)\s*//;
    my $minor = $2;
    $minor++ if /Patched/;
    $minor = "0".$minor if $minor < 10;
    $ans = "rw$1$minor$3\n";
}
print $ans;

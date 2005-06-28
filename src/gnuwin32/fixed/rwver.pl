while(<>) {
    s/([0-9]+)\.([0-9]+)\.([0-9]+)\s*//;
    my $major = $1;
    my $minor = $2;
    my $patch = $3;
    $patch = $patch."pat" if /Patched/;
    $patch = $patch."dev" if /unstable/;
    $patch = $patch."alpha" if /alpha/;
    $patch = $patch."beta" if /beta/;
#    $minor = "0".$minor if $minor < 10;
    $ans = "R-$major.$minor.$patch\n";
}
print $ans;

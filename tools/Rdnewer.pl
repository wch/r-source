use File::Basename;
use Getopt::Long;

my $OS = "unix";
GetOptions(("os|OS:s"));
$OS = $opt_os if $opt_os;

$file = $ARGV[1];
(-r $file) || exit 0;

my $age = (-M $file);
my $dir = "$ARGV[0]/man";

chdir $dir || die "whereami?";
foreach $f (<*.Rd>) {
    exit 0 if ((-M $f) < $age)
}
chdir $OS if (-d $OS);
foreach $f (<*.Rd>) {
    exit 0 if ((-M $f) < $age)
}
exit 1;

### Local Variables: ***
### mode: perl ***
### perl-indent-level: 4 ***
### End: ***

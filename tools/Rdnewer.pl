## Test whether any Rd file in the `man' and `man/$OS' subdirectories of
## directory DIR is newer than a given FILE.  Return 0 if such a file is
## found (i.e., in the case of `success'), and 1 otherwise, so that the
## return value can be used for shell `if' tests.
##
## Usage:
##   perl /path/to/Rdnewer.pl [options] DIR FILE

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

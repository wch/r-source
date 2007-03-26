## Test whether any Rd file in the 'man' and 'man/$OS' subdirectories of
## directory DIR is newer than a given FILE.  Return 0 if such a file is
## found (i.e., in the case of 'success'), and 1 otherwise, so that the
## return value can be used for shell 'if' tests.
##
## Usage:
##   perl /path/to/Rdnewer.pl [options] DIR FILE

## <NOTE>
## Could use &file_path() and &list_files_with_exts() to make this
## portable.
## </NOTE>

use File::Basename;
use Getopt::Long;

my $OS = "unix";
GetOptions("os|OS:s" => \$OS);

my $file = $ARGV[1];
(-r $file) || exit 0;

my $age = (-M $file);
my $dir = "$ARGV[0]/man";

chdir $dir or die "whereami?";
foreach my $f (<*.Rd>) {
    exit 0 if ((-M $f) < $age)
}
chdir $OS if (-d $OS);
foreach my $f (<*.Rd>) {
    exit 0 if ((-M $f) < $age)
}
exit 1;

### Local Variables: ***
### mode: perl ***
### perl-indent-level: 4 ***
### End: ***

use Cwd;

chdir($ARGV[0]);
my $currentdir = cwd();
print "$currentdir";


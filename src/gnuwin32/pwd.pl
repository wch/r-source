use Cwd;

chdir($ARGV[0]);
my $currentdir = getcwd();
print "$currentdir";


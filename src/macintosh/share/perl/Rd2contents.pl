use File::Basename;

$ARGV[0] =~ /([^\:]*)$/;
my $pkg = $1;

while(<$ARGV[0]:man:*.Rd>){ &do_one; }
if(-d  "$ARGV[0]:man:mac") {
    while(<$ARGV[0]:man:mac:*.Rd>){ &do_one; }
}

sub do_one {
    my $file = basename($_, (".Rd", ".rd"));

    open(rdfile, "<$_");
    undef $text;

    while(<rdfile>){
	$text .= $_;
    }
    close rdfile;

    $text =~ /\\name\{\s*([^\}]+)\s*\}/s;
    my $rdname = $1;
    $rdname =~ s/\n/ /sg;

    $text =~ /\\title\{\s*([^\}]+)\s*\}/s;
    my $rdtitle = $1;
    $rdtitle =~ s/\n/ /sg;

    undef @aliases;
    while($text =~ s/\\alias\{\s*(.*)\s*\}//){
	$alias = $1;
	$alias =~ s/\\%/%/g;
	push @aliases, $alias;
    }

    undef @keywords;
    while($text =~ s/\\keyword\{\s*(.*)\s*\}//){
	$keyword = $1;
	$keyword =~ s/\\%/%/g;
	push @keywords, $keyword;
    }

    $, = " ";
    print "Entry: $rdname\n";
    print "Aliases: @aliases\n";
    print "Keywords: @keywords\n";
    print "Description: $rdtitle\n";
    print "URL: ../../../library/$pkg/html/$file.html\n\n";
}


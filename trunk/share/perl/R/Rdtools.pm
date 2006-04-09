package R::Rdtools;

use Carp;
use Exporter;
use FileHandle;
use Text::DelimMatch;

@ISA = qw(Exporter);
@EXPORT = qw(get_section get_usages get_arglist Rdpp);

my $delimcurly = new Text::DelimMatch("\\{", "\\}");
$delimcurly->escape("\\");

my $delimround = new Text::DelimMatch("\\(", "\\)");

sub get_section {

    my ($text, $section) = @_;

    ## remove comments
    $text =~ s/([^\\])((\\\\)*)%.*\n/$1$2\n/g;

    my @text = split(/\\$section\{/, " " . $text);
    shift @text;

    my @sections;
    foreach $text (@text) {
	$delimcurly->match("\{" . $text);
	push(@sections, $delimcurly->matched);
    }

    @sections;
}

sub get_usages {

    my ($text, $mode, $verbose) = @_;

    ## remove comments
    $text =~ s/([^\\])((\\\\)*)%.*\n/$1$2\n/g;

    ## <FIXME>
    ## This apparently gets quoted args wrong, e.g. in read.table().
    ##   $delimround->quote("\"");
    ##   $delimround->quote("\'");
    ## </FIXME>

    sub add_to_functions {
	my($ref, $prefix, $match, $name) = @_;
	if($ref->{$prefix}) {
	    ## Multiple usages for a function are trouble.
	    ## We could try to build the full arglist for the function
	    ## from the usages.  A simple idea is to do
	    ##   chop($ref->{$prefix});
	    ##   my $foo = ", " . substr($match, 1);
	    ##   $ref->{$prefix} .= $foo;
	    ## which adds the 'new' args to the 'old' ones.
	    ## This is not good enough, as it could give duplicate args
	    ## which is not allowed.  In fact, we would generally need
	    ## an R parser for the arglist, as the usages could be as
	    ## bad as 
	    ##   foo(a = c("b", "c"), b = ...)
	    ##   foo(c = NULL, ...)
	    ## so that splitting on ',' is not good enough.
	    ## However, there are really only two functions with
	    ## justified multiple usage (abline and seq), so we simply
	    ## warn about multiple usage in case it was not shadowed by
	    ## a \synopsis unless in mode 'args', where we can cheat.
	    if(($mode eq "args") || ($mode eq "style")) {
		my $save_prefix = $prefix . "~";
		while($ref->{$save_prefix}) {
		    $save_prefix .= "~";
		}
		$ref->{$save_prefix} = $match;
	    }
	    else {
		print("Multiple usage for function $prefix in $name\n");
	    }
	} else {
	    $ref->{$prefix} = $match;
	}
    }

    sub add_to_S4methods {
	## <FIXME>
	## Could make this smarter similar to add_to_functions().
	my($ref, $prefix, $match, $name) = @_;
	if($ref->{$prefix}) {
	    print("Multiple usage for S4 method $prefix in ${name}\n");
	}
	else {
	    $ref->{$prefix} = $match;
	}
	## </FIXME>
    }

    my %usages;

    my %functions = ();
    my @variables;
    my @data_sets;
    my %S4methods;

    my $ref_to_functions = \%functions;
    my $ref_to_S4methods = \%S4methods;

    my @text;
    my $name;
    my $maybe_is_data_set_doc = 0;

    ## Get the \name documented.
    $name = $delimcurly->match(substr($text, index($text, "\\name")));
    $name = substr($name, 1, $#name);

    ## <FIXME>
    ## We need to special-case documentation for data sets, or the
    ## data(FOO) usage for data sets overrides the docs for function
    ## data().  Unless Rd is extended so that there is an Rd type for
    ## data docs, we need to rely on heuristics.  Older versions ignored
    ## all Rd files having 'datasets' as their only keyword.  But this
    ## is a problem: Rd authors might use other keywords to indicate
    ## that a data set is useful for a certain kind of statistical
    ## analysis.  Hence, we do the following: ignore all usages of
    ## data(FOO) in a file with keyword 'datasets' where FOO as only one
    ## argument in the sense that it does not match ','.
    $maybe_is_data_set_doc = 1
	if($text =~ /\\keyword\{\s*datasets\s*\}/);
    ## </FIXME>

    ## In 'codoc' mode, use \synopsis in case there is one, but warn
    ## about doing so if verbose.
    @text = split(/\\synopsis/, $text) if($mode eq "codoc");
    if($#text > 0) {
	print "Using synopsis in '$name'\n" if($verbose);
    } else {
	@text = split(/\\usage/, $text);
    }

    shift @text;

    foreach $text (@text) {

	my $usage = $delimcurly->match($text);

	## Get rid of leading and trailing braces.
	$usage = substr($usage, 1, -1) if($usage);

	## Remove R comment lines.  (Removing all comments is a bit
	## tricky as '#' could occur inside a string ...)
	$usage =~ s/(^|\n)[\s\n]*\#.*(\n|$)/$2/g;

	while($usage) {
	    $usage =~ s/^[\s\n]*//g;

	    ## Try to match usage for variables (i.e., a syntactic name
	    ## on a single line).
	    if($usage =~
	       /^([\.[:alpha:]][\.[:alnum:]]*)\s*([\#\n]|$)/) {
		push(@variables, $1);
		$usage =~ s/^.*(\n|$)//;
		next;
	    }

	    ## Try matching infix markup for '%whatever%' special binary
	    ## operators.  Note that R-lang says that 'whatever' can be
	    ## *anything* ...
	    if($usage =~
	       /^([\.[:alnum:]]+)\s+\\\%(.+)\\\%\s+([\.[:alnum:]]+)\s*([\#\n]|$)/) {
		## <FIXME>
		## We should really have a common regexp for matching
		## syntactic R names.
		## </FIXME>
		add_to_functions($ref_to_functions,
				 "\%" . $2 . "\%",
				 "($1, $3)",
				 $name);
		$usage =~ s/^.*(\n|$)//;
		next;
	    }

	    ## <FIXME>
	    ## Add something for matching subscripting and subassigning
	    ## (extracting and replacing subsets): look for
	    ##   ^${R_syntax_name_RE}[?
	    ## and use delimMatch to get the matching ']' (or ']]').
	    ## Similarly for subscripting/subassiging via '$'.
	    ##
	    ## Note that we also need to deal with markup for S3 methods
	    ## for subscripting and subassigning: unless we know what
	    ## Rdconv() should do with e.g.
	    ##   \S3method{[}{data.frame}(ARGLIST)
	    ## there is really not too much point in playing with this.
	    ## </FIXME>

	    my ($generic, $class, $siglist);

	    ## Try to match the next '(...)' arglist from $usage.
	    my ($prefix, $match, $rest) = $delimround->match($usage);

	    ## Play with $prefix.
	    $prefix =~ s/[\s\n]*$//;
	    $prefix =~ s/^([\s\n\{]*)//;
	    $prefix =~ s/(.*\n)*//g;
	    $prefix =~ s/^\"//;
	    $prefix =~ s/\"$//;

	    if(!$prefix || ($prefix =~ /.+<-.+/)) {
		## Ignore matches where $prefix is empty, or contains a
		## '<-' in the 'middle', most likely from something like
		## NAME <- FUN(ARGLIST), e.g., Random.Rd.
		$rest =~ s/^.*(\n|$)//g;
		$usage = $rest;
		next;
	    }

	    if($prefix =~
	       /\\(S3)?method\{([[:alnum:].]+)\}\{([[:alnum:].]+)\}/) {
		## We need a little magic for S3 replacement *methods*,
		## and hence remember S3 methods markup found.
		$generic = $2;
		$class = $3;
		if($mode eq "style") {
		    $prefix = "\\method{${generic}}{${class}}";
		}
		else {
		    $prefix = "${generic}.${class}";
		}
	    }
	    if($prefix =~
	       /\\S4method\{([[:alnum:].]+)\}\{([[:alnum:].,]+)\}/) {
		## We need a little magic for S4 replacement *methods*,
		## and hence remember S4 methods markup found.
		$generic = $1;
		$siglist = $2;
		$prefix = "\\S4method{${generic}}{${siglist}}";
	    }

	    ## Play with $match.
	    $match =~ s/=\s*([,\)])/$1/g;
	    $match =~ s/\<\</\"\<\</g; # foo = <<see below>> style
	    $match =~ s/\>\>/\>\>\"/g; # foo = <<see below>> style
	    $match =~ s/\\%/%/g; # comments

	    if($rest =~ /^\s*([\#\n]|$)/) {
		## Note that we need to allow for R comments in the
		## above regexp.

		## <NOTE>
		## Heuristics for data set documentation once more.
		if($maybe_is_data_set_doc
		   && ($prefix eq "data")
		   && ($match !~ /\,/)) {
		    push(@data_sets, substr($match, 1, -1));
		    last;
		}
		## </NOTE>

		if($siglist) {
		    add_to_S4methods($ref_to_S4methods,
				     $prefix, $match, $name);
		}
		else {
		    add_to_functions($ref_to_functions,
				     $prefix, $match, $name);
		}
	    }
	    elsif($rest =~ /^\s*\<-\s*([[:alpha:]]+)\s*(\n|$)/) {
		## Looks like documentation for a replacement function.
		if($siglist) {
		    ## Documentaion for an S4 replacement method in the
		    ## form
		    ##   \S4method{GENERIC}{SIGLIST}(ARGLIST) <- RHS
		    $prefix = "\\S4method{${generic}<-}{${siglist}}";
		    add_to_S4methods($ref_to_S4methods,
				     $prefix,
				     substr($match, 0, -1) . ", $1)",
				     $name);
		}
		else {
		    if($generic) {
			## Documentation for a replacement *method* in
			## the form
			##   \method{GENERIC}{CLASS}(ARGLIST) <- RHS
			## with GENERIC *without* the trailing '<-'.
			## Rewrite as
			##   "GENERIC<-.CLASS" (ARGLIST, VALUE)
			## We could also deal with
			##   \method{GENERIC<-}{CLASS}(ARGLIST) <- RHS
			## here but Rdconv() would not get this right
			## for the time being ...
			if($mode eq "style") {
			    $prefix =
			      "\\method{${generic}<-}{${class}}";
			}
			else {
			    $prefix = "${generic}<-.${class}";
			}
		    } else {
			$prefix = "${prefix}<-";
		    }
		    add_to_functions($ref_to_functions,
				     $prefix,
				     substr($match, 0, -1) . ", $1)",
				     $name);
		}
	    }

	    $rest =~ s/^.*(\n|$)//g;
	    $usage = $rest;
	}

    }

    @{$usages{"functions"}} = %functions;
    @{$usages{"variables"}} = @variables;
    @{$usages{"data_sets"}} = @data_sets;
    @{$usages{"S4methods"}} = %S4methods;

    %usages;
}

sub get_arglist {

    ## Get the list of all documented arguments, i.e., the first
    ## arguments from the top-level \item{}{}s in section \arguments,
    ## split on ','.

    my ($text) = @_;
    my @args = ();

    my @keywords = get_section($text, "keyword");
    foreach my $keyword (@keywords) {
    	return ("*internal*") if($keyword =~ /^\{\s*internal\s*\}$/);
    }

    my @chunks = get_section($text, "arguments");
    foreach my $chunk (@chunks) {
	my ($prefix, $match);
	my $rest = substr($chunk, 1, -1);
	while($rest) {
	    ## Try matching top-level \item{}{}.
	    ($prefix, $match, $rest) = $delimcurly->match($rest);
	    if($prefix =~ /\\item$/) {
		## If successful, $match contains the first argument to
		## the \item enclosed by the braces.
		$match =~ s/\\dots/.../g;
		$match =~ s/\n/ /g;
		push(@args, split(/\,\s*/, substr($match, 1, -1)));
	    } else {
		break;
	    }
	    ## Strip off the second argument to \item.
	    ($prefix, $match, $rest) = $delimcurly->match($rest);
	}
    }

    @args;
}

sub Rdpp {

    my ($file, $OS) = @_;
    my $fh = new FileHandle "< $file" or croak "open($file): $!\n";
    my $skip_level;
    my @skip_state;
    my $skip;
    my $text;
    $OS = "unix" unless $OS;
    while(<$fh>) {
        if (/^#ifdef\s+([A-Za-z0-9]+)/o) {
	    $skip = $1 ne $OS;
	    $skip_level += $skip;
	    push(@skip_state, $skip);
            next;
        }
        if (/^#ifndef\s+([A-Za-z0-9]+)/o) {
	    $skip = $1 eq $OS;
	    $skip_level += $skip;
	    push(@skip_state, $skip);
            next;
        }
        if (/^#endif/o) {
	    $skip_level -= pop(@skip_state);
            next;
        }
        next if $skip_level > 0;
	next if /^\s*%/o;
        $text .= $_;
    }
    close($fh);
    $text;
}

1;

#!/usr/bin/perl -w
#
## Replace multiple empty lines by single empty lines
## -- but still leave ONE EMPTY !!

$/="\n\n"; # input  record separator
Para:
 while (<>) {
 print $_ unless /^\n\n$/
}

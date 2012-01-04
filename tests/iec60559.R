## Tests for various features of IEC60559 doubles.
## Most of these are optional, so this is a sloppy test.

# Goes to denormal (aka subnormal) numbers at -708.4
exp(c(-745:-740, -730, -720, -710:-705))

# goes to subnormal numbers at -308, to zero at ca 5e-324.
10^-(324:307)
2^-(1022:1075)

# And because most libm pow() functions special-case integer powers.
10^-(324:307-0.01)/10^0.01

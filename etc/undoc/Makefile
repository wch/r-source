BASE  = ../../src/library/base/funs
Names = ../../src/main/names.c
manDIR= ../../src/library/base/man
Exe   = ../../bin/R


Targets = UNDOCUMENTED\
	 DOC_unfun Builtins unBuiltin Built_unfun\
	 No-Examples #nomore Examples
SUFFIXES = srt
%.srt : %
	sort $< > $@
##	sort $< | sort -f -d > $@

all: $(Targets)
	@echo '   lines   words   chars FILE'
	@echo '   -----   -----   ----- --------------'
	-@wc `ls $(Targets) ALL* *DOC* | grep -v \.srt| uniq` | sed '$$d'
ALL_FUNCTIONS: getFunctions R-funs ${BASE} ${Names}
	getFunctions ${BASE}  ${Names} > $@
Builtins: $(Exe)
	echo 'write(objects(pos=length(search())), "", nc=1)'| $(Exe) \
	 | sed '1,/write(/d'  > $@

DOCUMENTED: $(manDIR)/*
	getDoc $(manDIR) > $@

UNDOCUMENTED: diff_A_D
	 grep '^<' $^ | sed 's/^< //' > $@
DOC_unfun: diff_A_D
	 grep '^>' $^ | sed 's/^> //' > $@

diff_A_D: ALL_FUNCTIONS.srt DOCUMENTED.srt
	-diff $^ > $@

unBuiltin: diff_A_B
	 grep '^<' $^ | sed 's/^< //' > $@
Built_unfun: diff_A_B
	 grep '^>' $^ | sed 's/^> //' > $@

diff_A_B: ALL_FUNCTIONS.srt Builtins.srt
	-diff $^ > $@

# Those which lack an  EXAMPLES  section:
No-Examples: NoEx $(manDIR)/*
	NoEx $(manDIR) > $@

# All EXAMPLES  as a 'source()able' R-file: ---> now  RHOME/etc/extract-Examples
Examples: extrAllExamp DOCUMENTED
	extrAllExamp `sed 's/\*/\\\*/' DOCUMENTED` > $@
Examples.out: Examples
	echo "postscript('Examples.ps')"| cat - $^ | $(Exe) > $@
clean:
	@rm -f $(Targets) *.srt ALL_FUNCTIONS *DOCUMENTED DOC_* \
		diff_A_B diff_A_D\
		Examples.out Examples.ps data mean.out foo .RData

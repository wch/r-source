## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

all: before $(SHLIB) after

## do it this way as packages might add dependencies to this target
$(SHLIB): $(OBJECTS)
	@echo EXPORTS > tmp.def
	@$(NM) $^ | $(SED) -n 's/^.* [BCDRT] _/ /p' >> tmp.def
	$(SHLIB_LD) -shared $(DLLFLAGS) -o $@ tmp.def $(OBJECTS) $(ALL_LIBS)
	@$(RM) tmp.def

.PHONY: all before after shlib-clean
shlib-clean:
	@rm -f $(OBJECTS)

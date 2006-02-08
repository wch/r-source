## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

$(SHLIB): $(OBJECTS)
	$(SHLIB_LINK) -o $@ $(OBJECTS) $(ALL_LIBS)

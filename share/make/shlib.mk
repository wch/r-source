## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc/Makeconf

$(SHLIB): $(OBJS)
	$(SHLIB_LINK) -o $@ $(OBJS) $(ALL_LIBS)

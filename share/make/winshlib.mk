## ${R_HOME}/share/make/shlib.mk

include $(R_HOME)/etc${R_ARCH}/Makeconf

all: before $(SHLIB) after
$(SHLIB): $(OBJECTS)

.PHONY: all before after shlib-clean
shlib-clean:
	@rm -f $(OBJECTS)

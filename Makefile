
CPPFLAGS = -I/usr/local/include/lua53
WARNFLAGS = -Wall -Wextra -Wimplicit-fallthrough -Wno-unused-parameter
WERROR = -Werror
CFLAGS = $(CPPFLAGS) -g -O2 $(WARNFLAGS) $(WERROR) -fPIC -fvisibility=hidden
LDFLAGS = -shared

OBJS = lspawn.o myspawn.o simple_class.o auto_close.o

lspawn.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

$(OBJS): Makefile lspawn.h simple_class.h auto_close.h myspawn.h utils.h

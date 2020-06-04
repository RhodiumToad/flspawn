
CPPFLAGS = -I/usr/local/include/lua53
CFLAGS = $(CPPFLAGS) -g -O2 -Wall -Werror -Wextra -Wimplicit-fallthrough -fPIC -fvisibility=hidden
LDFLAGS = -shared

OBJS = lspawn.o myspawn.o simple_class.o auto_close.o

../lspawn.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

$(OBJS): Makefile lspawn.h simple_class.h auto_close.h myspawn.h utils.h

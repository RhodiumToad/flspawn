
MODNAME = flspawn
MODVERS = 1.1

LUA_INCDIR?= /usr/local/include/lua54
CPPFLAGS = -I$(LUA_INCDIR) -Imyspawn -DMODNAME=$(MODNAME) -DMODVERS=$(MODVERS)
WARNFLAGS = -Wall -Wextra -Wimplicit-fallthrough -Wno-unused-parameter \
		-Wno-unreachable-code-return
WERROR = -Werror
COPT = -O2
#SANITIZE = -fsanitize=undefined -fsanitize=integer -fsanitize-trap=all
#COV_CFLAGS = --coverage -DCOVERAGE
#COV_LDFLAGS = --coverage
#COPT = -O0
#CC = clang10
CFLAGS = $(CPPFLAGS) -g $(COPT) $(COV_CFLAGS) $(SANITIZE) $(WARNFLAGS) $(WERROR) -fPIC -fvisibility=hidden
LDFLAGS = -shared $(COV_LDFLAGS)
OBJS = lspawn.o myspawn/myspawn.o get_nfiles.o simple_class.o auto_close.o

$(MODNAME).so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

$(OBJS): Makefile lspawn.h get_nfiles.h simple_class.h auto_close.h myspawn/myspawn.h utils.h

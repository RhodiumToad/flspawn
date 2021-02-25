
MODNAME = flspawn

LUA_INCDIR?= /usr/local/include/lua54
CPPFLAGS = -I$(LUA_INCDIR) -DMODNAME=$(MODNAME)
WARNFLAGS = -Wall -Wextra -Wimplicit-fallthrough -Wno-unused-parameter
WERROR = -Werror
COPT = -O2
#COV_CFLAGS = --coverage -DCOVERAGE
#COV_LDFLAGS = --coverage
#COPT = -O0
#CC = clang10
CFLAGS = $(CPPFLAGS) -g $(COPT) $(COV_CFLAGS) $(WARNFLAGS) $(WERROR) -fPIC -fvisibility=hidden
LDFLAGS = -shared $(COV_LDFLAGS)
OBJS = lspawn.o myspawn.o get_nfiles.o simple_class.o auto_close.o

$(MODNAME).so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

$(OBJS): Makefile lspawn.h get_nfiles.h simple_class.h auto_close.h myspawn.h utils.h

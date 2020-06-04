/*
 * lspawn.h
 */

#ifndef H_9953AEC5_9FA0_11EA_A8A6_6CF049962D5A_
#define H_9953AEC5_9FA0_11EA_A8A6_6CF049962D5A_

#include <lua.h>
#include <lauxlib.h>

#include "utils.h"
#include "simple_class.h"


enum lspawn_fa_type {
    FA_CLOSE = 0,
    FA_NULL,
    FA_INHERIT,
	FA_INPIPE,
	FA_OUTPIPE,
    FA_OPEN,
    FA_INHERIT_FROM,
    FA_COPY_FROM,

    FA_LAST
};

struct lspawn_fa_data
{
    enum lspawn_fa_type type;
    int value1;
    int value2;
};


#endif

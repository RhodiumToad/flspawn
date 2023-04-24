/*
 * utils.h
 */

#ifndef H_E9D5E65D_9E2F_11EA_A8A6_6CF049962D5A_
#define H_E9D5E65D_9E2F_11EA_A8A6_6CF049962D5A_

#ifndef __has_builtin
#define __has_builtin(x_) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x_) 0
#endif
#ifndef __has_warning
#define __has_warning(x_) 0
#endif

#ifdef __clang__
#define W_CONSTANT_COMPARE_ "GCC diagnostic ignored \"-Wtautological-constant-out-of-range-compare\""
#else
#define W_CONSTANT_COMPARE_ "GCC diagnostic ignored \"-Wtype-limits\""
#endif

#define NO_WARN_CONSTANT_COMPARE_BEGIN _Pragma("GCC diagnostic push") _Pragma(W_CONSTANT_COMPARE_)
#define NO_WARN_CONSTANT_COMPARE_END _Pragma("GCC diagnostic pop")

#if !defined(__builtin_expect) && !defined(__GNUC__) && !__has_builtin(__builtin_expect)
#define __builtin_expect(x_,y_) (x_)
#endif

#if __has_attribute(__fallthrough__)
#define FALLTHROUGH __attribute__((__fallthrough__))
#else
#define FALLTHROUGH /* FALLTHROUGH */
#endif

#if defined(__GNUC__) || __has_attribute(__noinline__)
#define	NO_INLINE	__attribute__((__noinline__))
#else
#define	NO_INLINE
#endif

#if defined(__GNUC__) || __has_attribute(__noreturn__)
#define	NO_RETURN	__attribute__((__noreturn__))
#else
#define	NO_RETURN
#endif

#if defined(__GNUC__) || __has_attribute(__visibility__)
#define	EXPORTED	__attribute__((__visibility__("default")))
#else
#define	EXPORTED
#endif

#define likely(x)	(__builtin_expect(!!(x), 1))
#define unlikely(x)	(__builtin_expect(!!(x), 0))

#define countof(x) ((ssize_t)(sizeof(x) / sizeof((x)[0])))

#define CONCAT2(x,y) x##y
#define CONCAT(x,y) CONCAT2(x,y)
#define STRINGIFY2(a_) #a_
#define STRINGIFY(a_) STRINGIFY2(a_)

#if defined(LUA_VERSION_NUM) && defined(LUA_API)
LUA_API int   (lua_error) (lua_State *L) NO_RETURN;
#endif
#if defined(LUA_ERRFILE) && defined(LUALIB_API)
LUALIB_API int (luaL_argerror) (lua_State *L, int arg, const char *extramsg) NO_RETURN;
LUALIB_API int (luaL_typeerror) (lua_State *L, int arg, const char *tname) NO_RETURN;
LUALIB_API int (luaL_error) (lua_State *L, const char *fmt, ...) NO_RETURN;
#endif

#endif

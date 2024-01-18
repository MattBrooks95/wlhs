module Xkbcommon.Context where

#include <xkbcommon/context.h>

-- TODO is this enum template grabbing the numerical values from the
-- c pre processor?
--XKB_LOG_LEVEL_CRITICAL = 10, /**< Log critical internal errors only. */
--XKB_LOG_LEVEL_ERROR = 20,    /**< Log all errors. */
--XKB_LOG_LEVEL_WARNING = 30,  /**< Log warnings and errors. */
--XKB_LOG_LEVEL_INFO = 40,     /**< Log information, warnings, and errors. */
--XKB_LOG_LEVEL_DEBUG = 50     /**< Log everything. */
{{ enum
    XKB_log_level,
    XKB_LOG_LEVEL_CRITICAL,
    XKB_LOG_LEVEL_ERROR,
    XKB_LOG_LEVEL_WARNING,
    XKB_LOG_LEVEL_INFO,
    XKB_LOG_LEVEL_DEBUG,
};

{{ struct
    context.h,
    xkb_context,
    refcnt, CInt
    log_level, XKB_log_level
    log_verbosity, CInt
}}
--    ATTR_PRINTF(3, 0) void (*log_fn)(struct xkb_context *ctx,
--                                     enum xkb_log_level level,
--                                     const char *fmt, va_list args);

--struct xkb_context {
--    ATTR_PRINTF(3, 0) void (*log_fn)(struct xkb_context *ctx,
--                                     enum xkb_log_level level,
--                                     const char *fmt, va_list args);
--    enum xkb_log_level log_level;
--    int log_verbosity;
--    void *user_data;
--
--    struct xkb_rule_names names_dflt;
--
--    darray(char *) includes;
--    darray(char *) failed_includes;
--
--    struct atom_table *atom_table;
--
--    /* Used and allocated by xkbcommon-x11, free()d with the context. */
--    void *x11_atom_cache;
--
--    /* Buffer for the *Text() functions. */
--    char text_buffer[2048];
--    size_t text_next;
--
--    unsigned int use_environment_names : 1;
--    unsigned int use_secure_getenv : 1;
--};

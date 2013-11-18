! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

MODULE fluac
INTERFACE
  !===================================================================
  ! State Manipulation
  !===================================================================
  !> LUALIB_API lua_State *(luaL_newstate) (void);
  FUNCTION luaL_newstate_c() BIND(C, NAME="luaL_newstate")
    USE ISO_C_BINDING
    TYPE(C_PTR) :: luaL_newstate_c
  END FUNCTION luaL_newstate_c
  !
  !
  !> LUA_API void       (lua_close) (lua_State *L);
  SUBROUTINE lua_close_c(L) BIND(C, NAME="lua_close")
    USE ISO_C_BINDING
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_close_c
  
  !===================================================================
  ! Base libraries
  !===================================================================
  !
  !
  !> LUALIB_API void (luaL_openlibs) (lua_State *L); 
  SUBROUTINE luaL_openlibs_c(L) BIND(C, name="luaL_openlibs")
    USE ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE luaL_openlibs_c

  !===================================================================
  ! Load and execute files
  !===================================================================
  !
  !
  !> LUALIB_API int (luaL_loadfile) (lua_State *L, const char *filename);
  FUNCTION luaL_loadfile_c(L, filename) BIND(C, name="luaL_loadfile")
    USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: filename
    INTEGER(KIND=C_INT) :: luaL_loadfile_c
  END FUNCTION luaL_loadfile_c
  !
  !
  !> LUALIB_API int (luaL_loadstring) (lua_State *L, const char *s);
  FUNCTION luaL_loadstring_c(L, script) BIND(C, name="luaL_loadstring")
    USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: script
    INTEGER(KIND=C_INT) :: luaL_loadstring_c
  END FUNCTION luaL_loadstring_c  
  !
  !
  !> LUA_API int   (lua_pcall) (lua_State *L, int nargs, int nresults, int errfunc);
  FUNCTION lua_pcall_c(L, nargs, nresults, errfunc) BIND(C, name="lua_pcall")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: nargs, nresults, errfunc
    INTEGER(KIND=C_INT) :: lua_pcall_c
  END FUNCTION lua_pcall_c
  
  !===================================================================
  ! Basic Stack Manipulation
  !===================================================================
  !
  !
  !> LUA_API int   (lua_gettop) (lua_State *L);
  FUNCTION lua_gettop_c(L) BIND(C, name="lua_gettop")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT) :: lua_gettop_c
  END FUNCTION lua_gettop_c
  !
  !
  !> LUA_API void  (lua_settop) (lua_State *L, int idx);
  SUBROUTINE lua_settop_c(L, idx) BIND(C, name="lua_settop")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_settop_c
  !
  !
  !> LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
  SUBROUTINE lua_pushvalue_c(L, idx) BIND(C, name="lua_pushvalue")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_pushvalue_c
  !
  !
  !> LUA_API void  (lua_remove) (lua_State *L, int idx);
  SUBROUTINE lua_remove_c(L, idx) BIND(C, name="lua_remove")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_remove_c
  !
  !
  !> LUA_API void  (lua_insert) (lua_State *L, int idx);
  SUBROUTINE lua_insert_c(L, idx) BIND(C, name="lua_insert")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_insert_c
  !
  !
  !> LUA_API void  (lua_replace) (lua_State *L, int idx);
  SUBROUTINE lua_replace_c(L, idx) BIND(C, name="lua_replace")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_replace_c
  !
  !
  !> LUA_API int   (lua_checkstack) (lua_State *L, int sz);
  FUNCTION lua_checkstack_c(L, sz) BIND(C, name="lua_checkstack")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: sz
    INTEGER(KIND=C_INT) :: lua_checkstack_c
  END FUNCTION lua_checkstack_c
  
  !===================================================================
  ! Access functions (stack -> fortran)
  !===================================================================
  !
  !
  !> LUA_API int             (lua_isnumber) (lua_State *L, int idx);
  FUNCTION lua_isnumber_c(L, n) BIND(C, name="lua_isnumber")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_isnumber_c
  END FUNCTION lua_isnumber_c
  !
  !
  !> LUA_API int (lua_isstring) (lua_State *L, int idx);
  FUNCTION lua_isstring_c(L, n) BIND(C, name="lua_isstring")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_isstring_c
  END FUNCTION lua_isstring_c
  !
  !
  !> LUA_API int             (lua_iscfunction) (lua_State *L, int idx);
  FUNCTION lua_iscfunction_c(L, idx) BIND(C, name="lua_iscfunction")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_INT) :: lua_iscfunction_c
  END FUNCTION lua_iscfunction_c
  !
  !
  !> LUA_API int             (lua_isuserdata) (lua_State *L, int idx);
  FUNCTION lua_isuserdata_c(L, idx) BIND(C, name="lua_isuserdata")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_INT) :: lua_isuserdata_c
  END FUNCTION lua_isuserdata_c
  !
  !
  !>LUA_API int             (lua_type) (lua_State *L, int idx);
  FUNCTION lua_type_c(L, idx) BIND(C, name="lua_type")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_INT) :: lua_type_c
  END FUNCTION lua_type_c
  !
  !
  !>LUA_API const char     *(lua_typename) (lua_State *L, int tp);
  FUNCTION lua_typename_c(L, idx) BIND(C, name="lua_typename")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR):: lua_typename_c
  END FUNCTION lua_typename_c
  !
  !
  !>LUA_API int            (lua_equal) (lua_State *L, int idx1, int idx2);
  FUNCTION lua_equal_c(L, idx1, idx2) BIND(C, name="lua_equal")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_equal_c
  END FUNCTION lua_equal_c
  !
  !
  !>LUA_API int            (lua_rawequal) (lua_State *L, int idx1, int idx2);
  FUNCTION lua_rawequal_c(L, idx1, idx2) BIND(C, name="lua_rawequal")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_rawequal_c
  END FUNCTION lua_rawequal_c
  !
  !
  !> LUA_API int            (lua_lessthan) (lua_State *L, int idx1, int idx2);
  FUNCTION lua_lessthan_c(L, idx1, idx2) BIND(C, name="lua_lessthan")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_lessthan_c
  END FUNCTION lua_lessthan_c
  !
  !
  !> LUA_API lua_Number      (lua_tonumber) (lua_State *L, int idx);
  FUNCTION lua_tonumber_c(L, n) BIND(C, name="lua_tonumber")
    USE ISO_C_BINDING, only: C_INT, C_DOUBLE, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    REAL(KIND=C_DOUBLE) :: lua_tonumber_c 
  END FUNCTION lua_tonumber_c
  !
  ! *** lua_tointeger doesn't work for some reason
  !
  !> LUA_API lua_Integer     (lua_tointeger) (lua_State *L, int idx);
  !FUNCTION lua_tointeger_c(L, n) BIND(C, name="lua_tointeger")
  !  USE ISO_C_BINDING, only: C_INT, C_PTR, C_INT
  !  IMPLICIT NONE
  !  TYPE(C_PTR), VALUE :: L
  !  INTEGER(KIND=C_INT), VALUE :: n
  !  ! NOTE: The type of 'lua_tonumber_c' depends on how the integer value
  !  ! is defined in lua.
  !  REAL(KIND=C_INT) :: lua_tointeger_c
  !END FUNCTION lua_tointeger_c
  !
  !
  !> LUA_API int             (lua_toboolean) (lua_State *L, int idx);
  FUNCTION lua_toboolean_c(L, idx) BIND(C, name="lua_toboolean")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    Logical(KIND=C_BOOL) :: lua_toboolean_c
  END FUNCTION   lua_toboolean_c
  !
  !
  !> LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
  FUNCTION lua_tolstring_c(L, i, length) BIND(C, name="lua_tolstring")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: i
    INTEGER(KIND=C_SIZE_T), INTENT(OUT) :: length
    TYPE(C_PTR):: lua_tolstring_c
  END FUNCTION lua_tolstring_c
  !
  !
  !> LUA_API size_t          (lua_objlen) (lua_State *L, int idx);
  FUNCTION lua_objlen_c(L, idx) BIND(C, name="lua_objlen")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_SIZE_T) :: lua_objlen_c
  END FUNCTION lua_objlen_c
  !
  !
  !> LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
  FUNCTION lua_tocfunction_c(L, idx) BIND(C, name="lua_tocfunction")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_FUNPTR) :: lua_tocfunction_c
  END FUNCTION lua_tocfunction_c
  !
  !
  !> LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
  FUNCTION lua_touserdata_c(L, idx) BIND(C, name="lua_touserdata")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_touserdata_c
  END FUNCTION lua_touserdata_c
  !
  !
  !> LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
  FUNCTION lua_tothread_c(L, idx) BIND(C, name="lua_tothread")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_tothread_c
  END FUNCTION lua_tothread_c
  !
  !
  !> LUA_API const void     *(lua_topointer) (lua_State *L, int idx);
  FUNCTION lua_topointer_c(L, idx) BIND(C, name="lua_topointer")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_topointer_c
  END FUNCTION lua_topointer_c

  !===================================================================
  ! Push functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_pushnil) (lua_State *L);
  SUBROUTINE lua_pushnil_c(L) BIND(C, name="lua_pushnil")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_pushnil_c
  !
  !
  !> LUA_API void  (lua_pushnumber) (lua_State *L, lua_Number n);
  SUBROUTINE lua_pushnumber_c(L, n) BIND(C, name="lua_pushnumber")
    USE ISO_C_BINDING, only: C_PTR, C_DOUBLE
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default    
    REAL(KIND=C_DOUBLE), VALUE :: n  
  END SUBROUTINE lua_pushnumber_c
  !
  ! The integer calls are not working. Haven't figured out why yet.
  !
  !
  !> LUA_API void  (lua_pushinteger) (lua_State *L, lua_Integer n);  
  !SUBROUTINE lua_pushinteger_c(L, n) BIND(C, name="lua_pushinteger")
  !  USE ISO_C_BINDING, only: C_LONG, C_PTR
  !  IMPLICIT NONE
  !  TYPE(C_PTR), VALUE :: L
  !  ! NOTE: The type of 'n' depends on how the integer value is
  !  ! defined in lua. Lua uses ptrdiff_t by default, which is either 
  !  ! int or long.
  !  INTEGER(KIND=C_LONG), VALUE :: n
  !END SUBROUTINE

!> LUA_API void  (lua_pushlstring) (lua_State *L, const char *s, size_t len);
!> LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt, va_list argp);
!> LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);

  !
  !
  !> LUA_API void  (lua_pushstring) (lua_State *L, const char *s);
  SUBROUTINE lua_pushstring_c(L, str) BIND(C, name="lua_pushstring")
    USE ISO_C_BINDING, ONLY:C_CHAR, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: str
  END SUBROUTINE lua_pushstring_c
  !
  !
  !> LUA_API void  (lua_pushcclosure) (lua_State *L, lua_CFunction fn, int n);
  SUBROUTINE lua_pushcclosure_c(L, fn, n) BIND(C, name="lua_pushcclosure")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(KIND=C_INT), VALUE :: n
  END SUBROUTINE lua_pushcclosure_c
  !
  !
  !> LUA_API void  (lua_pushboolean) (lua_State *L, int b);
  SUBROUTINE lua_pushboolean_c(L, b) BIND(C, name="lua_pushboolean")
    USE ISO_C_BINDING, ONLY: C_PTR, C_BOOL
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    LOGICAL(C_BOOL), VALUE :: b
  END SUBROUTINE lua_pushboolean_c
  !
  !
  !> LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *ptr);
  SUBROUTINE lua_pushlightuserdata_c(L, ptr) BIND(C, name="lua_pushlightuserdata")
    USE ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    TYPE(C_PTR), VALUE :: ptr
  END SUBROUTINE lua_pushlightuserdata_c

!> LUA_API int   (lua_pushthread) (lua_State *L);

  !===================================================================
  ! Get functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_gettable) (lua_State *L, int idx);
  SUBROUTINE lua_gettable_c(L, idx) BIND(C, name="lua_gettable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_gettable_c
  !
  !
  !> LUA_API void  (lua_getfield) (lua_State *L, int idx, const char *k);
  SUBROUTINE lua_getfield_c(L, n, k) BIND(C, name="lua_getfield")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: k 
  END SUBROUTINE lua_getfield_c
  !
  !
  !> LUA_API void  (lua_rawget) (lua_State *L, int idx);
  SUBROUTINE lua_rawget_c(L, idx) BIND(C, name="lua_rawget")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_rawget_c
  !
  !
  !> LUA_API void  (lua_rawgeti) (lua_State *L, int idx, int n);
  SUBROUTINE lua_rawgeti_c(L, idx, n) BIND(C, name="lua_rawgeti")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx, n
  END SUBROUTINE lua_rawgeti_c
  !
  !
  !> LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
  SUBROUTINE lua_createtable_c(L, narr, nrec) BIND(C, name="lua_createtable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: narr, nrec
  END SUBROUTINE lua_createtable_c
  !
  !
  !> LUA_API void *(lua_newuserdata) (lua_State *L, size_t sz);
  FUNCTION lua_newuserdata_c(L, sz) BIND(C, name="lua_newuserdata")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_SIZE_T), VALUE :: sz
    TYPE(C_PTR) :: lua_newuserdata_c
  END FUNCTION lua_newuserdata_c
  !
  !
  !> LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
  FUNCTION lua_getmetatable_c(L, objindex) BIND(C, name="lua_getmetatable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: objindex
    INTEGER(KIND=C_INT) :: lua_getmetatable_c
  END FUNCTION lua_getmetatable_c
  !
  !
  !> LUA_API void  (lua_getfenv) (lua_State *L, int idx);
  SUBROUTINE lua_getfenv_c(L, idx) BIND(C, name="lua_getfenv")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_getfenv_c

  !===================================================================
  ! Set functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_settable) (lua_State *L, int idx);
  SUBROUTINE lua_settable_c(L, idx) BIND(C, name="lua_settable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_settable_c
  !
  !
  !> LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
  SUBROUTINE lua_setfield_c(L, idx, k) BIND(C, name="lua_setfield")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: k
  END SUBROUTINE lua_setfield_c
  !
  !
  !> LUA_API void  (lua_rawset) (lua_State *L, int idx);
  SUBROUTINE lua_rawset_c(L, idx) BIND(C, name="lua_rawset")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_rawset_c
  !
  !
  !> LUA_API void  (lua_rawseti) (lua_State *L, int idx, int n);
  SUBROUTINE lua_rawseti_c(L, idx, n) BIND(C, name="lua_rawseti")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx, n
  END SUBROUTINE lua_rawseti_c
  !
  !
  !> LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
  SUBROUTINE lua_setmetatable_c(L, objindex) BIND(C, name="lua_setmetatable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: objindex
  END SUBROUTINE lua_setmetatable_c
  !
  !
  !> LUA_API int   (lua_setfenv) (lua_State *L, int idx);  
  SUBROUTINE lua_setfenv_c(L, idx) BIND(C, name="lua_setfenv")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_setfenv_c
  
  !===================================================================
  !
  !
  !> LUA_API int   (lua_next) (lua_State *L, int idx);
  FUNCTION lua_next_c(L, n) BIND(C, name="lua_next")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    INTEGER(KIND=C_INT) :: lua_next_c
  END FUNCTION lua_next_c
  !
  !
  !> LUALIB_API const char *(luaL_checklstring) (lua_State *L, int numArg, size_t *len);
  FUNCTION luaL_checklstring_c(L, numArg, len) BIND(C, name="luaL_checklstring")
    USE ISO_C_BINDING, ONLY:C_PTR, C_INT, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: numArg
    TYPE(C_PTR) :: len
    TYPE(C_PTR) :: luaL_checklstring_c
  END FUNCTION luaL_checklstring_c  
  !
END INTERFACE
END MODULE fluac













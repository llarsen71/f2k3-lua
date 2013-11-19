! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

MODULE flua
  USE ISO_C_BINDING, ONLY: C_CHAR
  PRIVATE :: p_characterToCharArray
  INTEGER, PARAMETER :: LUA_MULTRET = -1
  INTEGER, PARAMETER :: LUA_GLOBALSINDEX = -10002

  ! LUA types
  INTEGER, PARAMETER :: LUA_TNONE = -1
  INTEGER, PARAMETER :: LUA_TNIL = 0
  INTEGER, PARAMETER :: LUA_TBOOLEAN = 1
  INTEGER, PARAMETER :: LUA_TLIGHTUSERDATA = 2
  INTEGER, PARAMETER :: LUA_TNUMBER = 3
  INTEGER, PARAMETER :: LUA_TSTRING = 4
  INTEGER, PARAMETER :: LUA_TTABLE = 5
  INTEGER, PARAMETER :: LUA_TFUNCTION = 6
  INTEGER, PARAMETER :: LUA_TUSERDATA = 7
  INTEGER, PARAMETER :: LUA_TTHREAD = 8

  TYPE cStrPTR
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(:), pointer :: str
  END TYPE cStrPTR

INTERFACE
  !===================================================================
  ! State Manipulation
  !===================================================================
  !> LUALIB_API lua_State *(luaL_newstate) (void);
  FUNCTION luaL_newstate() BIND(C, NAME="luaL_newstate")
    USE ISO_C_BINDING
    TYPE(C_PTR) :: luaL_newstate
  END FUNCTION luaL_newstate
  !
  !
  !> LUA_API void       (lua_close) (lua_State *L);
  SUBROUTINE lua_close(L) BIND(C, NAME="lua_close")
    USE ISO_C_BINDING
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_close

  !===================================================================
  ! Base libraries
  !===================================================================
  !
  !
  !> LUALIB_API void (luaL_openlibs) (lua_State *L);
  SUBROUTINE luaL_openlibs(L) BIND(C, name="luaL_openlibs")
    USE ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE luaL_openlibs

  !===================================================================
  ! Load and execute files
  !===================================================================
  !
  !
  !> LUALIB_API int (luaL_loadfile) (lua_State *L, const char *filename);
  FUNCTION luaL_loadfile(L, filename) BIND(C, name="luaL_loadfile")
    USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: filename
    INTEGER(KIND=C_INT) :: luaL_loadfile
  END FUNCTION luaL_loadfile
  !
  !
  !> LUALIB_API int (luaL_loadstring) (lua_State *L, const char *s);
  FUNCTION luaL_loadstring(L, script) BIND(C, name="luaL_loadstring")
    USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(*) :: script
    INTEGER(KIND=C_INT) :: luaL_loadstring
  END FUNCTION luaL_loadstring
  !
  !
  !> LUA_API int   (lua_pcall) (lua_State *L, int nargs, int nresults, int errfunc);
  FUNCTION lua_pcall(L, nargs, nresults, errfunc) BIND(C, name="lua_pcall")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: nargs, nresults, errfunc
    INTEGER(KIND=C_INT) :: lua_pcall
  END FUNCTION lua_pcall

  !===================================================================
  ! Basic Stack Manipulation
  !===================================================================
  !
  !
  !> LUA_API int   (lua_gettop) (lua_State *L);
  FUNCTION lua_gettop(L) BIND(C, name="lua_gettop")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT) :: lua_gettop
  END FUNCTION lua_gettop
  !
  !
  !> LUA_API void  (lua_settop) (lua_State *L, int idx);
  SUBROUTINE lua_settop(L, idx) BIND(C, name="lua_settop")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_settop
  !
  !
  !> LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
  SUBROUTINE lua_pushvalue(L, idx) BIND(C, name="lua_pushvalue")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_pushvalue
  !
  !
  !> LUA_API void  (lua_remove) (lua_State *L, int idx);
  SUBROUTINE lua_remove(L, idx) BIND(C, name="lua_remove")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_remove
  !
  !
  !> LUA_API void  (lua_insert) (lua_State *L, int idx);
  SUBROUTINE lua_insert(L, idx) BIND(C, name="lua_insert")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_insert
  !
  !
  !> LUA_API void  (lua_replace) (lua_State *L, int idx);
  SUBROUTINE lua_replace(L, idx) BIND(C, name="lua_replace")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_replace
  !
  !
  !> LUA_API int   (lua_checkstack) (lua_State *L, int sz);
  FUNCTION lua_checkstack(L, sz) BIND(C, name="lua_checkstack")
    USE ISO_C_BINDING, ONLY: C_PTR, C_INT
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: sz
    INTEGER(KIND=C_INT) :: lua_checkstack
  END FUNCTION lua_checkstack

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
  FUNCTION lua_type(L, idx) BIND(C, name="lua_type")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_INT) :: lua_type
  END FUNCTION lua_type
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
  FUNCTION lua_equal(L, idx1, idx2) BIND(C, name="lua_equal")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_equal
  END FUNCTION lua_equal
  !
  !
  !>LUA_API int            (lua_rawequal) (lua_State *L, int idx1, int idx2);
  FUNCTION lua_rawequal(L, idx1, idx2) BIND(C, name="lua_rawequal")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_rawequal
  END FUNCTION lua_rawequal
  !
  !
  !> LUA_API int            (lua_lessthan) (lua_State *L, int idx1, int idx2);
  FUNCTION lua_lessthan(L, idx1, idx2) BIND(C, name="lua_lessthan")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx1, idx2
    INTEGER(KIND=C_INT) :: lua_lessthan
  END FUNCTION lua_lessthan
  !
  !
  !> LUA_API lua_Number      (lua_tonumber) (lua_State *L, int idx);
  FUNCTION lua_tonumber(L, n) BIND(C, name="lua_tonumber")
    USE ISO_C_BINDING, only: C_INT, C_DOUBLE, C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: n
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    REAL(KIND=C_DOUBLE) :: lua_tonumber
  END FUNCTION lua_tonumber
  !
  ! *** lua_tointeger doesn't work for some reason
  !
  !> LUA_API lua_Integer     (lua_tointeger) (lua_State *L, int idx);
  !FUNCTION lua_tointeger(L, n) BIND(C, name="lua_tointeger")
  !  USE ISO_C_BINDING, only: C_INT, C_PTR, C_INT
  !  IMPLICIT NONE
  !  TYPE(C_PTR), VALUE :: L
  !  INTEGER(KIND=C_INT), VALUE :: n
  !  ! NOTE: The type of 'lua_tonumber' depends on how the integer value
  !  ! is defined in lua.
  !  REAL(KIND=C_INT) :: lua_tointeger
  !END FUNCTION lua_tointeger
  !
  !
  !> LUA_API int             (lua_toboolean) (lua_State *L, int idx);
  FUNCTION lua_toboolean(L, idx) BIND(C, name="lua_toboolean")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    Logical(KIND=C_BOOL) :: lua_toboolean
  END FUNCTION   lua_toboolean
  !
  !
  !> LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
  FUNCTION lua_tolstring(L, i, length) BIND(C, name="lua_tolstring")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: i
    INTEGER(KIND=C_SIZE_T), INTENT(OUT) :: length
    TYPE(C_PTR):: lua_tolstring
  END FUNCTION lua_tolstring
  !
  !
  !> LUA_API size_t          (lua_objlen) (lua_State *L, int idx);
  FUNCTION lua_objlen(L, idx) BIND(C, name="lua_objlen")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    INTEGER(KIND=C_SIZE_T) :: lua_objlen
  END FUNCTION lua_objlen
  !
  !
  !> LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
  FUNCTION lua_tocfunction(L, idx) BIND(C, name="lua_tocfunction")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_FUNPTR) :: lua_tocfunction
  END FUNCTION lua_tocfunction
  !
  !
  !> LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
  FUNCTION lua_touserdata(L, idx) BIND(C, name="lua_touserdata")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_touserdata
  END FUNCTION lua_touserdata
  !
  !
  !> LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
  FUNCTION lua_tothread(L, idx) BIND(C, name="lua_tothread")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_tothread
  END FUNCTION lua_tothread
  !
  !
  !> LUA_API const void     *(lua_topointer) (lua_State *L, int idx);
  FUNCTION lua_topointer(L, idx) BIND(C, name="lua_topointer")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
    TYPE(C_PTR) :: lua_topointer
  END FUNCTION lua_topointer

  !===================================================================
  ! Push functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_pushnil) (lua_State *L);
  SUBROUTINE lua_pushnil(L) BIND(C, name="lua_pushnil")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
  END SUBROUTINE lua_pushnil
  !
  !
  !> LUA_API void  (lua_pushnumber) (lua_State *L, lua_Number n);
  SUBROUTINE lua_pushnumber(L, n) BIND(C, name="lua_pushnumber")
    USE ISO_C_BINDING, only: C_PTR, C_DOUBLE
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    REAL(KIND=C_DOUBLE), VALUE :: n
  END SUBROUTINE lua_pushnumber
  !
  ! The integer calls are not working. Haven't figured out why yet.
  !
  !
  !> LUA_API void  (lua_pushinteger) (lua_State *L, lua_Integer n);
  !SUBROUTINE lua_pushinteger(L, n) BIND(C, name="lua_pushinteger")
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
  SUBROUTINE lua_pushcclosure(L, fn, n) BIND(C, name="lua_pushcclosure")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    TYPE(C_FUNPTR), VALUE :: fn
    INTEGER(KIND=C_INT), VALUE :: n
  END SUBROUTINE lua_pushcclosure
  !
  !
  !> LUA_API void  (lua_pushboolean) (lua_State *L, int b);
  SUBROUTINE lua_pushboolean(L, b) BIND(C, name="lua_pushboolean")
    USE ISO_C_BINDING, ONLY: C_PTR, C_BOOL
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    LOGICAL(C_BOOL), VALUE :: b
  END SUBROUTINE lua_pushboolean
  !
  !
  !> LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *ptr);
  SUBROUTINE lua_pushlightuserdata(L, ptr) BIND(C, name="lua_pushlightuserdata")
    USE ISO_C_BINDING, ONLY: C_PTR
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    TYPE(C_PTR), VALUE :: ptr
  END SUBROUTINE lua_pushlightuserdata

!> LUA_API int   (lua_pushthread) (lua_State *L);

  !===================================================================
  ! Get functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_gettable) (lua_State *L, int idx);
  SUBROUTINE lua_gettable(L, idx) BIND(C, name="lua_gettable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_gettable
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
  SUBROUTINE lua_rawget(L, idx) BIND(C, name="lua_rawget")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_rawget
  !
  !
  !> LUA_API void  (lua_rawgeti) (lua_State *L, int idx, int n);
  SUBROUTINE lua_rawgeti(L, idx, n) BIND(C, name="lua_rawgeti")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx, n
  END SUBROUTINE lua_rawgeti
  !
  !
  !> LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
  SUBROUTINE lua_createtable(L, narr, nrec) BIND(C, name="lua_createtable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: narr, nrec
  END SUBROUTINE lua_createtable
  !
  !
  !> LUA_API void *(lua_newuserdata) (lua_State *L, size_t sz);
  FUNCTION lua_newuserdata(L, sz) BIND(C, name="lua_newuserdata")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_SIZE_T), VALUE :: sz
    TYPE(C_PTR) :: lua_newuserdata
  END FUNCTION lua_newuserdata
  !
  !
  !> LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
  FUNCTION lua_getmetatable(L, objindex) BIND(C, name="lua_getmetatable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: objindex
    INTEGER(KIND=C_INT) :: lua_getmetatable
  END FUNCTION lua_getmetatable
  !
  !
  !> LUA_API void  (lua_getfenv) (lua_State *L, int idx);
  SUBROUTINE lua_getfenv(L, idx) BIND(C, name="lua_getfenv")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_getfenv

  !===================================================================
  ! Set functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_settable) (lua_State *L, int idx);
  SUBROUTINE lua_settable(L, idx) BIND(C, name="lua_settable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_settable
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
  SUBROUTINE lua_rawset(L, idx) BIND(C, name="lua_rawset")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_rawset
  !
  !
  !> LUA_API void  (lua_rawseti) (lua_State *L, int idx, int n);
  SUBROUTINE lua_rawseti(L, idx, n) BIND(C, name="lua_rawseti")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx, n
  END SUBROUTINE lua_rawseti
  !
  !
  !> LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
  SUBROUTINE lua_setmetatable(L, objindex) BIND(C, name="lua_setmetatable")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: objindex
  END SUBROUTINE lua_setmetatable
  !
  !
  !> LUA_API int   (lua_setfenv) (lua_State *L, int idx);
  SUBROUTINE lua_setfenv(L, idx) BIND(C, name="lua_setfenv")
    USE ISO_C_BINDING
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: idx
  END SUBROUTINE lua_setfenv

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
  FUNCTION luaL_checklstring(L, numArg, len) BIND(C, name="luaL_checklstring")
    USE ISO_C_BINDING, ONLY:C_PTR, C_INT, C_SIZE_T
    IMPLICIT NONE
    TYPE(C_PTR), VALUE :: L
    INTEGER(KIND=C_INT), VALUE :: numArg
    TYPE(C_PTR) :: len
    TYPE(C_PTR) :: luaL_checklstring
  END FUNCTION luaL_checklstring
  !
END INTERFACE


CONTAINS

!=====================================================================

FUNCTION cSTR(str, doTrim)
! This recieves a fortran string and creates a character array pointer
! that holds the string. The new string is compatible with c strings.
! The pointer must be deallocated when it is not longer of use. The
! value must be trimmed before sending to this function.
!
  USE ISO_C_BINDING, ONLY: C_NULL_CHAR
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN) :: str
  LOGICAL, OPTIONAL:: doTrim
  TYPE(cStrPTR) :: cSTR
  INTEGER :: i, lng
  LOGICAL :: doTrim_

  doTrim_ = .TRUE.
  IF (PRESENT(doTrim)) THEN
    doTrim_ = doTrim
  ENDIF

  IF (doTrim_) THEN
    lng = LEN_TRIM(str)
  ELSE
    lng = LEN(str)
  ENDIF

  ALLOCATE(cSTR%str(lng+1))
  DO i = 1, lng
    cSTR%str(i) = str(i:i)
  END DO
  cSTR%str(lng+1) = C_NULL_CHAR
END FUNCTION cSTR

!=====================================================================

FUNCTION cSTR2fSTR(cstr_, dealloc)
  IMPLICIT NONE
  TYPE(cStrPTR) :: cstr_
  LOGICAL, OPTIONAL :: dealloc
  CHARACTER(LEN=SIZE(cstr_%str)-1) :: cSTR2fSTR
  LOGICAL :: dealloc_
  INTEGER :: i, sz

! Fill in the fortran string
  sz = SIZE(cstr_%str)-1
  do i = 1, sz
    cSTR2fSTR(i:i) = cstr_%str(i)
  END DO

! Deallocate the string if specified.
  dealloc_ = .true.
  if (PRESENT(dealloc)) THEN
    dealloc_ = dealloc
  ENDIF
  if (dealloc_) THEN
    DEALLOCATE(cstr_%str)
  ENDIF
END FUNCTION cSTR2fSTR

!=====================================================================

SUBROUTINE p_characterToCharArray(fstr, charArray, error)
  USE ISO_C_BINDING, ONLY: C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: fstr
  CHARACTER(KIND=C_CHAR, LEN=1), DIMENSION(:), INTENT(OUT) :: charArray
  INTEGER, INTENT(OUT) :: error
  INTEGER :: i, lenTrim

  lenTrim = LEN_TRIM(fstr)
  error = 0
  IF(SIZE(charArray) < (lenTrim + 1)) THEN
    error = 1
    RETURN
  END IF
  !
  DO i = 1, lenTrim
    charArray(i) = fstr(i:i)
  END DO
  charArray(lenTrim+1) = C_NULL_CHAR
END SUBROUTINE p_characterToCharArray

!=====================================================================

SUBROUTINE C_F_CSTR(cptr, ret, length, as_cSTR)
  USE ISO_C_BINDING
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: cptr
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: ret
  INTEGER, INTENT(OUT) :: length
  LOGICAL, OPTIONAL, INTENT(IN) :: as_cSTR
  INTERFACE
    FUNCTION strlen(cstr_) BIND(C,NAME="strlen")
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER(KIND=C_SIZE_T) :: strlen
      TYPE(C_PTR), VALUE :: cstr_
    END FUNCTION strlen
  END INTERFACE

  length = strlen(CPTR)
  IF (PRESENT(as_cSTR)) THEN
    IF (as_cSTR) THEN
!     Add 1 to capture the \0 at the end of string.
      length = length + 1
    ENDIF
  ENDIF

  ! This is a builtin fortran function that converts c pointers to
  ! fortran pointers.
  CALL C_F_POINTER(FPTR=ret, CPTR=cptr, SHAPE=(/ length /))
END SUBROUTINE

!=====================================================================

SUBROUTINE flua_tostring(L, n, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T, C_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(OUT) :: str
  !
  TYPE(C_PTR) :: cstr_
  INTEGER(KIND=C_SIZE_T) :: length
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: fstr
  INTEGER :: i, cstrLength

  str = ""
  cstr_ = lua_tolstring(L, n, length)
  CALL C_F_CSTR(cstr_, fstr, cstrLength)
  !
  DO i = 1, cstrLength
    str(i:i) = fstr(i)
  END DO
END SUBROUTINE flua_tostring

!=====================================================================

FUNCTION lua_tostring(L, n)
! Returns the Lua string that is at the given stack position.
! The return value is a cStrPTR structure that contains an
! allocated character array. This needs to be deallocated
! manually. This can be done by calling the function
! cSTR2fSTR on the returned value to get a fortran string.
!
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  TYPE(cStrPTR) :: lua_tostring
  TYPE(C_PTR) :: cstr_
  INTEGER(KIND=C_SIZE_T) :: length
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: fstr
  INTEGER :: length2, i

  cstr_ = lua_tolstring(L, n, length)
  CALL C_F_CSTR(cstr_, fstr, length2, .TRUE.)
  ALLOCATE(lua_tostring%str(length2))

! Copy the Lua string over to the new string.
  DO i = 1, length2
    lua_tostring%str(i) = fstr(i)
  END DO
END FUNCTION lua_tostring

!=====================================================================

FUNCTION lua_typename(L, typeid)
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: typeid
  TYPE(cStrPTR) :: lua_typename
  TYPE(C_PTR) :: cstr_
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: fstr
  INTEGER :: length2, i

  cstr_ = lua_typename_c(L, typeid)
  CALL C_F_CSTR(cstr_, fstr, length2, .TRUE.)
  ALLOCATE(lua_typename%str(length2))

! Copy the Lua string over to the new string.
  DO i = 1, length2
    lua_typename%str(i) = fstr(i)
  END DO
END FUNCTION lua_typename

!=====================================================================

SUBROUTINE lua_getglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key

  CALL lua_getfield(L, LUA_GLOBALSINDEX, key)
END SUBROUTINE lua_getglobal

!=====================================================================

SUBROUTINE lua_setglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  !
  CALL lua_setfield(L,  LUA_GLOBALSINDEX, key)
END SUBROUTINE lua_setglobal

!=====================================================================

FUNCTION flua_checkglobal(L, key) RESULT(has_globalvar)
! Check whether a global value exists. If it does, leave it on the
! stack and return .TRUE. If the value does not exist, don't leave a
! value on the stack
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  LOGICAL has_globalvar

  CALL lua_getglobal(L, key)
  IF (lua_isnil(L, -1)) THEN
    ! If getglobal returned nil, the global variable didn't exist.
    ! Pop the nil value off the stack.
    call lua_pop(L, 1)
    has_globalvar = .FALSE.
  ELSE
    ! If the gloval value was found, leave it on the stack and
    ! return true.
    has_globalvar = .TRUE.
  ENDIF
END FUNCTION flua_checkglobal

!=====================================================================
! Access Function (stack -> fortran) MACROS
!=====================================================================

!#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
FUNCTION lua_isfunction(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isfunction

  lua_isfunction = (lua_type(L, n) == LUA_TFUNCTION)
END FUNCTION lua_isfunction

!=====================================================================

!#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
FUNCTION lua_istable(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_istable

  lua_istable = (lua_type(L,  n) == LUA_TTABLE)
END FUNCTION lua_istable

!=====================================================================

!#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
FUNCTION lua_islightuserdata(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_islightuserdata

  lua_islightuserdata = (lua_type(L,  n) == LUA_TLIGHTUSERDATA)
END FUNCTION lua_islightuserdata

!=====================================================================

!#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
FUNCTION lua_isnil(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnil

  lua_isnil = (lua_type(L,  n) == LUA_TNIL)
END FUNCTION lua_isnil

!=====================================================================

!#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
FUNCTION lua_isboolean(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isboolean

  lua_isboolean = (lua_type(L,  n) == LUA_TBOOLEAN)
END FUNCTION

!=====================================================================

!#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
FUNCTION lua_isthread(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isthread

  lua_isthread = (lua_type(L,  n) == LUA_TTHREAD)
END FUNCTION

!=====================================================================

!#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
FUNCTION lua_isnone(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnone

  lua_isnone = (lua_type(L,  n) == LUA_TNONE)
END FUNCTION

!=====================================================================

!#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
FUNCTION lua_isnoneornil(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnoneornil

  lua_isnoneornil = (lua_type(L,  n) <= 0)
END FUNCTION

!=====================================================================

FUNCTION lua_isnumber(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnumber
  !
  lua_isnumber = lua_isnumber_c(L,  n) /= 0
END FUNCTION lua_isnumber

!=====================================================================

FUNCTION lua_isstring(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isstring
  !
  lua_isstring = lua_isstring_c(L,  n) /= 0
END FUNCTION lua_isstring

!=====================================================================

FUNCTION lua_tointeger(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION :: dbl
  INTEGER :: lua_tointeger
  !
  dbl = lua_tonumber(L, n)
  lua_tointeger = dbl
END FUNCTION lua_tointeger

!=====================================================================

SUBROUTINE lua_pushinteger(L, val)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: val
  DOUBLE PRECISION :: valD
  !
  valD = val
  CALL lua_pushnumber(L, valD)
END SUBROUTINE lua_pushinteger

!=====================================================================

SUBROUTINE lua_pop(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  !
  CALL lua_settop(L, -n-1)
END SUBROUTINE lua_pop

!=====================================================================

SUBROUTINE lua_pushstring(L, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: str
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(str)+1) :: cstr_
  INTEGER :: error

  CALL p_characterToCharArray(str, cstr_, error)
  ! error cannot be /= 0
  !
  CALL lua_pushstring_c(L, cstr_)
  !
END SUBROUTINE lua_pushstring

!=====================================================================

SUBROUTINE lua_pushtable(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L

  ! Add a new table to the stack.
  CALL lua_createtable(L, 0, 0)
END SUBROUTINE lua_pushtable

!=====================================================================

!> #define lua_newtable(L)		lua_createtable(L, 0, 0)
SUBROUTINE lua_newtable(L, tablename)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: tablename

  ! Add a new table to the stack.
  CALL lua_createtable(L, 0, 0)

  ! If a table name was specified, add this as a global variable with the
  ! given table name.
  IF (PRESENT(tablename)) THEN
    ! Copy the table (which is located at stack location -1)
    CALL lua_pushvalue(L,-1)
    ! Add the table as a global variable with the given tablename.
    ! Note that this pops the copy of the table from the stack, but
    ! the original table is still there
    CALL lua_setglobal(L, tablename)
  ENDIF
END SUBROUTINE lua_newtable

!=====================================================================

FUNCTION lua_next(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_next
  !
  lua_next = lua_next_c(L, n) /= 0
END FUNCTION lua_next

!=====================================================================

SUBROUTINE lua_pushcfunction(L, fn)
  USE ISO_C_BINDING, ONLY: C_PTR, C_FUNPTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  TYPE(C_FUNPTR), INTENT(IN) :: fn
  !
  call lua_pushcclosure(L, fn, 0)
END SUBROUTINE lua_pushcfunction

!=====================================================================

SUBROUTINE lua_getfield(L, n, k)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(IN) :: k
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(k)+1) :: c_key
  INTEGER :: error

  CALL p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_getfield_c(L, n, c_key)
END SUBROUTINE lua_getfield

!=====================================================================

!> LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
SUBROUTINE lua_setfield(L, idx, k)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: idx
  CHARACTER(LEN=*), INTENT(IN) :: k
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(k)+1) :: c_key
  INTEGER :: error

  CALL p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_setfield_c(L, idx, c_key)
END SUBROUTINE lua_setfield

!=====================================================================

!> #define luaL_dofile(L, fn) (luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))
SUBROUTINE luaL_dofile(L, fileName, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: fileName
  INTEGER, INTENT(OUT) :: error
  CHARACTER*1000 :: msg
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(fileName)+1) :: cFileName

  CALL p_characterToCharArray(fileName, cFileName, error)
  ! error cannot be /= 0
  !
  ! TODO: report error string
  error = luaL_loadfile(L, cFileName)
  IF (error == 0) THEN
    error = lua_pcall(L, 0, LUA_MULTRET, 0)
    IF (error /= 0) THEN
      ! TODO: report error string
      call flua_tostring(L, -1, msg)
    ENDIF
  ENDIF
END SUBROUTINE luaL_dofile

!=====================================================================

!> #define luaL_dostring(L, s) (luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))
SUBROUTINE luaL_dostring(L, script, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: script
  INTEGER, INTENT(OUT) :: error
  CHARACTER*1000 :: msg
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(script)+1) :: cscript

  CALL p_characterToCharArray(script, cscript, error)
  ! error cannot be /= 0
  !
  ! TODO: report error string
  error = luaL_loadstring(L, cscript)
  IF (error == 0) THEN
    error = lua_pcall(L, 0, LUA_MULTRET, 0)
    IF (error /= 0) THEN
      ! TODO: report error string
      call flua_tostring(L, -1, msg)
    ENDIF
  ENDIF
END SUBROUTINE luaL_dostring

!=====================================================================

SUBROUTINE stackDump(L)
  USE ISO_C_BINDING
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER :: i, top, t

  top = lua_gettop(L)
  WRITE(6, "(A)") "start stack:"
  DO i = 1,top
    t = lua_type(L, i)
    SELECT CASE (t)
      CASE (LUA_TSTRING)
        WRITE(6, 10) cStr2fSTR(lua_tostring(L, i))
10      FORMAT('"', A, '" ')
      CASE (LUA_TBOOLEAN)
        WRITE(6, "(L)") lua_toboolean(L, i)
      CASE (LUA_TNUMBER)
        WRITE(6, "(G14.6)") lua_tonumber(L, i)
      CASE DEFAULT
        WRITE(6, "(A)") cStr2fSTR(lua_typename(L, t))
    END SELECT
  END DO
  WRITE(6, "(A)") "end stack"
END SUBROUTINE stackDump

!=====================================================================

!void dumpTable(lua_State* L, int idx)
!{
!  lua_pushnil(L);  /* first key */
!  printf("start:\n");
!  while (lua_next(L, idx) != 0) {
!    /* `key' is at index -2 and `value' at index -1 */
!    printf("%s - %s\n",
!      lua_tostring(L, -2), lua_typename(L, lua_type(L, -1)));
!    lua_pop(L, 1);  /* removes `value'; keeps `key' for next iteration */
!  }
!  printf("end\n");
!}
!
!void flua_free(void* ptr)
!{
!  free(ptr);
!}

!=====================================================================

!#define luaL_checkstring(L,n)	(luaL_checklstring(L, (n), NULL))
SUBROUTINE luaL_checkstring(L, n, fstr, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(OUT) :: fstr
  INTEGER, INTENT(OUT) :: error
  !
  TYPE(C_PTR) :: cstr_
  CHARACTER(LEN=1), DIMENSION(:), POINTER :: chars
  INTEGER :: i, length

  fstr = ""
  error = 0
  cstr_ = luaL_checklstring(L, n, C_NULL_PTR)
  CALL C_F_CSTR(cstr_, chars, length)
  IF (LEN(fstr) < length) THEN
    error = 1
    RETURN
  END IF
  DO i = 1, length
    fstr(i:i) = chars(i)
  END DO
END SUBROUTINE luaL_checkstring

!=====================================================================

SUBROUTINE flua_registerfunction(L, fnname, fn)
  USE ISO_C_BINDING, ONLY: C_PTR, C_FUNPTR
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: fnname
  TYPE(C_FUNPTR), INTENT(IN) :: fn

  ! Push the function on the stack.
  CALL lua_pushcfunction(L, fn)

  ! Assign the function to a global name
  CALL lua_setglobal(L, fnname);
END SUBROUTINE flua_registerfunction

!=====================================================================

SUBROUTINE flua_opensandboxlibs(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L

  call luaL_openlibs(L)
END SUBROUTINE flua_opensandboxlibs

!=====================================================================

FUNCTION getTblField(L, fieldname, type_, tableidx)
!
! Note that this puts table values at the top of the
! stack. These need to be removed by the calling subroutine.
!
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  character*(*) :: fieldname
  INTEGER, optional :: type_, tableidx
  LOGICAL :: getTblField
  INTEGER tblidx

  ! Default index is just above the fieldname that is
  ! added to the stack.
  tblidx = -2
  IF (PRESENT(tableidx)) THEN
    tblidx = tableidx-1
  ENDIF

  ! Put the field value on top of the stack
  CALL lua_pushstring(L, fieldname)
  CALL lua_gettable(L, tblidx)

  IF (PRESENT(type_)) THEN
    IF (lua_type(L, -1) /= type_) THEN
      CALL lua_pop(L, 1)
      getTblField = .FALSE.
      RETURN
    ENDIF
  ENDIF

  getTblField = .TRUE.
END FUNCTION getTblField

!=====================================================================

END MODULE flua

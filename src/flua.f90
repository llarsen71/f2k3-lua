! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

module flua
  use ISO_C_BINDING, only: C_CHAR, C_INT
  private :: p_characterToCharArray
  integer, PARAMETER :: LUA_MULTRET = -1

  integer, PARAMETER :: LUA_REGISTRYINDEX = -10000
  integer, PARAMETER :: LUA_ENVIRONINDEX  = -10001
  integer, PARAMETER :: LUA_GLOBALSINDEX  = -10002

  ! LUA types
  integer, PARAMETER :: LUA_TNONE = -1
  integer, PARAMETER :: LUA_TNIL = 0
  integer, PARAMETER :: LUA_TBOOLEAN = 1
  integer, PARAMETER :: LUA_TLIGHTUSERDATA = 2
  integer, PARAMETER :: LUA_TNUMBER = 3
  integer, PARAMETER :: LUA_TSTRING = 4
  integer, PARAMETER :: LUA_TTABLE = 5
  integer, PARAMETER :: LUA_TFUNCTION = 6
  integer, PARAMETER :: LUA_TUSERDATA = 7
  integer, PARAMETER :: LUA_TTHREAD = 8

  type cStrPTR
    character(kind=C_CHAR, len=1), dimension(:), pointer :: str
  end type cStrPTR

  character*2 :: nwln = CHAR(13) // CHAR(10)

interface
  !===================================================================
  ! State Manipulation
  !===================================================================
  !> LUALIB_API lua_State *(luaL_newstate) (void);
  function luaL_newstate() bind(C, name="luaL_newstate")
    use ISO_C_BINDING
    type(C_PTR) :: luaL_newstate
  end function luaL_newstate
  !
  !
  !> LUA_API void       (lua_close) (lua_State *L);
  subroutine lua_close(L) bind(C, name="lua_close")
    use ISO_C_BINDING
    type(C_PTR), value :: L
  end subroutine lua_close

  !===================================================================
  ! Base libraries
  !==================================================================
  !
  !
  !> LUALIB_API void (luaL_openlibs) (lua_State *L);
  subroutine luaL_openlibs(L) bind(C, name="luaL_openlibs")
    use ISO_C_BINDING, only: C_PTR
    implicit none
    type(C_PTR), value :: L
  end subroutine luaL_openlibs

  !===================================================================
  ! Load and execute files
  !===================================================================
  !
  !
  !> LUALIB_API int (luaL_loadfile) (lua_State *L, const char *filename);
  function luaL_loadfile(L, filename) bind(C, name="luaL_loadfile")
    use ISO_C_BINDING, only: C_PTR, C_CHAR, C_INT
    implicit none
    type(C_PTR), value :: L
    character(kind=C_CHAR, len=1), dimension(*) :: filename
    integer(kind=C_INT) :: luaL_loadfile
  end function luaL_loadfile
  !
  !
  !> LUALIB_API int (luaL_loadstring) (lua_State *L, const char *s);
  function luaL_loadstring(L, script) bind(C, name="luaL_loadstring")
    use ISO_C_BINDING, only: C_PTR, C_CHAR, C_INT
    implicit none
    type(C_PTR), value :: L
    character(kind=C_CHAR, len=1), dimension(*) :: script
    integer(kind=C_INT) :: luaL_loadstring
  end function luaL_loadstring
  !
  !
  !> LUA_API int   (lua_pcall) (lua_State *L, int nargs, int nresults, int errfunc);
  function lua_pcall_c(L, nargs, nresults, errfunc) bind(C, name="lua_pcall")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: nargs, nresults, errfunc
    integer(kind=C_INT) :: lua_pcall_c
  end function lua_pcall_c

  !===================================================================
  ! Basic Stack Manipulation
  !===================================================================
  !
  !
  !> LUA_API int   (lua_gettop) (lua_State *L);
  function lua_gettop(L) bind(C, name="lua_gettop")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT) :: lua_gettop
  end function lua_gettop
  !
  !
  !> LUA_API void  (lua_settop) (lua_State *L, int idx);
  subroutine lua_settop(L, idx) bind(C, name="lua_settop")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_settop
  !
  !
  !> LUA_API void  (lua_pushvalue) (lua_State *L, int idx);
  subroutine lua_pushvalue(L, idx) bind(C, name="lua_pushvalue")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_pushvalue
  !
  !
  !> LUA_API void  (lua_remove) (lua_State *L, int idx);
  subroutine lua_remove(L, idx) bind(C, name="lua_remove")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_remove
  !
  !
  !> LUA_API void  (lua_insert) (lua_State *L, int idx);
  subroutine lua_insert(L, idx) bind(C, name="lua_insert")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_insert
  !
  !
  !> LUA_API void  (lua_replace) (lua_State *L, int idx);
  subroutine lua_replace(L, idx) bind(C, name="lua_replace")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_replace
  !
  !
  !> LUA_API int   (lua_checkstack) (lua_State *L, int sz);
  function lua_checkstack(L, sz) bind(C, name="lua_checkstack")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: sz
    integer(kind=C_INT) :: lua_checkstack
  end function lua_checkstack

  !===================================================================
  ! Access functions (stack -> fortran)
  !===================================================================
  !
  !
  !> LUA_API int             (lua_isnumber) (lua_State *L, int idx);
  function lua_isnumber_c(L, n) bind(C, name="lua_isnumber")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: n
    integer(kind=C_INT) :: lua_isnumber_c
  end function lua_isnumber_c
  !
  !
  !> LUA_API int (lua_isstring) (lua_State *L, int idx);
  function lua_isstring_c(L, n) bind(C, name="lua_isstring")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: n
    integer(kind=C_INT) :: lua_isstring_c
  end function lua_isstring_c
  !
  !
  !> LUA_API int             (lua_iscfunction) (lua_State *L, int idx);
  function lua_iscfunction_c(L, idx) bind(C, name="lua_iscfunction")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    integer(kind=C_INT) :: lua_iscfunction_c
  end function lua_iscfunction_c
  !
  !
  !> LUA_API int             (lua_isuserdata) (lua_State *L, int idx);
  function lua_isuserdata_c(L, idx) bind(C, name="lua_isuserdata")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    integer(kind=C_INT) :: lua_isuserdata_c
  end function lua_isuserdata_c
  !
  !
  !>LUA_API int             (lua_type) (lua_State *L, int idx);
  function lua_type(L, idx) bind(C, name="lua_type")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    integer(kind=C_INT) :: lua_type
  end function lua_type
  !
  !
  !>LUA_API const char     *(lua_typename) (lua_State *L, int tp);
  function lua_typename_c(L, idx) bind(C, name="lua_typename")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    type(C_PTR):: lua_typename_c
  end function lua_typename_c
  !
  !
  !>LUA_API int            (lua_equal) (lua_State *L, int idx1, int idx2);
  function lua_equal(L, idx1, idx2) bind(C, name="lua_equal")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx1, idx2
    integer(kind=C_INT) :: lua_equal
  end function lua_equal
  !
  !
  !>LUA_API int            (lua_rawequal) (lua_State *L, int idx1, int idx2);
  function lua_rawequal(L, idx1, idx2) bind(C, name="lua_rawequal")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx1, idx2
    integer(kind=C_INT) :: lua_rawequal
  end function lua_rawequal
  !
  !
  !> LUA_API int            (lua_lessthan) (lua_State *L, int idx1, int idx2);
  function lua_lessthan(L, idx1, idx2) bind(C, name="lua_lessthan")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx1, idx2
    integer(kind=C_INT) :: lua_lessthan
  end function lua_lessthan
  !
  !
  !> LUA_API lua_Number      (lua_tonumber) (lua_State *L, int idx);
  function lua_tonumber(L, n) bind(C, name="lua_tonumber")
    use ISO_C_BINDING, only: C_INT, C_DOUBLE, C_PTR
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: n
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    real(kind=C_DOUBLE) :: lua_tonumber
  end function lua_tonumber
  !
  ! *** lua_tointeger doesn't work for some reason
  !
  !> LUA_API lua_Integer     (lua_tointeger) (lua_State *L, int idx);
  !function lua_tointeger(L, n) bind(C, name="lua_tointeger")
  !  use ISO_C_BINDING, only: C_INT, C_PTR, C_INT
  !  implicit none
  !  type(C_PTR), value :: L
  !  integer(kind=C_INT), value :: n
  !  ! NOTE: The type of 'lua_tonumber' depends on how the integer value
  !  ! is defined in lua.
  !  real(kind=C_INT) :: lua_tointeger
  !end function lua_tointeger
  !
  !
  !> LUA_API int             (lua_toboolean) (lua_State *L, int idx);
  function lua_toboolean(L, idx) bind(C, name="lua_toboolean")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    logical(kind=C_BOOL) :: lua_toboolean
  end function   lua_toboolean
  !
  !
  !> LUA_API const char     *(lua_tolstring) (lua_State *L, int idx, size_t *len);
  function lua_tolstring(L, i, length) bind(C, name="lua_tolstring")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: i
    integer(kind=C_SIZE_T), intent(out) :: length
    type(C_PTR):: lua_tolstring
  end function lua_tolstring
  !
  !
  !> LUA_API size_t          (lua_objlen) (lua_State *L, int idx);
  function lua_objlen(L, idx) bind(C, name="lua_objlen")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    integer(kind=C_SIZE_T) :: lua_objlen
  end function lua_objlen
  !
  !
  !> LUA_API lua_CFunction   (lua_tocfunction) (lua_State *L, int idx);
  function lua_tocfunction(L, idx) bind(C, name="lua_tocfunction")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    type(C_FUNPTR) :: lua_tocfunction
  end function lua_tocfunction
  !
  !
  !> LUA_API void	       *(lua_touserdata) (lua_State *L, int idx);
  function lua_touserdata(L, idx) bind(C, name="lua_touserdata")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    type(C_PTR) :: lua_touserdata
  end function lua_touserdata
  !
  !
  !> LUA_API lua_State      *(lua_tothread) (lua_State *L, int idx);
  function lua_tothread(L, idx) bind(C, name="lua_tothread")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    type(C_PTR) :: lua_tothread
  end function lua_tothread
  !
  !
  !> LUA_API const void     *(lua_topointer) (lua_State *L, int idx);
  function lua_topointer(L, idx) bind(C, name="lua_topointer")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    type(C_PTR) :: lua_topointer
  end function lua_topointer

  !===================================================================
  ! Push functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_pushnil) (lua_State *L);
  subroutine lua_pushnil(L) bind(C, name="lua_pushnil")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
  end subroutine lua_pushnil
  !
  !
  !> LUA_API void  (lua_pushnumber) (lua_State *L, lua_Number n);
  subroutine lua_pushnumber(L, n) bind(C, name="lua_pushnumber")
    use ISO_C_BINDING, only: C_PTR, C_DOUBLE
    implicit none
    type(C_PTR), value :: L
    !
    ! NOTE: This depends on the lua configuration, but double is the
    ! default
    real(kind=C_DOUBLE), value :: n
  end subroutine lua_pushnumber
  !
  ! The integer calls are not working. Haven't figured out why yet.
  !
  !
  !> LUA_API void  (lua_pushinteger) (lua_State *L, lua_Integer n);
  !subroutine lua_pushinteger(L, n) bind(C, name="lua_pushinteger")
  !  use ISO_C_BINDING, only: C_LONG, C_PTR
  !  implicit none
  !  type(C_PTR), value :: L
  !  ! NOTE: The type of 'n' depends on how the integer value is
  !  ! defined in lua. Lua uses ptrdiff_t by default, which is either
  !  ! int or long.
  !  integer(kind=C_LONG), value :: n
  !end subroutine

!> LUA_API void  (lua_pushlstring) (lua_State *L, const char *s, size_t len);
!> LUA_API const char *(lua_pushvfstring) (lua_State *L, const char *fmt, va_list argp);
!> LUA_API const char *(lua_pushfstring) (lua_State *L, const char *fmt, ...);

  !
  !
  !> LUA_API void  (lua_pushstring) (lua_State *L, const char *s);
  subroutine lua_pushstring_c(L, str) bind(C, name="lua_pushstring")
    use ISO_C_BINDING, only:C_CHAR, C_PTR
    implicit none
    type(C_PTR), value :: L
    character(kind=C_CHAR, len=1), dimension(*) :: str
  end subroutine lua_pushstring_c
  !
  !
  !> LUA_API void  (lua_pushcclosure) (lua_State *L, lua_CFunction fn, int n);
  subroutine lua_pushcclosure(L, fn, n) bind(C, name="lua_pushcclosure")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    type(C_FUNPTR), value :: fn
    integer(kind=C_INT), value :: n
  end subroutine lua_pushcclosure
  !
  !
  !> LUA_API void  (lua_pushboolean) (lua_State *L, int b);
  subroutine lua_pushboolean(L, b) bind(C, name="lua_pushboolean")
    use ISO_C_BINDING, only: C_PTR, C_BOOL
    implicit none
    type(C_PTR), value :: L
    logical(C_BOOL), value :: b
  end subroutine lua_pushboolean
  !
  !
  !> LUA_API void  (lua_pushlightuserdata) (lua_State *L, void *ptr);
  subroutine lua_pushlightuserdata(L, ptr) bind(C, name="lua_pushlightuserdata")
    use ISO_C_BINDING, only: C_PTR
    implicit none
    type(C_PTR), value :: L
    type(C_PTR), value :: ptr
  end subroutine lua_pushlightuserdata

!> LUA_API int   (lua_pushthread) (lua_State *L);
  !
  !
  !> LUALIB_API int (luaL_ref) (lua_State *L, int t);
  function luaL_ref(L, tblidx) bind(C, name="luaL_ref")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: tblidx
    integer(kind=C_INT) :: luaL_ref
  end function luaL_ref
  !
  !
  !> LUALIB_API void (luaL_unref) (lua_State *L, int t, int ref);
  subroutine luaL_unref(L, tblidx, ref) bind(C, name="luaL_unref")
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: tblidx, ref
  end subroutine luaL_unref

  !===================================================================
  ! Get functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_gettable) (lua_State *L, int idx);
  subroutine lua_gettable(L, idx) bind(C, name="lua_gettable")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_gettable
  !
  !
  !> LUA_API void  (lua_getfield) (lua_State *L, int idx, const char *k);
  subroutine lua_getfield_c(L, n, k) bind(C, name="lua_getfield")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: n
    character(kind=C_CHAR, len=1), dimension(*) :: k
  end subroutine lua_getfield_c
  !
  !
  !> LUA_API void  (lua_rawget) (lua_State *L, int idx);
  subroutine lua_rawget(L, idx) bind(C, name="lua_rawget")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_rawget
  !
  !
  !> LUA_API void  (lua_rawgeti) (lua_State *L, int idx, int n);
  subroutine lua_rawgeti(L, idx, n) bind(C, name="lua_rawgeti")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx, n
  end subroutine lua_rawgeti
  !
  !
  !> LUA_API void  (lua_createtable) (lua_State *L, int narr, int nrec);
  subroutine lua_createtable(L, narr, nrec) bind(C, name="lua_createtable")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: narr, nrec
  end subroutine lua_createtable
  !
  !
  !> LUA_API void *(lua_newuserdata) (lua_State *L, size_t sz);
  function lua_newuserdata(L, sz) bind(C, name="lua_newuserdata")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_SIZE_T), value :: sz
    type(C_PTR) :: lua_newuserdata
  end function lua_newuserdata
  !
  !
  !> LUA_API int   (lua_getmetatable) (lua_State *L, int objindex);
  function lua_getmetatable(L, objindex) bind(C, name="lua_getmetatable")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: objindex
    integer(kind=C_INT) :: lua_getmetatable
  end function lua_getmetatable
  !
  !
  !> LUA_API void  (lua_getfenv) (lua_State *L, int idx);
  subroutine lua_getfenv(L, idx) bind(C, name="lua_getfenv")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_getfenv

  !===================================================================
  ! Set functions (Lua -> stack)
  !===================================================================
  !
  !
  !> LUA_API void  (lua_settable) (lua_State *L, int idx);
  subroutine lua_settable(L, idx) bind(C, name="lua_settable")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_settable
  !
  !
  !> LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
  subroutine lua_setfield_c(L, idx, k) bind(C, name="lua_setfield")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
    character(kind=C_CHAR, len=1), dimension(*) :: k
  end subroutine lua_setfield_c
  !
  !
  !> LUA_API void  (lua_rawset) (lua_State *L, int idx);
  subroutine lua_rawset(L, idx) bind(C, name="lua_rawset")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_rawset
  !
  !
  !> LUA_API void  (lua_rawseti) (lua_State *L, int idx, int n);
  subroutine lua_rawseti(L, idx, n) bind(C, name="lua_rawseti")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx, n
  end subroutine lua_rawseti
  !
  !
  !> LUA_API int   (lua_setmetatable) (lua_State *L, int objindex);
  subroutine lua_setmetatable(L, objindex) bind(C, name="lua_setmetatable")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: objindex
  end subroutine lua_setmetatable
  !
  !
  !> LUA_API int   (lua_setfenv) (lua_State *L, int idx);
  subroutine lua_setfenv(L, idx) bind(C, name="lua_setfenv")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: idx
  end subroutine lua_setfenv

!===================================================================
  !
  !
  !> LUA_API int   (lua_next) (lua_State *L, int idx);
  function lua_next_c(L, n) bind(C, name="lua_next")
    use ISO_C_BINDING
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: n
    integer(kind=C_INT) :: lua_next_c
  end function lua_next_c
  !
  !
  !> LUALIB_API const char *(luaL_checklstring) (lua_State *L, int numArg, size_t *len);
  function luaL_checklstring(L, numArg, len) bind(C, name="luaL_checklstring")
    use ISO_C_BINDING, only:C_PTR, C_INT, C_SIZE_T
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: numArg
    type(C_PTR) :: len
    type(C_PTR) :: luaL_checklstring
  end function luaL_checklstring
  !
end interface


CONTAINS

!=====================================================================

  subroutine initDefaultErrfunc(L, errorlogger)
    use ISO_C_BINDING, only: C_PTR
    type(C_PTR), value :: L
    character*(*), optional :: errorlogger
    integer :: error

    !   Put the error logging function on the stack
    if (PRESENT(errorlogger)) then
      call luaL_dostring(L, errorlogger, error)
    else
      call luaL_dostring(L, &
      'os.remove("lua_error.log")' // nwln // &
      'function logLuaError(...)' // nwln // &
      '  local f = assert(io.open("lua_error.log","a+"))' // nwln // &
      '  if f == nil then return end' // nwln // &
      '  for i, error in ipairs{...} do' // nwln // &
      '    f:write(error)' // nwln // &
      '  end' // nwln // &
      '  f:close()' // nwln // &
      'end', error)
    end if
  end subroutine initDefaultErrfunc

!=====================================================================

  function lua_pcall(L, nargs, nresults, errfunc)
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: nargs, nresults
    integer(kind=C_INT), optional :: errfunc
    integer(kind=C_INT) :: lua_pcall

!   If errfunc is passed in, use that value. Otherwise, use the
!   default errfunc.
    if (PRESENT(errfunc)) then
      lua_pcall = lua_pcall_c(L, nargs, nresults, errfunc)
    else
      lua_pcall = lua_pcall_c(L, nargs, nresults, 0)

      if (lua_pcall /= 0) then
        call handleError(L)
      end if
    end if
  end function lua_pcall

!=====================================================================

  function flua_pcall(L, nargs, nresults, errfunc) result(success)
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT), value :: nargs, nresults
    integer(kind=C_INT), optional :: errfunc
    logical :: success

    if (lua_pcall(L, nargs, nresults, errfunc) == 0) then
      success = .true.
    else
      success = .false.
    end if
  end function flua_pcall

!=====================================================================

  subroutine handleError(L)
    use ISO_C_BINDING, only: C_PTR, C_INT
    implicit none
    type(C_PTR), value :: L
    integer(kind=C_INT) :: err

    call lua_getglobal(L, "logLuaError")
    if (lua_isnil(L, -1)) then
      call lua_pop(L,1)
    else
      call lua_insert(L, -2)
      err = lua_pcall_c(L, 1, 0, 0)
    end if
  end subroutine handleError

!=====================================================================

function cSTR(str, doTrim)
! This recieves a fortran string and creates a character array pointer
! that holds the string. The new string is compatible with c strings.
! The pointer must be deallocated when it is not longer of use. The
! value must be trimmed before sending to this function.
!
  use ISO_C_BINDING, only: C_NULL_CHAR
  implicit none
  character*(*), intent(IN) :: str
  logical, optional:: doTrim
  type(cStrPTR) :: cSTR
  integer :: i, lng
  logical :: doTrim_

  doTrim_ = .TRUE.
  if (PRESENT(doTrim)) then
    doTrim_ = doTrim
  endif

  if (doTrim_) then
    lng = LEN_TRIM(str)
  else
    lng = len(str)
  endif

  allocate(cSTR%str(lng+1))
  do i = 1, lng
    cSTR%str(i) = str(i:i)
  end do
  cSTR%str(lng+1) = C_NULL_CHAR
end function cSTR

!=====================================================================

function cSTR2fSTR(cstr_, dealloc) result(fstr)
  implicit none
  type(cStrPTR) :: cstr_
  logical, optional :: dealloc
  character(len=SIZE(cstr_%str)-1) :: fstr
  logical :: dealloc_
  integer :: i, sz

! Fill in the fortran string
  sz = SIZE(cstr_%str)-1
  do i = 1, sz
    fstr(i:i) = cstr_%str(i)
  end do

! deallocate the string if specified.
  dealloc_ = .true.
  if (PRESENT(dealloc)) then
    dealloc_ = dealloc
  endif
  if (dealloc_) then
    deallocate(cstr_%str)
  endif
end function cSTR2fSTR

!=====================================================================

subroutine p_characterToCharArray(fstr, charArray, error)
  use ISO_C_BINDING, only: C_CHAR, C_NULL_CHAR
  implicit none
  character(len=*), intent(IN) :: fstr
  character(kind=C_CHAR, len=1), dimension(:), intent(out) :: charArray
  integer, intent(out) :: error
  integer :: i, lenTrim

  lenTrim = LEN_TRIM(fstr)
  error = 0
  if(SIZE(charArray) < (lenTrim + 1)) then
    error = 1
    return
  end if
  !
  do i = 1, lenTrim
    charArray(i) = fstr(i:i)
  end do
  charArray(lenTrim+1) = C_NULL_CHAR
end subroutine p_characterToCharArray

!=====================================================================

subroutine C_F_CSTR(cptr, ret, length, as_cSTR)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: cptr
  character(kind=C_CHAR), dimension(:), pointer :: ret
  integer, intent(out) :: length
  logical, optional, intent(IN) :: as_cSTR
  interface
    function strlen(cstr_) bind(C,name="strlen")
      use ISO_C_BINDING
      implicit none
      integer(kind=C_SIZE_T) :: strlen
      type(C_PTR), value :: cstr_
    end function strlen
  end interface

  length = strlen(CPTR)
  if (PRESENT(as_cSTR)) then
    if (as_cSTR) then
!     Add 1 to capture the \0 at the end of string.
      length = length + 1
    endif
  endif

  ! This is a builtin fortran function that converts c pointers to
  ! fortran pointers.
  call C_F_POINTER(FPTR=ret, CPTR=cptr, SHAPE=(/ length /))
end subroutine C_F_CSTR

!=====================================================================

subroutine flua_tostring(L, n, str)
  use ISO_C_BINDING, only: C_PTR, C_SIZE_T, C_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  character(len=*), intent(out) :: str
  !
  type(C_PTR) :: cstr_
  integer(kind=C_SIZE_T) :: length
  character(kind=C_CHAR), dimension(:), pointer :: fstr
  integer :: i, cstrLength

  str = ""
  cstr_ = lua_tolstring(L, n, length)
  call C_F_CSTR(cstr_, fstr, cstrLength)
  !
  do i = 1, cstrLength
    str(i:i) = fstr(i)
  end do
end subroutine flua_tostring

!=====================================================================

function lua_tostring(L, n)
! Returns the Lua string that is at the given stack position.
! The return value is a cStrPTR structure that contains an
! allocated character array. This needs to be deallocated
! manually. This can be done by calling the function
! cSTR2fSTR on the returned value to get a fortran string.
!
  use ISO_C_BINDING, only: C_PTR, C_SIZE_T
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  type(cStrPTR) :: lua_tostring
  type(C_PTR) :: cstr_
  integer(kind=C_SIZE_T) :: length
  character(kind=C_CHAR), dimension(:), pointer :: fstr
  integer :: length2, i

  cstr_ = lua_tolstring(L, n, length)
  call C_F_CSTR(cstr_, fstr, length2, .TRUE.)
  allocate(lua_tostring%str(length2))

! Copy the Lua string over to the new string.
  do i = 1, length2
    lua_tostring%str(i) = fstr(i)
  end do
end function lua_tostring

!=====================================================================

function lua_typename(L, typeid)
  use ISO_C_BINDING, only: C_PTR, C_SIZE_T
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: typeid
  type(cStrPTR) :: lua_typename
  type(C_PTR) :: cstr_
  character(kind=C_CHAR), dimension(:), pointer :: fstr
  integer :: length2, i

  cstr_ = lua_typename_c(L, typeid)
  call C_F_CSTR(cstr_, fstr, length2, .TRUE.)
  allocate(lua_typename%str(length2))

! Copy the Lua string over to the new string.
  do i = 1, length2
    lua_typename%str(i) = fstr(i)
  end do
end function lua_typename

!=====================================================================

subroutine lua_getglobal(L, key)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: key

  call lua_getfield(L, LUA_GLOBALSINDEX, key)
end subroutine lua_getglobal

!=====================================================================

subroutine lua_setglobal(L, key)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: key
  !
  call lua_setfield(L,  LUA_GLOBALSINDEX, key)
end subroutine lua_setglobal

!=====================================================================

function flua_checkglobal(L, key) RESULT(has_globalvar)
! Check whether a global value exists. if it does, leave it on the
! stack and return .TRUE. if the value does not exist, don't leave a
! value on the stack
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: key
  logical has_globalvar

  call lua_getglobal(L, key)
  if (lua_isnil(L, -1)) then
    ! if getglobal returned nil, the global variable didn't exist.
    ! Pop the nil value off the stack.
    call lua_pop(L, 1)
    has_globalvar = .FALSE.
  else
    ! if the gloval value was found, leave it on the stack and
    ! return true.
    has_globalvar = .TRUE.
  endif
end function flua_checkglobal

!=====================================================================
! Access function (stack -> fortran) MACROS
!=====================================================================

!#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
function lua_isfunction(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isfunction

  lua_isfunction = (lua_type(L, n) == LUA_TFUNCTION)
end function lua_isfunction

!=====================================================================

!#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
function lua_istable(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_istable

  lua_istable = (lua_type(L,  n) == LUA_TTABLE)
end function lua_istable

!=====================================================================

!#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
function lua_islightuserdata(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_islightuserdata

  lua_islightuserdata = (lua_type(L,  n) == LUA_TLIGHTUSERDATA)
end function lua_islightuserdata

!=====================================================================

!#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
function lua_isnil(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isnil

  lua_isnil = (lua_type(L,  n) == LUA_TNIL)
end function lua_isnil

!=====================================================================

!#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
function lua_isboolean(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isboolean

  lua_isboolean = (lua_type(L,  n) == LUA_TBOOLEAN)
end function

!=====================================================================

!#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
function lua_isthread(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isthread

  lua_isthread = (lua_type(L,  n) == LUA_TTHREAD)
end function

!=====================================================================

!#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
function lua_isnone(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isnone

  lua_isnone = (lua_type(L,  n) == LUA_TNONE)
end function

!=====================================================================

!#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
function lua_isnoneornil(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isnoneornil

  lua_isnoneornil = (lua_type(L,  n) <= 0)
end function

!=====================================================================

function lua_isnumber(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isnumber
  !
  lua_isnumber = lua_isnumber_c(L,  n) /= 0
end function lua_isnumber

!=====================================================================

function lua_isstring(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_isstring
  !
  lua_isstring = lua_isstring_c(L,  n) /= 0
end function lua_isstring

!=====================================================================

function lua_tointeger(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  double precision :: dbl
  integer :: lua_tointeger
  !
  dbl = lua_tonumber(L, n)
  lua_tointeger = dbl
end function lua_tointeger

!=====================================================================

subroutine lua_pushinteger(L, val)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: val
  double precision :: valD
  !
  valD = val
  call lua_pushnumber(L, valD)
end subroutine lua_pushinteger

!=====================================================================

subroutine lua_pop(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  !
  call lua_settop(L, -n-1)
end subroutine lua_pop

!=====================================================================

subroutine lua_pushstring(L, str)
  use ISO_C_BINDING, only: C_PTR, C_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: str
  !
  character(len=1, kind=C_CHAR), dimension(LEN_TRIM(str)+1) :: cstr_
  integer :: error

  call p_characterToCharArray(str, cstr_, error)
  ! error cannot be /= 0
  !
  call lua_pushstring_c(L, cstr_)
  !
end subroutine lua_pushstring

!=====================================================================

subroutine lua_pushtable(L)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(out) :: L

  ! Add a new table to the stack.
  call lua_createtable(L, 0, 0)
end subroutine lua_pushtable

!=====================================================================

!> #define lua_newtable(L)		lua_createtable(L, 0, 0)
subroutine lua_newtable(L, tablename)
  use ISO_C_BINDING, only: C_PTR, C_CHAR
  implicit none
  type(C_PTR), intent(out) :: L
  character(len=*), optional, intent(IN) :: tablename

  ! Add a new table to the stack.
  call lua_createtable(L, 0, 0)

  ! if a table name was specified, add this as a global variable with the
  ! given table name.
  if (PRESENT(tablename)) then
    ! Copy the table (which is located at stack location -1)
    call lua_pushvalue(L,-1)
    ! Add the table as a global variable with the given tablename.
    ! Note that this pops the copy of the table from the stack, but
    ! the original table is still there
    call lua_setglobal(L, tablename)
  endif
end subroutine lua_newtable

!=====================================================================

function lua_next(L, n)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  logical :: lua_next
  !
  lua_next = lua_next_c(L, n) /= 0
end function lua_next

!=====================================================================

subroutine lua_pushcfunction(L, fn)
  use ISO_C_BINDING, only: C_PTR, C_FUNPTR
  implicit none
  type(C_PTR), intent(IN) :: L
  type(C_FUNPTR), intent(IN) :: fn
  !
  call lua_pushcclosure(L, fn, 0)
end subroutine lua_pushcfunction

!=====================================================================

subroutine lua_getfield(L, n, k)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  character(len=*), intent(IN) :: k
  !
  character(len=1, kind=C_CHAR), dimension(LEN_TRIM(k)+1) :: c_key
  integer :: error

  call p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  call lua_getfield_c(L, n, c_key)
end subroutine lua_getfield

!=====================================================================

!> LUA_API void  (lua_setfield) (lua_State *L, int idx, const char *k);
subroutine lua_setfield(L, idx, k)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: idx
  character(len=*), intent(IN) :: k
  !
  character(len=1, kind=C_CHAR), dimension(LEN_TRIM(k)+1) :: c_key
  integer :: error

  call p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  call lua_setfield_c(L, idx, c_key)
end subroutine lua_setfield

!=====================================================================

function fluaL_loadfile(L, filename) result(success)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_INT
  implicit none
  type(C_PTR), value :: L
  character(kind=C_CHAR, len=1), dimension(*) :: filename
  logical :: success

  if (luaL_loadfile(L, filename) == 0) then
    success = .true.
  else
    success = .false.
  end if
end function fluaL_loadfile

!=====================================================================

!> #define luaL_dofile(L, fn) (luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))
subroutine luaL_dofile(L, fileName, error)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: fileName
  integer, intent(out) :: error
  character(len=1, kind=C_CHAR), dimension(LEN_TRIM(fileName)+1) :: cFileName

  call p_characterToCharArray(fileName, cFileName, error)
  ! error cannot be /= 0
  !
  ! TODO: report error string
  error = luaL_loadfile(L, cFileName)
  if (error == 0) then
    error = lua_pcall(L, 0, LUA_MULTRET, 0)
  else
    call handleError(L)
  endif
end subroutine luaL_dofile

!=====================================================================

function fluaL_loadstring(L, script) result(success)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_INT
  implicit none
  type(C_PTR), value :: L
  character*(*) :: script
  logical :: success
  type(cStrPTR) :: cscript

  cscript = cSTR(script,.FALSE.)
  if (luaL_loadstring(L, cscript%str) == 0) then
    success = .true.
  else
    success = .false.
  end if
  deallocate(cscript%str)
end function fluaL_loadstring

!=====================================================================

!> #define luaL_dostring(L, s) (luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))
subroutine luaL_dostring(L, script, error, errfnidx)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: script
  integer, optional, intent(out) :: error
  integer, optional, intent(in) :: errfnidx
  integer error_
  !
  character(len=1, kind=C_CHAR), dimension(LEN_TRIM(script)+1) :: cscript

  call p_characterToCharArray(script, cscript, error_)
  ! error should be 0
  !
  ! TODO: report error string
  error_ = luaL_loadstring(L, cscript)
  if (error_ == 0) then
    if (PRESENT(errfnidx)) then
      error_ = lua_pcall(L, 0, LUA_MULTRET, errfnidx)
    else
      error_ = lua_pcall(L, 0, LUA_MULTRET)
    endif
  else
    call handleError(L)
  endif
  if (PRESENT(error)) then
    error = error_
  end if
end subroutine luaL_dostring

!=====================================================================

function fluaL_dostring(L, script, errfnidx) result(success)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_CHAR
  implicit none
  type(C_PTR), intent(IN) :: L
  character(len=*), intent(IN) :: script
  integer, optional, intent(in) :: errfnidx
  logical :: success
  integer :: error

  call luaL_dostring(L, script, error, errfnidx)
  success = (error == 0)
end function fluaL_dostring

!=====================================================================

subroutine stackDump(L)
  use ISO_C_BINDING
  implicit none
  type(C_PTR), intent(IN) :: L
  integer :: i, top, t

  top = lua_gettop(L)
  write(6, "(A)") ""
  write(6, "(A)") "***start stack***"
  do i = 1,top
    t = lua_type(L, i)
    select case (t)
      case (LUA_TSTRING)
        write(6, 10) cStr2fSTR(lua_tostring(L, i))
10      FORMAT('"', A, '" ')
      case (LUA_TBOOLEAN)
        write(6, "(L)") lua_toboolean(L, i)
      case (LUA_TNUMBER)
        write(6, "(G14.6)") lua_tonumber(L, i)
      case default
        write(6, "(A)") cStr2fSTR(lua_typename(L, t))
    end select
  end do
  write(6, "(A)") "***end  stack***"
end subroutine stackDump

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
subroutine luaL_checkstring(L, n, fstr, error)
  use ISO_C_BINDING, only: C_PTR, C_CHAR, C_NULL_PTR
  implicit none
  type(C_PTR), intent(IN) :: L
  integer, intent(IN) :: n
  character(len=*), intent(out) :: fstr
  integer, intent(out) :: error
  !
  type(C_PTR) :: cstr_
  character(len=1), dimension(:), pointer :: chars
  integer :: i, length

  fstr = ""
  error = 0
  cstr_ = luaL_checklstring(L, n, C_NULL_PTR)
  call C_F_CSTR(cstr_, chars, length)
  if (len(fstr) < length) then
    error = 1
    return
  end if
  do i = 1, length
    fstr(i:i) = chars(i)
  end do
end subroutine luaL_checkstring

!=====================================================================

subroutine flua_registerfunction(L, fnname, fn)
  use ISO_C_BINDING, only: C_PTR, C_FUNPTR
  implicit none
  type(C_PTR), value, intent(IN) :: L
  character(len=*), intent(IN) :: fnname
  type(C_FUNPTR), intent(IN) :: fn

  ! Push the function on the stack.
  call lua_pushcfunction(L, fn)

  ! Assign the function to a global name
  call lua_setglobal(L, fnname);
end subroutine flua_registerfunction

!=====================================================================

subroutine flua_opensandboxlibs(L)
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), value, intent(IN) :: L

  call luaL_openlibs(L)
end subroutine flua_opensandboxlibs

!=====================================================================

function getTblField(L, fieldname, type_, tableidx)
!
! Note that this puts table values at the top of the
! stack. These need to be removed by the calling subroutine.
!
  use ISO_C_BINDING, only: C_PTR
  implicit none
  type(C_PTR), value, intent(IN) :: L
  character*(*) :: fieldname
  integer, optional :: type_, tableidx
  logical :: getTblField
  integer tblidx

  ! default index is just above the fieldname that is
  ! added to the stack.
  tblidx = -2
  if (PRESENT(tableidx)) then
    tblidx = tableidx-1
  endif

  ! Put the field value on top of the stack
  call lua_pushstring(L, fieldname)
  call lua_gettable(L, tblidx)

  if (PRESENT(type_)) then
    if (lua_type(L, -1) /= type_) then
      call lua_pop(L, 1)
      getTblField = .FALSE.
      return
    endif
  endif

  getTblField = .TRUE.
end function getTblField

!=====================================================================

end module flua

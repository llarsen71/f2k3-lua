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
  !
  INTEGER :: i, lenTrim
  ! ==================================================================== 
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

SUBROUTINE C_F_CSTR(cptr, ret, length, as_cSTR)
  USE ISO_C_BINDING
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: cptr
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: ret
  INTEGER, INTENT(OUT) :: length
  LOGICAL, OPTIONAL, INTENT(IN) :: as_cSTR
  !
  INTERFACE
    FUNCTION strlen_c(cstr_) BIND(C,NAME="strlen")
      USE ISO_C_BINDING
      IMPLICIT NONE
      INTEGER(KIND=C_SIZE_T) :: strlen_c
      TYPE(C_PTR), VALUE :: cstr_
    END FUNCTION strlen_c
  END INTERFACE
  ! ====================================================================
  length = strlen_c(CPTR)
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

SUBROUTINE luaL_newstate(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: luaL_newstate_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  L = luaL_newstate_c()  
END SUBROUTINE luaL_newstate

SUBROUTINE lua_close(L)
  USE ISO_C_BINDING
  USE fluac, ONLY: lua_close_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  CALL lua_close_c(L)
END SUBROUTINE lua_close

SUBROUTINE flua_tostring(L, n, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T, C_CHAR
  USE fluac, ONLY: lua_tolstring_c
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
  cstr_ = lua_tolstring_c(L, n, length)
  CALL C_F_CSTR(cstr_, fstr, cstrLength)
  !
  DO i = 1, cstrLength
    str(i:i) = fstr(i)
  END DO
END SUBROUTINE flua_tostring

FUNCTION lua_tostring(L, n)
! Returns the Lua string that is at the given stack position.
! The return value is a cStrPTR structure that contains an
! allocated character array. This needs to be deallocated
! manually. This can be done by calling the function
! cSTR2fSTR on the returned value to get a fortran string.
!
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T
  USE fluac, ONLY: lua_tolstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  TYPE(cStrPTR) :: lua_tostring
  TYPE(C_PTR) :: cstr_
  INTEGER(KIND=C_SIZE_T) :: length
  CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: fstr
  INTEGER :: length2, i

  cstr_ = lua_tolstring_c(L, n, length)
  CALL C_F_CSTR(cstr_, fstr, length2, .TRUE.)
  ALLOCATE(lua_tostring%str(length2))
  
! Copy the Lua string over to the new string.
  DO i = 1, length2
    lua_tostring%str(i) = fstr(i)
  END DO
END FUNCTION lua_tostring

FUNCTION lua_typename(L, typeid)
  USE ISO_C_BINDING, ONLY: C_PTR, C_SIZE_T
  USE fluac, ONLY: lua_typename_c
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

SUBROUTINE lua_getglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE fluac, ONLY: lua_getfield_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(key)+1) :: c_key
  INTEGER :: i, error
  ! ====================================================================
  CALL p_characterToCharArray(key, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_getfield_c(L, LUA_GLOBALSINDEX, c_key)
  !
END SUBROUTINE lua_getglobal

SUBROUTINE lua_setglobal(L, key)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE fluac, ONLY: lua_setfield_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: key
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(key)+1) :: c_key
  INTEGER :: i, error
  ! ====================================================================
  CALL p_characterToCharArray(key, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_setfield_c(L,  LUA_GLOBALSINDEX, c_key)
  !
END SUBROUTINE lua_setglobal

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

FUNCTION lua_type(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR, C_INT
  USE fluac, ONLY: lua_type_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  INTEGER(C_INT) :: lua_type

  lua_type = lua_type_c(L, n)
END FUNCTION lua_type

!#define lua_isfunction(L,n)	(lua_type(L, (n)) == LUA_TFUNCTION)
FUNCTION lua_isfunction(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isfunction

  lua_isfunction = (lua_type(L, n) == LUA_TFUNCTION)
END FUNCTION lua_isfunction

!#define lua_istable(L,n)	(lua_type(L, (n)) == LUA_TTABLE)
FUNCTION lua_istable(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_istable

  lua_istable = (lua_type(L,  n) == LUA_TTABLE)
END FUNCTION lua_istable

!#define lua_islightuserdata(L,n)	(lua_type(L, (n)) == LUA_TLIGHTUSERDATA)
FUNCTION lua_islightuserdata(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_islightuserdata

  lua_islightuserdata = (lua_type(L,  n) == LUA_TLIGHTUSERDATA)
END FUNCTION lua_islightuserdata

!#define lua_isnil(L,n)		(lua_type(L, (n)) == LUA_TNIL)
FUNCTION lua_isnil(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnil

  lua_isnil = (lua_type(L,  n) == LUA_TNIL)
END FUNCTION lua_isnil

!#define lua_isboolean(L,n)	(lua_type(L, (n)) == LUA_TBOOLEAN)
FUNCTION lua_isboolean(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isboolean

  lua_isboolean = (lua_type(L,  n) == LUA_TBOOLEAN)
END FUNCTION 

!#define lua_isthread(L,n)	(lua_type(L, (n)) == LUA_TTHREAD)
FUNCTION lua_isthread(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isthread

  lua_isthread = (lua_type(L,  n) == LUA_TTHREAD)
END FUNCTION 

!#define lua_isnone(L,n)		(lua_type(L, (n)) == LUA_TNONE)
FUNCTION lua_isnone(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnone

  lua_isnone = (lua_type(L,  n) == LUA_TNONE)
END FUNCTION 

!#define lua_isnoneornil(L, n)	(lua_type(L, (n)) <= 0)
FUNCTION lua_isnoneornil(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnoneornil

  lua_isnoneornil = (lua_type(L,  n) <= 0)
END FUNCTION 

FUNCTION lua_isstring(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_isstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isstring
  !
  lua_isstring = lua_isstring_c(L,  n) /= 0
END FUNCTION lua_isstring

FUNCTION lua_isnumber(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_isnumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_isnumber
  !
  lua_isnumber = lua_isnumber_c(L,  n) /= 0
END FUNCTION lua_isnumber

FUNCTION lua_tonumber(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_tonumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION :: lua_tonumber
  !
  lua_tonumber = lua_tonumber_c(L, n)
END FUNCTION lua_tonumber

FUNCTION lua_tointeger(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_tonumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  DOUBLE PRECISION :: dbl
  INTEGER :: lua_tointeger
  !
  dbl = lua_tonumber_c(L, n)
  lua_tointeger = dbl
END FUNCTION lua_tointeger

!=====================================================================

SUBROUTINE lua_pushinteger(L, val)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_pushnumber_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: val
  DOUBLE PRECISION :: valD
  !
  valD = val
  CALL lua_pushnumber_c(L, valD)
END SUBROUTINE lua_pushinteger

SUBROUTINE lua_pop(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_settop_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  !
  CALL lua_settop_c(L, -n-1)
END SUBROUTINE lua_pop

SUBROUTINE lua_pushnil(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_pushnil_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  !
  CALL lua_pushnil_c(L)
END SUBROUTINE lua_pushnil

SUBROUTINE lua_pushstring(L, str)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  USE fluac, ONLY: lua_pushstring_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: str
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(str)+1) :: cstr_
  INTEGER :: error
  ! ====================================================================
  CALL p_characterToCharArray(str, cstr_, error)
  ! error cannot be /= 0
  !
  CALL lua_pushstring_c(L, cstr_)
  !
END SUBROUTINE lua_pushstring

SUBROUTINE lua_pushtable(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_createtable_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L  
  
  ! Add a new table to the stack.
  CALL lua_createtable_c(L, 0, 0)
END SUBROUTINE lua_pushtable

!> #define lua_newtable(L)		lua_createtable(L, 0, 0)
SUBROUTINE lua_newtable(L, tablename)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR
  USE fluac, ONLY: lua_createtable_c, lua_pushvalue_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: L
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: tablename

  ! Add a new table to the stack.
  CALL lua_createtable_c(L, 0, 0)
  
  ! If a table name was specified, add this as a global variable with the
  ! given table name.
  IF (PRESENT(tablename)) THEN
    ! Copy the table (which is located at stack location -1)
    CALL lua_pushvalue_c(L,-1)
    ! Add the table as a global variable with the given tablename.
    ! Note that this pops the copy of the table from the stack, but
    ! the original table is still there
    CALL lua_setglobal(L, tablename)
  ENDIF
END SUBROUTINE lua_newtable

FUNCTION lua_next(L, n)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_next_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  LOGICAL :: lua_next
  !
  lua_next = lua_next_c(L, n) /= 0
END FUNCTION lua_next

SUBROUTINE lua_pushcfunction(L, fn)
  USE ISO_C_BINDING, ONLY: C_PTR, C_FUNPTR
  USE fluac, ONLY: lua_pushcclosure_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  TYPE(C_FUNPTR), INTENT(IN) :: fn
  !
  call lua_pushcclosure_c(L, fn, 0)
END SUBROUTINE lua_pushcfunction

SUBROUTINE lua_getfield(L, n, k)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE fluac, ONLY: lua_getfield_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(IN) :: k
  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(k)+1) :: c_key
  INTEGER :: error
  ! ====================================================================
  CALL p_characterToCharArray(k, c_key, error)
  ! error cannot be /= 0
  !
  CALL lua_getfield_c(L, n, c_key)
END SUBROUTINE lua_getfield

!> #define luaL_dofile(L, fn) (luaL_loadfile(L, fn) || lua_pcall(L, 0, LUA_MULTRET, 0))
SUBROUTINE luaL_dofile(L, fileName, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE fluac, ONLY: luaL_loadfile_c, lua_pcall_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: fileName
  INTEGER, INTENT(OUT) :: error
  CHARACTER*1000 :: msg

  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(fileName)+1) :: cFileName
  INTEGER :: i, lenTrim
  ! ====================================================================
  CALL p_characterToCharArray(fileName, cFileName, error)
  ! error cannot be /= 0
  !
  ! TODO: report error string
  error = luaL_loadfile_c(L, cFileName)
  IF (error == 0) THEN
    error = lua_pcall_c(L, 0, LUA_MULTRET, 0)
    IF (error /= 0) THEN
      ! TODO: report error string
      call flua_tostring(L, -1, msg)
    ENDIF
  ENDIF
END SUBROUTINE luaL_dofile

!> #define luaL_dostring(L, s) (luaL_loadstring(L, s) || lua_pcall(L, 0, LUA_MULTRET, 0))
SUBROUTINE luaL_dostring(L, script, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_CHAR
  USE fluac, ONLY: luaL_loadstring_c, lua_pcall_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  CHARACTER(LEN=*), INTENT(IN) :: script
  INTEGER, INTENT(OUT) :: error
  CHARACTER*1000 :: msg

  !
  CHARACTER(LEN=1, KIND=C_CHAR), DIMENSION(LEN_TRIM(script)+1) :: cscript
  INTEGER :: i, lenTrim
  ! ====================================================================
  CALL p_characterToCharArray(script, cscript, error)
  ! error cannot be /= 0
  !
  ! TODO: report error string
  error = luaL_loadstring_c(L, cscript)
  IF (error == 0) THEN
    error = lua_pcall_c(L, 0, LUA_MULTRET, 0)
    IF (error /= 0) THEN
      ! TODO: report error string
      call flua_tostring(L, -1, msg)
    ENDIF
  ENDIF
END SUBROUTINE luaL_dostring

SUBROUTINE stackDump(L)
  USE ISO_C_BINDING
  USE fluac, ONLY: lua_gettop_c, lua_toboolean_c
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(IN) :: L
  INTEGER :: i, top, t
  
  top = lua_gettop_c(L)
  WRITE(6, "(A)") "start stack:"
  DO i = 1,top
    t = lua_type(L, i)
    SELECT CASE (t)
      CASE (LUA_TSTRING)
        WRITE(6, 10) cStr2fSTR(lua_tostring(L, i))
10      FORMAT('"', A, '" ')
      CASE (LUA_TBOOLEAN)
        WRITE(6, "(L)") lua_toboolean_c(L, i)
      CASE (LUA_TNUMBER)
        WRITE(6, "(G14.6)") lua_tonumber(L, i)
      CASE DEFAULT
        WRITE(6, "(A)") cStr2fSTR(lua_typename(L, t))
    END SELECT
  END DO
  WRITE(6, "(A)") "end stack"  
END SUBROUTINE stackDump

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


!#define luaL_checkstring(L,n)	(luaL_checklstring(L, (n), NULL))
SUBROUTINE luaL_checkstring(L, n, fstr, error)
  USE ISO_C_BINDING, ONLY: C_PTR, C_CHAR, C_NULL_PTR
  USE fluac, ONLY: luaL_checklstring_c
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
  cstr_ = luaL_checklstring_c(L, n, C_NULL_PTR)
  CALL C_F_CSTR(cstr_, chars, length)
  IF (LEN(fstr) < length) THEN
    error = 1
    RETURN
  END IF
  DO i = 1, length
    fstr(i:i) = chars(i)
  END DO
END SUBROUTINE luaL_checkstring

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

SUBROUTINE flua_opensandboxlibs(L)
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, only: luaL_openlibs_c
  IMPLICIT NONE
  TYPE(C_PTR), VALUE, INTENT(IN) :: L
  
  call luaL_openlibs_c(L)
END SUBROUTINE flua_opensandboxlibs

FUNCTION getTblField(L, fieldname, type_, tableidx)
!
! Note that this puts table values at the top of the
! stack. These need to be removed by the calling subroutine.
!
  USE ISO_C_BINDING, ONLY: C_PTR
  USE fluac, ONLY: lua_gettable_c
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
  CALL lua_gettable_c(L, tblidx)
  
  IF (PRESENT(type_)) THEN
    IF (lua_type(L, -1) /= type_) THEN
      CALL lua_pop(L, 1)
      getTblField = .FALSE.
      RETURN
    ENDIF
  ENDIF
  
  getTblField = .TRUE.
END FUNCTION getTblField

END MODULE flua

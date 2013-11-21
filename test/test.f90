! Copyright (C) 2011 by the f2k3-lua authors, see AUTHORS file.
! Licensed under the MIT license, see LICENSE file.

program test
  use flua
  use ISO_C_BINDING
  implicit none
  !
  type(C_PTR) :: L
  integer :: error
  character(len=1024) :: buf
  character*2 :: nl = CHAR(13) // CHAR(10)
  !
  L = luaL_newstate()
  call flua_opensandboxlibs(L);
  call initDefaultErrfunc(L)

  call luaL_dostring(L, 'q = 10'//nl//'z ~ 4', error)

  call luaL_dostring(L, &
  'foo = "test"' //nl//&
  'bar = "test2"' //nl//&
  'buzz = "hundkatemaus"' //nl//&
  '' //nl//&
  'answer = 42' //nl//&
  '' //nl//&
  'myArray = {' //nl//&
  '   "foo",' //nl//&
  '   "bar"' //nl//&
  '}' //nl//&
  '' //nl//&
  'myTable = {' //nl//&
  '   foo = "bar",' //nl//&
  '   fuzz = "buss"' //nl//&
  '}' //nl//&
  '' //nl//&
  'for i in ipairs(myArray) do' //nl//&
  '   print(i)' //nl//&
  'end', &
  error)

  if(error > 0) then
    buf = " "
    call flua_tostring(L, -1, buf)
    print * , TRIM(buf)
    goto 9999
  end if
  call lua_getglobal(L, "buzz")
  if(.not. lua_isstring(L, -1) ) then
    call stackDump(L)
    print *, "expected string, got something else"
    goto 9999
  end if
  buf = " "
  call flua_tostring(L, -1, buf)
  call lua_pop(L, 1) ! pop string from lua stack

  call lua_getglobal(L, "answer")
  if(.not. lua_isnumber(L, -1) ) then
    call stackDump(L)
    print *, "expected string, got something else"
    goto 9999
  end if
  print *, lua_tonumber(L, -1)
  call lua_pop(L, 1) ! pop number from lua stack
  call stackDump(L)

  call lua_getglobal(L, "myArray")
  if(.not. lua_istable(L, -1) ) then
    call stackDump(L)
    print *, "expected table, got something else"
    goto 9999
  end if
  call lua_pushnil(L)
  do while(lua_next(L, -2))
    call lua_pop(L, 1)
    call stackDump(L)
  end do
  call lua_pop(L, 1) ! pop table
  !
  !
  ! <myTable>
  ! Iterate:
  call lua_getglobal(L, "myTable")
  if(.not. lua_istable(L, -1) ) then
    call stackDump(L)
    print *, "expected table, got something else"
    goto 9999
  end if
  call lua_pushnil(L)
  do while(lua_next(L, -2))
    call stackDump(L)
    call lua_pop(L, 1)
  end do
  !
  call lua_getfield(L, -1, "foo")
  call stackDump(L)
  call lua_pop(L, 1) ! pop value of "foo"
  call lua_getfield(L, -1, "fuzz")
  call stackDump(L)
  call lua_pop(L, 1) ! pop value of "fuzz"
  call stackDump(L)
  !call lua_pop(L, 1) ! pop table
  ! </myTable>
  !
  !
  !
  !
  continue
9999 continue
  call lua_close(L)
  print *, error
  !
contains
  subroutine p_reportExitCode(fehler)
    implicit none
    integer, intent(IN) :: fehler
    !
    if ( fehler > 0 ) then
      STOP 1
    else
      print *,0
    end if
  end subroutine p_reportExitCode
end program test

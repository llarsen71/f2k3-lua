module flua_test
  use flua
  use fruit
  use iso_c_binding
  implicit none

  type(C_PTR) :: L

contains

!=====================================================================

  subroutine flua_test_package()
    implicit none

    call run_case(test_loadfile_pcall,     "test_loadfile_pcall")
    call run_case(test_loadstring_pcall,   "test_loadstring_pcall")
    call run_case(test_lua_gettop,         "test_lua_gettop")
    call run_case(test_lua_settop,         "test_lua_settop")
    call run_case(test_lua_pushvalue,      "test_lua_pushvalue")
    call run_case(test_initDefaultErrfunc, "test_initDefaultErrfunc")
    call run_case(test_lua_pcall,          "test_lua_pcall")
  end subroutine flua_test_package

!=====================================================================

  subroutine run_case(tc, tc_name)
    implicit none
    interface
       subroutine tc()
       end subroutine
    end interface
    character(*), intent(in) :: tc_name

!   Run setup, then the test case, then teardown
    call setup()
    call run_test_case(tc, tc_name)
    call teardown()
  end subroutine

!=====================================================================
! Setup and Teardown
!=====================================================================

  subroutine setup
    implicit none

!   Create a new lua instance and open the default libraries
    L = luaL_newstate()
    call luaL_openlibs(L)
  end subroutine

!=====================================================================

  subroutine teardown
    implicit none
    call lua_close(L)
  end subroutine

!=====================================================================
! Test Cases
!=====================================================================

!=====================================================================
! Load and Execute Files
!=====================================================================

  subroutine test_loadfile_pcall
    implicit none
    integer error
    logical success
    type(cStrPTR) :: str

    open(15, file="test.lua")
    write(15,"(A)") "return 1, {2}"
    close(15)

!   Load the script and put it on the stack
    str = cSTR("test.lua")
    error = luaL_loadfile(L, str%str)
    call assert_equals(0, error, "luaL_loadstring failed")
    deallocate(str%str)
    if (error /= 0) then
      return
    end if

!   Execute the script
    error = lua_pcall_c(L,0,LUA_MULTRET,0)
    call assert_equals(0, error, "lua_pcall failed for script")
    if (error /= 0) then
      return
    end if

 !  Verify that arg 1 (at -2) is a number
    success = lua_isnumber(L, -2)
    call assert_equals(.TRUE., success, "The value on the stack should be 1")
    if (.not.success) then
      return
    end if
    call assert_equals(1, lua_tointeger(L,-2), "The value on the stack should be 1")

!   Verify that are 2 (at -1) is a table
    success = lua_istable(L, -1)
    call assert_equals(.TRUE., success, "The value on the stack should be {2}")
    if (.not.success) then
      return
    end if

!   Delete the file
    open(15, file="test.lua")
    close(15, status="delete")
  end subroutine

!=====================================================================

  subroutine test_loadstring_pcall
    implicit none
    integer error
    logical success
    type(cStrPTR) :: str

!   Load the script and put it on the stack
    str = cSTR("return 1, {2}")
    error = luaL_loadstring(L, str%str)
    call assert_equals(0, error, "luaL_loadstring failed")
    deallocate(str%str)
    if (error /= 0) then
      return
    end if

!   Execute the script
    error = lua_pcall_c(L,0,LUA_MULTRET,0)
    call assert_equals(0, error, "lua_pcall failed for script")
    if (error /= 0) then
      return
    end if

 !  Verify that arg 1 (at -2) is a number
    success = lua_isnumber(L, -2)
    call assert_equals(.TRUE., success, "The value on the stack should be 1")
    if (.not.success) then
      return
    end if
    call assert_equals(1, lua_tointeger(L,-2), "The value on the stack should be 1")

!   Verify that are 2 (at -1) is a table
    success = lua_istable(L, -1)
    call assert_equals(.TRUE., success, "The value on the stack should be {2}")
    if (.not.success) then
      return
    end if
  end subroutine

!=====================================================================
! Basic Stack Manipulation
!=====================================================================

  subroutine test_lua_gettop
!   Get the index for the top of the stack (i.e how many items are on
!   the stack).
    implicit none
    integer i, j

!   Should start with 0 items on the stack.
    i = lua_gettop(L)
    call assert_equals(i, 0, "lua_gettop should return 0")

!   Add 10 items and verify that the stack size is correct.
    do j = 1, 10
      call lua_pushnil(L)
      i = lua_gettop(L)
      call assert_equals(i, j, "lua_gettop returned a bad value")
    end do
  end subroutine

!=====================================================================

  subroutine test_lua_settop
!   Set the index for the top of the stack.
    implicit none
    integer i, top

!   Set the stack top index to the given value and verify that the
!   fame value is returned by lua_gettop.
    do i = 10, 0, -1
      call lua_settop(L, i)
      top = lua_gettop(L)
      call assert_equals(i, top, "lua_gettop did not match the lua_settop value")
    end do

!   Try setting the top using a negative index.
    call lua_settop(L,10)
    call lua_settop(L,-3) ! Backup the top by two values
    top = lua_gettop(L)
    call assert_equals(8, top, "Two values should have been popped from the stack")
  end subroutine

!=====================================================================

  subroutine test_lua_pushvalue
    implicit none
    integer i

    do i = 1, 3
      call lua_pushinteger(L, i)
    end do
    call lua_pushnil(L)

    call lua_pushvalue(L, 2)
    if (.not.lua_isnumber(L, -1)) then
      call add_fail("lua_pushvalue failed - top of stack should be integer")
      return
    endif

    i = lua_tointeger(L, -1)
    call assert_equals(2, i, "lua_pushvalue - value should be 2")
    if (i /= 2) then
      return
    end if

    call lua_pushvalue(L, -2)
    if (.not.lua_isnil(L, -1)) then
      call add_fail("lua_pushvalue failed - top of stack should be nil")
      return
    endif
  end subroutine

!=====================================================================

  subroutine test_initDefaultErrfunc
    implicit none
    integer :: error
    logical :: exists

!   The initDefaultErrorFunc creates a logLuaError function that gets
!   called by default when lua_pcall results in an error. This function
!   first deletes lua_error.log if it exits. If errors occur, it logs the
!   errors to this file.
    call initDefaultErrfunc(L)
    call lua_getglobal(L, "logLuaError")
    call assert_true(lua_isfunction(L,-1),"The logLuaError function was not registered")
    inquire(file="lua_error.log", exist=exists)
    call assert_false(exists, "The lua_error.log file should have been deleted")

!   Try to run a lua script with an error. Verify that the error file is created.
    call luaL_dostring(L, "a ~ 10", error)
    inquire(file="lua_error.log", exist=exists)
    call assert_true(exists, "The lua_error.log file should have been created")
    call luaL_dostring(L, "os.remove('lua_error.log')", error)
    inquire(file="lua_error.log", exist=exists)
    call assert_false(exists, "The lua_error.log file should have been deleted")

!   Set a different logLuaError function that creates a global value called 'worked'
!   Make sure this value is created when an error is raised.
    call initDefaultErrfunc(L, "function logLuaError(msg) worked='worked' end")
    call luaL_dostring(L, "a ~ 10", error)  ! Raise an error
    call lua_getglobal(L, "worked")         !
    if (lua_isstring(L,-1)) then
      call assert_equals("worked", cSTR2fSTR(lua_tostring(L, -1)), "Unexpected value from logLuaError")
    else
      call add_fail("The value returned by logLuaError should be a number")
    end if
    inquire(file="lua_error.log", exist=exists)
    call assert_false(exists, "The lua_error.log file should not be created")
  end subroutine

!=====================================================================

  subroutine test_lua_pcall
    implicit none
    integer error
    type(cStrPTR) :: str

    str = cSTR("return 'Got this'")
    error = luaL_loadstring(L, str%str)
    deallocate(str%str)
    error = lua_pcall(L, 0, 1)
    if (lua_isstring(L, -1)) then
      call assert_equals('Got this', cSTR2fSTR(lua_tostring(L, -1)))
    else
      call add_fail("A string should have been put on the stack by pcall")
    end if

!   Try setting an error function
    str = cSTR("function err(msg) test=1 end"//nwln//"return err")
    error = luaL_loadstring(L, str%str)
    deallocate(str%str)

    str = cSTR("a = nil > nil")
    error = luaL_loadstring(L, str%str)
    deallocate(str%str)

    error = lua_pcall(L, 0, 1, -2)
    call lua_getglobal(L, "test")
    call assert_true(lua_isnumber(L,-1), "The error function didn't appear to be called.")
  end subroutine

!=====================================================================

end module flua_test

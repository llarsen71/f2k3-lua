module flua_util_test
  use fluautil
  use flua
  use iso_c_binding
  use fruit
  implicit none

  type(C_PTR) :: L

contains

!=====================================================================

  subroutine flua_util_test_package()
    implicit none

    call run_case(test_PRM,     "test_PRM")
    call run_case(test_luaCall, "test_luaCall")
  end subroutine flua_util_test_package

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
    call initDefaultErrfunc(L, &
      'function logLuaError(...)' // nwln // &
      '  print "Lua Error:"' // nwln // &
      '  for i, error in ipairs{...} do' // nwln // &
      '    print(error)' // nwln // &
      '  end' // nwln // &
      'end')
  end subroutine

!=====================================================================

  subroutine teardown
    implicit none
    call lua_close(L)
  end subroutine

!=====================================================================
! Test Cases
!=====================================================================

  subroutine test_PRM
    implicit none
    type(PARAM), pointer :: prm_
    integer i
    double precision d

#ifdef hasclass
#define _PRM_
#else
#define _PRM_ prm_,
#endif
    prm_ => PRM(1)
    call prm_%push2Stack(_PRM_ L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(1, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    prm_ => PRM(2, .FALSE.)
    call prm_%push2Stack(_PRM_ L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(2, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    prm_ => PRM(5.4)
    call prm_%push2Stack(_PRM_ L)
    if (lua_isnumber(L, -1)) then
      d = lua_tonumber(L, -1)
      call assert_equals(5.4, real(d),"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    continue
  end subroutine

!=====================================================================

  subroutine test_luaCall
    implicit none
    type(cStrPTR) :: script

    script = cSTR( &
    "function callme(n, i, r, s)" // nwln // &
    "  local errs = ''" // nwln // &
    "  local function ER(err) errs=errs..err..';' end" // nwln // &
    "  if (n~=nil) then ER('Expected n=nil, got n='..n) end" // nwln // &
    "  if (i~=1) then ER('Expected i=1, got i='..i) end" // nwln // &
    "  if (r<2.399 or r>2.401) then ER('Expected r=2.4, got r='..r) end" // nwln // &
    "  if (s~='test') then ER('Expected s=test, got s='..s) end" // nwln // &
    "  if errs == '' then return nil end" // nwln // &
    "  return errs" // nwln // &
    "end" // nwln )

    if (.not.fluaL_dostring(L, cSTR2fSTR(script))) then
      call add_fail("Failed to run the luaCall test function")
      return
    end if

    if (.not.luaCall(L, "callme", (/ PRMnil(), PRM(1), PRM(2.4), PRM('test') /), 1)) then
      call add_fail("An error occurred executing the 'callme' function")
    else
!     A nil value should be on stack if callme was successful
      if (.not.lua_isnil(L, -1)) then
        if (lua_isstring(L, -1)) then
          call add_fail(cSTR2fSTR(lua_tostring(L,-1)))
        else
          call add_fail("A nil value was expected from 'callme'")
        end if
      else
      end if
    end if
  end subroutine

end module flua_util_test

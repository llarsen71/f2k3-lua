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

    prm_ => PRM(1)
    call prm_%push2Stack(prm_, L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(1, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    prm_ => PRM(2, .FALSE.)
    call prm_%push2Stack(prm_, L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(2, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    prm_ => PRM(5.4)
    call prm_%push2Stack(prm_, L)
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
    "function callme(i, r) " // nwln // &
    "  errors = {} " // nwln // &
    "  if (i/=1) then errors" // nwln // &
    "  " // nwln // &
    "  " // nwln // &
    "  " // nwln // &
    "  " // nwln // &
    "end" // nwln // &

    if (.not.fluaL_loadstring(L, )) then
      call add_fail("Failed to load the luaCall test function")
      return
    end if
    if (.not.flua_pcall(L, 0, -1)) then
      call add_fail("pcall failed to add luaCall test function")
      return
    end if

    if (.not.luaCall(L, "callme", (/ PRM(1), PRM(2.4) /))) then
      continue
    end if
  end subroutine

end module flua_util_test

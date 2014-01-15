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

    call run_case(test_PRM,             "test_PRM")
    call run_case(test_luaCall,         "test_luaCall")
    call run_case(test_pushtable_items, "test_pushtable_items")
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
    character*50 :: str = ""
    integer, pointer :: dat

    ! Test PRMint
    prm_ => PRM(1)
    call prm_%push2Stack(L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(1, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    prm_ => PRM(2, .FALSE.)
    call prm_%push2Stack(L)
    if (lua_isnumber(L, -1)) then
      i = lua_tointeger(L, -1)
      call assert_equals(2, i,"pushInt failed")
    else
      call add_fail("An integer should have been pushed to the stack")
    end if

    ! Test PRMreal
    prm_ => PRM(5.4)
    call prm_%push2Stack(L)
    if (lua_isnumber(L, -1)) then
      d = lua_tonumber(L, -1)
      call assert_equals(5.4, real(d),"pushReal failed")
    else
      call add_fail("A real value should have been pushed to the stack")
    end if

    ! Test PRMstring
    prm_ => PRM("this is a test")
    call prm_%push2Stack(L)
    if (lua_isstring(L, -1)) then
      call flua_tostring(L, -1, str)
      call assert_equals("this is a test", str, "pushStr failed")
    else
      call add_fail("A string should have been pushed to the stack")
    end if

    ! Test PRMnil
    prm_ => PRM()
    call prm_%push2Stack(L)
    if (.not.lua_isnil(L, -1)) then
      call add_fail("A nil value should have been pushed to the stack")
    end if

    ! Test PRMusrt
    call flua_register_usertype(L, "prm_test", &
        (/ FNCPTR("test", c_funloc(lua_test)) /))
    allocate(dat)
    dat = 5
    prm_ => PRM("prm_test", C_LOC(dat))
    call prm_%push2Stack(L)
    if (.not.lua_isuserdata(L, -1)) then
      call add_fail("A userdata should have been pushed to the stack")
    end if
    call c_f_pointer(flua_check_usertype(L, "prm_test", -1), dat)
    call assert_equals(5,dat,"Should have returned the integer value pass to PRM")
    deallocate(dat)

    continue
  end subroutine

  subroutine lua_test(lua)
    implicit none
    type(C_PTR), pointer :: lua
  end subroutine lua_test

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

    if (.not.luaCall(L, "callme", (/ PRMnil(), PRM(1), PRM(2.4), PRM('test') /), (/ LUA_TNIL /))) then
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

!=====================================================================

  subroutine test_pushtable_items()
    implicit none
    logical :: success
    type(PARAM), pointer :: prms(:)

    call pushtable_items(L, (/ PRM(4), PRM("test"), PRM(3) /))
    call assert_true(getTblItems(L, (/ 1, 2, 3 /), &
                     (/ LUA_TNUMBER, LUA_TSTRING, LUA_TNUMBER /)), &
                     "Type mismatch for pushtable_items")

    call assert_true(stackEquals((/ PRM(4), PRM("test"), PRM(3) /)), &
      "Item set by pushtable_items not correct")
    call lua_settop(L, 1)

    allocate(prms(2))
    prms = (/ PRM("bob"), PRM(2) /)
    call pushtable_items(L, prms, 1, 3)

    call assert_true(getTblItems(L, (/ 1, 2, 3, 4 /), &
                     (/ LUA_TNUMBER, LUA_TSTRING, LUA_TSTRING, LUA_TNUMBER /)), &
                     "Type mismatch for pushtable_items")

    call assert_true(stackEquals((/ PRM(4), PRM("test"), PRM("bob"), PRM(2) /)), &
      "Item set by pushtable_items not correct")
    call lua_settop(L, 1)
  end subroutine

!=====================================================================

  function stackEquals(prms, startat) result(isequal)
    implicit none
    type(PARAM) :: prms(:)
    integer, optional :: startat
    integer :: i, startat_, offset
    logical :: isequal

    startat_ = -size(prms)
    if (PRESENT(startat)) startat_ = startat

!   If this is a negative index take into account that we will add item to
!   stack.
    offset = 1
    if (startat_ < 0) offset = 2

    isequal = .false.
    do i = 1, size(prms)
      call prms(i)%push2stack(L)
      if(.not.flua_equal(L, -1, startat_-offset+i)) then
        call lua_pop(L, 1)
        return
      endif
      call lua_pop(L, 1)
    end do
    isequal = .true.
  end function

end module flua_util_test

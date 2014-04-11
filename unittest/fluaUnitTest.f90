program fluaUnitTest
  use fruit
  use flua_test
  use flua_util_test
  implicit none

  call init_fruit
  call flua_test_package
  call flua_util_test_package
  call fruit_summary
  call rw()
  
  continue

contains

  subroutine rw()
    use fluautil
    use flua
    use iso_c_binding
    implicit none
    type(C_PTR) :: L
    logical :: success
    integer :: err
    character*200 :: msg
    
    L = luaL_newstate()
    call luaL_openlibs(L)
    call luaL_doFile(L, "example.lua", err)
    if (err /= 0) then
      call flua_tostring(L, 1, msg)
    endif
    call lua_close(L)
    continue
  end subroutine
  
end program fluaUnitTest

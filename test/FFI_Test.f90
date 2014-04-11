module FFI_Test
  use iso_c_binding
  implicit none
  
  type mydata
    SEQUENCE
    integer i, j
  end type mydata
  
  abstract interface
    subroutine sendMyData_(info) bind(C)
      import :: mydata
      type(mydata) :: info
    end subroutine
  end interface
  
  procedure(sendMyData_), pointer :: sendMyData => NULL()
  
  contains

  subroutine registerLuaCallback(callback)
! The following exports this function so that it can be called
! using the LuaJIT FFI interface.
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"registerLuaCallback" :: registerLuaCallback
    use iso_c_binding, only: c_funptr, c_f_procpointer
    implicit none
    type(c_funptr), value :: callback
    type(mydata) :: info
    
    call c_f_procpointer(callback, sendMyData)
    
    info%i = 15
    info%j = 25
    call sendMyData(info)
  end subroutine
  
  subroutine callme(info)
! The following exports this function so that it can be called
! using the LuaJIT FFI interface.
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"callme" :: callme
    use iso_c_binding, only: c_int
    implicit none
    type(mydata) :: info
    
    write(*,*) info%i, info%j
  end subroutine
  
end module FFI_Test
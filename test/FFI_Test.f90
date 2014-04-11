module FFI_Test
  use iso_c_binding
  implicit none
  
  type mydata
    SEQUENCE
    integer i, j
  end type mydata
  
  contains
  
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
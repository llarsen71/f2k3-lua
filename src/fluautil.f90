MODULE fluautil
  use iso_c_binding
  use flua
  
  type PARAM_TYPE
    character :: symbol
    procedure(), nopass, pointer :: push2Stack => NULL()
    type(PARAM_TYPE), pointer :: next => NULL()
  end type PARAM_TYPE
  
  type(PARAM_TYPE), pointer :: param_types => NULL()

  type PARAM
    type(C_PTR) :: val
    type(cStrPTR), pointer :: name => NULL()
    type(PARAM_TYPE) :: type
  end type PARAM
  
CONTAINS

!---------------------------------------------------------------------

  subroutine init_fluautil()
    implicit none
    call addParamType('i', pushInteger)
    call addParamType('r', pushReal)
  end subroutine init_fluautil
  
!---------------------------------------------------------------------

  subroutine addParamType(symbol, pushfn)
    character :: symbol
    procedure(), pointer :: pushfn
    type(PARAM_TYPE), pointer :: next
    
    if (.not.associated(param_types)) then
      allocate(param_types)
      next => param_types
    else
      next => param_types
      do
        if (associated(next%next)) then
          next => next%next
          cycle
        end if
        allocate(next%next)
        next => next%next
        exit
      end do
    endif
    
    next%symbol = symbol
    next%push2Stack => pushfn
  end subroutine addParamType
  
!---------------------------------------------------------------------

  subroutine pushInteger(L, idx, prm)
    type(C_PTR), value :: L
    integer :: idx
    type(PARAM) :: prm
    integer, pointer :: i
    
    call C_F_POINTER(prm%val, i)
    call lua_pushInteger(L, i)
  end subroutine pushInteger

!---------------------------------------------------------------------

  subroutine pushReal(L, idx, prm)
    type(C_PTR), value :: L
    integer :: idx
    type(PARAM) :: prm
    integer, pointer :: i
    real, pointer :: r
    double precision :: d
    
    call C_F_POINTER(prm%val, r)
    d = r
    call lua_pushNumber(L, d)
  end subroutine pushReal

!---------------------------------------------------------------------

  function luaCall(L, fncname, params, returntypes) result(success)
    implicit none
    type(C_PTR), value :: L
    character*(*) :: fncname
    type(PARAM) :: params(:)
    type(PARAM) :: returntypes(:)
    logical :: success
    
    
  end function luaCall

!---------------------------------------------------------------------

END MODULE fluautil
MODULE fluautil
  use iso_c_binding
  use flua
  implicit none

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

!=====================================================================

  subroutine init_fluautil()
    implicit none
    procedure(), pointer :: fn
    fn => pushInteger
    call addParamType('i', fn)
    fn => pushReal
    call addParamType('r', fn)
  end subroutine init_fluautil

!=====================================================================

  subroutine addParamType(symbol, pushfn)
    implicit none
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

!=====================================================================

  subroutine pushInteger(L, prm)
    implicit none
    type(C_PTR), value :: L
    type(PARAM) :: prm
    integer, pointer :: i

    call C_F_POINTER(prm%val, i)
    call lua_pushInteger(L, i)
  end subroutine pushInteger

!=====================================================================

  subroutine pushReal(L, prm)
    implicit none
    type(C_PTR), value :: L
    type(PARAM) :: prm
    real, pointer :: r
    double precision :: d

    call C_F_POINTER(prm%val, r)
    d = r
    call lua_pushNumber(L, d)
  end subroutine pushReal

!=====================================================================

  function luaCall(L, fncname, params, returntypes) result(success)
    implicit none
    type(C_PTR), value :: L
    character*(*) :: fncname
    type(PARAM), optional :: params(:)
    type(PARAM), optional :: returntypes(:)
    logical :: success
    integer :: i, n, nretvals, error

!   TODO: parse this for dot notation.
    call lua_getglobal(L, fncname)
    if (lua_isnil(L, -1)) then
      success = .false.
      return
    end if

  if (PRESENT(params)) then
    n = size(params)
    do i = 1, n
      call params(i)%type%push2Stack(L, params(i))
    end do
  else
    n = 0
  end if

  if (PRESENT(returntypes)) then
    nretvals = size(returntypes)
  else
    nretvals = LUA_MULTRET
  end if

  error = lua_pcall(L, n, nretvals, 0)
  if (error /= 0) then
    !TODO: print error
  end if

  end function luaCall

!=====================================================================

END MODULE fluautil

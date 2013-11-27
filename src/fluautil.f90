MODULE fluautil
  use iso_c_binding
  use flua
  implicit none

  type PARAM
    type(C_PTR) :: val
    character*2 :: type
    procedure(), nopass, pointer :: push2Stack => NULL()
  end type PARAM

  interface PRM
    module procedure PRMint, PRMreal, PRMstr
  end interface

CONTAINS

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
        call params(i)%push2Stack(params(i),L)
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

  function PRMint(intval, copy)
    implicit none
    integer, target :: intval
    logical, optional :: copy
    type(PARAM), pointer :: PRMint
    logical copy_
    integer, pointer :: intval_

    copy_ = .true.
    if (PRESENT(copy)) then
      copy_ = copy
    end if
    if (copy_) then
      allocate(intval_)
      intval_ = intval
    else
      intval_ => intval
    endif

    allocate(PRMint)
    PRMint%type = "i"
    PRMint%push2Stack => pushInt
    PRMint%val = C_LOC(intval_)
  end function PRMint

!=====================================================================

  subroutine pushInt(prm, L)
    implicit none
    type(PARAM) :: prm
    type(C_PTR) :: L
    integer, pointer :: i

    call C_F_POINTER(prm%val, i)
    call lua_pushInteger(L, i)
  end subroutine pushInt

!=====================================================================

  function PRMreal(realval, copy)
    implicit none
    real, target :: realval
    logical, optional :: copy
    type(PARAM), pointer :: PRMreal
    logical copy_
    real, pointer :: realval_

    copy_ = .true.
    if (PRESENT(copy)) then
      copy_ = copy
    end if
    if (copy_) then
      allocate(realval_)
      realval_ = realval
    else
      realval_ => realval
    endif

    allocate(PRMreal)
    PRMreal%type = "r"
    PRMreal%push2Stack => pushReal
    PRMreal%val = C_LOC(realval_)
  end function PRMreal

!=====================================================================

  subroutine pushReal(prm, L)
    implicit none
    type(PARAM) :: prm
    type(C_PTR) :: L
    real, pointer :: r
    double precision :: d

    call C_F_POINTER(prm%val, r)
    d = r
    call lua_pushNumber(L, d)
  end subroutine pushReal

!=====================================================================

  function PRMstr(strval)
    implicit none
    character*(*) :: strval
    type(PARAM), pointer :: PRMstr
    type(cStrPTR), target :: strval_

    strval_ = cSTR(strval, .FALSE.)

    allocate(PRMstr)
    PRMstr%type = "s"
    PRMstr%push2Stack => pushStr
    PRMstr%val = C_LOC(strval_)
  end function PRMstr

!=====================================================================

  subroutine pushStr(prm, L)
    implicit none
    type(PARAM) :: prm
    type(C_PTR) :: L
    type(cStrPTR), pointer :: strval

    call C_F_POINTER(prm%val, strval)
    call lua_pushstring(L, cstr2fstr(strval))
  end subroutine pushStr

!=====================================================================

  function PRMcfn(cfunc)
    implicit none
    type(C_FUNPTR), target :: cfunc
    type(PARAM), pointer :: PRMcfn

    allocate(PRMcfn)
    PRMcfn%type = "fn"
    PRMcfn%push2Stack => pushcfn
    PRMcfn%val = C_LOC(cfunc)
  end function PRMcfn

!=====================================================================

  subroutine pushcfn(prm, L)
    implicit none
    type(PARAM) :: prm
    type(C_PTR) :: L
    type(C_FUNPTR), pointer :: cfn

    call C_F_POINTER(prm%val, cfn)
    call pushcfn_(cfn)
  contains
    subroutine pushcfn_(fn)
      type(C_FUNPTR) :: fn
      call lua_pushcfunction(L, fn)
    end subroutine
  end subroutine pushcfn

!=====================================================================

  function NIL()
    implicit none
    type(PARAM), pointer :: NIL
    allocate(NIL)
    NIL%type = "nl"
    NIL%push2Stack => pushnil
  end function NIL

!=====================================================================

  subroutine pushnil(prm, L)
    implicit none
    type(PARAM) :: prm
    type(C_PTR) :: L
    call lua_pushnil(L)
  end subroutine pushnil

!=====================================================================

END MODULE fluautil

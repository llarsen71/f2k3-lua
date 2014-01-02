MODULE fluautil
  use iso_c_binding
  use flua
  implicit none

  type PARAM
    type(C_PTR) :: val = C_NULL_PTR
    character*2 :: type
    procedure(push2stack_), PASS(this), pointer :: push2stack => NULL()
  end type PARAM

  abstract interface
    subroutine push2stack_(this, L)
      use iso_c_binding, only: c_ptr
      import :: PARAM
      class(PARAM) :: this
      type(c_ptr) :: L
    end subroutine
  end interface

  interface PRM
    module procedure PRMint, PRMreal, PRMstr, PRMnil
  end interface

CONTAINS

!=====================================================================
! luaCall - for convenience in calling Lua functions
!=====================================================================

  function luaCall(L, fncname, params, retvals) result(success)
    implicit none
    type(C_PTR), value    :: L
    character*(*)         :: fncname
    type(PARAM), optional :: params(:)
    integer, optional     :: retvals(:)
    logical :: success
    integer :: i, n, error, nretvals_

    call luaL_dostring(L, "return "//fncname, error)
    if (error /= 0) then
      success = .false.
      return
    end if

    if (PRESENT(params)) then
      n = size(params)
      do i = 1, n
        call params(i)%push2Stack(L)
      end do
    else
      n = 0
    end if

    if (PRESENT(retvals)) then
      nretvals_ = size(retvals)
    else
      nretvals_ = LUA_MULTRET
    end if

    error = lua_pcall(L, n, nretvals_, 0)
    if (error == 0) then
      if (PRESENT(retvals)) then
        success = checkStackTypes(L, retvals)
      else
        success = .true.
      end if
    else
      call handleError(L)
      success = .false.
    end if
  end function luaCall

!=====================================================================
! Parameters
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
    class(PARAM) :: prm
    type(C_PTR) :: L
    integer, pointer :: i

    call C_F_POINTER(prm%val, i)
    call lua_pushInteger(L, i)
  end subroutine pushInt

!=====================================================================

  function PRMreal(realval, copy) result(prm_)
    implicit none
    real, target :: realval
    logical, optional :: copy
    type(PARAM), pointer :: prm_
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

    allocate(prm_)
    prm_%type = "r"
    prm_%push2Stack => pushReal
    prm_%val = C_LOC(realval_)
  end function PRMreal

!=====================================================================

  subroutine pushReal(prm, L)
    implicit none
    class(PARAM) :: prm
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
    type(cStrPTR), pointer :: strval_

    allocate(strval_)
    strval_ = cSTR(strval, .FALSE.)

    allocate(PRMstr)
    PRMstr%type = "s"
    PRMstr%push2Stack => pushStr
    PRMstr%val = C_LOC(strval_)
  end function PRMstr

!=====================================================================

  subroutine pushStr(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    type(cStrPTR), pointer :: strval

    call C_F_POINTER(prm%val, strval)
    call lua_pushstring(L, cSTR2fSTR(strval))
    deallocate(strval)
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
    class(PARAM) :: prm
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

  function PRMnil() result(NIL)
    implicit none
    type(PARAM), pointer :: NIL
    allocate(NIL)
    NIL%type = "nl"
    NIL%push2Stack => pushnil
  end function PRMnil

!=====================================================================

  subroutine pushnil(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L

    call lua_pushnil(L)
  end subroutine pushnil

!=====================================================================

END MODULE fluautil

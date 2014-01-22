MODULE fluautil
  use iso_c_binding
  use flua
  implicit none

! The PARAM structure is used to create parameter arrays for calling lua
! functions (via luaCall), for pushing values to tables (via push_tableitems).
  type PARAM
    type(C_PTR)   :: val = C_NULL_PTR
    character*2   :: type
    procedure(push2stack_), PASS(this), pointer :: push2stack => NULL()
  end type PARAM

  type fluautil_usertype
    type(cStrPTR) :: typename
    type(C_PTR)   :: cptr
  end type fluautil_usertype

  abstract interface
    subroutine push2stack_(this, L)
      use iso_c_binding, only: c_ptr
      import :: PARAM
      class(PARAM) :: this
      type(c_ptr) :: L
    end subroutine
  end interface

  interface PRM
    module procedure PRMint, PRMreal, PRMstr, PRMcstr, PRMnil, PRMusrt
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
    integer :: n, error, nretvals_

    call luaL_dostring(L, "return "//fncname, error)
    if (error /= 0) then
      success = .false.
      return
    end if

    if (PRESENT(params)) then
      n = size(params)
      call PRM2Stack(L, params)
    else
      n = 0
    end if

    if (PRESENT(retvals)) then
      nretvals_ = size(retvals)
    else
      nretvals_ = LUA_MULTRET
    end if

    error = lua_pcall(L, n, nretvals_)
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

  subroutine PRM2Stack(L, params)
    implicit none
    type(C_PTR), value    :: L
    type(PARAM), optional :: params(:)
    integer :: i, n

    n = size(params)
    do i = 1, n
        call params(i)%push2Stack(L)
    end do
  end subroutine

!=====================================================================

  subroutine pushtable_items(L, params, idx, start_at)
    implicit none
    type(C_PTR), value    :: L
    type(PARAM), optional :: params(:)
    integer, optional     :: idx
    integer, optional     :: start_at
    integer :: i, n, idx_, start_at_

!   If a table index wasn't passed in, push a table to the stack.
    if (present(idx)) then
      idx_ = idx
    else
      call lua_pushtable(L)
      idx_ = -1
    endif

    start_at_ = 1
    if (present(start_at)) start_at_ = start_at

!       If this is a negative index take into account that we will add
!       key and value to the stack before pushing them to the table.
    if (idx_ < 0) idx_ = idx_-2

    n = size(params)
    do i = 0, n-1
      call PRM2Stack(L, (/ PRM(start_at_ + i), params(i+1) /))
      call lua_settable(L, idx_) ! set table[i] = params(i)
    end do
  end subroutine pushtable_items

!=====================================================================
! Parameter: int
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
! Parameter: real
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
! Parameter: string
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
! Parameter: cstring
!=====================================================================

  function PRMcstr(strval)
    implicit none
    type(cStrPTR) :: strval
    type(PARAM), pointer :: PRMcstr
    type(cStrPTR), pointer :: strval_

    allocate(strval_)
    strval_%str => strval_%str

    allocate(PRMcstr)
    PRMcstr%type = "s"
    PRMcstr%val = C_LOC(strval_)
    ! Use the pushStr subroutine since the data is the same as PRMstr
    PRMcstr%push2Stack => pushStr
  end function PRMcstr

!=====================================================================
! Parameter: cfunction (cfn)
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
    call pushcfn_(L, cfn)
  end subroutine pushcfn

  subroutine pushcfn_(L, fn)
    implicit none
    type(C_PTR) :: L
    type(C_FUNPTR) :: fn
    call lua_pushcfunction(L, fn)
  end subroutine

!=====================================================================
! Parameter: nil
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
! Parameter: usertype
!=====================================================================

  function PRMusrt(typename, cptr)
    implicit none
    character(*) :: typename
    type(C_PTR)  :: cptr
    type(PARAM), pointer :: PRMusrt
    type(fluautil_usertype), pointer :: usertype

    allocate(usertype)
    usertype%typename = cSTR(typename)
    usertype%cptr = cptr

    allocate(PRMusrt)
    PRMusrt%type = "ut"
    PRMusrt%push2Stack => pushusrt
    PRMusrt%val = C_LOC(usertype)
  end function PRMusrt

!=====================================================================

  subroutine pushusrt(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    type(fluautil_usertype), pointer :: usertype

    call C_F_POINTER(prm%val, usertype)
    call flua_push_usertype(L, cStr2FStr(usertype%typename), usertype%cptr)
  end subroutine pushusrt

!=====================================================================
! Parameter from the stack
!=====================================================================

  function PRMat(idx)
    implicit none
    integer :: idx
    type(PARAM), pointer :: PRMat
    integer, pointer :: idx_

    allocate(idx_)
    idx_ = idx

    allocate(PRMat)
    PRMat%type = "at"
    PRMat%push2Stack => pushvalueat
    PRMat%val = C_LOC(idx_)
  end function PRMat

!=====================================================================

  subroutine pushvalueat(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    integer, pointer :: idx

    call C_F_POINTER(prm%val, idx)
    call lua_pushvalue(L, idx)
    deallocate(idx)
  end subroutine pushvalueat

!=====================================================================

END MODULE fluautil

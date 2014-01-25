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
    procedure(dealloc_), PASS(this), pointer :: dealloc => NULL()
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
    subroutine dealloc_(this)
      import :: PARAM
      class(PARAM) :: this
    end subroutine
  end interface

  interface PRM
    module procedure PRMint, PRMreal, PRMstr, PRMcstr, PRMusrt, PRMtbl, &
      PRMTblItem
  end interface

  interface TblPRM
    module procedure PRMTblItem
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

  function PRMint(intval) result(prm_)
    implicit none
    integer, target :: intval
    type(PARAM), pointer :: prm_
    logical copy_
    integer, pointer :: intval_

    allocate(intval_)
    intval_ = intval

    allocate(prm_)
    prm_%type = "i"
    prm_%val = C_LOC(intval_)
    prm_%push2Stack => pushInt
    prm_%dealloc => deallocInt
  end function PRMint

!=====================================================================

  subroutine pushInt(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    integer, pointer :: i

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, i)
    call lua_pushInteger(L, i)
    call prm%dealloc()
  end subroutine pushInt

!=====================================================================

  subroutine deallocInt(prm)
    implicit none
    class(PARAM) :: prm
    integer, pointer :: i

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, i)
    deallocate(i)
  end subroutine deallocInt

!=====================================================================
! Parameter: real
!=====================================================================

  function PRMreal(realval) result(prm_)
    implicit none
    real, target :: realval
    type(PARAM), pointer :: prm_
    logical copy_
    real, pointer :: realval_

    allocate(realval_)
    realval_ = realval

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

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, r)
    d = r
    call lua_pushNumber(L, d)
  end subroutine pushReal

!=====================================================================

  subroutine deallocReal(prm)
    implicit none
    class(PARAM) :: prm
    real, pointer :: realval_

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, realval_)
    deallocate(realval_)
  end subroutine deallocReal

!=====================================================================
! Parameter: string
!=====================================================================

  function PRMstr(strval) result(prm_)
    implicit none
    character*(*) :: strval
    type(PARAM), pointer :: prm_
    type(cStrPTR), pointer :: strval_

    allocate(strval_)
    strval_ = cSTR(strval, .FALSE.)

    allocate(prm_)
    prm_%type = "s"
    prm_%val = C_LOC(strval_)
    prm_%push2Stack => pushStr
    prm_%dealloc => deallocStr
  end function PRMstr

!=====================================================================

  subroutine pushStr(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    type(cStrPTR), pointer :: strval

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, strval)
    call lua_pushstring(L, cSTR2fSTR(strval))
    call prm%dealloc()
  end subroutine pushStr

!=====================================================================

  subroutine deallocStr(prm)
    implicit none
    class(PARAM) :: prm
    type(cStrPTR), pointer :: strval

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, strval)
    call dealloc_cSTR(strval)
    deallocate(strval)
  end subroutine deallocStr

!=====================================================================
! Parameter: cstring
!=====================================================================

  function PRMcstr(strval) result(prm_)
    implicit none
    type(cStrPTR) :: strval
    type(PARAM), pointer :: prm_
    type(cStrPTR), pointer :: strval_

    allocate(strval_)
    strval_%str => strval_%str

    allocate(prm_)
    prm_%type = "s"
    prm_%val = C_LOC(strval_)
    ! Use the pushStr subroutine since the data is the same as PRMstr
    prm_%push2Stack => pushStr
    prm_%dealloc => deallocStr
  end function PRMcstr

!=====================================================================
! Parameter: cfunction (cfn)
!=====================================================================

  function PRMcfn(cfunc) result(prm_)
    implicit none
    type(C_FUNPTR), target :: cfunc
    type(PARAM), pointer :: prm_

    allocate(prm_)
    prm_%type = "fn"
    prm_%val = C_LOC(cfunc)
    prm_%push2Stack => pushcfn
    prm_%dealloc => dealloc_cfn
  end function PRMcfn

!=====================================================================

  subroutine pushcfn(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L
    type(C_FUNPTR), pointer :: cfn

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
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

  subroutine dealloc_cfn(prm)
    implicit none
    class(PARAM) :: prm
  end subroutine dealloc_cfn

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

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, usertype)
    call flua_push_usertype(L, cStr2FStr(usertype%typename), usertype%cptr)
  end subroutine pushusrt

!=====================================================================
! Parameter to Add Table
!=====================================================================

  function PRMTbl() result(prm_)
    implicit none
    type(PARAM), pointer :: prm_

    allocate(prm_)
    prm_%type = "tb"
    prm_%push2stack => pushTableParam
  end function PRMTbl

!=====================================================================

  subroutine pushTableParam(prm, L)
    implicit none
    class(PARAM) :: prm
    type(C_PTR) :: L

    call lua_pushtable(L)
  end subroutine pushTableParam

!=====================================================================
! Parameters to Add Table Items
!=====================================================================

  function PRMTblItem(key_or_val, val, tbl_idx) result(prm_)
    implicit none
    type kv
      type(PARAM), pointer :: key => NULL()
      type(PARAM), pointer :: val => NULL()
      integer :: tbl_idx
    end type kv
    type(PARAM), pointer :: key_or_val
    type(PARAM), pointer, optional :: val
    integer, optional :: tbl_idx
    type(PARAM), pointer :: prm_
    type(kv), pointer :: kvptr

    allocate(kvptr)
    if (PRESENT(val)) then
      kvptr%key => key_or_val
      kvptr%val => val
    else
      kvptr%val => key_or_val
    endif

!   0 is used to indicate that value was not set.
    kvptr%tbl_idx = 0
    if (PRESENT(tbl_idx)) kvptr%tbl_idx = tbl_idx

    allocate(prm_)
    prm_%type = "ti"
    prm_%push2stack => pushTableItemParam
  end function PRMTblItem

!=====================================================================

  subroutine pushTableItemParam(prm, L)
    implicit none
    type kv
      type(PARAM), pointer :: key => NULL()
      type(PARAM), pointer :: val => NULL()
      integer :: tbl_idx
    end type kv
    class(PARAM) :: prm
    type(C_PTR) :: L
    integer, pointer :: idx
    type(kv), pointer :: kvptr

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, kvptr)
    if (kvptr%tbl_idx == 0) then
!     If no table index was given, and the top of the stack is
!     not a table, add one that we can add items to.
      if (.not.lua_istable(L, -1)) then
        call lua_pushtable(L)
      endif
      kvptr%tbl_idx = -1
    else

    endif
    call lua_pushtable(L)
    deallocate(idx)
  end subroutine pushTableItemParam

!*********************************************************************
! Parameters that are NOT part of the PRM interface
!*********************************************************************

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

    ! Should an error be raised here?
    if (.not.c_associated(prm%val)) return
    call C_F_POINTER(prm%val, idx)
    call lua_pushvalue(L, idx)
    deallocate(idx)
  end subroutine pushvalueat

!=====================================================================

END MODULE fluautil

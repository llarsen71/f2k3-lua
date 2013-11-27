program fluaUnitTest
  use fruit
  use flua_test
  use flua_util_test
  implicit none

  call init_fruit
  call flua_test_package
  call flua_util_test_package
  call fruit_summary
end program fluaUnitTest

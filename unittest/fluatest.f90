program fluatest
  use fruit
  use flua_test
  implicit none

  call init_fruit
  call flua_test_package
  call fruit_summary
end program fluatest

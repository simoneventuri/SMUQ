Module SharedLib_Tools

  use, intrinsic :: iso_c_binding

  implicit none

  private
  public  ::  rtld_lazy
  public  ::  rtld_now
  public  ::  dlopen
  public  ::  dlsym
  public  ::  dlclose

  integer(c_int)  ,parameter :: rtld_lazy = 1 ! value extracte from the C header file
  integer(c_int)  ,parameter :: rtld_now  = 2 ! value extracte from the C header file

  Interface

    Function dlopen(filename,mode) bind(c,name="dlopen")
      use iso_c_binding
      implicit none
      type(c_ptr) :: dlopen
      character(c_char), intent(in) :: filename(*)
      integer(c_int), value :: mode
    End Function

    Function dlsym(handle,name) bind(c,name="dlsym")
      use iso_c_binding
      implicit none
      type(c_funptr) :: dlsym
      type(c_ptr), value :: handle
      character(c_char), intent(in) :: name(*)
    End Function

    Function dlclose(handle) bind(c,name="dlclose")
      use iso_c_binding
      implicit none
      integer(c_int) :: dlclose
      type(c_ptr), value :: handle
    End Function

  End Interface

End Module
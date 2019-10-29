add_subdirectory(Axis)
add_subdirectory(Colorbar)
add_subdirectory(Data)
add_subdirectory(Linestyle)
add_subdirectory(Margin)
add_subdirectory(Plot)
add_subdirectory(pm3d)

add_sources (
  GPF_Size_Class.F90      GPF_Size_SubClass.F90
  GPF_Origin_Class.F90    GPF_Origin_SubClass.F90
  GPF_View_Class.F90      GPF_View_SubClass.F90
  GPF_Title_Class.F90     GPF_Title_SubClass.F90
  GPF_Key_Class.F90       GPF_Key_SubClass.F90
  GPF_Grid_Class.F90      GPF_Grid_SubClass.F90
  GPF_Graph_Class.F90     GPF_Graph_SubClass.F90
)
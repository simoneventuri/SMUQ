add_sources (
  gpf_data_base_class.f90
  gpf_data_x1y1_class.f90
  gpf_data_x1y2_class.f90
  gpf_data_x2y2_class.f90
  gpf_data_x2y2_p_class.f90
  gpf_data_x2y2z2_class.f90
  GPF_Data_DataBlocks_Class.F90   GPF_Data_DataBlocks_SubClass.F90
  GPF_Data_Module.F90           GPF_Data_SubModule.F90
)

# include $(SPARKHOME)/Make.inc
#
# OBJ =   gpf_data_base_class.o           \
#         gpf_data_x1y1_class.o           \
#         gpf_data_x1y2_class.o           \
#         gpf_data_x2y2_class.o           \
#         gpf_data_x2y2_p_class.o         \
#         gpf_data_x2y2z2_class.o         \
#         gpf_data_module.o
#
# all:
#         @$(MAKE) .compile_current_directory
#
# clean:
#         @$(MAKE) .clean_current_directory
#
# cleanall:
#         @$(MAKE) .cleanall_current_directory
#

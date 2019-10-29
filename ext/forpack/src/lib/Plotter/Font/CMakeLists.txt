add_sources (
  GPF_Font_Class.F90      GPF_Font_SubClass.F90
  gpf_offset_class.f90
  gpf_colorspec_class.f90
  gpf_coordinates_class.f90
)

# include $(SPARKHOME)/Make.inc
#
# OBJ =   gpf_font_class.o                \
#         gpf_offset_class.o              \
#         gpf_colorspec_class.o           \
#         gpf_coordinates_class.o
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

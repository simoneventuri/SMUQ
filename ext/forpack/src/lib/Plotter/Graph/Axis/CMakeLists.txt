add_sources (
  gpf_axis_logscale_class.f90
  gpf_axis_format_class.f90
  gpf_axis_range_class.f90
  gpf_axis_label_class.f90
  gpf_axis_class.f90
  gpf_axes_class.f90
)

# include $(SPARKHOME)/Make.inc
#
# OBJ =   gpf_axis_logscale_class.o       \
#         gpf_axis_format_class.o         \
#         gpf_axis_range_class.o          \
#         gpf_axis_label_class.o          \
#         gpf_axis_class.o                \
#         gpf_axes_class.o
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

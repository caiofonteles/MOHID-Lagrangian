# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# Note that incremental build could trigger a call to cmake_copy_f90_mod on each re-build

examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o: common/CMakeFiles/FoX_common.dir/fox_common.mod.stamp
examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o: sax/CMakeFiles/FoX_sax.dir/fox_sax.mod.stamp
examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o.provides.build: examples/CMakeFiles/sax_example_2.dir/m_handlers_two.mod.stamp
examples/CMakeFiles/sax_example_2.dir/m_handlers_two.mod.stamp: examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o
	$(CMAKE_COMMAND) -E cmake_copy_f90_mod modules/m_handlers_two examples/CMakeFiles/sax_example_2.dir/m_handlers_two.mod.stamp GNU
examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o.provides.build:
	$(CMAKE_COMMAND) -E touch examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o.provides.build
examples/CMakeFiles/sax_example_2.dir/build: examples/CMakeFiles/sax_example_2.dir/sax_example_2.f90.o.provides.build

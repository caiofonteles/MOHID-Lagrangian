# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Applications/CMake.app/Contents/bin/cmake

# The command to remove a file.
RM = /Applications/CMake.app/Contents/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build

# Include any dependencies generated for this target.
include examples/CMakeFiles/wxml_example.dir/depend.make

# Include the progress variables for this target.
include examples/CMakeFiles/wxml_example.dir/progress.make

# Include the compile flags for this target's objects.
include examples/CMakeFiles/wxml_example.dir/flags.make

examples/CMakeFiles/wxml_example.dir/wxml_example.f90.o: examples/CMakeFiles/wxml_example.dir/flags.make
examples/CMakeFiles/wxml_example.dir/wxml_example.f90.o: ../examples/wxml_example.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object examples/CMakeFiles/wxml_example.dir/wxml_example.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/examples/wxml_example.f90 -o CMakeFiles/wxml_example.dir/wxml_example.f90.o

examples/CMakeFiles/wxml_example.dir/wxml_example.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/wxml_example.dir/wxml_example.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/examples/wxml_example.f90 > CMakeFiles/wxml_example.dir/wxml_example.f90.i

examples/CMakeFiles/wxml_example.dir/wxml_example.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/wxml_example.dir/wxml_example.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/examples/wxml_example.f90 -o CMakeFiles/wxml_example.dir/wxml_example.f90.s

# Object files for target wxml_example
wxml_example_OBJECTS = \
"CMakeFiles/wxml_example.dir/wxml_example.f90.o"

# External object files for target wxml_example
wxml_example_EXTERNAL_OBJECTS =

bin/wxml_example: examples/CMakeFiles/wxml_example.dir/wxml_example.f90.o
bin/wxml_example: examples/CMakeFiles/wxml_example.dir/build.make
bin/wxml_example: lib/libFoX_wxml.a
bin/wxml_example: lib/libFoX_common.a
bin/wxml_example: lib/libFoX_utils.a
bin/wxml_example: lib/libFoX_fsys.a
bin/wxml_example: examples/CMakeFiles/wxml_example.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable ../bin/wxml_example"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/wxml_example.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
examples/CMakeFiles/wxml_example.dir/build: bin/wxml_example

.PHONY : examples/CMakeFiles/wxml_example.dir/build

examples/CMakeFiles/wxml_example.dir/clean:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples && $(CMAKE_COMMAND) -P CMakeFiles/wxml_example.dir/cmake_clean.cmake
.PHONY : examples/CMakeFiles/wxml_example.dir/clean

examples/CMakeFiles/wxml_example.dir/depend:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/examples /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/examples/CMakeFiles/wxml_example.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : examples/CMakeFiles/wxml_example.dir/depend


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
include sax/CMakeFiles/FoX_sax.dir/depend.make

# Include the progress variables for this target.
include sax/CMakeFiles/FoX_sax.dir/progress.make

# Include the compile flags for this target's objects.
include sax/CMakeFiles/FoX_sax.dir/flags.make

sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.o: ../sax/FoX_sax.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/FoX_sax.f90 -o CMakeFiles/FoX_sax.dir/FoX_sax.f90.o

sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/FoX_sax.f90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/FoX_sax.f90 > CMakeFiles/FoX_sax.dir/FoX_sax.f90.i

sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/FoX_sax.f90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/FoX_sax.f90 -o CMakeFiles/FoX_sax.dir/FoX_sax.f90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o: ../sax/m_sax_operate.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_operate.F90 -o CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_operate.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_operate.F90 > CMakeFiles/FoX_sax.dir/m_sax_operate.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_operate.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_operate.F90 -o CMakeFiles/FoX_sax.dir/m_sax_operate.F90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o: ../sax/m_sax_parser.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_parser.F90 -o CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_parser.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_parser.F90 > CMakeFiles/FoX_sax.dir/m_sax_parser.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_parser.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_parser.F90 -o CMakeFiles/FoX_sax.dir/m_sax_parser.F90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o: ../sax/m_sax_reader.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_reader.F90 -o CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_reader.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_reader.F90 > CMakeFiles/FoX_sax.dir/m_sax_reader.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_reader.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_reader.F90 -o CMakeFiles/FoX_sax.dir/m_sax_reader.F90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o: ../sax/m_sax_tokenizer.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_tokenizer.F90 -o CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_tokenizer.F90 > CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_tokenizer.F90 -o CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.o: ../sax/m_sax_types.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_types.F90 -o CMakeFiles/FoX_sax.dir/m_sax_types.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_types.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_types.F90 > CMakeFiles/FoX_sax.dir/m_sax_types.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_types.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_types.F90 -o CMakeFiles/FoX_sax.dir/m_sax_types.F90.s

sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o: sax/CMakeFiles/FoX_sax.dir/flags.make
sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o: ../sax/m_sax_xml_source.F90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building Fortran object sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_xml_source.F90 -o CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o

sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.i"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_xml_source.F90 > CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.i

sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.s"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && /usr/local/bin/gfortran $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax/m_sax_xml_source.F90 -o CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.s

# Object files for target FoX_sax
FoX_sax_OBJECTS = \
"CMakeFiles/FoX_sax.dir/FoX_sax.f90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_types.F90.o" \
"CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o"

# External object files for target FoX_sax
FoX_sax_EXTERNAL_OBJECTS =

lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/FoX_sax.f90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_operate.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_parser.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_reader.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_tokenizer.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_types.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/m_sax_xml_source.F90.o
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/build.make
lib/libFoX_sax.a: sax/CMakeFiles/FoX_sax.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Linking Fortran static library ../lib/libFoX_sax.a"
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && $(CMAKE_COMMAND) -P CMakeFiles/FoX_sax.dir/cmake_clean_target.cmake
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/FoX_sax.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
sax/CMakeFiles/FoX_sax.dir/build: lib/libFoX_sax.a

.PHONY : sax/CMakeFiles/FoX_sax.dir/build

sax/CMakeFiles/FoX_sax.dir/clean:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax && $(CMAKE_COMMAND) -P CMakeFiles/FoX_sax.dir/cmake_clean.cmake
.PHONY : sax/CMakeFiles/FoX_sax.dir/clean

sax/CMakeFiles/FoX_sax.dir/depend:
	cd /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/sax /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax /Users/rbc-laptop/Documents/GitHub/MOHID-Lagrangian/Libs/fox/build/sax/CMakeFiles/FoX_sax.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : sax/CMakeFiles/FoX_sax.dir/depend


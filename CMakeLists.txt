cmake_minimum_required(VERSION 3.1.0)
project(assign2)
find_package(LLVM REQUIRED CONFIG HINTS ${LLVM_DIR} ${LLVM_DIR}/lib/cmake/llvm
	                NO_DEFAULT_PATH)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS} SYSTEM)
link_directories(${LLVM_LIBRARY_DIRS})
message(STATUS "LLVM_LIB DIR : ${LLVM_LIBRARY_DIRS}")
set(LLVM_LINK_COMPONENTS
  LLVMCore
  LLVMIRReader
  LLVMPasses
  )

set(ANALYSIS_VIEW off)
if(${ANALYSIS_INTERNAL})
add_definitions(-DANALYSIS_VIEW)
endif()

message(STATUS "LLVM LIBS : ${LLVM_LINK_COMPONENTS}")
# Support plugins.
file(GLOB SOURCE "./*.cpp") 
add_executable(assignment3 ${SOURCE}) 

target_link_libraries(assignment3
	${LLVM_LINK_COMPONENTS}
	)

  install(TARGETS assignment3 
  RUNTIME DESTINATION bin)
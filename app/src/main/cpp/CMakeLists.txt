cmake_minimum_required(VERSION 3.4.1)


set(${CMAKE_C_FLAGS}, "${CMAKE_C_FLAGS}")
add_library(native_app_glue STATIC
    ${ANDROID_NDK}/sources/android/native_app_glue/android_native_app_glue.c)


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11 -Wall -Werror")

set(CMAKE_SHARED_LINKER_FLAGS
    "${CMAKE_SHARED_LINKER_FLAGS} -u ANativeActivity_onCreate")

include_directories(shadertoy
        utils
        shadertoy)
file(GLOB src-files
        ${CMAKE_SOURCE_DIR}/shadertoy/*.cpp
        ${CMAKE_SOURCE_DIR}/utils/*.cpp)

add_library(native-activity SHARED main.cpp
        ${src-files})


target_include_directories(native-activity PRIVATE
    ${ANDROID_NDK}/sources/android/native_app_glue)

target_link_libraries(native-activity
        android
        native_app_glue
        GLESv3
        EGL
        GLESv1_CM
        log)

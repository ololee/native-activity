//
// Created by ololee on 2021/11/24.
//

#ifndef NATIVE_ACTIVITY_UTILS_H
#define NATIVE_ACTIVITY_UTILS_H

/**
    * Computes an orthographic projection matrix.
    *
    * m returns the result
    * mOffset
    * left
    * right
    * bottom
    * top
    * near
    * far
    */
void orthoM(float m[], int mOffset,
                   float left, float right, float bottom, float top,
                   float near, float far);

#endif //NATIVE_ACTIVITY_UTILS_H

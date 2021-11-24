#include <initializer_list>
#include <memory>
#include <cstdlib>
#include <cstring>
#include <jni.h>
#include <cerrno>
#include <cassert>
#include <EGL/egl.h>
#include <GLES/gl.h>
#include <GLES3/gl3.h>
#include <GLES3/gl3ext.h>
#include <android/sensor.h>
#include <android/log.h>
#include <android_native_app_glue.h>
#include "../utils/utils.h"

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "native-activity", __VA_ARGS__))
#define LOGW(...) ((void)__android_log_print(ANDROID_LOG_WARN, "native-activity", __VA_ARGS__))


struct saved_state {
    float angle;
    int32_t x;
    int32_t y;
    struct timeval startTime;
};

struct glstruct {
    GLuint program;
    GLuint vertexShader;
    GLuint fragmentShader;
};


struct engine {
    struct android_app *app;
    ASensorManager *sensorManager;
    const ASensor *accelerometerSensor;
    ASensorEventQueue *sensorEventQueue;
    int animating;
    EGLDisplay display;
    EGLSurface surface;
    EGLContext context;
    int32_t width;
    int32_t height;
    struct saved_state state;
    struct glstruct gldata;
};

float matrix[]={1.0f,1.0f,1.0f,1.0f,
                1.0f,1.0f,1.0f,1.0f,
                1.0f,1.0f,1.0f,1.0f,
                1.0f,1.0f,1.0f,1.0f};


static const GLchar *vertexShaderSource[] = {"#version 300 es \n"
                                             "layout(location = 0) in vec4 vPosition;\n"
                                             "uniform mat4 u_Matrix;"
                                             "out vec2 fragCoord;\n"
                                             "uniform vec4 touchPoint;\n"
                                             "out vec3 iMouse;\n"
                                             "out vec3 iResolution;\n"
                                             "uniform vec3 viewRange;\n"
                                             "void main()\n"
                                             "{\n"
                                             "   vec2 ratio = (vPosition.xy+vec2(1.0,1.0))*.5;\n"
                                             "   fragCoord = vec2(ratio.x*viewRange.x,ratio.y*viewRange.y);\n"
                                             "   iMouse = (touchPoint).xyz;\n"
                                             "   iResolution = viewRange;\n"
                                             "   gl_Position = vPosition ;\n"
                                             "}\0"};
static const GLchar *fragmentShaderSource[] = {"#version 300 es \n"
                                               "precision mediump float;\n"
                                               "out vec4 fragColor;\n"
                                               "uniform float iTime;\n"
                                               "in vec3 iMouse;\n"
                                               "in vec3 iResolution;\n"
                                               "in vec2 fragCoord;\n"
                                               "float sdCircle( in vec2 p, in float r ) \n"
                                               "{\n"
                                               "    return length(p)-r;\n"
                                               "}\n"
                                               "void main()\n"
                                               "{\n"
                                               "   vec2 p = (2.*fragCoord-iResolution.xy)/(iResolution.x);\n"
                                               "   vec2 m = vec2((2.*iMouse.x-iResolution.x)/(iResolution.x),(iResolution.y-2.*iMouse.y)/(iResolution.x));\n"
                                               "   float d = sdCircle(p,0.5);\n"
                                               "    // coloring\n"
                                               "    vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);\n"
                                               "    col *= 1.0 - exp(-3.0*abs(d));\n"
                                               "    col *= 0.8 + 0.2*cos(150.0*d);\n"
                                               "    col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.01,abs(d)) );\n"
                                               "    if( iMouse.z!=0.001 )\n"
                                               "    {\n"
                                               "    d = sdCircle(m,0.5);\n"
                                               "    col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, abs(length(p-m)-abs(d))-0.0025));\n"
                                               "    col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, length(p-m)-0.015));\n"
                                               "    }\n"
                                               "    fragColor = vec4(col, 1.);\n"
                                               "}\n\0"};


static void checkShaderCompile(GLuint shader) {
    GLint compiled;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
    GLint infoLen = 0;
    glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
    if (!compiled) {
        if (infoLen > 1) {
            char *infoLog = (char *) malloc(sizeof(char) * infoLen);
            glGetShaderInfoLog(shader, infoLen, NULL, infoLog);
            LOGW("Error compiling shader:[%s]", infoLog);
            free(infoLog);
        }
        glDeleteShader(shader);
    } else {
        LOGI("编译完成，没有错误");
    }
}

static int engine_init_display(struct engine *engine) {
    const EGLint attribs[] = {
            EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
            EGL_BLUE_SIZE, 8,
            EGL_GREEN_SIZE, 8,
            EGL_RED_SIZE, 8,
            EGL_NONE
    };
    EGLint w, h, format;
    EGLint numConfigs;
    EGLConfig config = nullptr;
    EGLSurface surface;
    EGLContext context;
    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
    if (!eglInitialize(display, nullptr, nullptr)) {
        LOGW("egl 初始化失败");
        return 0;
    }
    eglChooseConfig(display, attribs, nullptr, 0, &numConfigs);
    std::unique_ptr<EGLConfig[]> supportedConfigs(new EGLConfig[numConfigs]);
    assert(supportedConfigs);
    eglChooseConfig(display, attribs, supportedConfigs.get(), numConfigs, &numConfigs);
    assert(numConfigs);
    auto i = 0;
    for (; i < numConfigs; i++) {
        auto &cfg = supportedConfigs[i];
        EGLint r, g, b, d;
        if (eglGetConfigAttrib(display, cfg, EGL_RED_SIZE, &r) &&
            eglGetConfigAttrib(display, cfg, EGL_GREEN_SIZE, &g) &&
            eglGetConfigAttrib(display, cfg, EGL_BLUE_SIZE, &b) &&
            eglGetConfigAttrib(display, cfg, EGL_DEPTH_SIZE, &d) &&
            r == 8 && g == 8 && b == 8 && d == 0) {

            config = supportedConfigs[i];
            break;
        }
    }
    if (i == numConfigs) {
        config = supportedConfigs[0];
    }

    if (config == nullptr) {
        LOGW("Unable to initialize EGLConfig");
        return -1;
    }
    eglGetConfigAttrib(display, config, EGL_NATIVE_VISUAL_ID, &format);
    surface = eglCreateWindowSurface(display, config, engine->app->window, nullptr);
    EGLint contextAttribs[] =
            {
                    EGL_CONTEXT_CLIENT_VERSION,
                    3,
                    EGL_NONE
            };
    context = eglCreateContext(display, EGL_NO_CONTEXT, EGL_NO_CONTEXT, contextAttribs);
    eglMakeCurrent(display, surface, surface, context);
    if (auto error = glGetError()) {
        LOGW("=======egl创建失败或者context创建失败:%d", error);
    }
    GLuint program = glCreateProgram();

    if (program == 0) {
        LOGW("create program failed.");
    } else {
        LOGW("program 创建成功");
    }
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    if (vertexShader == 0) {
        LOGW("vertexShader create failed.");
    }
    glShaderSource(vertexShader, 1, vertexShaderSource, NULL);
    glCompileShader(vertexShader);
    checkShaderCompile(vertexShader);

    glShaderSource(fragmentShader, 1, fragmentShaderSource, NULL);
    glCompileShader(fragmentShader);
    checkShaderCompile(fragmentShader);


    glAttachShader(program, vertexShader);
    glAttachShader(program, fragmentShader);
    glLinkProgram(program);
    GLint linked;
    glGetProgramiv(program, GL_LINK_STATUS, &linked);
    if (!linked) {
        GLint infoLen = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &infoLen);
        if (infoLen > 1) {
            char *infoLog = (char *) malloc(sizeof(char) * infoLen);
            glGetProgramInfoLog(program, infoLen, NULL, infoLog);
            LOGW("Error linking program:[%s]", infoLog);
            free(infoLog);
        }
        glDeleteProgram(program);
        return -1;
    }
    engine->gldata = {
            .program=program,
            .vertexShader=vertexShader,
            .fragmentShader=fragmentShader
    };
    if (eglMakeCurrent(display, surface, surface, context) == EGL_FALSE) {
        LOGW("Unable to eglMakeCurrent");
        return -1;
    }

    eglQuerySurface(display, surface, EGL_WIDTH, &w);
    eglQuerySurface(display, surface, EGL_HEIGHT, &h);

    engine->display = display;
    engine->context = context;
    engine->surface = surface;
    engine->width = w;
    engine->height = h;
    engine->state.angle = 0;

    float aspectRatio =w>h?(float(w))/h:float (h)/w;
    if(w>h){
        orthoM(matrix,0,-aspectRatio,aspectRatio,-1.f,1.f,-1.f,1.f);
    } else{
        orthoM(matrix,0,-1.f,1.f,-aspectRatio,aspectRatio,-1.f,1.f);
    }



    // Check openGL on the system
    auto opengl_info = {GL_VENDOR, GL_RENDERER, GL_VERSION, GL_EXTENSIONS};
    for (auto name : opengl_info) {
        auto info = glGetString(name);
        LOGI("OpenGL Info: %s", info);
    }
    // Initialize GL state.
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
    glEnable(GL_CULL_FACE);
    glShadeModel(GL_SMOOTH);
    glDisable(GL_DEPTH_TEST);

    return 0;
}

#define RANGE 1.0f

static void engine_draw_frame(struct engine *engine) {
    if (engine->display == nullptr) {
        return;
    }

//    glClearColor(((float) engine->state.x) / engine->width, engine->state.angle,((float) engine->state.y) / engine->height, 1);
    glClear(GL_COLOR_BUFFER_BIT);
//    float originX = ((float) engine->state.x) * 2 / engine->width - 1;
//    float originY = -((float) engine->state.y) * 2 / engine->height + 1;
    float originX = 0.0, originY = 0.0;
    struct timeval now;
    gettimeofday(&now,NULL);
    float deltaTime = (now.tv_usec - engine->state.startTime.tv_usec)/1000000.;
//    originX=sin(deltaTime);
    GLfloat vertices[] = {originX-RANGE,originY+RANGE, 0.0f,
                          originX-RANGE, originY-RANGE, 0.0f,
                          originX+RANGE, originY-RANGE, 0.0f,
                          originX-RANGE,originY+RANGE,0.0f,
                          originX+RANGE, originY-RANGE,0.0f,
                          originX+RANGE, originY+RANGE, 0.0f
                          };
    GLfloat resolution[] = {engine->width * 1.0f, engine->height * 1.0f, 0.f};
    GLfloat touchPoints[] ={engine->state.x*1.f,1.f*engine->state.y,1.0f,0.0f};
    glViewport(0, 0, engine->width, engine->height);
    glUseProgram(engine->gldata.program);
    glUniformMatrix4fv(glGetUniformLocation(engine->gldata.program,"u_Matrix"),1,GL_FALSE,matrix);
    glUniform3fv(glGetUniformLocation(engine->gldata.program, "viewRange"), 1, resolution);
    glUniform4fv(glGetUniformLocation(engine->gldata.program, "touchPoint"), 1, touchPoints);
    glUniform1f(glGetUniformLocation(engine->gldata.program, "iTime"), deltaTime);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, vertices);
    glEnableVertexAttribArray(0);
    glDrawArrays(GL_TRIANGLES,0,3);
    glDrawArrays(GL_TRIANGLES,3,3);

    eglSwapBuffers(engine->display, engine->surface);
}


static void engine_term_display(struct engine *engine) {
    if (engine->display != EGL_NO_DISPLAY) {
        eglMakeCurrent(engine->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
        if (engine->context != EGL_NO_CONTEXT) {
            eglDestroyContext(engine->display, engine->context);
        }
        if (engine->surface != EGL_NO_SURFACE) {
            eglDestroySurface(engine->display, engine->surface);
        }
        eglTerminate(engine->display);
    }
    engine->animating = 0;
    engine->display = EGL_NO_DISPLAY;
    engine->context = EGL_NO_CONTEXT;
    engine->surface = EGL_NO_SURFACE;
}

/**
 * Process the next input event.
 */
static int32_t engine_handle_input(struct android_app *app, AInputEvent *event) {
    auto *engine = (struct engine *) app->userData;
    if (AInputEvent_getType(event) == AINPUT_EVENT_TYPE_MOTION) {
        engine->animating = 1;
        engine->state.x = AMotionEvent_getX(event, 0);
        engine->state.y = AMotionEvent_getY(event, 0);
        return 1;
    }
    return 0;
}

/**
 * Process the next main command.
 */
static void engine_handle_cmd(struct android_app *app, int32_t cmd) {
    auto *engine = (struct engine *) app->userData;
    switch (cmd) {
        case APP_CMD_SAVE_STATE:
            engine->app->savedState = malloc(sizeof(struct saved_state));
            *((struct saved_state *) engine->app->savedState) = engine->state;
            engine->app->savedStateSize = sizeof(struct saved_state);
            break;
        case APP_CMD_INIT_WINDOW:
            if (engine->app->window != nullptr) {
                engine_init_display(engine);
                engine_draw_frame(engine);
            }
            break;
        case APP_CMD_TERM_WINDOW:
            engine_term_display(engine);
            break;
        case APP_CMD_GAINED_FOCUS:
            if (engine->accelerometerSensor != nullptr) {
                ASensorEventQueue_enableSensor(engine->sensorEventQueue,
                                               engine->accelerometerSensor);
                ASensorEventQueue_setEventRate(engine->sensorEventQueue,
                                               engine->accelerometerSensor,
                                               (1000L / 60) * 1000);
            }
            break;
        case APP_CMD_LOST_FOCUS:
            if (engine->accelerometerSensor != nullptr) {
                ASensorEventQueue_disableSensor(engine->sensorEventQueue,
                                                engine->accelerometerSensor);
            }
            // Also stop animating.
            engine->animating = 0;
            engine_draw_frame(engine);
            break;
        default:
            break;
    }
}

/*
 * AcquireASensorManagerInstance(void)
 *    Workaround ASensorManager_getInstance() deprecation false alarm
 *    for Android-N and before, when compiling with NDK-r15
 */
#include <dlfcn.h>

ASensorManager *AcquireASensorManagerInstance(android_app *app) {

    if (!app)
        return nullptr;

    typedef ASensorManager *(*PF_GETINSTANCEFORPACKAGE)(const char *name);
    void *androidHandle = dlopen("libandroid.so", RTLD_NOW);
    auto getInstanceForPackageFunc = (PF_GETINSTANCEFORPACKAGE)
            dlsym(androidHandle, "ASensorManager_getInstanceForPackage");
    if (getInstanceForPackageFunc) {
        JNIEnv *env = nullptr;
        app->activity->vm->AttachCurrentThread(&env, nullptr);

        jclass android_content_Context = env->GetObjectClass(app->activity->clazz);
        jmethodID midGetPackageName = env->GetMethodID(android_content_Context,
                                                       "getPackageName",
                                                       "()Ljava/lang/String;");
        auto packageName = (jstring) env->CallObjectMethod(app->activity->clazz,
                                                           midGetPackageName);

        const char *nativePackageName = env->GetStringUTFChars(packageName, nullptr);
        ASensorManager *mgr = getInstanceForPackageFunc(nativePackageName);
        env->ReleaseStringUTFChars(packageName, nativePackageName);
        app->activity->vm->DetachCurrentThread();
        if (mgr) {
            dlclose(androidHandle);
            return mgr;
        }
    }

    typedef ASensorManager *(*PF_GETINSTANCE)();
    auto getInstanceFunc = (PF_GETINSTANCE)
            dlsym(androidHandle, "ASensorManager_getInstance");
    // by all means at this point, ASensorManager_getInstance should be available
    assert(getInstanceFunc);
    dlclose(androidHandle);

    return getInstanceFunc();
}




/**
 * This is the main entry point of a native application that is using
 * android_native_app_glue.  It runs in its own thread, with its own
 * event loop for receiving input events and doing other things.
 */
void android_main(struct android_app *state) {
    struct engine engine{};

    memset(&engine, 0, sizeof(engine));
    state->userData = &engine;
    state->onAppCmd = engine_handle_cmd;
    state->onInputEvent = engine_handle_input;
    engine.app = state;


    // Prepare to monitor accelerometer
    engine.sensorManager = AcquireASensorManagerInstance(state);
    engine.accelerometerSensor = ASensorManager_getDefaultSensor(
            engine.sensorManager,
            ASENSOR_TYPE_ACCELEROMETER);
    engine.sensorEventQueue = ASensorManager_createEventQueue(
            engine.sensorManager,
            state->looper, LOOPER_ID_USER,
            nullptr, nullptr);

    if (state->savedState != nullptr) {
        // We are starting with a previous saved state; restore from it.
        engine.state = *(struct saved_state *) state->savedState;
    }

    gettimeofday(&engine.state.startTime, NULL);

    // loop waiting for stuff to do.

    while (true) {
        // Read all pending events.
        int ident;
        int events;
        struct android_poll_source *source;

        // If not animating, we will block forever waiting for events.
        // If animating, we loop until all events are read, then continue
        // to draw the next frame of animation.
        while ((ident = ALooper_pollAll(engine.animating ? 0 : -1, nullptr, &events,
                                        (void **) &source)) >= 0) {

            // Process this event.
            if (source != nullptr) {
                source->process(state, source);
            }

            // If a sensor has data, process it now.
            if (ident == LOOPER_ID_USER) {
                if (engine.accelerometerSensor != nullptr) {
                    ASensorEvent event;
//                    while ( > 0) {
////                        LOGI("accelerometer: x=%f y=%f z=%f",
////                             event.acceleration.x, event.acceleration.y,
////                             event.acceleration.z);
//                    }
                    ASensorEventQueue_getEvents(engine.sensorEventQueue,
                                                &event, 1);
                }
            }

            // Check if we are exiting.
            if (state->destroyRequested != 0) {
                engine_term_display(&engine);
                return;
            }
        }

        if (engine.animating) {
            engine.state.angle += .01f;
            if (engine.state.angle > 1) {
                engine.state.angle = 0;
            }

        }

        engine_draw_frame(&engine);
    }


}
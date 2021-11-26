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
#include <time.h>
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
                                             "uniform mat4 u_Matrix;\n"
                                             "uniform float u_time;\n"
                                             "out vec2 fragCoord;\n"
                                             "uniform vec4 touchPoint;\n"
                                             "out vec3 iMouse;\n"
                                             "out vec3 iResolution;\n"
                                             "out float iTime;\n"
                                             "uniform vec3 viewRange;\n"
                                             "void main()\n"
                                             "{\n"
                                             "   vec2 ratio = (vPosition.xy+vec2(1.0,1.0))*.5;\n"
                                             "   fragCoord = vec2(ratio.x*viewRange.x,ratio.y*viewRange.y);\n"
                                             "   iMouse = (touchPoint).xyz;\n"
                                             "   iResolution = viewRange;\n"
                                             "   iTime = u_time ;\n"
                                             "   gl_Position = vPosition ;\n"
                                             "}\0"};
/*
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
                                               "   // col *= 1.0 - exp(-3.0*abs(d));\n"
                                               "    //col *= 0.8 + 0.2*cos(150.0*d);\n"
                                               "    //col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.01,abs(d)) );\n"
                                               "    if( iMouse.z!=0.001 )\n"
                                               "    {\n"
                                               "    d = sdCircle(m,0.5);\n"
                                               "    col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, abs(length(p-m)-abs(d))-0.0025));\n"
                                               "    col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, length(p-m)-0.015));\n"
                                               "    }\n"
                                               "    fragColor = vec4(col, 1.);\n"
                                               "}\n\0"};
*/
static const GLchar *fragmentShaderSource[] = {"#version 300 es \n"
                                               "precision mediump float;\n"
                                               "out vec4 fragColor;\n"
                                               "in float iTime;\n"
                                               "in vec3 iMouse;\n"
                                               "in vec3 iResolution;\n"
                                               "in vec2 fragCoord;\n"
                                               "float sdRoundBox( in vec2 p, in vec2 b, in vec4 r ) \n"
                                               "{\n"
                                               "    r.xy = (p.x>0.0)?r.xy : r.zw;\n"
                                               "    r.x  = (p.y>0.0)?r.x  : r.y;\n"
                                               "    vec2 q = abs(p)-b+r.x;\n"
                                               "    return min(max(q.x,q.y),0.0) + length(max(q,0.0)) - r.x;\n"
                                               "}\n"
                                               "\n"
                                               "void main()\n"
                                               "{\n"
                                               "    vec2 p = (2.*fragCoord-iResolution.xy)/(iResolution.x);\n"
                                               "    vec2 m = vec2((2.*iMouse.x-iResolution.x)/(iResolution.x),(iResolution.y-2.*iMouse.y)/(iResolution.x));\n"
                                               "\n"
                                               "    vec2 si = vec2(0.9,0.6) + 0.3*cos(iTime+vec2(0,2));\n"
                                               "    vec4 ra = 0.3 + 0.3*cos( 2.0*iTime + vec4(0,1,2,3) );\n"
                                               "    ra = min(ra,min(si.x,si.y));\n"
                                               "\n"
                                               "    float d = sdRoundBox( p, si, ra );\n"
                                               "\n"
                                               "    vec3 col = vec3(1.0) - sign(d)*vec3(0.1,0.4,0.7);\n"
                                               "    col *= 1.0 - exp(-3.0*abs(d));\n"
                                               "    col *= 0.8 + 0.2*cos(150.0*d);\n"
                                               "    col = mix( col, vec3(1.0), 1.0-smoothstep(0.0,0.01,abs(d)) );\n"
                                               "\n"
                                               "    if( iMouse.z>0.001 )\n"
                                               "    {\n"
                                               "       d = sdRoundBox(m, si, ra );\n"
                                               "       col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, abs(length(p-m)-abs(d))-0.0025));\n"
                                               "       col = mix(col, vec3(1.0,1.0,0.0), 1.0-smoothstep(0.0, 0.005, length(p-m)-0.015));\n"
                                               "    }\n"
                                               "\n"
                                               "  fragColor = vec4(col,1.0);\n"
                                               "}\n\0"};

/*static const GLchar *fragmentShaderSource[]={
       "#version 300 es\n"
       "#define GLOW\n"
       "#define OUTER_FACE_DECO\n"
       "#define OFFSET_TRIS\n"
       "#define ANIMATE\n"
       "#define COLOR 0\n"
       "precision mediump float;\n"
       "out vec4 fragColor;\n"
       "in float iTime;\n"
       "in vec3 iMouse;\n"
       "in vec3 iResolution;\n"
       "in vec2 fragCoord;\n"
       "\n"
       "mat2 rot2(in float a){ float c = cos(a), s = sin(a); return mat2(c, -s, s, c); }\n"
       "float hash21(vec2 p){  return fract(sin(dot(p, vec2(27.619, 57.583)))*43758.5453); }\n"
       "vec2 hash22B(vec2 p) {\n"
       "    float n = sin(dot(p, vec2(1, 113)));\n"
       "    p = fract(vec2(262144, 32768)*n)*2. - 1.;\n"
       "    #ifdef ANIMATE\n"
       "    return sin(p*6.2831853 + iTime/2.);\n"
       "    #else\n"
       "    return p;\n"
       "    #endif\n"
       "}\n"
       "\n"
       "vec2 hash22C(vec2 p) {\n"
       "    float n = sin(dot(p, vec2(289, 41)));\n"
       "    return fract(vec2(262144, 32768)*n)*2. - 1.;\n"
       "}\n"
       "\n"
       "float n2D3G( in vec2 p ){\n"
       "    vec2 i = floor(p); p -= i;\n"
       "    vec4 v;\n"
       "    v.x = dot(hash22C(i), p);\n"
       "    v.y = dot(hash22C(i + vec2(1, 0)), p - vec2(1, 0));\n"
       "    v.z = dot(hash22C(i + vec2(0, 1)), p - vec2(0, 1));\n"
       "    v.w = dot(hash22C(i + 1.), p - 1.);\n"
       "    p = p*p*(3. - 2.*p);\n"
       "    return mix(mix(v.x, v.y, p.x), mix(v.z, v.w, p.x), p.y);\n"
       "}\n"
       "\n"
       "float fBm(vec2 p){ return n2D3G(p)*.57 + n2D3G(p*2.)*.28 + n2D3G(p*4.)*.15; }\n"
       "\n"
       "float sdTri(in vec2 p, in vec2 p0, in vec2 p1, in vec2 p2){\n"
       "    vec2 e0 = p1 - p0, e1 = p2 - p1, e2 = p0 - p2;\n"
       "        vec2 v0 = p - p0, v1 = p - p1, v2 = p - p2;\n"
       "        vec2 pq0 = v0 - e0*clamp( dot(v0, e0)/dot(e0, e0), 0., 1.);\n"
       "        vec2 pq1 = v1 - e1*clamp( dot(v1, e1)/dot(e1, e1), 0., 1.);\n"
       "        vec2 pq2 = v2 - e2*clamp( dot(v2, e2)/dot(e2, e2), 0., 1.);\n"
       "    float s = sign( e0.x*e2.y - e0.y*e2.x);\n"
       "    vec2 d = min( min( vec2(dot(pq0, pq0), s*(v0.x*e0.y - v0.y*e0.x)),\n"
       "                       vec2(dot(pq1, pq1), s*(v1.x*e1.y - v1.y*e1.x))),\n"
       "                       vec2(dot(pq2, pq2), s*(v2.x*e2.y - v2.y*e2.x)));\n"
       "        return -sqrt(d.x)*sign(d.y);\n"
       "}\n"
       "\n"
       "vec3 inCentRad(vec2 p0, vec2 p1, vec2 p2){\n"
       "    float bc = length(p1 - p2), ac = length(p0 - p2), ab = length(p0 - p1);\n"
       "    vec2 inCir = (bc*p0 + ac*p1 + ab*p2)/(bc + ac + ab);\n"
       "    float p = (bc + ac + ab)/2.;\n"
       "    float area = sqrt(p*(p - bc)*(p - ac)*(p - ab));\n"
       "    return vec3(inCir, area/p);\n"
       "}\n"
       "\n"
       "vec2 skewXY(vec2 p, vec2 s){ return mat2(1, -s.yx, 1)*p; }\n"
       "\n"
       "vec2 unskewXY(vec2 p, vec2 s){ return inverse(mat2(1, -s.yx, 1))*p; }\n"
       "\n"
       "struct triS{\n"
       "    vec2[3] v; // Outer vertices.\n"
       "    vec2 p; // Local coordinate.\n"
       "    vec2 id; // Position based ID.\n"
       "    float dist; // Distance field value.\n"
       "    float triID; // Triangle ID.\n"
       "};\n"
       "\n"
       "const float tf = 2./sqrt(3.);\n"
       "const vec2 scale = vec2(tf, 1)*vec2(1./3.);\n"
       "\n"
       "const vec2 dim = vec2(scale);\n"
       "const vec2 s = dim*2.;\n"
       "\n"
       "const vec2 sk = vec2(tf/2., 0);\n"
       "\n"
       "triS blocks(vec2 q){\n"
       "    float d = 1e5;\n"
       "    vec2 p, ip;\n"
       "    vec2 id = vec2(0), cntr;\n"
       "    const vec2[4] ps4 = vec2[4](vec2(-.5, .5), vec2(.5), vec2(.5, -.5), vec2(-.5));\n"
       "    float triID = 0.; // Triangle ID. Not used in this example, but helpful.\n"
       "    const float hs = .5;\n"
       "    triS gT, tri1, tri2;\n"
       "    for(int i = min(0, 0); i<4; i++){\n"
       "        cntr = ps4[i]/2.;// -  ps4[0];\n"
       "        p = skewXY(q.xy, sk);// - cntr*s;\n"
       "        ip = floor(p/s - cntr) + .5; // Local tile ID.\n"
       "        p -= (ip + cntr)*s; // New local position.\n"
       "        p = unskewXY(p, sk);\n"
       "        vec2 idi = ip + cntr;\n"
       "        vec2[4] vert = ps4;\n"
       "\n"
       "        #ifdef OFFSET_TRIS\n"
       "        vert[0] += hash22B((idi + vert[0]/2.))*.2;\n"
       "                vert[1] += hash22B((idi + vert[1]/2.))*.2;\n"
       "        vert[2] += hash22B((idi + vert[2]/2.))*.2;\n"
       "        vert[3] += hash22B((idi + vert[3]/2.))*.2;\n"
       "        #endif\n"
       "\n"
       "                vert[0] = unskewXY(vert[0]*dim, sk);\n"
       "        vert[1] = unskewXY(vert[1]*dim, sk);\n"
       "        vert[2] = unskewXY(vert[2]*dim, sk);\n"
       "        vert[3] = unskewXY(vert[3]*dim, sk);\n"
       "                idi = unskewXY(idi*s, sk);\n"
       "        tri1.v = vec2[3](vert[0], vert[1], vert[2]);\n"
       "        tri1.id = idi + inCentRad(tri1.v[0], tri1.v[1], tri1.v[2]).xy; // Position Id.\n"
       "        tri1.triID = float(i); // Triangle ID. Not used here.\n"
       "        tri1.dist = sdTri(p, tri1.v[0], tri1.v[1], tri1.v[2]); // Field distance.\n"
       "        tri1.p = p; // 2D coordinates.\n"
       "\n"
       "        tri2.v = vec2[3](vert[0], vert[2], vert[3]);\n"
       "        tri2.id = idi + inCentRad(tri2.v[0], tri2.v[1], tri2.v[2]).xy; // Position Id.\n"
       "        tri1.triID = float(i + 4); // Triangle ID. Not used here.\n"
       "        tri2.dist = sdTri(p, tri2.v[0], tri2.v[1], tri2.v[2]); // Field distance.\n"
       "        tri2.p = p; // 2D coordinates.\n"
       "\n"
       "        triS gTi;\n"
       "        if(tri1.dist<tri2.dist) gTi = tri1;\n"
       "        else gTi = tri2;\n"
       "\n"
       "        if(gTi.dist<d){\n"
       "            d = gTi.dist;\n"
       "            gT = gTi;\n"
       "            gT.id = idi;//(idi + inCentRad(gT.v[0], gT.v[1], gT.v[2]).xy)*s;\n"
       "        }\n"
       "    }\n"
       "    return gT;\n"
       "}\n"
       "\n"
       "\n"
       "\n"
       "void main(){\n"
       "    float iRes = iResolution.y;\n"
       "    vec2 uv = (fragCoord - iResolution.xy*.5)/iRes;\n"
       "\n"
       "    uv.xy *= rot2(-3.14159/9.);\n"
       "\n"
       "    vec3 rd = normalize(vec3(uv, 1));\n"
       "    vec3 ro = vec3(0);\n"
       "    const float gSc = 1.;\n"
       "    vec2 p = uv*gSc - vec2(0, iTime/32.).yx;\n"
       "\n"
       "    float sf = gSc/iResolution.y;\n"
       "    triS gT = blocks(p);\n"
       "\n"
       "    vec2[3] v = gT.v;\n"
       "    vec2 rp = gT.p;\n"
       "    vec2 svID = gT.id;\n"
       "    vec3 col = vec3(0);\n"
       "    vec3 tCol = vec3(0);\n"
       "    vec3 cntr = inCentRad(v[0], v[1], v[2]);\n"
       "    vec3 lp = vec3(-.5, 1, -1);\n"
       "    vec3 ld = lp - vec3(uv, 0);\n"
       "    float lDist = length(ld);\n"
       "    ld /= lDist;\n"
       "    float atten = 2./(1. + lDist*lDist*.5);\n"
       "    float triCell = sdTri(rp, v[0], v[1], v[2]);\n"
       "    float cir = length(rp - cntr.xy);\n"
       "    float opening = triCell;\n"
       "    vec3 glCol = vec3(1, .35, .2);\n"
       "    #if COLOR == 1\n"
       "    glCol = mix(glCol.zyx, glCol.zxy, clamp(-uv.y*1.25 + .5, 0., 1.));\n"
       "    #endif\n"
       "    glCol = mix(glCol, glCol.xzy, dot(sin(p*2. - cos(p.yx*4.)*3.), vec2(.125)) + .25);\n"
       "    glCol *= (fBm((rp -svID*s)*128.)*.25 + .75)*1.25; // Adding noise.\n"
       "    col += glCol*max(.2 - triCell/scale.x*6., 0.);\n"
       "    float edge = 1e5, openMax = 0.;\n"
       "    for(int j = min(0, 0); j<3; j++){\n"
       "        float rnd = hash21(svID + .08);\n"
       "        float rndJ = hash21(svID + float(j)/9. + .13);\n"
       "        float open = smoothstep(.9, .966, sin(rnd*6.2831 + rndJ/6. + iTime*1.)*.5 + .5);\n"
       "        #ifndef GLOW\n"
       "        open = 0.;\n"
       "        #endif\n"
       "        vec3 p0 = vec3(v[j], 0);\n"
       "        vec3 p1 = vec3(v[(j + 1)%3], 0);\n"
       "        vec3 p2 = vec3(cntr.xy, -.2);\n"
       "        p2.xy -= normalize(vec3(v[(j + 2)%3], 0) - p2).xy*.065*scale.x*open;\n"
       "        float triJ = sdTri(rp, v[j], v[(j + 1)%3], p2.xy);\n"
       "        float z = 1./(1. - p2.z);\n"
       "        p2.xy += normalize(v[(j + 2)%3] - cntr.xy)*.008*open;\n"
       "        float triEdge = sdTri(rp, v[j], v[(j + 1)%3], p2.xy);\n"
       "        vec3 nJ = normalize(cross(p1 - p0, p2 - p0));\n"
       "        float diff = max(dot(ld, nJ), 0.);\n"
       "        diff = pow(diff, 4.)*2.;\n"
       "        // Specular lighting.\n"
       "        float spec = pow(max(dot(reflect(ld, nJ), rd ), 0.), 8.);\n"
       "\n"
       "        vec3 tCol = vec3(.035);\n"
       "\n"
       "\n"
       "        // Applying the diffuse and specular to the triangle.\n"
       "        tCol = tCol*(diff + .25 + spec*4.);\n"
       "\n"
       "        if(open>1e-5){\n"
       "            col = mix(col, vec3(0), open*(1. - smoothstep(0., sf*4., triEdge - .00)));\n"
       "            col = mix(col, mix(tCol*vec3(1.5, 1.25, 1), vec3(0), open), (1. - smoothstep(0., sf*2., triEdge)));// + .005\n"
       "            col = mix(col, mix(tCol, glCol, .5)*(open), (1. - smoothstep(0., sf, triEdge + .005)));// + .005/3.\n"
       "        }\n"
       "        //else col = mix(col, vec3(0), open*(1. - smoothstep(0., sf*4., triPat[j] - .00)));// + .005/3.\n"
       "\n"
       "        col = mix(col, tCol*vec3(1.5, 1.25, 1)*(1. - open), 1. - smoothstep(0., sf*2., triJ));// + .005/3.\n"
       "        col = mix(col, tCol, 1. - smoothstep(0., sf*2., triJ + .005));// .005*2./3.\n"
       "\n"
       "        #ifdef OUTER_FACE_DECO\n"
       "        // Outer face decoration.\n"
       "        col = mix(col, mix(tCol, glCol*1., open), 1. - smoothstep(0., sf*4., abs(triJ + .035) - .002));// .005*2./3.\n"
       "        col = mix(col, mix(tCol, tCol/3., open), 1. - smoothstep(0., sf, abs(abs(triJ + .035) - .006) - .00125));// .005\n"
       "        #endif\n"
       "\n"
       "        #ifdef FACE_DECO\n"
       "        // Face decoration.\n"
       "        col = mix(col, mix(tCol, (diff + .25 + spec*4.)*glCol, open), (1. - smoothstep(0., sf*3., triJ + .035))*.2);\n"
       "        col = mix(col, mix(tCol, vec3(0), open), 1. - smoothstep(0., sf*2., triJ + .035));\n"
       "        col = mix(col, mix(tCol, (diff + .25 + spec*4.)*glCol*2., open), 1. - smoothstep(0., sf, triJ + .035 + .005));\n"
       "\n"
       "        #endif\n"
       "\n"
       "        edge = min(edge, abs(triJ));\n"
       "        p0.xy -= (p2.xy - p0.xy)*1.;\n"
       "        p1.xy -= (p2.xy - p1.xy)*1.;\n"
       "        float eTri = sdTri(rp, p0.xy*8., p1.xy*8., p2.xy);\n"
       "        openMax = max(openMax, open);\n"
       "        opening = max(opening, -eTri);\n"
       "\n"
       "    }\n"
       "\n"
       "    cir = mix(cir, opening, .65);\n"
       "    col = mix(col, col + col*glCol*(openMax*2.5 + .5), (1. - smoothstep(0., sf*24., cir)));// + .005/3.\n"
       "    col *= clamp(.5 - triCell/scale.x*4., 0., 1.);\n"
       "\n"
       "    float ns = fBm((rp - cntr.xy)*128.)*.5 + .5;\n"
       "    col *= .5 + ns*.75;\n"
       "\n"
       "    col *= atten;\n"
       "\n"
       "        fragColor = vec4(sqrt(max(col, 0.)), 1);\n"
       "}\n\0"
};*/

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
//    time_t timer = time(NULL);
//    struct tm *localtm = localtime(&timer);

//    float deltaTime = (now.tv_sec - engine->state.startTime.tv_sec);
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
    glUniform1f(glGetUniformLocation(engine->gldata.program, "u_time"), (now.tv_usec-engine->state.startTime.tv_usec)*.000001f);
    LOGI("now.tv_sec:%f",now.tv_usec*1.f);


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
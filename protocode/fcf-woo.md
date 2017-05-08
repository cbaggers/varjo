first class functions are even useful sometimes!

Here we call `perlin-noise` passing in the hashing function we would like it to use

```
(defun-g ctoy-quad-frag ((uv :vec2)
                         &uniform (now :float)
                         (mouse :vec2) (mouse-norm :vec2) (mouse-buttons :vec2)
                         (screen-res :vec2))
  (v3! (+ 0.4 (* (perlin-noise #'sgim-qpp-hash-2-per-corner
                               (* uv 5))
                 0.5))))
```

Here is the generated glsl. the lisp for gpu perlin noise and the polynomial permutation hash are below the glsl.

```
// vertex-stage
#version 450

uniform float NOW;
uniform vec2 MOUSE;
uniform vec2 MOUSE_NORM;
uniform vec2 MOUSE_BUTTONS;
uniform vec2 SCREEN_RES;

vec3 CTOY_QUAD_FRAG(vec2 UV);
float PERLIN_NOISE(vec2 P);
vec2 PERLIN_QUINTIC(vec2 X2);
vec4 SGIM_QPP_HASH_2_PER_CORNER(vec2 GRID_CELL, out vec4 return_1);
vec4 QPP_RESOLVE(vec4 X1);
vec4 QPP_PERMUTE(vec4 X0);
vec4 QPP_COORD_PREPARE(vec4 X);

vec4 QPP_COORD_PREPARE(vec4 X)
{
    return (X - (floor((X * (1.0f / 289.0f))) * 289.0f));
}

vec4 QPP_PERMUTE(vec4 X0)
{
    return (fract((X0 * (((34.0f / 289.0f) * X0) + vec4((1.0f / 289.0f))))) * 289.0f);
}

vec4 QPP_RESOLVE(vec4 X1)
{
    return fract((X1 * (7.0f / 288.0f)));
}

vec4 SGIM_QPP_HASH_2_PER_CORNER(vec2 GRID_CELL, out vec4 return_1)
{
    vec4 HASH_0;
    vec4 HASH_1;
    vec4 HASH_COORD = QPP_COORD_PREPARE(vec4(GRID_CELL.xy,(GRID_CELL.xy + vec2(1.0f))));
    HASH_0 = QPP_PERMUTE((QPP_PERMUTE(HASH_COORD.xzxz) + HASH_COORD.yyww));
    HASH_1 = QPP_RESOLVE(QPP_PERMUTE(HASH_0));
    HASH_0 = QPP_RESOLVE(HASH_0);
    vec4 g_G5388 = HASH_0;
    return_1 = HASH_1;
    return g_G5388;
}

vec2 PERLIN_QUINTIC(vec2 X2)
{
    return (X2 * (X2 * (X2 * ((X2 * ((X2 * 6.0f) - vec2(15.0f))) + vec2(10.0f)))));
}

float PERLIN_NOISE(vec2 P)
{
    vec2 PI = floor(P);
    vec4 PF_PFMIN1 = (P.xyxy - vec4(PI,(PI + vec2(1.0f))));
    vec4 MVB_0;
    vec4 MVB_1;
    MVB_0 = SGIM_QPP_HASH_2_PER_CORNER(PI,MVB_1);
    vec4 GRAD_X = (MVB_0 - vec4(0.49999));
    vec4 GRAD_Y = (MVB_1 - vec4(0.49999));
    vec4 GRAD_RESULTS = (inversesqrt(((GRAD_X * GRAD_X) + (GRAD_Y * GRAD_Y))) * ((GRAD_X * PF_PFMIN1.xzxz) + (GRAD_Y * PF_PFMIN1.yyww)));
    GRAD_RESULTS *= vec4(1.4142135623730950488016887242097);
    vec2 BLEND = PERLIN_QUINTIC(PF_PFMIN1.xy);
    vec4 BLEND2 = vec4(BLEND,(vec2(1.0f) - BLEND));
    return dot(GRAD_RESULTS,(BLEND2.zxzx * BLEND2.wwyy));
}

vec3 CTOY_QUAD_FRAG(vec2 UV)
{
    vec4(MOUSE_NORM.x,MOUSE_BUTTONS.x,MOUSE_NORM.y,float(1));
    return vec3((0.4f + (PERLIN_NOISE((UV * float(5))) * 0.5f)));
}

void main()
{
    CTOY_QUAD_FRAG(<dummy v-vec2>);
    gl_Position = vec4(float(1),float(2),float(3),float(4));
    return;
}
```

For completeness here are a couple of the other functions involved

```
(defun-g perlin-noise ((hash-func (function (:vec2) (:vec4 :vec4)))
                       (p :vec2))
  ;; looks much better than revised noise in 2D, and with an efficent hash
  ;; function runs at about the same speed.
  ;;
  ;; requires 2 random numbers per point.
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
    (multiple-value-bind (hash-x hash-y) (funcall hash-func pi)
      (let* ((grad-x (- hash-x (v4! "0.49999")))
             (grad-y (- hash-y (v4! "0.49999")))
             (grad-results
              (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                 (+ (* grad-x (s~ pf-pfmin1 :xzxz))
                    (* grad-y (s~ pf-pfmin1 :yyww))))))
        (multf grad-results (v4! "1.4142135623730950488016887242097"))
        (let* ((blend (perlin-quintic (s~ pf-pfmin1 :xy)))
               (blend2 (v! blend (- (v2! 1.0) blend))))
          (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))))
```

```
(defun-g sgim-qpp-hash-2-per-corner ((grid-cell :vec2))
  (let (((hash-0 :vec4)) ((hash-1 :vec4)))
    (let* ((hash-coord
            (qpp-coord-prepare
             (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0))))))
      (setf hash-0
            (qpp-permute
             (+ (qpp-permute (s~ hash-coord :xzxz)) (s~ hash-coord :yyww))))
      (setf hash-1 (qpp-resolve (qpp-permute hash-0)))
      (setf hash-0 (qpp-resolve hash-0)))
    (values hash-0 hash-1)))
```

All credit of implementation goes to Brian Sharpe for this excellent noise library

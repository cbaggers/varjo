;; http://prideout.net/blog/?p=48  -  SUCH a good guide
;; We will recreate his example in varjo and then, when it is possible, move it
;; to CEPL

;; -- Vertex
;;
;; in vec4 Position;
;; out vec3 vPosition;
;;
;; void main()
;; {
;;     vPosition = Position.xyz;
;; }

;; -- TessControl
;;
;; in vec3 vPosition[];
;;
;; layout(vertices = 3) out;
;; out vec3 tcPosition[];
;;
;; uniform float TessLevelInner;
;; uniform float TessLevelOuter;
;;
;; void main()
;; {
;;     tcPosition[gl_InvocationID] = vPosition[gl_InvocationID];
;;     if (gl_InvocationID == 0) {
;;         gl_TessLevelInner[0] = TessLevelInner;
;;         gl_TessLevelOuter[0] = TessLevelOuter;
;;         gl_TessLevelOuter[1] = TessLevelOuter;
;;         gl_TessLevelOuter[2] = TessLevelOuter;
;;     }
;; }

;; -- TessEval
;;
;; layout(triangles, equal_spacing, cw) in;
;; in vec3 tcPosition[];
;; out vec3 tePosition;
;; out vec3 tePatchDistance;
;; uniform mat4 Projection;
;; uniform mat4 Modelview;
;;
;; void main()
;; {
;;     vec3 p0 = gl_TessCoord.x * tcPosition[0];
;;     vec3 p1 = gl_TessCoord.y * tcPosition[1];
;;     vec3 p2 = gl_TessCoord.z * tcPosition[2];
;;     tePatchDistance = gl_TessCoord;
;;     tePosition = normalize(p0 + p1 + p2);
;;     gl_Position = Projection * Modelview * vec4(tePosition, 1);
;; }

;; -- Geometry

;; uniform mat4 Modelview;
;; uniform mat3 NormalMatrix;
;; layout(triangles) in;
;; layout(triangle_strip, max_vertices = 3) out;
;; in vec3 tePosition[3];
;; in vec3 tePatchDistance[3];
;; out vec3 gFacetNormal;
;; out vec3 gPatchDistance;
;; out vec3 gTriDistance;
;;
;; void main()
;; {
;;     vec3 A = tePosition[2] - tePosition[0];
;;     vec3 B = tePosition[1] - tePosition[0];
;;     gFacetNormal = NormalMatrix * normalize(cross(A, B));
;;
;;     gPatchDistance = tePatchDistance[0];
;;     gTriDistance = vec3(1, 0, 0);
;;     gl_Position = gl_in[0].gl_Position; EmitVertex();
;;
;;     gPatchDistance = tePatchDistance[1];
;;     gTriDistance = vec3(0, 1, 0);
;;     gl_Position = gl_in[1].gl_Position; EmitVertex();
;;
;;     gPatchDistance = tePatchDistance[2];
;;     gTriDistance = vec3(0, 0, 1);
;;     gl_Position = gl_in[2].gl_Position; EmitVertex();
;;
;;     EndPrimitive();
;; }


;; out vec4 FragColor;
;; in vec3 gFacetNormal;
;; in vec3 gTriDistance;
;; in vec3 gPatchDistance;
;; uniform vec3 LightPosition;
;; uniform vec3 DiffuseMaterial;
;; uniform vec3 AmbientMaterial;
;;
;; float amplify(float d, float scale, float offset)
;; {
;;     d = scale * d + offset;
;;     d = clamp(d, 0, 1);
;;     d = 1 - exp2(-2*d*d);
;;     return d;
;; }
;;
;; void main()
;; {
;;     vec3 N = normalize(gFacetNormal);
;;     vec3 L = LightPosition;
;;     float df = abs(dot(N, L));
;;     vec3 color = AmbientMaterial + df * DiffuseMaterial;
;;
;;     float d1 = min(min(gTriDistance.x, gTriDistance.y), gTriDistance.z);
;;     float d2 = min(min(gPatchDistance.x, gPatchDistance.y), gPatchDistance.z);
;;     color = amplify(d1, 40, -0.5) * amplify(d2, 60, -0.5) * color;
;;
;;     FragColor = vec4(color, 1.0);
;; }

;; HELL YES

(glsl-code
 (v-compile
  '((tess-level-inner :float) (tess-level-outer :float)
    (projection :mat4) (model-view :mat4) (normal-mat :mat3)
    (light-position :vec3) (diffuse-material :vec3) (ambient-material :vec3))
  :450
  :draw-mode '(:patch 3)

  :vertex
  '(((position :vec4))
    (values position
     (s~ position :xyz)))

  :tessellation-control
  '(((position (:vec3 3)))
    (declare (output-patch :vertices 3))
    (emit-data (values (aref position gl-invocation-id)))
    (when (= gl-invocation-id 0)
      (setf (aref gl-tess-level-inner 0) tess-level-inner
            (aref gl-tess-level-outer 0) tess-level-outer
            (aref gl-tess-level-outer 1) tess-level-outer
            (aref gl-tess-level-outer 2) tess-level-outer))
    (values))

  :tessellation-evaluation
  '(((position (:vec3 *)))
    (declare (tessellate-to
              :primitive :triangles
              :spacing :equal
              :order :cw))
    (let* ((p0 (* (x gl-tess-coord) (aref position 0)))
           (p1 (* (y gl-tess-coord) (aref position 1)))
           (p2 (* (z gl-tess-coord) (aref position 2)))
           (pos (normalize (+ p0 p1 p2))))
      (values
       (* projection model-view (v! pos 1))
       pos
       gl-tess-coord)))

  :geometry
  '(((position (:vec3 3)) (patch-distance (:vec3 3)))
    (declare (output-primitive :kind :triangle-strip
              :max-vertices 3))
    (let* ((a (- (aref position 2) (aref position 0)))
           (b (- (aref position 2) (aref position 0)))
           (facet-normal (* normal-mat (normalize (cross a b)))))
      (emit ()
            (gl-position (aref gl-in 0))
            (aref patch-distance 0)
            facet-normal
            (v! 1 0 0))
      (emit ()
            (gl-position (aref gl-in 1))
            (aref patch-distance 1)
            facet-normal
            (v! 0 1 0))
      (emit ()
            (gl-position (aref gl-in 2))
            (aref patch-distance 2)
            facet-normal
            (v! 0 0 1))
      (varjo-lang::end-primitive)
      (values)))

  :fragment
  '(((patch-distance :vec3) (facet-normal :vec3) (tri-distance :vec3))
    (labels ((amplify ((d :float) (scale :float) (offset :float))
               (let* ((d (+ (* scale d) offset))
                      (d (clamp d 0 1)))
                 (- 1 (expt (* d d -2) 2)))))
      (let* ((n (normalize facet-normal))
             (l light-position)
             (df (abs (dot n l)))
             (color (+ ambient-material (* df diffuse-material)))
             (d1 (min (min (x tri-distance) (y tri-distance))
                      (z tri-distance)))
             (d2 (min (min (x patch-distance) (y patch-distance))
                      (z patch-distance)))
             (color (* (+ ambient-material (* df diffuse-material))
                       (amplify d1 40 -0.5)
                       (amplify d2 50 -0.5))))
        (v! color 1))))))

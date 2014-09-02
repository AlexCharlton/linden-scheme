;;;; tree.scm

;;;; This example illustrates a simple tree L-system

;;;; NOTE:
;;;; This file must be compiled.
;;;; Since it uses glls-render, it must also be linked with OpenGL
;;;; E.g.:
;;;; csc -lGL tree.scm

;;;; Use arrow keys to rotate, zoom camera.


(import chicken scheme)
(use linden-scheme
     glls-render gl-math (prefix gl-utils gl:) gl-utils-srfi-4
     (prefix glfw3 glfw:) (prefix opengl-glew gl:)
     random-mtzig)
(include "mesh-tools")

;;; The l-system
(define trunk-contraction-ratio 0.8)
(define branch-contraction-ratio 0.6)
(define trunk-branching-angle 45)
(define branch-branching-angle 45)
(define divergance-angle 137.5)
(define width-decrease-rate 0.707)
(thickness 0.1)

(define random-state (random-mtzig:init))

(define (random-normal #!optional mean variance)
  (+ mean
     (* (random-mtzig:randn! random-state)
        variance)))

(define-rule tree (apex l)
  `((stem ,l)
    (grow ,width-decrease-rate)
    (branch (pitch ,trunk-branching-angle)
            (branch-a ,(* l branch-contraction-ratio)))
    (roll ,divergance-angle)
    (apex ,(* l trunk-contraction-ratio))))

(define-rule tree (branch-a l)
  `((stem ,l)
    (grow ,width-decrease-rate)
    (branch (turn ,(- trunk-branching-angle))
            (branch-b ,(* l branch-contraction-ratio)))
    (roll ,(random-normal 0 30))
    (branch-b ,(* l trunk-contraction-ratio))))

(define-rule tree (branch-b l)
  `((stem ,l)
    (grow ,width-decrease-rate)
    (branch (turn ,trunk-branching-angle)
            (branch-a ,(* l branch-contraction-ratio)))
    (roll ,(random-normal 0 30))
    (branch-a ,(* l trunk-contraction-ratio))))

(define-l-system tree ()
  (apex 1))

(define-render-rule (stem length)
  (render-target
   (cons (cylinder length (thickness) (transform-matrix))
         (render-target)))
  (move-forward length))

;;; Rendering a tree
(define a-tree (make-parameter #f))

(define brown (f32vector 0.3 0.1 0.1))

(define cylinder
  (let ((mesh (cylinder-mesh 1 12)))
    (lambda (length radius #!optional transform)
      (make-mesh (mesh-vertex-data mesh)
                 (mesh-index-data mesh)
                 (if transform
                     (m* transform (3d-scaling radius length radius))
                     (3d-scaling radius length radius))))))

(define-pipeline mesh-shader
  ((#:vertex input: ((vertex #:vec3))
             uniform: ((mvp #:mat4)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 vertex 1.0)))))
  ((#:fragment uniform: ((color #:vec3))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (vec4 color 1.0)))))

(define (generate-tree)
  (let* ((mesh (mesh-append (render-l-system (step-l-system-times 13 (tree)) '())))
         (vao (gl:make-vao (mesh-vertex-data mesh) (mesh-index-data mesh)
                           `((,(pipeline-attribute 'vertex mesh-shader) float: 3)))))
    (a-tree (cons (make-mesh-shader-renderable
                   n-elements: (u32vector-length (mesh-index-data mesh))
                   element-type: (gl:type->gl-type uint:)
                   vao: vao
                   color: brown
                   mvp: (mvp))
                  mesh))))

;;; Matrices
(define projection-matrix (perspective 480 640 0.01 1000 70))
(define view-matrix (make-parameter #f))
(define mvp (make-parameter (make-f32vector 16)))

;;; Camera movement
(define pan (make-parameter 0))
(define zoom (make-parameter 0))
(define distance (make-parameter 4))
(define angle (make-parameter 0))

(glfw:key-callback
 (lambda (window key scancode action mods)
   (cond
    [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
     (glfw:set-window-should-close window 1)]
    [(and (eq? key glfw:+key-left+) (eq? action glfw:+press+))
     (pan (sub1 (pan)))]
    [(and (eq? key glfw:+key-right+) (eq? action glfw:+press+))
     (pan (add1 (pan)))]
    [(and (eq? key glfw:+key-left+) (eq? action glfw:+release+))
     (pan (add1 (pan)))]
    [(and (eq? key glfw:+key-right+) (eq? action glfw:+release+))
     (pan (sub1 (pan)))]
    [(and (eq? key glfw:+key-up+) (eq? action glfw:+press+))
     (zoom (sub1 (zoom)))]
    [(and (eq? key glfw:+key-down+) (eq? action glfw:+press+))
     (zoom (add1 (zoom)))]
    [(and (eq? key glfw:+key-up+) (eq? action glfw:+release+))
     (zoom (add1 (zoom)))]
    [(and (eq? key glfw:+key-down+) (eq? action glfw:+release+))
     (zoom (sub1 (zoom)))])))

(define (update)
  (angle (+ (angle) (/ (pan) 30)))
  (if (positive? (+ (distance) (* (zoom) 0.005)))
      (distance (+ (distance) (* (zoom) 0.05))))
   (let ([camera-x (* (distance) (sin (angle)))]
         [camera-z (* (distance) (cos (angle)))])
     (view-matrix (look-at (make-point camera-x 2 camera-z)
                           (make-point 0 2 0)
                           (make-point 0 1 0))))
   (mvp (m* projection-matrix
            (view-matrix)
            (mvp))))

;;; Initialize and main loop
(glfw:with-window (480 640 "A tree" resizable: #f)
   (gl:init)
   (gl:enable gl:+depth-test+)
   (gl:depth-func gl:+less+)
   (gl:clear-color 0.9 0.9 1.0 1)
   (compile-pipelines)
   (generate-tree)
   (let loop ()
     (glfw:swap-buffers (glfw:window))
     (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
     (update)
     (render-mesh-shader (car (a-tree)))
     (gl:check-error)
     (glfw:poll-events)
     (unless (glfw:window-should-close (glfw:window))
       (loop))))

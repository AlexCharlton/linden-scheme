;;;; tree.scm

;;;; This example illustrates a simple tree L-system

;;;; Use arrow keys to rotate, zoom camera.

(module tree ()
(import chicken scheme)
(use linden-scheme hypergiant)


(define brown-values '(0.3 0.1 0.1))
(define brown (apply make-rgb-color brown-values))
(define cylinder
  (let ((mesh (cylinder-mesh 1 1 1 12
                             index-type: #:uint
                             color: (lambda (_) brown-values)
                             normals?: #t)))
    (lambda (length radius transform rotation-matrix)
      (list mesh (m* transform (3d-scaling radius length radius))
            rotation-matrix))))


;;; The l-system
(define trunk-contraction-ratio 0.8)
(define branch-contraction-ratio 0.6)
(define trunk-branching-angle 45)
(define branch-branching-angle 45)
(define divergance-angle 137.5)
(define width-decrease-rate 0.707)
(thickness 0.1)

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
   (cons (cylinder length (thickness) (transform-matrix) (rotation-matrix))
         (render-target)))
  (move-forward length))


;;; Camera
(define scene (make-parameter #f))
(define camera (make-parameter #f))
(define pan (make-parameter 0))
(define tilt (make-parameter 0))
(define zoom (make-parameter 0))
(define c-roll (make-parameter 0))

(define keys (make-bindings
              `((quit ,+key-escape+ press: ,stop)
                (tilt-up ,+key-up+ toggle: ,tilt)
                (tilt-down ,+key-down+ reverse-toggle: ,tilt)
                (zoom-in ,+key-up+ mods: (,+mod-shift+) reverse-toggle: ,zoom)
                (zoom-out ,+key-down+ mods: (,+mod-shift+) toggle: ,zoom)
                (pan-right ,+key-right+ toggle: ,pan)
                (pan-left ,+key-left+ reverse-toggle: ,pan)
                (roll-right ,+key-right+ mods: (,+mod-shift+) reverse-toggle: ,c-roll)
                (roll-left ,+key-left+ mods: (,+mod-shift+) toggle: ,c-roll))))

(define (update delta)
  (yaw-camera! (camera) (/ (pan) 30))
  (pitch-camera! (camera) (/ (tilt) 30))
  (roll-camera! (camera) (/ (c-roll) 30))
  (zoom-camera! (camera) (/ (zoom) 10)))

(define a-tree (make-parameter #f))
(define shiny-material (make-material 0.5 0.5 0.5 10))

(define-pipeline phong-pipeline
  ((#:vertex input: ((position #:vec3) (normal #:vec3) (color #:vec3))
             uniform: ((mvp #:mat4) (model #:mat4))
             output: ((p #:vec3) (n #:vec3) (c #:vec3)))
   (define (main) #:void
     (set! gl:position (* mvp (vec4 position 1.0)))
     (set! p (vec3 (* model (vec4 position 1))))
     (set! c color)
     (set! n normal)))
  ((#:fragment input: ((n #:vec3) (p #:vec3) (c #:vec3))
               use: (phong-lighting)
               uniform: ((camera-position #:vec3)
                         (inverse-transpose-model #:mat4)
                         (ambient #:vec3)
                         (n-lights #:int)
                         (light-positions (#:array #:vec3 8))
                         (light-colors (#:array #:vec3 8))
                         (light-intensities (#:array #:float 8))
                         (material #:vec4))
               output: ((frag-color #:vec4)))
   (define (main) #:void
     (set! frag-color (light (vec4 c 1.0) p
                             (normalize (* (mat3 inverse-transpose-model) n)))))))


(define (init)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (activate-extension (scene) (lighting))
  (set-ambient-light! (scene) (make-rgb-color 0.1 0.1 0.1))
  (camera (make-camera #:perspective #:orbit (scene)))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 4)
  (add-light (scene) white 1000000
             position: (make-point 10 10 0))
  (a-tree (mesh-transform-append (render-l-system (step-l-system-times 13 (tree))
                                                  '())))
  (add-node (scene) phong-pipeline-render-pipeline
            mesh: (a-tree)
            material: shiny-material
            position: (make-point 0 -2 0)))

(start 640 480 "A tree" resizable: #f init: init update: update)

) ;end tree

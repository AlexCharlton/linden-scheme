;;;; tree.scm

;;;; This example illustrates a simple tree L-system

;;;; Use arrow keys to rotate, zoom camera.

(module tree ()
(import chicken scheme)
(use linden-scheme hypergiant)

(define cylinder
  (let ((mesh (cylinder-mesh 1 1 1 12 index-type: #:uint)))
    (lambda (length radius transform)
      (cons mesh (m* transform (3d-scaling radius length radius))))))


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
   (cons (cylinder length (thickness) (transform-matrix))
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
(define brown (make-rgb-color 0.3 0.1 0.1))

(define (init)
  (push-key-bindings keys)
  (gl:clear-color 0.9 0.9 1.0 1)
  (scene (make-scene))
  (camera (make-camera #:perspective #:orbit (scene)))
  (camera-look-at! (camera) (make-point 0 0 0))
  (set-camera-zoom! (camera) 4)
  (a-tree (mesh-transform-append 'position
                                 (render-l-system (step-l-system-times 13 (tree))
                                                  '())))
  (add-node (scene) mesh-pipeline-render-pipeline
            mesh: (a-tree)
            color: brown
            position: (make-point 0 -2 0)))

(start 640 480 "A tree" resizable: #f init: init update: update)

) ;end tree

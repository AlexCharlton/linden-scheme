(use test linden-scheme)

(define developemental-switch-time 3)
(define leaf-growth-limit 4)
(define flower-growth-limit 2)

(define-rule crocus (apex time)
  (cond
     ((< time developemental-switch-time)
      `((stem 1)
        (branch (pitch -30) (leaf 0))
        (roll 138)
        (apex ,(add1 time))))
     (else '((stem 20) (flower 0)))))

(define-rule crocus (flower size)
    (cond
     ((< size flower-growth-limit)
      `((flower ,(add1 size))))))

(define-rule plant (stem length)
  (cond
   ((< length 4) `((stem ,(add1 length))))))

(define-rule (leaf size)
    (cond
     ((< size leaf-growth-limit)
      `((leaf ,(add1 size))))))

(define-production crocus (plant)
  (apex 1))

(test '(crocus (apex 1))
      (crocus))

(test '(crocus (stem 1) (branch (pitch -30) (leaf 0)) (roll 138) (apex 2))
      (step-production (crocus)))

(test '(crocus
        (stem 2)
        (branch (pitch -30) (leaf 1))
        (roll 138)
        (stem 1)
        (branch (pitch -30) (leaf 0))
        (roll 138)
        (apex 3))
      (step-production-times 2 (crocus)))

(test '(crocus
        (stem 3)
        (branch (pitch -30) (leaf 2))
        (roll 138)
        (stem 2)
        (branch (pitch -30) (leaf 1))
        (roll 138)
        (stem 20)
        (flower 0))
      (step-production-times 3 (crocus)))

(test '(crocus
        (stem 4)
        (branch (pitch -30) (leaf 3))
        (roll 138)
        (stem 3)
        (branch (pitch -30) (leaf 2))
        (roll 138)
        (stem 20)
        (flower 1))
      (step-production-times 4 (crocus)))

(test '(crocus
        (stem 4)
        (branch (pitch -30) (leaf 4))
        (roll 138)
        (stem 4)
        (branch (pitch -30) (leaf 3))
        (roll 138)
        (stem 20)
        (flower 2))
      (step-production-times 5 (crocus)))

(test '(crocus
        (stem 4)
        (branch (pitch -30) (leaf 4))
        (roll 138)
        (stem 4)
        (branch (pitch -30) (leaf 4))
        (roll 138)
        (stem 20)
        (flower 2))
      (step-production-times 6 (crocus)))


(define-rule context-test (apex)
  (context
   (((stem len) * : (> len 2))
    '((leaf 1) (branch (leaf 1) (stem 1) (apex)) (stem 1) (apex)))
   (((stem len) *)
    '((apex)))
   (else '((leaf 1) (stem 1) (apex)))))

(define-production context-test (plant)
  (apex))

(test '(context-test
        (leaf 4)
        (stem 4)
        (leaf 1)
        (branch (leaf 1) (stem 1) (apex))
        (stem 1)
        (apex))
      (step-production-times 4 (context-test)))

(test-exit)
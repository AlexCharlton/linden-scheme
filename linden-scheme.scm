(module linden-scheme
  (define-rule
    rule-table
    class-table
    define-render-rule
    render-rule-table
    define-l-system
    step-l-system
    step-l-system-times
    render-l-system
    define-state
    get-state
    set-state
    context
    probability)

(import chicken scheme)
(use srfi-69 extras)

;;; State
(define state (make-parameter (make-hash-table)))

(define (define-state var default)
  (hash-table-set! (state) var (list default)))

(define (get-state var)
  (hash-table-ref (state) var))

(define (set-state var value)
  (hash-table-update! (state) var (lambda (v) (cons value (cdr v)))))

(define (push-state)
  (hash-table-walk (state)
                   (lambda (k v)
                     (hash-table-set! (state) k (cons (car v) v)))))

(define (pop-state)
  (hash-table-walk (state)
                   (lambda (k v)
                     (hash-table-set! (state) k (cdr v)))))

(define-state 'previous #f)
(define-state 'next #f)
(define-state 'render-target #f)


;;; Defining L-systems
(define class-table (make-hash-table))
(define rule-table (alist->hash-table `((default . ,(make-hash-table)))))
(define render-rule-table (make-hash-table))

(define (get-rule* table class rule)
  (let* ((supers (or (hash-table-ref/default class-table class #f)
                    (error 'get-rule "No such L-system" class)))
         (rules (hash-table-ref table class))
         (r (hash-table-ref/default rules rule #f)))
    (or r (let loop ((supers supers))
           (if (null? supers)
               #f
               (let* ((rules (hash-table-ref table (car supers)))
                      (r (hash-table-ref/default rules rule #f)))
                 (or r (loop (cdr supers)))))))))

(define (get-rule class rule)
  (get-rule* rule-table class rule))

(define (get-render-rule class rule)
  (get-rule* render-rule-table class rule))

(define-syntax define-l-system
  (syntax-rules ()
    ((define-l-system class (superclasses ...) (rule . args))
     (begin
       (hash-table-set! class-table 'class
                        (reverse (list 'default (quote superclasses) ...)))
       (define (class)
         '(class (rule . args)))))))

(define-syntax define-rule
  (syntax-rules ()
    ((define-rule (rule . args) body . body-rest)
     (define-rule default (rule . args) body . body-rest))
    ((define-rule class (rule . args) body . body-rest)
     (hash-table-update! rule-table 'class
                         (lambda (table)
                           (hash-table-set! table 'rule (lambda args body . body-rest))
                           table)
                         (lambda () (make-hash-table))))))

(define-syntax define-render-rule
  (syntax-rules ()
    ((define-render-rule (rule . args) body . body-rest)
     (define-render-rule default (rule . args) body . body-rest))
    ((define-render-rule class (rule . args) body . body-rest)
     (hash-table-update! render-rule-table 'class
                         (lambda (table)
                           (hash-table-set! table 'rule (lambda args body . body-rest))
                           table)
                         (lambda () (make-hash-table))))))


;;; Stepping and rendering
(define (apply-rule class rule args)
  (let ((r (get-rule class rule)))
    (if r
        (let ((r (apply r args)))
          (if (list? r)
              r
              (list (cons rule args))))
        (list (cons rule args)))))

(define (step-l-system system)
  (push-state)
  (let ((class (car system))
        (system (cdr system)))
    (define (set-next s)
      (set-state
       'next
       (let recur ((s s))
         (cond
          ((null? s)
           #f)
          ((not (list? (car s))) (error 'step-l-system "Malformed L-system"
                                      system))
          ((equal? (caar s) 'branch)
           (cons (cadar s) (recur (cdr s))))
          (else (car s))))))
    (define (step s)
      (cond
       ((null? s)
        (pop-state)
        '())
       ((equal? (caar s) 'branch)
        (push-state)
        (cons (cons 'branch (step (cdar s)))
              (step (cdr s))))
       (else
        (set-next (cdr s))
        (let ((result (apply-rule class (caar s) (cdar s))))
          (set-state 'previous (car s))
          (append result (step (cdr s)))))))
    (cons class (step system))))

(define (step-l-system-times k system)
  (let loop ((i k) (s system))
    (if (zero? i)
        s
        (loop (sub1 i) (step-l-system s)))))

(define (render-l-system system render-target)
  (push-state)
  (set-state 'render-target render-target)
  (let ((class (car system))
        (system (cdr system)))
    (define (render s)
      (cond
       ((null? s)
        (pop-state))
       ((not (list? (car s))) (error 'render-l-system "Malformed L-system"
                                   system))
       ((equal? (caar s) 'branch)
        (push-state)
        (render (cdar s))
        (render (cdr s)))
       (else
        (let ((r (get-render-rule class (caar s))))
          (when r
            (apply r (cdar s))))
        (render (cdr s)))))
    (render system))
  render-target)


;; Context
(define-syntax bind-args*
  (syntax-rules ()
    ((bind-args state vars () body . body-rest)
     (syntax-error 'context "Too many parameters (limit is 16)" state))

    ((bind-args state (var) (elt . rest-elts) body . body-rest)
     (let ((var (and (list? (car state)) (list-ref (car state) elt))))
       body . body-rest))

    ((bind-args state (var . rest-vars) (elt . rest-elts) body . body-rest)
     (let ((var (and (list? (car state)) (list-ref (car state) elt))))
       (bind-args state rest-vars rest-elts body . body-rest)))))

(define-syntax bind-args
  (syntax-rules ()
    ((bind-args state vars body . body-rest)
     (bind-args* state vars (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
                 body . body-rest))))

(define-syntax context
  (syntax-rules (else * :)
    ;; Fallback
    ((context (else body . body-rest))
     (begin body . body-rest))

    ;; Single clause
    ((context (((prev . prev-args) *) body . body-rest))
     (let ((previous (get-state 'previous)))
       (when (and (list? (car previous))
                (equal? (caar previous) 'prev))
         (bind-args previous prev-args body . body-rest))))

    ((context ((* (nekst . next-args)) body . body-rest))
     (let ((next (get-state 'next)))
       (when (and (list? (car next))
                (equal? (caar next) 'nekst))
         (bind-args next next-args body . body-rest))))

    ((context (((prev . prev-args) (nekst . next-args)) body . body-rest))
     (let ((next (get-state 'next))
           (previous (get-state 'previous)))
       (when (and (list? (car next))
                (list? (car previous))
                (equal? (caar next) 'nekst)
                (equal? (caar previous) 'prev))
         (bind-args next next-args
            (bind-args previous prev-args body . body-rest)))))

    ;; Multiple clauses
    ((context (((prev . prev-args) *) body . body-rest) . cl)
     (let ((previous (get-state 'previous)))
       (if (and (list? (car previous))
              (equal? (caar previous) 'prev))
           (bind-args previous prev-args body . body-rest)
           (context . cl))))

    ((context ((* (nekst . next-args)) body . body-rest) . cl)
     (let ((next (get-state 'next)))
       (if (and (list? (car next))
              (equal? (caar next) 'nekst))
           (bind-args next next-args body . body-rest)
           (context . cl))))

    ((context (((prev . prev-args) (nekst . next-args)) body . body-rest) . cl)
     (let ((next (get-state 'next))
           (previous (get-state 'previous)))
       (if (and (list? (car next))
              (list? (car previous))
              (equal? (caar next) 'nekst)
              (equal? (caar previous) 'prev))
           (bind-args next next-args
             (bind-args previous prev-args body . body-rest))
           (context . cl))))

    ;; Single clause with guard
    ((context (((prev . prev-args) * : test) body . body-rest))
     (let ((previous (get-state 'previous)))
       (bind-args previous prev-args
         (when (and (list? (car previous))
                  (equal? (caar previous) 'prev)
                  test)
           body . body-rest))))

    ((context ((* (nekst . next-args) : test) body . body-rest))
     (let ((next (get-state 'next)))
       (bind-args next next-args 
         (when (and (list? (car next))
                  (equal? (caar next) 'nekst)
                  test)
           body . body-rest))))

    ((context (((prev . prev-args) (nekst . next-args) : test) body . body-rest))
     (let ((next (get-state 'next))
           (previous (get-state 'previous)))
       (bind-args next next-args
         (bind-args previous prev-args
           (when (and (list? (car next))
                    (list? (car previous))
                    (equal? (caar next) 'nekst)
                    (equal? (caar previous) 'prev)
                    test)
             body . body-rest)))))

    ;; Multiple clauses with guard
    ((context (((prev . prev-args) * : test) body . body-rest) . cl)
     (let ((previous (get-state 'previous)))
       (bind-args previous prev-args
         (if (and (list? (car previous))
                (equal? (caar previous) 'prev)
                test)
             (begin body . body-rest)
             (context . cl)))))

    ((context ((* (nekst . next-args) : test) body . body-rest) . cl)
     (let ((next (get-state 'next)))
       (bind-args next next-args
         (if (and (list? (car next))
                (equal? (caar next) 'nekst)
                test)
             (begin body . body-rest)
             (context . cl)))))

    ((context (((prev . prev-args) (nekst . next-args) : test) body . body-rest) . cl)
     (let ((next (get-state 'next))
           (previous (get-state 'previous)))
       (bind-args next next-args
         (bind-args previous prev-args
           (if (and (list? (car next))
                  (list? (car previous))
                  (equal? (caar next) 'nekst)
                  (equal? (caar previous) 'prev)
                  test)
               (begin body . body-rest)
               (context . cl))))))))

;; Probability
(define-syntax probability*
  (syntax-rules (else)
    ((probability* random-number current-number (else body . body-rest))
     (begin body . body-rest))
    ((probability* random-number current-number (prob body . body-rest))
     (when (> (* (+ prob current-number) 32768) random-number)
       body . body-rest))
    ((probability* random-number current-number (prob body . body-rest) . cl)
     (if (> (* (+ prob current-number) 32768) random-number)
         (begin body . body-rest)
         (probability* random-number (+ current-number prob) . cl)))))

(define-syntax probability
  (syntax-rules ()
    ((probability clause . clauses)
     (let ((random-number (random 32768)))
       (probability* random-number 0 clause . clauses)))))

) ; end module linden-scheme

; dominoes

(define (range x)
  (if (< x 0) '() (cons x (range (- x 1))))
  )

(define (dominoes-help x)
  (let ((h x)) (map (lambda (x) (cons h x)) (range x)))
  )

(define (dominoes x)
  (if (< x 0) '()
      (append (dominoes-help x) (dominoes (- x 1)))
      )
  )

; permutations L


(define (remove ls elm)
  (cond ((null? ls) '())
	((equal? (car ls) elm) (cdr ls))
	(else (cons (car ls) (remove (cdr ls) elm)))
	)
  )


(define (permutations items)
  (if (null? items) '(())
      (apply append (map (construct-permutation-generator items) items))
   )
  )

(define (construct-permutation-generator items)
  (lambda (element)
    (map (construct-permutation-prepender element) (permutations (remove items element)))
    )
  )

(define (construct-permutation-prepender element) (lambda (perm) (cons element perm)))

;; (loop? L)

(define (flip t) (cons (cdr t) (car t)))

(define (first-flip-list x)
  (cons (flip (car x)) (cdr x))
  
  )



(define (pair-eq? x y)
  (cond
   ((= (cdr x) (car y)) #t)
   (#t #f)
   )
  )


(define (match inp res)
  (cond
   ((null? res) (match (cdr inp) (cons (car inp) res)))
   ((and (null? inp) (pair-eq? (car res) (car (reverse res)))) (reverse res))
   ((and (null? inp) (not (pair-eq? (car res) (car (reverse res))))) '())
   ((pair-eq? (car res) (car inp)) (match (cdr inp) (cons (car inp) res)))
   ((pair-eq? (car res) (flip (car inp))) (match (cdr inp) (cons (flip (car inp)) res)))
   (#t '())
   )
  )

(define (match-full inp res)
  (append (match inp res) (match (first-flip-list inp) res))
  )

(define (loop? L)
  (match-full L '())
  )


(define (test-filter x)
  (if (>= x 5) x '())
  )

(define filter
  (lambda (f L)
    (if (null? L) L
	(let ((N (f (car L))))
			      (if (null? N)
				  (filter f (cdr L))
				  (cons N (filter f (cdr L)))
				  )
			      )	
	)
    )
  )

(define domino-loops

  (lambda (n) (filter loop? (permutations (dominoes n))))
  )






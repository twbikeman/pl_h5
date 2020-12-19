

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

(define (match x y) (cond 
			   ((= (cdr x) (car y)) (cons (car x) (cdr y)))
			   ((= (cdr x) (cdr y)) (cons (car x) (car y)))
			   (#t (cons -1 -2))
			   )
  )


(define (first items)
   (cons(cons (cdr (car items)) (car (car items))) (cdr items))     
   )


(define (reduce f l)
  (if (= (length l) 1) l 
       (reduce f (cons (f (car l) (car (cdr l))) (cdr (cdr l))))
      )
    )

(define (verify items)
  (if (= (car (car items)) (cdr (car items))) #t #f)
  )



(define (loop-helper r l)
  (cond ((null? l) r)

	((eq? (last-pair r) (car l)) (loop-helper (cons r (car l)) (cdr l)))
	
      
      )

  )




(define (loop? items)
  (verify (reduce match items))
  )

;; domino-loops

(define (filter-help pred lst res) 
  (cond ((null? lst) res)
	((pred (car lst)) (filter-help pred (cdr lst) (cons (car lst) res)))
	(#t (filter-help pred (cdr lst) res))      
	)
  )





(define (domino-loops n)
  (append (reverse (filter-help loop? (permutations (dominoes n)) '()))     (reverse (filter-help loop? (map first (permutations (dominoes n))) '())))
  )


;; (define (adjust-help r l)
;;   (if (= (length r) 1) (cons r l) 
;;       (adjust (cdr r) (l car r))
;;       )
;;   )



(define (adjust x y) (cond
		      ((= (length x) 0) (reverse x))
		      ((= (cdr x) (car x)) (cons (car x) (car y)))
		      ((= (cdr x) (cdr y)) (cons (car x) (car y)))
		      (#t (cons -1 -2))
		      )
  )




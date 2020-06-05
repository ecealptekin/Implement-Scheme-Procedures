(define symbol-length
     (lambda (inSym)
         (if (symbol? inSym)
             (string-length (symbol->string inSym))
              0 )))

(define sequence? (lambda (inSeq)
    (cond 
       ((null? inSeq) #t)
       ((not (symbol? (car inSeq))) #f)
       ((= (symbol-length (car inSeq)) 0) #f)
       ((not (= (symbol-length (car inSeq)) 1)) #f) 
       (else (car inSeq) (sequence? (cdr inSeq))))))

(define same-sequence? (lambda (inSeq1 inSeq2)
     (cond
     ((and (sequence? inSeq1) (sequence? inSeq2) (equal? inSeq1 inSeq2)) #t)
     ((and (sequence? inSeq1) (sequence? inSeq2) (not (equal? inSeq1 inSeq2))) #f)
     ((or  (not(sequence? inSeq1)) (not(sequence? inSeq2))) (error "ERROR")))))

(define reverse-sequence (lambda (inSeq)
   (cond
   ((null? inSeq) '())
   ((equal? (sequence? inSeq) #f) (error "ERROR"))
   (else (append (reverse-sequence (cdr inSeq)) (list (car inSeq)))))))

(define (palindrome? inSeq)
  (cond
   ((null? inSeq) #t)
   ((not (sequence? inSeq)) (error "ERROR"))
   ((null? (cdr inSeq)) #t)
   ((list? (car inSeq))
    ((null? (car inSeq))())
    ((null? (cdr inSeq))())
    ((equal? (caar inSeq) (caar (reverse inSeq)))
     (palindrome? (cdar (reverse(cdar inSeq))))))
   ((equal? (car inSeq) (car(reverse inSeq))) 
    (palindrome? (cdr (reverse (cdr inSeq)))))
   (else #F)))

(define member? (lambda (inSym inSeq)
  (cond 
     ((null? inSeq) #f)
     ((or (not (sequence? inSeq)) (not (symbol? inSym))) (error "ERROR"))
     ((equal? inSym (car inSeq)) #t)
     (else (member? inSym (cdr inSeq))))))

(define remove-member (lambda (inSym inSeq)
    (cond
     ((null? inSeq) (error "ERROR"))
     ((equal? inSym (car inSeq)) (cdr inSeq))
     ((or (not (sequence? inSeq)) (not (symbol? inSym)) (not (member? inSym inSeq))) (error "ERROR"))
     (else (cons (car inSeq) (remove-member inSym (cdr inSeq)))))))

(define anagram? (lambda (inSeq1 inSeq2)
   (cond  
     ((not (sequence? inSeq1)) (error "ERROR")) 
     ((not (sequence? inSeq2)) (error "ERROR")) 
     ((and (null? inSeq1) (null? inSeq2)) #t) 
     ((or (null? inSeq1) (null? inSeq2)) #f)                          
        ((member? (car inSeq1) inSeq2)
        (anagram? (cdr inSeq1) (remove-member (car inSeq1) inSeq2)))
     (else #f)))) 

(define anapoli? (lambda (inSeq1 inSeq2)
 (cond 
     ((or (not (sequence? inSeq1)) (not (sequence? inSeq2))) (error "ERROR"))
     ((and (sequence? inSeq1) (sequence? inSeq2))
         (cond 
            ((and (palindrome? inSeq2) (anagram? inSeq1 inSeq2)) #t)
            (else #f))
     )
     (else #f))))

; Pranav Katkamwar (pranavk)
; Section 06
; Spring 2017 - CS314 - Project 2

; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2017                              *
; *  Student Version                          *
; *********************************************

;; contains "ctv", "A", and "reduce" definitions
(load "include.ss")

;; contains simple dictionary definition
(load "test-dictionary.ss")

;; -----------------------------------------------------
;; HELPER FUNCTIONS

;;
;; Reverse A List (taken from HW6 Solutions)
;; (not sure about this)

(define rev
  (lambda (l)
    (cond
     ((null? l) '())
     ((list? (car l))
      (append (rev (cdr l)) (cons (rev (car l)) '())))
     (else
      (append (rev (cdr l)) (cons (car l) '()))))))

;;
;; Return whether or not an element is in the given list

(define foundInList?
  (lambda (lookUp l)
    (cond
      ((null? l)
        #f
        )
      ((= lookUp (car l))
        #t
        )
      (else 
        (foundInList? lookUp (cdr l))
        )
      )
  )
)

;;
;; Apply all the Hash Functions and Map the Hashed Values into a list
;; Can use as many hash functions as you want - setup for future user

(define applyHashes
  (lambda (hashfunctionlist hashedList)
    (if (null? hashfunctionlist)
      '()
      (append
        (map (car hashfunctionlist) hashedList)
        (applyHashes (cdr hashfunctionlist) hashedList)
        )
      )
    )
  )

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
     (if (null? w)
      5187
      (+ (ctv (car w)) (* 29 (key (cdr w))))
     )
  )
)

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))       = 106402241991
;;   (key '(m a y))           = 126526810
;;   (key '(t r e e f r o g)) = 2594908189083745

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (divWord)
	     (if (null? divWord)
	     	0
        (modulo (key divWord) size)
	     )
	   )
  )
)

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (multWord)
        (if (null? multWord)
          0
          (let (
                  (kA (* (key multWord) A))
                )
            (floor (* size (- kA (floor kA))))
          )
        )
     )
  )
)


;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 7224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o))       ==> 35616
;;  (hash-1 '(m a y))           ==> 46566
;;  (hash-1 '(t r e e f r o g)) ==> 48238
;;
;;  (hash-2 '(h e l l o))       ==> 48849
;;  (hash-2 '(m a y))           ==> 81025
;;  (hash-2 '(t r e e f r o g)) ==> 16708
;;
;;  (hash-3 '(h e l l o))       ==> 6331.0
;;  (hash-3 '(m a y))           ==> 2456.0
;;  (hash-3 '(t r e e f r o g)) ==> 1806.0
;;
;;  (hash-4 '(h e l l o))       ==> 788.0
;;  (hash-4 '(m a y))           ==> 306.0
;;  (hash-4 '(t r e e f r o g)) ==> 225.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     (let ((hashedDict (applyHashes hashfunctionlist dict)))
        (lambda (w)
          (if (null? w)
            0
            (let* (
                   ; Map the word to a list with its hashed values
                    (hashedWordList 
                      (map (lambda (x) (x w)) hashfunctionlist)
                      )
                    ; Map the above list to a list of #t or #f, depending on if its hash was found in the master hashedDict
                    (crossCheckedResults 
                      (map (lambda (x) (foundInList? x hashedDict)) hashedWordList)
                      )
                  )
              ; Reduce the above list - print #t only if all hashed values are found in hashedDict
              (reduce (lambda (x y) (and x y)) crossCheckedResults #t)
            )
          )
        )
     )
  )
)


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(a r g g g g)) ==> #t  // false positive

#lang racket

(require rackunit)

(struct package
  (name file-list version#)
  #:guard (λ(name file-list version# type-name)
            (let ([valid-file-list (file-list? file-list)]
                  [valid-name (name? name)]
                  [valid-version# (unknown->version# version#)])
              (cond [(not valid-file-list) (error "File list is not a list")]
                    [(not valid-name) (error "Invalid name for package")]
                    [(not valid-version#) (error "Invalid version number")]
                    [else (values valid-name valid-file-list valid-version#)]))))

;; Validate file list

(define (file-list? files)
  (if (list? files)
      files
      #f))


;; Validate package name

(define (name? name)
  (cond [(string? name) (valid-package-name? name)]
        [(symbol? name) (valid-package-name? (symbol->string name))]
        [else "Invalid value for name"]))


(define (valid-package-name? name)
  (let ([char-name (string->list name)])
    (cond [(null? char-name) (error "Empty package name")]
          [(any invalid-char? char-name) (error "Invalid char in package name: ~e. Package names should only contain lowercase letters and dashes."
                                                (any invalid-char? char-name))]
          [(not (char-lower-case? (car char-name))) (error"Package names start with letters")]
          [else name])))

(define (any condition ls)
  (cond [(null? ls) #f]
        [(condition (car ls)) (car ls)]
        [else (any condition (cdr ls))]))

(define (invalid-char? x)
  (if (or (char-lower-case? x) (char=? x #\-))
      #f
      x))

;; Validate version number

(define (unknown->version# unk)
  (define (get-parts str)
    (match (map string->number (string-split str "."))
      [(list major) (version# major 0 0)]
      [(list major minor) (version# major minor 0)]
      [(list major minor patch) (version# major minor patch)]
      [else #f]))

  (cond [(string? unk) (get-parts unk)]
        [(symbol? unk) (get-parts (symbol->string unk))]
        [else (error "Invalid value for version#")]))


(struct version# (major minor patch)
  #:guard (λ(major minor patch type-name)
            (cond [(not (integer? major)) (error "Major is not an integer")]
                  [(not (integer? minor)) (error "Minor is not an integer")]
                  [(not (integer? patch)) (error "Patch is not an integer")]
                  [else (values major minor patch)]))
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (version#-major a) (version#-major b))
          (equal?-recur (version#-minor a) (version#-minor b))
          (equal?-recur (version#-patch a) (version#-patch b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (version#-major a))
        (* 3 (hash-recur (version#-minor a)))
        (* 9 (hash-recur (version#-patch a)))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (version#-major a))
        (hash2-recur (version#-minor a))
        (hash2-recur (version#-patch a))0))])


;; Reader

(define (extract-from-hash hash . rest)
  (apply values (map (curry hash-ref hash) rest)))

(define (read-config path)
  (let*-values ([(file) (open-input-file path)]
                [(contents) (read file)]
                [(content-hash) (make-hash contents)]
                [(name files version) (extract-from-hash content-hash 'name 'files 'version)])
        (package name files version)))

;; Test

(check-not-false (invalid-char? #\T))
(check-not-false (invalid-char? #\5))
(check-false (invalid-char? #\t))
(check-false (invalid-char? #\-))

(check-eq? (valid-package-name? "memes") "memes")
(check-exn exn:fail? (thunk (valid-package-name? "Memes")))
(check-exn exn:fail? (thunk(valid-package-name? "-oof")))

(check-true (version#? (unknown->version# "1.1.0")))
(check-true (version#? (unknown->version# '1.1.0)))
(check-true (version#? (unknown->version# "1")))
(check-true (version#? (unknown->version# "1.1")))

(check-true (package? (package 'my-package '() '1.0.0)))

(check-true (package? (read-config "../example/test.mpm")))
(check-true (let* ([stdlib (read-config "../example/test.mpm")]
                   [name (package-name stdlib)]
                   [files (package-file-list stdlib)]
                   [version (package-version# stdlib)])
              (and (string=? name "std")
                   (null? files)
                   (equal? version (version# 1 0 0)))))

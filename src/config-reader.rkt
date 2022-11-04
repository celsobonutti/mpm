#lang racket

(require rackunit)

(struct package
  (name file-list version#)
  #:guard (λ(name file-list version# type-name)
            (cond [(not (list? file-list)) (error "File list is not a list")]
                  [(not (name? name) (error "Invalid name for package"))]
                  [(not (version#? version#)) (error "Invalid version number")]
                  [else (values name file-list version#)])))

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
      [else (error "Invalid version")]))

  (cond [(string? unk) (get-parts unk)]
        [(symbol? unk) (get-parts (symbol->string unk))]
        [else (error "Invalid value for version#")]))


(struct version# (major minor patch)
  #:guard (λ(major minor patch type-name)
            (cond [(not (integer? major)) (error "Major is not an integer")]
                  [(not (integer? minor)) (error "Minor is not an integer")]
                  [(not (integer? patch)) (error "Patch is not an integer")]
                  [else (values major minor patch)])))



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

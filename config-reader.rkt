#lang racket

(provide version
         (contract-out (struct package ((name name?) (file-list file-list?) (version# version?)))
                       [version->string (version? . -> . string?)]
                       [read-config ((and/c path? file-exists?) . -> . package?)]))

(struct package (name file-list version#)
  #:guard (λ (name file-list version# type-name)
            (let ([valid-file-list (file-list? file-list)]
                  [valid-name (name? name)]
                  [valid-version# (make-version version#)])
              (cond
                [(not valid-file-list) (error "File list is not a list")]
                [(not valid-name) (error "Invalid name for package")]
                [(not valid-version#) (error "Invalid version number")]
                [else (values valid-name valid-file-list valid-version#)]))))

;; Validate file list

(define (file-list? files)
  (if (list? files) (existing-files? files) #f))

(define (existing-files? files)
  (define (existing-file? file)
    (define path
      (cond
        [(path-string? file) file]
        [(symbol? file) (symbol->string file)]
        [else (error "Invalid type for file")]))
    (if (or (file-exists? path) (directory-exists? path))
        path
        (error "File/directory does not exist")))
  (map existing-file? files))

;; Validate package name

(define (name? name)
  (or (and (string? name) (valid-package-name? name))
      (and (symbol? name) (valid-package-name? (symbol->string name)))))

(define (valid-package-name? name)
  (let ([char-name (string->list name)])
    (cond
      [(null? char-name) (error "Empty package name")]
      [(any invalid-char? char-name)
       (error
        "Invalid char in package name: ~e. Package names should only contain lowercase letters and dashes."
        (any invalid-char? char-name))]
      [(not (char-lower-case? (car char-name))) (error "Package names start with letters")]
      [else name])))

(define (any condition ls)
  (cond
    [(null? ls) #f]
    [(condition (car ls)) (car ls)]
    [else (any condition (cdr ls))]))

(define (invalid-char? x)
  (if (or (char-lower-case? x) (char=? x #\-)) #f x))

;; Validate version number

(define (make-version value)
  (define (get-parts str)
    (match (map string->number (string-split str "."))
      [(list major) (version major 0 0)]
      [(list major minor) (version major minor 0)]
      [(list major minor patch) (version major minor patch)]
      [else #f]))

  (cond
    [(string? value) (get-parts value)]
    [(symbol? value) (get-parts (symbol->string value))]
    [(and (integer? value) (number? value)) (version value 0 0)]
    [else (error "Invalid value for version#")]))

(struct/contract version
                 ([major nonnegative-integer?] [minor nonnegative-integer?]
                                               [patch nonnegative-integer?]))

(define (version->string version)
  (apply string-append
         (add-between (map number->string
                           (map (λ (f) (f version)) (list version-major version-minor version-patch)))
                      ".")))

(define (version=? v1 v2)
  (and (= (version-major v1) (version-major v2))
       (= (version-minor v1) (version-minor v2))
       (= (version-patch v1) (version-patch v2))))

;; Reader

(define (extract-from-hash hash . rest)
  (apply values (map (curry hash-ref hash) rest)))

(define (read-config path)
  (call-with-input-file
   path
   (λ (file)
     (let*-values ([(contents) (read file)]
                   [(content-hash) (make-hash contents)]
                   [(base-path _ __) (split-path (simple-form-path path))]
                   [(name files version) (extract-from-hash content-hash 'name 'files 'version)])
       (package name (map (curry prepend-path base-path) files) version)))))

(define (prepend-path base file)
  (define file-path
    (cond
      [(path-string? file) file]
      [(symbol? file) (symbol->string file)]
      [else (error "Invalid value for file")]))
  (build-path base file-path))

;; Test

(module+ test
  (require rackunit)

  (check-not-false (invalid-char? #\T))
  (check-not-false (invalid-char? #\5))
  (check-false (invalid-char? #\t))
  (check-false (invalid-char? #\-))

  (check-eq? (valid-package-name? "memes") "memes")
  (check-exn exn:fail? (thunk (valid-package-name? "Memes")))
  (check-exn exn:fail? (thunk (valid-package-name? "-oof")))

  (check-true (version? (make-version "1.1.0")))
  (check-true (version? (make-version '1.1.0)))
  (check-true (version? (make-version "1")))
  (check-true (version? (make-version "1.1")))
  (check-true (version? (make-version 1)))

  (check-true (package? (package 'my-package '() '1.0.0)))

  (check-true (package? (read-config "./example/test.mpm")))
  (check-true (let*-values ([(path) "./example/test.mpm"]
                            [(stdlib) (read-config path)]
                            [(name) (package-name stdlib)]
                            [(files) (package-file-list stdlib)]
                            [(version#) (package-version# stdlib)]
                            [(base-path _ __) (split-path (simple-form-path path))])
                (and (string=? name "std")
                     (equal? files (map (curry prepend-path base-path) (list "src")))
                     (version=? version# (version 1 0 0))))))

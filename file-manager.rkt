#lang racket

(require (prefix-in reader: mpm/config-reader))

(define pkg (reader:read-config (string->path "./example/test.mpm")))

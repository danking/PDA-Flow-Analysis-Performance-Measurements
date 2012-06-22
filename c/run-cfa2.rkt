#lang racket

(require "ansi-c-pda.rkt")
(require "../run-cfa2.rkt")

(file-stream-buffer-mode (current-output-port) 'none)

; (pretty-print (run-cfa2 ansi-c-pda-risc))
(match-define (list fv-set sum call pre) (run-cfa2 ansi-c-pda-risc))

(require "../../pda-to-pda-risc/risc-enhanced/data.rkt"
         "../../pda-to-pda-risc/risc-enhanced/decorate.rkt")

(define pre2 (decorate ansi-c-pda-risc))

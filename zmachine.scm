;;; zmachine.scm -- Z-Machine interpreter entry point
;;;
;;; Usage: mit-scheme --load zmachine.scm -- <story-file>
;;;
;;; Note: must be started from this directory; the `load` calls below
;;; resolve relative to the working directory (MIT Scheme gives us no
;;; reliable way to locate the script itself under --load).

(load "memory")
(load "decode")
(load "ops")

(define *trace* #f)  ; Set to #t for instruction tracing

(define (setup-game-input!)
  ;; Reading EOF from MIT Scheme's console port (terminal OR pipe)
  ;; aborts the whole process ("End of input stream reached") instead
  ;; of returning an eof-object. Reopen /dev/stdin as an ordinary file
  ;; port, which behaves properly at end-of-file; fall back to the
  ;; console port where /dev/stdin is unavailable.
  (set! *game-input-port*
        (guard (exn (#t (current-input-port)))
          (open-input-file "/dev/stdin"))))

(define (main-loop)
  (let loop ()
    (when *running*
      (when *trace*
        (display (string-append "[PC=" (number->string *pc* 16) "] "))
        (flush-output-port (current-output-port)))
      (let ((instr (decode-instruction!)))
        (when *trace*
          (display (string-append (symbol->string (instr-opcount instr))
                                  ":" (number->string (instr-opnum instr))
                                  " " (write-to-string (instr-operands instr))))
          (newline)
          (flush-output-port (current-output-port)))
        (dispatch-instruction! instr))
      (loop))))

(define (run-zmachine filename)
  (setup-game-input!)
  (display (string-append "Loading " filename "...\n"))
  (load-story-file filename)
  (display (string-append "Z-Machine version " (number->string *version*) "\n"))
  (display (string-append "Initial PC: $" (number->string *initial-pc* 16) "\n"))
  (display (string-append "High memory: $" (number->string *high-mem-base* 16) "\n"))
  (display (string-append "Static memory: $" (number->string *static-mem-base* 16) "\n"))
  (display (string-append "Dictionary: $" (number->string *dictionary-addr* 16) "\n"))
  (display (string-append "Object table: $" (number->string *object-table-addr* 16) "\n"))
  (display (string-append "Globals: $" (number->string *globals-addr* 16) "\n"))
  (display (string-append "Abbreviations: $" (number->string *abbrev-table-addr* 16) "\n"))
  (newline)
  (init-opcodes!)
  ;; Clear screen for V3 (also resets any scroll region left over)
  (when (= *version* 3)
    (display "\033[r\033[2J\033[1;1H")
    (flush-output-port (current-output-port)))
  (main-loop)
  (newline)
  (display "Game ended.\n"))

;; Parse command line arguments
(let ((args (command-line)))
  (cond
   ((< (length args) 2)
    (display "Usage: mit-scheme --load zmachine.scm -- <story-file>\n"))
   (else
    (let ((filename (list-ref args (- (length args) 1))))
      ;; Check for --trace flag
      (when (member "--trace" args)
        (set! *trace* #t))
      (run-zmachine filename)))))

;;; memory.scm -- Z-Machine memory map, header, variables, stack, call frames

;;;
;;; Global machine state
;;;

(define *memory* #f)           ; bytevector holding the entire story file
(define *memory-size* 0)       ; size of the story file
(define *pc* 0)                ; program counter
(define *stack* '())           ; evaluation stack (list, head = top)
(define *call-stack* '())      ; call frames
(define *version* 0)           ; Z-machine version (1-8)
(define *running* #t)          ; main loop flag
(define *current-arg-count* 0) ; args passed to current routine

;; Header cache (set during load)
(define *high-mem-base* 0)
(define *initial-pc* 0)
(define *dictionary-addr* 0)
(define *object-table-addr* 0)
(define *globals-addr* 0)
(define *static-mem-base* 0)
(define *abbrev-table-addr* 0)

;;;
;;; Memory access
;;;

(define (mem-byte addr)
  (bytevector-u8-ref *memory* addr))

(define (mem-set-byte! addr val)
  (bytevector-u8-set! *memory* addr (bitwise-and val #xFF)))

(define (mem-word addr)
  (bitwise-ior (arithmetic-shift (mem-byte addr) 8)
               (mem-byte (+ addr 1))))

(define (mem-set-word! addr val)
  (let ((v (bitwise-and val #xFFFF)))
    (mem-set-byte! addr (arithmetic-shift v -8))
    (mem-set-byte! (+ addr 1) (bitwise-and v #xFF))))

;; Read from PC and advance
(define (read-pc-byte!)
  (let ((b (mem-byte *pc*)))
    (set! *pc* (+ *pc* 1))
    b))

(define (read-pc-word!)
  (let ((w (mem-word *pc*)))
    (set! *pc* (+ *pc* 2))
    w))

;;;
;;; 16-bit arithmetic helpers
;;;

(define (u16 val)
  (bitwise-and val #xFFFF))

(define (s16 val)
  ;; Convert unsigned 16-bit to signed
  (let ((v (bitwise-and val #xFFFF)))
    (if (> v 32767) (- v 65536) v)))

(define (u16-from-signed val)
  ;; Convert signed to unsigned 16-bit
  (bitwise-and val #xFFFF))

;;;
;;; Stack operations
;;;

(define (stack-push! val)
  (set! *stack* (cons (u16 val) *stack*)))

(define (stack-pop!)
  (if (null? *stack*)
      (error "Stack underflow")
      (let ((val (car *stack*)))
        (set! *stack* (cdr *stack*))
        val)))

(define (stack-top)
  (if (null? *stack*)
      (error "Stack empty")
      (car *stack*)))

;;;
;;; Variables: 0 = stack, 1-15 = locals, 16-255 = globals
;;;

;; Current frame's local variables (a vector, up to 15 entries)
(define *locals* (make-vector 15 0))
(define *num-locals* 0)

(define (get-variable var-num)
  (cond
   ((= var-num 0) (stack-pop!))
   ((<= var-num 15)
    (if (> var-num *num-locals*)
        (error "Access to nonexistent local variable" var-num)
        (vector-ref *locals* (- var-num 1))))
   (else
    (mem-word (+ *globals-addr* (* 2 (- var-num 16)))))))

(define (set-variable! var-num val)
  (let ((v (u16 val)))
    (cond
     ((= var-num 0) (stack-push! v))
     ((<= var-num 15)
      (if (> var-num *num-locals*)
          (error "Write to nonexistent local variable" var-num)
          (vector-set! *locals* (- var-num 1) v)))
     (else
      (mem-set-word! (+ *globals-addr* (* 2 (- var-num 16))) v)))))

;; Indirect variable access (for inc, dec, load, store, pull opcodes)
;; When var-num is 0, reads/writes top of stack in-place rather than push/pop
(define (get-variable-indirect var-num)
  (if (= var-num 0)
      (stack-top)
      (get-variable var-num)))

(define (set-variable-indirect! var-num val)
  (let ((v (u16 val)))
    (if (= var-num 0)
        ;; Overwrite top of stack
        (if (null? *stack*)
            (stack-push! v)
            (set-car! *stack* v))
        (set-variable! var-num v))))

;;;
;;; Call frames
;;; Each frame: #(return-pc result-var locals num-locals saved-stack saved-arg-count)
;;;

(define (push-frame! return-pc result-var locals num-locals saved-stack saved-arg-count)
  (set! *call-stack*
        (cons (vector return-pc result-var *locals* *num-locals* saved-stack saved-arg-count)
              *call-stack*))
  (set! *locals* locals)
  (set! *num-locals* num-locals))

(define (pop-frame!)
  ;; Returns (return-pc . result-var)
  (if (null? *call-stack*)
      (error "Cannot return from top-level routine"))
  (let ((frame (car *call-stack*)))
    (set! *call-stack* (cdr *call-stack*))
    (set! *locals* (vector-ref frame 2))
    (set! *num-locals* (vector-ref frame 3))
    (set! *stack* (vector-ref frame 4))
    (cons (vector-ref frame 0) (vector-ref frame 1))))

;;;
;;; Packed address conversion
;;;

(define (unpack-addr packed-addr)
  (case *version*
    ((1 2 3) (* 2 packed-addr))
    ((4 5) (* 4 packed-addr))
    ((8) (* 8 packed-addr))
    (else (* 2 packed-addr))))

;;;
;;; Header parsing
;;;

(define (parse-header!)
  (set! *version* (mem-byte 0))
  (set! *high-mem-base* (mem-word #x04))
  (set! *initial-pc* (mem-word #x06))
  (set! *dictionary-addr* (mem-word #x08))
  (set! *object-table-addr* (mem-word #x0A))
  (set! *globals-addr* (mem-word #x0C))
  (set! *static-mem-base* (mem-word #x0E))
  (set! *abbrev-table-addr* (mem-word #x18)))

(define (setup-header-flags!)
  ;; Set interpreter-provided header fields
  (when (= *version* 3)
    ;; Signal that we support screen splitting
    (let ((flags1 (mem-byte 1)))
      (mem-set-byte! 1 (bitwise-ior (bitwise-and flags1 #b10111111) #b00100000))))
  (when (>= *version* 4)
    ;; Flags 1 for V4+
    (let ((flags1 (mem-byte 1)))
      ;; Set bit 0 (colors), bit 2 (bold), bit 3 (italic), bit 4 (fixed), bit 7 (timed input)
      (mem-set-byte! 1 (bitwise-ior flags1 #b10011101)))
    ;; Interpreter number 6 (IBM PC) and version 'Z' (ASCII 90)
    (mem-set-byte! #x1E 6)
    (mem-set-byte! #x1F 90)
    (mem-set-byte! #x20 25)    ; screen height (lines)
    (mem-set-byte! #x21 80))   ; screen width (characters)
  (when (>= *version* 5)
    ;; Screen dimensions in units
    (mem-set-word! #x22 80)    ; screen width in units
    (mem-set-word! #x24 25)    ; screen height in units
    (mem-set-byte! #x26 1)     ; font width in units
    (mem-set-byte! #x27 1)     ; font height in units
    ;; Default colors: white on black
    (mem-set-byte! #x2C 2)     ; default background = black
    (mem-set-byte! #x2D 9)     ; default foreground = white
    ;; Clear bits in flags 2 for unsupported features
    (let ((flags2 (mem-word #x10)))
      ;; Clear bit 3 (pictures), bit 5 (mouse), bit 7 (sound), bit 8 (menus)
      (mem-set-word! #x10 (bitwise-and flags2 (bitwise-not #b101101000)))))
  ;; Standard revision 1.1
  (mem-set-byte! #x32 1)
  (mem-set-byte! #x33 1))

;;;
;;; Story file loading
;;;

(define (load-story-file filename)
  (call-with-binary-input-file filename
    (lambda (port)
      ;; Read entire file
      (let* ((all-bytes (let loop ((chunks '()))
                          (let ((chunk (read-bytevector 4096 port)))
                            (if (eof-object? chunk)
                                (apply bytevector-append (reverse chunks))
                                (loop (cons chunk chunks)))))))
        (set! *memory* all-bytes)
        (set! *memory-size* (bytevector-length all-bytes)))))
  (parse-header!)
  (setup-header-flags!)
  ;; Reset machine state
  (set! *pc* *initial-pc*)
  (set! *stack* '())
  (set! *call-stack* '())
  (set! *locals* (make-vector 15 0))
  (set! *num-locals* 0)
  (set! *running* #t))

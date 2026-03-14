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
;;; Save/Restore
;;;

;; Binary I/O helpers for save files
(define (write-u8! port val)
  (write-u8 (bitwise-and val #xFF) port))

(define (write-u16! port val)
  (write-u8! port (arithmetic-shift val -8))
  (write-u8! port (bitwise-and val #xFF)))

(define (write-u32! port val)
  (write-u8! port (bitwise-and (arithmetic-shift val -24) #xFF))
  (write-u8! port (bitwise-and (arithmetic-shift val -16) #xFF))
  (write-u8! port (bitwise-and (arithmetic-shift val -8) #xFF))
  (write-u8! port (bitwise-and val #xFF)))

(define (read-u8! port)
  (read-u8 port))

(define (read-u16! port)
  (let* ((hi (read-u8! port))
         (lo (read-u8! port)))
    (bitwise-ior (arithmetic-shift hi 8) lo)))

(define (read-u32! port)
  (let* ((b3 (read-u8! port))
         (b2 (read-u8! port))
         (b1 (read-u8! port))
         (b0 (read-u8! port)))
    (bitwise-ior (arithmetic-shift b3 24)
                 (arithmetic-shift b2 16)
                 (arithmetic-shift b1 8)
                 b0)))

(define *save-magic* '(90 83 65 86))  ; "ZSAV"
(define *save-format-version* 1)

(define (prompt-save-filename)
  (display "Save filename [save.zsav]: ")
  (flush-output-port (current-output-port))
  (let ((line (read-line (current-input-port))))
    (if (or (eof-object? line) (string=? line ""))
        "save.zsav"
        line)))

(define (prompt-restore-filename)
  (display "Restore filename [save.zsav]: ")
  (flush-output-port (current-output-port))
  (let ((line (read-line (current-input-port))))
    (if (or (eof-object? line) (string=? line ""))
        "save.zsav"
        line)))

(define (do-save-game saved-pc)
  ;; Save game state to file. saved-pc is the PC to resume at on restore.
  ;; Returns #t on success, #f on failure.
  (let ((filename (prompt-save-filename)))
    (guard (exn (#t #f))
      (call-with-binary-output-file filename
        (lambda (port)
          ;; Magic
          (for-each (lambda (b) (write-u8! port b)) *save-magic*)
          ;; Format version
          (write-u8! port *save-format-version*)
          ;; Z-machine version
          (write-u8! port *version*)
          ;; Static memory base and saved PC
          (write-u32! port *static-mem-base*)
          (write-u32! port saved-pc)
          ;; Dynamic memory
          (write-bytevector *memory* port 0 *static-mem-base*)
          ;; Current eval stack
          (write-u32! port (length *stack*))
          (for-each (lambda (v) (write-u16! port v)) *stack*)
          ;; Current frame: locals, num-locals, arg-count
          (write-u8! port *num-locals*)
          (write-u8! port *current-arg-count*)
          (let loop ((i 0))
            (when (< i 15)
              (write-u16! port (vector-ref *locals* i))
              (loop (+ i 1))))
          ;; Call stack frames
          (write-u32! port (length *call-stack*))
          (for-each
           (lambda (frame)
             (let ((return-pc (vector-ref frame 0))
                   (result-var (vector-ref frame 1))
                   (saved-locals (vector-ref frame 2))
                   (saved-num-locals (vector-ref frame 3))
                   (saved-stack (vector-ref frame 4))
                   (saved-arg-count (vector-ref frame 5)))
               ;; return-pc
               (write-u32! port return-pc)
               ;; result-var: #f encoded as #xFFFF, else the var number
               (write-u16! port (if result-var result-var #xFFFF))
               ;; locals
               (write-u8! port saved-num-locals)
               (write-u8! port saved-arg-count)
               (let loop ((i 0))
                 (when (< i 15)
                   (write-u16! port (vector-ref saved-locals i))
                   (loop (+ i 1))))
               ;; saved stack
               (write-u32! port (length saved-stack))
               (for-each (lambda (v) (write-u16! port v)) saved-stack)))
           *call-stack*)))
      #t)))

(define (do-restore-game)
  ;; Restore game state from file.
  ;; Returns #t on success (state has been restored), #f on failure.
  (let ((filename (prompt-restore-filename)))
    (guard (exn (#t #f))
      (call-with-binary-input-file filename
        (lambda (port)
          ;; Verify magic
          (for-each
           (lambda (expected)
             (let ((got (read-u8! port)))
               (when (not (= got expected))
                 (error "Not a valid save file"))))
           *save-magic*)
          ;; Format version
          (let ((fmt (read-u8! port)))
            (when (not (= fmt *save-format-version*))
              (error "Unsupported save format version" fmt)))
          ;; Z-machine version
          (let ((ver (read-u8! port)))
            (when (not (= ver *version*))
              (error "Save file is for a different Z-machine version" ver)))
          ;; Static memory base and saved PC
          (let* ((saved-static-base (read-u32! port))
                 (saved-pc (read-u32! port)))
            (when (not (= saved-static-base *static-mem-base*))
              (error "Save file has different static memory base"))
            ;; Restore dynamic memory
            (let ((mem-data (read-bytevector *static-mem-base* port)))
              (bytevector-copy! *memory* 0 mem-data))
            ;; Re-apply interpreter header flags (they live in dynamic memory)
            (setup-header-flags!)
            ;; Restore eval stack
            (let ((stack-depth (read-u32! port)))
              (set! *stack*
                    (let loop ((i 0) (acc '()))
                      (if (= i stack-depth)
                          (reverse acc)
                          (loop (+ i 1) (cons (read-u16! port) acc))))))
            ;; Restore current frame
            (set! *num-locals* (read-u8! port))
            (set! *current-arg-count* (read-u8! port))
            (set! *locals* (make-vector 15 0))
            (let loop ((i 0))
              (when (< i 15)
                (vector-set! *locals* i (read-u16! port))
                (loop (+ i 1))))
            ;; Restore call stack
            (let ((num-frames (read-u32! port)))
              (set! *call-stack*
                    (let loop ((i 0) (acc '()))
                      (if (= i num-frames)
                          (reverse acc)
                          (let* ((return-pc (read-u32! port))
                                 (result-var-raw (read-u16! port))
                                 (result-var (if (= result-var-raw #xFFFF) #f result-var-raw))
                                 (num-locals (read-u8! port))
                                 (arg-count (read-u8! port))
                                 (locals (make-vector 15 0))
                                 (_ (let lp ((j 0))
                                      (when (< j 15)
                                        (vector-set! locals j (read-u16! port))
                                        (lp (+ j 1)))))
                                 (stack-depth (read-u32! port))
                                 (stk (let lp ((j 0) (a '()))
                                        (if (= j stack-depth)
                                            (reverse a)
                                            (lp (+ j 1) (cons (read-u16! port) a))))))
                            (loop (+ i 1)
                                  (cons (vector return-pc result-var locals num-locals stk arg-count)
                                        acc)))))))
            ;; Set PC to saved position
            (set! *pc* saved-pc)
            #t))))))

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

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
(define *original-dynamic-memory* #f) ; copy of dynamic memory at load time
(define *undo-state* #f)       ; bytevector holding undo snapshot, or #f

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
      ;; Set bit 0 (colors), bit 2 (bold), bit 3 (italic), bit 4 (fixed)
      (mem-set-byte! 1 (bitwise-ior flags1 #b00011101)))
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
    ;; Clear bits in Flags 2 for unsupported features. We preserve the
    ;; game's requests for transcription (bit 0), fixed-pitch font
    ;; (bit 1) and undo (bit 4), since all three are actually provided.
    ;; Cleared: bit 3 (pictures/font3), bit 5 (mouse), bit 7 (sound),
    ;; bit 8 (menus).
    (let ((flags2 (mem-word #x10)))
      (mem-set-word! #x10 (bitwise-and flags2 (bitwise-not #b110101000)))))
  ;; Do not claim full Standard 1.1 compliance.
  (mem-set-byte! #x32 0)
  (mem-set-byte! #x33 0))

(define (flags2-preserved-bits)
  (bitwise-and (mem-word #x10) #b11))

(define (restore-flags2-preserved-bits! preserved-bits)
  (mem-set-word! #x10
                 (bitwise-ior (bitwise-and (mem-word #x10) (bitwise-not #b11))
                              (bitwise-and preserved-bits #b11))))

(define (story-file-length)
  (let* ((scale (case *version*
                  ((1 2 3) 2)
                  ((4 5) 4)
                  (else 8)))
         (header-length (* scale (mem-word #x1A))))
    (if (= header-length 0)
        *memory-size*
        (min header-length *memory-size*))))

(define (story-checksum-matches?)
  (if (< *version* 3)
      #t
      (let ((expected (mem-word #x1C))
            (file-length (story-file-length)))
        (if (or (= expected 0) (<= file-length #x40))
            #t
            (let loop ((addr #x40) (sum 0))
              (if (>= addr file-length)
                  (= (bitwise-and sum #xFFFF) expected)
                  (loop (+ addr 1) (+ sum (mem-byte addr)))))))))

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

;;;
;;; Quetzal save format ("IFZS" IFF container)
;;;
;;; Files are IFF FORM/IFZS containers holding three chunks:
;;;   IFhd -- release number, serial number, checksum and the resume PC
;;;           stored as 3 bytes; identifies the owning story file
;;;   CMem -- dynamic memory XORed against the pristine story image,
;;;           run-length encoded (zero byte, n encodes n+1 unchanged
;;;           bytes); a short decode implies trailing unchanged bytes
;;;   Stks -- call frames oldest first, preceded by a mandatory dummy
;;;           frame carrying any top-level evaluation stack
;;; Reference: "Quetzal Z-machine Common Save-File Format Standard" 1.4.

(define (quetzal-chunk id payload)
  ;; Build a complete IFF chunk: 4-char ID, big-endian length, payload,
  ;; plus one pad byte when the payload length is odd.
  (let ((port (open-output-bytevector)))
    (for-each (lambda (c) (write-u8! port (char->integer c))) (string->list id))
    (write-u32! port (bytevector-length payload))
    (write-bytevector payload port 0 (bytevector-length payload))
    (when (= 1 (modulo (bytevector-length payload) 2)) (write-u8! port 0))
    (get-output-bytevector port)))

;; One Quetzal stack frame -> bytes.
;;   ret-pc: resume address in the caller; res-var: result variable
;;   number or #f when the call discards its result; args: count of
;;   arguments supplied; locs/nlocs: locals vector and count; stk:
;;   evaluation stack, most recent first.
(define (quetzal-frame ret-pc res-var args locs nlocs stk)
  (let ((port (open-output-bytevector)))
    ;; Long: three bytes of return PC then flags 000pLLLL, where p is
    ;; set for result-discarding calls and LLLL is the local count.
    (write-u32! port (+ (* ret-pc 256)
                        nlocs
                        (if res-var 0 16)))
    (write-u8! port (if res-var res-var 0))
    ;; Argument-supplied bitmap (zero means no arguments).
    (write-u8! port (if (= args 0) 0 (- (arithmetic-shift 1 args) 1)))
    (write-u16! port (length stk))
    (let loop ((i 0))
      (when (< i nlocs)
        (write-u16! port (vector-ref locs i))
        (loop (+ i 1))))
    (for-each (lambda (w) (write-u16! port w)) (reverse stk)) ; least recent first
    (get-output-bytevector port)))

;; Stks chunk body: dummy frame followed by real frames, oldest first.
;;
;; Orientation note: our frames snapshot the CALLER of each call (its
;; locals, eval stack and argument count, plus return PC and result
;; variable), while a Quetzal frame describes the CALLED routine. A
;; Quetzal frame therefore takes its return PC and result variable from
;; our matching frame, but its locals / eval stack / argument count from
;; the NEXT-NEWER snapshot -- or from live machine state for the
;; innermost routine. The dummy frame holds main's eval stack at the
;; moment it made the first call (or the current stack if none).
(define (quetzal-stks)
  (let ((port (open-output-bytevector))
        (frames *call-stack*))                     ; newest first
    (let loop ((i 0))
      (when (< i 6) (write-u8! port 0) (loop (+ i 1))))
    (let ((top-stack (if (null? frames)
                         *stack*
                         (vector-ref (car (reverse frames)) 4))))
      (write-u16! port (length top-stack))
      (for-each (lambda (w) (write-u16! port w)) (reverse top-stack)))
    (let walk ((seq (reverse frames)))               ; oldest first
      (when (pair? seq)
        (let* ((f (car seq))
               (body (if (pair? (cdr seq))
                         (cadr seq)                  ; next-newer snapshot
                         ;; innermost routine: live state
                         (vector #f #f *locals* *num-locals*
                                 *stack* *current-arg-count*)))
               (payload (quetzal-frame (vector-ref f 0)     ; return PC
                                       (vector-ref f 1)     ; result var
                                       (vector-ref body 5)  ; args
                                       (vector-ref body 2)  ; locals
                                       (vector-ref body 3)  ; local count
                                       (vector-ref body 4)))) ; eval stack
          (write-bytevector payload port 0 (bytevector-length payload))
          (walk (cdr seq)))))
    (get-output-bytevector port)))

;; CMem chunk body: dynamic-memory delta against the pristine story
;; image, run-length encoded. Trailing runs are omitted on purpose --
;; readers reconstruct them from the original story file (spec 3.4).
(define (quetzal-cmem)
  (let ((port (open-output-bytevector)))
    (let loop ((i 0) (run 0))
      (if (< i *static-mem-base*)
          (let ((delta (bitwise-xor (mem-byte i)
                                    (bytevector-u8-ref *original-dynamic-memory* i))))
            (if (= delta 0)
                (loop (+ i 1) (+ run 1))
                (begin
                  (let emit ((r run))
                    (when (> r 0)
                      (let ((n (min r 256)))
                        (write-u8! port 0)
                        (write-u8! port (- n 1))
                        (emit (- r n)))))
                  (write-u8! port delta)
                  (loop (+ i 1) 0))))
          (get-output-bytevector port)))))

;; IFhd chunk body: release, serial, checksum and resume PC. Per spec
;; 5.8 the saved PC points at the branch bytes (V1-3) or the store byte
;; (V4+) of the save instruction -- exactly where our handlers already
;; park *pc* before saving.
(define (quetzal-ifhd saved-pc)
  (let ((port (open-output-bytevector)))
    (write-u16! port (mem-word #x02))
    (let loop ((i 0))
      (when (< i 6)
        (write-u8! port (mem-byte (+ #x12 i)))
        (loop (+ i 1))))
    (write-u16! port (mem-word #x1C))
    (write-u8! port (bitwise-and (arithmetic-shift saved-pc -16) #xFF))
    (write-u8! port (bitwise-and (arithmetic-shift saved-pc -8) #xFF))
    (write-u8! port (bitwise-and saved-pc #xFF))
    (get-output-bytevector port)))

(define (sanitize-filename raw)
  ;; Per Z-machine spec S 7.6.1.1 and S 7.6.1.3:
  ;; 1. Convert to uppercase
  ;; 2. Strip illegal characters (slash, backslash, angle brackets,
  ;;    colon, double-quote, pipe, question-mark, asterisk)
  ;; 3. Truncate at first dot (delete dot and everything after)
  ;; 4. If result is empty, use "NULL"
  (let* ((upper (string-upcase raw))
         (stripped (let loop ((i 0) (acc '()))
                     (if (>= i (string-length upper))
                         (list->string (reverse acc))
                         (let ((ch (string-ref upper i)))
                           (cond
                            ((char=? ch #\.)
                             (list->string (reverse acc)))
                            ((member ch '(#\/ #\\ #\< #\> #\: #\" #\| #\? #\*))
                             (loop (+ i 1) acc))
                            (else
                             (loop (+ i 1) (cons ch acc))))))))
         (final (if (string=? stripped "") "NULL" stripped)))
    (let ((dot-pos (string-find-next-char final #\.)))
      (if dot-pos (substring final 0 dot-pos) final))))

(define (prompt-save-filename)
  (display "Save filename [save.zsav]: ")
  (flush-output-port (current-output-port))
  (let ((line (read-line *game-input-port*)))
    (if (or (eof-object? line) (string=? line ""))
        "save.zsav"
        (sanitize-filename line))))

(define (prompt-restore-filename)
  (display "Restore filename [save.zsav]: ")
  (flush-output-port (current-output-port))
  (let ((line (read-line *game-input-port*)))
    (if (or (eof-object? line) (string=? line ""))
        "save.zsav"
        (sanitize-filename line))))

(define (do-save-game saved-pc)
  ;; Save game state to file. saved-pc is the PC to resume at on restore.
  ;; Returns #t on success, #f on failure.
  (let ((filename (prompt-save-filename)))
    (guard (exn (#t #f))
      (call-with-binary-output-file filename
        (lambda (port)
          (save-state-to-port port saved-pc)))
      #t)))

(define (do-restore-game)
  ;; Restore game state from file.
  ;; Returns #t on success (state has been restored), #f on failure.
  (let ((filename (prompt-restore-filename)))
    (guard (exn (#t #f))
      (call-with-binary-input-file filename
        (lambda (port)
          (restore-state-from-port port))))))

;;;
;;; Undo (in-memory save/restore)
;;;

(define (save-state-to-port port saved-pc)
  ;; Serialize game state to a binary port as a Quetzal IFZS file
  ;; (shared by save and undo).
  (let ((out (open-output-bytevector)))
    (for-each (lambda (c) (write-u8! out (char->integer c))) (string->list "FORM"))
    (let* ((ifhd (quetzal-chunk "IFhd" (quetzal-ifhd saved-pc)))
           (cmem (quetzal-chunk "CMem" (quetzal-cmem)))
           (stks (quetzal-chunk "Stks" (quetzal-stks)))
           ;; FORM length counts the form type plus all chunk bytes.
           (form-len (+ 4 (bytevector-length ifhd)
                           (bytevector-length cmem)
                           (bytevector-length stks))))
      (write-u32! out form-len)
      (for-each (lambda (c) (write-u8! out (char->integer c))) (string->list "IFZS"))
      (for-each (lambda (bv) (write-bytevector bv out 0 (bytevector-length bv)))
                (list ifhd cmem stks)))
    (let ((bytes (get-output-bytevector out)))
      (write-bytevector bytes port 0 (bytevector-length bytes)))))

;; --- restore-side bytevector parsing helpers ---

(define (slurp-port port)
  (let loop ((chunks '()))
    (let ((chunk (read-bytevector 4096 port)))
      (if (eof-object? chunk)
          (apply bytevector-append (reverse chunks))
          (loop (cons chunk chunks))))))

(define (slice-bv bv start len)
  (let ((out (make-bytevector len)))
    (bytevector-copy! out 0 bv start (+ start len))
    out))

(define (bv-u16 bv i)
  (bitwise-ior (* 256 (bytevector-u8-ref bv i))
               (bytevector-u8-ref bv (+ i 1))))

(define (bv-u24 bv i)
  (+ (* 65536 (bytevector-u8-ref bv i))
     (* 256 (bytevector-u8-ref bv (+ i 1)))
     (bytevector-u8-ref bv (+ i 2))))

(define (bv-u32 bv i)
  (+ (* 16777216 (bytevector-u8-ref bv i))
     (* 65536 (bytevector-u8-ref bv (+ i 1)))
     (* 256 (bytevector-u8-ref bv (+ i 2)))
     (bytevector-u8-ref bv (+ i 3))))

(define (ascii-at? bv i s)
  (let loop ((k 0))
    (or (= k (string-length s))
        (and (= (bytevector-u8-ref bv (+ i k))
                (char->integer (string-ref s k)))
             (loop (+ k 1))))))

;; Decode a CMem payload against the pristine story image. Returns a
;; fresh bytevector of exactly *static-mem-base* bytes.
(define (decode-cmem data)
  (when (> (bytevector-length data) *static-mem-base*)
    (error "CMem chunk overruns dynamic memory"))
  (let ((out (bytevector-copy *original-dynamic-memory*)))
    (let loop ((i 0) (pos 0))
      (if (< pos (bytevector-length data))
          (let ((b (bytevector-u8-ref data pos)))
            (if (= b 0)
                (begin
                  (when (>= (+ pos 1) (bytevector-length data))
                    (error "CMem chunk ends mid-run"))
                  (let ((run (+ 1 (bytevector-u8-ref data (+ pos 1)))))
                    (when (> (+ i run) *static-mem-base*)
                      (error "CMem chunk overruns dynamic memory"))
                    ;; Unchanged bytes: `out' already holds the pristine
                    ;; image there, so just skip over them.
                    (loop (+ i run) (+ pos 2))))
                (begin
                  (when (>= i *static-mem-base*)
                    (error "CMem chunk overruns dynamic memory"))
                  (bytevector-u8-set! out i
                                      (bitwise-xor b
                                                   (bytevector-u8-ref *original-dynamic-memory* i)))
                  (loop (+ i 1) (+ pos 1)))))
          out))))

;; Parse an Stks payload. Returns two values as a cons:
;;   (q-frames-oldest-first . top-level-stack)
;; where each q-frame is #(ret-pc res-var args locs nlocs stk).
(define (parse-stks data)
  (define (u16at i) (bv-u16 data i))
  (when (< (bytevector-length data) 8) (error "Stks chunk truncated"))
  ;; Dummy frame: six zero bytes, then the top-level stack depth.
  (let check ((i 0))
    (when (< i 6)
      (unless (= 0 (bytevector-u8-ref data i)) (error "Bad dummy stack frame"))
      (check (+ i 1))))
  (let ((top-depth (u16at 6)))
    (when (> (+ 8 (* 2 top-depth)) (bytevector-length data))
      (error "Stks chunk truncated in dummy frame"))
    (let* ((top-stack
            (let pull ((i 0) (acc '()))
              (if (= i top-depth)
                  (reverse acc)                    ; head = most recent
                  (pull (+ i 1)
                        (cons (u16at (+ 8 (* 2 i))) acc)))))
           (start (+ 8 (* 2 top-depth))))
      (cons
       (let loop ((pos start) (qs '()))
         (if (>= pos (bytevector-length data))
             (reverse qs)                          ; oldest first
             (let* ((ret-pc (bv-u24 data pos))
                    (flags (bytevector-u8-ref data (+ pos 3)))
                    (nlocs (bitwise-and flags 15))
                    (res-var (if (= 0 (bitwise-and flags 16))
                                 (bytevector-u8-ref data (+ pos 4))
                                 #f))
                    (args-byte (bytevector-u8-ref data (+ pos 5)))
                    (nstk (u16at (+ pos 6)))
                    (base (+ pos 8)))
               (when (> (+ base (* 2 (+ nlocs nstk)))
                        (bytevector-length data))
                 (error "Stks frame truncated"))
               ;; Args bitmap must be (2^count - 1): bits filled from bit 0.
               (let argcheck ((x (+ args-byte 1)) (count 0))
                 (cond ((= x 1)
                        (let* ((locs (make-vector 15 0)))
                          (let lp ((j 0))
                            (when (< j nlocs)
                              (vector-set! locs j (u16at (+ base (* 2 j))))
                              (lp (+ j 1))))
                          (let ((stk
                                 (let pull ((i 0) (acc '()))
                                   (if (= i nstk)
                                       (reverse acc)
                                       (pull (+ i 1)
                                             (cons (u16at (+ base (* 2 (+ nlocs i))))
                                                   acc))))))
                            (loop (+ base (* 2 (+ nlocs nstk)))
                                  (cons (vector ret-pc res-var count locs nlocs stk)
                                        qs)))))
                       ((odd? x) (error "Unsupported argument mask in save file"))
                       (else (argcheck (/ x 2) (+ count 1))))))))
       top-stack))))

(define (restore-state-from-port port)
  ;; Deserialize a Quetzal IFZS file from a binary port (shared by
  ;; restore and undo). Returns #t on success, raises on failure.
  ;; Everything is parsed into local variables FIRST and only committed
  ;; to machine state at the very end, so a corrupt/truncated file or a
  ;; save from a different story leaves the interpreter untouched.
  (apply-quetzal! (slurp-port port)))

(define (apply-quetzal! data)
  (define len (bytevector-length data))
  (when (< len 12) (error "Not a Quetzal save file"))
  (unless (ascii-at? data 0 "FORM") (error "Not an IFF file"))
  (unless (ascii-at? data 8 "IFZS") (error "Not a Quetzal save file"))
  (unless (= (bv-u32 data 4) (- len 8))
    (error "Quetzal container length mismatch"))
  (let scan ((pos 12)
             (ifhd #f) (mem-data #f) (stks-data #f)
             (seen-mem #f) (seen-stks #f))
    (if (>= pos len)
        (finish-quetzal! ifhd mem-data stks-data)
        (let* ((chunk-len (bv-u32 data (+ pos 4)))
               (body-start (+ pos 8))
               (body-end (+ body-start chunk-len))
               (next (if (= 1 (modulo chunk-len 2)) (+ body-end 1) body-end)))
          (when (> (+ pos 8) len) (error "Truncated chunk header"))
          (when (> body-end len) (error "Chunk overruns file"))
          (cond
           ((and (= pos 12) (ascii-at? data pos "IFhd"))
            ;; IFhd must be the first chunk (spec 5.4).
            (scan next (slice-bv data body-start chunk-len)
                  mem-data stks-data seen-mem seen-stks))
           ((ascii-at? data pos "IFhd")
            ;; Duplicate IFhd: skip it (spec 8.8/8.9).
            (scan next ifhd mem-data stks-data seen-mem seen-stks))
           ((and (not seen-mem) (ascii-at? data pos "UMem"))
            (unless (= chunk-len *static-mem-base*)
              (error "UMem chunk has wrong size"))
            (scan next ifhd (slice-bv data body-start chunk-len)
                  stks-data #t seen-stks))
           ((and (not seen-mem) (ascii-at? data pos "CMem"))
            (scan next ifhd (decode-cmem (slice-bv data body-start chunk-len))
                  stks-data #t seen-stks))
           ((and (not seen-stks) (ascii-at? data pos "Stks"))
            (scan next ifhd mem-data
                  (slice-bv data body-start chunk-len) seen-mem #t))
           ;; Unknown or duplicate chunk: skip it (spec 8.9).
           (else
            (scan next ifhd mem-data stks-data seen-mem seen-stks)))))))

(define (finish-quetzal! ifhd mem-data stks-data)
  (unless ifhd (error "Save file has no IFhd chunk"))
  (unless mem-data (error "Save file has no memory chunk"))
  (unless stks-data (error "Save file has no Stks chunk"))
  (when (< (bytevector-length ifhd) 13) (error "IFhd chunk too small"))
  ;; Story identity: release, serial, checksum against this story's header.
  (unless (and (= (bv-u16 ifhd 0) (mem-word #x02))
               (let serial-ok? ((k 0))
                 (or (= k 6)
                     (and (= (bytevector-u8-ref ifhd (+ 2 k))
                             (mem-byte (+ #x12 k)))
                          (serial-ok? (+ k 1)))))
               (= (bv-u16 ifhd 8) (mem-word #x1C)))
    (error "Save file was not made from this story"))
  (let* ((saved-pc (bv-u24 ifhd 10))
         (parsed (parse-stks stks-data)))
    (commit-quetzal! saved-pc mem-data (car parsed) (cdr parsed))))

(define (commit-quetzal! saved-pc mem-data qs top-stack)
  ;; qs: Quetzal frames oldest first. Invert the caller-snapshot mapping:
  ;; our frame for routine j takes return PC / result var from q_j, and
  ;; locals / eval stack / argument count from q_{j-1} (dummy context for
  ;; the oldest -- main's locals are dead data). The newest q is the live
  ;; routine and becomes current machine state.
  (define dummy-src (vector 0 0 0 (make-vector 15 0) 0 '()))
  (let build ((todo qs)                       ; oldest -> newest
              (prev #f)                       ; previous q (= caller context)
              (acc '()))                      ; our frames, newest first
    (if (null? todo)
        (let* ((live (or prev dummy-src))
               (preserved (flags2-preserved-bits)))   ; before clobbering memory
          (bytevector-copy! *memory* 0 mem-data)
          (setup-header-flags!)
          (restore-flags2-preserved-bits! preserved)
          (set! *stack* top-stack)
          (set! *locals* (vector-ref live 3))
          (set! *num-locals* (vector-ref live 4))
          (set! *current-arg-count* (vector-ref live 2))
          (set! *call-stack* acc)
          (set! *pc* saved-pc)
          #t)
        (let* ((q (car todo))
               (src (or prev dummy-src))
               (frame (vector (vector-ref q 0)      ; return PC
                              (vector-ref q 1)      ; result var
                              (vector-ref src 3)    ; caller locals
                              (vector-ref src 4)    ; caller local count
                              (vector-ref src 5)    ; caller eval stack
                              (vector-ref src 2)))) ; caller arg count
          (build (cdr todo) q (cons frame acc))))))
(define (do-save-undo saved-pc)
  ;; Save game state to in-memory undo buffer. Returns #t on success.
  (guard (exn (#t #f))
    (let ((port (open-output-bytevector)))
      (save-state-to-port port saved-pc)
      (set! *undo-state* (get-output-bytevector port))
      #t)))

(define (do-restore-undo)
  ;; Restore game state from undo buffer. Returns #t on success.
  (if (not *undo-state*)
      #f
      (guard (exn (#t #f))
        (let ((port (open-input-bytevector *undo-state*)))
          (restore-state-from-port port)))))

;;;
;;; Restart
;;;

(define (do-restart!)
  ;; Restore dynamic memory to original state and reset machine.
  (let ((preserved-flags2 (flags2-preserved-bits)))
  (bytevector-copy! *memory* 0 *original-dynamic-memory*)
  (setup-header-flags!)
  (restore-flags2-preserved-bits! preserved-flags2)
  (set! *pc* *initial-pc*)
  (set! *stack* '())
  (set! *call-stack* '())
  (set! *locals* (make-vector 15 0))
  (set! *num-locals* 0)
  (set! *current-arg-count* 0)
  (set! *undo-state* #f)
  ;; Reset I/O and RNG state (defined in ops.scm, available at runtime)
  (set! *current-window* 0)
  (set! *upper-window-height* 0)
  (set! *output-buffer* '())
  (set! *stream2-active?* #f)
  (set! *stream2-warning-shown?* #f)
  (set! *stream3-stack* '())
  (set! *input-stream* 0)
  (set! *current-style* 0)
  (set! *win1-row* 1)
  (set! *win1-col* 1)
  (when (not (null? *transcript-port*))
    (guard (exn (#t 'ok)) (close-output-port *transcript-port*))
    (set! *transcript-port* '()))
  (set! *rng-a* 1)
  (set! *rng-interval* 0)
  (set! *rng-counter* 0)))

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
  ;; Save original dynamic memory before interpreter modifies header
  (set! *original-dynamic-memory* (bytevector-copy *memory* 0 *static-mem-base*))
  (setup-header-flags!)
  ;; Reset machine state
  (set! *pc* *initial-pc*)
  (set! *stack* '())
  (set! *call-stack* '())
  (set! *locals* (make-vector 15 0))
  (set! *num-locals* 0)
  (set! *current-arg-count* 0)
  (set! *undo-state* #f)
  (set! *running* #t))

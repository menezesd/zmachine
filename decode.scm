;;; decode.scm -- Instruction decoding and Z-character text encoding/decoding

;;;
;;; Text decoding: Z-characters to ZSCII to Scheme strings
;;;

;; Alphabet tables for V2+ (V1 differs slightly but we skip it)
(define *a0* "abcdefghijklmnopqrstuvwxyz")   ; Z-chars 6-31
(define *a1* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")   ; Z-chars 6-31
(define *a2* " \n0123456789.,!?_#'\"/\\-:()")  ; Z-chars 6-31
;; Note: A2 char 6 = ZSCII escape (handled specially), char 7 = newline

;; Unpack a 2-byte word into three 5-bit Z-characters
;; Returns (list z1 z2 z3 end-bit?)
(define (unpack-zword word)
  (let ((end? (bit-set? 15 word))
        (z1 (bitwise-and (arithmetic-shift word -10) #x1F))
        (z2 (bitwise-and (arithmetic-shift word -5) #x1F))
        (z3 (bitwise-and word #x1F)))
    (list z1 z2 z3 end?)))

;; Decode a Z-encoded string starting at byte address addr
;; Returns (cons decoded-string bytes-consumed)
(define (decode-zstring addr)
  ;; Collect all Z-characters first
  (let loop ((offset 0) (zchars '()))
    (let* ((word (mem-word (+ addr offset)))
           (unpacked (unpack-zword word))
           (z1 (car unpacked))
           (z2 (cadr unpacked))
           (z3 (caddr unpacked))
           (end? (cadddr unpacked))
           (new-zchars (append zchars (list z1 z2 z3))))
      (if end?
          (cons (zchars->string new-zchars) (+ offset 2))
          (loop (+ offset 2) new-zchars)))))

;; Convert a list of Z-characters to a string
(define (zchars->string zchars)
  (let ((result '())
        (alphabet 0)         ; 0=A0, 1=A1, 2=A2
        (abbrev-pending #f)  ; #f or the abbreviation bank (0,1,2)
        (tenbit-stage #f)    ; #f, 'high, or a number (the high 5 bits)
        )
    (define (emit-char ch)
      (set! result (cons ch result)))
    (define (translate-zchar z)
      (cond
       ;; Ten-bit ZSCII assembly
       (tenbit-stage
        (cond
         ((eq? tenbit-stage 'high)
          (set! tenbit-stage z))  ; save high 5 bits
         (else
          ;; tenbit-stage has high 5 bits, z has low 5 bits
          (let ((code (bitwise-ior (arithmetic-shift tenbit-stage 5) z)))
            (when (and (>= code 32) (<= code 126))
              (emit-char (integer->char code))))
          (set! tenbit-stage #f))))

       ;; Abbreviation pending
       (abbrev-pending
        (let* ((entry (+ (* 32 abbrev-pending) z))
               (abbrev-addr (* 2 (mem-word (+ *abbrev-table-addr* (* 2 entry)))))
               (decoded (car (decode-zstring abbrev-addr))))
          (for-each (lambda (c) (emit-char c)) (string->list decoded)))
        (set! abbrev-pending #f))

       ;; Z-char 0: space
       ((= z 0)
        (emit-char #\space))

       ;; Z-chars 1,2,3: abbreviations (V3+)
       ((and (>= *version* 3) (<= z 3))
        (set! abbrev-pending (- z 1)))

       ;; Z-chars 1,2,3 in V2: only 1 is abbreviation
       ((and (= *version* 2) (= z 1))
        (set! abbrev-pending 0))

       ;; Z-char 4: shift to A1
       ((= z 4)
        (set! alphabet 1))

       ;; Z-char 5: shift to A2
       ((= z 5)
        (set! alphabet 2))

       ;; Z-chars 6-31: look up in current alphabet
       (else
        (let ((index (- z 6)))
          (cond
           ((= alphabet 0)
            (emit-char (string-ref *a0* index)))
           ((= alphabet 1)
            (emit-char (string-ref *a1* index))
            (set! alphabet 0))
           ((= alphabet 2)
            (if (= z 6)
                ;; ZSCII escape: next two Z-chars form a 10-bit value
                (set! tenbit-stage 'high)
                ;; Z-char 7 in A2 = newline
                (if (= z 7)
                    (emit-char #\newline)
                    (emit-char (string-ref *a2* index))))
            (set! alphabet 0)))
          ;; Reset alphabet after printing (V3+ single shift)
          (when (and (>= *version* 3) (= alphabet 1))
            'ok)  ; already reset above for A1 and A2
          (when (and (>= *version* 3) (= alphabet 0))
            'ok)))))

    ;; Process each Z-character
    (for-each translate-zchar zchars)
    (list->string (reverse result))))

;;;
;;; Text encoding: Scheme strings to Z-encoded dictionary form
;;;

(define (encode-ztext str)
  ;; Encode a string into Z-characters for dictionary lookup (V3: 6 Z-chars = 4 bytes)
  (let* ((target-zchars (if (<= *version* 3) 6 9))
         (zchars (string->zchars (string-downcase str) target-zchars)))
    (zchars->bytes zchars)))

(define (string->zchars str max-zchars)
  ;; Convert string to Z-character list, padded to max-zchars with 5s
  (let loop ((chars (string->list str)) (zchars '()) (count 0))
    (if (or (null? chars) (>= count max-zchars))
        ;; Pad with 5s
        (let pad ((zc (reverse zchars)))
          (if (>= (length zc) max-zchars)
              zc
              (pad (append zc (list 5)))))
        (let* ((ch (car chars))
               (new-zchars (char->zchars ch)))
          (loop (cdr chars)
                (append (reverse new-zchars) zchars)
                (+ count (length new-zchars)))))))

(define (char->zchars ch)
  ;; Convert a single character to a list of Z-characters
  (let ((pos-a0 (string-search-forward (string ch) *a0*)))
    (if pos-a0
        (list (+ pos-a0 6))
        (let ((pos-a2 (string-search-forward (string ch) *a2*)))
          (if pos-a2
              (list 5 (+ pos-a2 6))
              ;; 10-bit ZSCII escape
              (let ((code (char->integer ch)))
                (list 5 6
                      (arithmetic-shift code -5)
                      (bitwise-and code #x1F))))))))

(define (zchars->bytes zchars)
  ;; Pack Z-characters into 2-byte words
  ;; V3: 6 Z-chars -> 3 words (6 bytes) ... wait, 6 Z-chars -> 2 words (4 bytes)
  ;; 3 Z-chars per word, so 6 Z-chars = 2 words = 4 bytes
  (let* ((num-words (/ (length zchars) 3))
         (result (make-bytevector (* 2 num-words) 0)))
    (let loop ((i 0) (zc zchars))
      (if (>= i num-words)
          result
          (let* ((z1 (car zc))
                 (z2 (cadr zc))
                 (z3 (caddr zc))
                 (word (bitwise-ior
                        (arithmetic-shift z1 10)
                        (arithmetic-shift z2 5)
                        z3))
                 ;; Set end bit on last word
                 (word (if (= i (- num-words 1))
                           (bitwise-ior word #x8000)
                           word)))
            (bytevector-u8-set! result (* 2 i) (arithmetic-shift word -8))
            (bytevector-u8-set! result (+ (* 2 i) 1) (bitwise-and word #xFF))
            (loop (+ i 1) (cdddr zc)))))))

;;;
;;; Instruction decoding
;;;

;; Instruction representation: (vector opcount opnum operands)
;; opcount is one of: '2op '1op '0op 'var
;; opnum is the opcode number within its category
;; operands is a list of values

(define (make-instruction opcount opnum operands)
  (vector opcount opnum operands))

(define (instr-opcount instr) (vector-ref instr 0))
(define (instr-opnum instr) (vector-ref instr 1))
(define (instr-operands instr) (vector-ref instr 2))

;; Read operands given their types
(define (read-operand type)
  (case type
    ((large) (read-pc-word!))
    ((small) (read-pc-byte!))
    ((var) (get-variable (read-pc-byte!)))
    (else (error "Bad operand type" type))))

;; Decode operand type from 2-bit field
(define (decode-op-type bits)
  (case bits
    ((0) 'large)
    ((1) 'small)
    ((2) 'var)
    ((3) 'omitted)))

;; Decode and execute the next instruction at *pc*
(define (decode-instruction!)
  (let ((opbyte (read-pc-byte!)))
    (cond
     ;; Extended form (0xBE) -- V5+ only
     ((and (= opbyte #xBE) (>= *version* 5))
      (decode-extended!))

     ;; Variable form: top 2 bits = 11
     ((= (bitwise-and opbyte #xC0) #xC0)
      (decode-variable-form! opbyte))

     ;; Short form: top 2 bits = 10
     ((= (bitwise-and opbyte #xC0) #x80)
      (decode-short-form! opbyte))

     ;; Long form: top 2 bits = 00 or 01
     (else
      (decode-long-form! opbyte)))))

(define (decode-long-form! opbyte)
  ;; Always 2OP. Bit 6: type of op1 (0=small, 1=var). Bit 5: type of op2.
  (let* ((opnum (bitwise-and opbyte #x1F))
         (type1 (if (bit-set? 6 opbyte) 'var 'small))
         (type2 (if (bit-set? 5 opbyte) 'var 'small))
         (op1 (read-operand type1))
         (op2 (read-operand type2)))
    (make-instruction '2op opnum (list op1 op2))))

(define (decode-short-form! opbyte)
  ;; Bits 5-4 = operand type. If 11 = 0OP, else 1OP.
  (let* ((opnum (bitwise-and opbyte #x0F))
         (type-bits (bitwise-and (arithmetic-shift opbyte -4) 3))
         (type (decode-op-type type-bits)))
    (if (eq? type 'omitted)
        (make-instruction '0op opnum '())
        (let ((op (read-operand type)))
          (make-instruction '1op opnum (list op))))))

(define (decode-variable-form! opbyte)
  ;; Bit 5: 0 = 2OP, 1 = VAR
  (let* ((opnum (bitwise-and opbyte #x1F))
         (is-var (bit-set? 5 opbyte))
         (opcount (if is-var 'var '2op))
         ;; Read operand types byte
         (type-byte (read-pc-byte!))
         ;; For call_vs2 and call_vn2 (opnums 12 and 26 in VAR), read second type byte
         (has-second-type-byte (and is-var (or (= opnum 12) (= opnum 26))))
         (type-byte2 (if has-second-type-byte (read-pc-byte!) #xFF))
         (operands (read-operands-from-type-byte type-byte)))
    ;; If there's a second type byte, read more operands
    (when has-second-type-byte
      (set! operands (append operands (read-operands-from-type-byte type-byte2))))
    (make-instruction opcount opnum operands)))

(define (read-operands-from-type-byte type-byte)
  ;; Type byte has 4 2-bit fields: bits 7-6, 5-4, 3-2, 1-0
  (let loop ((shift 6) (operands '()))
    (if (< shift 0)
        (reverse operands)
        (let* ((bits (bitwise-and (arithmetic-shift type-byte (- shift)) 3))
               (type (decode-op-type bits)))
          (if (eq? type 'omitted)
              (reverse operands)
              (loop (- shift 2)
                    (cons (read-operand type) operands)))))))

(define (decode-extended!)
  ;; Second byte is the opcode number, then a type byte
  (let* ((opnum (read-pc-byte!))
         (type-byte (read-pc-byte!))
         (operands (read-operands-from-type-byte type-byte)))
    (make-instruction 'ext opnum operands)))

;;;
;;; Branch and store reading (called by opcode handlers)
;;;

(define (read-store-target!)
  ;; Read 1 byte giving the variable number to store into
  (read-pc-byte!))

(define (read-branch!)
  ;; Read branch data. Returns (sense offset) as a list.
  ;; sense = #t means branch on true, #f = branch on false
  ;; offset: 0 = return false, 1 = return true, else jump
  (let* ((byte1 (read-pc-byte!))
         (sense (bit-set? 7 byte1))
         (short-branch (bit-set? 6 byte1)))
    (if short-branch
        ;; 1-byte branch: offset in bottom 6 bits
        (list sense (bitwise-and byte1 #x3F))
        ;; 2-byte branch: 14-bit signed offset
        (let* ((byte2 (read-pc-byte!))
               (raw (bitwise-ior (arithmetic-shift (bitwise-and byte1 #x3F) 8) byte2))
               ;; Sign-extend 14-bit value
               (offset (if (bit-set? 13 raw)
                           (- raw 16384)
                           raw)))
          (list sense offset)))))

(define (do-branch! condition)
  ;; Process a branch given the condition result
  (let* ((branch-info (read-branch!))
         (sense (car branch-info))
         (offset (cadr branch-info))
         (should-branch (eq? (not (not condition)) sense)))
    (when should-branch
      (cond
       ((= offset 0) (do-return! 0))
       ((= offset 1) (do-return! 1))
       (else (set! *pc* (+ *pc* offset -2)))))))

(define (do-return! value)
  ;; Return from current routine with value
  (let* ((frame (car *call-stack*))
         (saved-arg-count (vector-ref frame 5))
         (frame-info (pop-frame!))
         (return-pc (car frame-info))
         (result-var (cdr frame-info)))
    (set! *pc* return-pc)
    (set! *current-arg-count* saved-arg-count)
    (when result-var
      (set-variable! result-var (u16 value)))))

;; Read an inline Z-string (for print and print_ret opcodes)
(define (read-inline-string!)
  (let* ((result (decode-zstring *pc*))
         (text (car result))
         (bytes-consumed (cdr result)))
    (set! *pc* (+ *pc* bytes-consumed))
    text))

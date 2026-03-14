;;; ops.scm -- Opcode dispatch tables and implementations

;;;
;;; Dispatch tables
;;;

(define *op-2op* (make-vector 32 #f))
(define *op-1op* (make-vector 16 #f))
(define *op-0op* (make-vector 16 #f))
(define *op-var* (make-vector 32 #f))
(define *op-ext* (make-vector 256 #f))

(define (dispatch-instruction! instr)
  (let* ((opcount (instr-opcount instr))
         (opnum (instr-opnum instr))
         (operands (instr-operands instr))
         (table (case opcount
                  ((2op) *op-2op*)
                  ((1op) *op-1op*)
                  ((0op) *op-0op*)
                  ((var) *op-var*)
                  ((ext) *op-ext*)
                  (else (error "Unknown opcount" opcount))))
         (handler (if (< opnum (vector-length table))
                      (vector-ref table opnum)
                      #f)))
    (if handler
        (handler operands)
        (error (string-append "Unimplemented opcode: "
                              (symbol->string opcount)
                              ":" (number->string opnum))))))

;;;
;;; Output
;;;

(define *output-buffer* '())
(define *buffering* #t)
(define *current-window* 0)       ; 0 = lower, 1 = upper
(define *upper-window-height* 0)  ; height of upper window in lines

(define (z-print str)
  (display str)
  (flush-output-port (current-output-port)))

(define (z-print-char ch)
  (display ch)
  (flush-output-port (current-output-port)))

(define (z-newline)
  (newline)
  (flush-output-port (current-output-port)))

;;;
;;; Object table helpers (V3: 9-byte entries, V4+: 14-byte entries)
;;;

(define (obj-entry-size) (if (<= *version* 3) 9 14))
(define (obj-prop-defaults-count) (if (<= *version* 3) 31 63))
(define (obj-attr-bytes) (if (<= *version* 3) 4 6))

(define (obj-addr obj-num)
  ;; Object entries start after property defaults table
  (+ *object-table-addr*
     (* 2 (obj-prop-defaults-count))
     (* (obj-entry-size) (- obj-num 1))))

(define (obj-attr? obj-num attr)
  ;; Attributes stored as 32 bits (V3) or 48 bits (V4+) at start of entry
  ;; Attribute 0 is bit 7 of byte 0, attribute 7 is bit 0 of byte 0, etc.
  (let* ((addr (obj-addr obj-num))
         (byte-offset (quotient attr 8))
         (bit-num (- 7 (remainder attr 8)))
         (byte-val (mem-byte (+ addr byte-offset))))
    (bit-set? bit-num byte-val)))

(define (obj-set-attr! obj-num attr)
  (let* ((addr (obj-addr obj-num))
         (byte-offset (quotient attr 8))
         (bit-num (- 7 (remainder attr 8)))
         (byte-addr (+ addr byte-offset))
         (byte-val (mem-byte byte-addr)))
    (mem-set-byte! byte-addr (bitwise-ior byte-val (arithmetic-shift 1 bit-num)))))

(define (obj-clear-attr! obj-num attr)
  (let* ((addr (obj-addr obj-num))
         (byte-offset (quotient attr 8))
         (bit-num (- 7 (remainder attr 8)))
         (byte-addr (+ addr byte-offset))
         (byte-val (mem-byte byte-addr)))
    (mem-set-byte! byte-addr (bitwise-and byte-val (bitwise-not (arithmetic-shift 1 bit-num))))))

;; V3: parent/sibling/child are 1 byte each, at offsets 4,5,6
;; V4+: parent/sibling/child are words (2 bytes), at offsets 6,8,10
(define (obj-parent obj-num)
  (if (<= *version* 3)
      (mem-byte (+ (obj-addr obj-num) 4))
      (mem-word (+ (obj-addr obj-num) 6))))
(define (obj-sibling obj-num)
  (if (<= *version* 3)
      (mem-byte (+ (obj-addr obj-num) 5))
      (mem-word (+ (obj-addr obj-num) 8))))
(define (obj-child obj-num)
  (if (<= *version* 3)
      (mem-byte (+ (obj-addr obj-num) 6))
      (mem-word (+ (obj-addr obj-num) 10))))

(define (obj-set-parent! obj-num val)
  (if (<= *version* 3)
      (mem-set-byte! (+ (obj-addr obj-num) 4) val)
      (mem-set-word! (+ (obj-addr obj-num) 6) val)))
(define (obj-set-sibling! obj-num val)
  (if (<= *version* 3)
      (mem-set-byte! (+ (obj-addr obj-num) 5) val)
      (mem-set-word! (+ (obj-addr obj-num) 8) val)))
(define (obj-set-child! obj-num val)
  (if (<= *version* 3)
      (mem-set-byte! (+ (obj-addr obj-num) 6) val)
      (mem-set-word! (+ (obj-addr obj-num) 10) val)))

;; Property table address: V3 at offset 7, V4+ at offset 12
(define (obj-prop-table-addr obj-num)
  (mem-word (+ (obj-addr obj-num) (if (<= *version* 3) 7 12))))

(define (obj-short-name obj-num)
  ;; First byte of property table is text-length in words
  (let* ((prop-addr (obj-prop-table-addr obj-num))
         (text-len (mem-byte prop-addr)))
    (if (= text-len 0)
        ""
        (car (decode-zstring (+ prop-addr 1))))))

;; Remove object from its parent's child chain
(define (obj-remove! obj-num)
  (let ((parent (obj-parent obj-num)))
    (when (not (= parent 0))
      (let ((first-child (obj-child parent)))
        (if (= first-child obj-num)
            ;; Object is the first child; replace with its sibling
            (begin
              (obj-set-child! parent (obj-sibling obj-num))
              (obj-set-sibling! obj-num 0)
              (obj-set-parent! obj-num 0))
            ;; Walk sibling chain to find previous sibling
            (let loop ((prev first-child))
              (let ((next (obj-sibling prev)))
                (cond
                 ((= next 0)
                  ;; Not found, just clear
                  (obj-set-parent! obj-num 0)
                  (obj-set-sibling! obj-num 0))
                 ((= next obj-num)
                  (obj-set-sibling! prev (obj-sibling obj-num))
                  (obj-set-sibling! obj-num 0)
                  (obj-set-parent! obj-num 0))
                 (else (loop next))))))))))

;; Insert object as first child of destination
(define (obj-insert! obj-num dest)
  ;; First remove from current parent
  (obj-remove! obj-num)
  ;; Make it first child of dest
  (obj-set-sibling! obj-num (obj-child dest))
  (obj-set-child! dest obj-num)
  (obj-set-parent! obj-num dest))

;; Property access
;; V3: size byte bits 7-5 = (size-1), bits 4-0 = property number
;; V4+: bit 7 of first byte: if set, second size byte follows
;;       First byte: bits 5-0 = property number, bit 6 = (if no second byte) 0=size 1, 1=size 2
;;       Second byte (if bit 7 set): bits 5-0 = size (0 means 64), bit 7 always set

(define (prop-parse-size-byte addr)
  ;; Returns (values prop-num data-size header-bytes)
  ;; header-bytes is how many bytes the size header occupies (1 or 2)
  (let ((size-byte (mem-byte addr)))
    (if (<= *version* 3)
        ;; V3: bits 7-5 = size-1, bits 4-0 = prop number
        (let ((pnum (bitwise-and size-byte #x1F))
              (psize (+ 1 (arithmetic-shift size-byte -5))))
          (list pnum psize 1))
        ;; V4+
        (let ((pnum (bitwise-and size-byte #x3F)))
          (if (bit-set? 7 size-byte)
              ;; Two-byte header
              (let* ((byte2 (mem-byte (+ addr 1)))
                     (psize (bitwise-and byte2 #x3F)))
                (list pnum (if (= psize 0) 64 psize) 2))
              ;; One-byte header
              (let ((psize (if (bit-set? 6 size-byte) 2 1)))
                (list pnum psize 1)))))))

(define (prop-num-from-byte addr)
  ;; Extract just the property number from a size byte
  (let ((size-byte (mem-byte addr)))
    (if (<= *version* 3)
        (bitwise-and size-byte #x1F)
        (bitwise-and size-byte #x3F))))

(define (find-prop obj-num prop-num)
  ;; Returns (data-addr . data-size) or #f
  (let* ((prop-table (obj-prop-table-addr obj-num))
         (name-len (mem-byte prop-table))
         (start (+ prop-table 1 (* 2 name-len))))
    (let loop ((addr start))
      (let ((size-byte (mem-byte addr)))
        (if (= size-byte 0)
            #f  ; end of property list
            (let* ((parsed (prop-parse-size-byte addr))
                   (pnum (car parsed))
                   (psize (cadr parsed))
                   (hdr-bytes (caddr parsed))
                   (data-addr (+ addr hdr-bytes)))
              (cond
               ((= pnum prop-num)
                (cons data-addr psize))
               ((< pnum prop-num)
                ;; Properties are in descending order; if we passed it, not found
                #f)
               (else
                (loop (+ data-addr psize))))))))))

(define (get-prop obj-num prop-num)
  (let ((found (find-prop obj-num prop-num)))
    (if found
        (let ((addr (car found))
              (size (cdr found)))
          (cond
           ((= size 1) (mem-byte addr))
           ((= size 2) (mem-word addr))
           (else (mem-word addr))))
        ;; Return default from property defaults table
        (mem-word (+ *object-table-addr* (* 2 (- prop-num 1)))))))

(define (get-prop-addr obj-num prop-num)
  (let ((found (find-prop obj-num prop-num)))
    (if found (car found) 0)))

(define (get-next-prop obj-num prop-num)
  ;; If prop-num = 0, return first property number
  ;; Otherwise return the property number after prop-num
  (let* ((prop-table (obj-prop-table-addr obj-num))
         (name-len (mem-byte prop-table))
         (start (+ prop-table 1 (* 2 name-len))))
    (if (= prop-num 0)
        ;; Return first property number
        (let ((size-byte (mem-byte start)))
          (if (= size-byte 0) 0 (prop-num-from-byte start)))
        ;; Find prop-num, then return the next one
        (let loop ((addr start))
          (let ((size-byte (mem-byte addr)))
            (if (= size-byte 0)
                (error "get_next_prop: property not found" prop-num)
                (let* ((parsed (prop-parse-size-byte addr))
                       (pnum (car parsed))
                       (psize (cadr parsed))
                       (hdr-bytes (caddr parsed))
                       (next-addr (+ addr hdr-bytes psize)))
                  (if (= pnum prop-num)
                      ;; Return next property number
                      (let ((next-byte (mem-byte next-addr)))
                        (if (= next-byte 0) 0 (prop-num-from-byte next-addr)))
                      (loop next-addr)))))))))

(define (put-prop! obj-num prop-num value)
  (let ((found (find-prop obj-num prop-num)))
    (if (not found)
        (error "put_prop: property not found" obj-num prop-num)
        (let ((addr (car found))
              (size (cdr found)))
          (cond
           ((= size 1) (mem-set-byte! addr (bitwise-and value #xFF)))
           ((= size 2) (mem-set-word! addr value))
           (else (error "put_prop: property size > 2" size)))))))

(define (get-prop-len prop-data-addr)
  ;; Given the address of property DATA, get its length
  ;; The size byte is at (prop-data-addr - 1)
  (if (= prop-data-addr 0)
      0
      (if (<= *version* 3)
          (let ((size-byte (mem-byte (- prop-data-addr 1))))
            (+ 1 (arithmetic-shift size-byte -5)))
          ;; V4+: check if this is a second size byte (bit 7 set)
          (let ((prev-byte (mem-byte (- prop-data-addr 1))))
            (if (bit-set? 7 prev-byte)
                ;; This is byte 2 of a two-byte header
                (let ((psize (bitwise-and prev-byte #x3F)))
                  (if (= psize 0) 64 psize))
                ;; Single-byte header
                (if (bit-set? 6 prev-byte) 2 1))))))

;;;
;;; Dictionary and lexical analysis
;;;

(define (dict-word-separators)
  ;; Returns a list of ZSCII codes that are word separators
  (let* ((n (mem-byte *dictionary-addr*)))
    (let loop ((i 0) (seps '()))
      (if (>= i n)
          (reverse seps)
          (loop (+ i 1)
                (cons (mem-byte (+ *dictionary-addr* 1 i)) seps))))))

(define (dict-entry-length)
  (mem-byte (+ *dictionary-addr* 1 (mem-byte *dictionary-addr*))))

(define (dict-num-entries)
  ;; This is a signed word -- negative means unsorted
  (let ((w (mem-word (+ *dictionary-addr* 2 (mem-byte *dictionary-addr*)))))
    (s16 w)))

(define (dict-entries-start)
  (+ *dictionary-addr* 4 (mem-byte *dictionary-addr*)))

(define (dict-lookup encoded-bytes)
  ;; Binary search the dictionary. encoded-bytes is a bytevector of 4 bytes (V3).
  ;; Returns the dictionary address of the entry, or 0 if not found.
  (let* ((num-entries (abs (dict-num-entries)))
         (entry-len (dict-entry-length))
         (base (dict-entries-start))
         (encoded-len (bytevector-length encoded-bytes)))
    (let search ((lo 0) (hi (- num-entries 1)))
      (if (> lo hi)
          0
          (let* ((mid (quotient (+ lo hi) 2))
                 (addr (+ base (* mid entry-len)))
                 (cmp (compare-encoded addr encoded-bytes encoded-len)))
            (cond
             ((= cmp 0) addr)
             ((< cmp 0) (search (+ mid 1) hi))
             (else (search lo (- mid 1)))))))))

(define (compare-encoded dict-addr encoded-bytes len)
  ;; Compare encoded bytes at dict-addr with encoded-bytes
  ;; Returns negative if dict < encoded, 0 if equal, positive if dict > encoded
  (let loop ((i 0))
    (if (>= i len)
        0
        (let ((d (mem-byte (+ dict-addr i)))
              (e (bytevector-u8-ref encoded-bytes i)))
          (cond
           ((< d e) -1)
           ((> d e) 1)
           (else (loop (+ i 1))))))))

(define (tokenize-input text-buf parse-buf)
  ;; text-buf: byte 0 = max chars, bytes 1+ = input characters (lowercase)
  ;; parse-buf: byte 0 = max words, then we fill in results
  (let* ((max-words (mem-byte parse-buf))
         (separators (dict-word-separators))
         (sep-chars (map integer->char separators))
         ;; Read input text from buffer
         (text-start (+ text-buf 1))
         (input-text (let loop ((i 0) (chars '()))
                       (let ((ch (mem-byte (+ text-start i))))
                         (if (= ch 0)
                             (list->string (reverse chars))
                             (loop (+ i 1) (cons (integer->char ch) chars)))))))
    ;; Split into words
    (let* ((words (split-into-words input-text sep-chars))
           (num-words (min (length words) max-words)))
      ;; Write number of words
      (mem-set-byte! (+ parse-buf 1) num-words)
      ;; Write each word entry: 2-byte dict addr, 1-byte length, 1-byte position
      (let loop ((ws words) (i 0))
        (when (and (< i max-words) (not (null? ws)))
          (let* ((word-info (car ws))
                 (word (car word-info))
                 (pos (cdr word-info))
                 (encoded (encode-ztext word))
                 (dict-addr (dict-lookup encoded))
                 (entry-addr (+ parse-buf 2 (* 4 i))))
            (mem-set-word! entry-addr dict-addr)
            (mem-set-byte! (+ entry-addr 2) (string-length word))
            (mem-set-byte! (+ entry-addr 3) (+ pos 1))  ; 1-indexed in text buffer
            (loop (cdr ws) (+ i 1))))))))

(define (tokenize-input-v5 text-buf parse-buf)
  ;; V5+: text-buf byte 0 = max, byte 1 = num chars, chars from byte 2
  ;; parse-buf: byte 0 = max words
  (let* ((max-words (mem-byte parse-buf))
         (num-chars (mem-byte (+ text-buf 1)))
         (separators (dict-word-separators))
         (sep-chars (map integer->char separators))
         (input-text (let loop ((i 0) (chars '()))
                       (if (>= i num-chars)
                           (list->string (reverse chars))
                           (loop (+ i 1)
                                 (cons (integer->char (mem-byte (+ text-buf 2 i))) chars))))))
    (let* ((words (split-into-words input-text sep-chars))
           (num-words (min (length words) max-words)))
      (mem-set-byte! (+ parse-buf 1) num-words)
      (let loop ((ws words) (i 0))
        (when (and (< i max-words) (not (null? ws)))
          (let* ((word-info (car ws))
                 (word (car word-info))
                 (pos (cdr word-info))
                 (encoded (encode-ztext word))
                 (dict-addr (dict-lookup encoded))
                 (entry-addr (+ parse-buf 2 (* 4 i))))
            (mem-set-word! entry-addr dict-addr)
            (mem-set-byte! (+ entry-addr 2) (string-length word))
            (mem-set-byte! (+ entry-addr 3) (+ pos 2))  ; offset from start of text-buf
            (loop (cdr ws) (+ i 1))))))))

(define (split-into-words text sep-chars)
  ;; Returns list of (word . position) pairs
  ;; Spaces divide words and are ignored. Separators divide and are words themselves.
  (let loop ((i 0) (current '()) (start 0) (words '()))
    (if (>= i (string-length text))
        ;; Flush any remaining word
        (if (null? current)
            (reverse words)
            (reverse (cons (cons (list->string (reverse current)) start) words)))
        (let ((ch (string-ref text i)))
          (cond
           ;; Space: flush current word
           ((char=? ch #\space)
            (if (null? current)
                (loop (+ i 1) '() (+ i 1) words)
                (loop (+ i 1) '() (+ i 1)
                      (cons (cons (list->string (reverse current)) start) words))))
           ;; Separator character: flush current word, then add separator as its own word
           ((member ch sep-chars)
            (let ((words2 (if (null? current)
                              words
                              (cons (cons (list->string (reverse current)) start) words))))
              (loop (+ i 1) '() (+ i 1)
                    (cons (cons (string ch) i) words2))))
           ;; Normal character: accumulate
           (else
            (let ((new-start (if (null? current) i start)))
              (loop (+ i 1) (cons ch current) new-start words))))))))

;;;
;;; Status line (V3)
;;;

(define (show-status-line)
  (when (= *version* 3)
    (let* ((loc-obj (mem-word *globals-addr*))  ; global 0 = first global variable
           (loc-name (if (> loc-obj 0) (obj-short-name loc-obj) "???"))
           (val1 (s16 (mem-word (+ *globals-addr* 2))))   ; global 1
           (val2 (mem-word (+ *globals-addr* 4)))          ; global 2
           (is-time (bit-set? 1 (mem-byte 1)))
           (right-side (if is-time
                           (string-append "Time: "
                                          (number->string (modulo val1 12))
                                          ":"
                                          (if (< val2 10) "0" "")
                                          (number->string val2)
                                          (if (>= val1 12) " PM" " AM"))
                           (string-append "Score: " (number->string val1)
                                          "  Turns: " (number->string val2))))
           (width 80)
           (padding (max 1 (- width (string-length loc-name) (string-length right-side)))))
      ;; ANSI: save cursor, move to 1,1, reverse video
      (display "\033[s\033[1;1H\033[7m ")
      (display loc-name)
      (display (make-string padding #\space))
      (display right-side)
      (display " \033[0m\033[u")
      (flush-output-port (current-output-port)))))

;;;
;;; Random number generator
;;;

(define *random-state* 'random)
(define *random-counter* 0)
(define *random-seed* 0)

(define (z-random range)
  (cond
   ((<= range 0)
    ;; Seed the RNG
    (let ((seed (- range)))
      (if (= seed 0)
          (set! *random-state* 'random)
          (begin
            (set! *random-state* 'predictable)
            (set! *random-seed* seed)
            (set! *random-counter* 0)))
      0))
   ((eq? *random-state* 'random)
    (+ 1 (random range)))
   (else
    ;; Predictable mode
    (set! *random-counter* (+ *random-counter* 1))
    (+ 1 (modulo (- *random-counter* 1) range)))))

;;;
;;; Input handling
;;;

(define (z-read text-buf parse-buf)
  ;; Show status line first (V3 requirement)
  (when (<= *version* 3) (show-status-line))
  ;; Read a line of input
  (display ">")
  (flush-output-port (current-output-port))
  (let* ((line (read-line))
         (line (if (eof-object? line) "quit" line))
         (line (string-downcase line))
         (max-len (- (mem-byte text-buf) 1))  ; leave room
         (len (min (string-length line) max-len)))
    (if (<= *version* 4)
        ;; V1-4: store characters starting at byte 1, zero-terminate
        (begin
          (let loop ((i 0))
            (when (< i len)
              (mem-set-byte! (+ text-buf 1 i) (char->integer (string-ref line i)))
              (loop (+ i 1))))
          (mem-set-byte! (+ text-buf 1 len) 0)
          (tokenize-input text-buf parse-buf))
        ;; V5+: byte 0 = max chars, byte 1 = num chars typed, chars from byte 2
        (begin
          (mem-set-byte! (+ text-buf 1) len)
          (let loop ((i 0))
            (when (< i len)
              (mem-set-byte! (+ text-buf 2 i) (char->integer (string-ref line i)))
              (loop (+ i 1))))
          ;; Tokenize if parse-buf is nonzero
          (when (> parse-buf 0)
            (tokenize-input-v5 text-buf parse-buf))))))

;;;
;;; Opcode registration
;;;

(define (init-opcodes!)
  ;; ---- 2OP opcodes ----

  ;; 2OP:1 je a b ?(label) -- also works with 3 or 4 operands in VAR form
  (vector-set! *op-2op* 1
    (lambda (ops)
      (let ((a (car ops)))
        (do-branch! (any (lambda (b) (= a b)) (cdr ops))))))

  ;; 2OP:2 jl a b ?(label) -- signed
  (vector-set! *op-2op* 2
    (lambda (ops)
      (do-branch! (< (s16 (car ops)) (s16 (cadr ops))))))

  ;; 2OP:3 jg a b ?(label) -- signed
  (vector-set! *op-2op* 3
    (lambda (ops)
      (do-branch! (> (s16 (car ops)) (s16 (cadr ops))))))

  ;; 2OP:4 dec_chk (variable) value ?(label)
  (vector-set! *op-2op* 4
    (lambda (ops)
      (let* ((var-num (car ops))
             (val (s16 (cadr ops)))
             (current (s16 (get-variable-indirect var-num)))
             (new-val (u16-from-signed (- current 1))))
        (set-variable-indirect! var-num new-val)
        (do-branch! (< (s16 new-val) val)))))

  ;; 2OP:5 inc_chk (variable) value ?(label)
  (vector-set! *op-2op* 5
    (lambda (ops)
      (let* ((var-num (car ops))
             (val (s16 (cadr ops)))
             (current (s16 (get-variable-indirect var-num)))
             (new-val (u16-from-signed (+ current 1))))
        (set-variable-indirect! var-num new-val)
        (do-branch! (> (s16 new-val) val)))))

  ;; 2OP:6 jin obj1 obj2 ?(label) -- branch if obj1 is child of obj2
  (vector-set! *op-2op* 6
    (lambda (ops)
      (do-branch! (= (obj-parent (car ops)) (cadr ops)))))

  ;; 2OP:7 test bitmap flags ?(label) -- branch if (bitmap AND flags) == flags
  (vector-set! *op-2op* 7
    (lambda (ops)
      (do-branch! (= (bitwise-and (car ops) (cadr ops)) (cadr ops)))))

  ;; 2OP:8 or a b -> (result)
  (vector-set! *op-2op* 8
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (u16 (bitwise-ior (car ops) (cadr ops)))))))

  ;; 2OP:9 and a b -> (result)
  (vector-set! *op-2op* 9
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (u16 (bitwise-and (car ops) (cadr ops)))))))

  ;; 2OP:10 test_attr object attribute ?(label)
  (vector-set! *op-2op* 10
    (lambda (ops)
      (do-branch! (obj-attr? (car ops) (cadr ops)))))

  ;; 2OP:11 set_attr object attribute
  (vector-set! *op-2op* 11
    (lambda (ops)
      (obj-set-attr! (car ops) (cadr ops))))

  ;; 2OP:12 clear_attr object attribute
  (vector-set! *op-2op* 12
    (lambda (ops)
      (obj-clear-attr! (car ops) (cadr ops))))

  ;; 2OP:13 store (variable) value
  (vector-set! *op-2op* 13
    (lambda (ops)
      (set-variable-indirect! (car ops) (cadr ops))))

  ;; 2OP:14 insert_obj object destination
  (vector-set! *op-2op* 14
    (lambda (ops)
      (obj-insert! (car ops) (cadr ops))))

  ;; 2OP:15 loadw array word-index -> (result)
  (vector-set! *op-2op* 15
    (lambda (ops)
      (let ((result-var (read-store-target!))
            (addr (+ (car ops) (* 2 (s16 (cadr ops))))))
        (set-variable! result-var (mem-word addr)))))

  ;; 2OP:16 loadb array byte-index -> (result)
  (vector-set! *op-2op* 16
    (lambda (ops)
      (let ((result-var (read-store-target!))
            (addr (+ (car ops) (s16 (cadr ops)))))
        (set-variable! result-var (mem-byte addr)))))

  ;; 2OP:17 get_prop object property -> (result)
  (vector-set! *op-2op* 17
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (get-prop (car ops) (cadr ops))))))

  ;; 2OP:18 get_prop_addr object property -> (result)
  (vector-set! *op-2op* 18
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (get-prop-addr (car ops) (cadr ops))))))

  ;; 2OP:19 get_next_prop object property -> (result)
  (vector-set! *op-2op* 19
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (get-next-prop (car ops) (cadr ops))))))

  ;; 2OP:20 add a b -> (result)
  (vector-set! *op-2op* 20
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (u16 (+ (s16 (car ops)) (s16 (cadr ops))))))))

  ;; 2OP:21 sub a b -> (result)
  (vector-set! *op-2op* 21
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (u16 (- (s16 (car ops)) (s16 (cadr ops))))))))

  ;; 2OP:22 mul a b -> (result)
  (vector-set! *op-2op* 22
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (u16 (* (s16 (car ops)) (s16 (cadr ops))))))))

  ;; 2OP:23 div a b -> (result)
  (vector-set! *op-2op* 23
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (if (= (cadr ops) 0)
            (error "Division by zero")
            (set-variable! result-var (u16 (truncate (/ (s16 (car ops)) (s16 (cadr ops))))))))))

  ;; 2OP:24 mod a b -> (result)
  (vector-set! *op-2op* 24
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (if (= (cadr ops) 0)
            (error "Division by zero (mod)")
            (let* ((a (s16 (car ops)))
                   (b (s16 (cadr ops)))
                   (result (- a (* b (truncate (/ a b))))))
              (set-variable! result-var (u16 result)))))))

  ;; 2OP:6 jin -- already registered above

  ;; ---- 1OP opcodes ----

  ;; 1OP:0 jz a ?(label)
  (vector-set! *op-1op* 0
    (lambda (ops)
      (do-branch! (= (car ops) 0))))

  ;; 1OP:1 get_sibling object -> (result) ?(label)
  (vector-set! *op-1op* 1
    (lambda (ops)
      (let* ((result-var (read-store-target!))
             (sib (obj-sibling (car ops))))
        (set-variable! result-var sib)
        (do-branch! (not (= sib 0))))))

  ;; 1OP:2 get_child object -> (result) ?(label)
  (vector-set! *op-1op* 2
    (lambda (ops)
      (let* ((result-var (read-store-target!))
             (child (obj-child (car ops))))
        (set-variable! result-var child)
        (do-branch! (not (= child 0))))))

  ;; 1OP:3 get_parent object -> (result)
  (vector-set! *op-1op* 3
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (obj-parent (car ops))))))

  ;; 1OP:4 get_prop_len property-address -> (result)
  (vector-set! *op-1op* 4
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (get-prop-len (car ops))))))

  ;; 1OP:5 inc (variable)
  (vector-set! *op-1op* 5
    (lambda (ops)
      (let* ((var-num (car ops))
             (val (s16 (get-variable-indirect var-num))))
        (set-variable-indirect! var-num (u16-from-signed (+ val 1))))))

  ;; 1OP:6 dec (variable)
  (vector-set! *op-1op* 6
    (lambda (ops)
      (let* ((var-num (car ops))
             (val (s16 (get-variable-indirect var-num))))
        (set-variable-indirect! var-num (u16-from-signed (- val 1))))))

  ;; 1OP:7 print_addr byte-address-of-string
  (vector-set! *op-1op* 7
    (lambda (ops)
      (z-print (car (decode-zstring (car ops))))))

  ;; 1OP:8 call_1s routine -> (result) [V4+, but let's handle anyway]
  (vector-set! *op-1op* 8
    (lambda (ops)
      (z-call (car ops) '() #t)))

  ;; 1OP:9 remove_obj object
  (vector-set! *op-1op* 9
    (lambda (ops)
      (obj-remove! (car ops))))

  ;; 1OP:10 print_obj object
  (vector-set! *op-1op* 10
    (lambda (ops)
      (z-print (obj-short-name (car ops)))))

  ;; 1OP:11 ret value
  (vector-set! *op-1op* 11
    (lambda (ops)
      (do-return! (car ops))))

  ;; 1OP:12 jump ?(label) -- unconditional jump (signed offset from current PC)
  (vector-set! *op-1op* 12
    (lambda (ops)
      (let ((offset (s16 (car ops))))
        (set! *pc* (+ *pc* offset -2)))))

  ;; 1OP:13 print_paddr packed-address-of-string
  (vector-set! *op-1op* 13
    (lambda (ops)
      (z-print (car (decode-zstring (unpack-addr (car ops)))))))

  ;; 1OP:14 load (variable) -> (result)
  (vector-set! *op-1op* 14
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (get-variable-indirect (car ops))))))

  ;; 1OP:15 not value -> (result) [V3] / call_1n routine [V5+]
  (vector-set! *op-1op* 15
    (lambda (ops)
      (if (<= *version* 4)
          ;; not
          (let ((result-var (read-store-target!)))
            (set-variable! result-var (u16 (bitwise-and (bitwise-not (car ops)) #xFFFF))))
          ;; call_1n
          (z-call (car ops) '() #f))))

  ;; ---- 0OP opcodes ----

  ;; 0OP:0 rtrue
  (vector-set! *op-0op* 0
    (lambda (ops) (do-return! 1)))

  ;; 0OP:1 rfalse
  (vector-set! *op-0op* 1
    (lambda (ops) (do-return! 0)))

  ;; 0OP:2 print (literal-string)
  (vector-set! *op-0op* 2
    (lambda (ops)
      (z-print (read-inline-string!))))

  ;; 0OP:3 print_ret (literal-string)
  (vector-set! *op-0op* 3
    (lambda (ops)
      (z-print (read-inline-string!))
      (z-newline)
      (do-return! 1)))

  ;; 0OP:4 nop
  (vector-set! *op-0op* 4
    (lambda (ops) 'nop))

  ;; 0OP:5 save ?(label) [V3]
  (vector-set! *op-0op* 5
    (lambda (ops)
      ;; For now, always fail (branch on false = don't branch)
      (do-branch! #f)))

  ;; 0OP:6 restore ?(label) [V3]
  (vector-set! *op-0op* 6
    (lambda (ops)
      ;; For now, always fail
      (do-branch! #f)))

  ;; 0OP:7 restart
  (vector-set! *op-0op* 7
    (lambda (ops)
      ;; TODO: reload story file
      (error "restart not implemented")))

  ;; 0OP:8 ret_popped
  (vector-set! *op-0op* 8
    (lambda (ops)
      (do-return! (stack-pop!))))

  ;; 0OP:9 pop [V1-4] / catch -> (result) [V5+]
  (vector-set! *op-0op* 9
    (lambda (ops)
      (if (<= *version* 4)
          (stack-pop!)  ; discard
          (let ((result-var (read-store-target!)))
            (set-variable! result-var (length *call-stack*))))))

  ;; 0OP:10 quit
  (vector-set! *op-0op* 10
    (lambda (ops)
      (set! *running* #f)))

  ;; 0OP:11 new_line
  (vector-set! *op-0op* 11
    (lambda (ops) (z-newline)))

  ;; 0OP:12 show_status [V3]
  (vector-set! *op-0op* 12
    (lambda (ops) (show-status-line)))

  ;; 0OP:13 verify ?(label)
  (vector-set! *op-0op* 13
    (lambda (ops)
      ;; Always pass verification for now
      (do-branch! #t)))

  ;; 0OP:15 piracy ?(label)
  (vector-set! *op-0op* 15
    (lambda (ops)
      (do-branch! #t)))

  ;; ---- VAR opcodes ----

  ;; VAR:0 call / call_vs routine ...args... -> (result)
  (vector-set! *op-var* 0
    (lambda (ops)
      (z-call (car ops) (cdr ops) #t)))

  ;; VAR:1 storew array word-index value
  (vector-set! *op-var* 1
    (lambda (ops)
      (let ((addr (+ (car ops) (* 2 (s16 (cadr ops))))))
        (mem-set-word! addr (caddr ops)))))

  ;; VAR:2 storeb array byte-index value
  (vector-set! *op-var* 2
    (lambda (ops)
      (let ((addr (+ (car ops) (s16 (cadr ops)))))
        (mem-set-byte! addr (caddr ops)))))

  ;; VAR:3 put_prop object property value
  (vector-set! *op-var* 3
    (lambda (ops)
      (put-prop! (car ops) (cadr ops) (caddr ops))))

  ;; VAR:4 sread / aread
  (vector-set! *op-var* 4
    (lambda (ops)
      (let ((text-buf (car ops))
            (parse-buf (if (>= (length ops) 2) (cadr ops) 0)))
        (z-read text-buf parse-buf)
        ;; V5+ returns terminating character; V1-4 does not store
        (when (>= *version* 5)
          (let ((result-var (read-store-target!)))
            (set-variable! result-var 13))))))

  ;; VAR:5 print_char output-character-code
  (vector-set! *op-var* 5
    (lambda (ops)
      (let ((code (car ops)))
        (when (and (>= code 32) (<= code 126))
          (z-print-char (integer->char code)))
        (when (= code 13)
          (z-newline)))))

  ;; VAR:6 print_num value
  (vector-set! *op-var* 6
    (lambda (ops)
      (z-print (number->string (s16 (car ops))))))

  ;; VAR:7 random range -> (result)
  (vector-set! *op-var* 7
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (z-random (s16 (car ops)))))))

  ;; VAR:8 push value
  (vector-set! *op-var* 8
    (lambda (ops)
      (stack-push! (car ops))))

  ;; VAR:9 pull (variable) [V1-5]
  (vector-set! *op-var* 9
    (lambda (ops)
      (set-variable-indirect! (car ops) (stack-pop!))))

  ;; VAR:10 split_window lines
  (vector-set! *op-var* 10
    (lambda (ops)
      (set! *upper-window-height* (car ops))))

  ;; VAR:11 set_window window
  (vector-set! *op-var* 11
    (lambda (ops)
      (let ((win (car ops)))
        (set! *current-window* win)
        ;; When selecting upper window, reset cursor to top-left
        (when (= win 1)
          (display "\033[s")  ; save lower window cursor
          (display "\033[1;1H")
          (flush-output-port (current-output-port)))
        ;; When selecting lower window, restore cursor
        (when (= win 0)
          (display "\033[u")  ; restore cursor
          (flush-output-port (current-output-port))))))

  ;; VAR:13 erase_window window [V4+]
  (vector-set! *op-var* 13
    (lambda (ops)
      (let ((win (s16 (car ops))))
        (cond
         ((= win -1)
          ;; Clear whole screen, collapse upper window, select lower
          (set! *upper-window-height* 0)
          (set! *current-window* 0)
          (display "\033[2J\033[1;1H")
          (flush-output-port (current-output-port)))
         ((= win -2)
          ;; Clear screen without changing windows
          (display "\033[2J\033[1;1H")
          (flush-output-port (current-output-port)))
         ((= win 0)
          ;; Clear lower window
          (let ((start-row (+ *upper-window-height* 1)))
            (display (string-append "\033[" (number->string start-row) ";1H\033[J"))
            (flush-output-port (current-output-port))))
         ((= win 1)
          ;; Clear upper window
          (let loop ((row 1))
            (when (<= row *upper-window-height*)
              (display (string-append "\033[" (number->string row) ";1H\033[K"))
              (loop (+ row 1))))
          (flush-output-port (current-output-port)))))))

  ;; VAR:14 erase_line value [V4+]
  (vector-set! *op-var* 14
    (lambda (ops)
      (when (= (car ops) 1)
        (display "\033[K")
        (flush-output-port (current-output-port)))))

  ;; VAR:15 set_cursor line column [V4+]
  (vector-set! *op-var* 15
    (lambda (ops)
      ;; ANSI cursor positioning (only effective in upper window)
      (when (= *current-window* 1)
        (display (string-append "\033[" (number->string (car ops))
                                ";" (number->string (cadr ops)) "H"))
        (flush-output-port (current-output-port)))))

  ;; VAR:17 set_text_style style [V4+]
  (vector-set! *op-var* 17
    (lambda (ops)
      ;; 0=Roman, 1=Reverse, 2=Bold, 4=Italic, 8=Fixed
      (let ((style (car ops)))
        (cond
         ((= style 0) (display "\033[0m"))
         ((= style 1) (display "\033[7m"))
         ((= style 2) (display "\033[1m"))
         ((= style 4) (display "\033[4m"))  ; italic as underline
         ((= style 8) 'ok))  ; fixed-pitch, ignore
        (flush-output-port (current-output-port)))))

  ;; VAR:18 buffer_mode flag [V4+]
  (vector-set! *op-var* 18
    (lambda (ops)
      (set! *buffering* (not (= (car ops) 0)))))

  ;; VAR:19 output_stream number
  (vector-set! *op-var* 19
    (lambda (ops)
      ;; Minimal: ignore stream selection
      'ok))

  ;; VAR:20 input_stream number
  (vector-set! *op-var* 20
    (lambda (ops)
      ;; Minimal: ignore
      'ok))

  ;; VAR:21 sound_effect
  (vector-set! *op-var* 21
    (lambda (ops)
      ;; Ignore sound effects
      'ok))

  ;; VAR:23 scan_table x table len form -> (result) ?(label) [V4+]
  (vector-set! *op-var* 23
    (lambda (ops)
      (let* ((x (car ops))
             (table (cadr ops))
             (len (caddr ops))
             (form (if (>= (length ops) 4) (cadddr ops) #x82))
             (field-size (bitwise-and form #x7F))
             (is-word (bit-set? 7 form))
             (result-var (read-store-target!)))
        (let loop ((i 0) (addr table))
          (if (>= i len)
              (begin
                (set-variable! result-var 0)
                (do-branch! #f))
              (let ((val (if is-word (mem-word addr) (mem-byte addr))))
                (if (= val x)
                    (begin
                      (set-variable! result-var addr)
                      (do-branch! #t))
                    (loop (+ i 1) (+ addr field-size)))))))))

  ;; VAR:24 not value -> (result) [V5+]
  (vector-set! *op-var* 24
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        (set-variable! result-var (bitwise-and (bitwise-not (car ops)) #xFFFF)))))

  ;; VAR:25 call_vn routine ...args... [V5+]
  (vector-set! *op-var* 25
    (lambda (ops)
      (z-call (car ops) (cdr ops) #f)))

  ;; VAR:27 tokenise text parse [V5+]
  (vector-set! *op-var* 27
    (lambda (ops)
      (tokenize-input (car ops) (cadr ops))))

  ;; VAR:29 copy_table first second size [V5+]
  (vector-set! *op-var* 29
    (lambda (ops)
      (let ((first (car ops))
            (second (cadr ops))
            (size (s16 (caddr ops))))
        (cond
         ((= second 0)
          ;; Zero out first table
          (let loop ((i 0))
            (when (< i (abs size))
              (mem-set-byte! (+ first i) 0)
              (loop (+ i 1)))))
         ((> size 0)
          ;; Copy forward (safe for non-overlapping or dest < src)
          (let loop ((i 0))
            (when (< i size)
              (mem-set-byte! (+ second i) (mem-byte (+ first i)))
              (loop (+ i 1)))))
         (else
          ;; Negative size: copy backward (for overlapping regions)
          (let ((asize (abs size)))
            (let loop ((i (- asize 1)))
              (when (>= i 0)
                (mem-set-byte! (+ second i) (mem-byte (+ first i)))
                (loop (- i 1))))))))))

  ;; VAR:30 print_table zscii-text width height skip [V5+]
  (vector-set! *op-var* 30
    (lambda (ops)
      (let* ((text (car ops))
             (width (cadr ops))
             (height (if (>= (length ops) 3) (caddr ops) 1))
             (skip (if (>= (length ops) 4) (cadddr ops) 0)))
        (let loop ((row 0) (addr text))
          (when (< row height)
            (let inner ((col 0) (a addr))
              (when (< col width)
                (z-print-char (integer->char (mem-byte a)))
                (inner (+ col 1) (+ a 1))))
            (when (< row (- height 1)) (z-newline))
            (loop (+ row 1) (+ addr width skip)))))))

  ;; VAR:31 check_arg_count argument-number [V5+]
  (vector-set! *op-var* 31
    (lambda (ops)
      (do-branch! (<= (car ops) *current-arg-count*))))

  ;; ---- Additional 2OP opcodes for V5+ ----

  ;; 2OP:25 call_2s routine arg1 -> (result) [V4+]
  (vector-set! *op-2op* 25
    (lambda (ops)
      (z-call (car ops) (list (cadr ops)) #t)))

  ;; 2OP:26 call_2n routine arg1 [V5+]
  (vector-set! *op-2op* 26
    (lambda (ops)
      (z-call (car ops) (list (cadr ops)) #f)))

  ;; 2OP:27 set_colour foreground background [V5+]
  ;; Z-machine: 2=black 3=red 4=green 5=yellow 6=blue 7=magenta 8=cyan 9=white
  ;; ANSI fg:   30=black 31=red 32=green 33=yellow 34=blue 35=magenta 36=cyan 37=white
  (vector-set! *op-2op* 27
    (lambda (ops)
      (let ((fg (car ops))
            (bg (cadr ops)))
        (when (= fg 1) (display "\033[39m"))   ; default fg
        (when (= bg 1) (display "\033[49m"))   ; default bg
        (when (and (>= fg 2) (<= fg 9))
          (display (string-append "\033[" (number->string (+ 28 fg)) "m")))
        (when (and (>= bg 2) (<= bg 9))
          (display (string-append "\033[" (number->string (+ 38 bg)) "m")))
        (flush-output-port (current-output-port)))))

  ;; 2OP:28 throw value stack-frame [V5+]
  (vector-set! *op-2op* 28
    (lambda (ops)
      (let ((value (car ops))
            (frame-num (cadr ops)))
        ;; Unwind call stack until we reach the target frame
        (let loop ()
          (when (> (length *call-stack*) frame-num)
            (pop-frame!)
            (loop)))
        ;; Now return from the target frame
        (do-return! value))))

  ;; ---- Additional VAR opcodes for V5+ ----

  ;; VAR:12 call_vs2 routine ...0 to 7 args... -> (result) [V4+]
  (vector-set! *op-var* 12
    (lambda (ops)
      (z-call (car ops) (cdr ops) #t)))

  ;; VAR:26 call_vn2 routine ...0 to 7 args... [V5+]
  (vector-set! *op-var* 26
    (lambda (ops)
      (z-call (car ops) (cdr ops) #f)))

  ;; VAR:28 encode_text zscii-text length from coded-text [V5+]
  (vector-set! *op-var* 28
    (lambda (ops)
      (let* ((zscii-addr (car ops))
             (len (cadr ops))
             (from (caddr ops))
             (coded-addr (cadddr ops))
             ;; Read the substring
             (text (let loop ((i 0) (chars '()))
                     (if (>= i len)
                         (list->string (reverse chars))
                         (loop (+ i 1)
                               (cons (integer->char (mem-byte (+ zscii-addr from i))) chars)))))
             (encoded (encode-ztext text)))
        ;; Write encoded bytes to coded-addr
        (let loop ((i 0))
          (when (< i (bytevector-length encoded))
            (mem-set-byte! (+ coded-addr i) (bytevector-u8-ref encoded i))
            (loop (+ i 1)))))))

  ;; ---- EXT opcodes ----

  ;; EXT:0 save table bytes name prompt -> (result) [V5+]
  (vector-set! *op-ext* 0
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        ;; Stub: always fail
        (set-variable! result-var 0))))

  ;; EXT:1 restore table bytes name prompt -> (result) [V5+]
  (vector-set! *op-ext* 1
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        ;; Stub: always fail
        (set-variable! result-var 0))))

  ;; EXT:2 log_shift number places -> (result) [V5+]
  (vector-set! *op-ext* 2
    (lambda (ops)
      (let* ((result-var (read-store-target!))
             (number (car ops))
             (places (s16 (cadr ops)))
             (result (if (>= places 0)
                         (arithmetic-shift number places)
                         ;; Logical right shift: mask to 16-bit first to prevent sign extension
                         (arithmetic-shift (bitwise-and number #xFFFF) places))))
        (set-variable! result-var (u16 result)))))

  ;; EXT:3 art_shift number places -> (result) [V5+]
  (vector-set! *op-ext* 3
    (lambda (ops)
      (let* ((result-var (read-store-target!))
             (number (s16 (car ops)))
             (places (s16 (cadr ops)))
             (result (if (>= places 0)
                         (arithmetic-shift number places)
                         ;; Arithmetic right shift: sign bit preserved
                         (arithmetic-shift number places))))
        (set-variable! result-var (u16-from-signed result)))))

  ;; EXT:4 set_font font -> (result) [V5+]
  (vector-set! *op-ext* 4
    (lambda (ops)
      (let ((result-var (read-store-target!))
            (font (car ops)))
        ;; Only support font 1 (normal) and 4 (fixed-pitch)
        (cond
         ((or (= font 1) (= font 4))
          (set-variable! result-var 1))  ; return previous font (always 1)
         (else
          (set-variable! result-var 0))))))  ; 0 = font not available

  ;; EXT:9 save_undo -> (result) [V5+]
  (vector-set! *op-ext* 9
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        ;; -1 = interpreter doesn't support undo
        (set-variable! result-var (u16-from-signed -1)))))

  ;; EXT:10 restore_undo -> (result) [V5+]
  (vector-set! *op-ext* 10
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        ;; 0 = failed
        (set-variable! result-var 0))))

  ;; EXT:11 print_unicode char-number [V5+]
  (vector-set! *op-ext* 11
    (lambda (ops)
      (let ((code (car ops)))
        (if (and (>= code 32) (<= code #xFFFF))
            (z-print-char (integer->char code))
            (z-print-char #\?)))))

  ;; EXT:12 check_unicode char-number -> (result) [V5+]
  (vector-set! *op-ext* 12
    (lambda (ops)
      (let ((result-var (read-store-target!)))
        ;; Bit 0: can print, Bit 1: can read
        ;; Claim we can print and read ASCII range
        (let ((code (car ops)))
          (set-variable! result-var (if (and (>= code 32) (<= code 126)) 3 1))))))

  'opcodes-initialized)

;;;
;;; Call implementation
;;;

(define (z-call packed-addr args store?)
  (if (= packed-addr 0)
      ;; Call to address 0 returns false
      (when store?
        (let ((result-var (read-store-target!)))
          (set-variable! result-var 0)))
      ;; Real call
      (let* ((result-var (if store? (read-store-target!) #f))
             (routine-addr (unpack-addr packed-addr))
             (num-locals (mem-byte routine-addr))
             (locals (make-vector 15 0)))
        ;; Read default local values (V1-4: stored in routine header)
        (when (<= *version* 4)
          (let loop ((i 0))
            (when (< i num-locals)
              (vector-set! locals i (mem-word (+ routine-addr 1 (* 2 i))))
              (loop (+ i 1)))))
        ;; Overwrite locals with arguments
        (let loop ((i 0) (a args))
          (when (and (< i num-locals) (not (null? a)))
            (vector-set! locals i (car a))
            (loop (+ i 1) (cdr a))))
        ;; Push frame (also saves current arg count)
        (push-frame! *pc* result-var locals num-locals *stack* *current-arg-count*)
        ;; Set arg count for new routine
        (set! *current-arg-count* (length args))
        ;; New empty stack for the routine
        (set! *stack* '())
        ;; Set PC to first instruction after locals
        (let ((header-size (+ 1 (if (<= *version* 4) (* 2 num-locals) 0))))
          (set! *pc* (+ routine-addr header-size))))))

;;;
;;; Helper: any
;;;

(define (any pred lst)
  (cond
   ((null? lst) #f)
   ((pred (car lst)) #t)
   (else (any pred (cdr lst)))))

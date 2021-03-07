;; parse.scm -- Parser Combinators
;;
;; This file provides Scheme parser combinators for binary data. This
;; implementation is based on (chibi parse) as provided by the
;; chibi-scheme <https://github.com/ashinn/chibi-scheme> R7RS Scheme
;; implementation. The original code was written by Alex Shinn
;; (license below).

;; Copyright (C) 2000-2015 Alex Shinn. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Copyright (C) 2021 Sören Tempel
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;> \section{Parse Streams}

;;> Parse streams are an abstraction to treat ports as proper streams
;;> so that we can backtrack from previous states.  A single
;;> Parse-Stream record represents a single buffered chunk of text.

(define-record-type Parse-Stream
  (%make-parse-stream
   filename port buffer offset prev-byte tail)
  parse-stream?
  ;; The file the data came from, for debugging and error reporting.
  (filename parse-stream-filename)
  ;; The underlying port.
  (port parse-stream-port)
  ;; A bytevector of characters read from the port.
  (buffer parse-stream-buffer)
  ;; The current offset of filled characters in the buffer.
  ;; If offset is non-zero, (bytevector-u8-ref buffer (- offset 1)) is
  ;; valid.
  (offset parse-stream-offset parse-stream-offset-set!)
  ;; The previous byte before the beginning of this Parse-Stream.
  ;; Used for line/word-boundary checks.
  (prev-byte parse-stream-prev-byte)
  ;; The successor Parse-Stream chunk, created on demand and filled
  ;; from the same port.
  (tail %parse-stream-tail %parse-stream-tail-set!))

;; We want to balance avoiding reallocating buffers with avoiding
;; holding many memoized values in memory.
(define default-buffer-size 256)

;;> Create a parse stream open on the given \var{filename}, with a
;;> possibly already opened \var{port}.

(define (make-parse-stream filename . o)
  (let ((port (if (pair? o) (car o) (open-input-file filename)))
        (len (if (and (pair? o) (pair? (cdr o)))
               (cadr o)
               default-buffer-size)))
    (%make-parse-stream
     filename port (make-bytevector len 0) 0 #f #f)))

;;> Create a parse stream on a bytevector \var{bv}.

(define (bytevector->parse-stream bv)
  (make-parse-stream #f (open-input-bytevector bv)))

;;> Access the next buffered chunk of a parse stream.

(define (parse-stream-tail source)
  (or (%parse-stream-tail source)
      (let* ((len (vector-length (parse-stream-buffer source)))
             (tail (%make-parse-stream (parse-stream-filename source)
                                       (parse-stream-port source)
                                       (make-bytevector len 0)
                                       0
                                       (parse-stream-last-byte source)
                                       #f)))
        (%parse-stream-tail-set! source tail)
        tail)))

;;> Fill the buffer of the given parse \var{source} with up to
;;> \var{i} bytes.

(define (parse-stream-fill! source i)
  (let ((off (parse-stream-offset source))
        (buf (parse-stream-buffer source)))
    (if (<= off i)
        (do ((off off (+ off 1)))
            ((> off i) (parse-stream-offset-set! source off))
          (bytevector-u8-set! buf off (read-u8 (parse-stream-port source))))
        #f)))

;;> Returns the byte in parse stream \var{source} indexed by \var{i}.

(define (parse-stream-ref source i)
  (parse-stream-fill! source i)
  (bytevector-u8-ref (parse-stream-buffer source) i))

;;> Return last byte in parse stream \var{source}.

(define (parse-stream-last-byte source)
  (let ((buf (parse-stream-buffer source)))
    (let lp ((i (min (- (bytevector-length buf) 1)
                     (parse-stream-offset source))))
      (if (negative? i)
          (parse-stream-prev-byte source)
          (let ((ch (bytevector-u8-ref buf i)))
            (if (eof-object? ch)
                (lp (- i 1))
                ch))))))

;;> TODO.

(define (parse-stream-next-source source i)
  (if (>= (+ i 1) (bytevector-length (parse-stream-buffer source)))
      (parse-stream-tail source)
      source))

;;> TODO.

(define (parse-stream-next-index source i)
  (if (>= (+ i 1) (bytevector-length (parse-stream-buffer source)))
      0
      (+ i 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Parser Interface}

;;> Call the parser combinator \var{f} on the parse stream
;;> \var{source}, starting at index \var{index}, passing the result to
;;> the given success continuation \var{sk}, which should be a
;;> procedure of the form \scheme{(result source index fail)}.  The
;;> optional failure continuation should be a procedure of the form
;;> \scheme{(source index reason)}, and defaults to just returning
;;> \scheme{#f}.

(define (call-with-parse f source index sk . o)
  (let ((s (if (bytevector? source)
             (bytevector->parse-stream source)
             source))
        (fk (if (pair? o)
              (car o)
              (lambda (s i reason) #f))))
    (f s index sk fk)))

;;> Call the parser combinator \var{f} on the parse stream
;;> \var{source}, at index \var{index}, and return the result, or
;;> \scheme{#f} if parsing fails.

(define (parse f source . o)
  (let ((index (if (pair? o) (car o) 0)))
    (call-with-parse f source index (lambda (r s i fk) r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Constant Parsers}

;;> Parse nothing successfully.

(define parse-epsilon
  (lambda (source index sk fk)
    (sk #t source index fk)))

;;> Always fails to parse with \var{msg}.

(define (parse-fail msg)
  (lambda (source index sk fk)
    (fk source index "unexpected tag in tagged union")))

;;> Parse byte if it statifies the predicate \var{pred}, fail otherwise.

(define (parse-pred pred)
  (lambda (source index sk fk)
    (let ((byte (parse-stream-ref source index)))
      (if (pred byte)
          (sk byte
              (parse-stream-next-source source index)
              (parse-stream-next-index source index)
              fk)
          (fk source index "failed predicate")))))

;;> Parse next byte.

(define parse-byte
  (parse-pred (lambda (x) #t)))

;;> Parse bytevector of the given \var{size} in bytes. Returns a
;;> bytevector of the given size, not a list.

(define (parse-bytevector size)
  (parse-map
    (apply parse-seq
           (make-list size parse-byte))
    (lambda (lst)
      (apply bytevector lst))))

;;> TODO.

(define (parse-with-context ctx f)
  (define yield (lambda (r s i fk) r))

  (lambda (source index sk fk)
    ;; call-with-parse modifies source and needs to be called first.
    (let* ((size (call-with-parse ctx source index yield fk))
           (field-start (parse-stream-offset source)))
      (if size
        ((f size) source field-start sk fk)
        (fk source index "expected field of given size")))))

;;> TODO.

(define ignored-value (list 'ignore))

;;> TODO.

(define (parse-seq-list o)
  (cond
   ((null? o)
    parse-epsilon)
   ((null? (cdr o))
    (let ((f (car o)))
      (lambda (s i sk fk)
        (f s i (lambda (r s i fk) (sk (list r) s i fk)) fk))))
   (else
    (let* ((f (car o))
           (o (cdr o))
           (g (car o))
           (o (cdr o))
           (g (if (pair? o)
                  (apply parse-seq g o)
                  (lambda (s i sk fk)
                    (g s i (lambda (r s i fk) (sk (list r) s i fk)) fk)))))
      (lambda (source index sk fk)
        (f source
           index
           (lambda (r s i fk)
             (g s i (lambda (r2 s i fk)
                      (let ((r2 (if (eq? r ignored-value) r2 (cons r r2))))
                        (sk r2 s i fk)))
                fk))
           fk))))))

;;> The sequence combinator. Each combinator is applied in turn just
;;> past the position of the previous. If all succeed, returns a list
;;> of the results in order, skipping any ignored values.

(define (parse-seq . o)
  (parse-seq-list o))

;;> The repetition combinator.  Parse \var{f} repeatedly and return a
;;> list of the results.  \var{lo} is the minimum number of parses
;;> (deafult 0) to be considered a successful parse, and \var{hi} is
;;> the maximum number (default infinite) before stopping.

(define (parse-repeat f . o)
  (let ((lo (if (pair? o) (car o) 0))
        (hi (and (pair? o) (pair? (cdr o)) (cadr o))))
    (lambda (source0 index0 sk fk)
      (let repeat ((source source0) (index index0) (fk fk) (j 0) (res '()))
        (let ((fk (if (>= j lo)
                      (lambda (s i r) (sk (reverse res) source index fk))
                      fk)))
          (if (and hi (= j hi))
              (sk (reverse res) source index fk)
              (f source
                 index
                 (lambda (r s i fk) (repeat s i fk (+ j 1) (cons r res)))
                 fk)))))))

;;> Parse \var{f} and apply the procedure \var{proc} to the result on success.

(define (parse-map f proc)
  (lambda (source index sk fk)
    (f source index (lambda (res s i fk) (sk (proc res) s i fk)) fk)))

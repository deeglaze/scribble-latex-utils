#lang at-exp racket

(require scribble/eval scribble/core scribble/sigplan)
(require scribble/manual
         scribble/decode
         scribble/bnf
         scribble/racket
         scribble/latex-properties
         scheme/string
         "unmap.rkt"
         (for-syntax syntax/id-table syntax/parse)
         (only-in scribble/struct make-flow make-omitable-paragraph flow-paragraphs
                  make-blockquote make-styled-paragraph)
         (for-label racket))

(provide (all-defined-out))

(define exact-style (make-style "identity" '(exact-chars)))

(define (exact . items)
  (make-element exact-style (map content->latex-content items)))

(define-syntax-rule (m items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "$" items ... "$"))]))
(define-syntax-rule (mp items ...)
  (cond [(math-mode) (exact items ...)]
        [else (in-math (exact "\\[" items ... "\\]"))]))
(define-syntax-rule (um items ...)
  (cond [(math-mode) (unmath (exact "\\mbox{" items ... "}"))]
        [else (exact items ...)]))

(define bg-color-style
  (make-style "BgColor" (list (make-tex-addition "bgcolor.tex"))))
(define (graybox elm) (make-element bg-color-style elm))

(struct bracket (element))
(struct curlies (element))
(struct parens (element))
(define (interpret-option option)
  (match option
    [(bracket e) `("[" ,e "]")]
    [(curlies e) `("{" ,e "}")]
    [(parens e) `("(" ,e ")")]))

(define (env t #:opt [optional '()] . items)
  (apply exact `("\\begin{" ,t "}"
                 ,@(append-map interpret-option optional)
                 ,@items
                 "\\end{" ,t "}")))
(define (tenv t title items)
  (keyword-apply env '() '() t items #:opt (list (bracket title))))

(define (lstlisting #:math-escape? [math-escape? #f] . items)
  (list (make-element (make-style "setbox\\mylistings=\\vbox" '(exact-chars))
                      (list "\n\\begin{lstlisting}"
                            (cond [math-escape? "[mathescape]\n"]
                                  [else "\n"])
                            (parameterize ([math-mode #t])
                              (map content->latex-content items))
                            "\n\\end{lstlisting}\n"))
        #;(make-element (make-style "copy\\mylistings" '(exact-chars)) "")
        (make-element (make-style "box\\mylistings" '(exact-chars)) "")))

(define (lstset #:basicstyle [basicstyle #f]
                #:keywordstyle [keywordstyle #f]
                #:identifierstyle [identifierstyle #f]
                #:commentstyle [commentstyle #f]
                #:stringstyle [stringstyle #f]
                #:showstringspaces [showstringspaces #f]
                #:numbers [numbers #f]
                #:numberstyle [numberstyle #f]
                #:stepnumber [stepnumber #f]
                #:numbersep [numbersep #f]
                #:backgroundcolor [backgroundcolor #f]
                #:showspaces [showspaces #f]
                #:showtabs [showtabs #f]
                #:frame [frame #f]
                #:label [label #f]
                #:rulecolor [rulecolor #f]
                #:tabsize [tabsize #f]
                #:language [language #f]
                #:caption [caption #f]
                #:captionpos [captionpos #f]
                #:breaklines [breaklines #f]
                #:breakatwhitespace [breakatwhitespace #f]
                #:title [title #f]
                #:escapeinside [escapeinside #f]
                #:morekeywords [morekeywords #f]
                #:moredelim [moredelim #f])
  (define key-values 
    `(;; styling
      ("basicstyle" . ,basicstyle)
      ("keywordstyle" . ,keywordstyle)
      ("identifierstyle" . ,identifierstyle)
      ("commentstyle" . ,commentstyle)
      ("stringstyle" . ,stringstyle)
      ;; line numbering
      ("numbers" . ,numbers)
      ("numberstyle" . ,numberstyle)
      ("stepnumber" . ,stepnumber)
      ("numbersep" . ,numbersep)
      ;; display
      ("backgroundcolor" . ,backgroundcolor)
      ("rulecolor" . ,rulecolor)
      ("frame" . ,frame)
      ;; spacing
      ("showstringspaces" . ,showstringspaces)
      ("showspaces" . ,showspaces)
      ("showtabs" . ,showtabs)
      ("tabsize" . ,tabsize)
      ;; line breaking
      ("breaklines" . ,breaklines)
      ("breakatwhitespace" . ,breakatwhitespace)
      ;; legend
      ("title" . ,title)
      ("caption" . ,caption)
      ("captionpos" . ,captionpos)
      ("label" . ,label)
      ;; special
      ("language" . ,language)
      ;; extra
      ("escapeinside" . ,escapeinside)
      ("morekeywords" . ,morekeywords)
      ("moredelim" . ,moredelim)))
  (make-element (make-style "lstset" '(exact-chars))
                (string-join 
                 (foldr (λ (pair acc)
                           (match-define (cons key val) pair)
                           (cond [val
                                  (cons (format "~a=~a" key val)
                                        acc)]
                                 [else acc]))
                        '() key-values)
                 ",\n")))

(define (array style . items)
  (keyword-apply env '() '() "array" items #:opt (list (curlies style))))

;; For working with Jesse's pfsteps library
(define (byCases . items) (apply env "byCases" items))
(define (pfsteps* . items) (apply env "pfsteps*" items))
(define-syntax-rule (bc-case title items ...)
  (exact "\\case{" (in-math (um title)) "}" items ...))
(define (bc-otherwise . items)
  (apply exact `("\\otherwise{}" ,@items)))

(define (tagit tag . items)
  (cond [tag (cons (exact `("\\label{" ,tag "}")) items)]
        [else items]))

(define (parblock env title tag items)
  (make-compound-paragraph exact-style
                           (append (list (make-paragraph exact-style
                                                         (exact `("\\begin{" ,env "}"
                                                                  ,@(if title
                                                                        `("[" ,title "]")
                                                                        '())))))
                                   (map content->block (collapse-content (apply tagit tag items)))
                                   (list (make-paragraph exact-style (exact `("\\end{" ,env "}")))))))

(define (mdef title #:tag [tag #f] . items)
  (tenv "definition" title (apply tagit tag items)))
(define (mthm title #:tag [tag #f] . items) (tenv "theorem" title (apply tagit tag items)))
(define (unthm title #:tag [tag #f] . items) (tenv "untheorem" title (apply tagit tag items)))

(define (mlem title #:tag [tag #f] . items)
  (tenv "lemma" title (apply tagit tag items)))
(define (mprop title #:tag [tag #f] . items)
  (tenv "property" title (apply tagit tag items)))
(define (parlem title #:tag [tag #f] . items)
  (parblock "lemma" title tag items))
(define (parthm title #:tag [tag #f] . items)
  (parblock "theorem" title tag items))
(define (parprf #:tag [tag #f] . items)
  (parblock "proof" #f tag items))
(define (ntthm . items) (apply env "theorem" items))
(define (ntlem . items) (apply env "lemma" items))
(define (ntprf . items) (apply env "proof" items))
(define (tprf title . items) (tenv "proof" title items))
(define (mcor title . items) (tenv "corollary" title items))
(define (mnotation title . items) (tenv "notation" title items))
;; align* is a special LaTeX environment that puts its body directly in a math mode context.
(define-syntax-rule (envalign* items ...)
  (in-math (env "align*" items ...)))

(define (content->block c)
  (if (content? c)
      (make-paragraph exact-style c)
      c))
(define (collapse-content items)
  (let recur ([items items]
              [current '()]
              [all '()])
    (define (extend)
      (if (empty? current)
          all
          (cons (reverse current) all)))
    (cond [(empty? items) (reverse (extend))]
          [(content? (car items))
           (recur (cdr items) (cons (car items) current) all)]
          [else (recur (cdr items) '() (cons (car items) (extend)))])))

(define-syntax (for/append stx)
  (syntax-case stx ()
    [(_ clauses body1 body ...)
     (syntax/loc stx (reverse (for/fold/derived stx ([res '()]) clauses
                                 (let ([app (let () body1 body ...)]) (append (reverse app) res)))))]))

(define (zip-kvs lst)
  (cond [(empty? lst) '()]
        [(empty? (rest lst)) (error 'zip-kvs "Expected even number of arguments")]
        [else (cons (cons (first lst) (second lst)) (zip-kvs (cddr lst)))]))

(define-syntax (sep-rows stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let ([rows (reverse
                  (let recur ([rows (map syntax->list (syntax->list #'((args ...) ...)))]
                              [acc '()])
                    (define (dorow row last? acc)
                      (cond [(null? row) acc]
                            [(null? (cdr row))
                             (cond [last? (cons (car row) acc)]
                                   [else (list* "\\\\" (car row) acc)])]
                            [else (dorow (cdr row) last? (list* "&" (car row) acc))]))
                    (cond [(null? rows) acc]
                          [else (recur (cdr rows)
                                       (append (dorow (car rows) (null? (cdr rows)) '())
                                               acc))])))])
       (quasisyntax/loc stx (#,@rows)))]))

(define-syntax (align* stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let ([rows (local-expand #'(sep-rows (args ...) ...) 'expression #f)])
       #`(envalign* #,@rows))]))

(define-syntax (style-matrix stx)
  (define lut (make-free-id-table (hasheq #'l #\l #'r #\r #'c #\c #'bar #\|)))
  (define-syntax-class style
    #:attributes (kind)
    (pattern x:id #:attr kind (free-id-table-ref lut #'x #f) #:when (attribute kind)))
  (syntax-parse stx
    [(_ (s:style ...) (args ...) ...)
     (let* ([argss (map syntax->list (syntax->list #'((args ...) ...)))]
            [n (length argss)]
            [_ (when (zero? n) (raise-syntax-error #f "matrix needs at least one row." stx))]
            [m (length (car argss))]
            [ss (attribute s.kind)])
       (unless (for/and ([arg (in-list (cdr argss))])
                 (= m (length arg)))
         (raise-syntax-error #f "matrix needs same number of columns in each row." stx))
       (let ([rows (local-expand #'(sep-rows (args ...) ...) 'expression #f)])
         (quasisyntax/loc stx (array #,(list->string ss) #,@rows))))]))

(define-syntax (matrix stx)
  (syntax-case stx ()
    [(_ (args ...) ...)
     (let* ([argss (map syntax->list (syntax->list #'((args ...) ...)))]
            [_ (when (null? argss) (raise-syntax-error #f "matrix needs at least one row." stx))]
            [m (length (car argss))]
            [style (datum->syntax stx (build-list m (λ _ #'l)))])
       (quasisyntax/loc stx (style-matrix #,style (args ...) ...)))]))

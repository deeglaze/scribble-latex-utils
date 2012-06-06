#lang racket/base

(require racket/port scribble/core racket/match racket/contract)
(provide/contract
[content->latex-content
 (->* (content?) (#:operators any/c) content?)])
(provide overload-op* math-mode in-math unmath conv-file)

(define math-mode (make-parameter #f))
(define unsafe-math-mode (make-parameter #f))
(define debug-prev-output #f)
(define (prev-printf . args)
  (parameterize ([current-output-port debug-prev-output])
    (apply printf args)))

;; avoid double superscript errors
(define (consume-all-primes str i)
  (define i* i)
  (define primestr
    (with-output-to-string
      (λ ()
         (let/ec break
           (for* ([j (in-range (+ i 1) (string-length str))]
                  [c (in-value (string-ref str j))])
             (cond [(eqv? c #\')
                    (display c)
                    (set! i* j)]
                   [else (break (void))]))))))
  (values primestr i*))

(define ((wrap-prime-consumption fmt) c str i)
  (define-values (primestr i*) (consume-all-primes str i))
  (values (mathify (format fmt (in-math (translate-char c)) primestr))
          (+ i* 1)))

(define default-ops
  (hasheqv #\u0302 (λ (c str i)
                      (values (mathify (format "\\hat{~a}{}" (in-math (translate-char c))))
                              (+ i 1)))
          #\u0303 (λ (c str i)
                      (values (mathify (format "\\tilde{~a}{}" (in-math (translate-char c))))
                              (+ i 1)))
          #\u0307 (λ (c str i)
                     (values (cond [(math-mode) (mathify (format "\\dot{~a}" (translate-char c)))]
                                   [else (format "\\.{~a}" (translate-char c))])
                             (+ i 1)))
          #\u0338 (λ (c str i)
                     (values (mathify (format "\\centernot{~a}" (in-math (translate-char c))))
                             (+ i 1)))))

(define (overload-op* . kvs)
  (let recur ([kvs kvs])
    (cond [(null? kvs) default-ops]
          [(null? (cdr kvs))
           (error 'overload-op* "expected key-value pair, got uneven amount of arguments")]
          [else (hash-set (overload-op* (cddr kvs)) (car kvs) (cadr kvs))])))

(define (content->latex-content e0 #:operators [ophash default-ops])
  (let recur ([e e0])
    (cond [(string? e) (string->latex-string e ophash)]
          [(element? e)
           (make-element (element-style e)
                         (recur (element-content e)))]
          [(multiarg-element? e)
           (make-multiarg-element (multiarg-element-style e)
                                  (map recur (multiarg-element-contents e)))]
          [(traverse-element? e)
           (make-traverse-element (λ (f g)
                                     (define trav ((traverse-element-traverse) f g))
                                     (cond [(content? trav) (recur trav)]
                                           [else (make-traverse-element trav)])))]
          [(delayed-element? e) (error "unsupported")]
          [(part-relative-element? e) (error "unsupported")]
          [(list? e) (map recur e)]
          [else (error 'content->latex-content "non-content ~a" e0)])))

(define-syntax-rule (unmath e1 e ...) (parameterize ([math-mode #f]) e1 e ...))
(define-syntax-rule (in-math e1 e ...) (parameterize ([math-mode #t]) e1 e ...))
(define-syntax-rule (in-unsafe-math e1 e ...) (parameterize ([unsafe-math-mode #t] [math-mode #t]) e1 e ...))

(define (conv-file in-file out-file)
  (define fstr (with-input-from-file in-file port->string))
  (with-output-to-file out-file (process-string fstr default-ops) #:mode 'text))

(define (string->latex-string str ophash)
  (set! debug-prev-output (current-output-port))
  (with-output-to-string (process-string str ophash)))

(define (process-string str ophash)
  (λ ()
     (define strl (string-length str))
     (let recur ([i 0])
       (unless (>= i strl)
         (define c (string-ref str i))
         (cond [(= i (- strl 1)) (display (translate-char c))]
               [else
                (define look-ahead (string-ref str (+ i 1)))
                (define (non-composite c d i) (values (translate-char c) i))
                (define-values (s i*)
                  ((hash-ref ophash look-ahead (λ () non-composite))
                   c str (+ i 1)))
                (display s)
                (recur i*)])))))

(define (mathify str)
  (cond [(unsafe-math-mode) (unsafe-mathify str)]
        [(math-mode) (format "~a" str)]
        [else (format "${~a}$" str)]))
(define (unsafe-mathify str)
  (cond [(math-mode) (format "~a" str)]
        [else (format "$~a$" str)]))

(define (translate-char c)
  (case c
    ;; Don't escape latex commands
    #;[(#\\) (mathify "\\backslash")]
    #;[(#\_) (mathify "\\_")]
    #;[(#\^) "{\\char'136}"]
    #;[(#\>) (mathify ">")]
    #;[(#\<) (if (rendering-tt) "{\\texttt <}" "$<$")]
    #;[(#\|) (if (rendering-tt) "{\\texttt |}" "$|$")]
    #;[(#\-) "{-}"] ;; avoid en- or em-dash
    #;[(#\`) "{`}"] ;; avoid double-quotes
    #;[(#\') "{'}"] ;; avoid double-quotes
    #;[(#\? #\! #\. #\:)
    (if (rendering-tt) (format "{\\hbox{\\texttt{~a}}}" c) c)]
    #;[(#\~) "$\\sim$"]
    #;[(#\{ #\}) (if (rendering-tt)
    (format "{\\char`\\~a}" c)
    (format "\\~a" c))]
            #;[(#\[ #\]) (if (escape-brackets)
                           (if (eq? c #\[)
                               "{\\SOpenSq}"
                               "{\\SCloseSq}")
                           c)]
            #;[(#\# #\% #\& #\$) (format "\\~a" c)]
    [(#\uA0) "~"] ; non-breaking space
    [(#\uAD) "\\-"] ; soft hyphen; unfortunately, also disables auto-hyphen
    [(#\uDF) "{\\ss}"]
    [else
     (if ((char->integer c) . > . 127)
         (case c
           [(#\u2011) "\\mbox{-}"] ; non-breaking hyphen
           [(#\uB0) (mathify "^{\\circ}")] ; degree
           [(#\uB2) (mathify "^2")]
           [(#\u039A) "K"] ; kappa
           [(#\u0391) "A"] ; alpha
           [(#\u039F) "O"] ; omicron
           [(#\u03A3) (mathify "\\Sigma")]
           [(#\u03BA) (mathify "\\kappa")]
           [(#\u03B1) (mathify "\\alpha")]
           [(#\u03B2) (mathify "\\beta")]
           [(#\u03B3) (mathify "\\gamma")]
           [(#\u03BF) "o"] ; omicron
           [(#\u03C3) (mathify "\\sigma")]
           [(#\u03C2) (mathify "\\varsigma")]
           [(#\u03BB) (mathify "\\lambda")]
           [(#\u039B) (mathify "\\Lambda")]
           [(#\u03BC) (mathify "\\mu")]
           [(#\u03C0) (mathify "\\pi")]
           [(#\‘) "{`}"]
           [(#\’) "{'}"]
           [(#\“) "{``}"]
           [(#\”) "{''}"]
           [(#\u2013) "{--}"]
           [(#\u2014) "{---}"]
           [(#\∞) (mathify "\\infty")]
           ;; Arrows
           [(#\→) (mathify "\\rightarrow")]
           [(#\←) (mathify "\\leftarrow")]
           [(#\↑) (mathify "\\uparrow")]
           [(#\↓) (mathify "\\downarrow")]
           [(#\⇒) (mathify "\\Rightarrow")]
           [(#\⇐) (mathify "\\Leftarrow")]
           [(#\⇑) (mathify "\\Uparrow")]
           [(#\⇓) (mathify "\\Downarrow")]
           [(#\⇚) (mathify "\\Lleftarrow")]
           [(#\⇛) (mathify "\\Rrightarrow")]
           [(#\↖) (mathify "\\nwarrow")]
           [(#\↘) (mathify "\\searrow")]
           [(#\↗) (mathify "\\nearrow")]
           [(#\↙) (mathify "\\swarrow")]
           [(#\↝) (mathify "\\leadsto")]
           [(#\↜) (mathify "FIND")]
           [(#\⇢) (mathify "\\dashrightarrow")]
           [(#\⇠) (mathify "\\dashleftarrow")]
           [(#\⇝) (mathify "\\rightsquigarrow")]
           [(#\⇜) (mathify "\\leftsquigarrow")]
           [(#\⟶) (mathify "\\longrightarrow")]
           [(#\⇔) (mathify "\\Leftrightarrow")]
           [(#\↕) (mathify "\\updownarrow")]
           [(#\↔) (mathify "\\leftrightarrow")]
           [(#\⇕) (mathify "\\Updownarrow")]
           [(#\↦) (mathify "\\mapsto")]
           [(#\⤇) (mathify "\\Mapsto")]
           [(#\⟼) (mathify "\\longmapsto")]
           [(#\⟾) (mathify "\\Longmapsto")]
           ;; Harpoons
           [(#\⇀) (mathify "\\rightharpoonup")]
           [(#\↼) (mathify "\\leftharpoonup")]
           [(#\↾) (mathify "\\upharpoonright")]
           [(#\↿) (mathify "\\upharpoonleft")]
           [(#\⇂) (mathify "\\downharpoonright")]
           [(#\⇃) (mathify "\\downharpoonleft")]
           ;; End Harpoons
           [(#\א) (mathify "\\aleph")]
           [(#\′) (mathify "\\prime")]
           [(#\∅) (mathify "\\emptyset")]
           [(#\∇) (mathify "\\nabla")]
           [(#\♦) (mathify "\\diamondsuit")]
           [(#\♠) (mathify "\\spadesuit")]
           [(#\♣) (mathify "\\clubsuit")]
           [(#\♥) (mathify "\\heartsuit")]
           [(#\♯) (mathify "\\sharp")]
           [(#\♭) (mathify "\\flat")]
           [(#\♮) (mathify "\\natural")]
           [(#\√) (mathify "\\surd")]
           [(#\¬) (mathify "\\neg")]
           [(#\△) (mathify "\\triangle")]
           [(#\∀) (mathify "\\forall")]
           [(#\∃) (mathify "\\exists")]
           [(#\∄) (mathify "\\nexists")]
           [(#\∘) (mathify "\\circ")]
           [(#\·) (mathify "\\cdot")]
           [(#\θ) (mathify "\\theta")]
           [(#\τ) (mathify "\\tau")]
           [(#\υ) (mathify "\\upsilon")]
           [(#\φ) (mathify "\\phi")]
           [(#\δ) (mathify "\\delta")]
           [(#\ρ) (mathify "\\rho")]
           [(#\ε) (mathify "\\epsilon")]
           [(#\χ) (mathify "\\chi")]
           [(#\ψ) (mathify "\\psi")]
           [(#\ζ) (mathify "\\zeta")]
           [(#\ν) (mathify "\\nu")]
           [(#\ω) (mathify "\\omega")]
           [(#\η) (mathify "\\eta")]
           [(#\ι) (mathify "\\iota")]
           [(#\ξ) (mathify "\\xi")]
           [(#\Γ) (mathify "\\Gamma")]
           [(#\Ψ) (mathify "\\Psi")]
           [(#\∆) (mathify "\\Delta")]
           [(#\Δ) (mathify "\\Delta")]
           [(#\Ξ) (mathify "\\Xi")]
           [(#\Υ) (mathify "\\Upsilon")]
           [(#\Ω) (mathify "\\Omega")]
           [(#\Θ) (mathify "\\Theta")]
           [(#\Π) (mathify "\\Pi")]
           [(#\Φ) (mathify "\\Phi")]
           [(#\±) (mathify "\\pm")]
           [(#\∩) (mathify "\\cap")]
           [(#\◇) (mathify "\\diamond")]
           [(#\⊕) (mathify "\\oplus")]
           [(#\⨁) (unsafe-mathify "\\bigoplus")]
           [(#\∓) (mathify "\\mp")]
           [(#\∪) (mathify "\\cup")]
           [(#\△) (mathify "\\bigtriangleup")]
           [(#\⊖) (mathify "\\ominus")]
           [(#\×) (mathify "\\times")]
           [(#\⊎) (mathify "\\uplus")]
           [(#\▽) (mathify "\\bigtriangledown")]
           [(#\⊗) (mathify "\\otimes")]
           [(#\÷) (mathify "\\div")]
           [(#\▹) (mathify "\\triangleleft")]
           [(#\⊘) (mathify "\\oslash")]
           [(#\∗) (mathify "\\ast")]
           [(#\⊓) (mathify "\\sqcap")]
           [(#\⋂) (unsafe-mathify "\\bigcap")]
           [(#\∏) (unsafe-mathify "\\bigsqcap")]
           [(#\⊔) (mathify "\\sqcup")]
           [(#\⋃) (unsafe-mathify "\\bigcup")]
           [(#\∐) (unsafe-mathify "\\bigsqcup")]
           [(#\∨) (mathify "\\vee")]
           [(#\⋁) (unsafe-mathify "\\bigvee")]
           [(#\∧) (mathify "\\wedge")]
           [(#\⋀) (unsafe-mathify "\\bigwedge")]
           [(#\◃) (mathify "\\triangleleft")]
           [(#\▹) (mathify "\\triangleright")]
           [(#\⊙) (mathify "\\odot")]
           [(#\★) (mathify "\\star")]
           [(#\†) (mathify "\\dagger")]
           [(#\•) (mathify "\\bullet")]
           [(#\‡) (mathify "\\ddagger")]
           [(#\≀) (mathify "\\wr")]
           [(#\⨿) (mathify "\\amalg")]
           [(#\≤) (mathify "\\leq")]
           [(#\≥) (mathify "\\geq")]
           [(#\≡) (mathify "\\equiv")]
           [(#\≟) (mathify "\\deceq")]
           [(#\≢) (mathify "\\nequiv")]
           [(#\⊨) (mathify "\\models")]
           [(#\≺) (mathify "\\prec")]
           [(#\≻) (mathify "\\succ")]
           [(#\∼) (mathify "\\sim")]
           [(#\⊥) (mathify "\\perp")]
           [(#\≼) (mathify "\\preceq")]
           [(#\≽) (mathify "\\succeq")]
           [(#\≃) (mathify "\\simeq")]
           ;; like brackets
           [(#\≪) (mathify "\\ll")]
           [(#\≫) (mathify "\\gg")]
           [(#\〈) (mathify "\\langle")]
           [(#\〉) (mathify "\\rangle")]
           [(#\⟪) (mathify "\\llangle")]
           [(#\⟫) (mathify "\\rrangle")]
           [(#\⌊) (mathify "\\lfloor")]
           [(#\⌋) (mathify "\\rfloor")]
           [(#\⌈) (mathify "\\lceil")]
           [(#\⌉) (mathify "\\rceil")]
           ;; end like brackets
           [(#\≍) (mathify "\\asymp")]
           [(#\∥) (mathify "\\parallel")]
           [(#\⊂) (mathify "\\subset")]
           [(#\⊃) (mathify "\\supset")]
           [(#\≈) (mathify "\\approx")]
           [(#\⋈) (mathify "\\bowtie")]
           [(#\⊆) (mathify "\\subseteq")]
           [(#\⊈) (mathify "\\nsubseteq")]
           [(#\⊊) (mathify "\\subsetneq")]
           [(#\⊇) (mathify "\\supseteq")]
           [(#\⊉) (mathify "\\nsupseteq")]
           [(#\⊋) (mathify "\\supsetneq")]
           [(#\≅) (mathify "\\cong")]
           [(#\⊏) (mathify "\\sqsubset")]
           [(#\⊐) (mathify "\\sqsupset")]
           [(#\≠) (mathify "\\neq")]
           [(#\⌣) (mathify "\\smile")]
           [(#\⊑) (mathify "\\sqsubseteq")]
           [(#\⊒) (mathify "\\sqsupseteq")]
           [(#\≐) (mathify "\\doteq")]
           [(#\⌢) (mathify "\\frown")]
           [(#\∅) (mathify "\\varnothing")]
           [(#\∈) (mathify "\\in")]
           [(#\∉) (mathify "\\notin")]
           [(#\∋) (mathify "\\ni")]
           [(#\∝) (mathify "\\propto")]
           [(#\⊢) (mathify "\\vdash")]
           [(#\⊣) (mathify "\\dashv")]
           [(#\☠) (mathify "\\skull")]
           [(#\☺) (mathify "\\smiley")]
           [(#\☻) (mathify "\\blacksmiley")]
           [(#\☹) (mathify "\\frownie")]
           [(#\à) "\\`{a}"]
           [(#\À) "\\`{A}"]
           [(#\á) "\\'{a}"]
           [(#\â) "\\^{a}"]
           [(#\Â) "\\^{A}"]
           [(#\Á) "\\'{A}"]
           [(#\ç) "\\c{c}"]
           [(#\Ç) "\\c{C}"]
           [(#\è) "\\`{e}"]
           [(#\È) "\\`{E}"]
           [(#\é) "\\'{e}"]
           [(#\É) "\\'{E}"]
           [(#\ê) "\\^{e}"]
           [(#\Ê) "\\^{E}"]
           [(#\í) "\\'{i}"]
           [(#\Í) "\\'{I}"]
           [(#\ï) "\\\"{i}"]
           [(#\Ï) "\\\"{I}"]
           [(#\î) "\\^{i}"]
           [(#\Î) "\\^{I}"]
           [(#\ô) "\\^{o}"]
           [(#\Ô) "\\^{O}"]
           [(#\û) "\\^{u}"]
           [(#\Û) "\\^{U}"]
           [(#\ä) "\\\"a"]
           [(#\Ä) "\\\"A"]
           [(#\ü) "\\\"u"]
           [(#\Ü) "\\\"U"]
           [(#\ö) "\\\"o"]
           [(#\Ö) "\\\"O"]
           [(#\ø) "{\\o}"]
           [(#\Ø) "{\\O}"]
           [(#\ł) "{\\l}"]
           [(#\Ł) "{\\L}"]
           [(#\š) "{\\v s}"]
           [(#\uA7) "{\\S}"]
           [(#\…) "\\ldots"]
           [(#\𝔸) (mathify "{\\mathbb A}")]
           [(#\𝔹) (mathify "{\\mathbb B}")]
           [(#\𝔻) (mathify "{\\mathbb D}")]
           [(#\𝔼) (mathify "{\\mathbb E}")]
           [(#\𝔽) (mathify "{\\mathbb F}")]
           [(#\𝔾) (mathify "{\\mathbb G}")]
           [(#\ℍ) (mathify "{\\mathbb H}")]
           [(#\𝕀) (mathify "{\\mathbb I}")]
           [(#\𝕁) (mathify "{\\mathbb J}")]
           [(#\𝕂) (mathify "{\\mathbb K}")]
           [(#\𝕃) (mathify "{\\mathbb L}")]
           [(#\𝕄) (mathify "{\\mathbb M}")]
           [(#\ℕ) (mathify "{\\mathbb N}")]
           [(#\𝕆) (mathify "{\\mathbb O}")]
           [(#\ℙ) (mathify "{\\mathbb P}")]
           [(#\ℚ) (mathify "{\\mathbb Q}")]
           [(#\ℝ) (mathify "{\\mathbb R}")]
           [(#\𝕊) (mathify "{\\mathbb S}")]
           [(#\𝕋) (mathify "{\\mathbb T}")]
           [(#\𝕌) (mathify "{\\mathbb U}")]
           [(#\𝕍) (mathify "{\\mathbb V}")]
           [(#\𝕎) (mathify "{\\mathbb W}")]
           [(#\𝕏) (mathify "{\\mathbb X}")]
           [(#\𝕐) (mathify "{\\mathbb Y}")]
           [(#\ℤ) (mathify "{\\mathbb Z}")]
           [(#\𝒜) (mathify "{\\mathcal A}")]
           [(#\ℬ) (mathify "{\\mathcal B}")]
           [(#\𝒞) (mathify "{\\mathcal C}")]
           [(#\𝒟) (mathify "{\\mathcal D}")]
           [(#\ℰ) (mathify "{\\mathcal E}")]
           [(#\ℱ) (mathify "{\\mathcal F}")]
           [(#\𝒢) (mathify "{\\mathcal G}")]
           [(#\ℋ) (mathify "{\\mathcal H}")]
           [(#\ℐ) (mathify "{\\mathcal I}")]
           [(#\𝒥) (mathify "{\\mathcal J}")]
           [(#\𝒦) (mathify "{\\mathcal K}")]
           [(#\ℒ) (mathify "{\\mathcal L}")]
           [(#\ℳ) (mathify "{\\mathcal M}")]
           [(#\𝒩) (mathify "{\\mathcal N}")]
           [(#\𝒪) (mathify "{\\mathcal O}")]
           [(#\𝒫) (mathify "{\\mathcal P}")]
           [(#\𝒬) (mathify "{\\mathcal Q}")]
           [(#\ℛ) (mathify "{\\mathcal R}")]
           [(#\𝒮) (mathify "{\\mathcal S}")]
           [(#\𝒯) (mathify "{\\mathcal T}")]
           [(#\𝒰) (mathify "{\\mathcal U}")]
           [(#\𝒱) (mathify "{\\mathcal V}")]
           [(#\𝒲) (mathify "{\\mathcal W}")]
           [(#\𝒳) (mathify "{\\mathcal X}")]
           [(#\𝒴) (mathify "{\\mathcal Y}")]
           [(#\𝒵) (mathify "{\\mathcal Z}")]
           [(#\₀) (mathify "_0")]
           [(#\₁) (mathify "_1")]
           [(#\₂) (mathify "_2")]
           [(#\₃) (mathify "_3")]
           [(#\₄) (mathify "_4")]
           [(#\₅) (mathify "_5")]
           [(#\₆) (mathify "_6")]
           [(#\₇) (mathify "_7")]
           [(#\₈) (mathify "_8")]
           [(#\₉) (mathify "_9")]
           [(#\⁰) (mathify "^0")]
           [(#\¹) (mathify "^1")]
           [(#\²) (mathify "^2")]
           [(#\³) (mathify "^3")]
           [(#\⁴) (mathify "^4")]
           [(#\⁵) (mathify "^5")]
           [(#\⁶) (mathify "^6")]
           [(#\⁷) (mathify "^7")]
           [(#\⁸) (mathify "^8")]
           [(#\⁹) (mathify "^9")]
           [(#\〚) (mathify "[\\![")]
           [(#\〛) (mathify "]\\!]")]
           [(#\⊤) (mathify "\\top")]
           [(#\⊥) (mathify "\\bot")]
           [(#\ℓ) (mathify "\\ell")]
           [(#\¥) "{\\textyen}"]
           [(#\™) "{\\texttrademark}"]
           [else c])
         c)]))

#lang scribble/manual
@(require (for-label scribble/struct
                    "../utils.rkt"
                    "../unmap.rkt"
                    racket/base
                    racket/contract))

@title[#:tag "scributils"]{Scribble + LaTeX utils}

This library provides support for an assortment of LaTeX environments,
particularly for mathematics rendering. Most of these functions/macros render
LaTeX strings rather than use Scribble's elements to get the same effect. This
is primarily because brackets are not supported (for rendering tabular
environments, say). Because of this, scribble must be given an extra style tex
file that includes the following macro:

@tt{\newcommand{\identity}[1]{#1}}

This macro does lead to some nasty interop with environments that parse their
bodies, such as @tt{mathpar} (from @tt{mathpartir}) and @tt{lstlisting} (from
@tt{listings}).

@section{Unicode Translation}
@(declare-exporting "../unmap.rkt")

@defthing[default-ops operators/c]{

Default operations for Unicode composing characters. Specifically
@racket{#\u0302} for @tt{\hat}, @racket{#\u0303} for @tt{\tilde},
@racket{#\u0307} for @tt{\dot}, and @racket{#\u0338} for @tt{\centernot}
(requires @tt{centernot} package).}

@defthing[operator/c contract?]{

Contract for a composing character operation. The translator provides the
character before the composing character leading to this operator's use, the
entire string, and the current string position. If the composing character is
consumed by the operation, return the string position plus 1.

For example, the operator for @racket[#\u0302] (Unicode composing hat) is
@codeblock|{
(λ (c str i)
   (values (format "\\hat{~a}{}" (in-math (translate-char c)))
           (+ i 1)))}|}

@defthing[operators/c contract?]{

Contracts a hash of @racket[char?] to @racket[operator/c].}

@defproc[(content->latex-content [e content?]
                                 [#:operators ophash operators/c default-ops])
                                 content?]{

Maps over @racket[e] and translates strings to the proper LaTeX commands,
allowing for composing characters to be manipulated according to
@racket[ophash]. Only does post-composing operations, not pre-composing.}

@defproc[(translate-char [c char?]) string?]{

The Unicode character to LaTeX command procedure.}

@defproc[(overload-op* [key char?] [value operator/c] ... ...) operators/c]{

Extends or overrides @racket[default-ops].}

@defparam[math-mode in-math? boolean?]{

Parameter determines how math is rendered.}

@defform[(in-math items ...+)]{

Renders items in a context where @racket[math-mode] is @racket[#t]}

@defform[(unmath items ...+)]{

Renders items in a context where @racket[math-mode] is @racket[#f]}

@section{General utilities}
@(declare-exporting "../utils.rkt")

@defproc[(exact [#:operators operators operators/c default-ops]
                [item content?] ...) content?]{
Core form that renders all input to text and wraps it in @tt{\identity}. If the
content contains Unicode characters, then many of them will be rendered to the
right LaTeX command. Some composing Unicode characters are supported too. See
@racket[overload-op*].

This Unicode translation does not play nicely with @racket[delayed-element?]
or @racket[part-relative-element?] since I don't know how to correctly wrap
these to do the translation.}

@defform[(m item ...)]{
Renders items in math mode with the same guarantees as @racket[exact]. Uses of
@racket[m] nest, using an internal parameter @racket[in-math] that is
maintained by the different forms in utils. It does not use the
@tt{\ensuremath} command.}

@defform[(um item ...)]{
Renders items ensuring not in math mode (uses @tt{\mbox}).}

@defform[(mp item ...)]{
Like @racket[m], only uses @tt{\[...\]} instead of @tt{$...$}.}

@defproc[(renewcommand [left content?] [right content?]) content?]{
Wrapper for LaTeX's @tt{\renewcommand}.}

@deftogether[(
@defstruct[brackets ([element content?])]
@defstruct[curlies ([element content?])]
@defstruct[parens ([element content?])])]{
Structures that are intepreted by @racket[env] to wrap the given element in
brackets, braces or parens respectively.}

@defproc[(env [t content?]
              [#:opt optional (listof (or/c brackets? curlies? parens?)) '()]
              [items content?] ...) content?]{

Wraps items in the @racket[t] environment, and optionally renders an assortment
of wrapped content to allow for inputting optional arguments to the
environment. For example, @tt{\begin{array}{cc}l&r\end{array}} is @racket[(env
"array" (list (curlies "cc")) "l&r")].}

@defproc[(tenv [t content?]
               [title content?]
               [items content?] ...) content?]{

Uses @racket[env] in an idiomatic way, giving a ``title'' to an environment by
using @racket[(list (brackets title))] as the optional argument.}

@defform[(envalign* items ...)]{

Uses @racket[env] for the @tt{align*} environment. Sets math mode parameter to
work correctly with @racket[m]. Expects @racket[items] to be expressions that
evaluate to @racket[content?].}

@defform[(align* (args ...) ...)]{

Like @racket[envalign*], only inserts the @tt{&} and @tt{\\} between individual
args and list of args, respectively.}

@defproc[(array [style content?]
                [items content?] ...) content?]{

Renders items in an @tt{array} environment with the given style. Example above
in @racket[env] takes the form @racket[(array "cc" "l&r")].}

@defform[(style-matrix (s:style ...) (args ...) ...)]{

Uses same separation as @racket[align*], only creates an array with the given
style, which is a sequence of the identifiers @racket[l], @racket[r],
@racket[c] or @racket[bar].}

@defform[(matrix (args ...) ...)]{

Uses @racket[style-matrix] with all left-aligned colunms.}

@section{@tt{amsthm} utilities}
@(declare-exporting "../utils.rkt")

@defproc[(mdef [title content?]
               [#:tag tag (or/c content? #f)]
               [items content?] ...) content?]{

Uses @tt{definition} theorem environment (must be declared in style file:
@tt{\newtheorem{definition}{Definition}}) to give a definition. To use a LaTeX
label (not a scribble one!), provide an argument to @racket{#:tag}.}

@deftogether[(
@defproc[(mthm [title content?]
               [#:tag tag (or/c content? #f)]
               [items content?] ...) content?]
@defproc[(mlem [title content?]
               [#:tag tag (or/c content? #f)]
               [items content?] ...) content?]
@defproc[(mprop [title content?]
                [#:tag tag (or/c content? #f)]
                [items content?] ...) content?]
@defproc[(mcor [title content?]
               [#:tag tag (or/c content? #f)]
               [items content?] ...) content?]
@defproc[(mnotation [title content?]
                    [#:tag tag (or/c content? #f)]
                    [items content?] ...) content?]
@defproc[(unthm [title content?]
                [#:tag tag (or/c content? #f)]
                [items content?] ...) content?])]{

Like @racket[mdef], only use @tt{theorem}, @tt{lemma}, @tt{property},
@tt{corollary}, @tt{notation} and @tt{untheorem} (used for "Conjecture")
theorem environments respectively.}

@defproc[(tprf [title content?]
               [items content?] ...) content?]{

Renders items in the @tt{proof} environment with the given title. Proof does
not need declaration like theorem, etc.}

@deftogether[(
@defproc[(ntthm [items content?] ...) content?]
@defproc[(ntlem [items content?] ...) content?]
@defproc[(ntprf [items content?] ...) content?])]{
Like @racket[mthm], @racket[mlem] and @racket[tprf], only with no titles or
tags. @racket[ntprf] uses the @tt{proof} environment.}

@defproc[(parthm [title content?]
                 [#:tag tag (or/c content? #f)]
                 [items content?] ...) content?]{

Like @racket[mthm], only allows blocks (e.g. uses of @racket[itemlist]) in
@racket[items]. It wraps content in @racket[make-paragraph] and makes sure not
to add more paragraphs than it needs.}

@deftogether[(
@defproc[(parlem [title content?]
                 [#:tag tag (or/c content? #f)]
                 [items content?] ...) content?]
@defproc[(parprop [title content?]
                  [#:tag tag (or/c content? #f)]
                  [items content?] ...) content?]
@defproc[(parunthm [title content?]
                   [#:tag tag (or/c content? #f)]
                   [items content?] ...) content?]
@defproc[(parprf [#:tag tag (or/c content? #f)]
                 [items content?] ...) content?])]{

Like @racket[parthm], only for @racket[mlem], @racket[mprop], @racket[unthm]
and @racket[ntprf] respectively.}

@section{@tt{listings} utilities}
@(declare-exporting "../utils.rkt")

Requires including @tt{listings} package and putting @tt{\newbox\mylistings}
in additional style tex file.

@defproc[(lstlisting [#:math-escape? math-escape? #f]
                     [items content?] ...) content?]{

Renders items in @tt{lstlisting} environment. Uses extra trickery with boxes to
get it to work with scribble. Give @racket[#t] for @racket[#:math-escape?] to
get math rendering for @tt{$...$}.}

@defproc[(lstset [#:basicstyle basicstyle (or/c content? #f) #f]
                 [#:keywordstyle keywordstyle (or/c content? #f) #f]
                 [#:identifierstyle identifierstyle (or/c content? #f) #f]
                 [#:commentstyle commentstyle (or/c content? #f) #f]
                 [#:stringstyle stringstyle (or/c content? #f) #f]
                 [#:showstringspaces showstringspaces (or/c content? #f) #f]
                 [#:numbers numbers (or/c content? #f) #f]
                 [#:numberstyle numberstyle (or/c content? #f) #f]
                 [#:numberblanklines numberblanklines (or/c content? #f) #f]
                 [#:stepnumber stepnumber (or/c content? #f) #f]
                 [#:numbersep numbersep (or/c content? #f) #f]
                 [#:backgroundcolor backgroundcolor (or/c content? #f) #f]
                 [#:showspaces showspaces (or/c content? #f) #f]
                 [#:showtabs showtabs (or/c content? #f) #f]
                 [#:frame frame (or/c content? #f) #f]
                 [#:label label (or/c content? #f) #f]
                 [#:rulecolor rulecolor (or/c content? #f) #f]
                 [#:tabsize tabsize (or/c content? #f) #f]
                 [#:language language (or/c content? #f) #f]
                 [#:caption caption (or/c content? #f) #f]
                 [#:captionpos captionpos (or/c content? #f) #f]
                 [#:breaklines breaklines (or/c content? #f) #f]
                 [#:breakatwhitespace breakatwhitespace (or/c content? #f) #f]
                 [#:title title (or/c content? #f) #f]
                 [#:escapeinside escapeinside (or/c content? #f) #f]
                 [#:morekeywords morekeywords (or/c content? #f) #f]
                 [#:moredelim moredelim (or/c content? #f) #f]
                 [#:xleftmargin xleftmargin (or/c content? #f) #f]
                 [#:xrightmargin xrightmargin (or/c content? #f) #f])
                 content?]{

Provides a subset of possible inputs to the extensive @tt{\lstset} macro. See
@tt{listings} documentation for usage.}
 
@section{@tt{mathpartir} utilities}
@(declare-exporting "../utils.rkt")

Requires including @tt{mathpartir} package and putting @tt{\newbox\mymathpar}
in additional style tex file.

@defform[(mathpar items ...)]{

Renders items in @tt{mathpar} environment. Uses extra trickery with boxes to
get it to work with scribble.}

@section{@tt{pfsteps} utilities}
@(declare-exporting "../utils.rkt")

Jesse Tov has an excellent LaTeX package for
@hyperlink["http://www.eecs.harvard.edu/~tov/code/latex/"]{typesetting
rigorous proofs.}. Given his @tt{pfsteps} package is included in the extra
style latex file, you can use the following forms to interact with it.

@defproc[(byCases [items content?] ...) content?]{

Form for case analysis. To be used with @racket[bc-case] and @racket[bc-otherwise].}

@defform[(bc-case title items ...)]{
For use in @racket[byCases] items. Gives case a title (e.g. "Base case") and
the following proof.}

@defproc[(bc-otherwise [items content?] ...) content?]{
For use as the last of the @racket[byCases] items. Instead of saying "Case
[title]" it says "Otherwise". It does not have to be used.}

@section{Miscellaneous}
@(declare-exporting "../utils.rkt")

Random stuff I threw it for papers.

@defproc[(graybox [elm content?]) content?]{

Requires @tt{color} package and a file bgcolor.tex with content
@tt{\newcommand{\BgColor}[1]{\colorbox{lightgray}{#1}}}. Gives @racket[elm] a
light gray background color.}
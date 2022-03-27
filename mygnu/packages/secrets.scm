(define-module (mygnu packages secrets)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix licenses)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libffi) 
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages flex)
  #:use-module (mygnu packages ocaml-extunix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages video)
  #:use-module (guix utils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages vulkan)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (ice-9 match)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages gawk))

(define (janestreet-origin name version hash)
  (origin (method url-fetch)
          (uri (string-append "https://ocaml.janestreet.com/ocaml-core/v"
                              (version-major+minor version) "/files/"
                              name "-v" (version-major+minor+point version)
                              ".tar.gz"))
          (sha256 (base32 hash))))


(define-public dwl-custom
  (package
    (inherit dwl)
    (name "dwl-custom")
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure
         (add-after 'install 'add-desktop-file-for-session-managers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (with-output-to-file (string-append xsessions "/dwl.desktop")
                 (lambda ()
                   (display "[Desktop Entry]")
                   (newline)
                   (display "Name=dwl")
                   (newline)
                   (display "Comment=dwl window manager")
                   (newline)
                   (display (string-append "Exec=" out "/bin/dwl"))
                   (newline)
                   (display "Type=Application")
                   (newline)))))))))))

(define dune2.8.5-bootstrap
  (package
    (name "dune2.8.5")
    (version "2.8.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ocaml/dune")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a1jj6njzsfjgklsirs6a79079wg4jhy6n888vg3dgp44awwq5jn"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; require odoc
       #:make-flags (list "release"
                          (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "LIBDIR=" (assoc-ref %outputs "out")
                                         "/lib/ocaml/site-lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "src/dune")
             (invoke "./configure")
             #t)))))
    (home-page "https://github.com/ocaml/dune")
    (synopsis "OCaml build system")
    (description "Dune is a build system that was designed to simplify the
release of Jane Street packages.  It reads metadata from @file{dune} files
following a very simple s-expression syntax.")
    (license license:expat)))

(define-public ocaml-parsexp
  (package
    (name "ocaml-parsexp")
    (version "0.14.2")
    (home-page "https://github.com/janestreet/parsexp")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14yi0licf1cp6b7qny5pz6dmlalqdksx0m0kzcrwi6byjxwjkbi9"))))
    (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
    (inputs
     (list ocaml-sexplib0 ocaml-base))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-parsexp))))
    (synopsis "S-expression parsing library")
    (description
     "This library provides generic parsers for parsing S-expressions from
strings or other medium.

The library is focused on performances but still provide full generic
parsers that can be used with strings, bigstrings, lexing buffers,
character streams or any other sources effortlessly.

It provides three different class of parsers:
@itemize
@item
the normal parsers, producing [Sexp.t] or [Sexp.t list] values
@item
the parsers with positions, building compact position sequences so
that one can recover original positions in order to report properly
located errors at little cost
@item
the Concrete Syntax Tree parsers, produce values of type
@code{Parsexp.Cst.t} which record the concrete layout of the s-expression
syntax, including comments
@end itemize

This library is portable and doesn't provide IO functions.  To read
s-expressions from files or other external sources, you should use
parsexp_io.")
    (license license:expat)))

(define-public dune2.8.5-configurator
  (package
    (inherit dune2.8.5-bootstrap)
    (name "dune2.8.5-configurator")
    (build-system dune-build-system)
    (arguments
     `(#:package "dune-configurator"
       #:dune ,dune2.8.5-bootstrap
       ; require ppx_expect
       #:tests? #f))
    (propagated-inputs
     `(("ocaml-csexp" ,ocaml-csexp)))
    ;; (properties `((ocaml4.09-variant . ,(delay ocaml4.09-dune-configurator))))
    (synopsis "Dune helper library for gathering system configuration")
    (description "Dune-configurator is a small library that helps writing
OCaml scripts that test features available on the system, in order to generate
config.h files for instance.  Among other things, dune-configurator allows one to:

@itemize
@item test if a C program compiles
@item query pkg-config
@item import #define from OCaml header files
@item generate config.h file
@end itemize")))

(define-public dune2.8.5
  (package
    (inherit dune2.8.5-bootstrap)
    (propagated-inputs
     `(("dune-configurator" ,dune2.8.5-configurator)))
    ;; (properties `((ocaml4.07-variant . ,(delay ocaml4.07-dune))
    ;;               (ocaml4.09-variant . ,(delay ocaml4.09-dune))))
    ))


;; newer version of this needed by bin_prot or ppx_custom_printf
(define-public ocaml-migrate-parsetree
  (package
   (name "ocaml-migrate-parsetree")
   (version "2.3.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://github.com/ocaml-ppx/ocaml-migrate-parsetree/releases/download/v2.3.0/ocaml-migrate-parsetree-2.3.0.tbz")
     (sha256
      (base32 "02mzh1rcvc2xpq4iz01z7kvzsgxns3774ggxi96f147i8yr2d08h"))))
   (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
   (native-inputs (list ocaml-cinaps))
   (properties `((upstream-name . "ocaml-migrate-parsetree")))
   (home-page "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
   (synopsis "Convert OCaml parsetrees between different versions")
   (description
    "Convert OCaml parsetrees between different versions

This library converts parsetrees, outcometree and ast mappers between different
OCaml versions.  High-level functions help making PPX rewriters independent of a
compiler version.")
   (license #f)))

;; (define-public ocaml-ppxlib
;;   (package
;;    (name "ocaml-ppxlib")
;;    (version "0.26.0")
;;    (source
;;     (origin
;;      (method url-fetch)
;;      (uri "https://github.com/ocaml-ppx/ppxlib/releases/download/0.26.0/ppxlib-0.26.0.tbz")
;;      (sha256
;;       (base32 "1zbyh6pr6fih2c1p6gs8y0q0ag1kzs41z4pyama96qsqx9kpn4b3"))))
;;    (build-system dune-build-system)
;;    (arguments
;;     `(#:tests? #f))
;;    (propagated-inputs
;;     (list ocaml-compiler-libs
;;           ocaml-ppx-derivers
;;           ocaml-sexplib0
;;           ocaml-stdlib-shims
;;           ocaml-odoc))
;;    (native-inputs
;;     (list ocaml-sexplib0
;;           ocaml-findlib
;;           ocaml-re
;;           ocaml-cinaps
;;           ocaml-base
;;           ocaml-stdio))
;;    (home-page "https://github.com/ocaml-ppx/ppxlib")
;;    (synopsis "Standard library for ppx rewriters")
;;    (description
;;     "Ppxlib is the standard library for ppx rewriters and other programs that
;; manipulate the in-memory reprensation of OCaml programs, a.k.a the \"Parsetree\".

;; It also comes bundled with two ppx rewriters that are commonly used to write
;; tools that manipulate and/or generate Parsetree values; `ppxlib.metaquot` which
;; allows to construct Parsetree values using the OCaml syntax directly and
;; `ppxlib.traverse` which provides various ways of automatically traversing values
;; of a given type, in particular allowing to inject a complex structured value
;; into generated code.")
;;    (license license:expat)))

(define-public ocaml-ppxlib
  (package
   (name "ocaml-ppxlib")
   (version "0.24.0")
   (home-page "https://github.com/ocaml-ppx/ppxlib")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1nkkdvqifa36hxj6msd74ld4dfd749d6d9ygfj7zsm328rqvpqf2"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs
    (list ocaml-base
          ocaml-compiler-libs
          ocaml-migrate-parsetree
          ocaml-stdlib-shims
          ocaml-ppx-derivers
          ocaml-stdio
          ocaml-result
          ocaml-sexplib0))
   (properties `((ocaml4.07-variant . ,(delay ocaml4.07-ppxlib))))
   (synopsis
    "Base library and tools for ppx rewriters")
   (description
    "A comprehensive toolbox for ppx development.  It features:
 @itemize
 @item an OCaml AST / parser / pretty-printer snapshot, to create a full frontend
 independent of the version of OCaml;
 @item a library for library for ppx rewriters in general, and type-driven code
 generators in particular;
 @item
 a feature-full driver for OCaml AST transformers;
 @item a quotation mechanism allowing to write values representing the
 OCaml AST in the OCaml syntax;
 @item a generator of open recursion classes from type definitions.
 @end itemize")
   (license license:expat)))
 

(define-public ocaml-uutf
  (package
    (name "ocaml-uutf")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri "http://erratique.ch/software/uutf/releases/uutf-1.0.2.tbz")
       (sha256
	(base32
	 "1nx1rly3qj23jzn9yk3x6fwqimcxjd84kv5859vvhdg56psq26p6"))))
    (build-system ocaml-build-system)
    (arguments
     `(
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
	 (delete 'check)
	 (delete 'configure))))
    (inputs `(("opam" ,opam)))
    (propagated-inputs
     `(("ocaml-uchar" ,ocaml-uchar)
       ("ocaml-cmdliner" ,ocaml-cmdliner)))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)
       ("ocaml-topkg" ,ocaml-topkg)))
    (home-page "http://erratique.ch/software/uutf")
    (synopsis
     "Non-blocking streaming Unicode codec for OCaml")
    (description
     "
Uutf is a non-blocking streaming codec to decode and encode the UTF-8,
UTF-16, UTF-16LE and UTF-16BE encoding schemes. It can efficiently
work character by character without blocking on IO. Decoders perform
character position tracking and support newline normalization.

Functions are also provided to fold over the characters of UTF encoded
OCaml string values and to directly encode characters in OCaml
Buffer.t values.

Uutf has no dependency and is distributed under the ISC license.
")
    (license #f)))

(define-public ocaml-uunf
  (package
    (name "ocaml-uunf")
    (version "13.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://erratique.ch/software/uunf/releases/uunf-13.0.0.tbz")
        (sha256
          (base32
            "1qci04nkp24kdls1z4s8kz5dzgky4nwd5r8345nwdrgwmxhw7ksm"))))
    (build-system ocaml-build-system)
    (arguments
     `(
       #:tests? #f
       #:build-flags (list "build" "--with-uutf" "true")
       #:phases
       (modify-phases %standard-phases
	 (delete 'check)
	 (delete 'configure))
       ))
    (propagated-inputs `(("uutf" ,ocaml-uutf)))
    (inputs `(("opam" ,opam)
	      ("topkg" ,ocaml-topkg)))
    (native-inputs
      `(("ocaml-findlib" ,ocaml-findlib)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/uunf")
    (synopsis "Unicode text normalization for OCaml")
    (description
      "
Uunf is an OCaml library for normalizing Unicode text. It supports all
Unicode [normalization forms][nf]. The library is independent from any
IO mechanism or Unicode text data structure and it can process text
without a complete in-memory representation.

Uunf has no dependency. It may optionally depend on [Uutf][uutf] for
support on OCaml UTF-X encoded strings. It is distributed under the
ISC license.

[nf]: http://www.unicode.org/reports/tr15/
[uutf]: http://erratique.ch/software/uutf
")
    (license #f)))

(define-public ocaml-uucd
  (package
    (name "ocaml-uucd")
    (version "13.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://erratique.ch/software/uucd/releases/uucd-13.0.0.tbz")
        (sha256
          (base32
            "1fg77hg4ibidkv1x8hhzl8z3rzmyymn8m4i35jrdibb8adigi8v2"))))
    (build-system ocaml-build-system)
    (arguments
     `(
       #:build-flags (list "build")
       #:phases
       (modify-phases %standard-phases
	 (delete 'check)
	 (delete 'configure))))
    (propagated-inputs `(("ocaml-xmlm" ,ocaml-xmlm)))
    (inputs `(
	      ("opam" ,opam)
	      ("topkg" ,ocaml-topkg)))
    (native-inputs
      `(("ocaml-findlib" ,ocaml-findlib)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/uucd")
    (synopsis
      "Unicode character database decoder for OCaml")
    (description
      "
Uucd is an OCaml module to decode the data of the [Unicode character 
database][1] from its XML [representation][2]. It provides high-level 
(but not necessarily efficient) access to the data so that efficient 
representations can be extracted.

Uucd is made of a single module, depends on [Xmlm][xmlm] and is distributed
under the ISC license.

[1]: http://www.unicode.org/reports/tr44/
[2]: http://www.unicode.org/reports/tr42/
[xmlm]: http://erratique.ch/software/xmlm 
")
    (license #f)))

(define-public ocaml-uucp
  (package
    (name "ocaml-uucp")
    (version "13.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://erratique.ch/software/uucp/releases/uucp-13.0.0.tbz")
        (sha256
          (base32
            "19kf8ypxaakacgg1dwwfzkc2zicaj88cmw11fw2z7zl24dn4gyiq"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--with-uutf" "true" "--with-uunf" "true")
       #:phases
       (modify-phases %standard-phases
	 (delete 'check)
	 (delete 'configure))))
    (native-inputs
     `(
       ("opam" ,opam)
       ("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)
       ("ocaml-topkg" ,ocaml-topkg)
       ("ocaml-uucd" ,ocaml-uucd)
       ("ocaml-uunf" ,ocaml-uunf)
       ("ocaml-uutf" ,ocaml-uutf)))
    (home-page "https://erratique.ch/software/uucp")
    (synopsis
      "Unicode character properties for OCaml")
    (description
      "
Uucp is an OCaml library providing efficient access to a selection of
character properties of the [Unicode character database][1].

Uucp is independent from any Unicode text data structure and has no
dependencies. It is distributed under the ISC license.

[1]: http://www.unicode.org/reports/tr44/
")
    (license #f)))

(define-public ocaml-uuseg
  (package
    (name "ocaml-uuseg")
    (version "13.0.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://erratique.ch/software/uuseg/releases/uuseg-13.0.0.tbz")
        (sha256
          (base32
            "1a635j8ra6p27g1ivfln3387lhwqmf6vq4r6bn7b6n1qsqyi1rls"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:build-flags (list "build" "--with-uutf" "true")
       #:phases
       (modify-phases %standard-phases
	 (delete 'check)
	 (delete 'configure))))
    (inputs `(("opam" ,opam)))
    (propagated-inputs
     `(
       ("uucp" ,ocaml-uucp)
       ("uutf" ,ocaml-uutf)
       ))
    (native-inputs
      `(("ocaml-findlib" ,ocaml-findlib)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-topkg" ,ocaml-topkg)))
    (home-page "https://erratique.ch/software/uuseg")
    (synopsis "Unicode text segmentation for OCaml")
    (description
      "
Uuseg is an OCaml library for segmenting Unicode text. It implements
the locale independent [Unicode text segmentation algorithms][1] to
detect grapheme cluster, word and sentence boundaries and the
[Unicode line breaking algorithm][2] to detect line break
opportunities.

The library is independent from any IO mechanism or Unicode text data
structure and it can process text without a complete in-memory
representation.

Uuseg depends on [Uucp](http://erratique.ch/software/uucp) and
optionally on [Uutf](http://erratique.ch/software/uutf) for support on
OCaml UTF-X encoded strings. It is distributed under the ISC license.

[1]: http://www.unicode.org/reports/tr29/
[2]: http://www.unicode.org/reports/tr14/
")
    (license #f)))

(define-public ocaml-menhirSdk
  (package
    (name "ocaml-menhirSdk")
    (version "20210419")
    (source
      (origin
        (method url-fetch)
        (uri "https://gitlab.inria.fr/fpottier/menhir/repository/20210419/archive.tar.gz")
        (sha256
          (base32
            "1z471apfcfs9d1s85wg33z5prfnifzx07dprjxq4fgfpcbqpqh7q"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "http://gitlab.inria.fr/fpottier/menhir")
    (synopsis
      "Compile-time library for auxiliary tools related to Menhir")
    (description #f)
    (license #f)))

(define-public ocaml-menhirLib
  (package
    (name "ocaml-menhirLib")
    (version "20210419")
    (source
      (origin
        (method url-fetch)
        (uri "https://gitlab.inria.fr/fpottier/menhir/repository/20210419/archive.tar.gz")
        (sha256
          (base32
            "1z471apfcfs9d1s85wg33z5prfnifzx07dprjxq4fgfpcbqpqh7q"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "http://gitlab.inria.fr/fpottier/menhir")
    (synopsis
      "Runtime support library for parsers generated by Menhir")
    (description #f)
    (license #f)))

(define-public ocaml-fix
  (package
    (name "ocaml-fix")
    (version "20201120")
    (source
      (origin
        (method url-fetch)
        (uri "https://gitlab.inria.fr/fpottier/fix/repository/20201120/archive.tar.gz")
        (sha256
          (base32
            "02xyn3wfcmz8if72y5pscy2imsnxv6s0fb0fvigjjdnknnd32wk9"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "https://gitlab.inria.fr/fpottier/fix")
    (synopsis
      "Facilities for memoization and fixed points")
    (description #f)
    (license #f)))

(define-public ocaml-dune-build-info
  (package
    (name "ocaml-dune-build-info")
    (version "2.7.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ocaml/dune/releases/download/2.7.1/dune-2.7.1.tbz")
        (sha256
          (base32
            "0pcjf209gynjwipnpplaqyvyivnawqiwhvqnivhkybisicpqyln3"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-odoc" ,ocaml-odoc)))
    (home-page "https://github.com/ocaml/dune")
    (synopsis
      "Embed build informations inside executable")
    (description
      "The build-info library allows to access information about how the
executable was built, such as the version of the project at which it
was built or the list of statically linked libraries with their
versions.  It supports reporting the version from the version control
system during development to get an precise reference of when the
executable was built.
")
    (license #f)))

(define-public ocamlformat
  (package
    (name "ocamlformat")
    (version "0.17.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ocaml-ppx/ocamlformat/releases/download/0.17.0/ocamlformat-0.17.0.tbz")
        (sha256
          (base32
            "0f1lxp697yq61z8gqxjjaqd2ns8fd1vjfggn55x0gh9dx098p138"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-version" ,ocaml-version)
        ("ocaml-base" ,ocaml-base)
        ("ocaml-cmdliner" ,ocaml-cmdliner)
        ("ocaml-dune-build-info" ,ocaml-dune-build-info)
        ("ocaml-fix" ,ocaml-fix)
        ("ocaml-fpath" ,ocaml-fpath)
        ("ocaml-menhir" ,ocaml-menhir)
        ("ocaml-menhirLib" ,ocaml-menhirLib)
        ("ocaml-menhirSdk" ,ocaml-menhirSdk)
        ("ocaml-odoc" ,ocaml-odoc)
        ("ocaml-migrate-parsetree" ,ocaml-migrate-parsetree)
        ("ocaml-ppxlib" ,ocaml-ppxlib)
        ("ocaml-re" ,ocaml-re)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-uuseg" ,ocaml-uuseg)
        ("ocaml-uutf" ,ocaml-uutf)))
    (native-inputs
      `(("ocaml-alcotest" ,ocaml-alcotest)
        ("ocaml-ocp-indent" ,ocaml-ocp-indent)
        ("ocaml-bisect-ppx" ,ocaml-bisect-ppx)))
    (home-page
      "https://github.com/ocaml-ppx/ocamlformat")
    (synopsis "Auto-formatter for OCaml code")
    (description
      "OCamlFormat is a tool to automatically format OCaml code in a uniform style.")
    (license #f)))

(define-public ocaml-spawn
  (package
    (name "ocaml-spawn")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/spawn/releases/download/v0.13.0/spawn-v0.13.0.tbz")
        (sha256
          (base32
            "00kcdy6lrqllh7n00d7lkvjvqf4kj0y9a2hj2shp0bjqphnjd9nh"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (native-inputs
      `(("ocaml-ppx-expect" ,ocaml-ppx-expect)))
    (home-page "https://github.com/janestreet/spawn")
    (synopsis "Spawning sub-processes")
    (description
      "Spawn is a small library exposing only one functionality: spawning sub-process.

It has three main goals:

1. provide missing features of Unix.create_process such as providing a
working directory

2. provide better errors when a system call fails in the
sub-process. For instance if a command is not found, you get a proper
[Unix.Unix_error] exception

3. improve performances by using vfork when available. It is often
claimed that nowadays fork is as fast as vfork, however in practice
fork takes time proportional to the process memory while vfork is
constant time. In application using a lot of memory, vfork can be
thousands of times faster than fork.
")
    (license #f)))

(define-public ocaml-timezone
  (package
    (name "ocaml-timezone")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/timezone-v0.14.0.tar.gz")
        (sha256
          (base32
            "095xni0szjqqax2r9zh9820l72ixfga2pl0njnarp3795vkw0rdp"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-core-kernel" ,ocaml-core-kernel)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)))
    (home-page
      "https://github.com/janestreet/timezone")
    (synopsis "Time-zone handling")
    (description
      "
Timezone handles parsing timezone data and create [Timezone.t] that
can later be used to manipulate time in core_kernel or core.
")
    (license #f)))

(define-public ocaml-typerep
  (package
    (name "ocaml-typerep")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/typerep-v0.15.0.tar.gz")
        (sha256
          (base32
            "0ghqlfkpb20l3wp7dgb27viz5zdsmsd3ijrzb4bs8h0lniyx7j16"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-base" ,ocaml-base)))
    (home-page
      "https://github.com/janestreet/typerep")
    (synopsis
      "Typerep is a library for runtime types")
    (description "
")
    (license #f)))

(define-public ocaml-ppx-typerep-conv
  (package
    (name "ocaml-ppx-typerep-conv")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_typerep_conv-v0.15.0.tar.gz")
        (sha256
          (base32
            "17sy7fpn3v82aj1ga3q9yc8bbs37y45k8mhhaj7zvjm32gnjj5z7"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-typerep" ,ocaml-typerep)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_typerep_conv")))
    (home-page
      "https://github.com/janestreet/ppx_typerep_conv")
    (synopsis
      "Generation of runtime types from type declarations")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-string
  (package
    (name "ocaml-ppx-string")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_string-v0.15.0.tar.gz")
        (sha256
          (base32
            "14qkny138jfbh3gifyrd803alxxzzill5vwf7z325l1gg5b4jggk"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_string")))
    (home-page
      "https://github.com/janestreet/ppx_string")
    (synopsis
      "Ppx extension for string interpolation")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-stable
  (package
    (name "ocaml-ppx-stable")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_stable-v0.15.0.tar.gz")
        (sha256
          (base32
            "1crb0464fd10h4r690pf4ljx6hv59bmql51bvcw5js8cb5xfid4h"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_stable")))
    (home-page
      "https://github.com/janestreet/ppx_stable")
    (synopsis "Stable types conversions generator")
    (description
      "
A ppx extension for easier implementation of conversion functions between almost
identical types.
")
    (license #f)))

(define-public ocaml-ppx-pipebang
  (package
    (name "ocaml-ppx-pipebang")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_pipebang-v0.15.0.tar.gz")
        (sha256
          (base32
            "1a10aqrwcbc3ykz0zsc4fpfhq4dhv812p4ncj8286qdbhr6mamhy"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_pipebang")))
    (home-page
      "https://github.com/janestreet/ppx_pipebang")
    (synopsis
      "A ppx rewriter that inlines reverse application operators `|>` and `|!`")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-optional
  (package
    (name "ocaml-ppx-optional")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_optional-v0.15.0.tar.gz")
        (sha256
          (base32
            "0af7ayhfc1jc01mxs4k253gq49yss2ymkmjsy6fpcz39zhci7fvj"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_optional")))
    (home-page
      "https://github.com/janestreet/ppx_optional")
    (synopsis "Pattern matching on flat options")
    (description
      "
A ppx rewriter that rewrites simple match statements with an if then
else expression.
")
    (license #f)))

(define-public ocaml-ppx-module-timer
  (package
    (name "ocaml-ppx-module-timer")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_module_timer-v0.15.0.tar.gz")
        (sha256
          (base32
            "0qkwjq5m017g10zkf50gsaphvfcs0l7jbzqnld4s7ixhghsd1a12"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-time-now" ,ocaml-time-now)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_module_timer")))
    (home-page
      "https://github.com/janestreet/ppx_module_timer")
    (synopsis
      "Ppx rewriter that records top-level module startup times")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-fixed-literal
  (package
    (name "ocaml-ppx-fixed-literal")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_fixed_literal-v0.15.0.tar.gz")
        (sha256
          (base32
            "1z59hpljqsy8qbjkj05fly3b1p5lf7rifamrmn6c16gr6g73chgh"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_fixed_literal")))
    (home-page
      "https://github.com/janestreet/ppx_fixed_literal")
    (synopsis
      "Simpler notation for fixed point literals")
    (description
      "
A ppx rewriter that rewrites fixed point literal of the 
form 1.0v to conversion functions currently in scope.
")
    (license #f)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_expect-v0.15.0.tar.gz")
        (sha256
          (base32
            "0yyaz4r0l63y6p7dvhz8xa5ikdbp04y1ijwvf6b2z9rxni2mlc86"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-ppxlib" ,ocaml-ppxlib)
        ("ocaml-re" ,ocaml-re)))
    (properties `((upstream-name . "ppx_expect")))
    (home-page
      "https://github.com/janestreet/ppx_expect")
    (synopsis "Cram like framework for OCaml")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))








(define-public ocaml-ppx-bin-prot
  (package
    (name "ocaml-ppx-bin-prot")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_bin_prot-v0.15.0.tar.gz")
        (sha256
          (base32
            "04s02l8124qdcyw1haha29rnqhg02aw16ilqj5gsx8yr24ifdj5j"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-bin-prot" ,ocaml-bin-prot)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_bin_prot")))
    (home-page
      "https://github.com/janestreet/ppx_bin_prot")
    (synopsis
      "Generation of bin_prot readers and writers from types")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

;; (define-public dune-configurator
;;   (package
;;     (inherit dune-bootstrap)
;;     (name "dune-configurator")
;;     (build-system dune-build-system)
;;     (arguments
;;      `(#:package "dune-configurator"
;;        #:dune ,dune-bootstrap
;;        ; require ppx_expect
;;        #:tests? #f))
;;     (propagated-inputs
;;      `(("ocaml-csexp" ,ocaml-csexp)))
;;     (properties `((ocaml4.09-variant . ,(delay ocaml4.09-dune-configurator))))
;;     (synopsis "Dune helper library for gathering system configuration")
;;     (description "Dune-configurator is a small library that helps writing
;; OCaml scripts that test features available on the system, in order to generate
;; config.h files for instance.  Among other things, dune-configurator allows one to:

;; @itemize
;; @item test if a C program compiles
;; @item query pkg-config
;; @item import #define from OCaml header files
;; @item generate config.h file
;; @end itemize")))




;; (define-public ocaml-dune-configurator
;;   (package
;;     (name "ocaml-dune-configurator")
;;     (version "2.8.5")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/ocaml/dune/releases/download/2.8.5/dune-2.8.5.tbz")
;;         (sha256
;;           (base32
;;             "0a9n8ilsi3kyx5xqvk5s7iikk6y3pkpm5mvsn5za5ivlzf1i40br"))))
;;     (build-system dune-build-system)
;;     (arguments `(#:tests? #f))
;;     (propagated-inputs
;;       `(("ocaml-result" ,ocaml-result)
;;         ("ocaml-csexp" ,ocaml-csexp)
;;         ("ocaml-odoc" ,ocaml-odoc)))
;;     (home-page "https://github.com/ocaml/dune")
;;     (synopsis
;;       "Helper library for gathering system configuration")
;;     (description
;;       "dune-configurator is a small library that helps writing OCaml scripts that
;; test features available on the system, in order to generate config.h
;; files for instance.
;; Among other things, dune-configurator allows one to:
;; - test if a C program compiles
;; - query pkg-config
;; - import #define from OCaml header files
;; - generate config.h file
;; ")
;;     (license #f)))

(define-public ocaml-jst-config
  (package
    (name "ocaml-jst-config")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/jst-config-v0.15.0.tar.gz")
        (sha256
          (base32
            "066wg5p2xcs4y7gp7ak113x5crqn6f6cjnzgfmxzzixki5ml24s9"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-stdio" ,ocaml-stdio)
        ;; ("ocaml-dune-configurator"
        ;;  ,ocaml-dune-configurator)
	))
    (home-page
      "https://github.com/janestreet/jst-config")
    (synopsis
      "Compile-time configuration for Jane Street libraries")
    (description
      "
Defines compile-time constants used in Jane Street libraries such as Base, Core, and
Async.

This package has an unstable interface; it is intended only to share configuration between
different packages from Jane Street. Future updates may not be backward-compatible, and we
do not recommend using this package directly.
")
    (license #f)))

(define-public ocaml-jane-street-headers
  (package
    (name "ocaml-jane-street-headers")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/jane-street-headers-v0.15.0.tar.gz")
        (sha256
          (base32
            "1r27r0bxxa0iaah5rm84lwhrmh784vfpmb6056hpv0p34rxs7r1l"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/janestreet/jane-street-headers")
    (synopsis "Jane Street C header files")
    (description
      "
C header files shared between the various Jane Street packages
")
    (license #f)))

(define-public ocaml-time-now
  (package
    (name "ocaml-time-now")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/time_now-v0.15.0.tar.gz")
        (sha256
          (base32
            "1a6b1f55mwci1bd8w8vji0qn6wbs60jbwixvwgy4klx2blq57cqk"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-jane-street-headers"
         ,ocaml-jane-street-headers)
        ("ocaml-jst-config" ,ocaml-jst-config)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-optcomp" ,ocaml-ppx-optcomp)))
    (properties `((upstream-name . "time_now")))
    (home-page
      "https://github.com/janestreet/time_now")
    (synopsis "Reports the current time")
    (description
      "
Provides a single function to report the current time in nanoseconds
since the start of the Unix epoch.
")
    (license #f)))

(define-public ocaml-ppx-inline-test
  (package
    (name "ocaml-ppx-inline-test")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_inline_test-v0.15.0.tar.gz")
        (sha256
          (base32
            "06w7l575wqw2zgvsfdlslwxd6ni1nbw9jhimd7zy5a64md0x2y1z"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-time-now" ,ocaml-time-now)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_inline_test")))
    (home-page
      "https://github.com/janestreet/ppx_inline_test")
    (synopsis
      "Syntax extension for writing in-line tests in ocaml code")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-bench
  (package
    (name "ocaml-ppx-bench")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_bench-v0.15.0.tar.gz")
        (sha256
          (base32
            "0zp1qal278g9ccqhvgym7qk3gybd43d55qgcxkxp6wnd7qncg25i"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_bench")))
    (home-page
      "https://github.com/janestreet/ppx_bench")
    (synopsis
      "Syntax extension for writing in-line benchmarks in ocaml code")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-assert
  (package
    (name "ocaml-ppx-assert")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_assert-v0.15.0.tar.gz")
        (sha256
          (base32
            "0rsr1yz2rs12w6qw0dz09dg3k2x2pfgd014fgp6nj993hhznapsf"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-cold" ,ocaml-ppx-cold)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_assert")))
    (home-page
      "https://github.com/janestreet/ppx_assert")
    (synopsis
      "Assert-like extension nodes that raise useful errors on failure")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-splittable-random
  (package
    (name "ocaml-splittable-random")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/splittable_random-v0.15.0.tar.gz")
        (sha256
          (base32
            "006vxsmjlayqm39r343mishikx63ifd4kkwbr92rzhxmbz4fdy4z"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-bench" ,ocaml-ppx-bench)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-sexp-message"
         ,ocaml-ppx-sexp-message)))
    (properties
      `((upstream-name . "splittable_random")))
    (home-page
      "https://github.com/janestreet/splittable_random")
    (synopsis
      "PRNG that can be split into independent streams")
    (description
      "
PRNG that can be split into independent streams

A splittable pseudo-random number generator (SPRNG) functions like a PRNG in that it can
be used as a stream of random values; it can also be \"split\" to produce a second,
independent stream of random values.

This library implements a splittable pseudo-random number generator that sacrifices
cryptographic-quality randomness in favor of performance.
")
    (license #f)))

(define-public ocaml-ppx-sexp-value
  (package
    (name "ocaml-ppx-sexp-value")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_sexp_value-v0.15.0.tar.gz")
        (sha256
          (base32
            "13ambcwm8qd4nfl752rvr7ksqm9jxjns4h9s1hnv561gzgfd6vip"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_sexp_value")))
    (home-page
      "https://github.com/janestreet/ppx_sexp_value")
    (synopsis
      "A ppx rewriter that simplifies building s-expressions from ocaml values")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-here
  (package
    (name "ocaml-ppx-here")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_here-v0.15.0.tar.gz")
        (sha256
          (base32
            "1pyaw31j9n6r98ar947n3j2qj6rrszbdxr8jghk96j4ajdy05g65"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_here")))
    (home-page
      "https://github.com/janestreet/ppx_here")
    (synopsis "Expands [%here] into its location")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-sexp-message
  (package
    (name "ocaml-ppx-sexp-message")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_sexp_message-v0.15.0.tar.gz")
        (sha256
          (base32
            "1fnszzzvigf1z0y3hp758rmbm7lhl1ibfd1fkqk9fnz4h1gpi50d"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_sexp_message")))
    (home-page
      "https://github.com/janestreet/ppx_sexp_message")
    (synopsis
      "A ppx rewriter for easy construction of s-expressions")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-here
  (package
   (name "ocaml-ppx-here")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_here-v0.15.0.tar.gz")
     (sha256
      (base32 "1pyaw31j9n6r98ar947n3j2qj6rrszbdxr8jghk96j4ajdy05g65"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs (list ocaml-base ocaml-ppxlib))
   (properties `((upstream-name . "ppx_here")))
   (home-page "https://github.com/janestreet/ppx_here")
   (synopsis "Expands [%here] into its location")
   (description " Part of the Jane Street's PPX rewriters collection.")
   (license license:expat)))

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_let-v0.15.0.tar.gz")
        (sha256
          (base32
            "0m9niyiiv3qzv5x8hw0ifxjjzshnmx40dchka9d93mmnx88jqx34"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_let")))
    (home-page
      "https://github.com/janestreet/ppx_let")
    (synopsis "Monadic let-bindings")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))


(define-public ocaml-ppx-js-style
  (package
   (name "ocaml-ppx-js-style")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_js_style-v0.15.0.tar.gz")
     (sha256
      (base32 "1szbizivg20qsvg9xclvccw3g1vpvjfgjfssx5312dgrggwy61cx"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-base ocaml-octavius ocaml-ppxlib))
   (properties `((upstream-name . "ppx_js_style")))
   (home-page "https://github.com/janestreet/ppx_js_style")
   (synopsis "Code style checker for Jane Street Packages")
   (description
    " Part of the Jane Street's PPX rewriters collection.

This packages is a no-op ppx rewriter.  It is used as a 'lint' tool to enforce
some coding conventions across all Jane Street packages.")
   (license license:expat)))



(define-public ocaml-ppx-hash
  (package
    (name "ocaml-ppx-hash")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_hash-v0.15.0.tar.gz")
        (sha256
          (base32
            "048pim0xicj8j9whd5lnchf62788sk3w89h12aybbalk1xm6dfs5"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_hash")))
    (home-page
      "https://github.com/janestreet/ppx_hash")
    (synopsis
      "A ppx rewriter that generates hash functions from type expressions and definitions")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-enumerate
  (package
    (name "ocaml-ppx-enumerate")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_enumerate-v0.15.0.tar.gz")
        (sha256
          (base32
            "16yhk3xk2hskmlspb6mikmdp60qaypyiqgq9p17kxpial6fgpdfy"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_enumerate")))
    (home-page
      "https://github.com/janestreet/ppx_enumerate")
    (synopsis
      "Generate a list containing all values of a finite type")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))











(define-public ocaml-ppx-cold
  (package
    (name "ocaml-ppx-cold")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_cold-v0.15.0.tar.gz")
        (sha256
          (base32
            "13gqmfw2sq80anag9bwpm35600l1fnfn7mh9cbj1291k84rsx7wb"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_cold")))
    (home-page
      "https://github.com/janestreet/ppx_cold")
    (synopsis
      "Expands [@cold] into [@inline never][@specialise never][@local never]")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-base
  (package
    (name "ocaml-ppx-base")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_base-v0.15.0.tar.gz")
        (sha256
          (base32
            "181w7y2has8jsrqdsvd08q5nhnkx523vwsk3lg0cjix55qssvfyn"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-ppx-cold" ,ocaml-ppx-cold)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-enumerate" ,ocaml-ppx-enumerate)
        ("ocaml-ppx-hash" ,ocaml-ppx-hash)
        ("ocaml-ppx-js-style" ,ocaml-ppx-js-style)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_base")))
    (home-page
      "https://github.com/janestreet/ppx_base")
    (synopsis "Base set of ppx rewriters")
    (description
      "
ppx_base is the set of ppx rewriters used for Base.

Note that Base doesn't need ppx to build, it is only used as a
verification tool.
")
    (license #f)))

(define-public ocaml-base-quickcheck
  (package
    (name "ocaml-base-quickcheck")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/base_quickcheck-v0.15.0.tar.gz")
        (sha256
          (base32
            "1wl107032kcd439p50yg7bk0wdv1fygcpzl8xhv8jfb06s7nn3in"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-let" ,ocaml-ppx-let)
        ("ocaml-ppx-sexp-message"
         ,ocaml-ppx-sexp-message)
        ("ocaml-ppx-sexp-value" ,ocaml-ppx-sexp-value)
        ("ocaml-splittable-random"
         ,ocaml-splittable-random)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "base_quickcheck")))
    (home-page
      "https://github.com/janestreet/base_quickcheck")
    (synopsis
      "Randomized testing framework, designed for compatibility with Base")
    (description
      "
Base_quickcheck provides randomized testing in the style of Haskell's Quickcheck library,
with support for built-in types as well as types provided by Base.
")
    (license #f)))

(define-public ocaml-ppx-log
  (package
  (name "ocaml-ppx-log")
  (version "0.15.0")
  (source
    (origin
      (method url-fetch)
      (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_log-v0.15.0.tar.gz")
      (sha256
        (base32 "0gknk2b4yss9r7rir4snqkmawby98v0q3salj77cg9fwqk4a6yh3"))))
  (build-system dune-build-system)
  (propagated-inputs
    (list ocaml-base
          ocaml-ppx-here
          ocaml-ppx-sexp-conv
          ocaml-ppx-sexp-message
          ocaml-sexplib
          ocaml-ppxlib))
  (properties `((upstream-name . "ppx_log")))
  (home-page "https://github.com/janestreet/ppx_log")
  (synopsis
    "Ppx_sexp_message-like extension nodes for lazily rendering log messages")
  (description " Part of the Jane Street's PPX rewriters collection. ")
  (license license:expat)))

(define-public ocaml-ppx-ignore-instrumentation
  (package
   (name "ocaml-ppx-ignore-instrumentation")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_ignore_instrumentation-v0.15.0.tar.gz")
     (sha256
      (base32 "00lzm0agvh4vn5rwxb2byzixgmxjziimik99ncng6drrw0disi12"))))
   (build-system dune-build-system)
    (arguments `(#:tests? #f))
   (propagated-inputs (list ocaml-ppxlib))
   (properties `((upstream-name . "ppx_ignore_instrumentation")))
   (home-page "https://github.com/janestreet/ppx_ignore_instrumentation")
   (synopsis "Ignore Jane Street specific instrumentation extensions")
   (description
    " Ignore Jane Street specific instrumentation extensions from internal PPXs or
compiler     features not yet upstreamed.")
   (license license:expat)))

(define-public ocaml-ppx-disable-unused-warnings
  (package
   (name "ocaml-ppx-disable-unused-warnings")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_disable_unused_warnings-v0.15.0.tar.gz")
     (sha256
      (base32 "1gpfyr3a6zcifwcmpv83j371w5lxzy9889jhv8i9n7slny633axf"))))
   (build-system dune-build-system)
    (arguments `(#:tests? #f))
   (propagated-inputs (list ocaml-base ocaml-ppxlib))
   (properties `((upstream-name . "ppx_disable_unused_warnings")))
   (home-page "https://github.com/janestreet/ppx_disable_unused_warnings")
   (synopsis
    "Expands [@disable_unused_warnings] into [@warning \"-20-26-32-33-34-35-36-37-38-39-60-66-67\"]")
   (description " Part of the Jane Street's PPX rewriters collection.")
   (license license:expat)))


(define-public ocaml-ppx-jane
  (package
   (name "ocaml-ppx-jane")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_jane-v0.15.0.tar.gz")
     (sha256
      (base32 "0il1is2q2h91fqwga2w16z3wr7xym79adry74d1d5pws4z949ynh"))))
   (build-system dune-build-system)
   (propagated-inputs
    (list ocaml-base-quickcheck
          ocaml-ppx-assert
          ocaml-ppx-base
          ocaml-ppx-bench
          ocaml-ppx-bin-prot
          ocaml-ppx-custom-printf
          ocaml-ppx-disable-unused-warnings
          ocaml-ppx-expect
          ocaml-ppx-fields-conv
          ocaml-ppx-fixed-literal
          ocaml-ppx-here
          ocaml-ppx-ignore-instrumentation
          ocaml-ppx-inline-test
          ocaml-ppx-let
          ocaml-ppx-log
          ocaml-ppx-module-timer
          ocaml-ppx-optcomp
          ocaml-ppx-optional
          ocaml-ppx-pipebang
          ocaml-ppx-sexp-message
          ocaml-ppx-sexp-value
          ocaml-ppx-stable
          ocaml-ppx-string
          ocaml-ppx-typerep-conv
          ocaml-ppx-variants-conv
          ocaml-ppxlib))
   (properties `((upstream-name . "ppx_jane")))
   (home-page "https://github.com/janestreet/ppx_jane")
   (synopsis "Standard Jane Street ppx rewriters")
   (description
    " This package installs a ppx-jane executable, which is a ppx driver including
all standard Jane Street ppx rewriters.")
   (license license:expat)))

(define-public ocaml-ppx-module-timer
  (package
    (name "ocaml-ppx-module-timer")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_module_timer-v0.15.0.tar.gz")
        (sha256
          (base32
            "0qkwjq5m017g10zkf50gsaphvfcs0l7jbzqnld4s7ixhghsd1a12"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-time-now" ,ocaml-time-now)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_module_timer")))
    (home-page
      "https://github.com/janestreet/ppx_module_timer")
    (synopsis
      "Ppx rewriter that records top-level module startup times")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-base-bigstring
  (package
    (name "ocaml-base-bigstring")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/base_bigstring-v0.14.0.tar.gz")
        (sha256
          (base32
            "1fhldk58w56ixkin763kpic512xvkkf9b4mrnjfsbm8in75kzndq"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)))
    (properties
      `((upstream-name . "base_bigstring")))
    (home-page
      "https://github.com/janestreet/base_bigstring")
    (synopsis
      "String type based on [Bigarray], for use in I/O and C-bindings")
    (description
      "
String type based on [Bigarray], for use in I/O and C-bindings.
")
    (license #f)))

(define-public ocaml-int-repr
  (package
   (name "ocaml-int-repr")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/int_repr-v0.15.0.tar.gz")
     (sha256
      (base32 "06b7xp0v3s1jmf1bkjj2dyiwmk3ffbyglb1g8f1s3jjbj5c7ndax"))))
   (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs (list ocaml-base ocaml-ppx-jane))
   (properties `((upstream-name . "int_repr")))
   (home-page "https://github.com/janestreet/int_repr")
   (synopsis "Integers of various widths")
   (description " Integers of various widths.")
   (license license:expat)))

(define-public ocaml-core-kernel
  (package
   (name "ocaml-core-kernel")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/core_kernel-v0.15.0.tar.gz")
     (sha256
      (base32 "1h0na5sg2w6f923r3bqg64yhdmvpqrfcn5mdwj86nz022s7ji81l"))))
   (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
    (list ocaml-base ocaml-core ocaml-int-repr ocaml-ppx-jane))
   (properties `((upstream-name . "core_kernel")))
   (home-page "https://github.com/janestreet/core_kernel")
   (synopsis "Industrial strength alternative to OCaml's standard library")
   (description
    " The Core suite of libraries is an industrial strength alternative to OCaml's
standard library that was developed by Jane Street, the largest industrial user
of OCaml.

Core_kernel is the system-independent part of Core.")
   (license license:expat)))

(define-public ocaml-core
  (package
   (name "ocaml-core")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/core-v0.15.0.tar.gz")
     (sha256
      (base32 "07syx268zakascxjpvqmzqgd4qrjrvddi6j5lqnhbp85hqmnyr9s"))))
   (build-system dune-build-system)
   (arguments
    `(#:tests? #f))
   (propagated-inputs
    (list ocaml-base
          ocaml-base-bigstring
          ocaml-base-quickcheck
          ocaml-bin-prot
          ocaml-fieldslib
          ocaml-jane-street-headers
          ocaml-jst-config
          ocaml-ppx-assert
          ocaml-ppx-base
          ocaml-ppx-hash
          ocaml-ppx-inline-test
          ocaml-ppx-jane
          ocaml-ppx-sexp-conv
          ocaml-ppx-sexp-message
          ocaml-sexplib
          ocaml-splittable-random
          ocaml-stdio
          ocaml-time-now
          ocaml-typerep
          ocaml-variantslib))
   (home-page "https://github.com/janestreet/core")
   (synopsis "Industrial strength alternative to OCaml's standard library")
   (description
    " The Core suite of libraries is an industrial strength alternative to OCaml's
standard library that was developed by Jane Street, the largest industrial user
of OCaml.

This is the system-independent part of Core.  Unix-specific parts were moved to
[core-unix].")
   (license license:expat)))


(define-public secrets
  (package
    (name "secrets")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/constfun/secrets/archive/a06f749f733516bf166b96ab4103a26fb0e5048f.tar.gz")
       (sha256
	(base32
         "1bk7c6m9iqb860c8kzfv489q5gmgjwxfz8kqljby8k1i1gbrgdbx"))))
    ;; (outputs '("out" "lib" "bin"))
    (build-system dune-build-system)
    (arguments
     `(
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune
       #:tests? #f
       #:build-flags `("--verbose")
       ))
    ;; (inputs
    ;;  `(("xclip" ,xclip)))
    (native-inputs
     `(
       ;; ("linux-libre-headers"  ,linux-libre-headers)
       ("python"  ,python-2)
       ("ocaml-dune"  ,ocaml4.07-dune)
       ("ocaml-menhir"  ,ocaml4.07-menhir)
       ("ocaml-core"  ,ocaml4.07-core)
       
       ("ocaml-core-kernel"  ,ocaml4.07-core-kernel)
       ("ocaml-ppxlib"  ,ocaml4.07-ppxlib)
       ("ocaml-extunix"  ,ocaml4.07-extunix)
       ("ocaml-re2"  ,ocaml4.07-re2)
       ("ocaml-qrc"  ,ocaml4.07-qrc)
       ("libsodium"  ,libsodium)
       ;("coreutils"  ,coreutils)
       ;; ("git" ,git)
       ))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public ocaml-ANSITerminal
  (package
    (name "ocaml-ANSITerminal")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/Chris00/ANSITerminal/releases/download/0.8.2/ANSITerminal-0.8.2.tbz")
        (sha256
          (base32
            "04n15ki9h1qawlhkxbglzfbx0frm593nx2cahyh8riwc2g46q148"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (home-page
      "https://github.com/Chris00/ANSITerminal")
    (synopsis
      "Basic control of ANSI compliant terminals and the windows shell")
    (description
      "ANSITerminal is a module allowing to use the colors and cursor
movements on ANSI terminals. It also works on the windows shell (but
this part is currently work in progress).")
    (license #f)))

;; (define-public ocaml-integers
;;   (package
;;     (name "ocaml-integers")
;;     (version "0.3.0")
;;     (home-page "https://github.com/ocamllabs/ocaml-integers")
;;    (source (origin
;;              (method git-fetch)
;;              (uri (git-reference
;;                     (url home-page)
;;                     (commit version)))
;;              (file-name (git-file-name name version))
;;              (sha256
;;               (base32
;;                "1yhif5zh4srh63mhimfx3p5ljpb3lixjdd3i9pjnbj2qgpzlqj8p"))))
;;     (build-system dune-build-system)
;;     (arguments
;;      `(#:tests? #f; no tests
;;        #:phases
;;        (modify-phases %standard-phases
;;          (replace 'build
;;            ;; make warnings non fatal (jbuilder behaviour)
;;            (lambda _
;;              (invoke "dune" "build" "@install" "--profile=release"))))
;;        ))
;;     (synopsis "Various signed and unsigned integer types for OCaml")
;;     (description "The ocaml-integers library provides a number of 8-, 16-, 32-
;; and 64-bit signed and unsigned integer types, together with aliases such as
;; long and size_t whose sizes depend on the host platform.")
;;     (license license:expat)))

;; (define-public ocaml-ctypes
;;   (package
;;    (name "ocaml-ctypes")
;;    (version "0.18.0")
;;    (home-page "https://github.com/ocamllabs/ocaml-ctypes")
;;    (source (origin
;;              (method git-fetch)
;;              (uri (git-reference
;;                     (url home-page)
;;                     (commit version)))
;;              (file-name (git-file-name name version))
;;              (sha256
;;               (base32
;;                "03zrbnl16m67ls0yfhq7a4k4238x6x6b3m456g4dw2yqwc153vks"))))
;;    (build-system ocaml-build-system)
;;    (arguments
;;     `(#:tests? #f; require an old lwt
;;       #:make-flags
;;       (list (string-append "INSTALL_HEADERS = $(wildcard $($(PROJECT).dir)/*.h)"))
;;       #:phases
;;       (modify-phases %standard-phases
;;         (add-after 'unpack 'make-writable
;;           (lambda _
;;             (for-each
;;               (lambda (file)
;;                 (let ((stat (stat file)))
;;                   (chmod file (+ #o200 (stat:mode stat)))))
;;               (find-files "." "."))
;;             #t))
;; 	(add-before 'build 'set-patch-makefile
;; 		    (lambda* (#:key outputs #:allow-other-keys)
;; 		      ;; Add ocaml-integers include path
;; 		      (substitute* "Makefile.rules" (("# see GPR#1535") (string-append "-L " (assoc-ref outputs "out") "/lib/ocaml/site-lib/stublibs")))
;; 		      #t))
;; 	(add-after 'install 'link-stubs
;;          (lambda* (#:key outputs #:allow-other-keys)
;;            (let* ((out (assoc-ref outputs "out"))
;;                   (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
;;                   (lib (string-append out "/lib/ocaml/site-lib/ctypes")))
;;              (mkdir-p stubs)
;;              (symlink (string-append lib "/dllctypes_stubs.so")
;;                       (string-append stubs "/dllctypes_stubs.so"))
;;              ;; (symlink (string-append lib "/dllctypes_stubs.so")
;;              ;;          (string-append stubs "/dllctypes_stubs.so"))
;;              #t)))
;;         (delete 'configure))))
;;    (native-inputs
;;     `(("pkg-config" ,pkg-config)))
;;    (propagated-inputs 
;;     `(("bigarray-compat" ,ocaml-bigarray-compat)
;;       ("integers" ,ocaml-integers)
;;     ))
;;    (inputs
;;     `(("libffi" ,libffi)
;;             ))
;;    (synopsis "Library for binding to C libraries using pure OCaml")
;;    (description "Ctypes is a library for binding to C libraries using pure
;; OCaml.  The primary aim is to make writing C extensions as straightforward as
;; possible.  The core of ctypes is a set of combinators for describing the
;; structure of C types -- numeric types, arrays, pointers, structs, unions and
;; functions.  You can use these combinators to describe the types of the
;; functions that you want to call, then bind directly to those functions -- all
;; without writing or generating any C!")
;;    (license #f)))


;; ;;; Mesa needs LibVA headers to build its Gallium-based VA API implementation;
;; ;;; LibVA itself depends on Mesa.  We use the following to solve the circular
;; ;;; dependency.
;; (define libva-without-mesa
;;   ;; Delay to work around circular import problem.
;;   (delay
;;     (package
;;       (inherit libva)
;;       (name "libva-without-mesa")
;;       (inputs `(,@(fold alist-delete (package-inputs libva)
;;                         '("mesa" "wayland"))))
;;       (arguments
;;        (strip-keyword-arguments
;;         '(#:make-flags)
;;         (substitute-keyword-arguments (package-arguments libva)
;;           ((#:configure-flags flags)
;;            '(list "--disable-glx" "--disable-egl"))))))))

;; (define-public mesa
;;   (package
;;     (name "mesa")
;;     (version "20.0.7")

;;     ;; Mesa 20.0.5 through 20.0.7 has problems with some graphic drivers, so
;;     ;; we need this newer version.
;;     ;; https://gitlab.freedesktop.org/mesa/mesa/-/issues/2882
;;     ;; https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/4861
;;     (replacement mesa-20.0.8)

;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (list (string-append "https://mesa.freedesktop.org/archive/"
;;                                   "mesa-" version ".tar.xz")
;;                    (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
;;                                   "mesa-" version ".tar.xz")
;;                    (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
;;                                   version "/mesa-" version ".tar.xz")))
;;         (sha256
;;          (base32
;;           "0y517qpdg6v6dsdgzb365p03m30511sbyh8pq0mcvhvjwy7javpy"))

;;        (patches 
;; 	(parameterize
;; 	    ((%patch-path
;; 	      (map (lambda (directory)
;; 		     (string-append directory "/mygnu/packages/patches"))
;; 		   %load-path)))

;; 	  (search-patches "mesa-skip-disk-cache-test.patch")))
;;         ))
;;     (build-system meson-build-system)
;;     (propagated-inputs
;;       `(;; The following are in the Requires.private field of gl.pc.
;;         ("libdrm" ,libdrm)
;;         ("libvdpau" ,libvdpau)
;;         ("libx11" ,libx11)
;;         ("libxdamage" ,libxdamage)
;;         ("libxfixes" ,libxfixes)
;;         ("libxshmfence" ,libxshmfence)
;;         ("libxxf86vm" ,libxxf86vm)
;;         ("xorgproto" ,xorgproto)))
;;     (inputs
;;       `(("expat" ,expat)
;;         ("libelf" ,elfutils)  ;required for r600 when using llvm
;;         ("libva" ,(force libva-without-mesa))
;;         ("libxml2" ,libxml2)
;;         ;; TODO: Add 'libxml2-python' for OpenGL ES 1.1 and 2.0 support
;;         ("libxrandr" ,libxrandr)
;;         ("libxvmc" ,libxvmc)
;;         ,@(match (%current-system)
;;             ((or "x86_64-linux" "i686-linux")
;;              ;; Note: update the 'clang' input of mesa-opencl when bumping this.
;;              `(("llvm" ,llvm-10)))
;;             (_
;;              `()))
;;         ("wayland" ,wayland)
;;         ("wayland-protocols" ,wayland-protocols)))
;;     (native-inputs
;;       `(("bison" ,bison)
;;         ("flex" ,flex)
;;         ("gettext" ,gettext-minimal)
;;         ,@(match (%current-system)
;;             ((or "x86_64-linux" "i686-linux")
;;              `(("glslang" ,glslang)))
;;             (_
;;              `()))
;;         ("pkg-config" ,pkg-config)
;;         ("python" ,python-wrapper)
;;         ("python-mako" ,python-mako)
;;         ("which" ,(@ (gnu packages base) which))))
;;     (outputs '("out" "bin"))
;;     (arguments
;;      `(#:configure-flags
;;        '(,@(match (%current-system)
;;              ((or "armhf-linux" "aarch64-linux")
;;               ;; TODO: Fix svga driver for aarch64 and armhf.
;;               '("-Dgallium-drivers=etnaviv,freedreno,kmsro,lima,nouveau,panfrost,r300,r600,swrast,tegra,v3d,vc4,virgl"))
;;              (_
;;               '("-Dgallium-drivers=iris,nouveau,r300,r600,radeonsi,svga,swrast,virgl")))
;;          ;; Enable various optional features.  TODO: opencl requires libclc,
;;          ;; omx requires libomxil-bellagio
;;          "-Dplatforms=x11,drm,surfaceless,wayland"
;;          "-Dglx=dri"        ;Thread Local Storage, improves performance
;;          ;; "-Dopencl=true"
;;          ;; "-Domx=true"
;;          "-Dosmesa=gallium"
;;          "-Dgallium-xa=true"

;;          ;; features required by wayland
;;          "-Dgles2=true"
;;          "-Dgles3=true"
;;          "-Dgbm=true"
;;          "-Dshared-glapi=true"

;;          ;; Enable Vulkan on i686-linux and x86-64-linux.
;;          ,@(match (%current-system)
;;              ((or "i686-linux" "x86_64-linux")
;;               '("-Dvulkan-drivers=intel,amd"))
;;              (_
;;               '("-Dvulkan-drivers=auto")))

;;          ;; Enable the Vulkan overlay layer on i686-linux and x86-64-linux.
;;          ,@(match (%current-system)
;;              ((or "x86_64-linux" "i686-linux")
;;               '("-Dvulkan-overlay-layer=true"))
;;              (_
;;               '()))

;;          ;; Also enable the tests.
;;          "-Dbuild-tests=true"

;;          ;; on non-intel systems, drop i915 and i965
;;          ;; from the default dri drivers
;;          ,@(match (%current-system)
;;              ((or "x86_64-linux" "i686-linux")
;;               '("-Ddri-drivers=i915,i965,nouveau,r200,r100"
;;                 "-Dllvm=true"))         ; default is x86/x86_64 only
;;              (_
;;               '("-Ddri-drivers=nouveau,r200,r100"))))

;;        ;; XXX: 'debugoptimized' causes LTO link failures on some drivers.  The
;;        ;; documentation recommends using 'release' for performance anyway.
;;        #:build-type "release"

;;        #:modules ((ice-9 match)
;;                   (srfi srfi-1)
;;                   (guix build utils)
;;                   (guix build meson-build-system))
;;        #:phases
;;        (modify-phases %standard-phases
;;          ,@(if (string-prefix? "i686" (or (%current-target-system)
;;                                           (%current-system)))
;;                ;; Disable new test from Mesa 19 that fails on i686.  Upstream
;;                ;; report: <https://bugs.freedesktop.org/show_bug.cgi?id=110612>.
;;                `((add-after 'unpack 'disable-failing-test
;;                    (lambda _
;;                      (substitute* "src/util/tests/format/meson.build"
;;                        (("'u_format_test',") ""))
;;                      #t)))
;;                '())
;;          (add-before 'configure 'fix-dlopen-libnames
;;            (lambda* (#:key inputs outputs #:allow-other-keys)
;;              (let ((out (assoc-ref outputs "out")))
;;                ;; Remain agnostic to .so.X.Y.Z versions while doing
;;                ;; the substitutions so we're future-safe.
;;                (substitute* "src/glx/meson.build"
;;                  (("-DGL_LIB_NAME=\"lib@0@\\.so\\.@1@\"")
;;                   (string-append "-DGL_LIB_NAME=\"" out
;;                                  "/lib/lib@0@.so.@1@\"")))
;;                (substitute* "src/gbm/backends/dri/gbm_dri.c"
;;                  (("\"libglapi\\.so")
;;                   (string-append "\"" out "/lib/libglapi.so")))
;;                (substitute* "src/gbm/main/backend.c"
;;                  ;; No need to patch the gbm_gallium_drm.so reference;
;;                  ;; it's never installed since Mesa removed its
;;                  ;; egl_gallium support.
;;                  (("\"gbm_dri\\.so")
;;                   (string-append "\"" out "/lib/dri/gbm_dri.so")))
;;                #t)))
;;          (add-after 'install 'split-outputs
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (let ((out (assoc-ref outputs "out"))
;;                    (bin (assoc-ref outputs "bin")))
;;                ,@(match (%current-system)
;;                    ((or "i686-linux" "x86_64-linux")
;;                     ;; Install the Vulkan overlay control script to a separate
;;                     ;; output to prevent a reference on Python, saving ~70 MiB
;;                     ;; on the closure size.
;;                     '((copy-recursively (string-append out "/bin")
;;                                         (string-append bin "/bin"))
;;                       (delete-file-recursively (string-append out "/bin"))))
;;                    (_
;;                     ;; XXX: On architectures without the Vulkan overlay layer
;;                     ;; just create an empty file because outputs can not be
;;                     ;; added conditionally.
;;                     '((mkdir-p (string-append bin "/bin"))
;;                       (call-with-output-file (string-append bin "/bin/.empty")
;;                         (const #t)))))
;;                #t)))
;;          (add-after 'install 'symlinks-instead-of-hard-links
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              ;; All the drivers and gallium targets create hard links upon
;;              ;; installation (search for "hardlink each megadriver instance"
;;              ;; in the makefiles).  This is no good for us since we'd produce
;;              ;; nars that contain several copies of these files.  Thus, turn
;;              ;; them into symlinks, which saves ~124 MiB.
;;              (let* ((out    (assoc-ref outputs "out"))
;;                     (lib    (string-append out "/lib"))
;;                     (files  (find-files lib
;;                                         (lambda (file stat)
;;                                           (and (string-contains file ".so")
;;                                                (eq? 'regular
;;                                                     (stat:type stat))))))
;;                     (inodes (map (compose stat:ino stat) files)))
;;                (for-each (lambda (inode)
;;                            (match (filter-map (match-lambda
;;                                                 ((file ino)
;;                                                  (and (= ino inode) file)))
;;                                               (zip files inodes))
;;                              ((_)
;;                               #f)
;;                              ((reference others ..1)
;;                               (format #t "creating ~a symlinks to '~a'~%"
;;                                       (length others) reference)
;;                               (for-each delete-file others)
;;                               (for-each (lambda (file)
;;                                           (if (string=? (dirname file)
;;                                                         (dirname reference))
;;                                               (symlink (basename reference)
;;                                                        file)
;;                                               (symlink reference file)))
;;                                         others))))
;;                          (delete-duplicates inodes))
;;                #t))))))
;;     (home-page "https://mesa3d.org/")
;;     (synopsis "OpenGL and Vulkan implementations")
;;     (description "Mesa is a free implementation of the OpenGL and Vulkan
;; specifications - systems for rendering interactive 3D graphics.  A variety of
;; device drivers allows Mesa to be used in many different environments ranging
;; from software emulation to complete hardware acceleration for modern GPUs.")
;;     (license license:x11)))


;; (define mesa-20.0.8
;;   (package
;;     (inherit mesa)
;;     (version "20.0.8")
;;     (source (origin
;;               (inherit (package-source mesa))
;;               (uri (list (string-append "https://mesa.freedesktop.org/archive/"
;;                                         "mesa-" version ".tar.xz")
;;                          (string-append "ftp://ftp.freedesktop.org/pub/mesa/"
;;                                         "mesa-" version ".tar.xz")))
;;               (sha256
;;                (base32
;;                 "0v0bfh3ay07s6msxmklvwfaif0q02kq2yhy65fdhys49vw8c1w3c"))))))


(define-public ocaml-tgls
  (package
   (name "ocaml-tgls")
   (version "0.8.5")
   (source
    (origin
     (method url-fetch)
     (uri "http://erratique.ch/software/tgls/releases/tgls-0.8.5.tbz")
     (sha256
      (base32
       "0xdc8j3grir91mbwigz9jhjgnlhy5g0sm9wdcsh3ih3fzzkjrdd3"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:build-flags '("build")
      #:tests? #f; tests require a display device
      #:phases
      (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'build 'set-path
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "myocamlbuild.ml" (("glesv3") "glesv2"))
            #t))
        )))
   (propagated-inputs
    `(
      ("ocaml-integers" ,ocaml-integers)
      ("ocaml-ctypes" ,ocaml-ctypes)))
   (native-inputs
    `(("ocaml-findlib" ,ocaml-findlib)
      ("ocamlbuild" ,ocamlbuild)
      ("opam" ,opam)
      ("mesa" ,mesa)
      ("pkg-config" ,pkg-config)
      ("ocaml-topkg" ,ocaml-topkg)
      ("ocaml-tsdl" ,ocaml-tsdl)
      ("ocaml-result" ,ocaml-result)))
   (home-page "http://erratique.ch/software/tgls")
   (synopsis
    "Thin bindings to OpenGL {3,4} and OpenGL ES {2,3} for OCaml")
   (description
    "Tgls is a set of independent OCaml libraries providing thin bindings
to OpenGL libraries. It has support for core OpenGL 3.{2,3} and
4.{0,1,2,3,4} and OpenGL ES 2 and 3.{0,1,2}.

Tgls depends on [ocaml-ctypes][ctypes] and the C OpenGL library of your
platform. It is distributed under the ISC license.

[ctypes]: https://github.com/ocamllabs/ocaml-ctypes")
   (license license:isc)))


;; (define-public ocaml-tsdl
;;   (package
;;     (name "ocaml-tsdl")
;;     (version "0.9.7")
;;     (home-page "https://erratique.ch/software/tsdl")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append home-page "/releases/tsdl-"
;;                                   version ".tbz"))
;;               (file-name (string-append name "-" version ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "1zwv0ixkigh1gzk5n49rwvz2f2m62jdkkqg40j7dclg4gri7691f"))))
;;     (build-system ocaml-build-system)
;;     (arguments
;;      `(#:build-flags '("build")
;;        #:tests? #f; tests require a display device
;;        #:phases
;;        (modify-phases
;; 	%standard-phases
;; 	(delete 'configure)
;; 	;; (add-before 'build 'set-patch-makefile
;; 	;; 	    (lambda* (#:key inputs #:allow-other-keys)
;; 	;; 	      ;; Add ocaml-integers include path
;; 	;; 	      (substitute* "myocamlbuild.ml" (("@ libs_L") (string-append "@ libs_L @ [A \"-L\"; A \"" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers\"; A \"-L\"; A \"" (assoc-ref inputs "ocaml-ctypes") "/lib/ocaml/site-lib/ctypes\"]")))
;; 	;; 	      #t))

;; 	)))
;;     (native-inputs
;;      `(("ocamlbuild" ,ocamlbuild)
;;        ("ocaml-astring" ,ocaml-astring)
;;        ("opam" ,opam)
;;        ("pkg-config" ,pkg-config)))
;;     (inputs
;;      `(("topkg" ,ocaml-topkg)
;;        ("sdl2" ,sdl2)
;;        ("mesa" ,mesa)
;;        ("ocaml-integers" ,ocaml-integers)
;;        ("ocaml-ctypes" ,ocaml-ctypes)))
;;     (synopsis "Thin bindings to SDL for OCaml")
;;     (description "Tsdl is an OCaml library providing thin bindings to the
;; cross-platform SDL C library.")
;;     (license license:isc)))

;; (define-public ocaml-sodium
;;   (package
;;     (name "ocaml-sodium")
;;     (version "0.6.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/dsheets/ocaml-sodium/archive/0.6.0.tar.gz")
;;        (sha256
;; 	(base32
;; 	 "0gilc2mg0kf4rag95cl507rajcasdpnff8idv8cf58c1b90lvqbf"))))
;;     (build-system ocaml-build-system)
;;     (arguments
;;      `(
;;        #:phases
;;        (modify-phases %standard-phases
;; 	 (delete 'configure)
;; 	 (add-after 'install 'link-stubs
;; 	   (lambda* (#:key outputs #:allow-other-keys)
;; 	     (let* ((out (assoc-ref outputs "out"))
;; 		    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
;; 		    (lib (string-append out "/lib/ocaml/site-lib/sodium")))
;; 	       (mkdir-p stubs)
;; 	       (symlink (string-append lib "/dllsodium_stubs.so")
;; 			(string-append stubs "/dllsodium_stubs.so"))
;; 	       #t)))
;; 	 (add-before 'build 'set-patch-makefile
;; 	   (lambda* (#:key inputs #:allow-other-keys)
;; 	     ;; Use gcc instead of cc
;; 	     (substitute* "myocamlbuild.ml"
;; 	       (("\"cc\"") "\"gcc\""))
;; 	     ;; Add ocaml-integers include path
;; 	     (substitute* "myocamlbuild.ml"
;; 	       (("A\"-o\";") (string-append "A(\"-I\"); "
;; 					    "A \"" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers\"; "
;; 					    "A\"-o\";")))
;; 	     ;; Add dll load path
;; 	     (substitute* "Makefile"
;; 	       (("^OCAMLBUILD=..ENV. ocamlbuild")
;; 		(string-append "OCAMLBUILD=$(ENV) ocamlbuild -ocamlc 'ocamlc -dllpath-all' -cflags -ccopt,-I'" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers' -cflags -ccopt,-Wno-discarded-qualifiers")))
;; 	     #t)))))
;;     (propagated-inputs
;;      `(("libsodium" ,libsodium)
;;        ("ocaml-integers" ,ocaml-integers)
;;        ("ocaml-ctypes" ,ocaml-ctypes)))
;;     (native-inputs
;;      `(("ocaml-findlib" ,ocaml-findlib)
;;        ("ocamlbuild" ,ocamlbuild)
;;        ("ocaml-ounit" ,ocaml-ounit)))
;;     (home-page
;;      "https://github.com/dsheets/ocaml-sodium/")
;;     (synopsis "Binding to libsodium UNAUDITED")
;;     (description
;;      "Binding to libsodium 1.0.9+, a shared library wrapper for djb's NaCl.

;; Binding uses ctypes' stub generation system. GNU/Linux, FreeBSD, and OS
;; X are supported.

;; UNAUDITED")
;;     (license #f)))

;; (define-public ocaml-sodium2
;;   (package
;;     (name "ocaml-sodium2")
;;     (version "0.6.0")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri "https://github.com/dsheets/ocaml-sodium/archive/0.6.0.tar.gz")
;;        (sha256
;; 	(base32
;; 	 "0gilc2mg0kf4rag95cl507rajcasdpnff8idv8cf58c1b90lvqbf"))))
;;     (build-system ocaml-build-system)
;;     (arguments
;;      `(
;;        #:phases
;;        (modify-phases %standard-phases
;; 	 (delete 'configure)
;; 	 (add-after 'install 'link-stubs
;; 	   (lambda* (#:key outputs #:allow-other-keys)
;; 	     (let* ((out (assoc-ref outputs "out"))
;; 		    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
;; 		    (lib (string-append out "/lib/ocaml/site-lib/sodium")))
;; 	       (mkdir-p stubs)
;; 	       (symlink (string-append lib "/dllsodium_stubs.so")
;; 			(string-append stubs "/dllsodium_stubs.so"))
;; 	       #t)))
;; 	 (add-before 'build 'set-patch-makefile
;; 	   (lambda* (#:key inputs #:allow-other-keys)
;; 	     ;; Use gcc instead of cc
;; 	     (substitute* "myocamlbuild.ml"
;; 	       (("\"cc\"") "\"gcc\""))
;; 	     ;; Add ocaml-integers include path
;; 	     (substitute* "myocamlbuild.ml"
;; 	       (("A\"-o\";") (string-append "A(\"-I\"); "
;; 					    "A \"" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers\"; "
;; 					    "A\"-o\";")))
;; 	     ;; Add dll load path
;; 	     (substitute* "Makefile"
;; 	       (("^OCAMLBUILD=..ENV. ocamlbuild")
;; 		(string-append "OCAMLBUILD=$(ENV) ocamlbuild -ocamlc 'ocamlc -dllpath-all' -cflags -ccopt,-I'" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers' -cflags -ccopt,-Wno-discarded-qualifiers")))
;; 	     #t)))))
;;     (propagated-inputs
;;      `(("libsodium" ,libsodium)
;;        ("ocaml-integers" ,ocaml-integers)
;;        ("ocaml-ctypes" ,ocaml-ctypes)))
;;     (native-inputs
;;      `(("ocaml-findlib" ,ocaml-findlib)
;;        ("ocamlbuild" ,ocamlbuild)
;;        ("ocaml-ounit" ,ocaml-ounit)))
;;     (home-page
;;      "https://github.com/dsheets/ocaml-sodium/")
;;     (synopsis "Binding to libsodium UNAUDITED")
;;     (description
;;      "Binding to libsodium 1.0.9+, a shared library wrapper for djb's NaCl.

;; Binding uses ctypes' stub generation system. GNU/Linux, FreeBSD, and OS
;; X are supported.

;; UNAUDITED")
;;     (license #f)))

(define-public sof-firmware
  (package
    (name "sof-firmware")
    (version "v1.6.1")
    (home-page "https://github.com/thesofproject/sof-bin.git")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/thesofproject/sof-bin.git")
		    (commit "53db4982df0f501efce5e01ced83797cf68ecce1")))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32 "1fd96ak4kgk88bqgjmqyn4jjd1p9kxqkpm9aranazr256h11fv5z"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-apl-v1.6.1.ri" "lib/firmware/intel/sof/sof-apl.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-apl-v1.6.1.ri" "lib/firmware/intel/sof/sof-glk.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-cnl-v1.6.1.ri" "lib/firmware/intel/sof/sof-cfl.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-cnl-v1.6.1.ri" "lib/firmware/intel/sof/sof-cnl.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-cnl-v1.6.1.ri" "lib/firmware/intel/sof/sof-cml.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-icl-v1.6.1.ri" "lib/firmware/intel/sof/sof-icl.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-tgl-v1.6.1.ri" "lib/firmware/intel/sof/sof-tgl.ri")
         ("lib/firmware/intel/sof/v1.6.1/intel-signed/sof-ehl-v1.6.1.ri" "lib/firmware/intel/sof/sof-ehl.ri")
         ("lib/firmware/intel/sof/v1.6.1/sof-bdw-v1.6.1.ri" "lib/firmware/intel/sof/sof-bdw.ri")
         ("lib/firmware/intel/sof/v1.6.1/sof-byt-v1.6.1.ri" "lib/firmware/intel/sof/sof-byt.ri")
         ("lib/firmware/intel/sof/v1.6.1/sof-cht-v1.6.1.ri" "lib/firmware/intel/sof/sof-cht.ri")
         ("lib/firmware/intel/sof-tplg-v1.6.1" "lib/firmware/intel/sof-tplg"))))
    (synopsis "Sound Open Firmware")
    (description "Free.")
    (license #f)))

(define-public intel-gmmlib
  (package
    (name "intel-gmmlib")
    (version "20.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/gmmlib.git")
             (commit "6f15b795e1511febf233b42d7dde603f52fcfdab")))
       (sha256
        (base32 "0qb0wpinfv8lg1pq1pxkl6v0kd8ax86m8zxzm6zjx91alsch1mi6"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) 
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://01.org/linuxmedia/vaapi")
    (synopsis "VA-API video acceleration driver for Intel GEN Graphics devices")
    (description
     "This is the @acronym{VA-API, Video Acceleration API} back end required for
hardware-accelerated video processing on Intel GEN Graphics devices supported by
the i915 driver, such as integrated Intel HD Graphics.  It provides access to
both hardware and shader functionality for faster encoding, decoding, and
post-processing of video formats like MPEG2, H.264/AVC, and VC-1.")
    (license #f)))    ; the rest, excluding the test suite


    

(define-public intel-media-driver
  (package
    (name "intel-media-driver")
    (version "21.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/media-driver.git")
             (commit "13c0e1480a96510ca1d3fff9f2e8574e1fba1979")))
       (sha256
        (base32 "17cgs52f42jdvfb6q3wpkxaz2b41z59jdribpgb9qmcvizsnglxc"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libva" ,libva)
       ("intel-gmmlib" ,intel-gmmlib)
       ("libx11" ,libx11)))
    (arguments
     `(#:tests? #f

       #:configure-flags (list (string-append "-DLIBVA_DRIVERS_PATH=" %output "/lib/dri"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "LIBVA_DRIVERS_PATH" (string-append out "/lib/dri"))
               #t))))))
    ;; XXX Because of <https://issues.guix.gnu.org/issue/22138>, we need to add
    ;; this to all VA-API back ends instead of once to libva.
    (native-search-paths
     (list (search-path-specification
            (variable "LIBVA_DRIVERS_PATH")
            (files '("lib/dri")))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://01.org/linuxmedia/vaapi")
    (synopsis "VA-API video acceleration driver for Intel GEN Graphics devices")
    (description
     "This is the @acronym{VA-API, Video Acceleration API} back end required for
hardware-accelerated video processing on Intel GEN Graphics devices supported by
the i915 driver, such as integrated Intel HD Graphics.  It provides access to
both hardware and shader functionality for faster encoding, decoding, and
post-processing of video formats like MPEG2, H.264/AVC, and VC-1.")
    (license #f)))    ; the rest, excluding the test suite


    
(define-public ocaml-variantslib
  (package
    (name "ocaml-variantslib")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/variantslib-v0.15.0.tar.gz")
        (sha256
          (base32
            "12dssx4by6rgjzfrvksz83hkcpmsq0brn87dh22pv1rrwhw79n75"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-base" ,ocaml-base)))
    (home-page
      "https://github.com/janestreet/variantslib")
    (synopsis "Part of Jane Street's Core library")
    (description
      "
The Core suite of libraries is an industrial strength alternative to
OCaml's standard library that was developed by Jane Street, the
largest industrial user of OCaml.
")
    (license #f)))

(define-public ocaml-ppx-variants-conv
  (package
  (name "ocaml-ppx-variants-conv")
  (version "0.15.0")
  (source
    (origin
      (method url-fetch)
      (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_variants_conv-v0.15.0.tar.gz")
      (sha256
        (base32 "175cvwwz624x81yhzd4547vk8hjplhkbbll5ds8zmhmnwzv9cw3f"))))
  (build-system dune-build-system)
  (propagated-inputs (list ocaml-base ocaml-variantslib ocaml-ppxlib))
  (properties `((upstream-name . "ppx_variants_conv")))
  (home-page "https://github.com/janestreet/ppx_variants_conv")
  (synopsis
    "Generation of accessor and iteration functions for ocaml variant types")
  (description " Part of the Jane Street's PPX rewriters collection.")
  (license license:expat)))

(define-public ocaml-ppx-optcomp
  (package
   (name "ocaml-ppx-optcomp")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_optcomp-v0.15.0.tar.gz")
     (sha256
      (base32 "1vd6ra0f1cm1kiqdh0iyzh24bp0ndl3s2va5z0mrw3y8ngbp0kn4"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-base ocaml-stdio ocaml-ppxlib))
   (properties `((upstream-name . "ppx_optcomp")))
   (home-page "https://github.com/janestreet/ppx_optcomp")
   (synopsis "Optional compilation for OCaml")
   (description " Part of the Jane Street's PPX rewriters collection.")
   (license license:expat)))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/fieldslib-v0.15.0.tar.gz")
        (sha256
          (base32
            "083izf854vzmi5zj63r7ipjf09y1dqf7iy8n6r4663444xrzs2h5"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-base" ,ocaml-base)))
    (home-page
      "https://github.com/janestreet/fieldslib")
    (synopsis
      "Syntax extension to define first class values representing record fields, to get and set record fields, iterate and fold over all fields of a record and create new record values")
    (description
      "
Part of Jane Street's Core library
The Core suite of libraries is an industrial strength alternative to
OCaml's standard library that was developed by Jane Street, the
largest industrial user of OCaml.
")
    (license #f)))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_fields_conv-v0.15.0.tar.gz")
        (sha256
          (base32
            "0q9fnv8jnwzgba90y8y0mx35nqnsj9yjm7xzw9ksyfq47fvb6anj"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-fieldslib" ,ocaml-fieldslib)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_fields_conv")))
    (home-page
      "https://github.com/janestreet/ppx_fields_conv")
    (synopsis
      "Generation of accessor and iteration functions for ocaml records")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-sexplib
  (package
    (name "ocaml-sexplib")
    (version "0.15.0")
    (home-page "https://github.com/janestreet/sexplib")
    (source
     (janestreet-origin "sexplib" version
                        "1xs55f11yhscnfrzpvy1vn05j6xi9kxy097465624l615j7k8qm5"))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base ocaml-num ocaml-parsexp ocaml-sexplib0))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-sexplib))))
    (synopsis
     "Library for serializing OCaml values to and from S-expressions")
    (description
     "This package is part of Jane Street's Core library.  Sexplib contains
functionality for parsing and pretty-printing s-expressions.")
    (license license:expat)))

(define-public ocaml-ppxlib
  (package
    (name "ocaml-ppxlib")
    (version "0.24.0")
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nkkdvqifa36hxj6msd74ld4dfd749d6d9ygfj7zsm328rqvpqf2"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ocaml-base
           ocaml-compiler-libs
           ocaml-migrate-parsetree
           ocaml-stdlib-shims
           ocaml-ppx-derivers
           ocaml-stdio
           ocaml-result
           ocaml-sexplib0))
    (properties `((ocaml4.07-variant . ,(delay ocaml4.07-ppxlib))))
    (synopsis
     "Base library and tools for ppx rewriters")
    (description
     "A comprehensive toolbox for ppx development.  It features:
@itemize
@item an OCaml AST / parser / pretty-printer snapshot, to create a full frontend
independent of the version of OCaml;
@item a library for library for ppx rewriters in general, and type-driven code
generators in particular;
@item
a feature-full driver for OCaml AST transformers;
@item a quotation mechanism allowing to write values representing the
OCaml AST in the OCaml syntax;
@item a generator of open recursion classes from type definitions.
@end itemize")
    (license license:expat)))

(define-public ocaml-sexplib0
  (package
   (name "ocaml-sexplib0")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/sexplib0-v0.15.0.tar.gz")
     (sha256
      (base32 "1fpg991n578m11r0ki4als4c76s3sp703b4khivx40v48402qill"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f))
   (home-page "https://github.com/janestreet/sexplib0")
   (synopsis
    "Library containing the definition of S-expressions and some base converters")
   (description
    " Part of Jane Street's Core library The Core suite of libraries is an industrial
strength alternative to OCaml's standard library that was developed by Jane
Street, the largest industrial user of OCaml.")
   (license license:expat)))


(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_sexp_conv-v0.15.0.tar.gz")
        (sha256
          (base32
            "1043bbck0nff6qchcy72wvd8bmlg3v85xaabjnwfvb3rl7pixkfr"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-sexplib0" ,ocaml-sexplib0)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_sexp_conv")))
    (home-page
      "https://github.com/janestreet/ppx_sexp_conv")
    (synopsis
      "[@@deriving] plugin to generate S-expression conversion functions")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_custom_printf-v0.15.0.tar.gz")
        (sha256
          (base32
            "1056lgr0hfx0wry9q8c8v79llrf47yakqdchmk5giayv7ivsf35f"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_custom_printf")))
    (home-page
      "https://github.com/janestreet/ppx_custom_printf")
    (synopsis
      "Printf-style format-strings for user-defined string conversion")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-stdio
  (package
    (name "ocaml-stdio")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/stdio-v0.15.0.tar.gz")
        (sha256
          (base32
            "0jsyg4jlp76d9gx1fngms6nfs7dcpsysdsvkywjq9a663n994wn3"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-base" ,ocaml-base)))
    (home-page "https://github.com/janestreet/stdio")
    (synopsis "Standard IO library for OCaml")
    (description
      "
Stdio implements simple input/output functionalities for OCaml.

It re-exports the input/output functions of the OCaml standard
libraries using a more consistent API.
")
    (license #f)))

(define-public ocaml-cinaps
  (package
    (name "ocaml-cinaps")
    (version "0.15.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ocaml-ppx/cinaps/archive/v0.15.1.tar.gz")
        (sha256
          (base32
            "0w3125jfwckvmd3dx3r0qx7hj9kahdi9nqf3cqsv19nqymq8xq8v"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs `(("ocaml-re" ,ocaml-re)))
    (home-page "https://github.com/ocaml-ppx/cinaps")
    (synopsis "Trivial metaprogramming tool")
    (description
      "
Cinaps is a trivial Metaprogramming tool using the OCaml toplevel.  It
is based on the same idea as expectation tests. The user write some
OCaml code inside special comments and cinaps make sure that what
follows is what is printed by the OCaml code.
")
    (license #f)))

;; (define-public ocaml-compiler-libs
;;   (package
;;     (name "ocaml-compiler-libs")
;;     (version "0.12.4")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri "https://github.com/janestreet/ocaml-compiler-libs/releases/download/v0.12.4/ocaml-compiler-libs-v0.12.4.tbz")
;;         (sha256
;;           (base32
;;             "0q3pl20pkx410gw9g4m26qq6dmzi9qan2dqlga6c2ifc6pnckjaf"))))
;;     (build-system dune-build-system)
;;     (arguments `(#:tests? #f))
;;     (properties
;;       `((upstream-name . "ocaml-compiler-libs")))
;;     (home-page
;;       "https://github.com/janestreet/ocaml-compiler-libs")
;;     (synopsis "OCaml compiler libraries repackaged")
;;     (description
;;       "This packages exposes the OCaml compiler libraries repackages under
;; the toplevel names Ocaml_common, Ocaml_bytecomp, Ocaml_optcomp, ...
;; ")
;;     (license #f)))


(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/ppx_compare-v0.15.0.tar.gz")
        (sha256
          (base32
            "11bkw7fgzfay8ws0piwphqip3y2lk2c9s2gil3zisnbvka92h1va"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_compare")))
    (home-page
      "https://github.com/janestreet/ppx_compare")
    (synopsis
      "Generation of comparison functions from types")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))



(define-public ocaml-bin-prot
  (package
    (name "ocaml-bin-prot")
    (version "0.15.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/bin_prot-v0.15.0.tar.gz")
        (sha256
          (base32
            "141nkl2qiwgii7biz074vrjbp6rsqkx9mx78nyf7xf6gxbda0jvs"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-ppx-compare" ,ocaml-ppx-compare)
        ("ocaml-ppx-custom-printf"
         ,ocaml-ppx-custom-printf)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-optcomp" ,ocaml-ppx-optcomp)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppx-variants-conv"
         ,ocaml-ppx-variants-conv)))
    (properties `((upstream-name . "bin_prot")))
    (home-page
      "https://github.com/janestreet/bin_prot")
    (synopsis "A binary protocol generator")
    (description
      "
Part of Jane Street's Core library
The Core suite of libraries is an industrial strength alternative to
OCaml's standard library that was developed by Jane Street, the
largest industrial user of OCaml.
")
    (license #f)))

(define-public ocaml-incremental
  (package
   (name "ocaml-incremental")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/incremental-v0.15.0.tar.gz")
     (sha256
      (base32 "0dhv6bmxqjrnbf1ppikrvyhl0fx68jkp439hc25hcliyzq8y3iwd"))))
   (build-system dune-build-system)
   (propagated-inputs (list ocaml-core ocaml-core-kernel ocaml-ppx-jane))
   (home-page "https://github.com/janestreet/incremental")
   (synopsis "Library for incremental computations")
   (description
    " Part of Jane Street's Core library The Core suite of libraries is an industrial
                   strength alternative to OCaml's standard library that was developed by Jane
                   Street, the largest industrial user of OCaml.")
   (license license:expat)))

(define-public ocaml-base
  (package
   (name "ocaml-base")
   (version "0.15.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://ocaml.janestreet.com/ocaml-core/v0.15/files/base-v0.15.0.tar.gz")
     (sha256
      (base32 "0gd5lq9474hbgzczb07pazqrbnnjjyfw8i9225bq95594i1swmw6"))))
   (build-system dune-build-system)
   (arguments `(#:tests? #f))
   (propagated-inputs (list ocaml-sexplib0))
   (home-page "https://github.com/janestreet/base")
   (synopsis "Full standard library replacement for OCaml")
   (description
    " Full standard library replacement for OCaml

Base is a complete and portable alternative to the OCaml standard library.  It
provides all standard functionalities one would expect from a language standard
library.  It uses consistent conventions across all of its module.

Base aims to be usable in any context.  As a result system dependent features
such as I/O are not offered by Base.  They are instead provided by companion
libraries such as stdio:

  https://github.com/janestreet/stdio")
   (license license:expat)))

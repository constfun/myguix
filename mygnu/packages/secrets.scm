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
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libffi) 
  #:use-module (gnu packages version-control)
  #:use-module (mygnu packages ocaml-extunix)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gawk))

;; (define-public libev
;;   (package
;;     (name "libev")
;;     (version "4.31")
;;     (source (origin
;;               (method url-fetch)
;;               (uri (string-append "http://dist.schmorp.de/libev/Attic/libev-"
;;                                   version
;;                                   ".tar.gz"))
;;               (sha256
;;                (base32
;;                 "0nkfqv69wfyy2bpga4d53iqydycpik8jp8x6q70353hia8mmv1gd"))))
;;     (build-system gnu-build-system)
;;     ;; (arguments
;;     ;;  '(#:configure-flags '("--disable-static")))
;;     (home-page "http://software.schmorp.de/pkg/libev.html")
;;     (synopsis "Event loop loosely modelled after libevent")
;;     (description
;;      "libev provides a full-featured and high-performance event loop that is
;; loosely modelled after libevent.  It includes relative timers, absolute timers
;; with customized rescheduling, synchronous signals, process status change
;; events, event watchers dealing with the event loop itself, file watchers, and
;; limited support for fork events.")
;;     (license
;;      (list bsd-2 gpl2+))))



;; (define dune-bootstrap
;;   (package
;;     (name "dune")
;;     (version "2.8.5")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                      (url "https://github.com/ocaml/dune")
;;                      (commit version)))
;;               (file-name (git-file-name name version))
;;               (sha256
;;                (base32
;;                 "10qgx83fq8b522y9mpllrp0l5cgmr2bs5s7aix5img21hlbm34in"))))
;;     (build-system ocaml-build-system)
;;     (arguments
;;      `(#:tests? #f; require odoc
;;        #:make-flags (list "release"
;;                           (string-append "PREFIX=" (assoc-ref %outputs "out"))
;;                           (string-append "LIBDIR=" (assoc-ref %outputs "out")
;;                                          "/lib/ocaml/site-lib"))
;;        #:phases
;;        (modify-phases %standard-phases
;;          (replace 'configure
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (mkdir-p "src/dune")
;;              (invoke "./configure")
;;              #t)))))
;;     (home-page "https://github.com/ocaml/dune")
;;     (synopsis "OCaml build system")
;;     (description "Dune is a build system that was designed to simplify the
;; release of Jane Street packages.  It reads metadata from @file{dune} files
;; following a very simple s-expression syntax.")
;;     (license #f)))

;; (define-public dune
;;   (package
;;     (inherit dune-bootstrap)
;;     (propagated-inputs
;;      `(("dune-configurator" ,dune-configurator)))
;;     (properties `((ocaml4.07-variant . ,(delay ocaml4.07-dune))
;;                   (ocaml4.09-variant . ,(delay ocaml4.09-dune))))))



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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/typerep-v0.14.0.tar.gz")
        (sha256
          (base32
            "0rmp5jsjg6sgn5yx0pcvch0phs7nak2fg1d48g5sjcyyyj8n1279"))))
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
    (version "0.14.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_typerep_conv/archive/v0.14.2.tar.gz")
        (sha256
          (base32
            "1g1sb3prscpa7jwnk08f50idcgyiiv0b9amkl0kymj5cghkdqw0n"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_string/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "1a8f7bplbxvwm4lh0m57j89jkwkxfm9r5ndcvvlj5v6py8pv69wj"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_stable/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "1dw8ilrvi5lssxnbflnzskmyi3k93ij2kbyz49y93agv0b8dsq01"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_pipebang-v0.14.0.tar.gz")
        (sha256
          (base32
            "19afbbvy72i1347prvkpy3ms75xnk7kl2hn83h40p6yh27100hky"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_optional-v0.14.0.tar.gz")
        (sha256
          (base32
            "1hh6ivlp1qpvyn8l0vhrahkkcp3scf7km254sgplprmk10wnyidz"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_module_timer-v0.14.0.tar.gz")
        (sha256
          (base32
            "04a7vzk4s3jn6wj94q0hn8kd9vxlzkpcq5ifpvz3bdfgmypjks5z"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_fixed_literal-v0.14.0.tar.gz")
        (sha256
          (base32
            "0w0a06143mhmczbpr0lfb66r6im7075gck4p0idbcari63sximqj"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_expect/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "188zaqdqxqw9xbga1ip4yi5f9p41zgdqr51idcnw4yayvdxg81q3"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_bin_prot-v0.14.0.tar.gz")
        (sha256
          (base32
            "0wa2jmvm2k88b37pbcafy1mdf5iaip0yxg5dw774sbh28nm08m2s"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/jst-config-v0.14.0.tar.gz")
        (sha256
          (base32
            "1fppr29vn91zpqda8jlnp8bcssd4bf3rn36ig8fnd1yhjrsvz8f6"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/jane-street-headers-v0.14.0.tar.gz")
        (sha256
          (base32
            "028yxb4h3iy025iy89v8653m5brh7flrjshghs4x99pd690pmfs7"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/time_now-v0.14.0.tar.gz")
        (sha256
          (base32
            "0hkn2jw4dz5gflnsblskl5wp6z7zbrahwjmaxmsskfviwjg82cqh"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_inline_test/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "0qzvm8rg07annl8zpqlhzx1z8ahrrf02r1brd43ykqas5sww3rfp"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_bench/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "1jr3cf4zsk894x64c8ir9ap9l412q35b2605pr7flrlxbm4vkf3f"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_assert-v0.14.0.tar.gz")
        (sha256
          (base32
            "1l2rr4jz2q5b35ryn2z146z7m9v6k8krp5gpn8ilib66mnz5zx15"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/splittable_random-v0.14.0.tar.gz")
        (sha256
          (base32
            "185rpmdnrzs80br138pnjbx9hfp1046zvj1ap0brq1sxdwzak6lf"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_sexp_value-v0.14.0.tar.gz")
        (sha256
          (base32
            "0yc6i1yx9mb8pwjkswy09aqg5kz1hgrpjyniq2v6whfjvxl1qrkj"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_here-v0.14.0.tar.gz")
        (sha256
          (base32
            "0b444djy68v6ji0ypwv5l02pkl151qzrgg96lyhl8dxfrzvj1zkj"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_sexp_message/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "06d1cx8nh6chgx09lqjgsagc02lfsvv18fydrviqjvydx52m2qjf"))))
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

(define-public ocaml-ppx-let
  (package
    (name "ocaml-ppx-let")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_let-v0.14.0.tar.gz")
        (sha256
          (base32
            "1qcrnd86pbr1di5m6z4ps4p15qawwa02jxwz3xfd82hdbjmdwf1s"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_js_style-v0.14.0.tar.gz")
        (sha256
          (base32
            "141fgxfipfn5jdaxc946dmp5y4fcnbhxms9maji9ddywgx82ya7l"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-octavius" ,ocaml-octavius)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_js_style")))
    (home-page
      "https://github.com/janestreet/ppx_js_style")
    (synopsis
      "Code style checker for Jane Street Packages")
    (description
      "
Part of the Jane Street's PPX rewriters collection.

This packages is a no-op ppx rewriter. It is used as a 'lint' tool to
enforce some coding conventions across all Jane Street packages.
")
    (license #f)))



(define-public ocaml-ppx-hash
  (package
    (name "ocaml-ppx-hash")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_hash-v0.14.0.tar.gz")
        (sha256
          (base32
            "0x4wgdvhgd8a49bzari52jpkykxpv6ncgp5ncda3xgg0a9r49s8n"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_enumerate-v0.14.0.tar.gz")
        (sha256
          (base32
            "1ij6sffgqhnjwnj9brhrrw1c6xgxlh0s6r17x1qkgnyrc73gfsz8"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_cold-v0.14.0.tar.gz")
        (sha256
          (base32
            "1madfzhpir9amnxmg530n70vll0jrl59vyp71miji73i6b9sy6n2"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_base-v0.14.0.tar.gz")
        (sha256
          (base32
            "0b7a3fmi90jk8paz0g36yzaq670fbnrbi1j8r5ibh9wbcfli7ji6"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/base_quickcheck/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "0n5h0ysn593awvz4crkvzf5r800hd1c55bx9mm9vbqs906zii6mn"))))
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

(define-public ocaml-ppx-jane
  (package
    (name "ocaml-ppx-jane")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_jane-v0.14.0.tar.gz")
        (sha256
          (base32
            "18js98xdqf8d54sjn1gccjkwbv2p56qy7bhvjgk94pr3fipfz0v7"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base-quickcheck" ,ocaml-base-quickcheck)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-bench" ,ocaml-ppx-bench)
        ("ocaml-ppx-bin-prot" ,ocaml-ppx-bin-prot)
        ("ocaml-ppx-custom-printf"
         ,ocaml-ppx-custom-printf)
        ("ocaml-ppx-expect" ,ocaml-ppx-expect)
        ("ocaml-ppx-fields-conv" ,ocaml-ppx-fields-conv)
        ("ocaml-ppx-fixed-literal"
         ,ocaml-ppx-fixed-literal)
        ("ocaml-ppx-here" ,ocaml-ppx-here)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-let" ,ocaml-ppx-let)
        ("ocaml-ppx-module-timer"
         ,ocaml-ppx-module-timer)
        ("ocaml-ppx-optcomp" ,ocaml-ppx-optcomp)
        ("ocaml-ppx-optional" ,ocaml-ppx-optional)
        ("ocaml-ppx-pipebang" ,ocaml-ppx-pipebang)
        ("ocaml-ppx-sexp-message"
         ,ocaml-ppx-sexp-message)
        ("ocaml-ppx-sexp-value" ,ocaml-ppx-sexp-value)
        ("ocaml-ppx-stable" ,ocaml-ppx-stable)
        ("ocaml-ppx-string" ,ocaml-ppx-string)
        ("ocaml-ppx-typerep-conv"
         ,ocaml-ppx-typerep-conv)
        ("ocaml-ppx-variants-conv"
         ,ocaml-ppx-variants-conv)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_jane")))
    (home-page
      "https://github.com/janestreet/ppx_jane")
    (synopsis "Standard Jane Street ppx rewriters")
    (description
      "
This package installs a ppx-jane executable, which is a ppx driver
including all standard Jane Street ppx rewriters.
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

(define-public ocaml-core-kernel
  (package
    (name "ocaml-core-kernel")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/core_kernel/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "0f24sagyzhfr6x68fynhsn5cd1p72vkqm25wnfg8164sivas148x"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-base-bigstring" ,ocaml-base-bigstring)
        ("ocaml-base-quickcheck" ,ocaml-base-quickcheck)
        ("ocaml-bin-prot" ,ocaml-bin-prot)
        ("ocaml-fieldslib" ,ocaml-fieldslib)
        ("ocaml-jane-street-headers"
         ,ocaml-jane-street-headers)
        ("ocaml-jst-config" ,ocaml-jst-config)
        ("ocaml-ppx-assert" ,ocaml-ppx-assert)
        ("ocaml-ppx-base" ,ocaml-ppx-base)
        ("ocaml-ppx-hash" ,ocaml-ppx-hash)
        ("ocaml-ppx-inline-test" ,ocaml-ppx-inline-test)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)
        ("ocaml-ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
        ("ocaml-ppx-sexp-message"
         ,ocaml-ppx-sexp-message)
        ("ocaml-sexplib" ,ocaml-sexplib)
        ("ocaml-splittable-random"
         ,ocaml-splittable-random)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-time-now" ,ocaml-time-now)
        ("ocaml-typerep" ,ocaml-typerep)
        ("ocaml-variantslib" ,ocaml-variantslib)))
    (properties `((upstream-name . "core_kernel")))
    (home-page
      "https://github.com/janestreet/core_kernel")
    (synopsis
      "Industrial strength alternative to OCaml's standard library")
    (description
      "
The Core suite of libraries is an industrial strength alternative to
OCaml's standard library that was developed by Jane Street, the
largest industrial user of OCaml.

Core_kernel is the system-independent part of Core.
")
    (license #f)))

(define-public ocaml-core
  (package
    (name "ocaml-core")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/core/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "1862zsk85i00vsv2chgb156b1chp8f7p508hsz6sadjx6h98q5cc"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
      `(("ocaml-core-kernel" ,ocaml-core-kernel)
        ("ocaml-jst-config" ,ocaml-jst-config)
        ("ocaml-ppx-jane" ,ocaml-ppx-jane)
        ("ocaml-sexplib" ,ocaml-sexplib)
        ("ocaml-timezone" ,ocaml-timezone)
        ("ocaml-spawn" ,ocaml-spawn)))
    (home-page "https://github.com/janestreet/core")
    (synopsis
      "Industrial strength alternative to OCaml's standard library")
    (description
      "
The Core suite of libraries is an industrial strength alternative to
OCaml's standard library that was developed by Jane Street, the
largest industrial user of OCaml.
")
    (license #f)))


(define-public secrets
  (package
    (name "secrets")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/constfun/secrets/archive/f620d2c5a0723e17a4b0239bfdd081a8ff8c16d6.tar.gz")
       (sha256
	(base32
         "08xvc6yp7hkw5cq69v1azqs7vh0vm4k2xl9wr2rz1vjiscp68anr"))))
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

(define-public ocaml-ctypes
  (package
   (name "ocaml-ctypes")
   (version "0.14.0")
   (home-page "https://github.com/ocamllabs/ocaml-ctypes")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url home-page)
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1b2q3h63ngf4x9qp65qwapf2dg9q0mcdah6qjm2q0c7v2p5vysv9"))))
   (build-system ocaml-build-system)
   (arguments
    `(#:tests? #f; require an old lwt
      #:make-flags
      (list (string-append "INSTALL_HEADERS = $(wildcard $($(PROJECT).dir)/*.h)"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'make-writable
          (lambda _
            (for-each
              (lambda (file)
                (let ((stat (stat file)))
                  (chmod file (+ #o200 (stat:mode stat)))))
              (find-files "." "."))
            #t))
	(add-after 'install 'link-stubs
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                  (lib (string-append out "/lib/ocaml/site-lib/ctypes")))
             (mkdir-p stubs)
             (symlink (string-append lib "/dllctypes_stubs.so")
                      (string-append stubs "/dllctypes_stubs.so"))
             #t)))
        (delete 'configure))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("libffi" ,libffi)
      ("ounit" ,ocaml-ounit)
      ("integers" ,ocaml-integers)
      ("lwt" ,ocaml-lwt)
      ("topkg" ,ocaml-topkg)
      ("opam" ,opam)))
   (synopsis "Library for binding to C libraries using pure OCaml")
   (description "Ctypes is a library for binding to C libraries using pure
OCaml.  The primary aim is to make writing C extensions as straightforward as
possible.  The core of ctypes is a set of combinators for describing the
structure of C types -- numeric types, arrays, pointers, structs, unions and
functions.  You can use these combinators to describe the types of the
functions that you want to call, then bind directly to those functions -- all
without writing or generating any C!")
   (license #f)))

(define-public ocaml-sodium
  (package
    (name "ocaml-sodium")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/dsheets/ocaml-sodium/archive/0.6.0.tar.gz")
       (sha256
	(base32
	 "0gilc2mg0kf4rag95cl507rajcasdpnff8idv8cf58c1b90lvqbf"))))
    (build-system ocaml-build-system)
    (arguments
     `(
       #:phases
       (modify-phases %standard-phases
	 (delete 'configure)
	 (add-after 'install 'link-stubs
	   (lambda* (#:key outputs #:allow-other-keys)
	     (let* ((out (assoc-ref outputs "out"))
		    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
		    (lib (string-append out "/lib/ocaml/site-lib/sodium")))
	       (mkdir-p stubs)
	       (symlink (string-append lib "/dllsodium_stubs.so")
			(string-append stubs "/dllsodium_stubs.so"))
	       #t)))
	 (add-before 'build 'set-patch-makefile
	   (lambda* (#:key inputs #:allow-other-keys)
	     ;; Use gcc instead of cc
	     (substitute* "myocamlbuild.ml"
	       (("\"cc\"") "\"gcc\""))
	     ;; Add ocaml-integers include path
	     (substitute* "myocamlbuild.ml"
	       (("A\"-o\";") (string-append "A(\"-I\"); "
					    "A \"" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers\"; "
					    "A\"-o\";")))
	     ;; Add dll load path
	     (substitute* "Makefile"
	       (("^OCAMLBUILD=..ENV. ocamlbuild")
		(string-append "OCAMLBUILD=$(ENV) ocamlbuild -ocamlc 'ocamlc -dllpath-all' -cflags -ccopt,-I'" (assoc-ref inputs "ocaml-integers") "/lib/ocaml/site-lib/integers' -cflags -ccopt,-Wno-discarded-qualifiers")))
	     #t)))))
    (propagated-inputs
     `(("libsodium" ,libsodium)
       ("ocaml-integers" ,ocaml-integers)
       ("ocaml-ctypes" ,ocaml-ctypes)))
    (native-inputs
     `(("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)
       ("ocaml-ounit" ,ocaml-ounit)))
    (home-page
     "https://github.com/dsheets/ocaml-sodium/")
    (synopsis "Binding to libsodium UNAUDITED")
    (description
     "Binding to libsodium 1.0.9+, a shared library wrapper for djb's NaCl.

Binding uses ctypes' stub generation system. GNU/Linux, FreeBSD, and OS
X are supported.

UNAUDITED")
    (license #f)))

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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/variantslib-v0.14.0.tar.gz")
        (sha256
          (base32
            "11zp27gh282dx9ifbhcp6i7fkc97fvk8amaj58mf1g1hwklc0lm3"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_variants_conv/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "00rqyghszjm6sp6r4b7b7lwkypiwkmkr1w02v3bjsragzml0alg0"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-variantslib" ,ocaml-variantslib)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties
      `((upstream-name . "ppx_variants_conv")))
    (home-page
      "https://github.com/janestreet/ppx_variants_conv")
    (synopsis
      "Generation of accessor and iteration functions for ocaml variant types")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-ppx-optcomp
  (package
    (name "ocaml-ppx-optcomp")
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_optcomp/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "04izdfyx6a7vhl4d2yzzkmn71paa6gss5xcjajjyk3yyl9lv4f1y"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-base" ,ocaml-base)
        ("ocaml-stdio" ,ocaml-stdio)
        ("ocaml-ppxlib" ,ocaml-ppxlib)))
    (properties `((upstream-name . "ppx_optcomp")))
    (home-page
      "https://github.com/janestreet/ppx_optcomp")
    (synopsis "Optional compilation for OCaml")
    (description
      "
Part of the Jane Street's PPX rewriters collection.
")
    (license #f)))

(define-public ocaml-fieldslib
  (package
    (name "ocaml-fieldslib")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/fieldslib-v0.14.0.tar.gz")
        (sha256
          (base32
            "10n5y376fb5jgqk9h8vq158rm1b36h9lzh6p11q33h6xgvb1v6n3"))))
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
    (version "0.14.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_fields_conv/archive/v0.14.2.tar.gz")
        (sha256
          (base32
            "0r7d51j54r1za6bwqsmhmhhfab8n10zyk5zznhkm91f20dx9ddip"))))
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

(define-public ocaml-ppx-sexp-conv
  (package
    (name "ocaml-ppx-sexp-conv")
    (version "0.14.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_sexp_conv/archive/v0.14.3.tar.gz")
        (sha256
          (base32
            "0fbnkhsd6yphc49pa21nlmbik99n7qkaz8l9paq96v012ipg9h9g"))))
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
    (version "0.14.1")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ppx_custom_printf/archive/v0.14.1.tar.gz")
        (sha256
          (base32
            "0kzbckbvhfn3s9an1hq01qd5iac7wgirw182ablpqxc6r3dmijrl"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/stdio-v0.14.0.tar.gz")
        (sha256
          (base32
            "1hj5hraprqy2i90a690l11yjszvb99j818q3d684ryx6p2lddk0l"))))
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

(define-public ocaml-compiler-libs
  (package
    (name "ocaml-compiler-libs")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/janestreet/ocaml-compiler-libs/releases/download/v0.12.3/ocaml-compiler-libs-v0.12.3.tbz")
        (sha256
          (base32
            "1jg32fjr7n4933r01iqgablshagx20vgjkjh5pdbn54w8cqkah58"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (properties
      `((upstream-name . "ocaml-compiler-libs")))
    (home-page
      "https://github.com/janestreet/ocaml-compiler-libs")
    (synopsis "OCaml compiler libraries repackaged")
    (description
      "This packages exposes the OCaml compiler libraries repackages under
the toplevel names Ocaml_common, Ocaml_bytecomp, Ocaml_optcomp, ...
")
    (license #f)))

;; newer version of this needed by bin_prot or ppx_custom_printf
(define-public ocaml-migrate-parsetree
  (package
    (name "ocaml-migrate-parsetree")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ocaml-ppx/ocaml-migrate-parsetree/releases/download/v2.1.0/ocaml-migrate-parsetree-v2.1.0.tbz")
        (sha256
          (base32
            "07x7lm45kny0mi0fjvzw51445brm0dgy099cw0gpyly0wj77hyrq"))))
    (build-system dune-build-system)
    (arguments `(#:tests? #f))
    (properties
      `((upstream-name . "ocaml-migrate-parsetree")))
    (home-page
      "https://github.com/ocaml-ppx/ocaml-migrate-parsetree")
    (synopsis
      "Convert OCaml parsetrees between different versions")
    (description
      "Convert OCaml parsetrees between different versions

This library converts parsetrees, outcometree and ast mappers between
different OCaml versions.  High-level functions help making PPX
rewriters independent of a compiler version.
")
    (license #f)))

(define-public ocaml-ppxlib
  (package
    (name "ocaml-ppxlib")
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/ocaml-ppx/ppxlib/releases/download/0.22.0/ppxlib-0.22.0.tbz")
        (sha256
          (base32
            "0ykdp55i6x1a5mbxjlvwcfvs4kvzxqnn2bi2lf224rk677h93sry"))))
    (build-system dune-build-system)
    (arguments
     `(
       ;; #:phases (modify-phases %standard-phases
       ;;   (add-before 'check 'set-topfind
       ;;     (lambda* (#:key inputs #:allow-other-keys)
       ;;       ;; add the line #directory ".." at the top of each file
       ;;       ;; using #use "topfind";; to be able to find topfind
       ;;       (let* ((findlib-path (assoc-ref inputs "findlib"))
       ;;              (findlib-libdir
       ;;               (string-append findlib-path "/lib/ocaml/site-lib")))
       ;;         (substitute* '("test/base/test.ml"
       ;;                        "test/code_path/test.ml"
       ;;                        "test/deriving/test.ml"
       ;;                        "test/driver/attributes/test.ml"
       ;;                        "test/driver/non-compressible-suffix/test.ml"
       ;;                        "test/driver/transformations/test.ml")
       ;;           (("#use \"topfind\";;" all)
       ;;            (string-append "#directory \"" findlib-libdir "\"\n"
       ;;                           all))))
       ;;       #t)))
       #:tests? #f))
    (propagated-inputs
      `(("ocaml-compiler-libs" ,ocaml-compiler-libs)
        ("ocaml-migrate-parsetree"
         ,ocaml-migrate-parsetree)
        ("ocaml-ppx-derivers" ,ocaml-ppx-derivers)
        ("ocaml-sexplib0" ,ocaml-sexplib0)
        ("ocaml-stdlib-shims" ,ocaml-stdlib-shims)
        ("ocaml-odoc" ,ocaml-odoc)))
    (native-inputs
      `(("ocaml-findlib" ,ocaml-findlib)
        ("ocaml-re" ,ocaml-re)
        ("ocaml-cinaps" ,ocaml-cinaps)
        ("ocaml-base" ,ocaml-base)
        ("ocaml-stdio" ,ocaml-stdio)))
    (home-page "https://github.com/ocaml-ppx/ppxlib")
    (synopsis "Standard library for ppx rewriters")
    (description
      "Ppxlib is the standard library for ppx rewriters and other programs
that manipulate the in-memory reprensation of OCaml programs, a.k.a
the \"Parsetree\".

It also comes bundled with two ppx rewriters that are commonly used to
write tools that manipulate and/or generate Parsetree values;
`ppxlib.metaquot` which allows to construct Parsetree values using the
OCaml syntax directly and `ppxlib.traverse` which provides various
ways of automatically traversing values of a given type, in particular
allowing to inject a complex structured value into generated code.
")
    (license #f)))

(define-public ocaml-ppx-compare
  (package
    (name "ocaml-ppx-compare")
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/ppx_compare-v0.14.0.tar.gz")
        (sha256
          (base32
            "0mqxa2s194nif7x4fjn1p5gd9i3bakr8nv27gf8x1g5nmi8q9pmp"))))
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
    (version "0.14.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://ocaml.janestreet.com/ocaml-core/v0.14/files/bin_prot-v0.14.0.tar.gz")
        (sha256
          (base32
            "1f1ng6cixi3ci0nb765yfzqk9b3s752hy1i3702kh59gni1psycp"))))
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


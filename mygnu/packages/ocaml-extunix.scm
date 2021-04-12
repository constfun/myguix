(define-module (mygnu packages ocaml-extunix)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix build-system gnu)
	       #:use-module (guix build-system ocaml)
	       #:use-module (guix build-system dune)
	       #:use-module (guix licenses)
	       #:use-module (gnu packages)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages ocaml)
	       #:use-module (gnu packages gawk))
    
(define-public ocaml4.07-migrate-parsetree
  (package
    (name "ocaml4.07-migrate-parsetree")
    (version "2.1.0")
    (source
     (origin
      (method url-fetch)
      (uri "https://github.com/ocaml-ppx/ocaml-migrate-parsetree/releases/download/v2.1.0/ocaml-migrate-parsetree-v2.1.0.tbz")
      (sha256
       (base32
        "07x7lm45kny0mi0fjvzw51445brm0dgy099cw0gpyly0wj77hyrq"))))
    (build-system dune-build-system)
    (arguments
     `(#:tests? #f))
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
    (license #f)) )

(define-public ocaml4.07-topkg
  (package
    (inherit ocaml-topkg)
    (name "ocaml4.07-topkg")
    (native-inputs
     `(("opam" ,(package-with-ocaml4.07 opam))
       ("ocamlbuild" ,(package-with-ocaml4.07 ocamlbuild)))
     )
    (propagated-inputs `(("result" ,(package-with-ocaml4.07 ocaml-result))))
    (arguments
     `(
       ,@(package-arguments ocaml-topkg)
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib	       
       ))
    ))

(define-public ocaml4.07-qrc
  (package
   (name "ocaml4.07-qrc")
   (version "0.1.0")
   (source
    (origin
     (method url-fetch)
     (uri "https://erratique.ch/software/qrc/releases/qrc-0.1.0.tbz")
     (sha256
      (base32
       "1xrgxya20rvab3jx5v0l4brp1iy664wjidzrqib6qkg3g6wlc22v"))))
   (build-system ocaml-build-system)
   (arguments
    `(
      #:ocaml ,ocaml-4.07
      #:findlib ,ocaml4.07-findlib
      #:tests? #f
      #:build-flags '("build" "--with-cmdliner" "false")
      #:phases (modify-phases %standard-phases (delete 'configure))
      ))
   (native-inputs
    `(("ocamlbuild" ,(package-with-ocaml4.07  ocamlbuild ) )
      ("ocaml-topkg" ,ocaml4.07-topkg)
      ("opam" ,(package-with-ocaml4.07 opam))))
   (home-page "https://erratique.ch/software/qrc")
   (synopsis "QR code encoder for OCaml")
   (description
    "
Qrc encodes your data into QR codes. It has built-in QR matrix
renderers for SVG, ANSI terminal and text.

Qrc is distributed under the ISC license. It has no dependencies.
")
   (license #f)) )

(define-public ocaml4.07-menhir
  (package
    (inherit ocaml-menhir)
    (name "ocaml4.07-menhir")
    (inputs
     `(("ocaml" ,ocaml-4.07)))
    (native-inputs
     `(
       ("ocamlbuild" ,(package-with-ocaml4.07  ocamlbuild ) )
       ))
    (arguments
     `(
       ,@(package-arguments ocaml-menhir)
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       )
     )))

(define-public ocaml4.07-extunix
  (package
    (name "ocaml4.07-extunix")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://ygrek.org/p/release/ocaml-extunix/ocaml-extunix-0.2.0.tar.gz")
       (patches 
	(parameterize
	    ((%patch-path
	      (map (lambda (directory)
		     (string-append directory "/gnu/packages/patches"))
		   %load-path)))

	  (search-patches "ocaml-extunix-do-not-assume-ls-bin-path.patch")))

       (sha256
	(base32
	 "1lgak2zrbi754rxysmgy845bs1y9vhnrdmmv08p7b4k25bdnr88n"))))
    (build-system ocaml-build-system)
    ;;(propagated-inputs
    ;; `(("ocaml-base-bytes" ,ocaml-base-bytes)
    ;;   ("ocaml-base-bigarray" ,ocaml-base-bigarray)
    ;;   ("ocaml-base-unix" ,ocaml-base-unix)))
    (arguments
     `(
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       ))
    (native-inputs
     `(("which" ,which)
       ("ocaml-findlib" ,ocaml4.07-findlib)
       ("ocaml-migrate-parsetree" ,(package-with-ocaml4.07 ocaml-migrate-parsetree) )
       ("ocaml-ounit" ,(package-with-ocaml4.07 ocaml-ounit))
       ("ocamlbuild" ,(package-with-ocaml4.07  ocamlbuild ) )))
    (home-page "https://ygrek.org/p/ocaml-extunix")
    (synopsis
     "Collection of thin bindings to various low-level system API")
    (description
     "Motto: \"Be to Unix, what extlib is to stdlib\"

		   * Implement thin C bindings that directly map to underlying system API.
		   * Provide common consistent ocaml interface: naming convention, exceptions.
		   * Simple to build - no extra dependencies.")
    (license #f)))

(define-public ocaml4.07-re2
  (package
    (name "ocaml4.07-re2")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://ocaml.janestreet.com/ocaml-core/v0.11/files/re2-v0.11.0.tar.gz")
       (sha256
	(base32
	 "13d72a989pxjnxca82ps05sipjvhg70xp86j1jr6v5k00fxjibd3"))))
    (build-system dune-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           ;; make warnings non fatal (jbuilder behaviour)
           (lambda _
             (invoke "dune" "build" "@install" "--profile=release"))))
       #:ocaml ,ocaml-4.07
       #:findlib ,ocaml4.07-findlib
       #:dune ,ocaml4.07-dune
       #:tests? #f
       ))
    (propagated-inputs
     `(("ocaml-core-kernel" ,ocaml4.07-core-kernel)
       ("ocaml-ppx-jane" ,ocaml4.07-ppx-jane)))
						;(native-inputs `(("g++" ,g++)))
    (home-page "https://github.com/janestreet/re2")
    (synopsis
     "OCaml bindings for RE2, Google's regular expression library")
    (description "
			      ")
    (license #f))
  )

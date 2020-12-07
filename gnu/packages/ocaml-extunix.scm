(define-module (gnu packages ocaml-extunix)
	       #:use-module (guix packages)
	       #:use-module (guix download)
	       #:use-module (guix build-system gnu)
	       #:use-module (guix build-system ocaml)
	       #:use-module (guix licenses)
	       #:use-module (gnu packages)
	       #:use-module (gnu packages base)
	       #:use-module (gnu packages ocaml)
	       #:use-module (gnu packages gawk))

(define-public ocaml-extunix
	       (package
		 (name "ocaml-extunix")
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
		 (native-inputs
		   `(("which" ,which)
		     ("ocaml-findlib" ,ocaml-findlib)
		     ("ocaml-migrate-parsetree"
		      ,ocaml-migrate-parsetree)
		     ("ocaml-ounit" ,ocaml-ounit)
		     ("ocamlbuild" ,ocamlbuild)))
		 (home-page "https://ygrek.org/p/ocaml-extunix")
		 (synopsis
		   "Collection of thin bindings to various low-level system API")
		 (description
		   "Motto: \"Be to Unix, what extlib is to stdlib\"

		   * Implement thin C bindings that directly map to underlying system API.
		   * Provide common consistent ocaml interface: naming convention, exceptions.
		   * Simple to build - no extra dependencies.")
		   (license #f)))

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
  #:use-module (gnu packages version-control)
  #:use-module (mygnu packages ocaml-extunix)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gawk))

(define-public secrets
  (package
    (name "secrets")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/constfun/secrets/archive/f620d2c5a0723e17a4b0239bfdd081a8ff8c16d6.zip")
       (sha256
	(base32
         "0v2h7ha6iliaapyrpl73rdr2r6ngiv86v32dmbjhb77my62bf3ab"))))
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


    

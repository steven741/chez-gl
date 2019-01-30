#! /usr/bin/scheme --script

;;; Compile Options

(compile-imported-libraries            #t)
(optimize-level                         3)
(debug-level                            0)
(generate-interrupt-trap               #f)
(compile-interpret-simple              #f)
(generate-inspector-information        #f)
(generate-procedure-source-information #f)
(enable-cross-library-optimization     #t)
(generate-wpo-files                    #t)
(compile-compressed                    #t)
(commonization-level                    0)

(commonization-level 0)
(compile-program "boot-simple.ss")
(compile-whole-program "boot-simple.wpo" "boot-simple.cwp")
(make-boot-file "simple.boot" '("scheme" "petite") "boot-simple.cwp")

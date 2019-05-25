#! /usr/bin/scheme --script

;;; Compile Options

(compile-imported-libraries            #t)
(optimize-level                         2)
(debug-level                            0)
(generate-interrupt-trap               #f)
(compile-interpret-simple              #f)
(generate-inspector-information        #f)
(generate-procedure-source-information #f)
(enable-cross-library-optimization     #t)
(generate-wpo-files                    #t)
(compile-compressed                    #t)
(commonization-level                    0)

(compile-program "boot-gl3.ss")
(compile-whole-program "boot-gl3.wpo" "boot-gl3.cwp")
(make-boot-file "gl3.boot" '("scheme" "petite") "boot-gl3.cwp")

(delete-file "boot-gl3.so")
(delete-file "boot-gl3.wpo")
(delete-file "boot-gl3.cwp")

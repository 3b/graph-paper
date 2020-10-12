(asdf:defsystem :graph-paper
  :description "graph paper"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (pngload
               alexandria sb-cga cl-opengl
               3b-glim/example/s 3b-glim/2d 3bgl-shader binpack)
  :serial t
  :components ((:file "shaders")
               (:file "package")
               (:file "graph-paper")))

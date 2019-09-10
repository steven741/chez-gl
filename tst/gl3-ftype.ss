;;;; -*- mode: Scheme; -*-

(import (chezscheme)
	(gl)
	(gl ftype)
	(sdl))



(define vertex-shader-source
"#version 330 core

layout (location = 0) in vec3 pos;

void main()
{
  gl_Position = vec4(pos.x, pos.y, pos.z, 1.0);
}")


(define fragment-shader-source
"#version 330 core

out vec4 frag;

void main()
{
    frag = vec4(1.0f, 1.0f, 1.0f, 1.0f);
}")



(define gl-vao (foreign-alloc (foreign-sizeof 'unsigned)))
(define gl-vbo (foreign-alloc (foreign-sizeof 'unsigned)))
(define triangle-data (foreign-alloc (* 9 (foreign-sizeof 'float))))



(define (print-error shader)
  (define info-log (foreign-alloc (* 512 (foreign-sizeof 'char))))
  (define (print-log offset)
    (define c (foreign-ref 'char info-log offset))
    (if (or (= offset 512)
	    (char=? c #\nul))
	0
	(begin
	  (printf "~c" c)
	  (print-char (+ offset 1)))))
  (glGetShaderInfoLog shader 512
		      (make-ftype-pointer int 0)
		      (make-ftype-pointer char info-log))
  (print-log 0)
  (foreign-free info-log))



(define (create-shader-program)
  (define gl-vertex-shader   (gl-create-shader GL-VERTEX-SHADER))
  (define gl-fragment-shader (gl-create-shader GL-FRAGMENT-SHADER))
  (define gl-program-shader  (gl-create-program))

  (gl-shader-source gl-vertex-shader vertex-shader-source)
  (gl-compile-shader gl-vertex-shader)
  (gl-shader-source gl-fragment-shader fragment-shader-source)
  (gl-compile-shader gl-fragment-shader)

  #|
  (glGetShaderiv gl-fragment-shader GL_COMPILE_STATUS (make-ftype-pointer int gl-vao))
  (pretty-print (foreign-ref 'unsigned gl-vao 0))
  (print-error gl-fragment-shader)
  |#

  (glAttachShader gl-program-shader gl-vertex-shader)
  (glAttachShader gl-program-shader gl-fragment-shader)
  (glLinkProgram gl-program-shader)
  (glDeleteShader gl-vertex-shader)
  (glDeleteShader gl-fragment-shader)
  (glUseProgram gl-program-shader)
  gl-program-shader)


(define (app-init)
  (sdl-init SDL-INIT-VIDEO
	    SDL-INIT-EVENTS)

  (sdl-gl-set-attribute! SDL-GL-RED-SIZE   8)
  (sdl-gl-set-attribute! SDL-GL-GREEN-SIZE 8)
  (sdl-gl-set-attribute! SDL-GL-BLUE-SIZE  8)
  (sdl-gl-set-attribute! SDL-GL-ALPHA-SIZE 8)

  (sdl-gl-set-attribute! SDL-GL-DOUBLEBUFFER 1)

  (sdl-gl-set-attribute! SDL-GL-CONTEXT-PROFILE-MASK
			 SDL-GL-CONTEXT-PROFILE-CORE)
  (sdl-gl-set-attribute! SDL-GL-CONTEXT-MAJOR-VERSION 3)
  (sdl-gl-set-attribute! SDL-GL-CONTEXT-MINOR-VERSION 3)

  (foreign-set! 'float triangle-data  0 -0.5)
  (foreign-set! 'float triangle-data  4 -0.5)
  (foreign-set! 'float triangle-data  8  0.0)

  (foreign-set! 'float triangle-data 12  0.5)
  (foreign-set! 'float triangle-data 16 -0.5)
  (foreign-set! 'float triangle-data 20  0.0)

  (foreign-set! 'float triangle-data 24  0.0)
  (foreign-set! 'float triangle-data 28  0.5)
  (foreign-set! 'float triangle-data 32  0.0))



(define (app-main)
  (define *window*
    (sdl-create-window "chezscheme"
		       SDL-WINDOWPOS-UNDEFINED
		       SDL-WINDOWPOS-UNDEFINED
		       640
		       480
		       SDL-WINDOW-OPENGL))


  (define *gl-context*
    (sdl-gl-create-context *window*))


  (define (main-loop)
    (define (should-run?)
      (sdl-poll-event)
      (cond ((sdl-event-none?) #t)
	    ((sdl-event-quit?) #f)
	    (else
	     (should-run?))))

    (define (loop-with-time proc)
      (define (loop t)
	(let ((start-time (sdl-get-performance-counter)))
	  (proc t)
	  (if (should-run?)
	      (let* ((end-time   (sdl-get-performance-counter))
		     (delta-time (/ (- end-time start-time)
				    (sdl-get-performance-frequency))))
		(loop (+ t delta-time))))))
      (loop 0.0))

    ;; Try to use adaptive vsync.
    ;; Fallback to regular vsync if unavailable.
    (if (= (sdl-gl-set-swap-interval! -1) -1)
	(sdl-gl-set-swap-interval! 1))

    ;; Load OpenGL and set the initial state.
    (gl-load-library)
    (glClearColor 1.0 1.0 1.0 1.0)
    (glViewport 0 0 640 480)

    ;; Create a shader program
    (let ((gl-shader-program (create-shader-program)))

      ;; Create a VAO and a VBO
      (glGenVertexArrays 1 (make-ftype-pointer unsigned gl-vao))
      (glGenBuffers 1 (make-ftype-pointer unsigned gl-vbo))

      (glBindVertexArray (foreign-ref 'unsigned gl-vao 0))
      (glBindBuffer GL_ARRAY_BUFFER (foreign-ref 'unsigned gl-vbo 0))
      (glBufferData GL_ARRAY_BUFFER (* 9 (foreign-sizeof 'float)) triangle-data GL_STATIC_DRAW)

      (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (* 3 (foreign-sizeof 'float)) 0)
      (glEnableVertexAttribArray 0)

      (loop-with-time
       (lambda (t)
	 (glClearColor (sin t) 0.5 (cos t) 1.0)
	 (glClear GL_COLOR_BUFFER_BIT)
	 (glDrawArrays GL_TRIANGLES 0 3)
	 (sdl-gl-swap-window *window*)))

      (glDeleteProgram gl-shader-program)))


  (main-loop)

  (glDeleteVertexArrays 1 (make-ftype-pointer unsigned gl-vao))
  (glDeleteBuffers 1 (make-ftype-pointer unsigned gl-vbo))

  (sdl-gl-delete-context *gl-context*)
  (sdl-destroy-window *window*)
  (sdl-quit))



(define (app-quit)
  (foreign-free gl-vao)
  (foreign-free gl-vbo)
  (foreign-free triangle-data))



(app-init)
(app-main)
(app-quit)

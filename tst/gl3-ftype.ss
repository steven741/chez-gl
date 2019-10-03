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


(define (create-shader-program)
  (define gl-vertex-shader   (gl-create-shader GL-VERTEX-SHADER))
  (define gl-fragment-shader (gl-create-shader GL-FRAGMENT-SHADER))
  (define gl-program-shader  (gl-create-program))

  (gl-shader-source gl-vertex-shader vertex-shader-source)
  (gl-compile-shader gl-vertex-shader)

  (gl-shader-source gl-fragment-shader fragment-shader-source)
  (gl-compile-shader gl-fragment-shader)

  (gl-attach-shader gl-program-shader gl-vertex-shader)
  (gl-attach-shader gl-program-shader gl-fragment-shader)
  (gl-link-program gl-program-shader)
  (gl-delete-shader gl-vertex-shader)
  (gl-delete-shader gl-fragment-shader)
  (gl-use-program gl-program-shader)
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
  (sdl-gl-set-attribute! SDL-GL-CONTEXT-MINOR-VERSION 3))



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
    (let ((gl-shader-program (create-shader-program))
	  (gl-vbo (gl-gen-buffers 1))
	  (gl-vao (gl-gen-vertex-arrays 1)))

      (gl-bind-vertex-array gl-vao)
      (gl-bind-buffer GL-ARRAY-BUFFER gl-vbo)
      (gl-buffer-data GL-ARRAY-BUFFER '(-0.5 -0.5 0.0 0.5 -0.5 0.0 0.0 0.5 0.0) GL-STATIC-DRAW)

      (gl-vertex-attrib-pointer 0 3 GL-FLOAT GL-FALSE (* 3 4) 0)
      (gl-enable-vertex-attrib-array 0)

      (loop-with-time
       (lambda (t)
	 (gl-clear-color (sin t) 0.5 (cos t) 1.0)
	 (gl-clear GL-COLOR-BUFFER-BIT)
	 (gl-draw-arrays GL-TRIANGLES 0 3)
	 (sdl-gl-swap-window *window*)))

      (gl-delete-program gl-shader-program)
      (gl-delete-vertex-arrays gl-vao)
      (gl-delete-buffers gl-vbo)))


  (main-loop)
  (sdl-gl-delete-context *gl-context*)
  (sdl-destroy-window *window*)
  (sdl-quit))



(app-init)
(app-main)

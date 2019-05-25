;;;; -*- mode: Scheme; -*-

(import (gl ftype)
	(sdl))



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

    (define (loop t)
      (define start-time (sdl-get-performance-counter))
 
      (glClearColor (sin t) 0.5 (cos t) 1.0)
      (glClear GL_COLOR_BUFFER_BIT)
      (sdl-gl-swap-window *window*)

      (let* ((end-time   (sdl-get-performance-counter))
	     (delta-time (/ (- end-time start-time)
			    (sdl-get-performance-frequency))))
	(if (should-run?)
	    (loop (+ t delta-time)))))

    ;; Try to use adaptive vsync.
    ;; Fallback to regular vsync if unavailable.
    (if (= (sdl-gl-set-swap-interval! -1) -1)
	(sdl-gl-set-swap-interval! 1))

    ;; Load OpenGL and set the initial state.
    (gl-load-library)
    (glClearColor 1.0 1.0 1.0 1.0)
    (glViewport 0 0 640 480)

    (loop 0.0))

  (main-loop)
  (sdl-gl-delete-context *gl-context*)
  (sdl-destroy-window *window*)
  (sdl-quit))

(app-init)
(app-main)

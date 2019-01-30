;;;; -*- mode: Scheme; -*-

(import (gl)
	(sdl))


(define (app-init)
  (sdl-set-main-ready!)

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

  (define (should-quit?)
    (sdl-poll-event)
    (cond
     ((sdl-event-none?) #f)
     ((sdl-event-quit?) #t)
     (else
      (should-quit?))))

  (define (main-loop)
    (gl-clear GL-COLOR-BUFFER-BIT)
    (sdl-gl-swap-window *window*)
    (if (should-quit?)
	'()
	(main-loop)))
 
  ;; Try to use adaptive vsync.
  ;; Fallback to regular vsync if unavailable.
  (if (= (sdl-gl-set-swap-interval! -1) -1)
      (sdl-gl-set-swap-interval! 1))

  ;; Load OpenGL and set the initial state.
  (gl-load-library)
  (gl-clear-color (/ 100.0 255.0)
		  (/  50.0 255.0)
		  (/  20.0 255.0)
		  1.0)
  (gl-viewport 0 0 640 480)

  (main-loop)
  (sdl-gl-delete-context *gl-context*)
  (sdl-destroy-window *window*)
  (sdl-quit))

(app-init)
(app-main)

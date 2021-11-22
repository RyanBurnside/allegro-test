;;;; allegro-test.lisp

(in-package #:allegro-test)

(defparameter *canvas* nil)
(defparameter *canvas-width* 640)
(defparameter *canvas-height* 480)
(defparameter *brush-width* 16)
(defparameter *brush-height* 16)
(defparameter *back-buffer* nil)
(defparameter *brush-bitmap* nil)


(defun half (number)
  (* number .5))


(defun setup (width height)
  "Create initial objects and set values for testing"
  (al:init)
  (al:init-primitives-addon)
  (al:set-new-display-flags '(:windowed :resizable :opengl))
  (al:set-new-display-option :vsync 0 :require)
  (al:install-keyboard)
  (al:install-mouse)
  ;; Create the display and load the input drivers
  (setf *back-buffer* (al:create-display width height))
  (setf *canvas* (al:create-bitmap width height))
  (setf *brush-bitmap* (al:create-bitmap *brush-width* *brush-height*))
  (al:set-target-bitmap *brush-bitmap*)
  (al:clear-to-color (al:map-rgba 0 0 0 0))
  (al:draw-filled-circle (/ *brush-width* 2)
		  (/ *brush-height* 2)
		  (/ (min *brush-width* *brush-height*) 2)
		  (HSL->RGB 200 1.0 .60)))


(defun get-mouse-data  ()
  "Returns a plist with mouse data"
  (al:with-current-mouse-state state
    ;; The mouse state can be exploded into a plist
    (let ((plist (cffi:mem-ref state '(:struct al:mouse-state))))
      plist)))


(defun main ()
  (setup *canvas-width* *canvas-height*)
  ;; This macro will initialize STATE by calling
  ;; (al:get-keyboard-state STATE) for you before the body code.  Use
  ;; WITH-KEYBOARD-STATE to leave uninitialized.

  (al:with-current-keyboard-state state
    (let ((mouse-last-x 0)
          (mouse-last-y 0))

      (do () ((al:key-down state :escape))
        (al:set-target-backbuffer *back-buffer*)
        (al:clear-to-color (al:map-rgb 0 0 0))

        (let* ((md (get-mouse-data))
               (m-x (getf md 'al:x))
               (m-y (getf md 'al:y))
               (btn-1 (= (getf md 'al:buttons) 1))
               (sprite-x (- m-x (half *brush-width*)))
               (sprite-y (- m-y (half *brush-height*))))
          (when btn-1
            ;; Stress test
            (al:set-target-bitmap *canvas*)

            (with-positions-between
                ((- mouse-last-x (half *brush-width*))
                 (- mouse-last-y (half *brush-height*))
                 sprite-x
                 sprite-y
                 pt-x
                 pt-y)
              (- pt-x (half *brush-width*))
              (- pt-y (half *brush-height*))
              (al:draw-bitmap *brush-bitmap*
                              pt-x
                              pt-y
                              nil)))
	  
          (setf mouse-last-x m-x)
          (setf mouse-last-y m-y)

          (al:set-target-backbuffer *back-buffer*)
          (al:draw-bitmap *canvas* 0 0 nil)
          (al:draw-bitmap *brush-bitmap*
                          (- m-x (half *brush-width*))
                          (- m-y (half *brush-height*))
                          nil)
          (al:flip-display))
        (al:get-keyboard-state state))
      ;; All done let's cleanup
      (al:destroy-display *back-buffer*)
      (al:uninstall-system))))

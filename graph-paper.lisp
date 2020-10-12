(in-package graph-paper)

(defvar *format*
  (glim:compile-vertex-format
   '(1
     (0 :vec4) ;; position
     (1 :vec3) ;; normal
     (2 :vec4) ;; uv
     (3 :vec4) ;; color
     )))

(declaim (inline vertex vertex-v color normal uv))
(defun vertex (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 0 x y z w))
(defun vertex-v (v)
  (glim:attrib-fv 0 v))
(defun normal (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 1 x y z w))
(defun uv (x &optional (y 0.0) (z 0.0) (w 1.0))
  (glim:attrib-f 2 x y z w))
(defun color (r g b &optional (a 1.0))
  (glim:attrib-f 3 r g b a))

(defparameter *esc* nil)

(defparameter *debug* 0)
(defparameter *flags* (make-array 10 :initial-element nil))
(defparameter *kx* 0)
(defparameter *ky* 0)
(defparameter *mouse* t)

(Defparameter *undo* nil)
#++(push (shapes *w*) *undo*)

(defclass graph-paper (scratchpad)
  ((scale :initform 1 :accessor scale)
   (spacing :initform 16 :accessor spacing)
   (origin :initform :center :accessor origin)
   (shapes :initform nil :accessor shapes)
   (x1 :initform 0 :accessor x1)
   (x2 :initform 0 :accessor x2)
   (y1 :initform 0 :accessor y1)
   (y2 :initform 0 :accessor y2)
   (gmx :initform 0 :accessor gmx)
   (gmy :initform 0 :accessor gmy)
   (snap :initform t :accessor snap)
   (show-colors :initform nil :accessor show-colors)
   (color-picker-w :initform 22 :accessor color-picker-w)
   (color-picker-bits :initform 3 :accessor color-picker-bits)
   (color-picker-col :initform '(1 0 0 1) :accessor color-picker-col)
   (edit-point :initform nil :accessor edit-point)
   (editing-point :initform nil :accessor editing-point)
   (edit-point-prev :initform nil :accessor edit-point-prev)
   (tool :initform 'polyline :accessor tool))

  (:default-initargs :shaders '((:tex :vertex vertex :fragment fragment)
                                (:solid :vertex vertex :fragment solid))))


(defun save-undo (w)
  (push (shapes w) *undo*))

(defvar *w* nil)
#++
(setf (color-picker-w *w*) 12
      (color-picker-bits *w*) 4)
#++
(setf (color-picker-w *w*) 4
      (color-picker-bits *w*) 5)
#++
(setf (color-picker-w *w*) 16
      (color-picker-bits *w*) 3
)

(defclass point ()
  ((x :initform 0 :accessor x :initarg :x)
   (y :initform 0 :accessor y :initarg :y)))

(defclass shape ()
  ((rgba :initform '(1 0 0 1) :accessor rgba :initarg :rgba)
   (vertex-rgba :initform '(1 0.5 1 1) :accessor vertex-rgba)
   (drawing :initform t :accessor drawing)))

(defclass polyline (shape)
  ((points :initform (make-array 0 :adjustable t :fill-pointer 0)
           :reader points)
   (closed :initform nil :accessor closed)
))

(defun move-polyline (pl dx dy)
  (loop for p across (points pl)
        do (incf (x p) dx)
           (incf (y p) dy)))

(defparameter *v* 0)
(incf *v*)
(defmethod draw-shape ((w graph-paper) (s polyline))
  (glim:with-draw (:lines :shader :solid)
    (apply #'color (rgba s))
    (loop for prev = nil then p
          for p across (points s)
          when prev
            do (vertex (x prev) (y prev))
               (vertex (x p) (y p))
          finally (when (and p (closed s))
                    (let ((e (aref (points s) 0)))
                      (vertex (x p) (y p))
                      (vertex (x e) (y e))))
                  (when (and p (drawing s))
                    (color 1 1 1 1)
                    (vertex (x p) (y p))
                    (vertex (gmx w) (gmy w)))))
  (when (vertex-rgba s)
    (gl:point-size 3)
    (glim:with-draw (:points :shader :solid)
      (apply #'color (vertex-rgba s))
      (loop for p across (points s)
            for i from 0
            ;;when (= i *v*)
            ;;  do (apply #'color (rnd-rgb))
            ;;else do (apply #'color (vertex-rgba s))
            do (vertex (x p) (y p)))))
  (dispatch-draws w ))

(defmethod click-shape ((w graph-paper) (s null) x y)
  (push (make-instance (tool w) :rgba (color-picker-col w)) (shapes w))
  (assert (not (closed (car (shapes w)))))
  (click-shape w (car (shapes w)) x y))

(defmethod click-shape ((w graph-paper) (s polyline) x y)
  (cond
    ((drawing s)
     (if (and (plusp (length (points s)))
              (= x (x (aref (points s) 0)))
              (= y (y (aref (points s) 0))))
         (progn
           (setf (closed s) t)
           (setf (drawing s) nil))
         (vector-push-extend (make-instance 'point :x x :y y)
                             (points s))))
    (t (click-shape w nil x y))))

(defmethod print-shape ((s polyline))
  (format t "polyline: ~s points, :closed ~s~%" (length (points s))
          (closed s))
  (loop for p across (points s)
        do (format t " ~s ~s~%" (x p) (y p))))
(defmethod print-shape2 ((s polyline))
  (format t "~s~%"
   (loop for p across (points s)
         collect (/ (x p) 16)
         
         collect (/ (y p) 16))))

(defun translate-mouse (w x y &key snap)
  (let ((mx (+ (x1 w)
               (/ x (/ (wx w)
                       (- (x2 w) (x1 w))))))
        (my (+ (y1 w)
               (/ (- (wy w) y)
                  (/ (wy w)
                     (- (y2 w) (y1 w)))))))
    (if (and snap (snap w))
        (let ((s (spacing w)))
          (values (* s (round mx s))
                  (* s (round my s))))
        (values mx my))))
#++(elt (shapes *w*) 14)
#++(setf (slot-value (elt (shapes *w*) 14)'points)
      (reverse (points (elt (shapes *w*) 14))))
(defun uniforms ()
  (glim:uniform 'mv (glim:ensure-matrix :modelview))
  (glim:uniform 'mvp (sb-cga:matrix*
                      (glim:ensure-matrix :projection)
                      (glim:ensure-matrix :modelview)))
  (glim:uniform 'normal-matrix (glim:ensure-matrix :modelview)))

(defmethod display ((w graph-paper) now)
  (setf *w* w)
  (glim:with-state (*format*)
    (when (editing-point w)
      (setf (x (editing-point w)) (gmx w)
            (y (editing-point w)) (gmy w)))

    (glim:uniform 'proj sb-cga:+identity-matrix+)
    (glim:matrix-mode :projection)
    (glim:load-identity)
    (glim:matrix-mode :modelview)
    (glim:load-identity)

    (gl:enable :depth-test :sample-alpha-to-coverage
               :polygon-smooth :line-smooth
               :blend :multisample :texture-2d )

    (gl:blend-func :src-alpha :one-minus-src-alpha)

    (gl:disable :cull-face :lighting :light0 :light1
                :depth-test :sample-alpha-to-coverage
                )
    (glim:uniform 'debug1 *debug* )
    (glim:uniform 'tex0 0)
    (glim:uniform 'tex1 1)
    (glim:uniform 'flip 0)

    (let* ((x1 0)
           (y1 0)
           (scale (scale w))
           (step (spacing w))
           (x2 (/ (wx w) scale))
           (y2 (/ (wy w) scale)))
      (glim:with-pushed-matrix (:modelview)
        (ecase (origin w)
          (:center
           (setf x2 (/ (wx w) scale))
           (setf y2 (/ (wy w) scale))
           (setf x1 (- x2))
           (setf y1 (- y2)))
          (:lower-left
           (glim:translate -1 -1 0))
          ;; todo: add others
          )
        (glim:scale (/ (wx w)) (/ (wy w)) 1)
        (glim:translate 0.5 0.5 0)
        (glim:scale scale scale 1)
        (uniforms)
        (setf (x1 w) x1)
        (setf (x2 w) x2)
        (setf (y1 w) y1)
        (setf (y2 w) y2)
        (setf (values (gmx w) (gmy w))
              (translate-mouse w (mx w) (my w)))
        ;(format t "x1=~s,~s, 2=~s,~s~%" x1 y1 x2 y2)
        (glim:with-draw (:lines :shader :solid)
          (color 0.1 0.1 0.2 1)
          (when (minusp y1)
            (loop for x from 0 downto x1 by step
                  do (vertex x y1)
                     (vertex x y2)))
          (loop for x from 0 upto x2 by step
                do (vertex x y1)
                   (vertex x y2))
          (when (minusp y1)
            (loop for y from 0 downto y1 by step
                  do (vertex x1 y)
                     (vertex x2 y)))
          (loop for y from 0 upto y2 by step
                do (vertex x1 y)
                   (vertex x2 y))

          (color 0.2 0.2 0.4 1)
          (vertex x1 0)
          (vertex x2 0)
          (vertex 0 y1)
          (vertex 0 y2))
        (dispatch-draws w )
        (when (shapes w)
          (loop for s in (shapes w)
                for i from 0
                ;unless (= i 14)
                do (draw-shape w s)))
        (dispatch-draws w )))
    (when (show-colors w)
      (glim:with-pushed-matrix (:modelview)
        (glim:load-identity)
        (glim:translate -1 -1 0)
        (glim:scale (/ 2 (wx w)) (/ 2 (wy w)) 1)
        (uniforms)
        (let* ((ww (color-picker-w w))
               (bits (color-picker-bits w))
               (cols (expt 2 bits))
               (rows (* cols (floor (wy w) (* cols ww))))
)
          (assert (< bits 6))
          (glim:with-draw (:quads :shader :solid)
            (loop for div = (expt 2.0 bits)
                  for c from 0 below (expt 2 (* bits 3))
                  for x1 = (* ww (+ (mod (floor c) cols)
                                    (* cols (floor c (* rows cols)))
                                    ))
                  for y1 = (* ww (floor (mod c (* rows cols))
                                        cols))
                  for x2 = (+ x1 ww)
                  for y2 = (+ y1 ww)
                  do (color (/ (ldb (byte bits 0) c) div)
                            (/ (ldb (byte bits bits) c) div)
                            (/ (ldb (byte bits (* bits 2)) c) div)
                            1)
                     (vertex x1 y1)
                     (vertex x1 y2)
                     (vertex x2 y2)
                     (vertex x2 y1))))
        (dispatch-draws w)))
    ))


(defmethod mouse ((w graph-paper) button state x y)
  (format t "~s ~s~%" button state)
  (setf *esc* nil)
  (when (eql state :down)
    (case button
     (:left-button
      (cond
        ((editing-point w)
         (multiple-value-bind (mx my)
             (translate-mouse w x y :snap t)
           (setf (x (editing-point w)) mx
                 (y (editing-point w)) my))
         (setf (editing-point w) nil))
        ((edit-point w)
         (multiple-value-bind (mx my)
             (translate-mouse w x y)
           (let ((d most-positive-single-float)
                 (s nil)
                 (p nil))
             (loop for shape in (shapes w)
                   do (loop for point across (points shape)
                            for px = (x point)
                            for py = (y point)
                            for pd = (+ (expt (- px mx) 2)
                                        (expt (- py my) 2))
                            when (< pd d)
                              do (setf s shape)
                                 (setf d pd)
                                 (setf p point)))
             (format t "edit point ~s~%" p)
             (when p
               (setf (editing-point w) p
                     (edit-point-prev w)
                     (cons (x p) (y p)))))))

        ((show-colors w)
         (let* ((y (- (wy w) y))
                (ww (color-picker-w w))
                (bits (color-picker-bits w))
                (div (expt 2.0 bits))
                (cols (expt 2 bits))
                (rows (* cols (floor (wy w) (* cols ww))))
                (cx (floor x ww))
                (cy (floor y ww))
                (r (mod cx cols))
                (g (mod cy cols))
                (b (+ (floor cy cols)
                      (* (floor rows cols)
                         (floor cx cols)) )))
           (setf (color-picker-col w)
                 (list (/ r div) (/ g div) (/ b div) 1))
           (when (shapes w)
             (setf (rgba (car (shapes w))) (color-picker-col w)))))
        (t
         (multiple-value-bind (mx my) (translate-mouse w x y :snap t)
           (click-shape w (first (shapes w)) mx my)
))))
     (:right-button
      (when (shapes w)
        (setf (closed (first (shapes w)))
              t))))))
(defun finish-shape (w &key close)
  (when (shapes w)
    (when close
      (setf (closed (first (shapes w))) t))
    (setf (drawing (first (shapes w))) nil)))

(defun rnd-rgb ()
  (let ((a (random (* 2 pi))))
    (list (1+ (sin a))
          (1+ (sin (+ a (* 1/3 pi))))
          (1+ (sin (+ a (* 2/3 pi))))
          1)))

(defun clear-shapes ()
  (when *w*
    (setf (shapes *w*) nil)))

(defun add-shape-points (points &key (close t))
  (when *w*
    (finish-shape *w*)
    (push (make-instance 'polyline) (shapes *w*))
    (loop for (x y) on points by #'cddr
          do (vector-push-extend (make-instance 'point :x x :y y)
                                 (points (first (shapes *w*)))))
    (finish-shape *w* :close close)))

(defun start-polyline (&key (rgba '(1 0 0 1)))
  (when *w*
    (finish-shape *w*)
    (push (make-instance 'polyline :rgba rgba) (shapes *w*))))

(defun add-shape-point (x y)
  (when *w*
    (vector-push-extend (make-instance 'point :x x :y y)
                        (points (first (shapes *w*))))))

(defmacro with-polyline ((&key (rgba ''(1 0 0 1))
                            (close t))
                         &body body)
  `(when *w*
     (progn
       (finish-shape *w*)
       (start-polyline :rgba ,rgba)
       ,@body
       (finish-shape *w* :close ,close))))

(defmethod mouse-wheel ((window graph-paper) button state x y)
  (format t "wheel ~s ~s~%" button state)
  (if (eql state :up)
      (setf (scale window) (* (scale window) 1.1))
      (setf (scale window) (/ (scale window) 1.1))))

(defun cancel-edit-point (w)
  (setf (edit-point w) nil)
  (when (editing-point w)
    (setf (x (editing-point w)) (car (edit-point-prev w))
          (y (editing-point w)) (cdr (edit-point-prev w))))
  (setf (editing-point w) nil))

(defmethod keyboard-up ((window graph-paper) key x y)
  (format t "key up ~s~%" key)
  (case key
    (:key-left-ctrl
     (setf (show-colors window) nil))
    (:key-left-alt
     (when (edit-point window)
       (format t "end edit point~%")
       (cancel-edit-point window)))))


(defmethod keyboard :around ((window graph-paper) key x y)
  (cond
    ((and (eql key #\q) *esc*)
     (destroy-window window))
    ((and (eql key #\d) *esc*)
       (pop (shapes window)))
    ((eql key #\esc)
     (cond
       ((and (shapes window) (drawing (car (shapes window))))
        (pop (shapes window)))
       ((edit-point window)
        (cancel-edit-point window)))
     (save-undo window)
     (setf *esc* t))
    (t
     (setf *esc* nil)
     (call-next-method))))

(defmethod keyboard ((window graph-paper) key x y)
  (declare (ignore x y))
  (print key)
  (case key
    (:key-left-ctrl (setf (show-colors window) t))
    (:key-left-alt (setf (edit-point window) t))
    (:key-right
     (setf *mouse* nil))
    (:key-left
     (setf *mouse* nil))
    (:key-down
     (setf *mouse* nil))
    (:key-up
     (setf *mouse* nil))

    ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
     (let ((i (digit-char-p key)))
       (when i
         (setf (aref *flags* i) (not (aref *flags* i)))
         (format t "flags ~s -> ~s~%" i (aref *flags* i)))))
    (#\p
     (finish-shape window)
     (setf (tool window) 'polyline)
     (push (make-instance 'polyline :rgba (color-picker-col window))
           (shapes window)))
    (#\c
     (finish-shape window :close t))
    (#\f
     (finish-shape window :close nil))
    (#\d
     (loop for s in (shapes window)
           do (print-shape2 s)))
    (#\l
     (glut:reshape-window 1024 768))
    (#\s
     (setf (snap window) (not (snap window))))
    (#\space
     (push (shapes window) *undo*)
     (setf (shapes window) nil))
    (#\m (setf *mouse* t))
   
    #++(#\d
     (unless *debug* (setf *debug* 0))
     (setf *debug* (mod (1+ *debug*) 4))
     (format t "debug = ~s~%" *debug*))
    (#\Esc
     (glut:destroy-current-window))))

(defmethod entry ((w graph-paper) state)
  (format t "mouse -> ~s~%" state)
  #++(when (eql state :left) (break "enter ~s~%" state))
  (setf *mouse* (eql state :entered)))

(defmethod init-gl ((w graph-paper))
  (setf *esc* nil)
  (gl:pixel-store :unpack-alignment 1)
  
  (gl:disable :dither))

(defun graph-paper (&rest args)
  (glut:display-window (apply #'make-instance 'graph-paper args)))

#++
(ql:quickload 'graph-paper)
#++
(graph-paper :pos-x 2440 :pos-y 140 :width 1400 :height 850)
#++
(glut:show-window)




#++
(setf (shapes *w*) (car *undo*))
#++
(setf (spacing *w*) 4)
#++
(setf (spacing *w*) 16)

#++
(loop for p in (shapes *w*)
      do (move-polyline p 0 -16))

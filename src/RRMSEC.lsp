;;; ==========================================================================
;;;  RRMSEC.lsp  -  RRM (Rubble Masonry) wall section generator
;;;                 SINGLE or MULTIPLE heights in one go
;;;  Works in AutoCAD / ZWCAD / BricsCAD / GstarCAD
;;;  Commands:  RRMSEC   (or RRMMULTI - same thing)
;;;
;;;  RRM height box accepts one value or a list:   2          or   2,2.5,3,4
;;;  (separators allowed: comma, semicolon or space)
;;;  One section is drawn for every height, placed side by side in a row
;;;  (left to right, in the order typed). All other inputs are shared.
;;;
;;;  Draws (no text, no labels, no dimensions - geometry only):
;;;     STONE_SOLING  - stone soling       (parallel to sloped bed)
;;;     PCC_BED       - PCC bed            (parallel to sloped bed)
;;;     RRM_WALL      - RRM masonry wall   (base parallel to sloped bed)
;;;     PCC_CAP       - PCC cap on top of RRM wall
;;;
;;;  Geometry (all inputs in metres), for every height H:
;;;     RRM base width  = base factor x H
;;;     Bottom width    = RRM base + 2 x cover   (PCC bed & soling width)
;;;     Bed drop        = bed slope % x bottom width  (bed falls toward front/right)
;;;     RRM top width   = PCC cap top width
;;;     Back offset     = back slope % x H (back face leans toward front)
;;;     Front face      = straight line from RRM top (front) to RRM base (front)
;;;     Insertion point = top-back corner of the PCC bed of the FIRST section.
;;;     Next section starts at  previous bottom width + gap  to the right.
;;; ==========================================================================

(vl-load-com)

;;; ---------------- default values (kept between runs) ----------------------
(setq *rrmv*
  '(("ts" . "0.15")   ; stone soling thickness
    ("tp" . "0.10")   ; PCC bed thickness
    ("cv" . "0.10")   ; cover
    ("hh" . "")       ; RRM height(s)  (blank - user must enter, e.g. 2,2.5,3)
    ("tc" . "0.05")   ; PCC cap thickness
    ("wc" . "0.50")   ; PCC cap top width
    ("sl" . "10")     ; bed slope  (% of bottom width)
    ("bf" . "0.6")    ; base factor
    ("bk" . "10")     ; back slope (% of RRM height)
    ("gp" . "2.0")    ; gap between sections (m)
    ("uf" . "1")))    ; drawing units per metre (1 = m, 1000 = mm)

(setq *rrmip* (list 0.0 0.0 0.0))

;;; layer name . colour index
(setq *rrmlay*
  '(("STONE_SOLING" . 8)
    ("PCC_BED"      . 2)
    ("RRM_WALL"     . 30)
    ("PCC_CAP"      . 4)))

;;; ---------------- helpers --------------------------------------------------
(defun rrm:num (k)
  (atof (cdr (assoc k *rrmv*))))

;; split a string at , ; or space  ->  list of non-empty strings
(defun rrm:split (s / i c cur out)
  (setq i 1 cur "" out nil)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c '("," ";" " "))
      (progn
        (if (/= cur "") (setq out (cons cur out)))
        (setq cur ""))
      (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (if (/= cur "") (setq out (cons cur out)))
  (reverse out))

;; list of heights typed by the user
(defun rrm:heights ()
  (mapcar 'atof (rrm:split (cdr (assoc "hh" *rrmv*)))))

;; parameter list for one height: ts tp cv H tc wc sl bf bk
(defun rrm:params (h)
  (list (rrm:num "ts") (rrm:num "tp") (rrm:num "cv") h
        (rrm:num "tc") (rrm:num "wc") (rrm:num "sl") (rrm:num "bf")
        (rrm:num "bk")))

(defun rrm:read ( / k)
  (foreach k '("ts" "tp" "cv" "hh" "tc" "wc" "sl" "bf" "bk" "gp" "uf")
    (setq *rrmv* (subst (cons k (get_tile k)) (assoc k *rrmv*) *rrmv*))))

;; check one section (p = params) -> error string or nil
(defun rrm:check (p / h)
  (setq h (nth 3 p))
  (cond
    ((<= h 0.0)               "Heights must be numbers greater than 0.")
    ((<= (nth 7 p) 0.0)       "Base factor must be greater than 0.")
    ((<= (nth 5 p) 0.0)       "PCC cap top width must be greater than 0.")
    ((< (apply 'min p) 0.0)   "Negative values are not allowed.")
    ((>= (+ (* (/ (nth 8 p) 100.0) h) (nth 5 p)) (* (nth 7 p) h))
     "top width + back slope is wider than RRM base (front face would overhang).")
    (T nil)))

;; check everything -> error string or nil
(defun rrm:checkall ( / hs err bad h)
  (setq hs (rrm:heights))
  (cond
    ((null hs)                "Enter RRM height(s), e.g.  2,2.5,3,4")
    ((<= (rrm:num "uf") 0.0)  "Drawing units per metre must be greater than 0.")
    ((< (rrm:num "gp") 0.0)   "Gap between sections cannot be negative.")
    (T
     (foreach h hs
       (if (null err)
         (progn
           (setq err (rrm:check (rrm:params h)))
           (if err (setq bad h)))))
     (if (and err (> (length hs) 1))
       (setq err (strcat "Height " (rtos bad 2 3) " : " err)))
     err)))

;; geometry of ONE section -> list of (layer (pt pt pt ...)), metres, local
(defun rrm:geom (p / ts tp cv h tc wc sl bf bk bb bw d
                     r0x r0y r1x r1y t0x t1x ty)
  (setq ts (nth 0 p) tp (nth 1 p) cv (nth 2 p) h  (nth 3 p)
        tc (nth 4 p) wc (nth 5 p)
        sl (/ (nth 6 p) 100.0) bf (nth 7 p) bk (/ (nth 8 p) 100.0)
        bb (* bf h)
        bw (+ bb (* 2.0 cv))
        d  (* sl bw)
        r0x cv              r0y (- (* sl r0x))
        r1x (+ cv bb)       r1y (- (* sl r1x))
        t0x (+ r0x (* bk h))
        t1x (+ t0x wc)
        ty  (+ r0y h))
  (list
    (list "STONE_SOLING"
          (list (list 0.0 (- tp))
                (list bw (- (- d) tp))
                (list bw (- (- (- d) tp) ts))
                (list 0.0 (- (- tp) ts))))
    (list "PCC_BED"
          (list (list 0.0 0.0)
                (list bw (- d))
                (list bw (- (- d) tp))
                (list 0.0 (- tp))))
    (list "RRM_WALL"
          (list (list r0x r0y)
                (list r1x r1y)
                (list t1x ty)
                (list t0x ty)))
    (list "PCC_CAP"
          (list (list t0x ty)
                (list t1x ty)
                (list t1x (+ ty tc))
                (list t0x (+ ty tc))))))

;; ALL sections in a row (metres, local coordinates, first section at 0,0)
(defun rrm:multi (hs gap / xo out p g pl)
  (setq xo 0.0 out nil)
  (foreach h hs
    (setq p (rrm:params h)
          g (rrm:geom p))
    (foreach pl g
      (setq out
        (cons (list (car pl)
                    (mapcar '(lambda (q) (list (+ xo (car q)) (cadr q)))
                            (cadr pl)))
              out)))
    (setq xo (+ xo (* (nth 7 p) h) (* 2.0 (nth 2 p)) gap)))
  (reverse out))

;;; ---------------- drawing --------------------------------------------------
(defun rrm:mklayer (nm col)
  (if (not (tblsearch "LAYER" nm))
    (entmake (list '(0 . "LAYER")
                   '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 nm)
                   '(70 . 0)
                   (cons 62 col)
                   '(6 . "Continuous")))))

(defun rrm:pline (lay pts)
  (entmake
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            (cons 8 lay)
            '(100 . "AcDbPolyline")
            (cons 90 (length pts))
            '(70 . 1))
      (mapcar '(lambda (q) (list 10 (car q) (cadr q))) pts))))

(defun rrm:draw (hs gap uf ip / g pl pts allp minx maxx miny maxy mx my)
  (foreach l *rrmlay* (rrm:mklayer (car l) (cdr l)))
  (setq g (rrm:multi hs gap) allp nil)
  (foreach pl g
    (setq pts (mapcar '(lambda (q)
                         (list (+ (car ip)  (* uf (car q)))
                               (+ (cadr ip) (* uf (cadr q)))))
                      (cadr pl)))
    (setq allp (append allp pts))
    (rrm:pline (car pl) pts))
  ;; zoom to all new sections
  (setq minx (apply 'min (mapcar 'car allp))
        maxx (apply 'max (mapcar 'car allp))
        miny (apply 'min (mapcar 'cadr allp))
        maxy (apply 'max (mapcar 'cadr allp))
        mx   (* 0.15 (- maxx minx))
        my   (* 0.15 (- maxy miny)))
  (command "_.ZOOM" "_W"
           (list (- minx mx) (- miny my))
           (list (+ maxx mx) (+ maxy my))))

;;; ---------------- dialog : preview ----------------------------------------
(defun rrm:edges (px)
  (mapcar 'list px (append (cdr px) (list (car px)))))

(defun rrm:fillpoly (px col / ymin ymax y yc xs e a b)
  (setq ymin (fix (apply 'min (mapcar 'cadr px)))
        ymax (fix (apply 'max (mapcar 'cadr px)))
        y ymin)
  (while (<= y ymax)
    (setq xs nil yc (+ y 0.5))
    (foreach e (rrm:edges px)
      (setq a (car e) b (cadr e))
      (if (and (/= (cadr a) (cadr b))
               (<= (min (cadr a) (cadr b)) yc)
               (<  yc (max (cadr a) (cadr b))))
        (setq xs (cons (+ (car a)
                          (/ (* (- yc (cadr a)) (- (car b) (car a)))
                             (- (cadr b) (cadr a))))
                       xs))))
    (if (> (length xs) 1)
      (vector_image (fix (apply 'min xs)) y (fix (apply 'max xs)) y col))
    (setq y (1+ y))))

(defun rrm:preview (polys / w h allp minx maxx miny maxy sc ox oy pl px e col)
  (setq w (dimx_tile "prev") h (dimy_tile "prev"))
  (start_image "prev")
  (fill_image 0 0 w h 0)
  (if polys
    (progn
      (setq allp (apply 'append (mapcar 'cadr polys))
            minx (apply 'min (mapcar 'car allp))
            maxx (apply 'max (mapcar 'car allp))
            miny (apply 'min (mapcar 'cadr allp))
            maxy (apply 'max (mapcar 'cadr allp))
            sc   (min (/ (- w 16.0) (- maxx minx))
                      (/ (- h 16.0) (- maxy miny)))
            ox   (/ (- w (* sc (- maxx minx))) 2.0)
            oy   (/ (- h (* sc (- maxy miny))) 2.0))
      (foreach pl polys
        (setq col (cdr (assoc (car pl) *rrmlay*))
              px  (mapcar
                    '(lambda (q)
                       (list (+ ox (* sc (- (car q) minx)))
                             (- h (+ oy (* sc (- (cadr q) miny))))))
                    (cadr pl)))
        (rrm:fillpoly px col)
        (foreach e (rrm:edges px)
          (vector_image (fix (car (car e)))  (fix (cadr (car e)))
                        (fix (car (cadr e))) (fix (cadr (cadr e))) 7)))))
  (end_image))

;; "a, b, c" from list of numbers
(defun rrm:join (l / s x)
  (setq s "")
  (foreach x l
    (setq s (strcat s (if (= s "") "" ", ") (rtos x 2 2))))
  s)

(defun rrm:clip (s)
  (if (> (strlen s) 46) (strcat (substr s 1 43) "...") s))

(defun rrm:calc (hs / ps)
  (setq ps (mapcar 'rrm:params hs))
  (set_tile "c1" (strcat "Sections            : " (itoa (length hs))))
  (set_tile "c2" (rrm:clip (strcat "Heights             : " (rrm:join hs))))
  (set_tile "c3" (rrm:clip (strcat "RRM base width      : "
                    (rrm:join (mapcar '(lambda (p) (* (nth 7 p) (nth 3 p))) ps)))))
  (set_tile "c4" (rrm:clip (strcat "Bottom width        : "
                    (rrm:join (mapcar '(lambda (p) (+ (* (nth 7 p) (nth 3 p)) (* 2.0 (nth 2 p)))) ps)))))
  (set_tile "c5" (rrm:clip (strcat "Bed drop            : "
                    (rrm:join (mapcar '(lambda (p) (* (/ (nth 6 p) 100.0)
                                                     (+ (* (nth 7 p) (nth 3 p)) (* 2.0 (nth 2 p))))) ps)))))
  (set_tile "c6" (rrm:clip (strcat "Back slope offset   : "
                    (rrm:join (mapcar '(lambda (p) (* (/ (nth 8 p) 100.0) (nth 3 p))) ps)))))
  (set_tile "c7" (strcat "RRM top width       : " (rtos (rrm:num "wc") 2 3))))

(defun rrm:clearcalc ( / k)
  (foreach k '("c1" "c2" "c3" "c4" "c5" "c6" "c7") (set_tile k "")))

;; called by every edit box / OK
(defun rrm:chg ( / err hs)
  (rrm:read)
  (setq err (rrm:checkall))
  (cond
    (err (set_tile "error" err)
         (rrm:clearcalc)
         (rrm:preview nil)
         nil)
    (T   (setq hs (rrm:heights))
         (set_tile "error" "")
         (rrm:calc hs)
         (rrm:preview (rrm:multi hs (rrm:num "gp")))
         T)))

;;; ---------------- dialog : DCL (written to a temp file) -------------------
(defun rrm:eb (k l)
  (strcat ": edit_box { key = \"" k "\"; label = \"" l "\"; edit_width = 14; }"))

(defun rrm:tx (k)
  (strcat ": text { key = \"" k "\"; label = \"\"; width = 46; }"))

(defun rrm:dcllines ()
  (list
    "rrmsec : dialog {"
    "  label = \"RRM Wall Section Generator  (single / multiple heights)\";"
    "  : row {"
    "    : column {"
    "      : boxed_column {"
    "        label = \"Section input (metres)\";"
    (rrm:eb "ts" "Thickness of stone soling")
    (rrm:eb "tp" "Thickness of PCC bed")
    (rrm:eb "cv" "Cover")
    (rrm:eb "hh" "RRM height(s)  e.g. 2,2.5,3,4")
    (rrm:eb "tc" "Thickness of PCC cap")
    (rrm:eb "wc" "Width of PCC cap (top width)")
    "      }"
    "      : boxed_column {"
    "        label = \"Factors\";"
    (rrm:eb "sl" "Bed slope (% of bottom width)")
    (rrm:eb "bf" "Base factor (RRM base / height)")
    (rrm:eb "bk" "Back slope (% of RRM height)")
    (rrm:eb "gp" "Gap between sections (m)")
    (rrm:eb "uf" "Drawing units per metre")
    "      }"
    "    }"
    "    : column {"
    "      : boxed_column {"
    "        label = \"Calculated (one value per height)\";"
    (rrm:tx "c1") (rrm:tx "c2") (rrm:tx "c3") (rrm:tx "c4")
    (rrm:tx "c5") (rrm:tx "c6") (rrm:tx "c7")
    "      }"
    "      : boxed_column {"
    "        label = \"Preview\";"
    "        : image { key = \"prev\"; width = 46; height = 14; color = 0; fixed_width = true; fixed_height = true; }"
    "      }"
    "    }"
    "  }"
    "  : row {"
    "    : button { key = \"pick\"; label = \"Pick insertion point <\"; width = 24; fixed_width = true; }"
    "    : text { key = \"ipt\"; label = \"\"; width = 40; }"
    "  }"
    "  : text { key = \"error\"; label = \"\"; width = 80; }"
    "  ok_cancel;"
    "}"))

;; returns 1 = OK, 2 = pick point, 0 = cancel
(defun rrm:dialog ( / fn f id res k)
  (setq fn (strcat (getvar "TEMPPREFIX") "rrmsec_tmp.dcl")
        f  (open fn "w")
        res 0)
  (if (null f)
    (alert "Cannot write temporary DCL file.")
    (progn
      (foreach k (rrm:dcllines) (write-line k f))
      (close f)
      (setq id (load_dialog fn))
      (if (not (new_dialog "rrmsec" id))
        (alert "Cannot load dialog.")
        (progn
          (foreach k *rrmv* (set_tile (car k) (cdr k)))
          (set_tile "ipt" (strcat "Insertion: "
                                  (rtos (car *rrmip*) 2 2) " , "
                                  (rtos (cadr *rrmip*) 2 2)))
          (foreach k '("ts" "tp" "cv" "hh" "tc" "wc" "sl" "bf" "bk" "gp" "uf")
            (action_tile k "(rrm:chg)"))
          (action_tile "pick"   "(rrm:read) (done_dialog 2)")
          (action_tile "accept" "(if (rrm:chg) (done_dialog 1))")
          (action_tile "cancel" "(done_dialog 0)")
          (rrm:chg)
          (setq res (start_dialog))))
      (if id (unload_dialog id))
      (vl-catch-all-apply 'vl-file-delete (list fn))))
  res)

;;; ---------------- main command --------------------------------------------
(defun c:RRMSEC ( / res pt hs)
  (setq res 2)
  (while (= res 2)
    (setq res (rrm:dialog))
    (if (= res 2)
      (progn
        (setq pt (getpoint "\nPick insertion point (top-back corner of first PCC bed) <0,0>: "))
        (if pt (setq *rrmip* pt)))))
  (if (= res 1)
    (progn
      (setq hs (rrm:heights))
      (rrm:draw hs (rrm:num "gp") (rrm:num "uf") *rrmip*)
      (princ (strcat "\n" (itoa (length hs)) " RRM section(s) drawn.")))
    (princ "\nRRMSEC cancelled."))
  (princ))

;; alias
(defun c:RRMMULTI () (c:RRMSEC))

(princ "\nRRMSEC loaded - type RRMSEC (or RRMMULTI) to start.")
(princ)

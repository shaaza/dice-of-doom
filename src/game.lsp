;;;; Global Variables
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;;; Game Representation
;;;+ Functional
;;; board-array : (list representation of board) ->  (array representation of board)

(defun board-array (lst)
             (make-array *board-hexnum* :initial-contents lst))

;;; player-letter : 0 -> a ; 1 -> b

(defun player-letter (number)
               (code-char (+ 97 number)))

;;;+ Imperative

;;; gen-board : Generate a list for a 2 by 2 board 

(defun gen-board ()
                 (board-array (loop for i below *board-hexnum*
                                    collect (list (random *num-players*) 
                                                  (random (1+ *max-dice*))))))

;;; draw-board : Draw the board in the CLI

(defun draw-board (board) 
                  (loop for y below *board-size*
                        do (progn (fresh-line)
                                  (loop repeat (- *board-size* y)
                                        do (princ " "))
                                  (loop for x below *board-size*
                                        for hex = (aref board (+ x (* *board-size* y)))
                                        do (format t "~a-~a " (player-letter (first hex)) 
                                                             (second hex))))))
                                                                               

;;;; Rule-Engine

;;;+ Functional 
;;; game-tree: (board, player#, spare-dice#, first-move?) -> (list of all legal moves)

(defun game-tree (board player spare-dice first-move)
                 (list player
                       board
                       (add-passing-move board 
                                         player 
                                         spare-dice 
                                         first-move
                                         (add-attacking-moves board
                                                              player
                                                              spare-dice)))) 

;; add-passing-move: (board, player#, spare-dice#, first-move?, list of attacking move) -> (all moves: passing move added to attacking ones)

(defun add-passing-move (board player spare-dice first-move moves)
                        (if first-move
                            moves
                            (cons (list nil
                                        (game-tree (add-new-dice board player (1- spare-dice))
                                                   (mod (1+ player) *num-players*)
                                                   0
                                                   t))
                                  moves)))      

;; add-attacking-moves: (board, player#, spare-dice#) -> (list of all attacking moves)

(defun add-attacking-moves (board cur-player spare-dice)
                           (labels ((player (pos)
                                            (car (aref board pos)))
                                    (dice (pos)
                                          (cadr (aref board pos))))
                                
                                (mapcan (lambda (src)
                                                (when (eq (player src) cur-player)
                                                      (mapcan (lambda (dst) 
                                                                      (when (and (not (equal (player dst) cur-player))
                                                                                 (> (dice src) (dice dst)))
                                                                            (list (list (list src dst)
                                                                                  (game-tree (board-attack board cur-player src dst (dice src))
                                                                                             cur-player
                                                                                             (+ spare-dice (dice dst))
                                                                                             nil)))))
                                                              (neighbors src))))
                                        (loop for i below *board-hexnum*
                                              collect i))))
                                         

; neighbors: (current hexagon) -> (neighbouring hexagons) 

(defun neighbors (pos)
                 (let ((up   (- pos *board-size*))
                       (down (+ pos *board-size*)))
                      (loop for p in (append (list up down)
                                             (unless (zerop (mod pos *board-size*))
                                                     (list (1- up) (1- pos)))
                                             (unless (zerop (mod (1+ pos) *board-size*))
                                                     (list (1+ pos) (1+ down))))
                            when (and (>= p 0) (< p *board-hexnum*))
                            collect p)))

; board-attack: (board, player#, src-pos#, dst-pos#, src-dice#) -> (new board position after attack)                                                                     

(defun board-attack (board player src dst dice)
                    (board-array (loop for pos 
                                       for hex across board
                                       collect (cond ((eq pos src) (list player 1))
                                                     ((eq pos dst) (list player (1- dice)))
                                                     (t hex)))))
                                                           
; add-new-dice (reinforcements): (board, player, spare-dice) -> (new board array with reinforcements added)

(defun add-new-dice (board player spare-dice)
                    (labels ((f (lst n)
                                (cond ((null lst) nil)
                                      ((zerop n) lst)
                                      (t (let ((cur-player (caar lst))
                                               (cur-dice (cadar lst)))
                                               (if (and (eq cur-player player) (< cur-dice *max-dice*))
                                                   (cons (list cur-player (1+ cur-dice))
                                                         (f (cdr lst) (1- n)))
                                                   (cons (car lst) (f (cdr lst) n))))))))
                            (board-array (f (coerce board 'list) spare-dice))))                   

;;;; Human Player


;;; Play vs Human: 1. Feedback 2. Input 3. If game over, announce winner
(defun play-vs-human (tree)
                     (fresh-line)
                     (print-info tree)
                     (if (caddr tree)
                         (play-vs-human (handle-human tree))
                         (announce-winner (cadr tree))))


;; print-info - Feedback: Printing game state

(defun print-info (tree)
                  (fresh-line)
                  (format t "Current player: ~a" (player-letter (car tree)))
                  (draw-board (cadr tree)))

;; handle-human - Input: handle user input

(defun handle-human (tree)
                    (fresh-line)
                    (princ "Choose your next move: ")
                    (let ((moves (caddr tree)))
                         (loop for move in moves
                               for n from 1
                               do (let ((action (car move)))
                                       (format t "~a. " n)
                                       (if action
                                           (format t "~a -> ~a " (car action) (cadr action))
                                           (princ "end turn "))))
                         (fresh-line)
                         (cadr (nth (1- (read)) moves))))

;; announce-winner : Announcing Winners: 1. Find winner 2. Announce the winner
;+ Functional
; Find winners

(defun winners (board) 
         (let* ((tally (loop for hex across board
                             collect (car hex)))
                (totals (mapcar (lambda (player)
                                        (cons player (count player tally)))
                                (remove-duplicates tally)))
                (best (apply #'max (mapcar #'cdr totals))))
               
               (mapcar #'car 
                       (remove-if (lambda (x)
                                          (not (eq (cdr x) best)))
                                  totals))))
;+ Imperative
; Announce Winners

(defun announce-winner (board)
                       (fresh-line)
                       (let ((w (winners board)))
                            (if (> (length w) 1)
                                (format t "The game is a tie between ~a" (mapcar #'player-letter w))
                                (format t "The winner is ~a." (player-letter (car w))))))

;; Handle-Human Moves




;;;; Computer Player
;;;; play-vs-computer : 1. Analyze: Point ratings for moves and boards 2. Handle computer input

(defun play-vs-computer (tree)
                        (print-info tree)
                        (cond ((null (caddr tree)) (announce-winner (cadr tree)))
                              ((zerop (car tree)) (play-vs-computer (handle-human tree)))
                              (t (play-vs-computer (handle-computer tree)))))

;;; 1. Analyze
;;+ Functional

;; rate-position

(defun rate-position (tree player)
                     (let ((moves (caddr tree)))
                          (if moves
                              (apply (if (eq (car tree) player)
                                         #'max
                                         #'min)
                                     (get-ratings tree player))
                              (let ((w (winners (cadr tree))))
                                   (if (member player w)
                                       (/ 1 (length w))
                                       0)))))

;; get-ratings

(defun get-ratings (tree player)
                   (mapcar (lambda (move)
                                   (rate-position (cadr move) player))
                           (caddr tree)))



;;; 2. Handle Computer Input

(defun handle-computer (tree)
                       (let ((ratings (get-ratings tree (car tree))))
                            (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))
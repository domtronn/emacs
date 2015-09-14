;;; poker.el --- Texas hold'em poker

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Version: 0.1
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; poker.el provides texas hold'em poker gameplay for Emacs.

;;; Requires:

(require 'cl-lib)
(require 'cookie1)
(require 'ert)

;;; Compatibility:

(eval-and-compile
  (unless (fboundp 'cookie-shuffle-vector)
    (defalias 'cookie-shuffle-vector 'shuffle-vector)))

;;; Constants:

(defconst poker-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))
(defconst poker-suits '(clubs diamonds hearts spades))
(defconst poker-deck (cl-loop for card from 0 to 51 collect card))
(defconst poker-unicode-cards
  (let ((unicode-suit '((clubs . #xD0) (diamonds . #XC0)
			(hearts . #XB0) (spades . #XA0))))
    (apply #'vector
	   (cl-loop for suit in poker-suits
		    nconc
		    (cl-loop for rank in poker-ranks
			     collect
			     (logior #x1f000
				     (cdr (assq suit unicode-suit))
				     (cond
				      ((eq rank 'ace)   #x1)
				      ((eq rank 'jack)  #xB)
				      ((eq rank 'queen) #xD)
				      ((eq rank 'king)  #XE)
				      (t                rank))))))))

;;; Code:

(defsubst poker-make-card (rank suit)
  "Make a poker card from RANK and SUIT.
RANK is one of `poker-ranks' and SUIT is one of `poker-suits'."
  (cl-assert (memq rank poker-ranks))
  (cl-assert (memq suit poker-suits))
  (+ (* (cl-position suit poker-suits) 13) (cl-position rank poker-ranks)))

(defsubst poker-card-rank (card)
  "The rank (a integer from 0 to 12) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (% card 13))

(defsubst poker-card-suit (card)
  "The suit (an integer from 0 to 3) of a poker CARD."
  (cl-check-type card (integer 0 51))
  (/ card 13))

(defsubst poker-card-name (card)
  "The name of a poker CARD (a string of two characters)."
  (cl-check-type card (integer 0 51))
  (concat (aref ["2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"]
		(poker-card-rank card))
	  (aref ["c" "d" "h" "s"] (poker-card-suit card))))

(defun poker-card-unicode (card)
  "The Unicode character for a poker CARD."
  (aref poker-unicode-cards card))

(defun poker-hand-value (hand)
  "Calculate the value of a given 5 card poker HAND.
The result is a 24 bit integer where the leftmost 4 bits (0-8) indicate the type
of hand, and the remaining nibbles are rank values of decisive cards.
The highest possible value is therefore #x8CBA98 and the lowest is #x053210."
  (let* ((ranks (mapcar #'poker-card-rank hand))
	 (rank-counts (sort (mapcar (lambda (rank) (cons (cl-count rank ranks) rank))
				    (cl-remove-duplicates ranks))
			    (lambda (lhs rhs) (or (> (car lhs) (car rhs))
						  (and (= (car lhs) (car rhs))
						       (> (cdr lhs) (cdr rhs)))))))
	 (ranks-length nil))
    (setq ranks (mapcar #'cdr rank-counts)
	  rank-counts (mapcar #'car rank-counts)
	  ranks-length (length ranks))
    (logior (ash (cond
		  ((equal rank-counts '(2 1 1 1)) 1)
		  ((eq ranks-length 5)
		   (let ((straight (or (when (and (eq (nth 0 ranks) 12)
						   (eq (nth 1 ranks) 3))
					  (setq ranks '(3 2 1 0 0)))
					(eq (- (nth 0 ranks) (nth 4 ranks)) 4)))
			  (flush (eq (length (cl-delete-duplicates
					      (mapcar #'poker-card-suit hand))) 1)))
		      (cond ((and straight flush) 8) (flush 5) (straight 4) (t 0))))
		  ((equal rank-counts '(2 2 1)) 2)
		  ((equal rank-counts '(3 1 1)) 3)
		  ((equal rank-counts '(3 2)) 6)
		  ((equal rank-counts '(4 1)) 7))
		 20)
	    (ash (nth 0 ranks) 16)
	    (ash (nth 1 ranks) 12)
	    (if (> ranks-length 2) (ash (nth 2 ranks) 8) 0)
	    (if (> ranks-length 3) (ash (nth 3 ranks) 4) 0)
	    (if (> ranks-length 4) (nth 4 ranks) 0))))

(defun poker-hand-> (hand1 hand2)
  "Return non-nil if HAND1 is better than HAND2."
  (> (poker-hand-value hand1) (poker-hand-value hand2)))

(defun poker-sort-hands (hands)
  "Sort HANDS (a list of list of cards) according to the value of the individual hands."
  (mapcar #'cdr
	  (cl-sort (mapcar (lambda (hand) (cons (poker-hand-value hand) hand)) hands)
		   #'> :key #'car)))

(defun poker-combinations (n list)
  "A list of all unique ways of taking N different elements from LIST."
  (when list
    (let ((length (length list)))
      (nconc (if (eq n 1)
		 (list (if (cdr list) (list (car list)) list))
	       (if (eq n length)
		   (list list)
		 (mapcar (lambda (rest) (cons (car list) rest))
			 (poker-combinations (1- n) (cdr list)))))
	     (when (> length n) (poker-combinations n (cdr list)))))))

(defun poker-possible-hands (cards)
  "Generate a list of possible 5 card poker hands from CARDS.
CARDS is a list of 5 to 7 poker cards."
  (cl-check-type (length cards) (integer 5 7))
  (cond
   ;; While this could certainly be made generic,
   ;; the performance of this hand-crafted implementation is unmatched.
   ((eq 7 (length cards))
    (let ((car (car cards))
	  (cdr (cdr cards)))
      (let ((cadr (car cdr))
	    (cddr (cdr cdr)))
	(let ((caddr (car cddr))
	      (cdddr (cdr cddr)))
	  (let ((cadddr (car cdddr))
		(cddddr (cdr cdddr)))
	    (let ((caddddr (car cddddr))
		  (cdddddr (cdr cddddr)))
	      (let ((cadddddr (car cdddddr))
		    (cddddddr (cdr cdddddr)))
		(list (list car cadr caddr cadddr caddddr)
		      (list car cadr caddr cadddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons cadddr cddddddr))))
		      (list car cadr caddr caddddr cadddddr)
		      (cons car (cons cadr (cons caddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons caddr cdddddr)))
		      (cons car (cons cadr (butlast cdddr)))
		      (cons car (cons cadr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons cadr (cons cadddr cdddddr)))
		      (cons car (cons cadr cddddr))
		      (cons car (butlast cddr))
		      (cons car (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons car (cons caddr (cons cadddr cdddddr)))
		      (cons car (cons caddr cddddr))
		      (cons car cdddr)
		      (butlast cdr)
		      (cons cadr (cons caddr (cons cadddr (cons caddddr cddddddr))))
		      (cons cadr (cons caddr (cons cadddr cdddddr)))
		      (cons cadr (cons caddr cddddr))
		      (cons cadr cdddr)
		      cddr))))))))
   (t (poker-combinations 5 cards))))

(defun poker-best-hand (cards)
  "Find the best hand for a number of CARDS (usually a list of 6 or 7 elements)."
  (let ((max 0) (best-hand nil))
    (dolist (hand (poker-possible-hands cards) best-hand)
      (let ((value (poker-hand-value hand)))
	(when (> value max) (setq max value best-hand hand))))))

(defun poker-rank-to-string (rank)
  "The english name of poker card RANK."
  (aref ["2" "3" "4" "5" "6" "7" "8" "9" "10" "jack" "queen" "king" "ace"] rank))

(defun poker-rank-to-plural-string (rank)
  "The plural english name of poker card RANK."
  (concat (poker-rank-to-string rank) "s"))

(defun poker-describe-hand (hand)
  "Return a string description of the value of the given poker HAND.
HAND is a list of 5 poker cards."
  (cl-assert (eq (length hand) 5))
  (pcase (let ((value (poker-hand-value hand)))
	   (cl-loop for i from 5 downto 0 collect (logand (ash value (- (* i 4))) #xf)))
    (`(8 ,high ,_ ,_ ,_ ,_) (pcase high
			      (12 "royal flush")
			      (_ (format "%s high straight flush"
					 (poker-rank-to-string high)))))
    (`(7 ,four ,high 0 0 0) (format "four %s, %s high"
				    (poker-rank-to-plural-string four)
				    (poker-rank-to-string high)))
    (`(6 ,three ,two 0 0 0) (format "full house of %s and %s"
				    (poker-rank-to-plural-string three)
				    (poker-rank-to-plural-string two)))
    (`(5 ,high ,k1 ,k2 ,k3 ,k4) (format "%s high flush, %s %s %s and %s kickers"
					(poker-rank-to-string high)
					(poker-rank-to-string k1)
					(poker-rank-to-string k2)
					(poker-rank-to-string k3)
					(poker-rank-to-string k4)))
    (`(4 ,high ,_ ,_ ,_ ,_) (pcase high
			      (3 "5 high straight (steel wheel)")
			      (_ (format "%s high straight"
					 (poker-rank-to-string high)))))
    (`(3 ,three ,high ,kicker 0 0) (format "three %s, %s high, %s kicker"
					   (poker-rank-to-plural-string three)
					   (poker-rank-to-string high)
					   (poker-rank-to-string kicker)))
    (`(2 ,two1 ,two2 ,high 0 0) (format "wwo pairs of %s and %s, %s high"
				    (poker-rank-to-plural-string two1)
				    (poker-rank-to-plural-string two2)
				    (poker-rank-to-string high)))
    (`(1 ,two ,high ,k1 ,k2 0) (format "a pair of %s, %s high, %s and %s kickers"
				       (poker-rank-to-plural-string two)
				       (poker-rank-to-string high)
				       (poker-rank-to-string k1)
				       (poker-rank-to-string k2)))
    (`(0 ,high ,k1 ,k2 ,k3 ,k4) (format "high card %s, %s %s %s and %s kickers"
					(poker-rank-to-string high)
					(poker-rank-to-string k1)
					(poker-rank-to-string k2)
					(poker-rank-to-string k3)
					(poker-rank-to-string k4)))))

(defun poker-random-deck ()
  "Return a shuffled deck of 52 poker cards."
  (append (cookie-shuffle-vector (apply 'vector poker-deck)) nil))

(defun poker-strength (pocket &optional community opponents)
  "Estimate the strength of POCKET and COMMUNITY cards against number of OPPONENTS.
The optional number of OPPONENTS defaults to 2."
  (let ((wins 0) (iterations 100))
    (dotimes (i iterations)
      (let ((deck (poker-random-deck))
	    (players (make-vector (or opponents 1) nil)))
	(dolist (card pocket) (setq deck (delete card deck)))
	(dolist (card community) (setq deck (delete card deck)))
	(dotimes (cards 2)
	  (dotimes (player (or opponents 1))
	    (push (pop deck) (aref players player))))
	(let ((board (append community nil)))
	  (dotimes (_ (- 5 (length community)))
	    (push (pop deck) board))
	  (setq wins (+ wins (caar (cl-sort
				    (mapcar (lambda (info)
					      (setcdr info (poker-best-hand
							    (append (cdr info) board)))
					      info)
					    (nconc (list (cons 1 pocket))
						   (mapcar (lambda (cards)
							     (cons 0 cards))
							   players)))
				    #'poker-hand-> :key #'cdr)))))))
    (/ (float wins) iterations)))

(defun poker-pre-flop-starting-hands (opponents)
  (let ((rank-name (vector "2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A"))
	(hands ()))
    (dolist (rank1 poker-ranks)
      (dolist (rank2 poker-ranks)
	(if (eq rank1 rank2)
	  (push (cons (poker-strength (list (poker-make-card rank1 'clubs)
					    (poker-make-card rank2 'hearts))
				      nil opponents)
		      (if (memq rank1 '(2 3 4 5 6 7 8 9))
			  (+ (* rank1 10) rank1)
			(intern (format "%s%s"
					(aref rank-name (cl-position rank1 poker-ranks))
					(aref rank-name (cl-position rank2 poker-ranks))))))
		hands)
	  (when (< (cl-position rank1 poker-ranks) (cl-position rank2 poker-ranks))
	    (let ((tmp rank1))
	      (setq tmp rank1
		    rank1 rank2
		    rank2 tmp)))
	  (dolist (suited '(nil t))
	    (let ((code (if (and (memq rank1 '(2 3 4 5 6 7 8 9))
				 (memq rank2 '(2 3 4 5 6 7 8 9))
				 (not suited))
			    (+ (* rank1 10) rank2)
			  (intern
			   (format "%s%s%s"
				   (aref rank-name (cl-position rank1 poker-ranks))
				   (aref rank-name (cl-position rank2 poker-ranks))
				   (if suited "s" ""))))))
	      (unless (rassq code hands)
		(accept-process-output)
		(message "%S" code)
		(push (cons (poker-strength
			     (list (poker-make-card rank1 'clubs)
				   (poker-make-card rank2 (if suited 'clubs 'hearts)))
			     nil opponents) code) hands)))))))
    (cl-sort hands #'> :key #'car)))

(defun poker-pot-odds (bet pot)
  "Return the odds when BET is added to POT."
  (/ (float bet) (+ pot bet)))

(defun poker-random-fold-call-raise (fold% call% raise%)
  "Randomly choose between FOLD%, CALL% and RAISE%."
  (cl-assert (= (+ fold% call% raise%) 100))
  (let ((value (random 100)))
    (cond
     ((< value fold%) 'fold)
     ((< value (+ fold% call%)) 'call)
     ((< value (+ fold% call% raise%)) 'raise)
     (t (error "Random FCR Error")))))

(defun poker-make-player (name fcr-fn)
  "Create a new poker player with NAME and FCR-FN.
FCR-FN specifies a function to use when a fold-call-raise decision is required."
  (list (cons 'name name)
	(cons 'stack 0)
	(cons 'wagered 0)
	(cons 'pocket nil)
	(cons 'fcr-fn fcr-fn)))
  
(defun poker-player-name (player)
  "Return the name of poker PLAYER."
  (cdr (assq 'name player)))

(defun poker-player-stack (player)
  "Return the remaining stack of poker PLAYER."
  (cdr (assq 'stack player)))

(defun poker-player-bet (player amount)
  "Make PLAYER bet AMOUNT of chips."
  (let ((actual (min (poker-player-stack player) amount)))
    (when (zerop actual) (message "WARNING: Actual is 0."))
    (unless (zerop actual)
      (cl-decf (cdr (assq 'stack player)) actual)
      (cl-incf (cdr (assq 'wagered player)) actual))
    actual))

(defun poker-player-payout (player amount)
  "Give PLAYER AMOUNT of chips."
  (cl-incf (cdr (assq 'stack player)) amount)
  amount)

(defun poker-player-wagered (player)
  "Return the amount of chips currently wagered by poker PLAYER."
  (cdr (assq 'wagered player)))

(defun poker-player-pocket (player)
  "Return the current pocket (hole) cards of PLAYER."
  (cdr (assq 'pocket player)))

(defun poker-player-fold (player)
  "Make PLAYER fold and forget about their cards."
  (setcdr (assq 'pocket player) nil))

(defun poker-player-active-p (player)
  (and (poker-player-pocket player) (> (poker-player-wagered player) 0)))

(defun poker-player-all-in-p (player)
  (and (poker-player-active-p player) (zerop (poker-player-stack player))))

(defun poker-player-can-bet-p (player)
  (and (poker-player-pocket player) (> (poker-player-stack player) 0)))

(defun poker-player-best-hand (player community)
  (cl-assert (>= (length (poker-player-pocket player)) 2))
  (cl-assert (>= (length community) 3))
  (poker-best-hand (append (poker-player-pocket player) community)))

(defun poker-player-give-card (player card)
  (cl-check-type card (integer 0 51))
  (push card (cdr (assq 'pocket player))))

(defun poker-player-fcr-fn (player)
  (cdr (assq 'fcr-fn player)))

(defun poker-player-fcr (player pot amount-to-call max-raise board opponents)
  (funcall (poker-player-fcr-fn player)
	   player pot amount-to-call max-raise board opponents))

(defun poker-read-fold-call-raise (pot to-call max-raise &optional prompt)
  (let ((cursor-in-echo-area t)
	(map (let ((map (make-sparse-keymap)))
	       (define-key map [?c] 'call)
	       (define-key map [?f] 'fold)
	       (when (> max-raise 0) (define-key map [?r] 'raise))
	       (define-key map [?q] 'quit)
	       map))
	(action nil))
    (while (not action)
      (message (format "%s%d in pot, %d to call: (f)old%s: "
		       (or prompt "") pot to-call
		       (if (> max-raise 0)
			   (if (zerop to-call)
			       ", (c)heck or (r)aise"
			     ", (c)all or (r)aise")
			 (if (zerop to-call)
			     " or (c)heck"
			   " or (c)all"))))
      (setq action (lookup-key map (vector (read-event)))))
    (cond
     ((eq action 'fold) nil)
     ((eq action 'call) to-call)
     ((eq action 'raise) (+ to-call (let ((raise (1+ max-raise)))
				      (while (> raise max-raise)
					(setq raise
					      (read-number (format "Raise by (max %d): "
								   max-raise))))
				      (cl-check-type raise integer)
				      raise))))))

(defun poker-interactive-fcr (player pot due max-raise board opponents)
  (poker-read-fold-call-raise
   pot due max-raise (format "%s%s, %d stack, "
			     (mapconcat #'poker-card-name (poker-player-pocket player) ", ")
			     (if board
				 (concat "(" (mapconcat #'poker-card-name board " ") ")")
			       "")
			     (poker-player-stack player))))

(defun poker-automatic-fcr (player pot due max-raise board &optional opponents)
  (let* ((strength (poker-strength (poker-player-pocket player) board opponents))
	 (pot-odds (poker-pot-odds due pot))
	 (rate-of-return (/ strength pot-odds))
	 (action (cond
		  ((< rate-of-return 0.8) (poker-random-fold-call-raise 95 1 4))
		  ((< rate-of-return 1.0) (poker-random-fold-call-raise 80 15 5))
		  ((< rate-of-return 1.3) (poker-random-fold-call-raise 0 60 40))
		  (t (poker-random-fold-call-raise 0 25 75)))))
    (when (and (memq action '(call raise))
	       (< (- (poker-player-stack player) due) 200) (< strength 0.5))
      (setq action 'fold))
    (when (and (eq action 'raise) (< strength 0.1))
      (setq action 'call))
    (when (and (zerop due) (eq action 'fold))
      (setq action 'call))
    (cond
     ((eq action 'fold) nil)
     ((eq action 'call) due)
     ((eq action 'raise) (+ due (min 100 max-raise))))))

(defun poker-rotate-to-first (player players)
  "Make PLAYER the first element of PLAYERS."
  (let ((position (cl-position player players)))
    (when position
      (let ((shift (- (length players) position)))
	(append (last players shift) (butlast players shift))))))

(defun poker-next-players (player players)
  (cdr (poker-rotate-to-first player players)))

(defun poker-next-player (player players)
  (car (poker-next-players player players)))

(defun poker-pot (players)
  "Return the amount of chips in the pot, the total wagered by all PLAYERS."
  (apply #'+ (mapcar #'poker-player-wagered players)))

(defun poker-current-wager (players)
  "Determine the maximum amount of chips wagered by any of PLAYERS."
  (apply #'max (mapcar #'poker-player-wagered players)))

(defun poker-collect-wager (amount players)
  "Collect AMOUNT of wager from PLAYERS."
  (let ((total 0))
    (dolist (player players total)
      (let ((wagered (assq 'wagered player)))
	(if (> amount (cdr wagered))
	    (progn
	      (setq total (+ total (cdr wagered)))
	      (setcdr wagered 0))
	  (setq total (+ total amount))
	  (setcdr wagered (- (cdr wagered) amount)))))))

(defun poker-distribute-winnings (winners players)
  "Distribute chips to WINNERS from PLAYERS accounting for split-pot rules."
  (cl-assert (not (null winners)))
  (cl-assert (> (length players) 1))
  (if (= (length winners) 1)
      (poker-player-payout (car winners)
			   (poker-collect-wager (poker-player-wagered (car winners))
						players))
    (let* ((lowest (apply #'min (mapcar #'poker-player-wagered winners)))
	   (total (poker-collect-wager lowest players))
	   (each (/ total (length winners)))
	   (leftover (- total (* each (length winners)))))
      (poker-player-payout (car winners) (+ each leftover))
      (dolist (player (cdr winners)) (poker-player-payout player each))
      total)))

(defun poker-player-max-raise (player players)
  "Determine the maximum amount allowed to raise for PLAYER considering PLAYERS stacks."
  (let ((other-stacks (mapcar #'poker-player-stack
			      (cl-remove
			       player
			       (cl-remove-if-not #'poker-player-can-bet-p players)))))
    (min (poker-player-stack player) (if other-stacks (apply #'max other-stacks) 0))))

(defun poker-interactive-p (players)
  (cl-find #'poker-interactive-fcr players :key #'poker-player-fcr-fn))

(defun poker-dealer-ask-player (player players board allow-raise)
  "Ask PLAYER for next action."
  (let ((pot (poker-pot players))
	(max-raise (if allow-raise (poker-player-max-raise player players) 0))
	(amount-to-call (- (poker-current-wager players)
			   (poker-player-wagered player)))
	(opponents (1- (length (cl-remove-if-not #'poker-player-pocket players)))))
    (cl-assert (> opponents 0))
    (let ((decision (poker-player-fcr player pot amount-to-call max-raise
				      board opponents)))
      (cl-assert (or (null decision)
		     (and (integerp decision)
			  (<= (- decision amount-to-call) max-raise))))
      (cond
       ((null decision)
	(message (format "%s folds." (poker-player-name player)))
	(poker-player-fold player))
       ((zerop decision)
	(message "%s checks." (poker-player-name player)))
       ((integerp decision)
	(if (= decision amount-to-call)
	    (message "%s calls %d." (poker-player-name player) decision)
	  (cl-assert (>= decision amount-to-call))
	  (message "%s raises by %d."
		   (poker-player-name player) (- decision amount-to-call)))
	(poker-player-bet player decision))))))

(defun poker-dealer (min-bet deck board players)
  "Deal a round of texas holdem poker with MIN-BET for PLAYERS."
  (cl-assert (> (length players) 1))
  (cond
   ;; pre-flop
   ((and (null board) (zerop (poker-pot players)))
    (let ((blinds players))
      (message "Collecting blinds.")
      (message "%s posts %d small blind." (poker-player-name (car blinds)) (/ min-bet 2))
      (poker-player-bet (car blinds) (/ min-bet 2))
      (message "%s posts %d big blind." (poker-player-name (cadr blinds)) min-bet)
      (poker-player-bet (cadr blinds) min-bet)
      (message "Dealing cards to players.")
      (dotimes (_ 2)
	(dolist (player players) (poker-player-give-card player (pop deck))))

      (message "Initial betting round.")

      (dolist (player (poker-next-players (cadr blinds) players))

	(unless (zerop (poker-player-stack player))
	  (poker-dealer-ask-player player players board t)))

      (when (and (not (zerop (poker-player-stack (cadr blinds))))
		 (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		     (< (poker-player-wagered (cadr blinds))
			(poker-current-wager players))))
	(poker-dealer-ask-player (cadr blinds) players board t))

      (poker-dealer min-bet deck board players)))

   ;; All but one have folded
   ((and (not (zerop (poker-pot players)))
	 (= (length (cl-remove-if-not #'poker-player-active-p players)) 1))
    (let ((winners (cl-remove-if-not #'poker-player-active-p players)))
      (message "%s silently wins %d."
	       (poker-player-name (car winners))
	       (poker-distribute-winnings winners players))
      winners))
   
   ;; pre-flop, second round of bets, no raises allowed
   ((and (null board) (cl-remove-if
		       (lambda (player)
			 (or (zerop (poker-player-wagered player))
			     (not (poker-player-pocket player))
			     (poker-player-all-in-p player)
			     (= (poker-player-wagered player)
				(poker-current-wager players))))
		       (poker-rotate-to-first (cadr players) players)))

    (message "Pre flop, second round of bets.")

    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (zerop (poker-player-wagered player))
			   (not (poker-player-pocket player))
			   (poker-player-all-in-p player)
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     (poker-rotate-to-first (cadr players) players)))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board nil)))

    (poker-dealer min-bet deck board players))

   ;; flop
   ((null board)
    (dotimes (_ 3) (push (pop deck) board))

    (message "The flop: %s" (mapconcat #'poker-card-name board " "))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; flop, second round of bets, no raises allowed
   ((and (= (length board) 3) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "The flop, second round of bets.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; turn
   ((= (length board) 3)
    (push (pop deck) board)

    (message "The turn: %s" (mapconcat #'poker-card-name board " "))

    (setq min-bet (* min-bet 2))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; turn, second round of bets, no raises allowed
   ((and (= (length board) 4) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "The turn, second round of bets.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; river
   ((= (length board) 4)
    (push (pop deck) board)
    (message "The river: %s" (mapconcat #'poker-card-name board " "))

    (dolist (player (cl-remove-if-not #'poker-player-can-bet-p players))
      (when (or (> (length (cl-remove-if-not #'poker-player-can-bet-p players)) 1)
		(< (poker-player-wagered player) (poker-current-wager players)))
	(poker-dealer-ask-player player players board t)))

    (poker-dealer min-bet deck board players))

   ;; river, second round of bets, no raises allowed
   ((and (= (length board) 5) (cl-remove-if
			       (lambda (player)
				 (or (not (poker-player-can-bet-p player))
				     (= (poker-player-wagered player)
					(poker-current-wager players))))
			       players))
    (message "Last betting round.")
    (dolist (player (cl-remove-if
		     (lambda (player)
		       (or (not (poker-player-can-bet-p player))
			   (= (poker-player-wagered player)
			      (poker-current-wager players))))
		     players))
      (poker-dealer-ask-player player players board nil))

    (poker-dealer min-bet deck board players))

   ;; showdown
   ((= (length board) 5)
    (cl-assert (not (zerop (poker-pot players))))
    (let ((in-play (cl-remove-if-not #'poker-player-active-p players))
	  (groups ())
	  (game-interactive-p (poker-interactive-p players)))
      (unless (> (length in-play) 1)
	(error "In-play to small: %S %S" in-play players))
      (while in-play
	(if (= (length in-play) 1)
	    (progn
	      (message "%s wins %d."
		       (poker-player-name (car in-play))
		       (poker-distribute-winnings in-play players))
	      (when game-interactive-p (sit-for 2))
	      (push in-play groups)
	      (setq in-play nil))
	  (let* ((best-hand-value (poker-hand-value
				   (car
				    (poker-sort-hands
				     (mapcar (lambda (player)
					       (poker-player-best-hand player board))
					     in-play)))))
		 (winners (cl-remove-if (lambda (player)
					  (< (poker-hand-value
					      (poker-player-best-hand player board))
					     best-hand-value))
					in-play)))
	    (dolist (player in-play)
	      (message "%s shows %s, %s."
		       (poker-player-name player)
		       (mapconcat #'poker-card-name (poker-player-pocket player) " ")
		       (poker-describe-hand (poker-player-best-hand player board)))
	      (when game-interactive-p (sit-for 2)))
	    (message "%s wins %d."
		     (mapconcat #'poker-player-name winners ", ")
		     (poker-distribute-winnings winners players))
	    (when game-interactive-p (sit-for 2))
	    (push winners groups))
	  (setq in-play (cl-remove-if-not #'poker-player-active-p players))))

      (cons board (nreverse groups))))

   (t (list 'error min-bet deck board players))))

;;;###autoload
(defun poker (initial-stack min-bet players)
  "Play a game of texas hold 'em poker."
  (interactive (list (read-number "Initial stack: " 1000)
		     (read-number "Minimum bet: " 50)
		     (list (poker-make-player "Angela" #'poker-automatic-fcr)
			   (poker-make-player "Bettina" #'poker-automatic-fcr)
			   (poker-make-player "Christina" #'poker-automatic-fcr)
			   (poker-make-player "Daniela" #'poker-automatic-fcr)
			   (poker-make-player "Emil" #'poker-automatic-fcr)
			   (poker-make-player "Frank" #'poker-automatic-fcr)
			   (poker-make-player "Günther" #'poker-automatic-fcr)
			   (poker-make-player "Harald" #'poker-automatic-fcr)
			   (poker-make-player "Ingrid" #'poker-automatic-fcr)
			   (poker-make-player (user-full-name) #'poker-interactive-fcr))))
  (cl-assert (> (length players) 1))
  (dolist (player players)
    (message "%s receives %d chips." (poker-player-name player) initial-stack)
    (setcdr (assq 'stack player) initial-stack))
  (let ((game-interactive-p (poker-interactive-p players))
	(button-player (nth (random (length players)) players))
	(rounds ())
	(losers ()))
    (setq players (poker-rotate-to-first button-player players))
    (while (and button-player
		(or (not game-interactive-p)
		    (poker-interactive-p players)))
      (message "Round %d, %d players." (1+ (length rounds)) (length players))

      (push (poker-dealer min-bet (poker-random-deck) () players)
	    rounds)

      (mapc #'poker-player-fold players)
      (setq button-player
	    (car-safe (cdr (cl-remove-if (lambda (player)
					   (zerop (poker-player-stack player)))
					 (poker-rotate-to-first button-player players)))))
      (let ((lost (cl-remove-if-not (lambda (player) (zerop (poker-player-stack player)))
				    players)))
	(when lost
	  (setq players (cl-remove-if
			 (lambda (player)
			   (when (member player lost)
			     (message "%s drops out." (poker-player-name player))
			     t))
			 players))
	  (setq losers (nconc losers lost))))
      (message "Remaining players: %s"
	       (mapconcat (lambda (player) (format "%s (%d)"
						   (poker-player-name player)
						   (poker-player-stack player)))
			  (cl-sort (append players nil)
				   #'> :key #'poker-player-stack)
			  " "))
      (when button-player
	(cl-assert (member button-player players))
	(let ((count (length players)))
	  (setq players (poker-rotate-to-first button-player players))
	  (cl-assert (= count (length players)))))

      (accept-process-output)

      (when (and game-interactive-p (not (poker-interactive-p players)))
	(message "You drop out in %s place."
		 (let ((rank (1+ (length players))))
		   (pcase rank
		     (2 "2nd")
		     (3 "3rd")
		     (n (format "%dth" n)))))))

    (when (and game-interactive-p (poker-interactive-p players))
      (message "You are the winner."))

    (cons players rounds)))

;;; Tests:

(ert-deftest poker-combinations ()
  (equal 21 (length (poker-combinations 5 (last poker-deck 7))))
  (equal 1326 (length (poker-combinations 2 poker-deck))))

(ert-deftest poker-possible-hands ()
  (equal (poker-possible-hands '(1 2 3 4 5 6 7))
	 (poker-combinations 5 '(1 2 3 4 5 6 7)))
  (equal (poker-possible-hands '(1 2 3 4 5 6))
	 (poker-combinations 5 '(1 2 3 4 5 6))))

(ert-deftest poker ()
  (let ((players (list (poker-make-player "Angela" #'poker-automatic-fcr)
		       (poker-make-player "Bettina" #'poker-automatic-fcr)
		       (poker-make-player "Christoph" #'poker-automatic-fcr)
		       (poker-make-player "Daniela" #'poker-automatic-fcr)
		       (poker-make-player "Emilia" #'poker-automatic-fcr)
		       (poker-make-player "Franz" #'poker-automatic-fcr)
		       (poker-make-player "Günter" #'poker-automatic-fcr)
		       (poker-make-player "Harald" #'poker-automatic-fcr)
		       (poker-make-player "Isabella" #'poker-automatic-fcr)
		       (poker-make-player "Jakob" #'poker-automatic-fcr))))
    (while (> (length players) 1)
      (should (equal (poker-player-stack (caar (poker 1000 100 players)))
		     (* 1000 (length players))))
      (setq players (cdr players)))))

;;;; ChangeLog:

;; 2014-05-18  Mario Lang	<mlang@delysid.org>
;; 
;; 	Add new simple package poker.el.
;; 


(provide 'poker)
;;; poker.el ends here

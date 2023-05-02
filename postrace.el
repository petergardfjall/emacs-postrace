;;; postrace.el --- Leave trace points to navigate important code paths.  -*- lexical-binding: t -*-
;;
;; Copyright © 2021 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-postrace
;; Keywords: workspace, project
;; Package-Requires: ((emacs "27.0") (cl-lib "0.5"))
;; Version: 0.0.1
;; Homepage: https://github.com/petergardfjall/emacs-postrace
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; postrace is similar in spirit to Emacs' global mark ring, but offers a more
;; intuitive interface for browsing/navigating buffer positions.  It is
;; particularly useful when examining code paths.
;;
;; postrace provides commands to push buffer positions onto a position stack and
;; navigate back and forth between these positions, with the position under the
;; cursor being previwed in the active window.  The functionality is primarily
;; offered through two interactive commands:
;;
;; - `postrace-push`: pushes the buffer position (marker) at point to the
;;   position stack.
;;
;; - `postrace-browse`: enters "browse mode".  The position stack is displayed
;;   and can be browsed in the minibuffer.  The position under the cursor is
;;   previewed in the active window.  The `postrace-browse-map` has the
;;   following default keybindings:
;;
;;   - `up`/`C-p`:   move one step up the position stack.
;;   - `down`/`C-n`: move one step down the position stack.
;;   - `RET`: jump to the position under the cursor.
;;   - `C-g`: exit the minibuffer and return to whichever buffer was active when
;;     `postrace-browse` was issued.
;;   - `M-up`/`M-p`:   move the position under the cursor one step up.
;;   - `M-down`/`M-n`: move the position under the cursor one step down.
;;   - `delete`: remove the position under the cursor from the position stack.
;;
;;; Code:

(require 'cl-lib)

;; TODO defcustom declarations
;; TODO cleanup and refactor
;; TODO test

(defgroup postrace-faces nil
  "Font-lock faces for `postrace`."
  :group 'postrace)

(defface postrace-browse-position-linum
  '((t :inherit compilation-line-number))
  "Face used for displaying line numbers when browsing the position stack."
  :group 'postrace)

(defface postrace-browse-position-bufname
  '((t :inherit compilation-info))
  "Face used for displaying buffer names when browsing the position stack."
  :group 'postrace)

(defface postrace-browse-position-preview
  '((t :inherit default))
  "Face used for displaying preview lines when browsing position stack."
  :group 'postrace)

(defface postrace-browse-selected-position
  '((t :inherit highlight))
  "Face for highlighting position under cursor when browsing the position stack."
  :group 'postrace)


(defvar postrace-browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")    'postrace-browse-select)
    (define-key map (kbd "<up>")     'postrace-browse-up)
    (define-key map (kbd "C-p")      'postrace-browse-up)
    (define-key map (kbd "<down>")   'postrace-browse-down)
    (define-key map (kbd "C-n")      'postrace-browse-down)
    (define-key map (kbd "M-<up>")   'postrace-browse-move-up)
    (define-key map (kbd "M-<down>") 'postrace-browse-move-down)
    (define-key map (kbd "<delete>") 'postrace-browse-delete)
    (define-key map (kbd "C-g")      'postrace-browse-exit)
    (define-key map (kbd "<escape>") 'postrace-browse-exit)
    map)
  "Default keybindings to use when browsing the position stack.")


;;
;; Interactive commands.
;;

(defun postrace-push ()
  "Push the position at point onto the global session stack."
  (interactive)
  ;; only allow pushing points from file-visiting buffers
  (if (not (buffer-file-name))
      (message "Won't push position. Buffer is not visiting a file.")
    (let* ((session (postrace--global))
	   (position (point-marker)))
      (postrace-session--push session position)
      (message "Position pushed onto position stack."))))


(defun postrace-browse ()
  "Enter stack navigation mode in the minibuffer.
In this mode the position stack can be browsed and the position
under the cursor is displayed in the active window.  If a
position is selected (`postrace-browse-select`) that buffer
position is displayed in the active window.  If browse mode is
aborted (`postrace-browse-exit`) the cursor is restored in all
visited buffers, and we return to the buffer from which
`postrace-browse` was called."
  (interactive)
  ;; avoid recursive postrace-browse calls.
  (when (postrace-session--browse-mode-p (postrace--global))
    (user-error "A postrace-browse session is already active"))
  ;; since we want to use the window from which we were called to visit selected
  ;; candidates (and possibly "render" the selected position) we cannot accept
  ;; being called from dedicated windows (like treemacs)
  (when (window-dedicated-p)
    (error "Cannot call postrace-browse from a dedicated window"))
  ;; Clear out positions whose buffer has been closed.
  (postrace-session--remove-positions-without-buffer (postrace--global))
  (let* ((session (postrace--global))
	 (stack (postrace-session-stack session)))
    (if (> (postrace-stack--len stack) 0)
	(progn
	  (postrace-session--clear-restore-points session)
	  ;; we'll use the active window to preview selected positions
	  (postrace-session--enter-browse-mode session (selected-window))
	  (save-excursion
	    (minibuffer-with-setup-hook
		;; add a hook to re-render the stack on user input
		(lambda () (add-hook 'post-command-hook
				     (apply-partially #'postrace--show session)
				     nil 'local))
	      (read-from-minibuffer
	       "Browse position stack:"
	       nil
	       postrace-browse-map))))
      (message "Position stack is empty."))))

(defun postrace-browse-select ()
  "Exit browse mode and move point to currently selected position."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (let* ((minibuf-win (active-minibuffer-window)))
    (when minibuf-win
      ;; trick to run code immediately after minibuffer is exited. See
      ;; https://emacs.stackexchange.com/a/20980.
      (run-at-time nil nil (lambda () (postrace-browse--goto-selected)))
      (postrace-browse-exit))))


(defun postrace-browse-exit ()
  "Exit minibuffer and restore point in all visited buffers."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (let* ((minibuf-win (active-minibuffer-window))
	 (session (postrace--global)))
    (when minibuf-win
      (postrace-session--restore-visited session)
      (postrace-session--exit-browse-mode session)
      (exit-minibuffer))))


(defun postrace-browse-move-up ()
  "Move the currently selected stack position one step up in the session stack."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (let* ((session (postrace--global))
	 (stack (postrace-session-stack session))
	 (selected-index (postrace-session-selected-index session))
	 (dest-index (- selected-index 1)))
    (when (>= dest-index 0)
      (postrace-stack--swap stack selected-index dest-index)
      (postrace-session--select session dest-index))))


(defun postrace-browse-move-down ()
  "Move the currently selected stack position one step down in the session stack."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (let* ((session (postrace--global))
	 (stack (postrace-session-stack session))
	 (selected-index (postrace-session-selected-index session))
	 (dest-index (+ selected-index 1)))
    (when (< dest-index (postrace-stack--len stack))
      (postrace-stack--swap stack selected-index dest-index)
      (postrace-session--select session dest-index))))


(defun postrace-browse--goto-selected ()
  "Go to currently selected position.
This will bring focus to the last active window (before the
minibuffer was entered at the time of `postrace-browse` and set
the currently selected position as the mark)."
  (let* ((session (postrace--global))
	 (active-win (postrace-session-browse-window session))
	 (selected-pos (postrace-session--selected session)))
      (set-window-buffer active-win (marker-buffer selected-pos))
      (select-window active-win)
      (goto-char (marker-position selected-pos))
      (recenter)))


(defun postrace-browse-down ()
  "Move the cursor down when browsing the position stack.
Increments the selected element index in the position stack."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (postrace-session--down (postrace--global)))

(defun postrace-browse-up ()
  "Move the cursor up when browsing the position stack.
Decrements the selected element index in the position stack."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (postrace-session--up (postrace--global)))


(defun postrace-browse-delete ()
  "Deletes the currently selected stack position from the session stack."
  (interactive)
  (unless (postrace-session--browse-mode-p (postrace--global))
    (user-error "No active postrace-browse session"))
  (let* ((session (postrace--global))
	 (stack (postrace-session-stack session))
	 (size (postrace-stack--len stack))
	 (selected-index (postrace-session-selected-index session)))
    (when (> size 0)
      (postrace-stack--remove-at stack selected-index)
      ;; when we remove the last position, update index.
      (when (= selected-index (- size 1))
	(let* ((last-index (- (postrace-stack--len stack) 1)))
	  (postrace-session--select session last-index)))
      ;; make sure we exit the minibuffer if we remove the last stack item
      (when (= size 1)
	(abort-recursive-edit)))))

;;
;; Position stack management
;;

(cl-defstruct postrace-stack
  "Represents a browsable stack of buffer positions."
  elements)

(defun postrace-stack--new ()
  "Create an empty postrace-stack."
  (make-postrace-stack :elements '()))

(defun postrace-stack--len (stack)
  "Gives the size of postrace-stack STACK."
  (length (postrace-stack-elements stack)))

(defun postrace-stack--last-index (stack)
  "Gives the index of the last element in postrace-stack STACK."
  (- (postrace-stack--len stack) 1))


(defun postrace-stack--get (stack index)
  "Get element at INDEX from postrace-stack STACK."
  (when (postrace-stack--index-out-of-bounds stack index)
    (error "Index out of bounds: %d (%d elements)" index (postrace-stack--len stack)))
  (nth index (postrace-stack-elements stack)))


(defun postrace-stack--push (stack element)
  "Push ELEMENT onto the (end of) postrace-stack STACK."
  (postrace-stack--insert-at stack (postrace-stack--len stack) element))


(defun postrace-stack--insert-at (stack index element)
  "Insert ELEMENT at the given INDEX of postrace-stack STACK."
  (when (or (< index 0) (> index (postrace-stack--len stack)))
    (error "Index out of bounds: %d (%d elements)" index (postrace-stack--len stack)))
  (let* ((st (postrace-stack-elements stack))
	(tail (nthcdr index st))
	(head (butlast st (- (length st) index))))
    (setf (postrace-stack-elements stack) (append head (list element) tail))))

(defun postrace-stack--remove-at (stack index)
  "Remove element at INDEX from pos-stack STACK."
  (when (postrace-stack--index-out-of-bounds stack index)
    (error "Index out of bounds: %d (%d elements)" index (postrace-stack--len stack)))
  (let* ((st (postrace-stack-elements stack))
	 (removed (nth index st))
	 (tail (nthcdr (+ index 1) st))
	 (head (butlast st (- (length st) index))))
    (setf (postrace-stack-elements stack) (append head tail))
    removed))

(defun postrace-stack--swap (stack index1 index2)
  "Swap positions of STACK elements at INDEX1 and INDEX2."
  (when (postrace-stack--index-out-of-bounds stack index1)
    (error "Stack index out of bounds: %d" index1))
  (when (postrace-stack--index-out-of-bounds stack index2)
    (error "Stack index out of bounds: %d" index2))
  (when (not (= index1 index2))
    (let* ((elem1 (postrace-stack--get stack index1))
	   (elem2 (postrace-stack--get stack index2)))
      (postrace-stack--remove-at stack index1)
      (postrace-stack--insert-at stack index1 elem2)
      (postrace-stack--remove-at stack index2)
      (postrace-stack--insert-at stack index2 elem1))))

(defun postrace-stack--pop (stack)
  "Remove the highest index element from the STACK."
  (when (= (postrace-stack--len stack) 0)
    (error "Cannot pop empty postrace-stack"))
  (postrace-stack--remove-at stack (- (postrace-stack--len stack) 1)))


(defun postrace-stack--index-out-of-bounds (stack index)
  "Test if INDEX is a valid element index in STACK."
  (or (< index 0) (> index (- (postrace-stack--len stack) 1))))


;;
;; postrace-session management
;;

(cl-defstruct postrace-session
  "Represents a postrace session for managing/browsing buffer positions."
  ;; the stack of positions
  (stack (postrace-stack--new))
  ;; the currently selected index within the stack
  (selected-index -1)

  ;; a boolean indicating if we're currently in "browse mode" (started by a call
  ;; to `postrace-browse`).
  (browse-mode nil)
  ;; tracks cursor positions of buffers visited during a `postrace-browse`
  ;; session in order to be able to restore the buffer positions when done.
  (buffer-restore-points '())
  ;; tracks the window that was active when a `postrace-browse` session started
  ;; in order to be able to return to the expected window on RET/C-g.
  (browse-window nil))


(defun postrace-session--clear (self)
  "Reset the SELF session stack to its initial state, clearing the position stack."
  (postrace-session--clear-stack self)
  (postrace-session--clear-selected self)

  (postrace-session--clear-browse-mode self)
  (postrace-session--clear-restore-points self)
  (postrace-session--clear-browse-window self))

(defun postrace-session--clear-stack (self)
  "Clear the position stack of session SELF."
  (setf (postrace-session-stack self) (postrace-stack--new)))

(defun postrace-session--clear-selected (self)
  "Clear the selected position stack index of session SELF."
  (postrace-session--select self -1))

(defun postrace-session--clear-browse-mode (self)
  "Unset the browse-mode flag for session SELF."
  (setf (postrace-session-browse-mode self) nil))

(defun postrace-session--clear-restore-points (self)
  "Clear the record of buffer restore points in session SELF."
  (setf (postrace-session-buffer-restore-points self) '()))

(defun postrace-session--clear-browse-window (self)
  "Unsets the browse-window in session SELF."
  (setf (postrace-session-browse-window self) nil))

(defun postrace-session--select (self index)
  "Set the selected stack position of session SELF to INDEX."
   (setf (postrace-session-selected-index self) index))

(defun postrace-session--push (self position)
  "Push the marker POSITION onto the position stack of session SELF."
  (let* ((stack (postrace-session-stack self)))
    (postrace-stack--push stack position)
    ;; advance current selection
    (postrace-session--select self (postrace-stack--last-index stack))))

(defun postrace-session--remove-positions-without-buffer (self)
  "Clear the position stack of session SELF from positions whose buffer has been closed."
  (let* ((stack (postrace-session-stack self))
         (start-index 0)
         (end-index (- (postrace-stack--len stack) 1)))
    (cl-loop for i from end-index downto start-index do
             ;; If the buffer does not exist, remove it from the stack.
             (unless (marker-buffer (postrace-stack--get stack i))
               (postrace-stack--remove-at stack i)
               ;; Note: ensure selected does not end up out-of-bounds.
               (postrace-session--select self 0)))))

(defun postrace-session--enter-browse-mode (self window)
  "Enter 'browse mode' for session SELF, using WINDOW as the active browse window."
  (setf (postrace-session-browse-mode self) t)
  (setf (postrace-session-browse-window self) window))


(defun postrace-session--exit-browse-mode (self)
  "Exit 'browse mode' for session SELF."
  (postrace-session--clear-browse-mode self))


(defun postrace-session--browse-mode-p (self)
  "Return t if session SELF is currently in 'browse mode', otherwise return nil.

A session is in browse mode while the minibuffer is active after
a call to `postrace-browse`, but before `postrace-browse-select`
or `postrace-browse-exit` has been called."
   (postrace-session-browse-mode self))


(defun postrace-session--up (self)
  "Move selected index up the position stack for session SELF."
  (let* ((stack (postrace-session-stack self))
	 (current (postrace-session-selected-index self))
	 (prev (- current 1)))
    (when (> current 0)
      (setf (postrace-session-selected-index self) prev))))

(defun postrace-session--down (self)
  ;; TODO semantics isn't quite right as this moves up the stack?
  "Move selected index down the position stack for session SELF."
  (let* ((stack (postrace-session-stack self))
	 (current (postrace-session-selected-index self))
	 (next (+ current 1)))
    (when (< current (postrace-stack--last-index stack))
      (setf (postrace-session-selected-index self) next))))

(defun postrace-session--selected (self)
  "Get the currently selected marker position in the position stack from session SELF."
  (let* ((current-index (postrace-session-selected-index self))
	 (stack (postrace-session-stack self)))
    (postrace-stack--get stack current-index)))


(defun postrace-session--display-range (self)
  "Calculate what range of position stack items to display for session SELF based on the stack size and the maximum size of the minibuffer window."
  (let* ((stack (postrace-session-stack self))
	 (size (postrace-stack--len stack))
	 (selected-index (postrace-session-selected-index self))
	 ;; Maximum candidates that can fit in minibuffer.
	 ;; Note: subtract one line for prompt.
	 (max-candidates (1- (truncate (max-mini-window-lines)))))
    (if (<= (postrace-stack--len stack) max-candidates)
	;; all stack items fit minibuffer
	(cons 0 (postrace-stack--last-index stack))
      ;; we can only fit a subset of stack positions in the minibuffer
      (let* ((start (- selected-index (/ max-candidates 2)))
	     (end   (+ start (1- max-candidates)))
	     ;; shift range into place if it ends up outside stack index range
	     (shift (cond
		     ((< start 0) (- start))
		     ((>= end size) (- (1+ (- end size))))
		     (t 0)))
	     (start (+ start shift))
	     (end   (+ end shift)))
	(cons start end)))))

(defun postrace-session--format (self &optional end-column)
  "Produce a string representation of the SELF session position stack.
Lines longer than END-COLUMN are truncated.  END-COLUMN defaults to 80."
  (let* ((stack (postrace-session-stack self))
	 (size (postrace-stack--len stack))
	 (display-range (postrace-session--display-range self))
	 (start-index (car display-range))
	 (end-index (cdr display-range))
	 (end-column (if end-column end-column 80)))
    (if (> size 0)
	(mapconcat
	 'identity
	 (cl-loop for i from start-index to end-index collect
		  (truncate-string-to-width
		   (postrace-session--format-element self i)
		   end-column))
	 "\n")
      "")))

(defun postrace-session--format-element (self index)
  "Procuce a string representation for position at stack INDEX in session SELF."
  ;; TODO: validate
  (let* ((stack (postrace-session-stack self))
	 (mark (postrace-stack--get stack index))
	 (mark-buf (marker-buffer mark))
	 (mark-pos (marker-position mark))
	 ;; highlight selected index.
	 (is-selected (= index (postrace-session-selected-index self)))
	 (line (format "%s:%s %s"
		       (propertize (buffer-name mark-buf) 'face '(postrace-browse-position-bufname))
		       (propertize (number-to-string (postrace--buffer-linenum mark-buf mark-pos)) 'face '(postrace-browse-position-linum))
		       (postrace--buffer-line mark-buf mark-pos) 'face '(postrace-browse-position-preview))))
    (when is-selected
	(add-face-text-property 0 (length line) 'postrace-browse-selected-position t line))
    line))

(defun postrace--buffer-line (mark-buf mark-pos)
  "Get the line from buffer MARK-BUF containing  containing position MARK-POS."
  (with-current-buffer mark-buf
    (save-mark-and-excursion
      (goto-char mark-pos)
      (string-trim-right (thing-at-point 'line)))))

(defun postrace--buffer-linenum (mark-buf mark-pos)
  "Get the line number in buffer MARK-BUF for position MARK-POS."
  (with-current-buffer mark-buf
    (save-mark-and-excursion
      (goto-char mark-pos)
      (line-number-at-pos))))


(defun postrace--show (session)
  "(Re)renders the position stack of SESSION in the minibuffer."
  (let* ((minibuf-win (active-minibuffer-window))
	 (active-win (previous-window))
	 (stack (postrace-session-stack session))
	 (selected-index (postrace-session-selected-index session))
	 (selected-pos (postrace-stack--get stack selected-index)))
    (when minibuf-win
      (with-selected-window minibuf-win
	;; clear minibuffer contents after prompt
	(delete-region (line-end-position) (point-max))
	;; render position stack again
	(insert (concat "\n" (postrace-session--format session (window-body-width))))
	(goto-char (minibuffer-prompt-end))
	;; switch buffer in active window to selected position
	;; record a buffer-restore-position for the buffer
	(postrace-session--record-restore-point session (marker-buffer selected-pos))
	(set-window-buffer active-win (marker-buffer selected-pos))
	(with-selected-window active-win
	  (goto-char (marker-position selected-pos))
	  (recenter))
	))))

(defun postrace-session--record-restore-point (self buffer)
  "Record point in BUFFER as a restore point for session SELF.
This allows us to restore BUFFER to its pre-visit state after
running `postrace-browse`."
  (let* ((buf-name (buffer-name buffer))
	 (restore-points-alist (postrace-session-buffer-restore-points self)))
    ;; if we haven't already recored a restore point for this buffer, add buffer
    ;; point to our restore points.
    (unless (assoc buf-name restore-points-alist)
      (with-current-buffer buffer
	(setf (postrace-session-buffer-restore-points self)
	      (cons (cons buf-name (point-marker)) restore-points-alist))))))

(defun postrace-session--restore-visited (self)
  "Restore point in all of session SELF's recorded visited buffers to its pre-visit position."
  (let* ((restore-points (postrace-session-buffer-restore-points self))
	 (active-win (previous-window)))
    (cl-loop for point in restore-points collect
	     (let* ((pt (cdr point)))
	       (set-window-buffer active-win (marker-buffer pt))
	       (select-window active-win)
	       (with-current-buffer (marker-buffer pt)
		 (goto-char (marker-position pt)))))))

;;
;; The global postrace-session.
;;

(defvar postrace--global-session (make-postrace-session)
  "The global postrace session.")

(defun postrace--global ()
  "Get the global postrace-session or create a new one if none exists."
  (unless postrace--global-session
    (setq postrace--global-session (make-postrace-session)))
  postrace--global-session)


(provide 'postrace)

;;; postrace.el ends here

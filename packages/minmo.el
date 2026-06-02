;; minmo.el --- minimal mode-line -*- lexical-binding: t; -*-

;; my mode line is extremely minimal. However there are a few things I want:
;;
;; buffer-name
;; git and disk status
;; project-name
;; git branch
;; major-mode
;; minor-mode very selectively
;; narrow indicator
;; line:col
;; lines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-name

(defun minmo-buffer-name ()
  (if (mode-line-window-selected-p)
      (propertize (buffer-name) 'face 'success)
    (buffer-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git and disk status

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unified symbol set for git and disk status on tty and pts
;;
;;                           disk  git
;; unmodified         : ◻  : grey  grey
;; --
;; modified/staged    : ◱  : red   orange
;; readonly/ignored   : ◳  : green green
;; --
;; new                : ◰  :       blue
;; nofile/untracked   : ◲  : blue  red

;; left                 ◱◰ : staged/new        : indexed
;; right                ◳◲ : ignored/untracked : unindexed
;; top                  ◰◳ : new/ignored       : clean
;; bottom               ◱◲ : staged/untracked  : dirty

;; NOTE: "clean" is not quite right, since a newly added file, staged, is still
;; a dirty repo. but vc-state doesn't tell us whether a modified file is staged
;; or unstaged.

(defconst minmo-status-alist
  '(
    (unmodified . (((unicode . "◻") (ascii . ".")) . ((disk . nil) (git . nil))))
    (modified/staged . (((unicode . "◱") (ascii . "*")) . ((disk . error) (git . warning))))
    (readonly/ignored . (((unicode . "◳") (ascii . "_")) . ((disk . success) (git . success))))
    (new . (((unicode . "◰") (ascii . "+")) . ((disk . link) (git . link))))
    (nofile/untracked . (((unicode . "◲") (ascii . "!")) . ((disk . link) (git . error))))
    )
  "Unified symbol set for git and disk status with unicode and ascii variants
  and their respective faces.")

(defun minmo--status-string-face (status fstype)
  (let* (
         (encoding (if (string-prefix-p "xterm" (tty-type)) 'unicode 'ascii))
         (row (cdr (assoc status minmo-status-alist)))
         (str (cdr (assoc encoding (car row))))
         (face (cdr (assoc fstype (cdr row))))
         )
    (cons str face)))

(defun minmo--status (status fstype)
  "returns a propertized string with the associated face, given the STATUS and
filesystem FSTYPE, 'git or 'disk."
  (let ((pair (minmo--status-string-face status fstype)))
    (propertize (car pair) 'face (cdr pair))
    ))

(defun minmo--file-exists-locally-p ()
  "Utility predicate to prevent expensive VC checks remotely."
  (and buffer-file-name (not (file-remote-p (buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; branch

(defvar-local minmo--vc-branch-cache nil
  "Cached mode-line string for git branch.")

(defun minmo--fetch-vc-branch (file)
  "Get the branch from vc state cache if available, otherwise call git."
  (concat ":" (or (vc-git--symbolic-ref file)
                  ;; first 7 of commit hash:
                  (substring (vc-git-working-revision file) 0 7)
                  "?")))

(defun minmo-branch () minmo--vc-branch-cache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; status

(defvar-local minmo--vc-status-cache nil
  "Cached mode-line string for git file status.")

(defun minmo-vc-status ()
  (or minmo--vc-status-cache " "))

(defun minmo--update-vc-cache ()
  "Call git and cache the mode-line string."
  (when (and (minmo--file-exists-locally-p)
             ;; NOTE: this is cached via vc-backend and vc-file-prop-obarray,
             ;; so it's safe as a guardrail:
             (eq (vc-responsible-backend buffer-file-name t) 'Git))

    ;; NOTE: this is cached via vc-file-prop-obarray
    (setq minmo--vc-branch-cache (minmo--fetch-vc-branch buffer-file-name))
    ;; NOTE: this is not cached and will run git:
    (let ((status (vc-git-state buffer-file-name)))
      (setq minmo--vc-status-cache
            (concat " " (pcase status
                          ('up-to-date   (minmo--status 'unmodified 'git))
                          ('edited       (minmo--status 'modified/staged 'git))
                          ('added        (minmo--status 'new 'git))
                          ('needs-merge  (minmo--status 'modified/staged 'git))
                          ('conflict     (minmo--status 'modified/staged 'git))
                          ('unregistered (minmo--status 'nofile/untracked 'git))
                          ('ignored      (minmo--status 'readonly/ignored 'git))
                          (_             (minmo--status 'unmodified 'git))))))))

;; NOTE: update the cache with file changes
(add-hook 'find-file-hook #'minmo--update-vc-cache)
(add-hook 'after-save-hook #'minmo--update-vc-cache)
(add-hook 'after-revert-hook #'minmo--update-vc-cache)

;; and when window state changes:
(defun minmo--update-vc-cache-window-status (frame-or-window)
  (let ((win (if (framep frame-or-window)
                 (frame-selected-window frame-or-window)
               frame-or-window)))
    (with-current-buffer (window-buffer win)
      (minmo--update-vc-cache))))

;; NOTE: window-state-change-functions includes size changes, which would be spammy.
;; this does selection changes, eg other-window:
(add-hook 'window-selection-change-functions #'minmo--update-vc-cache-window-status)
;; this does buffer changes, eg switch-to-buffer:
(add-hook 'window-buffer-change-functions #'minmo--update-vc-cache-window-status)

;; this is set by vc-hooks.el, which is loaded with `vc'
;; we still want this, because vc-responsible-backend and vc-git--symbolic-ref
;; use the cache set by this function:
;; (remove-hook 'find-file-hook #'vc-refresh-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; timer

;; NOTE: the idea here is to keep the timer like vc-mode uses when
;; auto-revert-check-vc-info is t, but restrict to visible windows. Which is
;; somewhat an obvious optimization.

(defcustom minmo-vc-cache-timer-interval 5
  "interval for `minmo-vc-cache-timer'")

(defvar minmo-vc-cache-timer nil)

(defun minmo--vc-update-cache-visible ()
  "Update vc cache for visible windows."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (minmo--update-vc-cache))))

;; guard against spawning multiple when this file is eval'd :
(when minmo-vc-cache-timer (cancel-timer minmo-vc-cache-timer))
(setq minmo-vc-cache-timer
      ;; TODO: run-with-idle-timer ?
      (run-with-timer
       minmo-vc-cache-timer-interval
       minmo-vc-cache-timer-interval
       'minmo--vc-update-cache-visible))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; disk

(defun minmo-disk-status ()
  (cond
   ((and buffer-file-name (buffer-modified-p))
    (minmo--status 'modified/staged 'disk))
   ;; for buffers with no file, like *scratch*:
   ((and (not buffer-file-name) (buffer-modified-p))
    (minmo--status 'nofile/untracked 'disk))
   (buffer-read-only
    (minmo--status 'readonly/ignored 'disk))
   (t (minmo--status 'unmodified 'disk))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; project

;; project-current has caching, but this extends it further: get the
;; project-name once when the file is opened.
;; running up directories looking for .git can be bad.
(defvar-local minmo--project-cache nil)

(defun minmo--cache-project ()
  "Cache the project name to prevent disk I/O during redisplay."
  (setq minmo--project-cache
        ;; don't show project for help buffers, remote files, etc:
        (when-let* (((minmo--file-exists-locally-p))
                    ;; NOTE: taken from project-mode-line-format
                    (last-coding-system-used last-coding-system-used)
                    ;; NOTE: project-current calls project--get-cached, which uses
                    ;; project-vc-non-essential-cache-timeout when non-essential is t
                    (non-essential t)
                    (project (project-current)))
          (concat " " (project-name project)))
        ))

(defun minmo-project () minmo--project-cache)

(add-hook 'find-file-hook #'minmo--cache-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; major-mode

;; current solution is to throw out a few redundant, obvious modes.
;; NOTE: using major-mode directly, rather than mode-name, because
;; the "pretty print" format is annoying.
(defun minmo-major-mode ()
  (unless (member major-mode '(
                               um-mode
                               markdown-mode
                               emacs-lisp-mode
                               ))
    ;; major-mode is first evaluated, then the symbol-name of
    ;; the return value is fetched as string:
    (concat " " (symbol-name major-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input-method

(defun minmo-input-method ()
  (when current-input-method-title
    (concat (propertize current-input-method-title 'face 'warning) " | ")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line column

;; line count can be expensive for large files, when run continuously. cache it.
(defvar-local minmo--total-lines-cache nil
  "Cached total line count for the current buffer.
Updates only on file load and save to guarantee zero redisplay lag.")

(defun minmo--cache-total-lines ()
  "Refresh the total line count cache."
  ;; 'line-number-at-pos' is C-level:
  (setq minmo--total-lines-cache (line-number-at-pos (point-max))))

(add-hook 'find-file-hook #'minmo--cache-total-lines)
(add-hook 'after-save-hook #'minmo--cache-total-lines)
(add-hook 'after-revert-hook #'minmo--cache-total-lines)

;; because when narrow is on line count is meaningless:
(defun minmo-narrow-or-linecol-total ()
  (when buffer-file-name
    (if (buffer-narrowed-p)
        (propertize "%n" 'face 'warning)
      ;; consult preview won't have filled out minmo--total-lines-cache:
      (when minmo--total-lines-cache
        (concat "%l:%c " (number-to-string minmo--total-lines-cache)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minor-modes

;; NOTE: customization via mode-line-minor-modes and
;; mode-line-collapse-minor-modes is almost right, but the strings still come
;; from minor-mode-alist, which I don't care for.
;; And rather than define what not to eliminate, I only define what I want and
;; check for t.
(defvar minmo-minor-modes-to-show
  '(
    view-mode
    outline-minor-mode
    olivetti-mode
    eglot--managed-mode
    )
  "minor modes to show in the mode-line")

(defvar minmo-minor-modes-face 'font-lock-keyword-face)

(defun minmo-minor-modes ()
  (concat (string-join
           (delq nil (mapcar (lambda (m)
                               ;; NOTE: bound-and-true-p does not work here, because
                               ;; it's a macro which doesn't evaluate its arguments,
                               ;; whereas boundp and symbol-value are functions which resolve
                               ;; the local m var first:
                               (when (and (boundp m) (symbol-value m))
                                 ;; don't use the :lighter from minor-mode-alist, just strip the end:
                                 (propertize (string-trim-right (symbol-name m) "-minor-mode\\|-mode\\|--managed-mode")
                                             'face minmo-minor-modes-face)))
                             minmo-minor-modes-to-show))
           " ") " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; truncate

(defun minmo-truncate ()
  "Truncate the `mode-line-format' starting from the
`mode-line-format-right-align' for this buffer."
  (setq-local
   mode-line-format
   (butlast mode-line-format (length (memq 'mode-line-format-right-align mode-line-format)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-line-format

;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
(setq-default
 mode-line-format
 (list

  ;;;;;;;;;;;;;
  ;; (buffer-name)
  '(:eval (minmo-buffer-name))

  ;;;;;;;;;;;;;
  ;; status
  '(:eval (minmo-vc-status))
  '(:eval (minmo-disk-status))

  ;;;;;;;;;;;;;
  ;; project
  '(:eval (minmo-project))

  ;;;;;;;;;;;;;
  ;; branch
  '(:eval (minmo-branch))

  ;;;;;;;;;;;;;
  ;; major-mode
  '(:eval (minmo-major-mode))

  ;;;;;;;;;;;;;
  ;; everything after will be right-aligned:
  'mode-line-format-right-align

  ;;;;;;;;;;;;;
  ;; minor modes
  '(:eval (minmo-minor-modes))

  ;;;;;;;;;;;;;
  ;; keycast
  ;; I use this dummy symbol because my modeline doesn't have the expected symbol.
  ;; `keycast' inserts itself after this point:
  'keycast-mode-line-identifier

  ;;;;;;;;;;;;;
  ;; input-method
  '(:eval (minmo-input-method))

  ;;;;;;;;;;;;;
  ;; narrowing notifier or line:col total
  '(:eval (minmo-narrow-or-linecol-total))

  ;; when the window is split, the line seems to eat the rightmost edge:
  " "
  ))

(provide 'minmo)

;; Local Variables:
;; outline-regexp: ";;;+ [^;]+"
;; eval: (outline-minor-mode 1)
;; End:

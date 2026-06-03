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

;; NOTE: this has been cleansed of vc-mode dependencies, despite the fact that
;; it might seem better practice to use builtin libraries. But `vc' tries much
;; too hard to be compatible with obsolete version control systems no one uses,
;; and as a result limits its functionality and performance. We all use git, so
;; just consolidate.
;;
;; Unforunately the `project' code uses the same code under the hood, so this
;; has also been stripped out in favor of just storing the directory containing
;; the ".git" folder, which we're checking for anyway.

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
;;                        disk    : git
;; ----------------------------------------
;; unmodified       : ◻ : shadow  : shadow
;; modified         : ◱ : error   : warning
;; readonly/ignored : ◳ : success : success
;; staged           : ◰ : warning : link
;; nofile/untracked : ◲ : link    : error

;; quadrant semantics:
;;
;; left              ◱◰ : modified/staged    : indexed
;; right             ◳◲ : ignored/untracked  : unindexed
;; top               ◰◳ : staged/ignored     : ready
;; bottom            ◱◲ : modified/untracked : unready

(defconst minmo-status-alist
  '(
    (unmodified . (((unicode . "◻") (ascii . ".")) . ((disk . nil) (git . nil))))
    (modified . (((unicode . "◱") (ascii . "*")) . ((disk . error) (git . warning))))
    (readonly/ignored . (((unicode . "◳") (ascii . "_")) . ((disk . success) (git . success))))
    (staged . (((unicode . "◰") (ascii . "+")) . ((disk . warning) (git . link))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; branch

(defcustom minmo-git-branch-prefix ":" "prefix for the branch string.")

(defvar-local minmo--git-branch-cache nil
  "Cached mode-line string for git branch.")

(defun minmo--git-branch (file)
  "Return the branch name or detached HEAD hash."
  (let* ((default-directory (file-name-directory file))
         (branch (with-temp-buffer
                   (when (eq 0 (call-process "git" nil t nil "branch" "--show-current"))
                     (string-trim (buffer-string))))))
    (concat minmo-git-branch-prefix
            (if (and branch (not (string-empty-p branch))) branch
              ;; fallback to short hash when HEAD is detached:
              (with-temp-buffer
                (if (eq 0 (call-process "git" nil t nil "rev-parse" "--short" "HEAD"))
                    (string-trim (buffer-string))
                  "?"))))))

(defun minmo-git-branch () minmo--git-branch-cache)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; status

(defcustom minmo-git-status-prefix " " "prefix for the git status string.")

(defvar-local minmo--git-status-cache nil
  "Cached mode-line string for git file status.")

(defun minmo-git-status ()
  (or minmo--git-status-cache " "))

(defvar minmo--git-directory-table (make-hash-table :test 'equal)
  "Store known directories with and without a ruling .git")

(defun minmo--find-git (file &optional force)
  "Walk up the directory tree looking for `.git'. Returns the path or nil.
Optional FORCE means ignore the minmo--git-directory-table."
  (let* ((dir (file-name-directory file))
         (hash (gethash dir minmo--git-directory-table)))
    (when (or force (not hash))
      ;; NOTE: store 'ignore for files not under .git, so these calls are also optimized:
      (setq hash (puthash dir (or
                               (locate-dominating-file file ".git")
                               'ignore)
                          minmo--git-directory-table)))
    ;; but return nil when 'ignore, so that this function serves as a guard:
    (if (eq hash 'ignore) nil hash)))

(defun minmo--file-exists-locally-p ()
  "Utility predicate to prevent expensive VC checks remotely."
  (and buffer-file-name (not (file-remote-p (buffer-file-name)))))

(defun minmo--git-status-short (file)
  "Return the 2-character git status for FILE. Returns a string in all cases."
  (with-temp-buffer
    (if-let* ((default-directory (file-name-directory file))
              (_ (eq 0 (call-process "git" nil t nil "status" "--porcelain"
                                     "--ignored" "--" (file-name-nondirectory file))))
              (_ (>= (buffer-size) 2)))
        (substring (buffer-string) 0 2)
      "  ")))

(defun minmo--git-status (file)
  "Return the minmo status string according to `minmo-status-alist'."
  (let* ((status (minmo--git-status-short file))
         (char-index (aref status 0))
         (char-work  (aref status 1)))
    (concat minmo-git-status-prefix
            (cond
             ((string= status "  ") (minmo--status 'unmodified 'git))
             ((string= status "!!") (minmo--status 'readonly/ignored 'git))
             ((string= status "??") (minmo--status 'nofile/untracked 'git))
             ;; report modified whether partially staged or not:
             ((eq char-work ?M) (minmo--status 'modified 'git))
             ((memq char-index '(?M ?A)) (minmo--status 'staged 'git))
             (t (minmo--status 'unmodified 'git))))))

(defun minmo--update-git-cache (&optional force)
  "Call git and cache the mode-line string."
  (when (minmo--file-exists-locally-p)
    (let ((old-status minmo--git-status-cache)
          (old-branch minmo--git-branch-cache)
          (git (minmo--find-git buffer-file-name force)))

      (if git
          (progn
            (setq minmo--git-status-cache (minmo--git-status buffer-file-name))
            (setq minmo--git-branch-cache (minmo--git-branch buffer-file-name)))
        ;; NOTE: not in a git repo: clear the caches if something changed:
        (setq minmo--git-branch-cache nil)
        (setq minmo--git-status-cache nil))
      ;; NOTE: redraw only when changed:
      (when (or (not (string= old-status minmo--git-status-cache))
                (not (string= old-branch minmo--git-branch-cache)))
        (force-mode-line-update)))))

(defun minmo--update-git-cache-force () (minmo--update-git-cache t))

;; NOTE: force update minmo--git-directory-table upon open:
(add-hook 'find-file-hook #'minmo--update-git-cache-force)
(add-hook 'after-revert-hook #'minmo--update-git-cache-force)
;; but not for a save because this is frequent:
(add-hook 'after-save-hook #'minmo--update-git-cache)

;; NOTE: but normal cache update when window state changes:
(defun minmo--update-git-cache-window-status (frame-or-window)
  (let ((win (if (framep frame-or-window)
                 (frame-selected-window frame-or-window)
               frame-or-window)))
    (with-current-buffer (window-buffer win)
      (minmo--update-git-cache))))

;; NOTE: window-state-change-functions includes size changes, which would be spammy.
;; this does selection changes, eg other-window:
(add-hook 'window-selection-change-functions #'minmo--update-git-cache-window-status)
;; this does buffer changes, eg switch-to-buffer:
(add-hook 'window-buffer-change-functions #'minmo--update-git-cache-window-status)

;; this is set by vc-hooks.el, which is loaded with `vc'
;; TODO: remove?
;; if we don't use vc-responsible-backend and vc-git--symbolic-ref
;; (remove-hook 'find-file-hook #'vc-refresh-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; timer

;; NOTE: the idea here is to keep the timer like vc-mode uses when
;; auto-revert-check-vc-info is t, but restrict to visible windows. Which is
;; somewhat an obvious optimization.

(defvar minmo-git-cache-timer nil)

(defun minmo--update-git-cache-visible ()
  "Update vc cache for visible windows."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (minmo--update-git-cache))))

(defun minmo--start-timer (symbol value)
  "Apply the new timer VALUE to SYMBOL and restart the timer."
  ;; explicitly set the variable, otherwise :set swallows it
  (set-default symbol value)
  ;; guard against spawning multiple:
  (when minmo-git-cache-timer
    (cancel-timer minmo-git-cache-timer)
    (setq minmo-git-cache-timer nil))
  ;; allow disabling:
  (when (and (numberp value) (> value 0))
    (setq minmo-git-cache-timer
          (run-with-timer value value #'minmo--update-git-cache-visible))))

(defcustom minmo-git-cache-timer-interval 5
  "Interval in seconds for background git caching. Set to nil or 0 to disable."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disable" nil))
  :set #'minmo--start-timer)

(minmo--start-timer 'minmo-git-cache-timer-interval
                    minmo-git-cache-timer-interval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; disk

(defun minmo-disk-status ()
  (cond
   ;; readonly:
   (buffer-read-only
    (minmo--status 'readonly/ignored 'disk))
   ;; no file but modified:
   ((and (not buffer-file-name) (buffer-modified-p))
    (minmo--status 'nofile/untracked 'disk))
   ;; has a path but was never visited: new unsaved file
   ;; NOTE: visited-file-modtime involves no disk read:
   ((and buffer-file-name (eq -1 (visited-file-modtime)))
    (minmo--status 'staged 'disk))
   ;; normally modified:
   ((and buffer-file-name (buffer-modified-p))
    (minmo--status 'modified 'disk))
   ;; unmodified (fallback):
   (t (minmo--status 'unmodified 'disk))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; project

(defcustom minmo-project-prefix " " "prefix for the project notifier.")

(defvar-local minmo--project-cache nil)

(defun minmo--cache-project ()
  "Cache the project name to prevent disk I/O during redisplay."
  (setq minmo--project-cache
        ;; don't show project for help buffers, remote files, etc:
        (when-let* ((_ (minmo--file-exists-locally-p))
                    ;; NOTE: don't rely on project-current anymore, since it
                    ;; actually uses the same old vc-mode logic I wanted to
                    ;; avoid.
                    (git (minmo--find-git buffer-file-name)))
          (concat minmo-project-prefix
                  (file-name-nondirectory (directory-file-name git))))
        ))

(defun minmo-project () minmo--project-cache)

(add-hook 'find-file-hook #'minmo--cache-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; major-mode

(defcustom minmo-major-mode-prefix " " "prefix for the major mode notifier.")

(defcustom minmo-major-modes-to-ignore '(emacs-lisp-mode markdown-mode um-mode)
  "list of major-modes to ignore")

;; current solution is to throw out a few redundant, obvious modes.
;; NOTE: using major-mode directly, rather than mode-name, because
;; the "pretty print" format is annoying.
(defun minmo-major-mode ()
  (unless (member major-mode minmo-major-modes-to-ignore)
    (concat minmo-major-mode-prefix
            ;; major-mode is first evaluated, then the symbol-name of
            ;; the return value is fetched as string:
            (symbol-name major-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; input-method

(defcustom minmo-input-method-suffix " | "
  "suffix for the input method notifier.")

(defun minmo-input-method ()
  (when current-input-method-title
    (concat (propertize current-input-method-title 'face 'warning) minmo-input-method-suffix)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line column

(defcustom minmo-line-column-format "%l:%c" "format for line number and column.")

(defcustom minmo-line-column-suffix " " "suffix for the line number and column.")

;; line count can be expensive for large files, when run continuously. cache it.
(defvar-local minmo--total-lines-cache nil
  "Cached total line count for the current buffer.")

(defun minmo--cache-total-lines ()
  "Refresh the total line count cache."
  ;; 'line-number-at-pos' is C-level:
  (setq minmo--total-lines-cache (line-number-at-pos (point-max))))

(add-hook 'find-file-hook #'minmo--cache-total-lines)
(add-hook 'after-save-hook #'minmo--cache-total-lines)
(add-hook 'after-revert-hook #'minmo--cache-total-lines)

;; because when narrow is on, line count is meaningless:
(defun minmo-narrow-or-linecol-total ()
  (when buffer-file-name
    (if (buffer-narrowed-p)
        (propertize "%n" 'face 'warning)
      ;; consult preview won't have filled out minmo--total-lines-cache:
      (when minmo--total-lines-cache
        (concat minmo-line-column-format
                minmo-line-column-suffix
                (number-to-string minmo--total-lines-cache)))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minor-modes

;; NOTE: customization via mode-line-minor-modes and
;; mode-line-collapse-minor-modes is almost right, but the strings still come
;; from minor-mode-alist, which I don't care for.
;; And rather than define what not to eliminate, I only define what I want and
;; check for t.
(defcustom minmo-minor-modes-to-show
  '(
    view-mode
    outline-minor-mode
    olivetti-mode
    eglot--managed-mode
    )
  "minor modes to show.")

(defcustom minmo-minor-modes-face 'font-lock-keyword-face "face for minor modes.")

(defcustom minmo-minor-modes-separator " " "separator for the minor modes list.")

(defcustom minmo-minor-modes-suffix " " "suffix for the minor modes list.")

(defcustom minmo-minor-modes-strip-suffix-regexp "-minor-mode\\|-mode\\|--managed-mode"
  "regexp of suffixes from minor modes to strip out.")

(defun minmo-minor-modes ()
  (concat (string-join
           (delq nil (mapcar (lambda (m)
                               ;; NOTE: bound-and-true-p does not work here, because
                               ;; it's a macro which doesn't evaluate its arguments,
                               ;; whereas boundp and symbol-value are functions which resolve
                               ;; the local m var first:
                               (when (and (boundp m) (symbol-value m))
                                 ;; don't use the :lighter from minor-mode-alist, just strip the end:
                                 (propertize (string-trim-right
                                              (symbol-name m)
                                              minmo-minor-modes-strip-suffix-regexp)
                                             'face minmo-minor-modes-face)))
                             minmo-minor-modes-to-show))
           minmo-minor-modes-separator)
          minmo-minor-modes-suffix))

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
  '(:eval (minmo-git-status))
  '(:eval (minmo-disk-status))

  ;;;;;;;;;;;;;
  ;; project
  '(:eval (minmo-project))

  ;;;;;;;;;;;;;
  ;; branch
  '(:eval (minmo-git-branch))

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

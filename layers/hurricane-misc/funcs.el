(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; @See: https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; Force update evil keymaps after git-timemachine-mode loaded.
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

(defun hurricane/insert-semicolon-at-the-end-of-this-line ()
  "Insert `;' at the end of current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun hurricane/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun hurricane/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun hurricane/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun hurricane/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "hurricane")))

(defun hurricane/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "hurricane")))

;; {{
;; @See: http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;; @See: http://wikemacs.org/wiki/Shell#Search_the_bash.2C_zsh_or_fish_history_with_Ivy-mode
(defun hurricane/counsel-yank-bash-history ()
  "Yank the `zsh' history."
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (setq collection (mapcar (lambda (item) (replace-regexp-in-string ".*;" "" item)) collection))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Zsh history:") collection))))
      ;; (setq val (replace-regexp-in-string "^:[^;]*;" "" val))
      ;; (setq val (replace-regexp-in-string ".*;" "" val))
      (kill-new val)
      (message "%s => kill-ring" val))))
;; }}

(defun hurricane//indent-region(spaces)
  (progn
    ;; Default to start and end of current line.
    (setq start (line-beginning-position))
    (setq end (line-end-position))

    ;; If there's a selection, use that instead of the current line.
    (when (use-region-p)
      (setq start (region-beginning))
      (setq end (region-end))
      )

    (save-excursion                          ;; Restore the position afterwards.
      (goto-char start)                      ;; Go to the start of region.
      (setq start (line-beginning-position)) ;; Save the start of the line.
      (goto-char end)                        ;; Go to the end of region.
      (setq end (line-end-position))         ;; Save the end of the line.

      (indent-rigidly start end spaces)      ;; Indent between start and end.
      (setq deactivate-mark nil)             ;; Restore the selected region.
      )))

(defun hurricane/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (hurricane//indent-region 4)            ;; Region was selected, call indent-region.
    (insert "    ")))                        ;; Else insert four spaces as expected.

(defun hurricane/untab-region (N)
  (interactive "p")
  (hurricane//indent-region -4))

(defun hurricane/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'hurricane/tab-region)
  (local-set-key (kbd "<S-tab>") 'hurricane/untab-region))

;; I'm don't like this settings too much.
;; (add-hook 'prog-mode-hook 'hurricane/hack-tab-key)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun hurricane//unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;; @See: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun hurricane//git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; Re-shape list for the ivy-read.
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 3 rev) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :unwind #'hurricane//unwind-git-timemachine
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun hurricane/git-timemachine ()
  "Open `git snapshot' with the selected version.
Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'hurricane//git-timemachine-show-selected-revision))

(defun hurricane/helm-hotspots ()
  "Helm interface to my hotspots, which includes my locations,
org-files and bookmarks."
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(hurricane//hotspots-sources))))

(defun hurricane//hotspots-sources ()
  "Construct the helm sources for my hotspots."
  `((name . "Mail and News")
    (candidates . (
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (hurricane/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defun hurricane/now ()
  "Insert string for the current time formatted like `2:34 PM'."
  ;; Permit invocation in minibuffer.
  (interactive)
  (insert (format-time-string "%D %-I:%M %p")))

(defun hurricane/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. `Sunday, September 17, 2000'."
  ;; Permit invocation in minibuffer.
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

;; @See: https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
(defun hurricane//browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "
  tell application \"Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))

(defun hurricane/goto-match-paren (arg)
  "Go to the matching  if on `(){}[]', similar to vi style of `%'."
  (interactive "p")
  ;; First, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; Now, try to succeed from inside of a bracket.
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun hurricane/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun hurricane/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun hurricane//retrieve-chrome-current-tab-url ()
  "Get the `URL' of the active tab of the first window."
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "    set theUrl to get URL of active tab of first window\n"
                  "    set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

(defun hurricane/insert-chrome-current-tab-url ()
  "Insert the `URL' of the active tab of the first window."
  (interactive)
  (insert (hurricane//retrieve-chrome-current-tab-url)))

(defun hurricane/copy-chrome-current-tab-url ()
  (interactive)
  (kill-new (hurricane//retrieve-chrome-current-tab-url)))

;; Remove all the duplicated emplies in current buffer.
(defun hurricane/single-lines-only ()
  "Replace multiple blank lines with a single one."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; For running long run ansi-term.
(defun hurricane/named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))

(defun hurricane/ash-term-hooks ()
  ;; Dabbrev-expand in term.
  (define-key term-raw-escape-map "/"
              (lambda ()
                (interactive)
                (let ((beg (point)))
                  (dabbrev-expand nil)
                  (kill-region beg (point)))
                (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; Yank in term (bound to C-c C-y).
  (define-key term-raw-escape-map "\C-y"
              (lambda ()
                (interactive)
                (term-send-raw-string (current-kill 0)))))

(defun hurricane/terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'hurricane/terminal)

;; {{
;; @See: http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
;; Add count for chinese, mainly used for writing chinese blog post.
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))

(defvar wc-regexp-chinese-punc
  "[.,!?,:「」『』(),[]《》〈〉※—]")

(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun hurricane/word-count-for-chinese ()
  "比较精确地统计中/日/英文字数.
- 文章中的注解不算在字数内.
- 平假名与片假名亦包含在 `中日字数' 内, 每个平/片假名都算单独一个字 (但片假名不含连音 `-').
- 英文只计算 `单子数', 不含标点.
- 韩文不包含在内."
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ;; 去掉 org 文件的 OPTIONS (以#+开头).
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
         ;; 把注释行删掉 (不把注释算进字数内).
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文 (含标点, 片假名).
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文标点符号.
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字数 (不含标点).
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字数 (不含标点): %s
中日文字数 (包含标点): %s
英文字数 (不含标点): %s
=======================
中英文合计 (不含标点): %s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))
;; }}

(defun hurricane/evil-quick-replace (beg end)
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun hurricane//git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;; {{
;; @See: http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/
(defun hurricane/iterm-shell-command (command &optional prefix)
  "Cd to `default-directory' then run COMMAND in iTerm.
with PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (hurricane//git-project-root)
                default-directory))
         ;; If `command' is empty, just change directory.
         (cmd (format "cd \\\"%s\\\" ;%s" (expand-file-name dir) command)))
    (do-applescript
     (format
      "
tell application \"iTerm2\"
     activate
     set _session to current session of current window
     tell _session
          set command to get the clipboard
          write text \"%s\"
     end tell
end tell
  " cmd))))
;; }}

(defun hurricane//mc-mark-next-like-this ()
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this 1)
    (er/expand-region 1)))

(defun hurricane//wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (when (sp-get-thing)
    (sp-forward-slurp-sexp)))

(defun hurricane//evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun hurricane//erc-hook (match-type nick message)
  "Shows a terminal notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (hurricane//notify-osx
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t)))

(defun hurricane/swiper-search (p)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'swiper-thing-at-point
       #'counsel-grep-or-swiper))))

(defun hurricane/counsel-goto-recent-directory ()
  "Recent directories."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; Fasd history.
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'dired
              :caller 'hurricane/counsel-goto-recent-directory)))

(defun hurricane/counsel-find-file-recent-directory ()
  "Find file in recent git repository."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; Fasd history.
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (shell-command "git config --global core.quotepath false")
    (ivy-read "directories:" collection
              :action 'hurricane//find-file-in-git-repo
              :caller 'hurricane/counsel-find-file-recent-directory)))

(defun hurricane//magit-visit-pull-request ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (let ((remote-branch (magit-get-current-branch)))
    (cond
     ((null remote-branch)
      (message "No remote branch"))
     (t
      (browse-url
       (format "%s"
               (replace-regexp-in-string
                "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                (magit-get "remote"
                           (magit-get-remote)
                           "url"))
               remote-branch))))))

(defun hurricane//markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

(defun hurricane//github-browse-file--relative-url ()
  "Return `username/repo' for current repository.
Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun hurricane/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (hurricane//github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

(defun hurricane/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

(defun hurricane/counsel-imenu ()
  (interactive)
  (counsel-imenu)
  (evil-set-jump))

(defun append-string-to-file (string file-name)
  "Append STRING to FILE-NAME."
  (interactive)
  (append-to-file string nil file-name))

(defun get-file-name-from-url (url)
  "Get the substring of URL which after the final slash."
  (with-temp-buffer
    (insert url)
    (let ((file-name-origin 0))
      (goto-char file-name-origin)
      (while (string-match "/" url file-name-origin)
        (progn
          (goto-char file-name-origin)
          (setq file-name-start (re-search-forward "/"))
          (setq file-name-origin (match-end 0))))
      (substring (buffer-string) (- file-name-start 1)))))

(defun pandoc-converter (input-file-name output-file-name read-format write-format)
  "Call pandoc-mode to convert file."
  (let* ((input-file (shell-quote-argument input-file-name))
         (output-file (shell-quote-argument output-file-name))
         (command-string (concat "pandoc " input-file " -f " read-format " -t "
                                 write-format " -s -o " output-file)))
    (shell-command command-string)))

(defun install-monitor (file secs func)
  "Pretend to monitor the given FILE by issuing a check every SECS seconds.
If a change in `file-attributes' happended call FUNC."
  (let ((monitor-attributes (file-attributes file))
        (fun func))
    (run-with-timer
     0 secs
     (lambda (f p)
       (let ((att (file-attributes f)))
         (unless (or (null monitor-attributes) (equalp monitor-attributes att))
           (funcall fun))
         (setq monitor-attributes att)))
     file secs)))

(defun install-monitor-file-exists (file secs func)
  (setq inner-timer
        (run-with-idle-timer
         secs t
         (lambda (file func)
           (let ((file file)
                 (func func))
             (unless (not (file-exists-p file))
               (progn
                 (funcall func)
                 (cancel-timer inner-timer)))))
         file func)))

(defun replace-unexpected-string-in-file (file-name replace-string-rule-lists)
  (with-temp-buffer
    (insert-file-contents file-name)
    (dolist (replace-string-rule replace-string-rule-lists)
      (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
    (write-file file-name)))

(defvar unhtml-string-pattern-list
  '(("&amp;" . "&")
    ("&lt;" . "<")
    ("&gt;" . ">")))

(defun unshift-prefix-if-necessary (string prefix new-prefix)
  "If STRING do not starts with PREFIX, concat NEW-PREFIX STRING and then encode.
Else, returns STRING."
  (if (not (string-prefix-p prefix string))
      (browse-url-encode-url (concat new-prefix string))
    (progn
      (with-temp-buffer
        (insert string)
        (dolist (replace-string-rule unhtml-string-pattern-list)
          (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
        (buffer-string)))))

(defun replace-url-with-file-path-in-org (file-name url-and-file-path-map-list)
  (with-temp-buffer
    (insert-file-contents (concat file-name ".org"))
    (let ((image-directory (concat org-screenshot-image-dir-name "/" file-name)))
      (dolist (url-file-path-map url-and-file-path-map-list)
        (progn
          (goto-char (point-min))
          (replace-string (unshift-prefix-if-necessary (car url-file-path-map) "http" "file:") (concat "file:" image-directory "/" (cdr url-file-path-map))))))
    (write-file (concat file-name ".org"))))

(defvar stackoverflow-question-string-pattern-list
  '("<div class=\"s-prose js-post-body\" itemprop=\"text\">\\|<div class=\"post-text\" itemprop=\"text\">" . "</div>"))

(defvar stackoverflow-answer-string-pattern-list
  '("\\(<div class=\"answercell post-layout--right\">\\|<div style=\"display: block;\" class=\"comment-body\">\\|<div class=\"comment-body\" style=\"display: block;\" >\\)" . "</div>"))

(defvar stackoverflow-image-url-pattern-list '("<img src=\"" . "\""))

(defvar stackoverflow-all-images-url-list)

;; Used to replace unexpected strings in raw extracted html file.
(defvar stackoverflow-html-replace-string-rule-lists
  '(("<span class=\"comment-date\" dir=\"ltr\">\.*" . "")
    ("<span\[^>\]*>" . "<blockquote>")
    ("</span\[^>\]*>" . "</blockquote>")
    ("<h1>" . "")
    ("<h2>" . "")
    ("<h3>" . "")
    ("<h4>" . "")
    ("<h5>" . "")
    ("<h6>" . "")
    ("</h1>" . "")
    ("</h2>" . "")
    ("</h3>" . "")
    ("</h4>" . "")
    ("</h5>" . "")
    ("</h6>" . "")
    ("{%" . "<")
    ("%}" . ">")))

(defun hurricane/extract-content-from-stackoverflow-to-org-file (src-code-type)
  (interactive
   (let ((src-code-type
          '("ipython" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
     (list (ido-completing-read "Source code type: " src-code-type))))

  (setq begin-marker "" url "" html-file-name "" org-file-name "")

  ;; Read `url' string from minibuffer, while the string read is empty, this loop will not stop.
  (setq url "")
  (while (string-equal url "")
    (setq url (read-string "Please input the StackOverFlow url to extract: "
                           nil nil "" nil)))

  ;; Get `file-name' from `url' to create html and org file.
  (setq file-name (replace-regexp-in-string
                   "-"
                   "_"
                   (get-file-name-from-url url)))
  (setq html-file-name (concat
                        file-name
                        ".html")
        org-file-name (concat
                       file-name
                       ".org"))

  ;; Used to replace unexpected strings in org file generated by pandoc.
  (setq org-image-url-template (concat "[[file:./static/" file-name "/\\1"))
  (setq org-replace-string-rule-lists '(("#\\+BEGIN_QUOTE\[\t\r\n \]*#\\+END_QUOTE" . "")
                                        ("\\\\_" . "_")
                                        ("#\\+END_EXAMPLE" . "#+END_SRC")
                                        ;; ("\\[\\[http.*/" . "[[file:./static/")
                                        ))
  (add-to-list 'org-replace-string-rule-lists `("\\[\\[\.*jpeg\.*/\\(.*\\)\\]\\]" . ,org-image-url-template))

  ;; Create begin-marker used to replace unexpected string `#+BEGIN_EXAMPLE'.
  (cond ((string-equal src-code-type "")
         (setq begin-marker (concat "#+BEGIN_SRC " "python")))
        (t
         (setq begin-marker (concat "#+BEGIN_SRC " src-code-type))))
  (add-to-list 'org-replace-string-rule-lists `("#\\+BEGIN_EXAMPLE" . ,begin-marker))

  ;; Extract content to file.
  (with-current-buffer (url-retrieve-synchronously url)
    ;; Remove the `^M' character in html file.
    (dos2unix)
    ;; Extract question strings to file.
    (progn
      (goto-char 0)
      (setq question-start (re-search-forward (car stackoverflow-question-string-pattern-list)))
      (goto-char question-start)
      (setq question-end (re-search-forward (cdr stackoverflow-question-string-pattern-list)))
      (setq question-string (buffer-substring question-start (- question-end 6)))
      (append-string-to-file "{%h1%}Question{%/h1%}" html-file-name)
      (append-string-to-file question-string html-file-name))
    ;; Extract image and comment strings to file.
    (let ((answer-search-origin 0) (answer-number 1))
      (while (string-match (car stackoverflow-answer-string-pattern-list) (buffer-string) answer-search-origin)
        (progn
          (goto-char answer-search-origin)
          (setq answer-string-start (re-search-forward (car stackoverflow-answer-string-pattern-list)))
          (setq answer-string-end (re-search-forward (cdr stackoverflow-answer-string-pattern-list)))
          (setq answer-string (buffer-substring answer-string-start (- answer-string-end 6)))
          (if answer-string
              (progn
                (if (string-suffix-p "</span>" (replace-regexp-in-string "[\t\n\r ]+" "" answer-string))
                    (append-string-to-file "{%h2%}Comment{%/h2%}" html-file-name)
                  (progn
                    (append-string-to-file (concat (concat "{%h1%}Answer" (number-to-string answer-number)) "{%/h1%}") html-file-name)
                    (setq answer-number (+ 1 answer-number))))
                (append-string-to-file answer-string html-file-name)
                (setq answer-search-origin (match-end 0))))))))

  (replace-unexpected-string-in-file html-file-name stackoverflow-html-replace-string-rule-lists)

  (setq stackoverflow-all-images-url-list (hurricane//get-all-images-url html-file-name stackoverflow-image-url-pattern-list))
  (message "%s" stackoverflow-all-images-url-list)

  ;; Download all images.
  (with-proxy (download-all-images stackoverflow-all-images-url-list file-name))

  (pandoc-converter html-file-name org-file-name "html" "org")

  (replace-url-with-file-path-in-org file-name stackoverflow-all-images-url-list)

  (defun callback-replace-unexpected-string-in-file ()
    (replace-unexpected-string-in-file org-file-name org-replace-string-rule-lists))

  (install-monitor-file-exists org-file-name 1 #'callback-replace-unexpected-string-in-file)
  (insert-header-to-org-content file-name))

(defun hurricane//org-as-mac-iTerm2-get-link ()
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    " set theName to custom title in tab 1 of window 1\n"
    " do script \"pwd | pbcopy\" in window 1\n"
    " set theUrl to do shell script \"pbpaste\"\n"
    " return theUrl & \"::split::\" & theName\n"
    "end tell")))

;; {{
;; @See: https://emacs-china.org/t/macos/10219
;; (setq english-ID-map '("1" . "美国"))
;; (setq chinese-ID-map '("2" . "搜狗拼音"))

;; (defun hurricane//switch-input-source (ID-map)
;;   (let ((script
;;          (format
;;            (mapconcat
;;              #'identity
;;              '("tell application \"System Events\" to tell process \"SystemUIServer\""
;;                "set result to get the value of the first menu bar item of menu bar 1 whose description is \"text input\""
;;                "set englishInputSourceIsSelected to result is \"%s\""
;;                "  if englishInputSourceIsSelected is false then"
;;                "      click menu bar item 5 of menu bar 1"
;;                "      click menu item \"%s\" of menu 1 of menu bar item 5 of menu bar 1"
;;                "  end if"
;;                "end tell")
;;              "\n")
;;            (car ID-map)
;;            (cdr ID-map))))
;;     (thread-first script
;;       (do-applescript)
;;       (string-trim "\"\n" "\n\"")
;;       (split-string "\n"))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (hurricane//switch-input-source chinese-ID-map)))
;; (add-hook 'evil-insert-state-exit-hook (lambda () (hurricane//switch-input-source english-ID-map)))
;;}}

;; {{
(setq english-ID-map '("com.apple.keylayout.US" . "美国"))
(setq chinese-ID-map '("com.sogou.inputmethod.sogou.pinyin" . "搜狗拼音"))

(defun hurricane//switch-input-source (ID-map)
  (if (not (string-equal (mac-input-source) (car ID-map)))
      (mac-select-input-source (car ID-map))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (hurricane//switch-input-source chinese-ID-map)))
(cond ((and sys/macp (fboundp 'mac-input-source)) (add-hook 'evil-insert-state-exit-hook (lambda () (hurricane//switch-input-source english-ID-map)))))
;; }}

;; {{
;; @See: https://emacs-china.org/t/topic/5518
(defun hurricane//chrome-tabs ()
  "Return `Chrome' tabs."
  (let ((script
         (mapconcat
          #'identity
          '("set titleString to return"
            ""
            "tell application \"Google Chrome\""
            "  set window_list to every window"
            "  set window_counter to 0"
            ""
            "  repeat with the_window in window_list"
            "    set window_counter to window_counter + 1"
            "    set tab_list to every tab in the_window"
            "    set tab_counter to 0"
            ""
            "    repeat with the_tab in tab_list"
            "      set tab_counter to tab_counter + 1"
            "      set coordinate to window_counter & \"⋘⋙\" & tab_counter"
            "      set the_title to the title of the_tab"
            "      set the_url to get URL of the_tab"
            "      set titleString to titleString & coordinate & \"⋘⋙\" & the_title & \"⋘⋙\" & the_url & return"
            "    end repeat"
            "  end repeat"
            "end tell")
          "\n")))
    (thread-first script
                  (do-applescript)
                  (string-trim "\"\n" "\n\"")
                  (split-string "\r"))))

;; (hurricane//chrome-tabs)
;; => ("1⋘⋙1⋘⋙Google⋘⋙www.google.com" "1⋘⋙2⋘⋙Home - BBC News⋘⋙www.bbc.com")
;; 1 - 第一个窗口
;; 1 - 第一个标签
;; Google - 标题
;; www.google.com - 地址

(defun hurricane//chrome-switch-tab-1 (window-id tab-id)
  ;; FIXME: 不知道如何处理多余一个窗口的情况。
  (do-applescript
   (concat "tell application \"Google Chrome\"\n"
           (format "  set active tab index of first window to %s\n" tab-id)
           "  activate\n"
           "end tell\n")))

(defun hurricane//chrome-close-tab-1 (window-id tab-id)
  (do-applescript
   (concat (format "tell window %s of application \"Google Chrome\"\n" window-id)
           (format "  close (tab index %s)\n" tab-id)
           "end tell\n")))

(defun hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x (x)
  (let*
      ((tabs (hurricane//chrome-tabs))
       (window-id-and-tab-id-and-tab-url
        (seq-some (lambda (s)
                    (and (string-match
                          (rx-to-string
                           `(and
                             string-start
                             (group (1+ num)) "⋘⋙" (group (1+ num)) "⋘⋙"
                             ,x "⋘⋙" (group (1+ not-newline))
                             string-end))
                          s)
                         (list (match-string 1 s)
                               (match-string 2 s)
                               (match-string 3 s)
                               )))
                  tabs)))
    window-id-and-tab-id-and-tab-url))

(defun hurricane//chrome-switch-tab-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (hurricane//chrome-switch-tab-1 (nth 0 window-id-and-tab-id-and-tab-url) (nth 1 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-close-tab-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (run-with-idle-timer 0.5 nil #'hurricane//chrome-close-tab-1 (nth 0 window-id-and-tab-id-and-tab-url) (nth 1 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-copy-tab-url-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (kill-new (nth 2 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-insert-tab-url-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (insert (nth 2 window-id-and-tab-id-and-tab-url))))

(defun hurricane/manage-chrome-tabs ()
  (interactive)
  (ivy-read "Chrome Tab (default activate): " (mapcar
                                               (lambda (s)
                                                 (and (string-match
                                                       (rx string-start
                                                           (1+ num) "⋘⋙" (1+ num) "⋘⋙"
                                                           (group (1+ not-newline))
                                                           "⋘⋙" (1+ not-newline)
                                                           string-end)
                                                       s)
                                                      (match-string 1 s)))
                                               (remove "" (hurricane//chrome-tabs)))
            :action 'hurricane//chrome-switch-tab-action
            ))

(with-eval-after-load 'ivy
  (ivy-add-actions
   #'hurricane/manage-chrome-tabs
   '(("d" hurricane//chrome-close-tab-action "close tab(s)")
     ("y" hurricane//chrome-copy-tab-url-action "copy tab(s) url")
     ("I" hurricane//chrome-insert-tab-url-action "insert tab(s) url"))))

(defun hurricane/open-noter-page ()
  (interactive)
  (let* ((page-location (org-noter--parse-location-property (org-entry-get-with-inheritance org-noter-property-note-location)))
         (pdf-url (org-entry-get-with-inheritance org-noter-property-doc-file))
         ($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ($pathStarts "\\[\\[")
                                ($pathStops "\\]\\]"))
                        (setq $p0 (point))
                        (re-search-forward $pathStops nil t)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (re-search-backward $pathStarts nil t)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         )
    (if (and pdf-url page-location (not (use-region-p)))
        (org-link-open-from-string (url-unhex-string (format "[[%s:%s#%s]]" org-noter-property-note-location pdf-url page-location)))
      (org-link-open-from-string (url-unhex-string $inputStr)))))

(defun hurricane/open-link-in-chrome ()
  "Open `url' under cursor in Chrome.
Work in macOS only."
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (shell-command
     (format "open -a Google\\ Chrome.app \"%s\"" $path))))
;; }}


(cond (sys/macp
       (progn
         (eval-and-compile
           (if (fboundp 'window-inside-edges)
               ;; Emacs devel.
               (defalias 'th-window-edges
                 'window-inside-edges)
             ;; Emacs 21.
             (defalias 'th-window-edges
               'window-edges)
             ))

         (defun th-point-position ()
           "Return the location of POINT as positioned on the selected frame.
    Return a cons cell `(x . y)'."
           (let* ((w (selected-window))
                  (f (selected-frame))
                  (edges (th-window-edges w))
                  (col (current-column))
                  (row (count-lines (window-start w) (point)))
                  (x (+ (car edges) col))
                  (y (+ (car (cdr edges)) row)))
             (cons x y)))

         (defun get-point-pixel-position ()
           "Return the position of point in pixels within the frame."
           (let ((point-pos (th-point-position)))
             (th-get-pixel-position (car point-pos) (cdr point-pos))))


         (defun th-get-pixel-position (x y)
           "Return the pixel position of location X Y (1-based) within the frame."
           (let ((old-mouse-pos (mouse-position)))
             (set-mouse-position (selected-frame)
                                 ;; The fringe is the 0th column, so x is OK
                                 x
                                 (1- y))
             (let ((point-x (car (cdr (mouse-pixel-position))))
                   (point-y (cdr (cdr (mouse-pixel-position)))))
               ;; On Linux with the Enlightenment window manager restoring the
               ;; mouse coordinates didn't work well, so for the time being it
               ;; is enabled for Windows only.
               (when (eq window-system 'w32)
                 (set-mouse-position
                  (selected-frame)
                  (cadr old-mouse-pos)
                  (cddr old-mouse-pos)))
               (cons point-x point-y))))

         (defun hurricane//display-current-input-method-title (arg1 &optional arg2 arg3)
           "Display current input method name."
           (when current-input-method-title
             (set-mouse-position (selected-frame) (car (th-point-position)) (cdr (th-point-position)))
             (x-show-tip current-input-method-title (selected-frame) nil 1  20 -30)))

         (advice-add 'evil-insert :after 'hurricane//display-current-input-method-title))))

(defvar official-accounts-all-images-url-list)

(defvar official-accounts-image-url-pattern-list
  '("<img data-s=\"300,640\" data-type=\"png\" data-src=\"\\|data-s=\"300,640\" data-src=\"\\|<img data-s=\"300,640\" data-type=\"jpeg\" data-src=\"\\|data-src=\"\\|src=\"" . "\"\\|?"))

(defvar official-accounts-content-pattern-list
  '("<div class=\"rich_media_content                                                                     \"
            id=\"js_content\" style=\"visibility: hidden;\">
\\|<div class=\"rich_media_content                                       \"
            id=\"js_content\" style=\"visibility: hidden;\">" . "</div>"))

(defun hurricane//get-all-images-url (file image-url-pattern-list)
  "Return a image file name and image URL map list, extract from html FILE.

Image file name is generated from `match-end' position string."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((image-url-list nil)
          (search-origin 0) )
      (while (string-match (car image-url-pattern-list) (buffer-string) search-origin)
        (progn
          (goto-char search-origin)
          (setq image-url-start (re-search-forward (car image-url-pattern-list)))
          (setq image-url-end (re-search-forward (cdr image-url-pattern-list)))
          (setq image-url (buffer-substring image-url-start (- image-url-end 1)))
          (if image-url
              (progn
                (push (cons image-url (concat (number-to-string (match-end 0)) ".jpeg")) image-url-list)
                (setq search-origin (match-end 0))))))
      image-url-list)))

(defun download-all-images (url-list file-name)
  "Use `org-download--image' to download image from URL-LIST,FILE-NAME be used to format directory name."
  (let ((image-directory (concat org-screenshot-image-dir-name "/" file-name)))
    (cl-loop for url in url-list
             do (progn
                  (unless (file-exists-p image-directory)
                    (make-directory image-directory t))
                  (ignore-errors (org-download--image (car url) (concat image-directory "/" (cdr url))))
                  (message "Begin to download: %s" url)
                  (sleep-for 1)))))

(defun insert-header-to-org-content (file-name)
  (with-temp-buffer
    (progn
      (insert-file-contents (concat file-name ".org"))
      (goto-char (point-min))
      (insert (format "# -*- eval: (setq org-download-image-dir (concat default-directory \"./static/%s/\")); -*-\n" file-name))
      (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" (org-id-new)))
      (insert (format "#+DATE: <%s>\n" (format-time-string "%Y-%m-%d %b %H:%M")))
      (insert (format "#+TITLE: %s\n" file-name))
      (insert "#+ROAM_KEY:\n#+PDF_KEY:\n#+PAGE_KEY:\n\n")
      (write-file (concat file-name ".org")))))

(setq html-image-url-pattern-list
      '(" src=\"\\|\]\(" . "\"\\|)"))

(defun hurricane/find-file-html-or-markdown-to-org (&optional in-file)
  (interactive)
  (setq in-file-org (if (and in-file (file-exists-p in-file))
                        (concat (file-name-nondirectory (file-name-sans-extension in-file)) ".org")
                      (concat (read-string "Please input the Org file name: "
                                           nil nil "" t) ".org")))
  (setq in-file (if (not in-file)
                    (if (derived-mode-p 'dired-mode)
                        (dired-get-file-for-visit)
                      buffer-file-name)
                  in-file))
  (setq in-file-extension (pcase (file-name-extension in-file)
                            ("md" "markdown")
                            ("html" "html")))
  (setq html-all-images-url-list (hurricane//get-all-images-url in-file html-image-url-pattern-list))

  (message "url-list %s" html-all-images-url-list)

  (download-all-images html-all-images-url-list (file-name-nondirectory (file-name-sans-extension in-file-org)))

  (sleep-for 1)

  (pandoc-converter in-file in-file-org in-file-extension "org")

  (sleep-for 1)

  (replace-url-with-file-path-in-org (file-name-nondirectory (file-name-sans-extension in-file-org)) html-all-images-url-list)

  (defun callback-insert-header-to-org-content ()
    (insert-header-to-org-content (file-name-sans-extension in-file-org)))

  (install-monitor-file-exists in-file-org 1 #'callback-insert-header-to-org-content))

(defun hurricane/extract-content-from-official-accounts-to-org-file ()
  (interactive)
  ;; Read `url' string from minibuffer, while the string read is empty, this loop will not stop.
  (setq url "")
  (setq file-name "")
  (while (string-equal url "")
    (setq url (read-string "Please input the Official Accounts url to extract: "
                           nil nil "" nil)))
  (while (string-equal file-name "")
    (setq file-name (read-string "Please input the File name: "
                                 nil nil "" nil)))
  (setq html-file-name (concat file-name ".html")
        org-file-name  (concat file-name ".org"))

  ;; Extract content to file.
  (with-current-buffer (url-retrieve-synchronously url)
    ;; Remove the `^M' character in html file.
    (dos2unix)
    (progn
      (goto-char 0)
      (setq content-start (re-search-forward (car official-accounts-content-pattern-list)))
      (goto-char content-start)
      (setq content-end (re-search-forward (cdr official-accounts-content-pattern-list)))
      (setq content-string (buffer-substring content-start (- content-end 6)))
      (append-string-to-file content-string html-file-name)))

  (setq official-accounts-all-images-url-list (hurricane//get-all-images-url html-file-name official-accounts-image-url-pattern-list))
  (message "%s" official-accounts-all-images-url-list)

  ;; Download all images.
  (download-all-images official-accounts-all-images-url-list file-name)

  (replace-unexpected-string-in-file html-file-name '(("data-src" . "src")))
  (sleep-for 1)
  (pandoc-converter html-file-name org-file-name "html" "org")

  (replace-url-with-file-path-in-org file-name official-accounts-all-images-url-list)

  (defun callback-insert-header-to-org-content ()
    (insert-header-to-org-content file-name))

  (install-monitor-file-exists org-file-name 1 #'callback-insert-header-to-org-content))

;;{{
(defvar hurricane-proxy  "127.0.0.1:1080")

;; Network Proxy
(defun hurricane/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" hurricane-proxy)
    (message "No HTTP proxy")))

(defun hurricane/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,hurricane-proxy)
          ("https" . ,hurricane-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (hurricane/proxy-http-show))

(defun hurricane/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (hurricane/proxy-http-show))

(defun hurricane/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (hurricane/proxy-http-disable)
    (hurricane/proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun hurricane/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1080 5))
  (proxy-socks-show))

(defun hurricane/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun hurricane/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (hurricane/proxy-socks-disable)
    (hurricane/proxy-socks-enable)))
;;}}

(defun hurricane/dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this (dired-get-filename t))
         (name-base (file-name-base this))
         (extension (file-name-extension this))
         (ctr 1)
         (new (format "%s_%d.%s" name-base ctr extension)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s_%d.%s" name-base ctr extension)))
    (dired-copy-file this new nil))
  (revert-buffer))

(defun hurricane/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun rime-predicate-blink-search-p ()
  "Whether a blink-search keymap is activated.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (featurep 'blink-search)
       (bound-and-true-p blink-search-mode-map)))

;; {{
;; Audio note (English) 用的是 knowclip.app 提供的模板。
;; @See: https://github.com/nicehiro/.emacs.d/blob/master/lisp/init-anki.el
(defvar Anki-connect-host "127.0.0.1"
  "Anki connect server host.")

(defvar anki-connect-port "8765"
  "Anki connect server port.")

(defvar Anki-deck-name "English"
  "Shengci in anki deck name.")

(defun anki-add-card (deck front back &optional translation screenshot tag)
  "Add anki basic card which contains FRONT and BACK elements to the DECK."
  (let* ((req-params (list `("note" . ,(list `("deckName" . ,deck)
                                             '("modelName" . "Antimoon without expression")
                                             `("fields" . ,(list `("audio" . ,front)
                                                                 `("sentence" . ,back)
                                                                 `("translation" . ,translation)
                                                                 `("image" . ,screenshot)))
                                             `("options" . ,(list
                                                             '("allowDuplicate" . t)))
                                             `("tags" . ,(list tag)))))))
    (request (format "%s:%s" Anki-connect-host anki-connect-port)
      :type "POST"
      :data (json-encode (list '("action" . "addNote")
                               '("version" . 6)
                               `("params" . ,req-params)))
      :headers '(("Content-Type" . "text/json"))
      :parser 'json-read
      :error
      (cl-function
       (lambda (&rest _args)
         (debug "Error response in variable '_args'")))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; (message "result: %S" (assoc-default 'result data))
                  (message "result: %S" data)
                  )))))
;; }}

;; {{
;; @See: http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html
(defun xah-cycle-hyphen-lowline-space ( &optional @begin @end )
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
When this command is called, pressing t will repeat it. Press other key to exit.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.
URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version 2019-02-12 2021-08-09"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let ($p1 $p2)
    (if (and @begin @end)
        (setq $p1 @begin $p2 @end)
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq $p1 (point))
              (skip-chars-forward "^\"")
              (setq $p2 (point)))
          (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
            (skip-chars-backward $skipChars (line-beginning-position))
            (setq $p1 (point))
            (skip-chars-forward $skipChars (line-end-position))
            (setq $p2 (point))
            (set-mark $p1)))))
    (let ( $charArray $length $regionWasActive-p $nowState $changeTo)
      (setq $charArray ["-" "_" " "])
      (setq $length (length $charArray))
      (setq $regionWasActive-p (region-active-p))
      (setq $nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0 ))
      (setq $changeTo (elt $charArray $nowState))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (while (re-search-forward (elt $charArray (% (+ $nowState 2) $length)) (point-max) "move")
            (replace-match $changeTo t t))))
      (when (or (string-equal $changeTo " ") $regionWasActive-p)
        (goto-char $p2)
        (set-mark $p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $length))))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "t") 'xah-cycle-hyphen-lowline-space ) $kmap)))
;; }}

;; {{
(defcustom lc-corpus-eww-sentence-abbrevs '("i.e." "etc." "U.S.")
  "Prevent to incorrectly determine sentence end."
  :type '(repeat string)
  :group 'language-chunk
  )

(defcustom lc-corpus-eww-sentence-ends (rx (or
                                            (and "." (or " " eol))
                                            (and "?" (or " " eol))
                                            (and "!" (or " " eol))
                                            (and ";" (or " " eol))
                                            "\n\n"))
  "A regexp used to determine where is the end of a sentence in eww."
  :type 'string
  :group 'language-chunk
  )

(defun lc-corpus--string-ends-with-any (str patterns)
  (cl-dolist (p patterns)
    (when (string-suffix-p p str t)
      (cl-return t))))

(defun lc-corpus--eww-sentence ()
  (let ((sentence-ends lc-corpus-eww-sentence-ends)
        (point (point))
        (stop nil)
        start end)
    (save-excursion
      (while (not stop)
        (setq end (search-forward-regexp sentence-ends nil t))
        ;; (message "end: %s" end)
        (if (not end)
            (setq end (point-max)
                  stop t)
          (unless (lc-corpus--string-ends-with-any (buffer-substring-no-properties point (- end 1)) lc-corpus-eww-sentence-abbrevs)
            (setq stop t))))

      (setq stop nil)
      (goto-char point)
      (while (not stop)
        (setq start (search-backward-regexp sentence-ends nil t))
        ;; (message "start: %s" start)
        (if (not start)
            (setq start (point-min)
                  stop t)
          (unless (lc-corpus--string-ends-with-any (buffer-substring-no-properties (point-at-bol) (1+ start)) lc-corpus-eww-sentence-abbrevs)
            (setq stop t)
            (setq start (1+ start))))))
    (string-trim (buffer-substring-no-properties start end))))

(defun lc-corpus-sentence ()
  "Used to test `lc-corpus--sentence'."
  (interactive)
  (message "%s" (lc-corpus--sentence)))

(defun lc-corpus--sentence ()
  (let (sentence)
    (cond
     ((derived-mode-p 'eww-mode)
      (setq sentence (lc-corpus--eww-sentence)))
     ((string-equal (buffer-name (current-buffer)) "*mybigword-list*")
      (setq sentence nil))
     (t
      (setq sentence (thing-at-point 'sentence t))))
    sentence))
;; }}

;; {{
(setq voicetube-subtitle-replace-string-rule-lists '(("" . "^[0-9]\\{1,2\\}\\.$")
                                                     ("" . ",")
                                                     ;; ("" . "\\.")
                                                     ("" . ":")
                                                     ("" . "!")
                                                     ("" . "！")
                                                     ("" . "?")
                                                     ("" . "\\[")
                                                     ("" . "\\]")
                                                     ("" . "(")
                                                     ("" . ")")
                                                     ("" . "%")
                                                     ("" . "#")
                                                     ("" . "@")
                                                     ("" . "&")
                                                     ;; ("１" . "1")
                                                     ;; ("２" . "2")
                                                     ;; ("３" . "3")
                                                     ;; ("４" . "4")
                                                     ;; ("５" . "5")
                                                     ;; ("６" . "6")
                                                     ;; ("７" . "7")
                                                     ;; ("８" . "8")
                                                     ;; ("９" . "9")
                                                     ;; ("０" . "0")
                                                     ;; ;; ("、" . ",")
                                                     ;; ("；" . ",")
                                                     ;; ("“" . "\"")
                                                     ;; ("”" . "\"")
                                                     ("" . ":")
                                                     ("" . "`")
                                                     ("" . ";")))

(defun voicetube-subtitle-buffer-filter ()
  (interactive)
  (dolist (replace-string-rule voicetube-subtitle-replace-string-rule-lists)
    (replace-region-or-buffer (cdr replace-string-rule) (car replace-string-rule) nil)))
;; }}

(defun anki-clip-mp3 (timestamp-a timestamp-b)
  (interactive)
  (let* ((media-path (mpv-get-property "path"))
         (processed-media-path (substring media-path 0 (string-match-p "?" media-path))))
    (if (string-match-p (regexp-quote "bilivideo") processed-media-path)
        (setq header-and-agent "-headers Referer:https://www.bilivideo.com/ -user_agent \"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36 Edg/89.0.774.76 \"")
      (setq header-and-agent ""))
    (setq media-clip-file-name
          (concat
           (replace-regexp-in-string "\\]" "_"
                                     (replace-regexp-in-string "\\[" "_"
                                                               (org-media-note--format-picture-file-name
                                                                (concat (file-name-base processed-media-path)
                                                                        " - clip - "
                                                                        ;; (org-media-note--get-current-timestamp)
                                                                        timestamp-a "--" timestamp-b (format-time-string "_%-I_%M_%p")))))
           "." "mp3"))
    (setq media-clip-target-path
          (cond
           ((eq org-media-note-screenshot-save-method 'attach)
            (expand-file-name media-clip-file-name (org-attach-dir t)))
           ((eq org-media-note-screenshot-save-method 'directory)
            (if (not (f-exists? Anki-media-dir))
                (make-directory Anki-media-dir))
            (expand-file-name media-clip-file-name Anki-media-dir))))
    (setq media-clip-screenshot (format "media-clip_%s.png" (format-time-string "%-I_%M_%p")))
    (setq media-clip-sentence (lc-corpus--sentence))
    (when (not (file-exists-p media-clip-target-path))
      (progn
        ;; cut media clip with ffmpeg.
        ;; (ffmpeg-utils-cut-clip media-path timestamp-a timestamp-b media-clip-target-path)
        (let* ((timestamp-start timestamp-a)
               (timestamp-stop timestamp-b)
               (final-cmd (format "ffmpeg %s -i \"%s\" -ss %s -to %s -q:a 0 -map a \"%s\" && ffmpeg %s -i \"%s\" -ss %s -frames:v 1 -vf scale=640:-1 \"%s%s\"" header-and-agent media-path timestamp-start timestamp-stop media-clip-target-path header-and-agent media-path timestamp-start (expand-file-name Anki-media-dir) media-clip-screenshot))
               (proc
                (start-process-shell-command
                 "media-clip"
                 nil
                 final-cmd)))
          (set-process-sentinel
           proc
           (lambda (proc event)
             (when (equal event "finished\n")
               (anki-add-card Anki-deck-name (format "[sound:%s]" media-clip-file-name) media-clip-sentence "" (format "<img src=\"%s\">" media-clip-screenshot) "subs2srs")
               )))
          t)
        ;; (org-media-note--display-inline-images)
        ;; display process indicator in mode-line and echo-area.
        ;; (setq mode-line-process
        ;;       (propertize
        ;;        (format " [org-media-note] clip between A-B loop: %s--%s." timestamp-a timestamp-b)
        ;;        'font-lock-face 'mode-line-highlight))
        ;; (force-mode-line-update t)
        ;; (message "[org-media-note] clip between timestamp A-B loop: %s--%s." timestamp-a timestamp-b)
        ))))

;; {{
(defun aspeak-tts (&optional sentence)
  (interactive (list (read-string "Speak: " (emacs-azure-tts-region-or-sentence))))
  (let ((final-cmd (format "edge-playback --text \"%s\" text \"%s\"" (file-truename aspeak-profile-file) sentence)))
    (shell-command final-cmd)
    ))
;; }}

;; {{
(defvar youtube-title-string-pattern-list
  '("<meta name=\\\"title\\\" content=\\\"" . "\\\">"))

(defun hurricane/download-youtube-transcript (&optional url)
  (interactive (list (read-string "URL: " (or eaf--buffer-url ""))))
  (let ((video-id (if (string-match "v=\\([^&]+\\)" url) (match-string 1 url) nil))
        (buffer (generate-new-buffer "*youtube_transcript_api*")))
    (if video-id
        (progn
          (with-current-buffer (url-retrieve-synchronously url)
            (dos2unix)
            (progn
              (goto-char 0)
              (setq title-start (re-search-forward (car youtube-title-string-pattern-list)))
              (goto-char title-start)
              (setq title-end (re-search-forward (cdr youtube-title-string-pattern-list)))
              (setq raw-title-string (buffer-substring title-start (- title-end 2)))
              (setq title-string (replace-regexp-in-string "[^[:alnum:][:digit:][:space:]]" "" raw-title-string))
              (setq youtube-transcript-filename (expand-file-name (concat title-string ".srt") mpv-storage-dir))
              ))

          (make-process
           :name "youtube_transcript_api"
           :command (append '("youtube_transcript_api") `(,video-id "--languages" "en" "--format" "srt"))
           :buffer buffer
           :sentinel `(lambda (p e)
                        (message "Process %s %s" p (replace-regexp-in-string "\n\\'" "" e))
                        (set-buffer ',buffer)
                        (goto-char (point-min))
                        (append-string-to-file (buffer-string) ',youtube-transcript-filename)))

          (let ((buf (find-file-noselect youtube-transcript-filename)))
            (with-current-buffer buf
              (set (make-local-variable 'youtube-transcript-url) url))))
      (message (format "Unavailable URL: %s" url)))))

(defun hurricane/mpv-play (&optional url)
  (interactive (list (read-string "URL: " (or eaf--buffer-url (ignore-errors (buffer-local-value 'youtube-transcript-url (current-buffer)))))))
  (let* ((video-url url)
         (buffer (generate-new-buffer "*you-get to mpv*"))
         (subtitle-file-path (spacemacs--file-path))
         (mpv-argv (if subtitle-file-path (concat "mpv --input-ipc-server=/var/tmp/mpv.socket --no-terminal" (format " --sub-files=\"%s\"" subtitle-file-path)) "mpv --input-ipc-server=/var/tmp/mpv.socket --no-terminal")))
    (while (string-equal video-url "")
      (setq video-url (read-string "Please input the URL to play: "
                                   nil nil "" nil)))
    (if video-url
        (make-process
         :name "you-get to mpv"
         :command (append '("you-get") `("-p" ,mpv-argv ,video-url))
         :buffer buffer
         :sentinel `(lambda (p e)
                      (message "Process %s %s" p (replace-regexp-in-string "\n\\'" "" e))))
      (message "Unavariable URL!")
      )))

(defun hurricane/mpv-toggle-ontop ()
  (interactive)
  (python-bridge-call-async "mpv_ontop" (list (subed-mpv--socket) "cycle" "ontop")))

(defun hurricane/ivy-you-get (&optional url)
  (interactive (list (read-string "URL: " (or eaf--buffer-url (ignore-errors (buffer-local-value 'youtube-transcript-url (current-buffer)))))))
  (let ((video-url url)
        (subtitle-file (and buffer-file-name (file-truename (buffer-file-name))))
        (socket-file (subed-mpv--socket))
        (buffer (get-buffer-create "*you-get formats*")))
    (while (string-equal video-url "")
      (setq video-url (read-string "Please input the URL to play: "
                                   nil nil "" nil)))
    (message "%s" (list "you-get" "-i" "-x" (format "%s:%s" provixy-host provixy-port) "--debug" video-url))
    (make-process :name "you-get formats"
                  :buffer buffer
                  :command (list "you-get" "-i" "-x" (format "%s:%s" provixy-host provixy-port) "--debug" video-url)
                  :connection-type 'pipe
                  :sentinel `(lambda (p e)
                               (set-buffer ',buffer)
                               (goto-char (point-min))
                               ;; (unless (search-forward "streams" nil t)
                               ;;   (kill-buffer)
                               ;;   (error "url not supported"))
                               (forward-line 1)
                               (let (list)
                                 (while (not (eobp))
                                   (setq list (cons
                                               (split-string
                                                (buffer-substring-no-properties
                                                 (point)
                                                 (point-at-eol)) "\n" t nil)
                                               list))
                                   (forward-line 1))
                                 (setq list (nreverse list))
                                 (kill-buffer ,buffer)
                                 (ivy-read "you-get formats (itag): " list
                                           :action '(1
                                                     ("o"
                                                      (lambda (x)
                                                        (hurricane//you-get
                                                         (if (string-match (rx (one-or-more digit)) (format "%s" x)) (match-string 0 (format "%s" x))) ',video-url ',subtitle-file ',socket-file t))
                                                      "play")
                                                     ("d"
                                                      (lambda (x)
                                                        (hurricane//you-get
                                                         (if (string-match (rx (one-or-more digit)) (format "%s" x)) (match-string 0 (format "%s" x))) ',video-url ',subtitle-file ',socket-file nil))
                                                      "download"))
                                           :sort nil
                                           :history 'hurricane//you-get
                                           :re-builder #'regexp-quote
                                           :preselect "best"))))))

(defun hurricane//you-get (fmt video-url subtitle-file socket-file &optional play-now)
  (let* ((buffer (generate-new-buffer "*you-get*"))
         (subtitle-file-path subtitle-file)
         (format-args (if (string-match-p "bilibili" video-url) (format "dash-flv%s" fmt) fmt))
         (mpv-args (if (and (not (string-match-p "bilibili" video-url)) subtitle-file-path) (concat (format "mpv --input-ipc-server=%s --idle --osd-level=2 --osd-fractions --no-terminal --referrer='https://www.bilibili.com' --user-agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36 Edg/89.0.774.76 '" socket-file) (format " --sub-files=\"%s\"" subtitle-file-path)) (format "mpv --input-ipc-server=%s --idle --osd-level=2 --osd-fractions --no-terminal --referrer='https://www.bilibili.com' --user-agent='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.114 Safari/537.36 Edg/89.0.774.76 '" socket-file)))
         (mpv-argv (list "-p" mpv-args))
         (output-dir-argv (list "-o" (file-truename mpv-storage-dir)))
         (mpv-or-output-dir-argv (if play-now mpv-argv output-dir-argv))
         (proxy-args (format "%s:%s" provixy-host provixy-port))
         (final-cmd (append '("you-get") `("-F" ,format-args) `,mpv-or-output-dir-argv `("-x" ,proxy-args) '("--debug" ) `(,video-url))))

    ;; (with-current-buffer buffer
    ;;   (ansi-color-for-comint-mode-on)
    ;;   (comint-mode))

    (message "%s" final-cmd)

    (if (and (and subtitle-file-path (file-exists-p subtitle-file-path)) play-now)
        (let ((buf (find-file-noselect subtitle-file-path)))
          (with-current-buffer buf
            (when (subed-mpv--server-started-p)
              (subed-mpv-kill))

            (condition-case err
                (setq subed-mpv--server-proc (make-process :name "you-get"
                                                           :buffer nil
                                                           :command final-cmd
                                                           ;; :connection-type 'pty
                                                           ;; :filter 'comint-output-filter
                                                           :noquery t
                                                           ))
              (error
               (error "%s" (mapconcat #'indentity (cdr (cdr err)) ": "))))

            (message ">>> you-get is going to play <<<")

            (setq subed-mpv-media-file video-url)
            (setq subed-mpv--retry-delays '(2 2 2 2 2 5 5 5 5 5 5 5 5 5 5))
            (subed-mpv--client-connect subed-mpv--retry-delays)
            (if (file-exists-p subtitle-file-path)
                (subed-mpv-add-subtitles subtitle-file-path)
              (add-hook 'after-save-hook #'subed-mpv--add-subtitle-after-first-save :append :local))
            (subed-mpv--client-send `(observe_property 1 time-pos))
            (subed-mpv-playback-speed subed-playback-speed-while-not-typing)
            ))
      (make-process :name "you-get"
                    :buffer buffer
                    :command final-cmd
                    :connection-type 'pty
                    ;; :filter 'comint-output-filter
                    :sentinel (lambda (p e)
                                (message
                                 "Process %s %s" p (replace-regexp-in-string "\n\\'" "" e)))))
    ))
;; }}

;; {{
(defvar wiznote-content-string-pattern-list
  '("<body class=\"wiz-editor-body\">\\|<body class=\"wiz-editor-body\" >\\|<body class=\"wiz-editor-body\" style=\"opacity: 1;\" spellcheck=\"false\">\\|<body class=\"wiz-editor-body\" style=\"opacity: 1;\" spellcheck=\"false\" >\\|<body spellcheck=\"false\" >\\|<body spellcheck=\"false\">" . "</body>"))

(defvar wiznote-image-url-pattern-list '("<img src=\"\\|<img border=\"0\" src=\"\\|<img style=\"vertical-align: bottom; max-width: 100%;\" src=\"\\|<img alt=\"这里写图片描述\" title=\"\" style=\"margin: 0px; padding: 0px 5px; display: inline;\" src=\"\\|<img border=\"0\" class=\"\" src=\"" . "\""))

(defvar wiznote-all-images-url-list)

(defun download-all-wiznote-images (url image-url-list file-name)
  "Use `org-download--image' to download image from URL-LIST,FILE-NAME be used to format directory name."
  (let ((image-directory (concat org-screenshot-image-dir-name "/" file-name)))
    (cl-loop for image-url in image-url-list
             do (progn
                  (unless (file-exists-p image-directory)
                    (make-directory image-directory t))
                  (ignore-errors (org-download--image (concat (file-name-directory url) (car image-url) )(concat image-directory "/" (cdr image-url))))
                  (message "Begin to download: %s" url)
                  (sleep-for 1)))))

(defun hurricane/extract-content-from-wiznote-to-org-file (src-code-type)
  (interactive
   (let ((src-code-type
          '("ipython" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
     (list (ido-completing-read "Source code type: " src-code-type))))

  (setq begin-marker "" url "" file-name "" html-file-name "" org-file-name "")

  ;; Read `url' string from minibuffer, while the string read is empty, this loop will not stop.
  (setq url "")
  (while (string-equal url "")
    (setq url (read-string "Please input the Wiznote url to extract: "
                           nil nil "" nil)))

  (while (string-equal file-name "")
    (setq file-name (read-string "Please input the File name: "
                                 nil nil "" nil)))

  (setq html-file-name (concat
                        file-name
                        ".html")
        org-file-name (concat
                       file-name
                       ".org"))

  ;; Used to replace unexpected strings in org file generated by pandoc.
  (setq org-image-url-template (concat "[[file:./static/" file-name "/\\1"))
  (setq org-replace-string-rule-lists '(("#\\+BEGIN_QUOTE\[\t\r\n \]*#\\+END_QUOTE" . "")
                                        ("\\\\_" . "_")
                                        ("#\\+END_EXAMPLE" . "#+END_SRC")
                                        ;; ("\\[\\[http.*/" . "[[file:./static/")
                                        ))
  (add-to-list 'org-replace-string-rule-lists `("\\[\\[\.*jpeg\.*/\\(.*\\)\\]\\]" . ,org-image-url-template))

  ;; Create begin-marker used to replace unexpected string `#+BEGIN_EXAMPLE'.
  (cond ((string-equal src-code-type "")
         (setq begin-marker (concat "#+BEGIN_SRC " "python")))
        (t
         (setq begin-marker (concat "#+BEGIN_SRC " src-code-type))))
  (add-to-list 'org-replace-string-rule-lists `("#\\+BEGIN_EXAMPLE" . ,begin-marker))

  ;; Extract content to file.
  (with-current-buffer (url-retrieve-synchronously url)
    ;; Remove the `^M' character in html file.
    (dos2unix)
    (progn
      (message "%s" (buffer-string))
      (goto-char 0)
      (setq content-start (re-search-forward (car wiznote-content-string-pattern-list)))
      (goto-char content-start)
      (setq content-end (re-search-forward (cdr wiznote-content-string-pattern-list)))
      (setq content-string (buffer-substring content-start (- content-end 7)))
      (append-string-to-file content-string html-file-name))
    )

  (setq wiznote-all-images-url-list (hurricane//get-all-images-url html-file-name wiznote-image-url-pattern-list))
  (message "%s" wiznote-all-images-url-list)

  ;; Download all images.
  (download-all-wiznote-images url wiznote-all-images-url-list file-name)

  (pandoc-converter html-file-name org-file-name "html" "org")

  (replace-url-with-file-path-in-org file-name wiznote-all-images-url-list)

  (defun callback-replace-unexpected-string-in-file ()
    (replace-unexpected-string-in-file org-file-name org-replace-string-rule-lists))

  (install-monitor-file-exists org-file-name 1 #'callback-replace-unexpected-string-in-file)
  (insert-header-to-org-content file-name)
  )
;; }}

;; {{
(defvar goldendict-frame nil)

(defcustom goldendict-buffer-name
  "*GoldenDict*"
  "GoldenDict buffer name."
  :type 'string)

(defcustom goldendict-api-host
  "127.0.0.1"
  "The network address SilverDict is listening."
  :type 'string)

(defcustom goldendict-api-port
  "2628"
  "The port number SilverDict is listening."
  :type 'string)

(defcustom goldendict-html-replace-string-rule-lists
  '(("<font face=\"Kingsoft Phonetic Plain, Tahoma\" color=#FF6600>[^>]*?>" . "")
    ("<audio\[^>\]*>" . ""))
  "The rule alists to filter dictionary content."
  :type 'alist)

(defun goldendict--suggestions-api-call (query)
  (let (reply err)
    (anki-editor--fetch
     (url-encode-url
      (format "http://%s:%s/api/suggestions/Default Group/%s"
              goldendict-api-host
              goldendict-api-port
              query)
      )
     :type "GET"
     :parser 'json-read
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (setq reply data)))
     :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                           (setq err (string-trim (cdr error-thrown)))))
     :sync t)
    (when err (error "Error communicating with SilverDict using cURL: %s" err))
    (or reply (error "Got empty reply from SilverDict"))))

(defun goldendict--query-api-call (query)
  (let (reply err)
    (anki-editor--fetch
     (url-encode-url
      (format "http://%s:%s/api/query/Default Group/%s"
              goldendict-api-host
              goldendict-api-port
              query))
     :type "GET"
     :success (cl-function (lambda (&key data &allow-other-keys)
                             (setq reply data)))
     :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                           (setq err (string-trim (cdr error-thrown)))))
     :sync t)
    (when err (error "Error communicating with SilverDict using cURL: %s" err))
    (or reply (error "Got empty reply from SilverDict"))))

(defun goldendict--suggestions-api-call-result (query)
  (let-alist (goldendict--suggestions-api-call query)
    (when .error (error .error))
    .suggestions))

(defun goldendict--render-html (content)
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (let (dom)
    (with-current-buffer (get-buffer-create goldendict-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (dolist (replace-string-rule goldendict-html-replace-string-rule-lists)
          (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
        (setq dom (libxml-parse-html-region (point-min) (point-max)))
        (erase-buffer)
        (shr-insert-document dom)
        (goto-char (point-min)))
      (unless (get-buffer-window (current-buffer))
        (switch-to-buffer-other-window goldendict-buffer-name)
        (goldendict-mode)
        ))))

(defun goldendict--pop-posframe-toggle (suggestion)
  (unless (and goldendict-frame
               (frame-live-p goldendict-frame))
    (let ((width  (max 100 (round (* (frame-width) 0.62))))
          (height (round (* (frame-height) 0.62))))
      (goldendict--render-html (goldendict--query-api-call suggestion))
      (setq goldendict-frame
            (posframe-show
             goldendict-buffer-name
             :poshandler #'posframe-poshandler-frame-center
             :hidehandler nil
             :left-fringe 8
             :right-fringe 8
             :width width
             :height height
             :min-width width
             :min-height height
             :border-width 2
             :border-color "light gray"
             :background-color (face-background 'tooltip nil t)
             :override-parameters '((cursor-type . t))
             :accept-focus t))
      (with-current-buffer goldendict-buffer-name
        (setq-local cursor-type 'box)
        (read-only-mode)
        (keymap-local-set "s-p" #'posframe-hide-all))))
  (goldendict--render-html (goldendict--query-api-call suggestion))
  (select-frame-set-input-focus goldendict-frame))

(define-derived-mode goldendict-mode fundamental-mode "GoldenDict"
  "Major mode for viewing golden dictionary result.
\\{goldendict-mode-map}"
  (read-only-mode 1))

(defun hurricane//goldendict-quit-window-and-switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)"
  (interactive)
  (quit-window)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))

(with-eval-after-load 'evil-evilified-state
  (evilified-state-evilify-map goldendict-mode-map
    :mode goldendict-mode
    :bindings
    "q" #'hurricane//goldendict-quit-window-and-switch-to-minibuffer-window
    "y" #'hurricane//evil-yank
    "Y" #'hurricane/yank-to-end-of-line
    ))

(defun hurricane/goldendict-find (&optional query)
  (interactive (list (read-string "GoldenDict query: " (hurricane//region-or-word))))
  (let ((suggestions (goldendict--suggestions-api-call-result query)))
    (if (and suggestions (called-interactively-p 'interactive))
        (ivy-read "Select a suggestion to preview: "
                  (mapcar
                   (lambda (x)
                     (format "%s" x)
                     )
                   suggestions)
                  :action (lambda (suggestion)
                            ;; (goldendict--pop-posframe-toggle suggestion)
                            (goldendict--render-html
                             (goldendict--query-api-call suggestion))
                            )))))
;; }}

(defun hurricane//popweb-silver-dict-query (suggestion)
  (let* ((popweb-org-roam-link-popup-window-height-scale 0.8)
         (popweb-org-roam-link-popup-window-width-scale 0.8)
         (html-string (goldendict--query-api-call suggestion))
         (html-replace-string-rule-lists
          '(("<font face=\"Kingsoft Phonetic Plain, Tahoma\" color=#FF6600>[^>]*?>" . "")
            ("<audio\[^>\]*>" . "")
            ("<span" . "<div")
            ("</span>" . "</div>"))))

    (if html-string
        (if (not (eq html-string popweb-org-roam-link-preview--previous-html))
            (progn
              (setq popweb-org-roam-link-preview--previous-html html-string)
              (setq processed-html-string
                    (with-temp-buffer
                      (insert html-string)
                      (dolist (replace-string-rule goldendict-html-replace-string-rule-lists)
                        (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
                      (buffer-string)))
              (if popweb-org-roam-link-preview-window-visible-p
                  (progn
                    (setq popweb-org-roam-link-preview-window-visible-p nil)
                    (ignore-errors
                      (popweb-call-async "hide_web_window" "silver_dict"))))
              (popweb-start 'popweb-org-roam-link-preview (list t processed-html-string nil nil))))
      (popweb-start 'popweb-org-roam-link-preview (list nil "Hello world" nil nil))))
  (add-hook 'post-command-hook #'popweb-org-roam-link-preview-window-hide-after-move))

(defun hurricane/reveal-cut-video (&optional if-get-timestamp-from-property)
  (interactive "P")
  (let (full-file-path
        video-timestamp-start
        video-timestamp-stop
        (audio-duration (unless if-get-timestamp-from-property (or (and (org-entry-get (point) "AUDIO_DURATION_MS") (string-to-number (org-entry-get (point) "AUDIO_DURATION_MS"))) (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start))))))
    ;; 适用于剪切过渡动画素材，没有对应的 tts 文件 AUDIO_DURATION_MS 做参考，需要手动设置 start，stop。
    (if if-get-timestamp-from-property
        (progn
          (setq video-timestamp-start (* 1000 (string-to-number (org-entry-get (point) "MATERIAL_TIMESTAMP_START"))))
          (setq video-timestamp-stop (* 1000 (string-to-number (org-entry-get (point) "MATERIAL_TIMESTAMP_STOP"))))
          (setq audio-duration (- video-timestamp-stop video-timestamp-start))))
    (and (org-entry-get (point) "MATERIAL_VIDEO") (setq full-file-path (expand-file-name (org-entry-get (point) "MATERIAL_VIDEO") reveal-project-directory)))
    (make-directory (file-name-concat reveal-project-directory "static" (file-name-sans-extension (buffer-name)) "temp") t)
    (python-bridge-call-async "mpv_cut_video" (list (subed-mpv--socket)
                                                    ;; output full file path
                                                    (or full-file-path
                                                        (format
                                                         "%sstatic/%s/temp/material_video_cut_%s.mp4"
                                                         (expand-file-name reveal-project-directory)
                                                         (file-name-sans-extension (buffer-name))
                                                         (format-time-string "%Y_%m_%d_%-I_%M_%p")))
                                                    ;; start timestamp
                                                    (if if-get-timestamp-from-property
                                                        (replace-regexp-in-string
                                                         "," "."
                                                         (compile-media-msecs-to-timestamp
                                                          video-timestamp-start))
                                                      (and subed-mpv-playback-position
                                                           (replace-regexp-in-string
                                                            "," "."
                                                            (compile-media-msecs-to-timestamp
                                                             subed-mpv-playback-position))))
                                                    ;; stop timestamp
                                                    (if if-get-timestamp-from-property
                                                        (replace-regexp-in-string
                                                         "," "."
                                                         (compile-media-msecs-to-timestamp
                                                          video-timestamp-stop))
                                                      (and subed-mpv-playback-position
                                                           (replace-regexp-in-string
                                                            "," "."
                                                            (compile-media-msecs-to-timestamp
                                        ;；多剪切 6 秒钟留白。
                                                             (+ 4000
                                                                subed-mpv-playback-position
                                                                audio-duration)))))
                                                    ;; duration
                                        ;；多剪切 6 秒钟留白。
                                                    (if if-get-timestamp-from-property
                                                        audio-duration
                                                      (+ 4000 audio-duration))
                                                    "get_property"
                                                    "path"))))

(defun hurricane/reveal-cut-backgroundmusic ()
  (interactive)
  (let (full-file-path
        (prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
        (audio-duration (or (and (org-entry-get (point) "AUDIO_DURATION_MS") (string-to-number (org-entry-get (point) "AUDIO_DURATION_MS"))) (- (subed-subtitle-msecs-stop) (subed-subtitle-msecs-start)))))
    (when (string-match "data-backgroundmusic-src=\"\\(.+?\\)\"" prop)
      (setq full-file-path (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (python-bridge-call-async "mpv_cut_video" (list (subed-mpv--socket)
                                                    ;; output full file path
                                                    (or full-file-path
                                                        (format
                                                         "%sstatic/%s/temp/backgroundmusic_cut_%s.mp3"
                                                         (expand-file-name reveal-project-directory)
                                                         (file-name-sans-extension (buffer-name))
                                                         (format-time-string "%Y_%m_%d_%-I_%M_%p")))
                                                    ;; start timestamp
                                                    (and subed-mpv-playback-position
                                                         (replace-regexp-in-string
                                                          "," "."
                                                          (compile-media-msecs-to-timestamp
                                                           subed-mpv-playback-position)))
                                                    ;; stop timestamp
                                                    (and subed-mpv-playback-position
                                                         (replace-regexp-in-string
                                                          "," "."
                                                          (compile-media-msecs-to-timestamp
                                                           (+ subed-mpv-playback-position
                                                              audio-duration))))
                                                    ;; duration
                                                    audio-duration
                                                    "get_property"
                                                    "path"))))

(defun hurricane/reveal--cut-media (final-cmd full-file-path)
  (setq reveal-cut-media-output-file full-file-path)
  (let* ((final-cmd final-cmd)
         (buffer (get-buffer-create "*ffmpeg reveal cut media*"))
         (process
          (start-process-shell-command
           "hurricane/reveal--cut-media"
           buffer
           final-cmd)))

    (message "%s"  final-cmd)

    (set-process-sentinel
     process
     (lambda (proc event)
       (when (equal event "finished\n")
         (if (equal "mp4" (file-name-extension reveal-cut-media-output-file))
             (progn
               (insert (concat "#+REVEAL_HTML: <video muted width=\"100%\" height=\"100%\" preload=\"auto\" src=\"" reveal-cut-media-output-file "\">"))
               (hurricane/slide-material-video (string-trim reveal-cut-media-output-file) nil))
           (message "Cut media %s finished." reveal-cut-media-output-file))
         )))
    t))

(defvar hurricane//note-words-target 2800)

(defun hurricane//org-collect-notes ()
  (let (results)
    (org-block-map
     (lambda ()
       (unless (org-in-commented-heading-p)
         (let ((elem (org-element-at-point)))
           (when (string= (org-element-property :type elem) "notes")
             (setq results
                   (cons (string-trim
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem)))
                         results)))))))
    (reverse results)))

(defun hurricane/org-create-notes-buffer ()
  (interactive)
  (let ((notes (hurricane//org-collect-notes)))
    (with-current-buffer (get-buffer-create "*Notes*")
      (insert (string-join notes "\n\n"))
      (switch-to-buffer (current-buffer)))))

(defun hurricane/count-words-in-notes ()
  (interactive)
  (let ((notes (hurricane//org-collect-notes)))
    (with-temp-buffer
      (insert (string-join notes "\n"))
      (let ((num (count-words-region (point-min) (point-max))))
        (message "%d words (%.f%% of %d, %d to go)"
                 num
                 (/ (* 100.0 num) hurricane//note-words-target)
                 my-note-words-target
                 (- hurricane//note-words-target num))))))

(defun hurricane//reveal-slide-id ()
  (replace-regexp-in-string
   "^-\\|-$" ""
   (replace-regexp-in-string
    "[^A-Za-z0-9]+" "-"
    (downcase (concat (string-join (org-get-outline-path) "-") "-" (org-entry-get (point) "ITEM"))))))

(defun hurricane/reveal-notes-media ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-entry-put
      (point)
      "REVEAL_EXTRA_ATTR"
      (format "data-audio-src=\"static/%s/%s.opus\" data-track-src=\"static/%s/%s.ass\" data-video-src=\"static/%s/%s.mp4\""
              (file-name-sans-extension (buffer-name)) (hurricane//reveal-slide-id)
              (file-name-sans-extension (buffer-name)) (hurricane//reveal-slide-id)
              (file-name-sans-extension (buffer-name)) (hurricane//reveal-slide-id)
              )))
   "-REVEAL_EXTRA_ATTR={.}"))

(defvar-local reveal-comment-block-position-list (list))

(defun hurricane/reveal-comment-block-translate ()
  (interactive)
  (org-block-map
   (lambda ()
     (unless (org-in-commented-heading-p)
       (let ((text (org-element-property :value (org-element-at-point)))
             (position (point)))
         (when (and text position)
           (add-to-list 'reveal-comment-block-position-list position t)
           )))))
  (mapcar
   (lambda (comment-pos)
     (progn
       (goto-char comment-pos)
       (insert (format "#+begin_notes\n%s\n#+end_notes\n" (mapconcat (lambda (trans) (concat "" trans)) (assoc-default 'translation (youdao-dictionary--request (org-element-property :value (org-element-at-point)))) "\n")))
       (newline-and-indent)
       ))
   (reverse reveal-comment-block-position-list)))

(defun hurricane/reveal-notes-tts ()
  (interactive)
  (let ((ms 0)
        results)
    (org-block-map
     (lambda ()
       (unless (org-in-commented-heading-p)
         (let ((elem (org-element-at-point)))
           (when (string= (org-element-property :type elem) "notes")
             (let ((text (string-trim
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
                   audio-output
                   prev-fragment
                   prev-heading
                   prop)
               (save-excursion (setq prev-heading (org-back-to-heading)))
               (save-excursion (setq prev-fragment (re-search-backward "#\\+ATTR_REVEAL:.*:audio \\(.+\\)" prev-heading t)))
               (if prev-fragment
                   (setq audio-output (match-string 1))
                 (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
                 (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
                   (setq audio-output (expand-file-name (match-string 1 prop) default-directory))))
               (emacs-azure-tts text audio-output t)
               ))))))
    ))

(defun hurricane/cache-media-duration (&optional video)
  (interactive "P")
  (let ((proj-dir reveal-project-directory))
    (org-map-entries
     (lambda ()
       (let ((prop (org-entry-get (point) "REVEAL_EXTRA_ATTR")))
         (when (and prop (string-match (format "static/%s/[-A-Za-z0-9]+?\\.%s" (file-name-sans-extension (buffer-name)) (if video "mp4" "opus")) prop))
           (message "%s" (expand-file-name (match-string 0 prop) proj-dir))
           (org-entry-put (point) (if video "VIDEO_DURATION_MS" "AUDIO_DURATION_MS")
                          (number-to-string
                           (compile-media-get-file-duration-ms
                            (expand-file-name (match-string 0 prop)
                                              proj-dir)))))))
     "REVEAL_EXTRA_ATTR={.}")))

;; 以下格式才能获取成功
;; [[video:static/《上海十三太保》vs《十月围城》/1.mp4]]
;; 不支持以下格式
;; #+REVEAL_HTML: <video width="100%" height="100%" preload="auto" src="static/《上海十三太保》vs《十月围城》/1.mp4">
(defun hurricane/reveal-cache-video-duration (&optional force)
  (interactive)
  (let ((proj-dir reveal-project-directory))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(?:video:\\([-~/A-Za-z0-9]+.webm\\)\\|\\(https:[^ ]+\\.gif\\)\\|file:\\([-A-Za-z0-9:]+.gif\\)\\)" nil t)
        (unless (and (null force) (save-match-data (org-entry-get (point) "VIDEO_DURATION_MS")))
          (org-entry-put (point) "VIDEO_DURATION_MS"
                         (number-to-string
                          (compile-media-get-file-duration-ms
                           (or (expand-file-name (match-string 1))
                               (match-string 2)
                               (expand-file-name (match-string 3) proj-dir))))))))))

(defun hurricane/reveal-notes-vtt ()
  (interactive)
  (let ((ms 0)
        results)
    (org-block-map
     (lambda ()
       (unless (org-in-commented-heading-p)
         (let ((elem (org-element-at-point)))
           (when (string= (org-element-property :type elem) "notes")
             (let ((text (string-trim
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
                   audio-output
                   prev-fragment
                   prev-heading
                   prop)
               (save-excursion (setq prev-heading (org-back-to-heading)))
               (save-excursion (setq prev-fragment (re-search-backward "#\\+ATTR_REVEAL:.*:audio \\(.+\\)" prev-heading t)))
               (if prev-fragment
                   (setq audio-output (match-string 1))
                 (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
                 (setq audio-duration (string-to-number (org-entry-get (point) "AUDIO_DURATION_MS")))
                 (setq material-video (org-entry-get (point) "MATERIAL_VIDEO"))
                 (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
                   (setq audio-output (match-string 1 prop))))
               (add-to-list
                'results
                (list nil ms (+ audio-duration ms)
                      text
                      (format "#+OUTPUT: %s\n#+MATERIAL_VIDEO: %s" audio-output material-video))
                t)
               (setq ms (+ audio-duration ms))))))))
    (with-current-buffer (get-buffer-create (format "%s.vtt" (file-name-sans-extension (buffer-name))))
      (erase-buffer)
      (subed-vtt-mode)
      (subed-auto-insert)
      (subed-append-subtitle-list results)
      (display-buffer (current-buffer)))))

(defun hurricane/reveal-slide-vtt ()
  (interactive)
  (org-narrow-to-subtree)
  (let ((ms 0)
        audio-output
        results)
    (org-block-map
     (lambda ()
       (unless (org-in-commented-heading-p)
         (let ((elem (org-element-at-point)))
           (when (string= (org-element-property :type elem) "notes")
             (let ((text (string-trim
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
                   prev-fragment
                   prev-heading
                   prop)
               (save-excursion (setq prev-heading (org-back-to-heading)))
               (save-excursion (setq prev-fragment (re-search-backward "#\\+ATTR_REVEAL:.*:audio \\(.+\\)" prev-heading t)))
               (if prev-fragment
                   (setq audio-output (match-string 1))
                 (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
                 (setq audio-duration (string-to-number (org-entry-get (point) "AUDIO_DURATION_MS")))
                 (setq material-video (org-entry-get (point) "MATERIAL_VIDEO"))
                 (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
                   (setq audio-output (match-string 1 prop))))
               (add-to-list
                'results
                (list nil ms (+ audio-duration ms)
                      text
                      (format "#+OUTPUT: %s\n#+MATERIAL_VIDEO: %s" audio-output material-video))
                t)
               (setq ms (+ audio-duration ms))))))))
    (with-current-buffer (find-file-noselect (format "%s.vtt" (expand-file-name (file-name-sans-extension audio-output) reveal-project-directory)))
      (erase-buffer)
      (subed-vtt-mode)
      (subed-auto-insert)
      (subed-append-subtitle-list results)
      (display-buffer (current-buffer)))))

;; aeneas 中文识别使用 eSpeak-ng
;; 安装：sudo port install espeak-ng
                                        ;；使用：https://github.com/espeak-ng/espeak-ng/blob/master/src/espeak-ng.1.ronn
                                        ;；使用：https://github.com/readbeyond/aeneas/issues/214
(defun hurricane/reveal-slide-txt ()
  (interactive)
  (org-narrow-to-subtree)
  (require 'subed-align)
  (let ((ms 0)
        (subed-align-language "cmn")
        (subed-align-options "-r=\"tts=espeak-ng\"")
        audio-output
        results)
    (org-block-map
     (lambda ()
       (unless (org-in-commented-heading-p)
         (let ((elem (org-element-at-point)))
           (when (string= (org-element-property :type elem) "notes")
             (let ((text (string-trim
                          (buffer-substring-no-properties
                           (org-element-property :contents-begin elem)
                           (org-element-property :contents-end elem))))
                   prev-fragment
                   prev-heading
                   prop)
               (save-excursion (setq prev-heading (org-back-to-heading)))
               (save-excursion (setq prev-fragment (re-search-backward "#\\+ATTR_REVEAL:.*:audio \\(.+\\)" prev-heading t)))
               (if prev-fragment
                   (setq audio-output (match-string 1))
                 (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
                 (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
                   (setq audio-output (match-string 1 prop))))
               (setq results text)))))))
    (with-current-buffer (find-file-noselect (format "%s.txt" (expand-file-name (file-name-sans-extension audio-output) reveal-project-directory)))
      (erase-buffer)
      (insert results)
      (display-buffer (current-buffer))
      (funcall-interactively 'subed-align (expand-file-name audio-output reveal-project-directory) (buffer-file-name) "VTT"))))

(defun hurricane/slide-material-video (&optional init-video if-losslesscut)
  (interactive nil "P")
  (org-narrow-to-subtree)
  (org-map-entries
   (lambda ()
     (unless (org-in-commented-heading-p)
       (let ((material-video (select-or-enter-file-name (expand-file-name (format "static/%s/" (file-name-sans-extension (buffer-name))) reveal-project-directory) init-video)))
         (org-entry-put
          (point)
          (if if-losslesscut "LOSSLESSCUT_MATERIAL_VIDEO" "MATERIAL_VIDEO")
          (file-relative-name material-video default-directory)))))))

(defun hurricane/slide-material-video-timestamp-start ()
  (interactive)
  (org-narrow-to-subtree)
  (org-map-entries
   (lambda ()
     (unless (org-in-commented-heading-p)
       (python-bridge-call-async "mpv_get_time_position" (list (subed-mpv--socket) (point) "MATERIAL_TIMESTAMP_START" "get_property" "time-pos"))))))

(defun hurricane/slide-material-video-timestamp-stop ()
  (interactive)
  (org-narrow-to-subtree)
  (org-map-entries
   (lambda ()
     (unless (org-in-commented-heading-p)
       (python-bridge-call-async "mpv_get_time_position" (list (subed-mpv--socket) (point) "MATERIAL_TIMESTAMP_STOP" "get_property" "time-pos"))))))

(defun hurricane//slide--material-video-timestamp (point start-or-stop sec)
  (progn
    (goto-char (string-to-number point))
    (org-entry-put
     (point)
     start-or-stop
     sec)))

(defun hurricane/create-proj-llc ()
  (interactive)
  (let (losslesscut-material-video
        video-timestamp-start
        video-timestamp-stop
        video-segment-name
        (result (make-hash-table :test 'equal)))
    (org-map-entries
     (lambda ()
       (unless (org-in-commented-heading-p)
         (setq losslesscut-material-video (org-entry-get (point) "LOSSLESSCUT_MATERIAL_VIDEO"))
         (setq video-timestamp-start (string-to-number (org-entry-get (point) "MATERIAL_TIMESTAMP_START")))
         (setq video-timestamp-stop (string-to-number (org-entry-get (point) "MATERIAL_TIMESTAMP_STOP")))
         (setq video-segment-name (expand-file-name (format "static/%s/%s.mp4" (file-name-sans-extension (buffer-name)) (hurricane//reveal-slide-id)) reveal-project-directory))
         (if (gethash `,losslesscut-material-video result)
             (progn
               (setq cut-segments (plist-get (gethash `,losslesscut-material-video result) :cutSegments))
               ;; push 不能 append，所以使用 add-to-list，注意细微区别，cut-segments 的处理。
               ;; (setq new-segments (push `(:start ,video-timestamp-start :end ,video-timestamp-stop) `,cut-segments))
               (setq new-segments (add-to-list 'cut-segments `((:start . ,video-timestamp-start) (:end . ,video-timestamp-stop) (:name . ,video-segment-name)) t))
               (remhash `,losslesscut-material-video result)
               (puthash `,losslesscut-material-video `(:version 1 :mediaFileName ,losslesscut-material-video :cutSegments ,new-segments) result)
               )
           (puthash `,losslesscut-material-video `(:version 1 :mediaFileName ,losslesscut-material-video :cutSegments (((:start . ,video-timestamp-start) (:end . ,video-timestamp-stop) (:name . ,video-segment-name)))) result ))
         )))
    (maphash
     (lambda (k v)
       (with-current-buffer (find-file-noselect (expand-file-name (format "static/%s/%s.llc" (file-name-sans-extension (buffer-name)) k) reveal-project-directory))
         (erase-buffer)
         (insert (json-encode v))
         (display-buffer (current-buffer))))
     result)))

(defun hurricane/convert-fade-images-to-video ()
  (interactive)
  (org-narrow-to-subtree)
  (setq reveal-convert-fade-images-to-video-output nil)
  (let (image-file-source-list
        audio-source)
    (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
      (push (expand-file-name (string-trim (match-string 1)) reveal-project-directory) image-file-source-list))
    (let ((index 0)
          merge-temp-output
          tts-temp-output
          track-temp-output
          temp-output
          audio-source
          track-source
          backgroundmusic-source
          (audio-duration (org-entry-get (point) "AUDIO_DURATION_MS"))
          image-configurations
          filter-complex-configurations-prefix
          filter-complex-configurations-append
          final-cmd
          (image-file-source-list-length (length image-file-source-list)))

      (if (equal 1 image-file-source-list-length)
          (progn
            (setq filter-complex-configurations-prefix "[0:v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2,setsar=1,fade=t=in:st=0:d=1[v0];")
            (setq filter-complex-configurations-append "[v0]"))
        (while (< index image-file-source-list-length)
          (if (equal (+ 1 index) image-file-source-list-length)
              (progn
                (setq filter-complex-configurations-append (concat filter-complex-configurations-append (format "[v%s]" index)))
                (setq filter-complex-configurations-prefix (concat filter-complex-configurations-prefix " " (format "[%s:v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2,setsar=1,fade=t=in:st=0:d=1[v%s];" index index))))
            (progn
              (setq filter-complex-configurations-append (concat filter-complex-configurations-append (format "[v%s]" index)))
              (setq filter-complex-configurations-prefix (concat filter-complex-configurations-prefix " " (format "[%s:v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2,setsar=1,fade=t=in:st=0:d=1,fade=t=out:st=4:d=1[v%s];" index index)))))
          (setq index (+ 1 index))
          ))

      (dolist (image-file-source (nreverse image-file-source-list))
        (setq image-configurations (concat image-configurations " -loop 1 -t 5 " "-i " "\"" image-file-source "\"")))

      (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
      (when (string-match "data-video-src=\"\\(.+?\\)\"" prop)
        (setq reveal-convert-fade-images-to-video-output (expand-file-name (match-string 1 prop) reveal-project-directory))
        (make-directory (expand-file-name (file-name-concat (file-name-directory reveal-convert-fade-images-to-video-output) "temp") reveal-project-directory) t)
        (setq merge-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-fade-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-fade-images-to-video-output))) "_merge_temp" ".mp4"))
        (setq tts-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-fade-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-fade-images-to-video-output))) "_tts_temp" ".mp4"))
        (setq track-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-fade-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-fade-images-to-video-output))) "_track_temp" ".mp4"))
        (setq temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-fade-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-fade-images-to-video-output))) "_temp" ".mp4")))
      (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
        (setq audio-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
      (when (string-match "data-track-src=\"\\(.+?\\)\"" prop)
        (setq track-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
      (when (string-match "data-backgroundmusic-src=\"\\(.+?\\)\"" prop)
        (setq backgroundmusic-source (expand-file-name (match-string 1 prop) reveal-project-directory)))

      (cond ((and (file-exists-p audio-source) (file-exists-p track-source) (file-exists-p backgroundmusic-source))
             (progn
               ;; https://shotstack.io/learn/use-ffmpeg-to-convert-images-to-video/
               (setq merge-cmd (format "ffmpeg%s -filter_complex \"%s %sconcat=n=%s:v=1:a=0,format=yuv420p[v]\" -map \"[v]\" -y \"%s\"" image-configurations filter-complex-configurations-prefix filter-complex-configurations-append image-file-source-list-length merge-temp-output))
               ;; 配音声音在画面出现两秒后出现。
               (setq tts-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1:a] adelay=2000|2000 [voice];[voice] amix=inputs=1:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" merge-temp-output audio-source tts-temp-output))
               ;; 合并字幕文件。
               (setq track-cmd (format "ffmpeg -i \"%s\" -vf ass=\"%s\" -c:v h264 -b:v 6m -c:a copy -y \"%s\"" tts-temp-output track-source track-temp-output))
               ;; 合并背景音乐文件。
               (setq backgroundmusic-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1] volume=0.01 [aud1];[aud1] afade=t=in:st=0:d=3 [aud2];[aud2] afade=t=out:st=%s:d=3 [aud3];[0:a][aud3]amix=inputs=2:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" track-temp-output backgroundmusic-source (- (string-to-number audio-duration) 2) reveal-convert-fade-images-to-video-output))
               (setq final-cmd (concat merge-cmd "&&" tts-cmd "&&" track-cmd "&&" backgroundmusic-cmd))))
            (t
             (setq final-cmd (format "ffmpeg%s -filter_complex \"%s %sconcat=n=%s:v=1:a=0,format=yuv420p[v]\" -map \"[v]\" -y \"%s\"" image-configurations filter-complex-configurations-prefix filter-complex-configurations-append image-file-source-list-length reveal-convert-fade-images-to-video-output))))

      (message "%s" final-cmd)

      (let* ((buffer (get-buffer-create "*ffmpeg convert fade images to video*"))
             (process
              (start-process-shell-command
               "hurricane/reveal-convert-fade-images-to-video"
               buffer
               final-cmd)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (equal event "finished\n")
             (message "Convert fade images to video: %s finished." reveal-convert-fade-images-to-video-output)
             ))))
      )
    )
  )

;; https://stackoverflow.com/questions/37327163/ffmpeg-input-link-in1v0-parameters-size-640x640-sar-169-do-not-match-the
;; https://creatomate.com/blog/how-to-zoom-images-and-videos-using-ffmpeg
(defun hurricane/convert-zoom-images-to-video ()
  (interactive)
  (org-narrow-to-subtree)
  (setq reveal-convert-zoom-images-to-video-output nil)
  (let (image-file-source-list
        audio-source)
    (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
      (push (list (expand-file-name (string-trim (match-string 1)) reveal-project-directory) (expand-file-name (file-name-concat (file-name-directory (string-trim (match-string 1))) "temp" (concat (file-name-sans-extension (file-name-nondirectory (string-trim (match-string 1)))) "_zoom_temp.mp4")) reveal-project-directory) (buffer-substring-no-properties (+ 11 (org-element-property :begin (org-element-at-point))) (- (org-element-property :contents-begin (org-element-at-point)) 1))) image-file-source-list))
    (let ((index 0)
          zoom-temp-output-list
          merge-temp-output
          tts-temp-output
          track-temp-output
          audio-source
          track-source
          backgroundmusic-source
          (audio-duration (org-entry-get (point) "AUDIO_DURATION_MS"))
          merge-video-configurations
          zoom-temp-cmd-list
          final-cmd)
      (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
      (when (string-match "data-video-src=\"\\(.+?\\)\"" prop)
        (setq reveal-convert-zoom-images-to-video-output (expand-file-name (match-string 1 prop) reveal-project-directory))
        (make-directory (expand-file-name (file-name-concat (file-name-directory reveal-convert-zoom-images-to-video-output) "temp") reveal-project-directory) t)
        (setq merge-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-zoom-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-zoom-images-to-video-output))) "_merge_temp" ".mp4"))
        (setq tts-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-zoom-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-zoom-images-to-video-output))) "_tts_temp" ".mp4"))
        (setq track-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory reveal-convert-zoom-images-to-video-output) "temp" (file-name-nondirectory reveal-convert-zoom-images-to-video-output))) "_track_temp" ".mp4")))
      (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
        (setq audio-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
      (when (string-match "data-track-src=\"\\(.+?\\)\"" prop)
        (setq track-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
      (when (string-match "data-backgroundmusic-src=\"\\(.+?\\)\"" prop)
        (setq backgroundmusic-source (expand-file-name (match-string 1 prop) reveal-project-directory)))

      (dolist (image-file-source (nreverse image-file-source-list))
        (add-to-list 'zoom-temp-output-list (format "-i \"%s\"" (nth 1 image-file-source)) t)
        (setq merge-video-configurations (concat merge-video-configurations (format "[%s]setdar=16/9%s;" index (substring "[a][b][c][d][e][f][g][h][i][j][k][l][m][n][o][p][q][r][s][t][u][v][w][x][y][z]" (* 3 index) (* 3 (+ 1 index))))))
        (add-to-list 'zoom-temp-cmd-list (format "ffmpeg %s -t 5 -c:v libx264 -pix_fmt yuv420p \"%s\"" (format (nth 2 image-file-source) (nth 0 image-file-source)) (nth 1 image-file-source)) t)
        (setq index (+ 1 index)))
      (setq zoom-cmd (string-join zoom-temp-cmd-list "&&"))

      (setq merge-video-configurations (format "\"%s%s\"" merge-video-configurations (format "%sconcat=n=%s:v=1" (substring "[a][b][c][d][e][f][g][h][i][j][k][l][m][n][o][p][q][r][s][t][u][v][w][x][y][z]" 0 (* 3 index)) (number-to-string index))))
      (setq merge-video-configurations (concat (string-join zoom-temp-output-list " ") " -filter_complex " merge-video-configurations))

      (cond ((and (file-exists-p audio-source) (file-exists-p track-source) (file-exists-p backgroundmusic-source))
             (progn
               ;; 合并图片生成的视频片段。
               (setq merge-cmd (format "ffmpeg %s -y \"%s\"" merge-video-configurations merge-temp-output))
               ;; 合并配音文件。
               ;; (setq tts-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -vcodec copy -acodec copy \"%s\"" merge-temp-output audio-source tts-temp-output))
               ;; 配音声音在画面出现两秒后出现。
               (setq tts-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1:a] adelay=2000|2000 [voice];[voice] amix=inputs=1:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" merge-temp-output audio-source tts-temp-output))
               ;; 合并字幕文件。
               (setq track-cmd (format "ffmpeg -i \"%s\" -vf ass=\"%s\" -c:v h264 -b:v 6m -c:a copy -y \"%s\"" tts-temp-output track-source track-temp-output))
               ;; 合并背景音乐文件。
               ;; merge-temp-output 的时长必须大于 audio-source，否则 tts-temp-output 播放时
               ;; video 画面在走完时长后停止，而 tts audio 继续播放。
               ;; backgroundmusic-source 在被合并时，tts audio 只被提取并合并至停止画面的时间戳，backgroundmusci-source 则合并至 tts audio 的完整时间戳。
               ;; 所以 reveal-convert-zoom-images-to-video-output 反而是在停止画面后没有 tts audio，却有 backgroundmusic-source。
               (setq backgroundmusic-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1] volume=0.01 [aud1];[aud1] afade=t=in:st=0:d=3 [aud2];[aud2] afade=t=out:st=%s:d=3 [aud3];[0:a][aud3] amix=inputs=2:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" track-temp-output backgroundmusic-source (- (string-to-number audio-duration) 2) reveal-convert-zoom-images-to-video-output))
               (setq final-cmd (concat zoom-cmd "&&" merge-cmd "&&" tts-cmd "&&" track-cmd "&&" backgroundmusic-cmd))))
            (t
             (setq merge-cmd (format "ffmpeg %s -y \"%s\"" merge-video-configurations reveal-convert-zoom-images-to-video-output))
             (setq final-cmd (concat zoom-cmd "&&" merge-cmd))))

      (message "%s" final-cmd)

      (let* ((buffer (get-buffer-create "*ffmpeg convert zoom images to video*"))
             (process
              (start-process-shell-command
               "hurricane/reveal-convert-zoom-images-to-video"
               buffer
               final-cmd)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (when (equal event "finished\n")
             (message "Convert zoom images to video: %s finished." reveal-convert-zoom-images-to-video-output)
             ))))
      )
    )
  )

(defun hurricane/create-transition ()
  (interactive)
  (setq create-transition-output nil)
  (let ((material-transition-video (expand-file-name (org-entry-get (point) "MATERIAL_TRANSITION_VIDEO") reveal-project-directory))
        (material-transition-music (expand-file-name (org-entry-get (point) "MATERIAL_TRANSITION_MUSIC") reveal-project-directory))
        (transition-music-delay (org-entry-get (point) "TRANSITION_MUSIC_DELAY"))
        (drawtext-fontcolor (org-entry-get (point) "DRAWTEXT_FONTCOLOR"))
        (drawtext-fontsize (org-entry-get (point) "DRAWTEXT_FONTSIZE"))
        (drawtext-fontfile (org-entry-get (point) "DRAWTEXT_FONTFILE"))
        (drawtext-text (org-entry-get (point) "DRAWTEXT_TEXT"))
        (transition-temp-output (format
                                 "%sstatic/%s/temp/transition_%s.mp4"
                                 (expand-file-name reveal-project-directory)
                                 (file-name-sans-extension (buffer-name))
                                 (format-time-string "%Y_%m_%d_%-I_%M_%p"))))
    (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
    (when (string-match "data-video-src=\"\\(.+?\\)\"" prop)
      (setq create-transition-output (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (setq backgroundmusic-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1:a] adelay=%s|%s [audio_out]\" -map 0:v -map \"[audio_out]\" -shortest -y \"%s\"" material-transition-video material-transition-music transition-music-delay transition-music-delay transition-temp-output))
    (setq drawtext-cmd (format "ffmpeg -i \"%s\" -vf \"drawtext=alpha=if(lt(t\\,0.3)\\,0\\,if(lt(t\\,1.3)\\,(t-0.3)/1\\,if(lt(t\\,3)\\,1\\,if(lt(t\\,4)\\,(1-(t-3))/1\\,0)))):fontcolor=%s:fontsize=%s:fontfile='%s':text='%s':x=(w-text_w)/2:y=(h-text_h)/2\" -c:v libx264 -c:a copy -movflags +faststart -y \"%s\"" transition-temp-output drawtext-fontcolor drawtext-fontsize drawtext-fontfile drawtext-text create-transition-output))

    (setq final-cmd (concat backgroundmusic-cmd "&&" drawtext-cmd))

    (message "%s" final-cmd)

    (let* ((buffer (get-buffer-create "*ffmpeg create transition*"))
           (process
            (start-process-shell-command
             "hurricane/create-transition"
             buffer
             final-cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (equal event "finished\n")
           (message "Create transition: %s finished." create-transition-output)
           ))))
    )
  )

(defun hurricane/ffmpeg-chapter-concat ()
  (interactive)
  (setq ffmpeg-chapter-concat-output-file (expand-file-name (format "static/%s/final.mp4" (file-name-sans-extension (buffer-name)))  reveal-project-directory))
  (let ((index 0)
        input-configurations
        concat-configurations)
    (dolist (chapter (read (hurricane//extract-value-from-keyword "CHAPTER_KEY")))
      (setq input-configurations (concat input-configurations  " -i " "\"" (expand-file-name (format "static/%s" (file-name-sans-extension (buffer-name))) reveal-project-directory) "/" (number-to-string chapter) ".mp4" "\""))
      (setq concat-configurations (concat concat-configurations (format "[%s:0][%s:1]" index index)))
      (setq index (+ 1 index))
      )
    (setq final-cmd (format "ffmpeg%s -filter_complex \"%s concat=n=%s:v=1:a=1[v][a]\" -map \"[v]\" -map \"[a]\" -c:v h264 -b:v 6m -y \"%s\"" input-configurations concat-configurations (length (read (hurricane//extract-value-from-keyword "CHAPTER_KEY"))) ffmpeg-chapter-concat-output-file))

    (message "%s" final-cmd)

    (let* ((buffer (get-buffer-create "*ffmpeg concat*"))
           (process
            (start-process-shell-command
             "hurricane/ffmpeg-concat"
             buffer
             final-cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (equal event "finished\n")
           (message "Concat: %s finished." ffmpeg-chapter-concat-output-file)
           ))))
    ))

(defun hurricane/ffmpeg-synthetic-process ()
  (interactive)
  (setq synthetic-process-output nil)
  (let (without-audio-temp-output
        tts-temp-output
        track-temp-output
        audio-source
        track-source
        backgroundmusic-source
        (audio-duration (org-entry-get (point) "AUDIO_DURATION_MS"))
        (material-video (expand-file-name (org-entry-get (point) "MATERIAL_VIDEO") reveal-project-directory))
        final-cmd)

    (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
    (when (string-match "data-video-src=\"\\(.+?\\)\"" prop)
      (setq synthetic-process-output (expand-file-name (match-string 1 prop) reveal-project-directory))
      (make-directory (expand-file-name (file-name-concat (file-name-directory synthetic-process-output) "temp") reveal-project-directory) t)
      (setq without-audio-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory synthetic-process-output) "temp" (file-name-nondirectory synthetic-process-output))) "_without_audio_temp" ".mp4"))
      (setq tts-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory synthetic-process-output) "temp" (file-name-nondirectory synthetic-process-output))) "_tts_temp" ".mp4"))
      (setq track-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory synthetic-process-output) "temp" (file-name-nondirectory synthetic-process-output))) "_track_temp" ".mp4")))

    (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
      (setq audio-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (when (string-match "data-track-src=\"\\(.+?\\)\"" prop)
      (setq track-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (when (string-match "data-backgroundmusic-src=\"\\(.+?\\)\"" prop)
      (setq backgroundmusic-source (expand-file-name (match-string 1 prop) reveal-project-directory)))

    (cond ((and (file-exists-p audio-source) (file-exists-p track-source) (file-exists-p backgroundmusic-source))
           (progn
             (setq remove-cmd (format "ffmpeg -i \"%s\" -c:v copy -an -y \"%s\"" material-video without-audio-temp-output))
             ;; 配音声音在画面出现两秒后出现。
             (setq tts-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1:a] adelay=2000|2000 [voice];[voice] amix=inputs=1:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" without-audio-temp-output audio-source tts-temp-output))
             ;; 合并字幕文件。
             (setq track-cmd (format "ffmpeg -i \"%s\" -vf ass=\"%s\" -c:v h264 -b:v 6m -c:a copy -y \"%s\"" tts-temp-output track-source track-temp-output))
             ;; 合并背景音乐文件。
             (setq backgroundmusic-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex '[1]volume=0.01[aud1];[aud1]afade=t=in:st=0:d=3[aud2];[aud2]afade=t=out:st=%s:d=3[aud3];[0:a][aud3]amix=inputs=2:duration=longest' -c:v copy -y \"%s\"" track-temp-output backgroundmusic-source (- (string-to-number audio-duration) 2) synthetic-process-output))
             (setq final-cmd (concat remove-cmd "&&" tts-cmd "&&" track-cmd "&&" backgroundmusic-cmd))))
          (t
           (setq final-cmd (format "ffmpeg -i \"%s\" -c:v copy -an -y \"%s\"" material-video synthetic-process-output))
           ))

    (message "%s" final-cmd)

    (let* ((buffer (get-buffer-create "*ffmpeg synthetic process*"))
           (process
            (start-process-shell-command
             "hurricane/ffmpeg-synthetic-process"
             buffer
             final-cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (equal event "finished\n")
           (message "Synthetic process: %s finished." synthetic-process-output)
           ))))
    )
  )

(defun hurricane/ffmpeg-subchapter-concat ()
  (interactive)
  (org-narrow-to-subtree)
  (setq ffmpeg-subchapter-concat-output-file nil)
  (let ((index 0)
        merge-temp-output
        tts-temp-output
        track-temp-output
        audio-source
        track-source
        backgroundmusic-source
        (audio-duration (org-entry-get (point) "AUDIO_DURATION_MS"))
        input-configurations
        concat-configurations)
    (setq prop (org-entry-get (point) "REVEAL_EXTRA_ATTR"))
    (when (string-match "data-video-src=\"\\(.+?\\)\"" prop)
      (setq ffmpeg-subchapter-concat-output-file (expand-file-name (match-string 1 prop) reveal-project-directory))
      (setq merge-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory ffmpeg-subchapter-concat-output-file) "temp" (file-name-nondirectory ffmpeg-subchapter-concat-output-file))) "_merge_temp" ".mp4"))
      (setq tts-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory ffmpeg-subchapter-concat-output-file) "temp" (file-name-nondirectory ffmpeg-subchapter-concat-output-file))) "_tts_temp" ".mp4"))
      (setq track-temp-output (concat (file-name-sans-extension (file-name-concat (file-name-directory ffmpeg-subchapter-concat-output-file) "temp" (file-name-nondirectory ffmpeg-subchapter-concat-output-file))) "_track_temp" ".mp4")))
    (when (string-match "data-audio-src=\"\\(.+?\\)\"" prop)
      (setq audio-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (when (string-match "data-track-src=\"\\(.+?\\)\"" prop)
      (setq track-source (expand-file-name (match-string 1 prop) reveal-project-directory)))
    (when (string-match "data-backgroundmusic-src=\"\\(.+?\\)\"" prop)
      (setq backgroundmusic-source (expand-file-name (match-string 1 prop) reveal-project-directory)))

    (dolist (subchapter (read (hurricane//headline-property "SUBCHAPTER_KEY")))
      (setq input-configurations (concat input-configurations  " -i " "\"" (expand-file-name (format "static/%s" (file-name-sans-extension (buffer-name))) reveal-project-directory) "/" (downcase (concat (string-join (org--get-outline-path-1) "-") "-" (number-to-string subchapter))) ".mp4" "\""))
      (setq concat-configurations (concat concat-configurations (format "[%s:0]" index index)))
      (setq index (+ 1 index)))

    (setq merge-cmd (format "ffmpeg%s -filter_complex \"%s concat=n=%s:v=1[v]\" -map \"[v]\" -c:v h264 -b:v 6m -y \"%s\"" input-configurations concat-configurations (length (read (hurricane//headline-property "SUBCHAPTER_KEY"))) merge-temp-output))
    (setq tts-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1:a] adelay=2000|2000 [voice];[voice] amix=inputs=1:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" merge-temp-output audio-source tts-temp-output))
    (setq track-cmd (format "ffmpeg -i \"%s\" -vf ass=\"%s\" -c:v h264 -b:v 6m -c:a copy -y \"%s\"" tts-temp-output track-source track-temp-output))
    (setq backgroundmusic-cmd (format "ffmpeg -i \"%s\" -i \"%s\" -filter_complex \"[1] volume=0.01 [aud1];[aud1] afade=t=in:st=0:d=3 [aud2];[aud2] afade=t=out:st=%s:d=3 [aud3];[0:a][aud3] amix=inputs=2:duration=longest [audio_out]\" -map 0:v -map \"[audio_out]\" -y \"%s\"" track-temp-output backgroundmusic-source (- (string-to-number audio-duration) 2) ffmpeg-subchapter-concat-output-file))
    (setq final-cmd (concat merge-cmd "&&" tts-cmd "&&" track-cmd "&&" backgroundmusic-cmd))

    (message "%s" final-cmd)

    (let* ((buffer (get-buffer-create "*ffmpeg subchapter concat*"))
           (process
            (start-process-shell-command
             "hurricane/ffmpeg-subchapter-concat"
             buffer
             final-cmd)))
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (equal event "finished\n")
           (message "Concat: %s finished." ffmpeg-subchapter-concat-output-file)
           ))))
    ))

;; @See: https://emacs-china.org/t/org-roam/26545/24
(add-to-list 'file-coding-system-alist '("\\.org\\'" . utf-8))

(defun hurricane/org-download-images ()
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2)))))
    (next-line)
    (org-download-image $inputStr)
    (org-display-inline-images)))

;; 在函数 switch_to_reader_mode 中加入 eval_in_emacs("hurricane/html-to-org-with-pandoc", [html])
(defun hurricane/html-to-org-with-pandoc (html)
  (kill-new (org-web-tools--html-to-org-with-pandoc html)))

;; @See: https://remacs.fun/posts/%E5%A4%A7%E6%A8%A1%E5%9E%8B%E6%97%B6%E4%BB%A3%E6%88%91%E4%BB%AC%E6%80%8E%E4%B9%88%E7%8E%A9emacs1.-%E4%B8%AD%E8%8B%B1%E6%96%87%E8%BE%93%E5%85%A5%E6%97%B6%E7%9A%84%E7%A9%BA%E6%A0%BC/
;; {{
(defun add-space-between-chinese-and-english ()
  "在中英文之间自动添加空格。"
  (let ((current-char (char-before))
        (prev-char (char-before (1- (point)))))
    (when (and current-char prev-char
               (or (and (is-chinese-character prev-char) (is-halfwidth-character current-char))
                   (and (is-halfwidth-character prev-char) (is-chinese-character current-char))
                   (and (is-closing-bracket prev-char) (is-opening-bracket current-char)))
               (not (eq prev-char ?\s))) ; 检查前一个字符不是空格
      (save-excursion
        (goto-char (1- (point)))
        (insert " ")))))

(defun is-chinese-character (char)
  "判断字符是否为中文字符。"
  (and char (or (and (>= char #x4e00) (<= char #x9fff))
                (and (>= char #x3400) (<= char #x4dbf))
                (and (>= char #x20000) (<= char #x2a6df))
                (and (>= char #x2a700) (<= char #x2b73f))
                (and (>= char #x2b740) (<= char #x2b81f))
                (and (>= char #x2b820) (<= char #x2ceaf)))))

(defun is-halfwidth-character (char)
  "判断字符是否为半角字符，包括英文字母、数字和标点符号。"
  (and char (or (and (>= char ?a) (<= char ?z))
                (and (>= char ?A) (<= char ?Z))
                (and (>= char ?0) (<= char ?9))
                )))

(defun is-opening-bracket (char)
  (= char ?[))

  (defun is-closing-bracket (char)
    (= char ?]))

(defun delayed-add-space-between-chinese-and-english ()
  "延迟执行，在中英文之间自动添加空格。"
  (run-with-idle-timer 0 nil 'add-space-between-chinese-and-english))

(define-minor-mode auto-space-mode
  "在中英文之间自动添加空格的模式。"
  :lighter " Auto-Space"
  :global t
  (if auto-space-mode
      (add-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)
    (remove-hook 'post-self-insert-hook 'add-space-between-chinese-and-english)))
;; }}

;; https://emacs-china.org/t/topic/16566/7
(defun remove-multiple-spaces-between-chinese ()
  "Remove one or more spaces between Chinese characters."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([[:multibyte:]]\\) +\\([[:multibyte:]]\\)" nil t)
      (replace-match "\\1\\2"))))

(defun hurricane/recursively-convert-source-files-from-GBK-to-UTF-8 ()
  "递归的把当前目录的源文件的编码从 GBK 转成 UTF-8。"
  (interactive)
  (let* ((local-root (or (hurricane//git-project-root) default-directory))
         (result nil))
    (when (yes-or-no-p (format "Do you want to execute the command in the directory: %s?" local-root))
      (setq default-directory local-root)
      (let* ((command (list eaf-python-command (expand-file-name gbk2utf-8-Python-file)))
             (output-buffer (generate-new-buffer "*gbk2utf-8-output*"))
             (exit-code (apply 'call-process (car command) nil output-buffer t (cdr command))))
        (if (eq exit-code 0)
            (progn
              (setq result (with-current-buffer output-buffer
                             (buffer-string)))
              (message "result: %s" result)
              (kill-buffer output-buffer))
          (message "Error running command. Check *gbk2utf-8-output* buffer for details.")
          (switch-to-buffer output-buffer))))
    result))

(defun hurricane/dired-get-marked-files-only ()
  "Return marked files in dired only if there are explicitly marked files.
   Return nil if no files are marked (instead of returning current file at point)."
  (let ((marked (dired-get-marked-files)))
    (when (> (length marked) 0)
      ;; 检查是否只是当前文件（没有明确标记）
      (if (and (= (length marked) 1)
               (equal (car marked) (dired-get-filename nil t)))
          ;; 如果只是当前文件且没有明确标记，返回 nil
          (when (dired-file-marker (dired-get-filename nil t))
            marked)
        ;; 否则返回标记的文件
        marked))))

(defun hurricane/recursively-convert-comments-style ()
  (interactive)
  (let* ((local-root (or (hurricane//git-project-root) default-directory))
         (marked-files (hurricane/dired-get-marked-files-only))
         (result nil))
    (when (yes-or-no-p (format "Do you want to execute the command with these files or in the directory: %s?"
                               (or
                                (and marked-files
                                     (mapconcat 'identity marked-files " ") )
                                local-root)))
      (setq default-directory local-root)
      (let* ((command (append (list "python3" (expand-file-name convert-comments-style-Python-file)) marked-files))
             (output-buffer (generate-new-buffer "*convert-comments-style*"))
             (exit-code (apply 'call-process (car command) nil output-buffer t (cdr command))))
        (if (eq exit-code 0)
            (progn
              (setq result (with-current-buffer output-buffer
                             (buffer-string)))
              (message "result: %s" result)
              (kill-buffer output-buffer))
          (message "Error running command. Check *convert-comments-style* buffer for details.")
          (switch-to-buffer output-buffer))))
    result))

(defun hurricane/recursively-clean-comments ()
  (interactive)
  (let* ((local-root (or (hurricane//git-project-root) default-directory))
         (marked-files (hurricane/dired-get-marked-files-only))
         (result nil))
    (when (yes-or-no-p (format "Do you want to execute the command with these files or in the directory: %s?"
                               (or
                                (and marked-files
                                     (mapconcat 'identity marked-files " ") )
                                local-root)))
      (setq default-directory local-root)
      (let* ((command (append (list "python3" (expand-file-name clean-comments-Python-file)) marked-files))
             (output-buffer (generate-new-buffer "*clean-comments*"))
             (exit-code (apply 'call-process (car command) nil output-buffer t (cdr command))))
        (if (eq exit-code 0)
            (progn
              (setq result (with-current-buffer output-buffer
                             (buffer-string)))
              (message "result: %s" result)
              (kill-buffer output-buffer))
          (message "Error running command. Check *clean-comments* buffer for details.")
          (switch-to-buffer output-buffer))))
    result))

(defun hurricane/recursively-build-reverse-index ()
  "Build reverse index for PDF files using build_reverse_index.py"
  (interactive)
  (let* ((local-root (or (file-name-directory pdf-tools-annotations-db-location)
                         default-directory))
         (marked-files (hurricane/dired-get-marked-files-only))
         (result nil))
    (when (yes-or-no-p (format "Do you want to build reverse index for files in directory: %s?"
                               (or
                                (and (not (null marked-files))
                                     (mapconcat 'identity marked-files " ") )
                                local-root)))
      (setq default-directory local-root)
      (let* ((command (append (list eaf-python-command (expand-file-name build-reverse-index-Python-file)) marked-files))
             (output-buffer (generate-new-buffer "*build-reverse-index*"))
             (exit-code (apply 'call-process (car command) nil output-buffer t (cdr command))))
        (if (eq exit-code 0)
            (progn
              (setq result (with-current-buffer output-buffer
                             (buffer-string)))
              (message "Reverse index built successfully: %s" result)
              (kill-buffer output-buffer))
          (message "Error building reverse index. Check *build-reverse-index* buffer for details.")
          (switch-to-buffer output-buffer))))
    result))

;; 为自定义的 counsel 函数配置高亮显示
(with-eval-after-load 'ivy
  (ivy-configure 'hurricane/counsel-ag
    :display-transformer-fn #'counsel-git-grep-transformer)

  (ivy-configure 'hurricane/counsel-rg
    :display-transformer-fn #'counsel-git-grep-transformer)

  (ivy-configure 'hurricane/pdf-tools-folder-search
    :display-transformer-fn #'counsel-git-grep-transformer))

;; 为 PDF 搜索创建专门的 counsel-rg 版本
(cl-defun hurricane/counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt
                                          &key caller)
  "Custom counsel-rg for PDF search with custom action."
  (interactive)
  (let ((counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (hurricane/counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                          :caller (or caller 'hurricane/counsel-rg))))

;; 为 PDF 搜索创建专门的 counsel-ag 版本
(cl-defun hurricane/counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt
                                          &key caller)
  "Custom counsel-ag for PDF search with custom action."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (let ((prog-name (car (if (listp counsel-ag-command) counsel-ag-command
                          (split-string counsel-ag-command))))
        (arg (prefix-numeric-value current-prefix-arg)))
    (when (>= arg 4)
      (setq initial-directory
            (or initial-directory
                (counsel-read-directory-name (concat
                                              prog-name
                                              " in directory: ")))))
    (when (>= arg 16)
      (setq extra-ag-args
            (or extra-ag-args
                (read-from-minibuffer (format "%s args: " prog-name)))))
    (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
    (let ((default-directory (or initial-directory
                                 (counsel--git-root)
                                 default-directory)))
      (ivy-read (or ag-prompt
                    (concat prog-name ": "))
                #'counsel-ag-function
                :initial-input initial-input
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                ;; 使用自定义的 action
                :action #'hurricane//counsel--git-grep-visit-wrapper
                :require-match t
                :caller (or caller 'hurricane/counsel-ag)))))

;; 修改主函数使用自定义版本
(defun hurricane/pdf-tools-folder-search ()
  "Search text within PDF files using grep for better performance"
  (interactive)
  (hurricane/spacemacs-counsel-search
   '("rg") nil
   (expand-file-name "Cache" (file-name-directory pdf-tools-annotations-db-location))))

;; 创建自定义的 spacemacs/counsel-search 版本
(defun hurricane/spacemacs-counsel-search
    (&optional tools use-initial-input initial-directory)
  "Custom search function for PDF tools with custom actions."
  (interactive)
  (require 'counsel)
  (cl-letf* ((initial-input (if use-initial-input
                                (rxt-quote-pcre
                                 (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (or (thing-at-point 'symbol t) "")))
                              ""))
             (tool (catch 'tool
                     (dolist (tool tools)
                       (when (and (assoc-string tool spacemacs--counsel-commands)
                                  (executable-find tool))
                         (throw 'tool tool)))
                     (throw 'tool "grep")))
             (default-directory
              (or initial-directory (read-directory-name "Start from directory: ")))
             (display-directory
              (if (< (length default-directory)
                     spacemacs--counsel-search-max-path-length)
                  default-directory
                (concat
                 "..." (substring default-directory
                                  (- (length default-directory)
                                     spacemacs--counsel-search-max-path-length)
                                  (length default-directory))))))
    (cond ((string= tool "ag")
           (hurricane/counsel-ag initial-input default-directory nil
                                 (format "ag from [%s]: " display-directory)))
          ((string= tool "rg")
           (hurricane/counsel-rg initial-input default-directory nil
                                 (format "rg from [%s]: " display-directory)))
          (t
           (ivy-read
            (format "%s from [%s]: "
                    tool
                    display-directory)
            (spacemacs//make-counsel-search-function tool)
            :initial-input (when initial-input (rxt-quote-pcre initial-input))
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'hurricane//counsel--git-grep-visit-wrapper
            :caller 'hurricane/pdf-tools-folder-search
            :keymap spacemacs--counsel-map
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup)))))))

;; Action wrapper 函数
(defun hurricane//counsel--git-grep-visit-wrapper (cand)
  "Wrapper function for custom PDF search action."
  (hurricane//counsel--git-grep-visit #'counsel--git-grep-visit cand))

;; 简化的自定义访问函数
(defun hurricane//counsel--git-grep-visit (orig-fun cand &optional other-window)
  "Custom visit function for PDF search results."
  (when (string-match "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)$" cand)
    (let* ((file-path (match-string 1 cand))
           (line-num (match-string 2 cand))
           (page-num (string-to-number (match-string 3 cand)))
           (content (string-trim (match-string 4 cand)))
           (txt-filename (file-name-nondirectory file-path))
           (filename (file-name-sans-extension txt-filename))
           (pdf-path (hurricane/get-pdf-path-from-db filename)))
      (if (file-exists-p pdf-path)
          (blink-search-grep-pdf-do pdf-path page-num content)
        (message "PDF file not found: %s" pdf-path)))))

;; 辅助函数：从数据库查询 PDF 路径
(defun hurricane/get-pdf-path-from-db (pdf-filename)
  "Query PDF file path from pdf-annotations.db based on title"
  (let* ((local-root (or (file-name-directory pdf-tools-annotations-db-location)
                         default-directory))
         (db-path (expand-file-name "pdf-annotations.db" local-root))
         (sql-query (format "SELECT file FROM files WHERE title = '%s';" pdf-filename))
         (result nil))

    (when (file-exists-p db-path)
      (with-temp-buffer
        (let* ((output-buffer (generate-new-buffer "*get-pdf-path-from-db*"))
               (exit-code (call-process "sqlite3" nil t nil db-path sql-query)))
          (when (eq exit-code 0)
            (setq result (string-trim (buffer-string)))
            (when (not (string-empty-p result))
              result)))))))

(defun make-search-frame (x)
  (let (summary
        doc-frame
        x-pos y-pos
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
        ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (abs-pixel-pos (save-excursion
                         ;; (beginning-of-thing 'symbol)
                         (window-absolute-pixel-position))))
    (setq x-pos (car abs-pixel-pos))
    ;; (setq y (cdr abs-pixel-pos))
    (setq y-pos (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setq doc-frame (make-frame '((minibuffer . t)
                                  (name . "*Search Peek*")
                                  (width . 80)
                                  (visibility . nil)
                                  (height . 15))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (set-frame-position doc-frame x-pos y-pos)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (with-selected-frame doc-frame
      (ivy-search-from-action x)
      ;; (read-only-mode)
      (recenter-top-bottom 0))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (make-frame-visible doc-frame)
    (select-frame-set-input-focus doc-frame)))

(defun hurricane/document-contents-extractor ()
  (interactive)
  (let* ((first-page (read-string "Please enter first page: "))
         (last-page (read-string "Please enter last page: "))
         (document-contents-extractor-command "extract_contents")
         (lang "chi_sim")
         (pdf-full-file-path (or eaf--buffer-id buffer-file-name))
         (full-command (list document-contents-extractor-command "-l" lang (expand-file-name pdf-full-file-path) first-page last-page))
         (output-buffer (generate-new-buffer "*document-contents-extractor*"))
         (exit-code (apply 'call-process (car full-command) nil output-buffer t (cdr full-command))))

    (if (eq exit-code 0)
        (progn
          (setq result (with-current-buffer output-buffer
                         (buffer-string)))
          (message "result: %s" result)
          (kill-buffer output-buffer))
      (message "Error running command. Check *document-contents-extractor* buffer for details.")
      (switch-to-buffer output-buffer))))

(defun hurricane/convert-to-fullwidth-punctuations ()
  (interactive)
  (let ((cjk "\\([\u4E00-\u9FCC\u3400-\u4DB5\uFA0E\uFA0F\uFA11\uFA13\uFA14\uFA1F\uFA21\uFA23\uFA24\uFA27-\uFA29]\\|[\ud840-\ud868][\udc00-\udfff]\\|\ud869[\udc00-\uded6\udf00-\udfff]\\|[\ud86a-\ud86c][\udc00-\udfff]\\|\ud86d[\udc00-\udf34\udf40-\udfff]\\|\ud86e[\udc00-\udc1d]\\)" )
        (punct-before-cjk '(("\"" . "“")
                            ("(" . "（")
                            ("'" . "‘")))
        (punct-after-cjk '(("." . "。")
                           ("," . "，")
                           ("!" . "！")
                           ("?" . "？")
                           (":" . "：")
                           (";" . "；")
                           ("\"" . "”")
                           (")" . "）"))))

    (pcase-dolist (`(,punct-half . ,punct-full) punct-before-cjk)
      (replace-regexp-in-region (concat (regexp-quote punct-half) cjk)
                                (concat punct-full "\\1")
                                (point-min)))
    (pcase-dolist (`(,punct-half . ,punct-full) punct-after-cjk)
      (replace-regexp-in-region (concat cjk (regexp-quote punct-half))
                                (concat "\\1" punct-full)
                                (point-min)))))

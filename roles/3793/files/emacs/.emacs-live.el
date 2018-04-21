;;; .emacs-live.el -- overtone-emacs-live config

;;; Commentary:

;;; Code:

(let* ((username "funisaya")
       (dest-dir (substitute-in-file-name (concat "$HOME/.live-packs/" username "-pack"))))
  (live-add-packs dest-dir))

;;; .emacs-live.el ends here

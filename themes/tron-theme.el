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

;; ---------------------------
;;
; ;
;;
;; ----------------------------

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme tron "A dark colour theme")

(let ((tron-background     "#1d2029")
      (tron-mid-gray       "#555555")
      (tron-foreground     "#90aecb")
      (tron-bright         "#dfedff")
      (tron-light-gray     "#666666")
      (tron-lightest-gray  "#999999")
      (tron-comment-slate  "#AAAAAA")
      (tron-orange         "#F38630")
      (tron-light-slate    "#465459")
      (tron-dark-slate     "#161A1F")
      (tron-light-blue     "#A7DBD8")
      (tron-subtle-blue    "#A7DBD8")
      (tron-dirty-white    "#F8F8F0")
      (tron-lighter-orange "#FD971F")
      (red                  "#FF0000")
      (bright-red           "#FF7777")
      (white                "#FFFFFF")
      (bright-green         "#77FF77")
      (green                "#00FF00")
      (tron-strong-green   "#16BF41")
      (purple               "#E1AEF2")
      (yellow               "#F0C674"))


  ;; js2-warning js2-error font-lock-keyword-face font-lock-variable-name-face font-lock-constant-face
  (custom-theme-set-faces
   'tron
;;hl-line-face
   `(eshell-prompt  ((t (:foreground ,yellow))))
   `(web-mode-html-attr-name-face  ((t (:foreground ,purple))))
   `(web-mode-html-tag-face  ((t (:foreground ,tron-strong-green))))
   `(aw-leading-char-face  ((t (:foreground ,red :height 500))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,white))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,bright-red))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,purple))))
   `(hl-line ((t (:background ,tron-orange))))
   `(js2-object-property  ((t (:foreground ,tron-strong-green))))
   `(js2-function-call  ((t (:foreground ,bright-green))))
   `(js2-external-variable  ((t (:foreground ,bright-red))))
   `(js2-function-param  ((t (:foreground ,yellow))))

     `(company-tooltip ((t (:background ,tron-background :box t))))
     `(company-scrollbar-bg ((t (:background ,tron-strong-green))))
     `(company-scrollbar-fg ((t (:background ,tron-dark-slate))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))

     `(aw-leading-char-face ((t (:height 500 :foreground ,red))))
   ;; ----------------- Frame stuff --------------------
   `(default ((t (:background ,tron-background :foreground ,tron-bright))))
   `(cursor  ((t (:foreground ,white :background ,red))))
   `(hl-line ((t (:background ,purple))))
   `(mode-line ((t (:foreground ,tron-background :background ,tron-bright :overline ,tron-bright))))
    `(mode-line-inactive ((t (:box nil :foreground ,tron-light-gray :background ,tron-background :overline ,tron-bright))))
    ;; `(mode-line ((t (:box nil :foreground ,tron-foreground :background ,tron-background :overline ,green))))
    `(fringe ((t (:background ,tron-background))))
   ;; Highlight region color
   `(region ((t (:foreground ,white :background ,bright-red))))
   ;; Dir-ed search prompt
   `(minibuffer-prompt ((default (:foreground ,tron-light-blue))))

   ;; ---------------- Code Highlighting ---------------
   ;; Builtins
   `(font-lock-builtin-face ((t (:foreground ,tron-orange))))
   ;; Constants
   `(font-lock-constant-face ((t (:foreground ,bright-red))))
   ;; Comments
   `(font-lock-comment-face ((t (:foreground ,tron-comment-slate))))
   ;; Function names
   `(font-lock-function-name-face ((t (:foreground ,purple))))
   ;; Keywords
   `(font-lock-keyword-face ((t (:foreground ,tron-subtle-blue))))
   ;; Strings
   `(font-lock-string-face ((t (:foreground ,bright-green))))

   ;; Variables
   `(font-lock-variable-name-face ((t (:foreground ,tron-lighter-orange))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-warning-face ((t (:foreground ,red :bold t))))
   ;; ediff
   `(ediff-odd-diff-A ((t (:background ,tron-light-gray))))
   `(ediff-odd-diff-B ((t (:background ,tron-light-gray))))
   `(ediff-odd-diff-C ((t (:background ,tron-dark-slate))))

   `(ediff-even-diff-A ((t (:background ,tron-light-gray))))
   `(ediff-even-diff-B ((t (:background ,tron-light-gray))))
   `(ediff-even-diff-C ((t (:background ,tron-dark-slate))))
   ;; ---------------- Package Specific Stuff -----------
   ;; Powerline
   `(powerline-active1 ((t (:background ,tron-dirty-white :foreground ,tron-background))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'tron)

;; Local Variables:
;; no-byte-compile: t
;; End:

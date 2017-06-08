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
;; Retro: A dark color theme
;;
;; ----------------------------

(unless (>= emacs-major-version 24)
  (error "requires Emacs 24 or later."))

(deftheme retro "A dark colour theme")

(let ((retro-background     "#001F24")
      (retro-foreground     "#F8F8F2")
      (retro-mid-gray       "#555555")
      (retro-light-gray     "#666666")
      (retro-lightest-gray  "#999999")
      (retro-comment-slate  "#CCCCCC")
      (retro-orange         "#F38630")
      (retro-light-slate    "#465459")
      (retro-dark-slate     "#161A1F")
      (retro-light-blue     "#A7DBD8")
      (retro-subtle-blue    "#A7DBD8")
      (retro-dirty-white    "#F8F8F0")
      (retro-lighter-orange "#FD971F")
      (red                  "#FF0000")
      (bright-red           "#FF7777")
      (white                "#FFFFFF")
      (bright-green         "#77FF77")
      (green                "#00FF00")
      (retro-strong-green   "#16BF41")
      (purple               "#E1AEF2")
      (yellow               "#F0C674"))

  
  ;; js2-warning js2-error font-lock-keyword-face font-lock-variable-name-face font-lock-constant-face
  (custom-theme-set-faces
   'retro

   `(web-mode-html-attr-name-face  ((t (:foreground ,purple))))
   `(web-mode-html-tag-face  ((t (:foreground ,retro-strong-green))))
   
   `(js2-object-property  ((t (:foreground ,retro-strong-green))))
   `(js2-function-call  ((t (:foreground ,bright-green))))
   `(js2-external-variable  ((t (:foreground ,bright-red))))
   `(js2-function-param  ((t (:foreground ,yellow))))

     `(company-tooltip ((t (:background ,retro-background :box t))))
     `(company-scrollbar-bg ((t (:background ,retro-strong-green))))
     `(company-scrollbar-fg ((t (:background ,retro-dark-slate))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))

     `(aw-leading-char-face ((t (:height 500 :foreground ,red))))
   ;; ----------------- Frame stuff --------------------
   `(default ((t (:background ,retro-background :foreground ,green))))
   `(cursor  ((t (:foreground ,white :background ,red))))
   `(hl-line ((t (:background ,purple))))
   `(modeline ((t (:foreground ,retro-lightest-gray :background ,retro-background :overline ,green))))
    `(mode-line-inactive ((t (:box nil :foreground ,retro-light-gray :background ,retro-background :overline ,green))))
    `(mode-line ((t (:box nil :foreground ,retro-foreground :background ,retro-background :overline ,green))))
    `(fringe ((t (:background ,retro-background))))
   ;; Highlight region color
   `(region ((t (:foreground ,white :background ,bright-red))))
   ;; Dir-ed search prompt
   `(minibuffer-prompt ((default (:foreground ,retro-light-blue))))

   ;; ---------------- Code Highlighting ---------------
   ;; Builtins
   `(font-lock-builtin-face ((t (:foreground ,retro-orange))))
   ;; Constants
   `(font-lock-constant-face ((t (:foreground ,bright-red))))
   ;; Comments
   `(font-lock-comment-face ((t (:foreground ,retro-comment-slate))))
   ;; Function names
   `(font-lock-function-name-face ((t (:foreground ,purple))))
   ;; Keywords
   `(font-lock-keyword-face ((t (:foreground ,retro-subtle-blue))))
   ;; Strings
   `(font-lock-string-face ((t (:foreground ,bright-green))))

   ;; Variables
   `(font-lock-variable-name-face ((t (:foreground ,retro-lighter-orange))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-warning-face ((t (:foreground ,red :bold t))))

   ;; ---------------- Package Specific Stuff -----------
   ;; Powerline
   `(powerline-active1 ((t (:background ,retro-dirty-white :foreground ,retro-background))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide-theme 'retro)

;; Local Variables:
;; no-byte-compile: t
;; End:

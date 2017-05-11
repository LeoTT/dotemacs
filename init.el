(require 'package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (retro)))
 '(custom-safe-themes
   (quote
    ("5627630c7bada8a95314d3fcaa5e21cd41450235a1cae9b397cf6d3a6ba71bbd" "d492117c937cf58052a59a46a6d1d581cfd67931ba5c6cc67bd469f947eebf9a" "20ba845d7e5e77f9c533ede6ac65e118f48f1c8fbdfa0b82d2dd855f5bbd30f2" "efcd5ab62141d98157c30bf26d3fd71e18c6ecbbbc87a959cf578118be278336" "fff976954ad691949cf06127bb63c12bd24a11454ebbee20e8c76bed32aeee33" "11ff66c0b5dec32502c54d525804e9c298e86e65fa3d8e31ff954fa5d82d1f8b" "658850a13d46b29d0407c25833d2885ce7b7a88916cdcfced01dd4ac5e56d79a" "01972d61270a27e0a1858c7d0257bc4042ba86a7d6969a940aed0144f6677109" "2464f6eec375c9906b099e2a55815a23abc1ca5f8a05f9c0ba7e8a54c33bf49f" "1450cffa4036becea74d2376db9331f841c8ba4a7b1be16e3a97f32f9990be26" "844e74fee2364c233bd3a44c617a5c3049e7fba5e277b0e4ed6cda5c32d0bf5e" "ae6d6cec5509d5b92900601b25325289c45ebdb0cfc4545fac233c599b15c217" "382b35f978ee5d3a67c08d6528e6e7376a5d0ded88ee4b22039495c3d3ab91b5" "e5733a73a20fcdbf2a553f9b1cff7b36e708eef194280afc113e930dd6b382c8" "c4269d94fe9a37c01788111fde29d899922ca3c557baab9b4cc13c9e60c18cb4" "c6837a3b0bfbcd4874c6b7bee9964993779f9a627da4ed02b093bc5d6030e53d" "75bb58fcbf945c7f632b2e8d015e5422d0f19321f576e0a9c0da11f1177a974e" "6bb466c89b7e3eedc1f19f5a0cfa53be9baf6077f4d4a6f9b5d087f0231de9c8" "cd547ce0f7b19dc3747c7e89f8b9b0df616c1929ea4ba0e9b12ba2a46675839e" "780a4a3a3a968b7a3c1df1acd3d2b1ad4b6bde4d32dcfae84ec3c20f063c559c" "c0ebd7ce3bb188f0baaf44275799c82f1dc1b06bc24238cd5143f605886d9014" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" default)))
 '(markdown-command "/usr/local/bin/pandoc")
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


(package-initialize)

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(load-file ".emacs.d/emacs_config/bootstrapper.el")

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'retro t)


(add-hook 'doc-view-mode-hook 'auto-revert-mode)

# Emacs Extensions

I use these scripts to streamline my development workflow with Emacs.

# local-snippet.el

To use this file, add the following to `~/.emacs`:

```
(require 'local-snippet)
(add-hook 'emacs-startup-hook (lambda () (load-local-snippets)))
```

Then, create a file in the root of your repository, e.g. to set a license
banner for all files:

```
(custom-set-variables
 '(file-banner-license-notice t)
 '(file-copyright-license 'file-gplv3-license))
```


(elpaca-leaf ggtags
  :bind (ggtags-mode-map
	 ("C-c g s" ggtags-find-other-symbol)
	 ("C-c g h" ggtags-view-tag-history)
	 ("C-c g r" ggtags-find-reference)
	 ("C-c g f" ggtags-find-file)
	 ("C-c g c" ggtags-create-tags)
	 ("C-c g u" ggtags-update-tags))
  :hook
  (c-mode-common-hook
   (lambda ()
     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
       (ggtags-mode 1)))))


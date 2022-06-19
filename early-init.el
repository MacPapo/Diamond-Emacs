;; Try to speedup the sturtup of Emacs

(setq
 site-run-file nil                         ; No site-wide run-time initializations.
 inhibit-default-init t                    ; No site-wide default library
 gc-cons-threshold most-positive-fixnum)   ; Very large threshold for garbage

;; Add my personal channel to list

(list
 (channel
  (name 'h-guix)
  (url "https://github.com/hiepph/h-guix.git"))

 ;; Pin Guix to v1.4.0
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit
   "8e2f32cee982d42a79e53fc1e9aa7b8ff0514714")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))

 ;; Latest Guix
 ;; %default-packages
 )

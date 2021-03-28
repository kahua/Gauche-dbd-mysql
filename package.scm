;;
;; Package Gauche-dbd-mysql
;;

(define-gauche-package "Gauche-dbd-mysql"
  ;;
  :version "0.4"

  ;; Description of the package.  The first line is used as a short
  ;; summary.
  :description "Mysql binding with dbd interface"

  ;; List of dependencies.
  ;; Example:
  ;;     :require (("Gauche" (>= "0.9.5"))  ; requires Gauche 0.9.5 or later
  ;;               ("Gauche-gl" "0.6"))     ; and Gauche-gl 0.6
  :require (("Gauche" (>= "0.9.7_")))

  ;; List name and contact info of authors.
  ;; e.g. ("Eva Lu Ator <eval@example.com>"
  ;;       "Alyssa P. Hacker <lisper@example.com>")
  :authors ("Kahua project <info@kahua.org>")

  ;; List name and contact info of package maintainers, if they differ
  ;; from authors.
  ;; e.g. ("Cy D. Fect <c@example.com>")
  :maintainers ("Shiro Kawai <shiro@acm.org>")

  ;; List licenses
  ;; e.g. ("BSD")
  :licenses ("GPL2")

  ;; Homepage URL, if any.
  ; :homepage "http://example.com/Gauche-dbd-mysql/"

  ;; Repository URL, e.g. github
  :repository "https://github.com/kahua/Gauche-dbd-mysql.git"

  :providing-modules (dbd.mysql)
  )

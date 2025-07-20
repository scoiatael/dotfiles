;;; modules/scoiatael/data/packages.el -*- lexical-binding: t; -*-

(package! ob-graphql :recipe (:host github :repo "jdormit/ob-graphql"))
(package! prisma-mode :recipe (:host github :repo "davidarenas/prisma-mode"))
(package! river-mode
  :type 'local
  :recipe (:local-repo "../../../packages"))
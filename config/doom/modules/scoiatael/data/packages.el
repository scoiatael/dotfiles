;;; modules/scoiatael/data/packages.el -*- lexical-binding: t; -*-

(package! ob-graphql :recipe (:host github :repo "jdormit/ob-graphql") :pin "7c35419f9eec5dc44967cbcfa13c7135b9a96bfc")
(package! prisma-mode :recipe (:host github :repo "davidarenas/prisma-mode") :pin "95af5bcdac824fa9f69e7dcc3016983be924fe7d")
(package! river-mode
  :type 'local
  :recipe (:local-repo "../../../packages"))

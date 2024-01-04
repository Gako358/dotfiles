;;; sql.el --- Git Configuration
;;; Commentary:
;;; Code:

(require 'sql)

(setq sql-mysql-login-params
      '((user :default "root")
        (server :default "localhost")
        (port :default 3306)))


;;; sql.el ends here

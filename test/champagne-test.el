;;; champagne-test.el --- test your freaking package!  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions, Psionik K

;; Author:  Psionik K <73710933+psionic-k@users.noreply.github.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Run the batch tests from root directory:
;; nix shell .github#emacsGit --quick --script .github/run-shim.el -- test
;; Test dependencies can be provided to the Emacsen declared inside the root
;; flake.nix.

;; For local development, dependencies should be installed by the user.  Tests
;; can be run from one of the project files using the `erk-ert-project'
;; command.

;;; Code:

(require 'ert)
(require 'champagne)

;; No unit-testable pieces were developed so far.  The byte compile is catching
;; most issues on its own.

(provide 'champagne-test)
;;; champagne-test.el ends here.

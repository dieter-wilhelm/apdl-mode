;;; Conti.el --- Company configurations              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Hans-Dieter Wilhelm

;; Author: Hans-Dieter Wilhelm <uidg1626@sbav106x.vs.de.conti.de>
;; Keywords: ANSYS

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(unless ansys-current-ansys-version
  (setq ansys-current-ansys-version "161"))

;; needed for our non default `ansys_inc/16.1.0/v161/' ANSYS
;; installation path on Linux...
(unless ansys-current-update-version
  (setq ansys-current-update-version "0"))

;;; Conti.el ends here

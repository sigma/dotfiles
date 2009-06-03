;;; grade.el --- Program for keeping track of grades

;; Copyright (C) 2001 Jay Belanger

;; Author: Jay Belanger
;; Maintainer: Jay Belanger <belanger@truman.edu>

;; $Revision: 1.87 $
;; $Date: 2004/04/21 18:13:42 $
;; Keywords: grade

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>.
;; The latest version of this package should be available at
;; ftp://vh213601.truman.edu/pub/Grade

;;; Commentary:

;; Quick intro

;; Grade is an Emacs program for keeping track of student's grades.
;; It requires the calc package
;; (ftp:://ftp.gnu.org/pub/gnu/calc/calc-2.02f.tar.gz).
;;
;; Starting up
;; -----------
;; If grade.el is somewhere in the Emacs load-path and a line of
;; the form
;; (autoload 'grade "grade" "Emacs grading program" t)
;; is in the .emacs file, the program can be started with
;; M-x grade
;; Once the program is started, the user will be prompted for a file
;; name.
;; If an existing grade file is chosen, it will be opened in grade-mode.
;; If a new file is chosen, the user will be prompted for some
;; information about the class, then a table for the student grades will
;; be inserted into the buffer.  The user will then be given the option
;; to enter the student names (in which case the user will then be prompted
;; for the names of the students in the class, the prompting will stop
;; when no first name is given) or to import a csv file.
;; (The csv file can be imported at a later time with the command
;; M-x grade-import-data)
;;
;; Inserting students
;; ------------------
;; If several more students are to be added, the prompting can be started
;; again by typing C-cC-a (Add students).
;; Individual students can be also added with C-cC-n (New student).  In
;; this case, the student name will be prompted for, and the user will be
;; asked if they want to insert scores for the student.  Any time later,
;; typing C-cC-i will prompt for and insert the scores for the current
;; student.
;; Students can be removed with C-k.  If a student is removed, their
;; information will be copied to the file filename-REMOVED in the
;; auxiliary directory.
;;
;; Moving students
;; ---------------
;; To move a student from one row to another, choose the student by
;; typing * when the point is in their row.  Move the point to the row
;; the student is to be moved to, and type @.
;;
;; Inserting columns
;; -----------------
;; A column in which to keep scores can be inserted at the end of the
;; table with C-cC-o (Open a column).  (To insert a column elsewhere,
;; move the point to where the column should be inserted and type
;; C-cC-s (insert Scores).)  The name of the column will be prompted for,
;; and a point value for the assignment will also be prompted for.
;; Normally the point value is a number, but if the point value begins
;; with an "=", it is assumed to be a formula.
;; In a formula, the contents of another row named "name" can be referred
;; to with <name>.  For example, suppose there is a column named "HW 1".
;; If a column named "Twice" is inserted and given the point value
;; "=2*<HW 1>", then the entries for column "Twice" will be twice those
;; of column "HW 1".
;; If the column is not a formula column, the user will be given the
;; option of entering the scores in that column, in which case scores for
;; each student will be prompted for.  Typing C-cC-g will also prompt for
;; and insert the scores for the current column.
;; The bottom row of a column will contain the average of the column
;; entries.  If the point value for the column is not a number or a
;; formula, however, then that column is assumed to contain
;; non-numerical data, and will not be averaged.
;; A column can be removed by typing C-cC-k.
;;
;; Note on computations
;; --------------------
;; Note that an empty score cell will not contribute to the average when
;; averaging a column, but will count as a 0 when computing a formula.
;;
;; Moving columns
;; --------------
;; The current column can be moved to the end of the table by typing C-c].
;; A column can be moved elsewhere by marking it with C-c*,
;; moving the point to where the column should go, and typing C-c@.
;;
;; Moving around
;; -------------
;; The cells in the table can be navigated with the arrow keys, with C-n,
;; C-p, C-f, C-b, and with RET (move down one row) and TAB (move forward
;; one column).
;; Additionally, M-p will move to the top row, M-n will move to the
;; bottom row, M-b will move to the first column, M-f will move to the
;; last column.
;; The command C-cC-c (also M-<) will move to the top left cell.
;; The grade-mode commands will keep the point in the table, and many
;; commands in grade-mode assume the point is in the table.  If the point
;; is outside the table, C-cC-c can be used to move the point back in.
;;
;; Editing cells
;; -------------
;; To edit the current cell, use =.
;; If the cell is a score computed by a formula, = will recompute it,
;; otherwise = will edit the cell contents.
;;
;; Editing other information
;; -------------------------
;; C-cC-ec will edit the class name
;; C-cC-es will edit the section name
;; C-cC-em will edit that semester name
;; C-cC-ei will edit the instructor's name
;;
;; Updating columns
;; ----------------
;; To update a column is to recompute the scores in the column, if they
;; are given by a formula, and recompute the average for the column, if
;; the column is a number column or a formula column.
;; Typing C-c= will update the current column.
;; To update all the columns, starting from the left, use C-c#.
;; (grade-mode will make some attempts to keep all the information up to
;; date, but in some cases information will need to be updated
;; explicitly.  Note that if this is necessary and a formula involves a
;; quantity that appears in a column after the row, C-c# may need to be
;; done twice.)
;;
;; Ordering the information
;; ------------------------
;; To put the students in alphabetical order, use C-cC-a.
;; (With an argument, this will put them in reverse alphabetical order.)
;; To arrange the students according the the scores on the current column,
;; (in decreasing order for a numeric column, in alphabetical order
;; otherwise), use C-c>.
;; To arrange the students reverse according to the scores on the current
;; column, use C-c<.
;;
;; Displaying information
;; ----------------------
;; To display the name of the student in the current row, type "s".
;; To display the name of the current column, type "c".
;;
;; Hiding information
;; ------------------
;; Individual student rows can be hidden with the command "/"
;; Individual columns can be hidden with the command "C-c/"
;; These rows and columns can be shown with the command "C-cC-v"
;; and will also appear again next time the grade file is opened.
;; Student rows and columns can be hidden in another fashion.
;; The command "." will mark a row for hiding, and a "C-c." will mark
;; a column for hiding. Then "C-c-" will hide all marked rows and
;; columns.  (C-cC-hs will just hide the rows, and C-cC-hc will just
;; hide the columns.)  Additionally, the next time the grade file is
;; opened, the marked rows and columns will still be hidden.
;; To display all the hidden rows and columns, use the command "C-c+"
;; (Note that the command "C-cC-v" will not cause the hidden marked
;; rows and columns to be displayed.)
;; Information will not appear on any output while it is hidden,
;; although hidden scores can still be used in formulas.
;;
;; Student reports
;; ---------------
;; To display a buffer which will show information on the students, one
;; student at a time, type C-cC-x (grade-show-reports).
;; If a previously existing grade file is started with an argument,
;; C-u M-x grade, then it will start up showing the reports.
;;
;; TeXing it up
;; ------------
;; To create a TeX file with the grade file contents, type C-cC-t.
;; To create a TeX file with student reports (each page will contain the
;; information on one student), type C-cC-r.
;; (Given a prefix, C-u, grade will try to fit two students per page.
;;  With a numeric prefix, C-u n, grade will try to fit n students
;;  per page.  They may not fit.)
;; To create a TeX file with a list of the names of the students, type
;; C-cC-l.
;;
;; Exporting the data
;; ------------------
;; The grade sheet can be exported in csv form with the command
;; M-x grade-export-data
;;
;; Customizability
;; ---------------
;; If the variable grade-use-studentid is non-nil, then there will be
;; a column for the student id numbers.
;; If the variable grade-default-instructor-name is given a value,
;; then that will be the initial information when the instructor's name
;; is prompted for.
;; By default, the width of the columns containing the first names, last
;; names, studentids and scores will be 10, 10, 9 and 5, respectively.
;; These can be changed by changing the values of  grade-fname-column-width,
;; grade-lname-column-width, grade-studentid-column-width and
;; grade-score-column-width.
;; By default, the students will be listed first name last name.
;; To list them last name, first name, set grade-first-name-first to nil.
;; By default, the grade files are kept in ~/Grades/, this can be changed
;; by changing the value of grade-default-directory.
;; The student will be inserted into the table in alphabetical order, if
;; for some reason you don't want this done, change
;; grade-use-alphabetical-order to nil.
;; (Whether or not grade-use-alphabetical-order is nil, giving C-cC-n an
;; argument will insert a student at the current row.)
;; The class averages will be included in the student reports.  If this
;; is not wanted, set grade-put-avg-in-reports to nil.
;; When the file is put into TeX form, it will put
;; grade-tex-columns-per-page on each page.  A reasonable value is
;; guessed, but it can be changed.
;; There will be a maximum of grade-tex-students-per-page students per
;; page, which is by default 35.  (35 students can fit on a page with
;; about 5 formulas.)
;; There will be at most 30 grades per page on each TeXed report.
;; This can be changed by changing the value of grade-reports-grades-per-page.
;; The auxiliary files, for the TeX output and for the deleted students,
;; are kept in a subdirectory named "aux".  This can be changed
;; by changing the value of grade-aux-dir.
;; (Note that once a grade file is created, the same values of
;; grade-fname-column-width, grade-lname-column-width,
;; grade-studentid-column-width, grade-score-column-width,
;; grade-use-alphabetical-order, grade-use-studentid and
;; grade-first-name-first must be used.
;; The values of these variables will be inserted at the end of the
;; grade file when the file is created.  If you never change these
;; values, and don't want them at the end of the file, set the
;; variable `grade-always-insert-local-variables' to nil.
;; If these values are not at the end of a file and you want to insert
;; them, the command M-x grade-insert-local-variables will insert them.
;; The command M-x grade-replace-local-variables will remove any old
;; information before inserting the new local variables.
;;
;; Miscellaneous notes
;; -------------------
;; The cells in the table are assumed to contain their bottom and right
;; boundaries (the - and |).
;; The first row cells are also assumed to contain the top boundary, and
;; the first column cells are assumed to contain the left boundary.
;;
;; Quitting
;; --------
;; `q' will bury the Grades buffer.  If there are unsaved changes, you
;; will be asked if you want to save them.  With an argument,
;; `q' will kill the buffer.
;;
;; KEYBINDING SUMMARY
;; ------------------
;; Motion
;; ------
;; Move forward one column:               C-f
;;  (This can be also be given a          Also rightarrow and TAB.
;;   numeric argument to move
;;   more than one column at a time.)
;; Move backward one column:              C-b
;;  (This can also be given a             Also leftarrow.
;;   numeric argument to move
;;   more than one column at a time.)
;; Move forward one row:                  C-n
;;  (This can also be given a             Also downarrow and RET.
;;   numeric argument to move
;;   more than one row at a time.)
;; Move backward one row:                 C-p
;;  (This can also be given a             Also uparrow.
;;   numeric argument to move
;;   more than one row at a time.)
;; Move to the first row:                 M-p
;; Move to the last row:                  M-n
;; Move to the first column:              C-a
;;                                        Also M-b.
;; Move to the last column:               C-e
;;                                        Also M-f.
;; Move to the beginning of the table:    C-cC-c
;;                                        Also M-<
;; Move to the end of the table:          M->
;;
;; Editing
;; -------
;; Edit (or evaluate) the current cell:   =
;; Edit the class name:                   C-cC-ec
;; Edit the section name:                 C-cC-es
;; Edit the semester name:                C-cC-em
;;
;; Updating
;; --------
;; Update the current column:             C-c=
;; Update all columns:                    C-c#
;;
;; Insertion
;; ---------
;; Add a student:                         C-cC-n
;; Add several students:                  C-cC-a
;; Add a new column (at the end):         C-cC-o
;; Add a new column (at point):           C-cC-s
;; Insert scores for student:             C-cC-g
;; Insert scores for column:              C-cC-i
;;
;; Deletion
;; --------
;; Delete student:                        C-k
;; Delete column:                         C-cC-k
;;
;; Displaying information
;; ----------------------
;; Display student name                   s
;; Display column name                    c
;;
;; Moving information
;; ------------------
;; Mark student for moving:               *
;; Move marked student:                   @
;; Mark column for moving:                C-c*
;; Move marked column:                    C-c@
;; Move current column to end:            C-c]
;;
;; Ordering information
;; --------------------
;; Alphabetize students                   C-cC-a
;; Alphabetize in reverse order           C-uC-cC-a
;; Arrange according to column            C-c>
;; Arrange in reverse order               C-c<
;;
;; Hiding information
;; ------------------
;; Hide student                           /
;; Hide column                            C-c/
;; Mark student for hiding                .
;; Mark column for hiding                 C-c.
;; Hide marked information                C-c-
;; Hide marked students                   C-cC-hs
;; Hide marked columns                    C-cC-hc
;; Show all hidden information            C-c+
;; Show unmarked hidden information       C-cC-v
;;
;; Reports
;; -------
;; Create a student reports buffer:       C-cC-x
;;
;; TeX
;; ---
;; Create a grade TeX file:               C-cC-t
;; Create a student reports TeX file:     C-cC-r
;; Create a list of students:             C-cC-l
;;
;; Exporting
;; ---------
;; Export as a csv file                   M-x grade-export-data
;;
;; Quitting
;; --------
;; Quit                                   q

;; Changes
;;
;; $Log: grade.el,v $
;; Revision 1.87  2004/04/21 18:13:42  belanger
;; The option `grade-latex-options' was added.
;;
;; Revision 1.86  2003/11/18 17:49:41  belanger
;; Many minor invisible changes.
;; Added the quit `q' key.
;;
;; Revision 1.85  2002/11/11 20:48:27  belanger
;; I added grade-interactive-forward/backward-row/column to bind to the
;; keys, so when moving the point when it isn't properly place, it will
;; end up in the right spot.
;;
;; Revision 1.84  2002/10/13 18:46:19  belanger
;; I changed a problem that arose when changing student names or ids.
;;
;; Revision 1.83  2002/10/13 18:15:32  belanger
;; I changed a couple more "documentstyle"s to "documentclass".
;;
;; Revision 1.82  2002/08/27 15:48:34  belanger
;; I changed `documentstyle' to `documentclass'.
;;
;; Revision 1.81  2002/05/30 16:44:13  jay
;; I changed the default value of grade-always-insert-local-variables.
;;
;; Revision 1.80  2002/05/29 03:31:01  jay
;; I fixed some problems that occured when there were 0 scores.
;;
;; Revision 1.79  2002/05/29 02:47:32  jay
;; I fixed a problem with TeXing the file when there were 0 rows.
;;
;; Revision 1.78  2002/05/29 02:35:02  jay
;; I fixed a problem with the menu.
;;
;; Revision 1.77  2002/05/29 02:27:09  jay
;; I added export to the menu.
;;
;; Revision 1.76  2002/05/29 02:24:09  jay
;; I added some documentation.
;;
;; Revision 1.75  2002/05/29 02:16:31  jay
;; I made some additions so it would work with XEmacs.
;;
;; Revision 1.74  2002/05/28 19:32:04  jay
;; I fixed a problem with writing the formulas.
;;
;; Revision 1.73  2002/05/28 16:26:42  jay
;; I changed grade-next-line to fix a problem with moving (and hiding)
;; columns.
;;
;; Revision 1.72  2002/05/26 21:55:08  jay
;; Fixed the problem with hiding columns.
;;
;; Revision 1.71  2002/05/26 21:34:17  jay
;; Some small changes to grade-hide-column.
;;
;; Revision 1.70  2002/05/26 20:20:33  jay
;; I made a small change to the formula output.
;;
;; Revision 1.69  2002/05/26 20:00:09  jay
;; I added export,
;; some more documentation,
;; and keymaps.
;;
;; Revision 1.68  2002/05/26 19:02:56  jay
;; I added an import function.
;;
;; Revision 1.67  2002/05/25 18:36:14  jay
;; Minor doc change.
;;
;; Revision 1.66  2002/05/24 21:40:31  jay
;; I fixed a problem with the reports buffer.
;;
;; Revision 1.65  2002/05/24 21:22:03  jay
;; I fixed grade-goto-first-row.
;;
;; Revision 1.64  2002/05/24 21:02:03  jay
;; I added a line to prevent some columns from being hidden.
;;
;; Revision 1.63  2002/05/24 20:38:16  jay
;; I made always inserting local variables a variable rather than a hook.
;;
;; Revision 1.62  2002/05/24 19:54:12  jay
;; I rearranged the definitions to look nicer, that's all...
;;
;; Revision 1.61  2002/05/24 18:28:27  jay
;; I added some more hide information functions.
;;
;; Revision 1.60  2002/05/23 20:45:48  jay
;; I may have added the hide student option...
;;
;; Revision 1.59  2002/05/23 19:48:18  jay
;; Minor fixes.
;;
;; Revision 1.58  2002/05/23 18:12:39  jay
;; I finished adding the order column commands.
;;
;; Revision 1.57  2002/05/23 17:45:15  jay
;; I added a reverse alphabetical order option.
;;
;; Revision 1.56  2002/05/23 17:37:39  jay
;; I added a command to put the students in alphabetical order.
;;
;; Revision 1.55  2002/05/22 21:56:01  jay
;; Some minor tidying up.
;;
;; Revision 1.54  2002/05/22 20:17:07  jay
;; I fixed a small bug with removing columns.
;;
;; Revision 1.53  2002/05/22 19:22:23  jay
;; Minor bugfixes.
;;
;; Revision 1.52  2002/05/22 17:35:48  jay
;; Some minor bugs were fixed, and some minor cleanup was done.
;;
;; Revision 1.51  2002/05/21 20:47:49  jay
;; I added the option of using a student id number.
;;
;; Revision 1.50  2002/05/21 14:51:54  jay
;; I added a hack-local-variables command to the grade function.
;;
;; Revision 1.49  2002/02/11 17:43:11  jay
;; I fixed a problem with inserting a single new student.
;;
;; Revision 1.48  2002/02/05 19:02:48  jay
;; I added a definition for string> if it isn't defined.
;;
;; Revision 1.47  2002/01/14 22:57:53  jay
;; I added the option of starting a grade file in report mode.
;; I added the commands grade-display-student-name and
;; grade-display-column-name.
;;
;; Revision 1.46  2002/01/11 05:06:44  jaycvs
;; I added the option of putting more than one student per report page.
;;
;; Revision 1.45  2002/01/10 19:21:04  jay
;; I fixed some problems with the insertion of new information (semester,
;; etc.)
;;
;; Revision 1.44  2002/01/10 19:11:19  jay
;; I made the instructor's name configurable.
;;
;; Revision 1.43  2001/12/21 21:52:10  jay
;; I moved the line (grade-goto-table) from grade-mode to grade.
;;
;; Revision 1.42  2001/12/21 19:49:06  jay
;; I fixed some typos in the documentation.
;;
;; Revision 1.41  2001/12/21 15:46:32  jaycvs
;; I added the option of having the last name first in the table.
;;
;; Revision 1.40  2001/12/20 21:02:19  jay
;; I made SPC scroll up, DEL scroll down in the report buffer.
;;
;; Revision 1.39  2001/12/20 20:50:34  jay
;; A small change to speed things up.
;;
;; Revision 1.38  2001/12/20 20:43:55  jay
;; I minor adjustment in show-reports.
;;
;; Revision 1.37  2001/12/20 20:28:28  jay
;; I added some notes to the output for when there is more than
;; one page.
;;
;; Revision 1.36  2001/12/20 17:35:55  jaycvs
;; I fixed the TeX output so it will spread the columns over
;; more than one page, if necessary, and spread the students over
;; more than one page, if necessary.
;;
;; Revision 1.35  2001/12/20 07:14:17  jaycvs
;; I arranged it so that if there are too many columns,
;; they will be split over more than one page.
;;
;; Revision 1.34  2001/12/19 20:48:43  jay
;; *** empty log message ***
;;
;; Revision 1.33  2001/12/19 20:48:22  jay
;; I changed the behavior of some functions so they'll check to see if
;; they're being called from the table or not.
;;
;; Revision 1.32  2001/12/19 19:22:17  jay
;; I added the function grade-in-table-p.
;;
;; Revision 1.31  2001/12/19 19:06:10  jay
;; I made some comments on where the cell boundaries are.
;;
;; Revision 1.30  2001/12/19 18:57:24  jay
;; I made some changes with inserting grades.
;; I changed it so it will automatically evaluate a formula column.
;;
;; Revision 1.29  2001/12/19 16:56:05  jaycvs
;; I changed the working of grade-forward-row, -next-row.
;;
;; Revision 1.28  2001/12/19 16:19:05  jaycvs
;; I changed the workings of grade-forward- and grade-backward-column.
;;
;; Revision 1.27  2001/12/19 05:31:36  jaycvs
;; I added the class averages to the student reports, with
;; the option of not including them.
;;
;; Revision 1.26  2001/12/19 04:57:39  jaycvs
;; I fixed a minor bug with the insertion of student grade.
;; I made marking students and columns a toggle.


(require 'calc)
(require 'easymenu)
(provide 'grade)

(defgroup grade nil
  "Grade mode"
  :prefix "grade-"
  :tag    "Grade")

(defcustom grade-fname-column-width 10
  "The amount of space used in the first name column"
  :group 'grade
  :type 'integer)

(defcustom grade-lname-column-width 10
  "The amount of space used in the last name column"
  :group 'grade
  :type 'integer)

(defcustom grade-studentid-column-width 9
  "The amount of space used in the student id column"
  :group 'grade
  :type 'integer)

(defcustom grade-score-column-width 5
  "The amount of space used in the score column"
  :group 'grade
  :type 'integer)

(defcustom grade-default-directory "~/Grades/"
  "The default directory which contains the grade files"
  :group 'grade
  :type 'directory)

(defcustom grade-use-alphabetical-order t
  "Non nil means insert the students in alphabetical order"
  :group 'grade
  :type 'boolean)

(defcustom grade-put-avg-in-reports t
  "Non nil means that student reports should include the grade averages"
  :group 'grade
  :type 'boolean)

(defcustom grade-use-studentid t
  "Non nil means that student ids should be part of the records"
  :group 'grade
  :type 'boolean)

(defcustom grade-default-instructor-name ""
  "The name of the instructor"
  :group 'grade
  :type 'string)

(defcustom grade-tex-students-per-page 35
  "How many students to put on a TeXed page"
  :group 'grade
  :type 'integer)

(defcustom grade-reports-grades-per-page 30
  "How many grades to put on each report page"
  :group 'grade
  :type 'integer)

(defcustom grade-aux-dir "aux"
  "The name of the auxiliary directory"
  :group 'grade
  :type 'string)

(defcustom grade-first-name-first t
  "Whether or not the first name is listed first"
  :group 'grade
  :type 'boolean)

(defcustom grade-always-insert-local-variables t
  "Non-nil means insert the local variables in any new grade file."
  :group 'grade
  :type 'boolean)

(defcustom grade-latex-options "12pt"
  "Options to pass the documentclass."
  :group 'grade
  :type 'string)

;;; Some variables to keep track of things

(defvar grade-report-column-width nil)
(defvar grade-report-avg-column nil)
(defvar grade-total-columns nil)
(defvar grade-total-rows nil)
(defvar grade-point-min nil)
(defvar grade-point-max nil)
(defvar grade-invisible-students 0)
(defvar grade-invisible-columns 0)
(defvar grade-first-column-width nil)
(defvar grade-second-column-width nil)
(defvar grade-name1-string nil)
(defvar grade-name2-string nil)
(defvar grade-tex-columns-per-page nil)

;;; Utility

(unless (fboundp 'string>)
  (defun string> (str1 str2)
    (string< str2 str1)))

;;; The next few are for Xemacs
(unless (fboundp 'line-beginning-position)
  (defun line-beginning-position ()
    (save-excursion
      (beginning-of-line)
      (point))))

(unless (fboundp 'line-end-position)
  (defun line-end-position ()
    (save-excursion
      (end-of-line)
      (point))))

(unless (fboundp 'overlay-put)
  (defalias 'overlay-put 'mic-overlay-put))

(unless (fboundp 'make-overlay)
  (defalias 'make-overlay 'mic-make-overlay))

(unless (fboundp 'delete-overlay)
  (defalias 'delete-overlay 'mic-delete-overlay))

(unless (fboundp 'overlays-in)
  (defun overlays-in (beg end)
    (extent-list nil beg end)))

;; (defun grade-next-line (n)
;;   "Move up or down N lines, keeping the same column"
;;   (let ((col (- (point) (line-beginning-position)))
;;         (l))
;;     (beginning-of-line)
;;     (next-line n)
;;     (setq l (- (line-end-position) (line-beginning-position)))
;;     (if (>= l col)
;;         (forward-char col)
;;       (forward-char l)
;;       (while (< l col)
;;         (insert " ")
;;         (setq l (1+ l))))))

(defun grade-next-line (n)
  "Move up or down N lines, keeping the same column"
  (let ((col (- (point) (line-beginning-position)))
        (l))
    (beginning-of-line)
    (next-line n)
    (setq l (- (line-end-position) (line-beginning-position)))
    (if (>= l col)
        (forward-char col)
      (forward-char l))))

(defun grade-add-to-list (list-var element) ;; From subr.el
  "Add to the end of the value of LIST-VAR the element ELEMENT."
  (set list-var
       (append (symbol-value list-var) (list element))))

(defun grade-remove-end-spaces (string)
  "Remove the spaces at the ends of a string"
  (while (and (> (length string) 0) (string= (substring string 0 1) " "))
    (setq string (substring string 1)))
  (while (and (> (length string) 0) (string= (substring string -1) " "))
    (setq string (substring string 0 -1)))
  string)

(defun grade-insert-values-into-formula (gvalues formula)
  (let ((val)
        (nm)
        (vl))
    (with-temp-buffer
      (insert formula)
      (while gvalues
        (goto-char 0)
        (setq val (car gvalues))
        (setq nm (car val))
        (setq vl (cdr val))
        (if (= (string-to-number vl) 0) (setq vl "0"))
        (replace-string (concat "<" nm ">") vl)
        (setq gvalues (cdr gvalues)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun grade-count-lines-from (beg)
  (let ((end (point))
        (i 0))
    (if (< (point) beg)
        -1
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "\n" end t)
          (setq i (1+ i))))
      i)))

(defun grade-in-table-p ()
  (let ((pt (point)))
    (if (and (>= pt grade-point-min)
             (<= pt grade-point-max))
        t
      (message "Not in table")
      nil)))

;;; Motion

(defun grade-goto-first-column ()
  "Go to the first column"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
      (goto-char (1+ (line-beginning-position)))))

(defun grade-next-column ()
  "Go forward one column"
  (let ((colnum (grade-column-number)))
    (if (= colnum grade-total-columns)
        nil
      (cond
       ((= colnum 1)
        (forward-char (1+ grade-first-column-width)))
       (grade-use-studentid
        (cond ((= colnum 2)
               (forward-char (1+ grade-second-column-width)))
              ((= colnum 3)
               (forward-char (+  grade-studentid-column-width
                                 grade-score-column-width)))
              (t
               (forward-char (1+ grade-score-column-width)))))
       (t
        (cond ((= colnum 2)
               (forward-char (+  grade-second-column-width
                                 grade-score-column-width)))
              (t
               (forward-char (1+ grade-score-column-width))))))
      (if (get-char-property (point) 'invisible)
          (grade-next-column)
        t))))

(defun grade-forward-column (&optional n)
  "Go forward N columns"
  (interactive "p")
    (when (or (not (interactive-p)) (grade-in-table-p))
      (unless n (setq n 1))
      (let ((pt (point))
            (i 1)
            (ok t))
        (while (and (<= i n) ok)
          (setq ok (grade-next-column))
          (setq i (1+ i)))
        (unless ok
          (goto-char pt)
          (message "No such column")))))

(defun grade-interactive-forward-column (&optional n)
  "Go forward N columns.
Make sure point is in right position."
  (interactive "p")
  (grade-goto-proper-cell-position)
  (grade-forward-column n))

(defun grade-goto-column (n)
  "Go to the nth column of the row"
  (grade-goto-first-column)
  (grade-forward-column (- n 1)))

(defun grade-next-column-nohide ()
  "Go forward one column"
  (let ((colnum (grade-column-number)))
    (if (= colnum grade-total-columns)
        nil
      (cond
       ((= colnum 1)
        (forward-char (1+ grade-first-column-width)))
       (grade-use-studentid
        (cond ((= colnum 2)
               (forward-char (1+ grade-second-column-width)))
              ((= colnum 3)
               (forward-char (+  grade-studentid-column-width
                                 grade-score-column-width)))
              (t
               (forward-char (1+ grade-score-column-width)))))
       (t
        (cond ((= colnum 2)
               (forward-char (+  grade-second-column-width
                                 grade-score-column-width)))
              (t
               (forward-char (1+ grade-score-column-width)))))))))

(defun grade-forward-column-nohide (&optional n)
  "Go forward N columns"
  (when (or (not (interactive-p)) (grade-in-table-p))
    (unless n (setq n 1))
    (if (> (+ (grade-column-number) n) grade-total-columns)
        (message "No such column")
      (let ((i 1))
        (while (<= i n)
          (grade-next-column-nohide)
          (setq i (1+ i)))))))

(defun grade-goto-column-nohide (n)
  "Go to the nth column of the row"
  (grade-goto-first-column)
  (grade-forward-column-nohide (- n 1)))

(defun grade-previous-column ()
  "Go backward one column"
  (let ((colnum (grade-column-number)))
    (cond
     ((= colnum 1)
      (message "No such column"))
     ((= colnum 2)
      (forward-char (- (1+ grade-first-column-width))))
     ((= colnum 3)
      (if grade-use-studentid
          (forward-char (- (1+ grade-second-column-width)))
        (forward-char (- (+ grade-second-column-width
                            grade-score-column-width)))))
     ((= colnum 4)
      (if grade-use-studentid
          (forward-char (- (+ grade-studentid-column-width
                              grade-score-column-width)))
        (forward-char (- (1+ grade-score-column-width)))))
     (t
      (forward-char (- (1+ grade-score-column-width)))))
    (if (get-char-property (point) 'invisible)
        (grade-previous-column))))

(defun grade-backward-column (&optional n)
  "Go backward N columns"
  (interactive "p")
  (when (or (not (interactive-p)) (grade-in-table-p))
    (unless n (setq n 1))
    (if (< (grade-column-number) n)
        (message "No such column")
      (let ((i 1))
        (while (<= i n)
          (grade-previous-column)
          (setq i (1+ i)))))))

(defun grade-interactive-backward-column (&optional n)
  "Go backward N columns.
Make sure the point is in the right position."
  (interactive "p")
  (grade-goto-proper-cell-position)
  (grade-backward-column n))

(defun grade-column-number ()
  "Return the column as a number"
  (let ((col (- (point) (line-beginning-position)))
        (i))
    (if (= (point) (line-end-position))
        grade-total-columns
      (cond
       ((<= col (1+ grade-first-column-width)) 1)
       ((<= col (+ grade-first-column-width grade-second-column-width 2)) 2)
       ((and grade-use-studentid
             (<= col (+ grade-first-column-width
                        grade-second-column-width
                        grade-studentid-column-width
                        3)))
        3)
       (t (if grade-use-studentid
              (+ 4 (/ (- col grade-first-column-width
                         grade-second-column-width
                         grade-studentid-column-width
                         3)
                      (1+ grade-score-column-width)))
            (+ 3 (/ (- col grade-first-column-width
                       grade-second-column-width 2)
                  (1+ grade-score-column-width)))))))))

(defun grade-column-number-visible ()
  "Return the column number from visible columns"
  (if (= grade-invisible-columns 0)
      (grade-column-number)
    (let ((c 1)
          (pt))
      (setq pt (point))
      (save-excursion
        (grade-goto-first-column)
        (while (< (point) pt)
          (grade-forward-column)
          (setq c (1+ c))))
      c)))

(defun grade-number-of-columns ()
  "Return the number of columns"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^ -")
    (re-search-forward "- *$")
    (search-backward "-")
    (grade-next-line 1)
    (grade-column-number)))

(defun grade-goto-last-column ()
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (grade-goto-column (- grade-total-columns grade-invisible-columns))))

(defun grade-goto-first-row ()
  "Go to the first row of the column"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((col (grade-column-number)))
      (grade-goto-table)
      (grade-goto-column-nohide col))))

(defun grade-forward-line (n)
  "Go forward N lines, ignoring invisible text."
  (let ((col (- (point) (line-beginning-position)))
        (i 0))
    (while (< i n)
      (forward-line 1)
      (while (get-char-property (point) 'invisible)
        (forward-line 1))
      (setq i (1+ i)))
    (forward-char col)))

(defun grade-forward-row (&optional n)
  "Go forward N rows"
  (interactive "p")
  (when (or (not (interactive-p)) (grade-in-table-p))
    (unless n (setq n 1))
    (let ((rownum (grade-row-number)))
      (if (and (<= (+ rownum n) grade-total-rows)
               (> (+ rownum n) 0))
          (grade-forward-line (* n 2))
        (message "No such row")))))

(defun grade-interactive-forward-row (&optional n)
  "Go forward N rows.
Make sure the point is in the right place."
  (interactive "p")
  (grade-goto-proper-cell-position)
  (grade-forward-row n))

(defun grade-forward-line-nohide (n)
  "Go forward N lines, ignoring invisible text."
  (let ((col (- (point) (line-beginning-position)))
        (i 0))
    (while (< i n)
      (forward-line 1)
      (setq i (1+ i)))
    (forward-char col)))

(defun grade-forward-row-nohide (&optional n)
  "Go forward N rows"
  (interactive "p")
  (when (or (not (interactive-p)) (grade-in-table-p))
    (unless n (setq n 1))
    (let ((rownum (grade-row-number)))
      (if (and (<= (+ rownum n) grade-total-rows)
               (> (+ rownum n) 0))
          (grade-forward-line-nohide (* n 2))
        (message "No such row")))))

(defun grade-backward-line (n)
  "Go backward N lines, ignoring invisible text."
  (let ((col (- (point) (line-beginning-position)))
        (i 0))
    (while (< i n)
      (forward-line -1)
      (while (get-char-property (point) 'invisible)
        (forward-line -1))
      (setq i (1+ i)))
    (forward-char col)))

(defun grade-backward-row (&optional n)
  "Go backward N rows"
  (interactive "p")
  (when (or (not (interactive-p)) (grade-in-table-p))
    (unless n (setq n 1))
    (let ((rownum (grade-row-number)))
      (if (and (<= (- rownum n) grade-total-rows)
               (> (- rownum n) 0))
          (grade-backward-line (* n 2))
        (message "No such row")))))

(defun grade-interactive-backward-row (&optional n)
  "Go backwards N rows.
Make sure the point is in the right position."
  (interactive "p")
  (grade-goto-proper-cell-position)
  (grade-backward-row n))

(defun grade-goto-row (n)
  "Go to row N"
  (grade-goto-first-row)
  (grade-forward-row (- n 1)))

(defun grade-goto-row-nohide (n)
  "Go to row N"
  (grade-goto-first-row)
  (grade-forward-row-nohide (- n 1)))

(defun grade-row-number ()
  "Return the row as a number"
  (let ((pt)
        (l))
    (save-excursion
      (grade-goto-first-row)
      (setq pt (point)))
    (setq l (grade-count-lines-from pt))
    (1+ (/ l 2))))

(defun grade-number-of-rows ()
  "Return the number of rows"
  (save-excursion
    (grade-goto-table)
    (setq pt1 (point))
    (goto-char (point-max))
    (re-search-backward "^ -")
    (forward-char 1)
    (grade-next-line -1)
    (1+ (/ (grade-count-lines-from pt1) 2))))

(defun grade-goto-last-row ()
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (grade-goto-row (- grade-total-rows grade-invisible-students))))

(defun grade-goto-table ()
  "Go to the top left-hand corner of the table"
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^ -")
  (forward-char -1)
  (grade-next-line 1))

(defun grade-goto-end-of-table ()
  "Go to the bottom right-hand corner of the table"
  (interactive)
  (grade-goto-table)
  (grade-goto-last-row)
  (grade-goto-last-column))

(defun grade-set-rows-columns ()
  (interactive)
  (setq grade-total-columns (grade-number-of-columns))
  (setq grade-total-rows (grade-number-of-rows)))

(defun grade-find-boundaries ()
  (interactive)
  (save-excursion
    (grade-goto-table)
    (grade-next-line -1)
    (setq grade-point-min (line-beginning-position))
    (grade-goto-end-of-table)
    (grade-next-line 1)
    (setq grade-point-max (line-end-position))))

;;; Cell type

(defun grade-first-name-cell-p ()
  (let ((rn (grade-row-number)))
    (and (if grade-first-name-first
             (= (grade-column-number) 1)
           (= (grade-column-number) 2))
         (> rn 2)
         (< rn grade-total-rows))))

(defun grade-last-name-cell-p ()
  (let ((rn (grade-row-number)))
    (and (if grade-first-name-first
             (= (grade-column-number) 2)
           (= (grade-column-number) 1))
         (> rn 2)
         (< rn grade-total-rows))))

(defun grade-studentid-cell-p ()
  (and grade-use-studentid
       (= (grade-column-number) 3)
       (> (grade-row-number) 2)
       (< (grade-row-number) grade-total-rows)))

(defun grade-column-name-cell-p ()
  (and (= (grade-row-number) 1)
       (or
        (> (grade-column-number) 3)
        (and (not grade-use-studentid) (= (grade-column-number) 3)))))

(defun grade-points-cell-p ()
  (and (= (grade-row-number) 2)
       (or
        (> (grade-column-number) 3)
        (and (not grade-use-studentid) (= (grade-column-number) 3)))))

(defun grade-score-cell-p ()
  (and (> (grade-row-number) 2)
       (< (grade-row-number) grade-total-rows)
       (or (> (grade-column-number) 3)
           (and (not grade-use-studentid) (= (grade-column-number) 3)))))

(defun grade-column-type ()
  (save-excursion
    (grade-goto-row 2)
    (let ((ctype (grade-score-cell-contents)))
      (if (> (string-to-number ctype) 0)
          "number"
        (if (string= ctype (make-string grade-score-column-width ?=))
            "formula"
          "other")))))

;;; Position in cells

(defun grade-cell-beginning ()
  "Return the first point of the cell"
  (let (pt)
    (save-excursion
      (grade-goto-cell-beginning)
      (setq pt (point)))
    pt))

(defun grade-goto-cell-beginning ()
  "Go to the first point of the cell"
  (let ((gcn (grade-column-number)))
    (grade-goto-column-nohide gcn)
    (cond
     ((= gcn 1)
      nil)
     ((= gcn 2)
      nil)
     (t
      (if (and grade-use-studentid (= gcn 3))
          nil
        (forward-char (- (1- grade-score-column-width))))))))

(defun grade-goto-cell-end ()
  "Go to the last point of the cell"
  (grade-goto-column-nohide (grade-column-number)))

(defun grade-goto-proper-cell-position ()
  "Go to the first or last point of cell, as appropriate"
  (let ((gcn (grade-column-number)))
    (if (or (< gcn 3) (and grade-use-studentid (= gcn 3)))
        (grade-goto-cell-beginning)
      (grade-goto-cell-end))))

;;; Information commands

(defun grade-score-cell-contents ()
  "Return the contents of the cell as a string"
  (let ((beg (grade-cell-beginning)))
    (grade-remove-end-spaces
     (buffer-substring-no-properties
      beg (+ beg grade-score-column-width)))))

(defun grade-fname-cell-contents ()
  "Return the contents of the first name cell"
  (let ((beg (grade-cell-beginning)))
    (grade-remove-end-spaces
     (buffer-substring-no-properties
      beg (+ beg grade-fname-column-width)))))

(defun grade-lname-cell-contents ()
  "Return the contents of the first name cell"
  (let ((beg (grade-cell-beginning)))
    (grade-remove-end-spaces
     (buffer-substring-no-properties
      beg (+ beg grade-lname-column-width)))))

(defun grade-studentid-cell-contents ()
  "Return the contents of the student id cell"
  (let ((beg (grade-cell-beginning)))
    (grade-remove-end-spaces
     (buffer-substring-no-properties
      beg (+ beg grade-studentid-column-width)))))

(defun grade-student-first-name ()
  "Return the first name of the student"
  (let ((fname))
    (save-excursion
      (if grade-first-name-first
          (grade-goto-first-column)
        (grade-goto-column 2))
      (setq fname (grade-fname-cell-contents)))
    fname))

(defun grade-student-last-name ()
  "Return the first name of the student"
  (let ((lname))
    (save-excursion
      (if grade-first-name-first
          (grade-goto-column 2)
        (grade-goto-first-column))
      (setq lname (grade-lname-cell-contents)))
    lname))

(defun grade-student-name ()
  "Return the name of the student"
  (concat (grade-student-first-name) " " (grade-student-last-name)))

(defun grade-studentid ()
  "Return the id of the student"
  (if (not grade-use-studentid)
      nil
    (save-excursion
      (grade-goto-column 3)
      (grade-studentid-cell-contents))))

(defun grade-column-name ()
  "Return the name of the column,
the contents of the top cell"
  (save-excursion
    (grade-goto-first-row)
    (grade-score-cell-contents)))

(defun grade-column-average ()
  "Return the average of the column,
the contents of the bottom cell"
  (save-excursion
    (grade-goto-last-row)
    (grade-score-cell-contents)))

(defun grade-column-points ()
  "Return the possible points of the column,
the contents of the second cell"
  (save-excursion
    (grade-goto-row 2)
    (grade-score-cell-contents)))

(defun grade-student-scores ()
  "Return the scores for a student,
in the form ((column-name . column-value) ...)"
  (let ((info nil)
        (i))
    (if grade-use-studentid (setq i 4) (setq i 3))
    (save-excursion
      (grade-goto-column-nohide i)
      (while (< i grade-total-columns)
        (grade-add-to-list
               'info
               (cons (grade-column-name) (grade-score-cell-contents)))
        (grade-forward-column-nohide 1)
        (setq i (1+ i)))
      (grade-add-to-list
               'info
               (cons (grade-column-name) (grade-score-cell-contents))))
    info))

(defun grade-student-info ()
  "Return the information for a student,
in the form ((column-name possible-points points average) ...)"
  (let ((info nil)
        (i))
    (if grade-use-studentid (setq i 4) (setq i 3))
    (when (not (= (- grade-total-columns grade-invisible-columns) (- i 1)))
      (save-excursion
        (grade-goto-column i)
        (while (< i (- grade-total-columns grade-invisible-columns))
          (grade-add-to-list
           'info
           (list (grade-column-name) (grade-column-points)
                 (grade-score-cell-contents) (grade-column-average)))
          (grade-forward-column 1)
          (setq i (1+ i)))
        (grade-add-to-list
         'info
         (list (grade-column-name) (grade-column-points)
               (grade-score-cell-contents) (grade-column-average)))))
    info))

(defun grade-row-info ()
  "Return the contents of the row,
in the form (col1 col2 col3 ..)"
  (let ((info)
        (col 2))
    (save-excursion
      (grade-goto-first-column)
      (if grade-first-name-first
          (setq info (list (grade-fname-cell-contents)))
        (setq info (list (grade-lname-cell-contents))))
      (grade-forward-column 1)
      (if grade-first-name-first
          (setq info (cons (grade-lname-cell-contents) info))
        (setq info (cons (grade-fname-cell-contents) info)))
      (if grade-use-studentid
          (progn
            (setq col 3)
            (grade-forward-column 1)
            (setq info (cons (grade-studentid-cell-contents) info))))
      (while (< col (- grade-total-columns grade-invisible-columns))
        (grade-forward-column 1)
        (setq info (cons (grade-score-cell-contents) info))
        (setq col (1+ col))))
    (reverse info)))

(defun grade-column-formula ()
  "Get the formula for the current column"
  (let ((name (grade-column-name)))
    (save-excursion
      (if (re-search-forward (concat "^" name "=") (point-max) t)
          (buffer-substring-no-properties (point) (line-end-position))
        (message (concat "No formula for " name))))))

(defun grade-class-name ()
  "Return the name of the class"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^Class: ")
    (buffer-substring-no-properties (point) (line-end-position))))

(defun grade-section-name ()
  "Return the section of the class"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Section: " (point-max) t)
        (buffer-substring-no-properties (point) (line-end-position))
      nil)))

(defun grade-semester-name ()
  "Return the semester of the class"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Semester: " (point-max) t)
        (buffer-substring-no-properties (point) (line-end-position))
      nil)))

(defun grade-instructor-name ()
  "Return the semester of the class"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Instructor: " (point-max) t)
        (buffer-substring-no-properties (point) (line-end-position))
      nil)))

(defun grade-formulas ()
  "Return the collection of formulas used in the grade book"
  (let ((beg))
    (save-excursion
      (goto-char (point-min))
      (search-forward "===== Formulas =====")
      (forward-line 1)
      (if (= (point) (point-max))
          ""
        (while (and (not (= (point) (point-max))) (looking-at "^ *$"))
          (forward-line 1))
        (if (looking-at "^Local Variables:$")
            nil
          (setq beg (point))
          (if (re-search-forward "^Local Variables:" (point-max) t)
              (forward-line -1)
            (goto-char (point-max)))
          (beginning-of-line)
          (while (and (looking-at "^ *$") (< beg (point)))
            (forward-line -1))
          (end-of-line)
          (buffer-substring-no-properties beg (point)))))))

(defun grade-tex-formulas ()
  "Return the collection of formulas used in the grade book,
suitable to be TeXed."
  (let ((form (grade-formulas)))
    (if (or (not form) (string= form ""))
        nil
      (with-temp-buffer
        (insert form)
        (goto-char (point-min))
        (replace-string "<" "\\textit{")
        (goto-char (point-min))
        (replace-string ">" "}")
        (goto-char (point-min))
        (replace-string "=" "} &=& ")
        (goto-char (point-min))
        (replace-regexp "^" "\\\\textbf{")
        (goto-char (point-min))
        (replace-regexp "$" "\\\\\\\\")
        (goto-char (point-min))
        (insert "\n\\begin{center}\n")
        (insert "\n\\textbf{$\\dagger$ Formulas used}\n")
        (insert "\\end{center}\n")
        (insert "\\small\n")
        (insert "\\begin{eqnarray*}\n")
        (goto-char (point-max))
        (insert "\n\\end{eqnarray*}\n")
        (insert "\\normalsize\n")
        (buffer-substring-no-properties (point-min) (point-max))))))

;;; Insertion command

(defun grade-insert-score (string)
  "Insert information into a score cell"
  (let ((i)
        (buffer-read-only nil))
    (setq string (grade-remove-end-spaces string))
    (setq i (length string))
    (if (> i grade-score-column-width)
        (setq string (substring string 0 grade-score-column-width))
      (while (< i grade-score-column-width)
        (setq string (concat " " string))
        (setq i (1+ i))))
    (grade-goto-cell-beginning)
    (delete-char grade-score-column-width)
    (insert string)
    (forward-char (- 1))))

(defun grade-insert-fname (string)
  "Insert information into an fname cell"
  (let ((i)
        (buffer-read-only nil))
    (setq string (grade-remove-end-spaces string))
    (setq i (length string))
    (if (> i grade-fname-column-width)
        (setq string (substring string 0 grade-fname-column-width))
      (while (< i grade-fname-column-width)
        (setq string (concat string " "))
        (setq i (1+ i))))
    (grade-goto-cell-beginning)
    (delete-char grade-fname-column-width)
    (insert string)
    (backward-char grade-fname-column-width)))

(defun grade-insert-lname (string)
  "Insert information into an lname cell"
  (let ((i)
        (buffer-read-only nil))
    (setq string (grade-remove-end-spaces string))
    (setq i (length string))
    (if (> i grade-lname-column-width)
        (setq string (substring string 0 grade-lname-column-width))
      (while (< i grade-lname-column-width)
        (setq string (concat string " "))
        (setq i (1+ i))))
    (grade-goto-cell-beginning)
    (delete-char grade-lname-column-width)
    (insert string)
    (backward-char grade-lname-column-width)))

(defun grade-insert-studentid (string)
  "Insert information into a studentid cell"
  (let ((i)
        (buffer-read-only nil))
    (setq string (grade-remove-end-spaces string))
    (setq i (length string))
    (if (> i grade-studentid-column-width)
        (setq string (substring string 0 grade-studentid-column-width))
      (while (< i grade-studentid-column-width)
        (setq string (concat string " "))
        (setq i (1+ i))))
    (grade-goto-cell-beginning)
    (delete-char grade-studentid-column-width)
    (insert string)
    (backward-char grade-studentid-column-width)))

(defun grade-insert-points-cell (contents)
  "Insert information into a points cell"
  (if (or (string= contents "")
          (not (string= (substring contents 0 1) "=")))
      (grade-insert-score contents)
    (grade-write-formula contents)
    (setq contents (make-string grade-score-column-width ?=))
    (grade-insert-score contents)
    (grade-adjust-for-new-formula)))

(defun grade-insert-points-cell-noask (contents)
  "Insert information into a points cell"
  (if (or (string= contents "")
          (not (string= (substring contents 0 1) "=")))
      (grade-insert-score contents)
    (grade-write-formula contents)
    (setq contents (make-string grade-score-column-width ?=))
    (grade-insert-score contents)))

(defun grade-write-formula (formula)
  "Write the formula at the bottom of the buffer"
  (let ((name (grade-column-name))
        (buffer-read-only nil))
    (save-excursion
      (if (re-search-forward (concat "^" name "=") (point-max) t)
          (progn
            (beginning-of-line)
            (kill-line)
            (insert name formula))
        (search-forward "===== Formulas =====")
        (forward-line 1)
        (while (not (looking-at "^ *$"))
          (forward-line 1))
        (insert name formula "\n")))))

(defun grade-insert-local-variables ()
  "Insert the local variables at the end of the buffer"
  (interactive)
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-max))
      (insert "Local Variables:\n")
;      (insert "mode: grade\n")
      (insert "grade-fname-column-width: ")
      (insert (int-to-string grade-fname-column-width) "\n")
      (insert "grade-lname-column-width: ")
      (insert (int-to-string grade-lname-column-width) "\n")
      (insert "grade-studentid-column-width: ")
      (insert (int-to-string grade-studentid-column-width) "\n")
      (insert "grade-score-column-width: ")
      (insert (int-to-string grade-score-column-width) "\n")
      (insert "grade-use-alphabetical-order: ")
      (if grade-use-alphabetical-order
          (insert "t\n")
        (insert "nil\n"))
      (insert "grade-use-studentid: ")
      (if grade-use-studentid
          (insert "t\n")
        (insert "nil\n"))
      (insert "grade-first-name-first: ")
      (if grade-first-name-first
          (insert "t\n")
        (insert "nil\n"))
      (insert "End:\n"))))

(defun grade-replace-local-variables ()
  "Replace the local variables with new values"
  (interactive)
  (save-excursion
    (if (re-search-forward "^Local Variables:" (point-max) t)
        (let ((buffer-read-only nil))
          (beginning-of-line)
          (delete-region (point) (point-max)))))
  (grade-insert-local-variables))

;;; Cell editing commands

(defun grade-edit-score-cell ()
  "Edit the contents of a score cell"
  (let* ((oldcontents (grade-score-cell-contents))
         (newcontents)
         (name (grade-student-name))
         (hw (grade-column-name))
         (buffer-read-only nil))
    (setq newcontents
           (read-from-minibuffer (concat name "'s " hw ": ") oldcontents))
    (grade-insert-score newcontents)))

(defun grade-edit-single-score-cell ()
  "Edit the contents of a score cell"
  (let* ((oldcontents (grade-score-cell-contents))
         (newcontents)
         (name (grade-student-name))
         (hw (grade-column-name))
         (buffer-read-only nil))
    (setq newcontents
           (read-from-minibuffer (concat name "'s " hw ": ") oldcontents))
    (grade-insert-score newcontents)
    (save-excursion
      (if (string= (grade-column-type) "number")
          (grade-average-column)))
    (grade-adjust-row)))

(defun grade-edit-fname-cell ()
  "Edit the contents of a first name cell"
  (let* ((oldname (grade-fname-cell-contents))
         (newname)
         (buffer-read-only nil))
    (setq newname
           (read-from-minibuffer "First name: " oldname))
    (grade-insert-fname newname)))

(defun grade-edit-lname-cell ()
  "Edit the contents of a last name cell"
  (let* ((oldname (grade-fname-cell-contents))
         (newname)
         (buffer-read-only nil))
    (setq newname
           (read-from-minibuffer "Last name: " oldname))
    (grade-insert-lname newname)))

(defun grade-edit-studentid-cell ()
  "Edit the contents of a student id cell"
  (let* ((oldid (grade-studentid-cell-contents))
         (newid)
         (name (grade-student-name))
         (buffer-read-only nil))
    (setq newid
           (read-from-minibuffer (concat name "'s ID: ") oldid))
    (grade-insert-studentid newid)))

(defun grade-edit-score-name-cell ()
  "Edit the contents of a score name cell"
  (let* ((oldcontents (grade-score-cell-contents))
         (newcontents)
         (buffer-read-only nil))
    (setq newcontents
           (read-from-minibuffer "Name: " oldcontents))
    (grade-insert-score newcontents)
    (if (string= (grade-column-type) "formula")
        (save-excursion
          (re-search-forward (concat "^" oldcontents "="))
          (forward-char -1)
          (delete-region (line-beginning-position) (point))
          (insert newcontents)))
    (save-excursion
      (search-forward "===== Formulas =====")
      (let ((case-replace nil))
        (replace-string (concat "<" oldcontents ">")
                        (concat "<" newcontents ">"))))))

(defun grade-edit-points-cell ()
  "Edit the contents of a points cell"
  (let ((contents (grade-score-cell-contents))
        (newcontents)
        (buffer-read-only nil))
    (if (and (not (string= contents ""))
             (string= contents (make-string grade-score-column-width ?=)))
        (grade-edit-formula-cell)
      (setq newcontents (read-from-minibuffer "Points: " contents))
      (grade-insert-points-cell newcontents))))

(defun grade-edit-formula-cell ()
  "Edit the contents of a formula cell"
  (let ((name (grade-column-name))
        (formula nil)
        (buffer-read-only nil)
        (newcontents))
    (save-excursion
      (if (re-search-forward (concat "^" name "=") (point-max) t)
          (setq formula
              (buffer-substring-no-properties (- (point) 1)
                                              (line-end-position)))))
    (setq newcontents (read-from-minibuffer "Points: " formula))
    (unless (and (not (string= newcontents ""))
                 (string= "=" (substring newcontents 0 1)))
      (grade-remove-formula name))
    (grade-insert-points-cell newcontents)))

(defun grade-edit-or-eval-cell ()
  "Edit a cell"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (cond
     ((grade-score-cell-p)
      (if (string= (grade-column-type) "formula")
          (grade-eval-cell)
        (grade-edit-single-score-cell)))
     ((grade-first-name-cell-p) (grade-edit-fname-cell))
     ((grade-last-name-cell-p) (grade-edit-lname-cell))
     ((grade-studentid-cell-p) (grade-edit-studentid-cell))
     ((grade-column-name-cell-p) (grade-edit-score-name-cell))
     ((grade-points-cell-p) (grade-edit-points-cell))
     (t (message "Not an editable cell")))))

(defun grade-edit-class-name ()
  "Edit the name of the class"
  (interactive)
  (let ((oldname)
        (newname)
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^Class: ")
      (setq oldname
            (buffer-substring-no-properties (point) (line-end-position)))
      (setq newname (read-from-minibuffer "Class name: " oldname))
      (delete-region (point) (line-end-position))
      (insert newname)
      (grade-find-boundaries))))

(defun grade-edit-section-name ()
  "Edit the section of the class"
  (interactive)
  (let ((oldname)
        (newname)
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^Section: " (point-max) t)
          (progn
            (setq oldname
                  (buffer-substring-no-properties (point) (line-end-position)))
            (setq newname (read-from-minibuffer "Section name: " oldname))
            (if (string= newname "")
                (progn
                  (beginning-of-line)
                  (kill-line 1))
              (delete-region (point) (line-end-position))
              (insert newname)))
        (setq newname (read-from-minibuffer "Section name: "))
        (unless (string= newname "")
          (re-search-forward "^Class:")
          (end-of-line)
          (insert "\n")
          (insert "Section: " newname)))
      (grade-find-boundaries))))

(defun grade-edit-semester-name ()
  "Edit the semester name"
  (interactive)
  (let ((oldname)
        (newname)
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^Semester: " (point-max) t)
          (progn
            (setq oldname
                  (buffer-substring-no-properties (point) (line-end-position)))
            (setq newname (read-from-minibuffer "Semester: " oldname))
            (if (string= newname "")
                (progn
                  (beginning-of-line)
                  (kill-line 1))
              (delete-region (point) (line-end-position))
              (insert newname)))
        (setq newname (read-from-minibuffer "Semester: "))
        (unless (string= newname "")
          (if (re-search-forward "^Section:" (point-max) t)
              (progn
                (end-of-line)
                (insert "\n")
                (insert "Semester: " newname))
            (re-search-forward "^Class:")
            (end-of-line)
            (insert "\n")
            (insert "Semester: " newname))))
      (grade-find-boundaries))))

(defun grade-edit-instructor-name ()
  "Edit the instructor's name"
  (interactive)
  (let ((oldname)
        (newname)
        (buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^Instructor: " (point-max) t)
          (progn
            (setq oldname
                  (buffer-substring-no-properties (point) (line-end-position)))
            (setq newname (read-from-minibuffer "Instructor: " oldname))
            (if (string= newname "")
                (progn
                  (beginning-of-line)
                  (kill-line 1))
              (delete-region (point) (line-end-position))
              (insert newname)))
        (setq newname (read-from-minibuffer "Instructor: "
                                            grade-default-instructor-name))
        (unless (string= newname "")
          (if (re-search-forward "^Semester:" (point-max) t)
              (progn
                (end-of-line)
                (insert "\n")
                (insert "Instructor: " newname))
            (if (re-search-forward "^Section:" (point-max) t)
                (progn
                  (end-of-line)
                  (insert "\n")
                  (insert "Instructor: " newname))
              (re-search-forward "^Class:")
              (end-of-line)
              (insert "\n")
              (insert "Instructor: " newname)))))
      (grade-find-boundaries))))

;;; Display information

(defun grade-display-student-name ()
  "Display the name of the student in the minibuffer"
  (interactive)
  (if grade-use-studentid
      (message (concat (grade-student-name) " (" (grade-studentid) ")"))
    (message (grade-student-name))))

(defun grade-display-column-name ()
  "Display the name of the column in the minibuffer"
  (interactive)
  (message (grade-column-name)))

;;; New information commands (and removing old information)

(defun grade-goto-alphabetical-row (fname lname)
  "Go to the correct row to insert the student"
  (if (> (length fname) grade-fname-column-width)
      (setq fname (substring fname 0 grade-fname-column-width)))
  (if (> (length lname) grade-lname-column-width)
      (setq lname (substring lname 0 grade-lname-column-width)))
  (grade-goto-table)
  (grade-goto-row 3)
  (unless (= grade-total-rows 3)
    (let ((i 3))
      (while (and (< i grade-total-rows)
                  (string> lname (grade-student-last-name)))
        (grade-forward-row 1)
        (setq i (1+ i)))
      (unless (= i grade-total-rows)
        (while (and (< i grade-total-rows)
                    (string= lname (grade-student-last-name))
                    (string> fname (grade-student-first-name)))
          (grade-forward-row 1)
          (setq i (1+ i)))))))

(defun grade-new-student (fname lname sid)
  "Insert a new student (before the current student)"
  (let ((rownum (grade-row-number))
        (i 2)
        (buffer-read-only nil))
    (if (> grade-invisible-columns 0)
        (message "Cannot insert new student when there are invisible scores")
      (if (< rownum 2)
          (message "Cannot insert a student before this row")
        (beginning-of-line)
        (insert "|"
                (make-string grade-first-column-width ?\ )
                "|"
                (make-string grade-second-column-width ?\ ))
        (if grade-use-studentid
            (progn
              (insert "|" (make-string grade-studentid-column-width ?\ ))
              (setq i 3)))
        (while (< i grade-total-columns)
          (insert "|"
                  (make-string grade-score-column-width ?\ ))
          (setq i (1+ i)))
        (insert "|\n")
        (insert "|"
                (make-string grade-first-column-width ?-)
                "|"
                (make-string grade-second-column-width ?-))
        (setq i 2)
        (if grade-use-studentid
            (progn
              (insert "|" (make-string grade-studentid-column-width ?-))
              (setq i 3)))
        (while (< i grade-total-columns)
          (insert "|"
                  (make-string grade-score-column-width ?-))
          (setq i (1+ i)))
        (insert "|\n")
        (forward-char 1)
        (grade-next-line -2)
        (setq grade-total-rows (1+ grade-total-rows))
        (grade-find-boundaries)
        (grade-goto-column 1)
        (if grade-first-name-first
            (grade-insert-fname fname)
          (grade-insert-lname lname))
        (grade-goto-column 2)
        (if grade-first-name-first
            (grade-insert-lname lname)
          (grade-insert-fname fname))
        (if grade-use-studentid
            (progn
              (grade-goto-column 3)
              (grade-insert-studentid sid)))
        (grade-goto-column 1)))))

(defun grade-single-new-student (arg)
  "Insert a single new student"
  (interactive "P")
  (if (> grade-invisible-columns 0)
      (message "Cannot insert new student when there are invisible scores")
    (let ((name1 (read-string grade-name1-string))
          (name2 (read-string grade-name2-string))
          (sid nil)
          (fname)
          (lname))
      (if grade-first-name-first
          (setq fname name1
                lname name2)
        (setq fname name2
              lname name1))
      (if grade-use-studentid
          (setq sid (read-string (concat fname " " lname "'s ID: "))))
      (if (and grade-use-alphabetical-order (not arg))
          (grade-goto-alphabetical-row fname lname)
        (unless (or (not (interactive-p)) (grade-in-table-p))
          (grade-goto-table)
          (grade-goto-row 2)))
      (grade-new-student fname lname sid)
      (if (y-or-n-p (concat "Insert scores for " fname " " lname "? "))
          (grade-insert-student-scores)))))

(defun grade-insert-student-scores ()
  "Go through the scores, editing each one for the current student"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((row (grade-row-number))
          (i))
      (if (or (<= row 2) (= row (- grade-total-rows grade-invisible-students )))
          (message "This is not a student row")
        (save-excursion
          (if grade-use-studentid
              (setq i 3)
            (setq i 2))
          (when (> (- grade-total-columns grade-invisible-columns) i)
            (grade-goto-column i)
            (setq i (1+ i))
            (while (<= i (- grade-total-columns grade-invisible-columns))
              (grade-forward-column 1)
              (if (string= (grade-column-type) "formula")
                  (grade-eval-cell)
                (grade-edit-single-score-cell))
              (setq i (1+ i)))))))))

(defun grade-put-student-out-of-misery ()
  "Remove the student on the current line"
  (interactive)
  (let ((rownum (grade-row-number))
        (buffer-read-only nil)
        (sname (grade-student-name))
        (sid nil)
        (cname)
        (sec)
        (sem)
        (pt)
        (auxdir)
        (rembuf))
    (when (or (not (interactive-p)) (grade-in-table-p))
      (if (or (< rownum 3) (= rownum grade-total-rows))
          (message "This row cannot be removed")
        (if (yes-or-no-p (concat "Really remove " sname " ?"))
            (progn
              (setq sid (grade-studentid))
              (setq cname (grade-class-name))
              (setq sec (grade-section-name))
              (setq sem (grade-semester-name))
              (setq auxdir (concat (file-name-directory (buffer-file-name))
                                   grade-aux-dir "/"))
              (unless (file-directory-p auxdir)
                (make-directory auxdir))
              (setq rembuf
                    (find-file-noselect
                     (concat auxdir
                             (file-name-nondirectory (buffer-file-name))
                             "-REMOVED")))
              (save-excursion
                (set-buffer rembuf)
                (goto-char (point-max))
                (insert "\n**********\n"))
              (grade-reports-insert-student sname
                                            sid
                                            (grade-student-info)
                                            rembuf)
              (beginning-of-line)
              (setq pt (point))
              (grade-next-line 2)
              (delete-region pt (point))
              (forward-char 1)
              (save-excursion
                (set-buffer rembuf)
                (insert "\n**********\n")
                (save-buffer))
              (setq grade-total-rows (- grade-total-rows 1))
              (grade-find-boundaries)))))))

(defun grade-remove-formula (frm)
  "Remove the formula"
  (save-excursion
    (re-search-forward (concat "^" frm "="))
    (beginning-of-line)
    (kill-line 1)))

(defun grade-remove-column ()
  "Remove the scores on the current column"
  (interactive)
  (let ((colnum (grade-column-number))
        (name (grade-column-name))
        (isform (string= (grade-column-type) "formula"))
        (in-formula)
        (inform)
        (pm)
        (beg)
        (end)
        (buffer-read-only nil))
    (when (or (not (interactive-p)) (grade-in-table-p))
      (if (or (< colnum 3) (and grade-use-studentid (< colnum 4)))
          (message "This column cannot be removed")
        (save-excursion
          (search-forward "===== Formulas =====")
          (setq in-formula
                (search-forward (concat "<" name ">") (point-max) t))
          (when in-formula
            (beginning-of-line)
            (setq beg (point))
            (search-forward "=")
            (setq end (- (point) 1))
            (setq inform (buffer-substring-no-properties beg end))))
        (if in-formula
            (message
             (concat "This column is used in the formula for " inform))
          (if (yes-or-no-p (concat "Really remove " name "?"))
              (progn
                (save-excursion
                  (grade-goto-first-row)
                  (grade-goto-cell-beginning)
                  (grade-next-line -1)
                  (setq pm (point)))
                (grade-goto-last-row)
                (grade-goto-cell-beginning)
                (grade-next-line 1)
                (while (>= (point) pm)
                  (delete-region (point) (+ (point) grade-score-column-width 1))
                  (grade-next-line -1))
                (grade-next-line 2)
                (if (= colnum grade-total-columns)
                    (forward-char (- 2))
                  (forward-char (1- grade-score-column-width)))
                (setq grade-total-columns (- grade-total-columns 1))
                (grade-find-boundaries)
                (if isform
                    (grade-remove-formula name)))))))))

(defun grade-insert-column ()
  "Insert a new column after the current column"
  (let ((colnum (grade-column-number))
        (i grade-total-rows))
    (when (or (not (interactive-p)) (grade-in-table-p))
      (grade-goto-first-row)
      (cond
       ((= colnum 2)
        (beginning-of-line)
        (forward-char
         (+ grade-fname-column-width grade-lname-column-width 3)))
       ((and grade-use-studentid (= colnum 3))
        (beginning-of-line)
        (forward-char
         (+ grade-fname-column-width
            grade-lname-column-width
            grade-studentid-column-width
            4)))
       (t
        (grade-goto-cell-beginning)
        (forward-char (+ grade-score-column-width 1))))
      (grade-next-line -1)
      (insert (make-string grade-score-column-width ?-)
              " ")
      (forward-char (- (1+ grade-score-column-width)))
      (grade-next-line 1)
      (insert (make-string grade-score-column-width ?\ )
              "|")
      (forward-char (- (1+ grade-score-column-width)))
      (grade-next-line 1)
      (while (> i 1)
        (insert (make-string grade-score-column-width ?-)
                "|")
        (forward-char (- (1+ grade-score-column-width)))
        (grade-next-line 1)
        (insert (make-string grade-score-column-width ?\ )
                "|")
        (forward-char (- (1+ grade-score-column-width)))
        (grade-next-line 1)
        (setq i (- i 1)))
      (insert (make-string grade-score-column-width ?-)
              " ")
      (forward-char -2)
      (grade-next-line -1)
      (setq grade-total-columns (1+ grade-total-columns))
      (grade-find-boundaries)
      (grade-goto-first-row))))

(defun grade-new-column (arg)
  "Insert a new column.
With an argument, insert after current column."
  (interactive "P")
  (let ((tc (if grade-use-studentid 3 2)))
    (when (or (not (interactive-p)) (grade-in-table-p))
      (unless arg (grade-backward-column 1))
      (if (and (or (= (grade-column-number) 1)
                   (and grade-use-studentid (= (grade-column-number) 2)))
               (not (= grade-total-columns tc)))
          (progn
            (message "Cannot insert score column there.")
            (if arg (grade-forward-column 1)))
        (let ((buffer-read-only nil)
              (name (read-string "Grade name: "))
              (points (read-string "Points: ")))
          (if (= grade-total-columns tc)
              (grade-goto-last-column))
          (grade-insert-column)
          (grade-insert-score name)
          (grade-forward-row)
          (grade-insert-points-cell-noask points)
          (grade-goto-first-row)
          (if (and (not (string= points ""))
                   (string= "=" (substring points 0 1)))
              (grade-eval-column);)
            (if (y-or-n-p "Insert scores? ")
                (grade-insert-scores))))))))

(defun grade-new-column-at-end ()
  "Add a new grade column at the end of the table"
  (interactive)
  (grade-goto-table)
  (grade-goto-last-column)
  (grade-new-column t))

(defun grade-insert-scores ()
  "Go through a column, and enter data in each cell"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((rownum 3)
          (buffer-read-only nil)
          (col (grade-column-number)))
      (if (or (<= col 2) (and grade-use-studentid (<= col 3)))
          (message "This is not a grade column")
        (grade-goto-row 3)
        (while (<= (grade-row-number) (- grade-total-rows 1))
          (grade-edit-score-cell)
          (grade-forward-row)))
      (unless (string= (grade-column-type) "other")
        (grade-average-column))
      (grade-goto-first-row))))

(defun grade-new-class ()
  "Start a new class"
  (let ((buffer-read-only nil)
        (class (read-from-minibuffer "Class name: "))
        (sec  (read-from-minibuffer "Section: "))
        (sem (read-from-minibuffer "Semester: "))
        (instr (read-from-minibuffer "Instructor: "
                                     grade-default-instructor-name)))
    (goto-char (point-min))
    (insert "\nClass: " class "\n")
    (unless (string= sec "")
      (insert "Section: " sec "\n"))
    (unless (string= sem "")
      (insert "Semester: " sem "\n"))
    (unless (string= instr "")
      (insert "Instructor: " instr "\n"))
    (insert "\n\n")
    (insert " "
            (make-string grade-first-column-width ?-)
            " "
            (make-string grade-second-column-width ?-)
            " ")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?-)))
    (insert " \n")
    (insert "|"
            (make-string grade-first-column-width ?\ )
            "|"
            (make-string grade-second-column-width ?\ )
            "|")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?\ ) "|"))
    (insert "\n")
    (insert "|"
            (make-string grade-first-column-width ?-)
            "|"
            (make-string grade-second-column-width ?-)
            "|")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?-) "|"))
    (insert "\n")
    (insert "|"
            (make-string grade-first-column-width ?\ )
            "|"
            (make-string grade-second-column-width ?\ )
            "|")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?\ ) "|"))
    (insert "\n")
    (insert "|"
            (make-string grade-first-column-width ?-)
            "|"
            (make-string grade-second-column-width ?-)
            "|")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?-) "|"))
    (insert "\n")
    (insert "|"
            (make-string grade-first-column-width ?\ )
            "|"
            (make-string grade-second-column-width ?\ )
            "|")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?\ ) "|"))
    (insert "\n")
    (insert " "
            (make-string grade-first-column-width ?-)
            " "
            (make-string grade-second-column-width ?-)
            " ")
    (if grade-use-studentid
        (insert (make-string grade-studentid-column-width ?-) " "))
    (insert "\n")
    (goto-char (point-max))
    (insert "\n===== Formulas =====\n\n\n")
    (forward-line -6)
    (forward-char 1)
    (setq grade-total-columns 2)
    (if grade-use-studentid
        (setq grade-total-columns 3))
    (setq grade-total-rows 3)
    (grade-goto-first-row)
    (if grade-first-name-first
        (grade-insert-fname "First name")
      (grade-insert-lname "Last name"))
    (grade-forward-column 1)
    (if grade-first-name-first
        (grade-insert-lname "Last name")
      (grade-insert-fname "First name"))
    (if grade-use-studentid
        (progn
          (grade-forward-column 1)
          (grade-insert-studentid "StudentID")
          (grade-backward-column 1)))
    (grade-backward-column 1)
    (grade-forward-row 1)
    (if grade-first-name-first
        (grade-insert-fname "Points")
      (grade-insert-lname "Points"))
    (grade-forward-row 1)
    (if grade-first-name-first
        (grade-insert-fname "Average")
      (grade-insert-lname "Average"))
    (grade-goto-first-row)
    (if (y-or-n-p "Add student names? ")
        (grade-add-students)
      (if (y-or-n-p "Import data? ")
          (grade-import-data)))
    (if grade-always-insert-local-variables
        (grade-insert-local-variables))
    (run-hooks 'grade-new-class-hook)))

(defun grade-add-students ()
  "Add several new students"
  (interactive)
  (let ((name1)
        (name2)
        (sid nil)
        (buffer-read-only nil))
    (grade-goto-table)
    (grade-goto-first-row)
    (grade-forward-row)
    (grade-forward-row)
    (if grade-first-name-first
        (setq name1 (read-string "First name: "))
      (setq name1 (read-string "Last name: ")))
    (while (not (string= name1 ""))
      (if grade-first-name-first
          (setq name2 (read-string "Last name: "))
        (setq name2 (read-string "First name: ")))
      (if grade-use-studentid
          (if grade-first-name-first
              (setq sid (read-string (concat name1 " " name2 "'s ID: ")))
            (setq sid (read-string (concat name2 " " name1 "'s ID: ")))))
      (if grade-use-alphabetical-order
          (if grade-first-name-first
              (grade-goto-alphabetical-row name1 name2)
            (grade-goto-alphabetical-row name2 name1)))
      (if grade-first-name-first
          (grade-new-student name1 name2 sid)
        (grade-new-student name2 name1 sid))
      (grade-forward-row)
      (if grade-first-name-first
          (setq name1 (read-string "First name: "))
        (setq name1 (read-string "Last name: "))))
    (grade-goto-first-row)))

;;; Moving information around

(defun grade-goto-marked-student ()
  "Go to the student who is marked for moving"
  (let ((pt)
        (end))
    (setq pt (point))
    (grade-goto-table)
    (grade-goto-last-row)
    (setq end (point))
    (grade-goto-table)
    (if (re-search-forward "^\*" end t)
        (point)
      (goto-char pt)
      nil)))

(defun grade-mark-student ()
  "Mark the current student for motion"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (currow (grade-row-number)))
      (if (or (>= currow grade-total-rows) (<= currow 2))
          (message "This is not a student row")
        (if (save-excursion
              (beginning-of-line)
              (looking-at "*"))
            (progn
              (save-excursion
                (beginning-of-line)
                (delete-char 1)
                (insert "|"))
              (if (= (point) (line-beginning-position))
                  (forward-char 1)))
          (save-excursion
            (when (grade-goto-marked-student)
              (beginning-of-line)
              (delete-char 1)
              (insert "|")))
          (save-excursion
            (beginning-of-line)
            (delete-char 1)
            (insert "*"))
          (if (= (point) (line-beginning-position))
              (forward-char 1)))))))

(defun grade-move-student ()
  "Move the marked student to before the current row"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (currow (grade-row-number))
          (curcol (current-column))
          (pt1)
          (pt2)
          (beg))
      (save-excursion
        (setq pt1 (grade-goto-marked-student)))
      (if (not pt1)
          (message "No student has been marked for motion")
        (if (<= currow 2)
            (message "The student cannot be moved here")
          (beginning-of-line)
          (if (> (point) pt1)
              (forward-line -2))
          (setq pt2 (point))
          (goto-char pt1)
          (beginning-of-line)
          (setq beg (point))
          (forward-line 2)
          (kill-region beg (point))
          (goto-char pt2)
          (yank)
          (forward-char 1)
          (grade-next-line -2)
          (beginning-of-line)
          (delete-char 1)
          (insert "|")
          (forward-char (1- curcol)))))))

(defun grade-goto-marked-column ()
  "Go to the column that is marked for motion"
  (let ((pt (point)))
    (grade-goto-table)
    (grade-next-line -1)
    (if (search-forward "*" (line-end-position) t)
        (progn
          (forward-char -1)
          (point))
      (goto-char pt)
      nil)))

(defun grade-mark-column ()
  "Mark the current column for motion"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (mcol 0)
          (col (grade-column-number)))
      (if (or (<= col 2) (and grade-use-studentid (= col 3)))
          (message "This column cannot be moved")
        (save-excursion
          (when (grade-goto-marked-column)
            (setq mcol (grade-column-number))
            (delete-char grade-score-column-width)
            (insert (make-string grade-score-column-width ?-))))
        (unless (= col mcol)
          (save-excursion
            (grade-goto-first-row)
            (grade-goto-cell-beginning)
            (grade-next-line -1)
            (delete-char grade-score-column-width)
            (insert (make-string grade-score-column-width ?*))))))))

(defun grade-move-column (arg)
  "Move the marked column to the current column.
With an argument, move it after the current column"
  (interactive "P")
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (col (grade-column-number))
          (oldcol nil)
          (pt1)
          (pt2)
          (beg))
      (save-excursion
        (setq pt1 (grade-goto-marked-column)))
      (if (not pt1)
          (message "No column has been marked for motion")
        (if arg (setq col (1+ col)))
        (if (or (< col 3) (and grade-use-studentid (= col 3)))
            (message "The column cannot be moved here")
          (grade-goto-first-row)
          (grade-goto-cell-beginning)
          (grade-next-line -1)
          (setq pt2 (point))
          (if (< pt1 pt2)
              (setq col (1- col)))
          (goto-char pt1)
          (grade-next-line 1)
          (grade-goto-last-row)
          (grade-goto-cell-beginning)
          (grade-next-line 1)
          (while (>= (point) pt1)
            (setq oldcol
                  (cons (buffer-substring (point)
                                        (+ (point) grade-score-column-width 1))
                        oldcol))
            (delete-region (point) (+ (point) grade-score-column-width 1))
            (grade-next-line -1))
          (grade-next-line 1)
          (grade-goto-column-nohide col)
          (grade-goto-cell-beginning)
          (while oldcol
            (insert (car oldcol))
            (setq oldcol (cdr oldcol))
            (forward-char (- (1+ grade-score-column-width)))
            (if oldcol
                (grade-next-line 1)))
          (grade-next-line -1)
          (grade-goto-first-row)
          (grade-goto-cell-beginning)
          (grade-next-line -1)
          (delete-char grade-score-column-width)
          (insert (make-string grade-score-column-width ?-))
          (grade-next-line 1)
          (forward-char -1))))))

(defun grade-move-column-to-end ()
  "Move the current column to the end."
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (col (grade-column-number))
          (oldcol nil)
          (pt1)
          (pt2)
          (beg))
      (save-excursion
        (grade-goto-first-row)
        (grade-goto-cell-beginning)
        (grade-next-line -1)
        (setq pt1 (point)))
      (grade-goto-last-row)
      (grade-goto-cell-beginning)
      (grade-next-line 1)
      (while (>= (point) pt1)
        (setq oldcol
              (cons (buffer-substring (point)
                                      (+ (point) grade-score-column-width 1))
                    oldcol))
        (delete-region (point) (+ (point) grade-score-column-width 1))
        (grade-next-line -1))
      (grade-next-line 1)
      (grade-goto-column-nohide (1- grade-total-columns))
      (forward-char 2)
      (while oldcol
        (insert (car oldcol))
        (setq oldcol (cdr oldcol))
        (forward-char (- (1+ grade-score-column-width)))
        (if oldcol
            (grade-next-line 1)))
      (grade-next-line -1)
      (grade-goto-first-row)
      (grade-goto-cell-beginning)
      (forward-char (1- grade-score-column-width)))))

;;; Ordering

(defun grade-switch-rows ()
  "Switch the current row and the next row."
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (currow (grade-row-number))
          (curcol (current-column))
          (pt1)
          (pt2)
          (beg))
      (save-excursion
        (grade-forward-row 1)
        (setq pt1 (point)))
      (if (<= currow 2)
          (message "The student cannot be moved here")
        (beginning-of-line)
        (if (> (point) pt1)
            (forward-line -2))
        (setq pt2 (point))
        (goto-char pt1)
        (beginning-of-line)
        (setq beg (point))
        (grade-next-line 2)
        (kill-region beg (point))
        (goto-char pt2)
        (yank)
        (forward-char 1)
        (grade-next-line -2)
        (beginning-of-line)
        (forward-char curcol)))))

(defun grade-alphabetize (arg)
  "Put the students in alphabetical order"
  (interactive "P")
  (let ((pt (point))
        (fname)
        (lname)
        (nfname)
        (nlname)
        (switch t)
        (ord)
        (i)
        (n (- grade-total-rows grade-invisible-students 3)))
    (if arg
        (defun ord (f1 l1 f2 l2)
          (or (string< l1 l2)
              (and (string= l1 l2)
                   (string< f1 f2))))
      (defun ord (f1 l1 f2 l2)
        (or (string> l1 l2)
            (and (string= l1 l2)
                 (string> f1 f2)))))
    (while (and switch (> n 1))
      (setq switch nil)
      (grade-goto-row 3)
      (setq i 1)
      (while (< i n)
        (save-excursion
          (setq fname (grade-student-first-name))
          (setq lname (grade-student-last-name))
          (grade-forward-row 1)
          (setq nfname (grade-student-first-name))
          (setq nlname (grade-student-last-name)))
        (when (ord fname lname nfname nlname)
          (grade-switch-rows)
          (setq switch t))
        (grade-forward-row 1)
        (setq i (1+ i)))
      (setq n (- n 1)))
    (goto-char pt)))

(defun grade-order-numerically (arg)
  "Put the students in order according to column's values."
  (interactive "P")
  (let ((pt (point))
        (val)
        (nval)
        (switch t)
        (ord)
        (i)
        (n (- grade-total-rows grade-invisible-students 3)))
    (if arg
        (defun ord (v1 v2)
          (let ((v (string-to-int v1))
                (vv (string-to-int v2)))
            (> v vv)))
      (defun ord (v1 v2)
        (let ((v (string-to-int v1))
              (vv (string-to-int v2)))
          (< v vv))))
    (while (and switch (> n 1))
      (setq switch nil)
      (grade-goto-row 3)
      (setq i 1)
      (while (< i n)
        (save-excursion
          (setq val (grade-score-cell-contents))
          (grade-forward-row 1)
          (setq nval (grade-score-cell-contents)))
        (when (ord val nval)
          (grade-switch-rows)
          (setq switch t))
        (grade-forward-row 1)
        (setq i (1+ i)))
      (setq n (- n 1)))
    (goto-char pt)))

(defun grade-order-alphabetically (arg)
  "Put the students in order according to column's values."
  (interactive "P")
  (let ((pt (point))
        (val)
        (nval)
        (switch t)
        (ord)
        (i)
        (n (- grade-total-rows grade-invisible-students 3)))
    (if arg
        (defun ord (v1 v2)
          (string< v1 v2))
      (defun ord (v1 v2)
        (string> v1 v2)))
    (while (and switch (> n 1))
      (setq switch nil)
      (grade-goto-row 3)
      (setq i 1)
      (while (< i n)
        (save-excursion
          (setq val (grade-score-cell-contents))
          (grade-forward-row 1)
          (setq nval (grade-score-cell-contents)))
        (when (ord val nval)
          (grade-switch-rows)
          (setq switch t))
        (grade-forward-row 1)
        (setq i (1+ i)))
      (setq n (- n 1)))
    (goto-char pt)))

(defun grade-order-column (arg)
  "Put the students in order, according to the column's values"
  (interactive "P")
  (let ((col (grade-column-number)))
    (if (or (= col 1) (= col 2))
        (grade-alphabetize arg)
      (if (string= (grade-column-type) "other")
          (grade-order-alphabetically arg)
        (grade-order-numerically arg)))))

(defun grade-order-column-backwards ()
  (interactive)
  (grade-order-column 1))

;;; Hiding

(defun grade-hide-region (from to)
  "Hide the region from FROM to TO."
  (overlay-put (make-overlay from to) 'invisible 'grade))

(defun grade-mark-student-for-hiding ()
  "Mark the current student for hiding"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (currow (grade-row-number)))
      (if (or (>= currow grade-total-rows) (<= currow 2))
          (message "This is not a student row")
        (if (save-excursion
              (beginning-of-line)
              (looking-at "\\."))
            (progn
              (save-excursion
                (beginning-of-line)
                (delete-char 1)
                (insert "|"))
              (if (= (point) (line-beginning-position))
                  (forward-char 1)))
          (save-excursion
            (beginning-of-line)
            (delete-char 1)
            (insert "."))
          (if (= (point) (line-beginning-position))
              (forward-char 1)))))))

(defun grade-hide-student ()
  "Remove the student on the current line"
  (interactive)
  (let ((rownum (grade-row-number))
        (buffer-read-only nil)
        (pt)
        (ept))
    (when (or (not (interactive-p)) (grade-in-table-p))
      (if (or (< rownum 3) (= rownum grade-total-rows))
          (message "This row cannot be hidden")
        (save-excursion
          (beginning-of-line)
          (when (looking-at "\\*")
              (delete-char 1)
              (insert "|")
              (forward-char -1))
          (setq pt (point))
          (grade-next-line 2)
          (setq ept (point)))
        (grade-forward-row)
        (grade-hide-region pt ept)
        (setq grade-invisible-students (1+ grade-invisible-students))))))

(defun grade-hide-marked-students ()
  "Hide the students who are marked for hiding"
  (interactive)
  (while (save-excursion
           (beginning-of-line)
           (looking-at "\\."))
    (grade-forward-row))
  (save-excursion
    (beginning-of-buffer)
    (while
        (re-search-forward "^\\." (point-max) t)
      (grade-hide-student))))

(defun grade-mark-column-for-hiding ()
  "Mark the current column for hiding"
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (let ((buffer-read-only nil)
          (mcol 0)
          (col (grade-column-number)))
      (if (or (<= col 2) (and grade-use-studentid (= col 3)))
          (message "This column cannot be hidden")
        (save-excursion
          (grade-goto-first-row)
          (grade-goto-cell-beginning)
          (grade-next-line -1)
          (if (looking-at "\\.")
              (progn
                (delete-char grade-score-column-width)
                (insert (make-string grade-score-column-width ?-)))
            (progn
              (delete-char grade-score-column-width)
              (insert (make-string grade-score-column-width ?.)))))))))

(defun grade-hide-column ()
  "Hide the current column"
  (interactive)
  (let ((r)
        (pt)
        (c)
        (beg)
        (buffer-read-only nil))
    (setq c (grade-column-number-visible))
    (if (or (< c 3) (and grade-use-studentid (= c 3)))
        (message "This column cannot be hidden")
      (save-excursion
        (grade-goto-first-row)
        (grade-goto-cell-beginning)
        (grade-next-line -1)
        (setq beg (point))
        (if (looking-at "\\*")
            (progn
              (delete-char grade-score-column-width)
              (insert (make-string grade-score-column-width ?-)))))
      (setq r (grade-row-number))
      (setq pt (point))
      (grade-goto-last-row)
      (grade-goto-cell-beginning)
      (grade-next-line 1)
      (while (>= (point) beg)
        (grade-hide-region (point) (+ (point) grade-score-column-width 1))
        (grade-next-line -1))
      (grade-goto-table)
      (grade-goto-row-nohide r)
      (if (= c (- grade-total-columns grade-invisible-columns))
          (grade-goto-column (1- c))
        (grade-goto-column c))
      (setq grade-invisible-columns (1+ grade-invisible-columns)))))

(defun grade-hide-marked-columns ()
  "Hide the columns which are marked for hiding"
  (interactive)
  (let ((le)
        (pt (point))
        (ok t))
    (while (and ok
                (save-excursion
                  (grade-goto-first-row)
                  (grade-next-line -1)
                  (forward-char -1)
                  (looking-at "\\.")))
      (setq ok (grade-next-column)))
    (unless ok
      (goto-char pt)
      (while (save-excursion
               (grade-goto-first-row)
               (grade-next-line -1)
               (forward-char -1)
               (looking-at "\\."))
        (grade-previous-column)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^ -")
      (save-excursion
        (end-of-line)
        (setq le (point)))
      (while
          (re-search-forward " \\." le t)
        (save-excursion
          (grade-hide-column))))))

(defun grade-hide-marked-students-and-columns ()
  "Hide all marked information"
  (interactive)
  (grade-hide-marked-students)
  (grade-hide-marked-columns))

(defun grade-show-all ()
  "Show all hidden information."
  (interactive)
  (let ((ol (overlays-in (point-min) (point-max))))
    (while ol
      (delete-overlay (car ol))
      (setq ol (cdr ol)))
    (setq grade-invisible-students 0)
    (setq grade-invisible-columns 0)))

(defun grade-show-some ()
  "Show the non-marked hidden information"
  (interactive)
  (grade-show-all)
  (grade-hide-marked-students-and-columns))

;;; Computation

(defun grade-average-column ()
  "Compute the average of the contents of a column,
insert into the bottom cell"
  (let ((rownum 3)
        (i 0)
        (cl)
        (total 0))
    (grade-goto-row 3)
    (setq cl (grade-score-cell-contents))
    (while (<= rownum (- grade-total-rows grade-invisible-students 1))
      (when (not (string= cl ""))
        (setq total (+ total (string-to-int cl)))
        (setq i (1+ i)))
      (grade-forward-row)
      (setq rownum (1+ rownum))
      (setq cl (grade-score-cell-contents)))
    (setq total (* 1.0 total))
    (if (= i 0)
        (setq total (make-string grade-score-column-width ?*))
      (setq total (/ total i))
      (setq total (number-to-string total)))
    (grade-insert-score total)
    (grade-goto-first-row)))

(defun grade-eval-cell ()
  "Evaluate the column's formula for the given student."
  (let ((formula (grade-column-formula))
        (gresult)
        (gvalues (grade-student-scores)))
    (setq formula (grade-insert-values-into-formula gvalues formula))
    (setq gresult (calc-eval formula))
    (grade-insert-score gresult)))

(defun grade-eval-column ()
  "Evaluate the entire column."
  (let ((formula (grade-column-formula))
        (gresult)
        (gvalues)
        (i 3))
    (grade-goto-row 3)
    (while (< i (- grade-total-rows grade-invisible-students))
      (setq gvalues (grade-student-scores))
      (setq gresult (grade-insert-values-into-formula gvalues formula))
      (setq gresult (calc-eval gresult))
      (grade-insert-score gresult)
      (grade-forward-row)
      (setq i (1+ i)))
    (grade-average-column)))

(defun grade-update-column ()
  (interactive)
  (when (or (not (interactive-p)) (grade-in-table-p))
    (save-excursion
      (cond
       ((string= (grade-column-type) "formula")
        (grade-eval-column))
       ((string= (grade-column-type) "number")
        (grade-average-column))
       (t nil)))))

(defun grade-adjust-row ()
  "Update all the entries to the row"
  (let ((col 3))
    (save-excursion
      (grade-goto-column 2)
      (while (<= col grade-total-columns)
        (grade-forward-column 1)
        (if (string= (grade-column-type) "formula")
            (grade-eval-column))
;          (grade-average-column))
        (setq col (1+ col))))))

(defun grade-adjust-for-new-formula ()
  "Update the entries of the grade table"
  (let ((col 2))
    (save-excursion
      (grade-goto-first-row)
      (grade-goto-column 2)
      (while (<= col grade-total-columns)
        (grade-forward-column 1)
        (when (string= (grade-column-type) "formula")
          (grade-eval-column))
        (setq col (1+ col))))))

(defun grade-update-all ()
  "Go through the columns, updating each one"
  (interactive)
  (save-excursion
    (grade-goto-table)
    (when (> grade-total-columns 2)
      (grade-goto-column 2)
      (let ((i 3))
        (while (<= i grade-total-columns)
          (grade-forward-column 1)
          (grade-update-column)
          (setq i (1+ i)))))))

;;; Printing

(defun grade-tex-insert-studentinfo (info beg end gscw)
  (let ((it))
    (if grade-put-avg-in-reports
        (insert "\n\\begin{tabular}{lll}\n")
      (insert "\n\\begin{tabular}{ll}\n"))
    (while (<= beg end)
      (setq it (nth (- beg 1) info))
      (insert "\\textbf{" (nth 0 it) "}")
      (if (string= (nth 1 it) (make-string gscw ?=))
          (insert "$\\dagger$"))
      (insert " & " (nth 2 it))
      (if (> (string-to-number (nth 1 it)) 0)
          (insert "/" (nth 1 it)))
      (if (and grade-put-avg-in-reports (not (string= (nth 3 it) "")))
          (insert " & (Class average: " (nth 3 it) ")" ))
      (insert "\\\\\n")
      (setq beg (1+ beg)))
    (insert "\\end{tabular}\n\n")))

(defun grade-tex-reports (num)
  "Create a TeX file which has one report per student"
  (interactive "P")
  (if num
      (if (listp num)
          (setq num 2))
    (setq num 1))
  (let ((output-buffer)
        (studentinfo)
        (studentname)
        (sid nil)
        (pageno)
        (tpage)
        (tgrades)
        (gbeg)
        (auxdir)
        (gscw grade-score-column-width)
        (totgrades (- grade-total-columns grade-invisible-columns 2))
        (pages-per-report)
        (gtexform (grade-tex-formulas))
        (numstudents (- grade-total-rows grade-invisible-students 3))
        (tst)
        (classname (grade-class-name))
        (classsec (grade-section-name))
        (classsem (grade-semester-name))
        (instr (grade-instructor-name)))
    (if grade-use-studentid
        (setq totgrades (1- totgrades)))
    (setq tst numstudents)
    (setq auxdir (concat (file-name-directory (buffer-file-name))
                         grade-aux-dir "/"))
    (unless (file-directory-p auxdir)
      (make-directory auxdir))
    (setq output-buffer
          (find-file-noselect
           (concat auxdir
                   (file-name-nondirectory (buffer-file-name))
                   "-reports.tex")))
    (setq pages-per-report (cond
                          ((<= totgrades grade-reports-grades-per-page)
                           1)
                          ((and
                            (= (mod totgrades grade-reports-grades-per-page) 0)
                            (not (= totgrades 0)))
                           (/ totgrades grade-reports-grades-per-page))
                          (t (1+ (/ totgrades grade-reports-grades-per-page)))))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (insert "\\documentclass")
      (if (and grade-latex-options (not (string= grade-latex-options "")))
          (insert "[" grade-latex-options "]"))
      (insert "{article}\n" )
      (insert "\\pagestyle{empty}\n")
      (insert "\\begin{document}\n"))
;      (insert "\\noindent\n"))
    (save-excursion
      (grade-goto-row 3)
      (while (> numstudents 0)
        (setq studentinfo (grade-student-info))
        (setq studentname (grade-student-name))
        (if grade-use-studentid (setq sid (grade-studentid)))
        (save-excursion
          (set-buffer output-buffer)
          (setq pageno 1)
          (while (<= pageno pages-per-report)
            (if (< pageno pages-per-report)
                (setq tgrades grade-reports-grades-per-page)
              (setq tgrades (mod totgrades grade-reports-grades-per-page)))
            (insert "\\textbf{Class:} " classname "\n")
            (if classsec (insert "\n\\textbf{Section:} " classsec "\n"))
            (if classsem (insert "\n\\textbf{Semester:} " classsem "\n"))
            (if instr (insert "\n\\textbf{Instructor:} " instr "\n"))
            (insert "\n\\begin{center}\n")
            (insert "\n\\large\\textbf{Report for " studentname "}\n\n")
            (if grade-use-studentid
                (insert "\\large\\textbf{Student ID:} " sid "\n\n"))
            (if (> pages-per-report 1)
                (insert "\\small Page " (number-to-string pageno)
                        " of "
                        (number-to-string pages-per-report)
                        "\n"))
            (insert "\\end{center}\n")
            (setq gbeg  (1+ (* (- pageno 1) grade-reports-grades-per-page)))
            (unless (= totgrades 0)
              (grade-tex-insert-studentinfo
                               studentinfo gbeg (1- (+ gbeg tgrades)) gscw))
            (when gtexform
              (insert gtexform))
            (insert "\n\\vfill\n\n")
            (if (= (% (1+ (- tst numstudents)) num) 0)
                (insert "\\newpage\n\n"))
            (setq pageno (1+ pageno))))
        (grade-forward-row)
        (setq numstudents (- numstudents 1))))
    (save-excursion
      (set-buffer output-buffer)
      (insert "\\end{document}\n")
      (goto-char 1)
      (save-buffer))
    (switch-to-buffer output-buffer)))

(defun grade-tex-file ()
  "Create a TeX file that can be printed"
  (interactive)
  (let ((output-buffer)
        (row)
        (col)
        (i)
        (r)
        (currow)
        (gtexform (grade-tex-formulas))
        (totalcol (- grade-total-columns grade-invisible-columns 2))
        (totalrow (- grade-total-rows grade-invisible-students 3))
        (texrows grade-tex-students-per-page)
        (texcols grade-tex-columns-per-page)
        (tcol)
        (trow)
        (frow)
        (srow)
        (lrow)
        (coltotalpages)
        (colpage)
        (rowpage)
        (rowtotalpages)
        (auxdir)
        (endw)
        (classname (grade-class-name))
        (classsec (grade-section-name))
        (classsem (grade-semester-name))
        (instr (grade-instructor-name)))
    (if grade-use-studentid (setq totalcol (1- totalcol)))
    (setq auxdir (concat (file-name-directory (buffer-file-name))
                         grade-aux-dir "/"))
    (unless (file-directory-p auxdir)
      (make-directory auxdir))
    (setq output-buffer
          (find-file-noselect
           (concat auxdir
                   (file-name-nondirectory (buffer-file-name))
                   ".tex")))
    (setq colpage 1)
    (setq coltotalpages (cond
                         ((<= totalcol texcols) 1)
                         ((and (= (mod totalcol texcols) 0)
                               (not (= totalcol 0)))
                          (/ totalcol texcols))
                         (t (1+ (/ totalcol texcols)))))
    (setq rowpage 1)
    (setq rowtotalpages (cond
                         ((<= totalrow texrows) 1)
                         ((= (mod totalrow texrows) 0)
                          (/ totalrow texrows))
                         (t (1+ (/ totalrow texrows)))))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (insert "\\documentclass")
      (if (and grade-latex-options (not (string= grade-latex-options "")))
          (insert "[" grade-latex-options "]"))
      (insert "{article}\n" )
      (insert "\\pagestyle{empty}\n")
      (insert "\\setlength{\\oddsidemargin}{-.25in}\n")
      (insert "\\setlength{\\evensidemargin}{-.25in}\n")
      (insert "\\setlength{\\topmargin}{-.5in}\n")
      (insert "\\setlength{\\textheight}{9.25in}\n")
      (insert "\\setlength{\\textwidth}{7in}\n")
      (insert "\\begin{document}\n"))
    (save-excursion
      (grade-goto-table)
      (setq frow (grade-row-info))
      (grade-forward-row 1)
      (setq srow (grade-row-info))
      (grade-goto-last-row)
      (setq lrow (grade-row-info))
      (grade-goto-row 3)
      (while (<= rowpage rowtotalpages)
        (setq colpage 1)
        (if (< rowpage rowtotalpages)
            (setq trow texrows)
          (setq trow (mod totalrow texrows))
          (if (and (= trow 0) (not (= totalrow 0)))
              (setq trow texrows)))
        (setq currow (point)) ;(grade-row-number))
        (while (<= colpage coltotalpages)
          (goto-char currow) ;(grade-goto-row currow)
          (if (< colpage coltotalpages)
              (setq tcol texcols)
            (setq tcol (mod totalcol texcols))
            (if (and (= tcol 0) (not (= totalcol 0)))
                (setq tcol texcols)))
          (save-excursion
            (set-buffer output-buffer)
            (insert "\\begin{center}\n")
            (insert "\\Large " classname "\n")
            (if classsec
                (insert "\n\\large Section: " classsec "\n"))
            (if classsem
                (insert "\n\\large Semester: " classsem "\n"))
            (if instr
                (insert "\n\\large Instructor: " instr "\n"))
            (when (or (> coltotalpages 1) (> rowtotalpages 1))
              (insert "\n\\small (")
              (when (> rowtotalpages 1)
                (insert "Students "
                        (number-to-string (1+ (* texrows (- rowpage 1))))
                        " to "
                        (number-to-string (+ (* texrows (- rowpage 1)) trow))
                        " out of "
                        (number-to-string totalrow)))
              (when (> coltotalpages 1)
                (if (> rowtotalpages 1)
                    (insert ", "))
                (insert "Grades "
                        (number-to-string (1+ (* texcols (- colpage 1))))
                        " to "
                        (number-to-string (+ (* texcols (- colpage 1)) tcol))
                        " out of "
                        (number-to-string totalcol)))
              (insert ")\n"))
            (insert "\\end{center}\n\n")
;            (insert "\\vspace{\\stretch{1}}\n")
            (insert "\\begin{center}\n")
            (insert "\\begin{tabular}{|l|l|")
            (if grade-use-studentid
                (insert "c|"))
            (setq i 0)
            (while (< i tcol)
              (insert "r|")
              (setq i (+ i 1)))
            (insert "}\n\\hline\n")
            (insert "\\textbf{" (nth 0 frow) "} ")
            (insert "& \\textbf{" (nth 1 frow) "} ")
            (if grade-use-studentid
                (progn
                  (insert "& \\textbf{" (nth 2 frow) "} ")
                  (setq col (+ 4 (* texcols (- colpage 1))))
                  (setq endw (+ 4 (* texcols (- colpage 1)) tcol)))
              (setq col (+ 3 (* texcols (- colpage 1))))
              (setq endw (+ 3 (* texcols (- colpage 1)) tcol)))
            (while (< col endw)
              (insert "& \\textbf{" (nth (1- col) frow) "} ")
              (setq col (1+ col)))
            (insert "\\\\\n\\hline\n")
            (insert "\\textbf{" (nth 0 srow) "} & ")
            (if grade-use-studentid
                (progn
                  (insert "& ")
                  (setq col (+ 4 (* texcols (- colpage 1)))))
              (setq col (+ 3 (* texcols (- colpage 1)))))
            (while (< col endw)
              (if (string= (nth (1- col) srow)
                           (make-string grade-score-column-width ?=))
                  (insert "& \\textsl{$\\dagger$} ")
                (insert "& \\textsl{" (nth (1- col) srow) "} "))
              (setq col (1+ col)))
            (insert "\\\\\n\\hline\n"))
          (setq r 1)
          (while (<= r trow)
            (setq row (grade-row-info))
            (save-excursion
              (set-buffer output-buffer)
              (insert (nth 0 row))
              (insert " & " (nth 1 row))
              (if grade-use-studentid
                  (progn
                    (insert " & " (nth 2 row))
                    (setq col (+ 4 (* texcols (- colpage 1))))
                    (setq endw (+ 4 (* texcols (- colpage 1)) tcol)))
                (setq col (+ 3 (* texcols (- colpage 1))))
                (setq endw (+ 3 (* texcols (- colpage 1)) tcol)))
              (while (< col endw)
                (insert " & "(nth (1- col) row))
                (setq col (1+ col)))
              (insert "\\\\\n\\hline\n"))
            (grade-forward-row 1)
            (setq r (1+ r)))
          (save-excursion
            (set-buffer output-buffer)
            (insert "\\textsl{" (nth 0 lrow) "} & ")
            (if grade-use-studentid
                (progn
                  (insert "& ")
                  (setq col (+ 4 (* texcols (- colpage 1))))
                  (setq endw (+ 4 (* texcols (- colpage 1)) tcol)))
              (setq col (+ 3 (* texcols (- colpage 1))))
              (setq endw (+ 3 (* texcols (- colpage 1)) tcol)))
            (while (< col endw)
              (insert "& \\textsl{" (nth (- col 1) lrow) "} ")
              (setq col (1+ col)))
            (insert "\\\\\n\\hline\n\\end{tabular}\n")
            (insert "\\end{center}\n")
            (when gtexform
              (insert gtexform))
 ;           (insert "\\vspace{\\stretch{5}}\n")
            (if (< colpage coltotalpages)
                (insert "\\newpage\n\n")))
          (setq colpage (1+ colpage)))
        (if (< rowpage rowtotalpages)
            (save-excursion
              (set-buffer output-buffer)
              (insert "\\newpage\n\n")))
      (setq rowpage (1+ rowpage))))
      (save-excursion
        (set-buffer output-buffer)
        (insert "\n\\end{document}\n")
        (goto-char 1))
    (switch-to-buffer output-buffer)
    (save-buffer)))

(defun grade-student-list ()
  "Create a list of the students"
  (interactive)
  (let ((output-buffer)
        (i)
        (r)
        (student)
        (auxdir)
        (sid)
        (classname (grade-class-name))
        (classsec (grade-section-name))
        (classsem (grade-semester-name))
        (instr (grade-instructor-name)))
    (setq auxdir (concat (file-name-directory (buffer-file-name))
                         grade-aux-dir "/"))
    (unless (file-directory-p auxdir)
      (make-directory auxdir))
    (setq output-buffer
          (find-file-noselect
           (concat auxdir
                   (file-name-nondirectory (buffer-file-name))
                   "-students.tex")))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer)
      (insert "\\documentclass")
      (if (and grade-latex-options (not (string= grade-latex-options "")))
          (insert "[" grade-latex-options "]"))
      (insert "{article}\n" )
      (insert "\\pagestyle{empty}\n")
      (insert "\\begin{document}\n")
      (insert "\\noindent\n")
      (insert "\\begin{center}\n")
      (insert "\\Large " classname "\n")
      (if classsec
          (insert "\n\\noindent\n\\large Section: " classsec "\n"))
      (if classsem
          (insert "\n\\noindent\n\\large Semester: " classsem "\n"))
      (if instr
          (insert "\n\\noindent\n\\large Instructor: " instr "\n"))
      (insert "\n \\large Students\n")
      (insert "\\end{center}\n\n"))
    (save-excursion
      (grade-goto-table)
      (save-excursion
        (set-buffer output-buffer)
        (insert "\\begin{enumerate}\n"))
      (grade-goto-row 3)
      (setq r 3)
      (while (< r (- grade-total-rows grade-invisible-students))
        (setq student (grade-student-name))
        (setq sid (grade-studentid))
        (save-excursion
          (set-buffer output-buffer)
          (insert "\\item " student)
          (if sid (insert " (" sid ")"))
          (insert "\n"))
        (grade-forward-row 1)
        (setq r (1+ r)))
      (save-excursion
        (set-buffer output-buffer)
        (insert "\\end{enumerate}\n")
        (insert "\n\\end{document}\n")
        (goto-char 1)))
    (switch-to-buffer output-buffer)
    (save-buffer)))

;;; Importing

(defun grade-new-empty-column-at-end ()
  "Insert a new column at the end."
  (save-excursion
    (grade-goto-table)
    (grade-goto-last-column)
    (let ((buffer-read-only nil))
      (grade-insert-column))))

(defun grade-import-data ()
  "Insert the contents of a comma delimited file"
  (interactive)
  (let* ((file (read-file-name "File to import: " default-directory "" t))
         (imp (find-file-noselect file))
        (end)
        (line)
        (len)
        (ii)
        (pt))
    (save-excursion
      (set-buffer imp)
      (goto-char (point-max))
      (beginning-of-line)
      (while (and (looking-at "^[ \t]*$") (> (point) (point-min)))
        (forward-line -1))
      (setq end (point))
      (goto-char (point-min))
      (while (and (looking-at "^[ \t]*$") (< (point) end))
        (forward-line 1))
      (setq pt (point))
      (setq line (buffer-substring pt (line-end-position)))
      (setq line (split-string line ","))
      (setq len (length line)))
    (grade-goto-table)
    (if (< grade-total-columns len)
        (let ((i grade-total-columns))
          (while (< i len)
            (grade-new-empty-column-at-end)
            (setq i (1+ i)))))
    (grade-goto-table)
    (grade-goto-last-row)
    (cond
     ((and grade-first-name-first grade-use-studentid)
      (grade-new-student (nth 0 line) (nth 1 line) (nth 2 line))
      (setq line (cdr (cdr (cdr line)))))
     (grade-use-studentid
      (grade-new-student (nth 1 line) (nth 0 line) (nth 2 line))
      (setq line (cdr (cdr (cdr line)))))
     (grade-first-name-first
      (grade-new-student (nth 0 line) (nth 1 line) nil)
      (setq line (cdr (cdr line))))
     (t
      (grade-new-student (nth 1 line) (nth 0 line) nil)
      (setq line (cdr (cdr line)))))
    (grade-goto-column (if grade-use-studentid 3 2))
    (while line
      (grade-forward-column)
      (grade-insert-score (car line))
      (setq line (cdr line)))
    (while (< pt end)
      (save-excursion
        (set-buffer imp)
        (goto-char pt)
        (forward-line 1)
        (setq pt (point))
        (setq line (buffer-substring pt (line-end-position)))
        (setq line (split-string line ",")))
      (grade-goto-table)
      (grade-goto-last-row)
      (cond
       ((and grade-first-name-first grade-use-studentid)
        (grade-new-student (nth 0 line) (nth 1 line) (nth 2 line))
        (setq line (cdr (cdr (cdr line)))))
       (grade-use-studentid
        (grade-new-student (nth 1 line) (nth 0 line) (nth 2 line))
        (setq line (cdr (cdr (cdr line)))))
       (grade-first-name-first
        (grade-new-student (nth 0 line) (nth 1 line) nil)
        (setq line (cdr (cdr line))))
       (t
        (grade-new-student (nth 1 line) (nth 0 line) nil)
        (setq line (cdr (cdr line)))))
      (grade-goto-column (if grade-use-studentid 3 2))
      (setq ii (if grade-use-studentid 3 2))
      (while (and line (< ii len))
        (grade-forward-column)
        (grade-insert-score (car line))
        (setq line (cdr line))
        (setq ii (1+ ii))))
    (grade-goto-table)))

;;; Exporting

(defun grade-export-data ()
  "Export the grade information as csv"
  (interactive)
  (let ((i 1)
        (auxdir)
        (output-buffer)
        (row))
    (setq auxdir (concat (file-name-directory (buffer-file-name))
                         grade-aux-dir "/"))
    (unless (file-directory-p auxdir)
      (make-directory auxdir))
    (setq output-buffer
          (find-file-noselect
           (concat auxdir
                   (file-name-nondirectory (buffer-file-name))
                   ".csv")))
    (save-excursion
      (set-buffer output-buffer)
      (erase-buffer))
    (save-excursion
      (grade-goto-table)
      (while (<= i grade-total-rows)
        (setq row (grade-row-info))
        (save-excursion
          (set-buffer output-buffer)
          (goto-char (point-max))
          (while row
            (insert (car row))
            (insert ",")
            (setq row (cdr row)))
          (forward-char -1)
          (delete-char 1)
          (insert "\n"))
        (setq i (1+ i))
        (if (< i grade-total-rows)
            (grade-forward-row))))
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (save-buffer))
    (switch-to-buffer output-buffer)))

;;; Quitting

(defun grade-quit (&optional arg)
  "Bury the buffer, or, with an argument, kill the buffer."
  (interactive)
  (if arg
      (kill-buffer (current-buffer))
    (if (and (buffer-modified-p)
             (y-or-n-p "Save buffer before exiting Grades? "))
        (save-buffer))
    (bury-buffer)))

;;; The mode

(defvar grade-mode-map nil
  "Keymap for grade mode.")

(if grade-mode-map
    ()
  (setq grade-mode-map (make-sparse-keymap))
  ;; Motion
  (define-key grade-mode-map "\C-f" 'grade-interactive-forward-column)
  (define-key grade-mode-map [(tab)] 'grade-interactive-forward-column)
  (define-key grade-mode-map [right] 'grade-interactive-forward-column)
  (define-key grade-mode-map "\C-b" 'grade-interactive-backward-column)
  (define-key grade-mode-map [left] 'grade-interactive-backward-column)
  (define-key grade-mode-map "\C-n" 'grade-interactive-forward-row)
  (define-key grade-mode-map (kbd "<return>") 'grade-interactive-forward-row)
  (define-key grade-mode-map [down] 'grade-interactive-forward-row)
  (define-key grade-mode-map "\C-p" 'grade-interactive-backward-row)
  (define-key grade-mode-map [up] 'grade-interactive-backward-row)
  (define-key grade-mode-map "\M-p" 'grade-goto-first-row)
  (define-key grade-mode-map "\M-n" 'grade-goto-last-row)
  (define-key grade-mode-map "\M-b" 'grade-goto-first-column)
  (define-key grade-mode-map "\C-a" 'grade-goto-first-column)
  (define-key grade-mode-map "\M-f" 'grade-goto-last-column)
  (define-key grade-mode-map "\C-e" 'grade-goto-last-column)
  (define-key grade-mode-map "\C-c\C-c" 'grade-goto-table)
  (define-key grade-mode-map "\M-<" 'grade-goto-table)
  (define-key grade-mode-map "\M->" 'grade-goto-end-of-table)
    ;; Editing
  (define-key grade-mode-map "=" 'grade-edit-or-eval-cell)
  (define-key grade-mode-map "\C-c\C-ec" 'grade-edit-class-name)
  (define-key grade-mode-map "\C-c\C-es" 'grade-edit-section-name)
  (define-key grade-mode-map "\C-c\C-em" 'grade-edit-semester-name)
  (define-key grade-mode-map "\C-c\C-ei" 'grade-edit-instructor-name)
  (define-key grade-mode-map "\C-c=" 'grade-update-column)
  (define-key grade-mode-map "\C-c#" 'grade-update-all)
  ;; Insertion
  (define-key grade-mode-map "\C-c\C-n" 'grade-single-new-student)
  (define-key grade-mode-map "\C-c\C-s" 'grade-new-column)
  (define-key grade-mode-map "\C-c\C-o" 'grade-new-column-at-end)
  (define-key grade-mode-map "\C-c\C-a" 'grade-add-students)
  (define-key grade-mode-map "\C-c\C-g" 'grade-insert-student-scores)
  (define-key grade-mode-map "\C-c\C-i" 'grade-insert-scores)
  ;; Deletion
  (define-key grade-mode-map "\C-k" 'grade-put-student-out-of-misery)
  (define-key grade-mode-map "\C-c\C-k" 'grade-remove-column)
  ;; Displaying information
  (define-key grade-mode-map "s" 'grade-display-student-name)
  (define-key grade-mode-map "c" 'grade-display-column-name)
  ;; Moving information
  (define-key grade-mode-map "*" 'grade-mark-student)
  (define-key grade-mode-map "@" 'grade-move-student)
  (define-key grade-mode-map "\C-c*" 'grade-mark-column)
  (define-key grade-mode-map "\C-c@" 'grade-move-column)
  (define-key grade-mode-map "\C-c]" 'grade-move-column-to-end)
  ;; Ordering
  (define-key grade-mode-map "\C-c\C-a" 'grade-alphabetize)
  (define-key grade-mode-map "\C-c>"  'grade-order-column)
  (define-key grade-mode-map "\C-c<"  'grade-order-column-backwards)
  ;; Hiding
  (define-key grade-mode-map "." 'grade-mark-student-for-hiding)
  (define-key grade-mode-map "\C-c\C-hs" 'grade-hide-marked-students)
  (define-key grade-mode-map "/"  'grade-hide-student)
  (define-key grade-mode-map "\C-c."  'grade-mark-column-for-hiding)
  (define-key grade-mode-map "\C-c\C-hc" 'grade-hide-marked-columns)
  (define-key grade-mode-map "\C-c-" 'grade-hide-marked-students-and-columns)
  (define-key grade-mode-map "\C-c/" 'grade-hide-column)
  (define-key grade-mode-map "\C-c+"  'grade-show-all)
  (define-key grade-mode-map "\C-c\C-v"  'grade-show-some)
  ;; Output
  (define-key grade-mode-map "\C-c\C-x" 'grade-show-reports)
  (define-key grade-mode-map "\C-c\C-t" 'grade-tex-file)
  (define-key grade-mode-map "\C-c\C-r" 'grade-tex-reports)
  (define-key grade-mode-map "\C-c\C-l" 'grade-student-list)
  ;; Quitting
  (define-key grade-mode-map "q" 'grade-quit))

(easy-menu-define grade-mode-menu grade-mode-map "Grade mode menu"
  '("Grade"
    ("Motion"
     ["Forward column" grade-forward-column t]
     ["Backward column" grade-backward-column t]
     ["Forward row" grade-forward-row t]
     ["Backward row" grade-backward-row t]
     ["Go to first row" grade-goto-first-row t]
     ["Go to last row" grade-goto-last-row t]
     ["Go to first column" grade-goto-first-column t]
     ["Go to last column" grade-goto-last-column t]
     ["Go to the table" grade-goto-table t]
     ["Go to the end of the table" grade-goto-end-of-table t])
    ("Editing"
     ["Edit (or eval) cell" grade-edit-or-eval-cell t]
     ["Edit class name" grade-edit-class-name]
     ["Edit section name" grade-edit-section-name t]
     ["Edit semester name" grade-edit-semester-name t]
     ["Edit instructor name" grade-edit-instructor-name t])
    ("Updating"
     ["Update the column" grade-update-column t]
     ["Update all columns" grade-update-all t])
    ("Insertion"
     ["Insert new student" grade-single-new-student t]
     ["Add several students" grade-add-students t]
     ["Insert new score column (at end)" grade-new-column-at-end t]
     ["Insert new score column" grade-new-column t]
     ["Insert scores for student" grade-insert-student-scores t]
     ["Insert scores for column" grade-insert-scores t])
    ("Deletion"
     ["Delete student" grade-put-student-out-of-misery t]
     ["Delete column" grade-remove-column t])
    ("Displaying"
     ["Display current student" grade-display-student-name t]
     ["Display current column name" grade-display-column-name t])
    ("Moving info"
     ["Mark student to be moved" grade-mark-student t]
     ["Move student" grade-move-student t]
     ["Mark column to be moved" grade-mark-column t]
     ["Move column" grade-move-column t]
     ["Move column to end" grade-move-column-to-end t])
    ("Ordering"
     ["Alphabetize students" grade-alphabetize t]
     ["Arrange in decreasing order" grade-order-column t]
     ["Arrange in increasing order" grade-order-column-backwards t])
    ("Hiding"
     ["Hide student" grade-hide-student t]
     ["Hide column" grade-hide-column t]
     ["Mark student for hiding" grade-mark-student-for-hiding t]
     ["Mark column for hiding" grade-mark-column-for-hiding t]
     ["Hide marked information" grade-hide-marked-students-and-columns t]
     ["Hide marked students" grade-hide-marked-students t]
     ["Hide marked columns" grade-hide-marked-students t]
     ["Show all hidden information" grade-show-all t]
     ["Show unmarked hidden information" grade-show-some t])
    ("Reports"
     ["Show student reports" grade-show-reports t]
     ["TeX the reports" grade-tex-reports t])
    ("TeX"
     ["TeX the file" grade-tex-file t]
     ["TeX the reports " grade-tex-reports t]
     ["TeX a student list" grade-student-list])
    ("Misc"
     ["Export as a csv file" grade-export-data t])))

(defun grade-mode ()
  "Major mode for taking care of grades.

KEYBINDING SUMMARY
------------------
Motion
------
Move forward one column:               C-f
 (This can be also be given a          Also rightarrow and TAB.
  numeric argument to move
  more than one column at a time.)
Move backward one column:              C-b
 (This can also be given a             Also leftarrow.
  numeric argument to move
  more than one column at a time.)
Move forward one row:                  C-n
 (This can also be given a             Also downarrow and RET.
  numeric argument to move
  more than one row at a time.)
Move backward one row:                 C-p
 (This can also be given a             Also uparrow.
  numeric argument to move
  more than one row at a time.)
Move to the first row:                 M-p
Move to the last row:                  M-n
Move to the first column:              C-a
                                       Also M-b.
Move to the last column:               C-e
                                       Also M-f.
Move to the beginning of the table:    C-cC-c
                                       Also M-<
Move to the end of the table:          M->

Editing
-------
Edit (or evaluate) the current cell:   =
Edit the class name:                   C-cC-ec
Edit the section name:                 C-cC-es
Edit the semester name:                C-cC-em
Edit the instructor's name:            C-cC-ei

Updating
--------
Update the current column:             C-c=
Update all columns:                    C-c#

Insertion
---------
Add a student:                         C-cC-n
Add several students:                  C-cC-a
Add a new column (at the end):         C-cC-o
Add a new column (at point):           C-cC-s
Insert scores for student:             C-cC-g
Insert scores for column:              C-cC-i

Deletion
--------
Delete student:                        C-k
Delete column:                         C-cC-k

Displaying information
----------------------
Display student name                   s
Display column name                    c

Moving information
------------------
Mark student for moving:               *
Move marked student:                   @
Mark column for moving:                C-c*
Move marked column:                    C-c@
Move current column to end:            C-c]

Ordering information
--------------------
Alphabetize students                   C-cC-a
Alphabetize in reverse order           C-uC-cC-a
Arrange according to column            C-c>
Arrange in reverse order               C-c<

Hiding information
------------------
Hide student                           /
Hide column                            C-c/
Mark student for hiding                .
Mark column for hiding                 C-c.
Hide marked information                C-c-
Hide marked students                   C-cC-hs
Hide marked columns                    C-cC-hc
Show all hidden information            C-c+
Show unmarked hidden information       C-cC-v

Reports
-------
Create a student reports buffer:       C-cC-x

TeX
---
Create a grade TeX file:               C-cC-t
Create a student reports TeX file:     C-cC-r
Create a list of students:             C-cC-l

Exporting
---------
Export as a csv file                   M-x grade-export-data
"
  (interactive)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map grade-mode-map)
  (setq mode-name "Grade")
  (setq major-mode 'grade-mode)
  (make-variable-buffer-local 'grade-total-columns)
  (make-variable-buffer-local 'grade-total-rows)
  (make-variable-buffer-local 'grade-point-min)
  (make-variable-buffer-local 'grade-point-max)
  (make-variable-buffer-local 'grade-report-column-width)
  (make-variable-buffer-local 'grade-report-avg-column)
  (make-variable-buffer-local 'grade-invisible-students)
  (make-variable-buffer-local 'grade-invisible-columns)
  (make-variable-buffer-local 'grade-first-column-width)
  (make-variable-buffer-local 'grade-second-column-width)
  (make-variable-buffer-local 'grade-name1-string)
  (make-variable-buffer-local 'grade-name2-string)
  (make-variable-buffer-local 'grade-fname-column-width)
  (make-variable-buffer-local 'grade-lname-column-width)
  (make-variable-buffer-local 'grade-studentid-column-width)
  (make-variable-buffer-local 'grade-score-column-width)
  (make-variable-buffer-local 'grade-use-alphabetical-order)
  (make-variable-buffer-local 'grade-use-studentid)
  (make-variable-buffer-local 'grade-tex-columns-per-page)
  (make-variable-buffer-local 'grade-first-name-first)
  (hack-local-variables)
  (setq grade-report-column-width (+ grade-score-column-width 5))
  (setq grade-report-avg-column (+ grade-report-column-width
                                   (* 2 grade-score-column-width)
                                   2))
  (setq grade-invisible-students 0)
  (setq grade-invisible-columns 0)
  (setq grade-first-column-width  (if grade-first-name-first
                                      grade-fname-column-width
                                    grade-lname-column-width))
  (setq grade-second-column-width (if grade-first-name-first
                                      grade-lname-column-width
                                    grade-fname-column-width))
  (setq grade-name1-string (if grade-first-name-first
                               "First name: "
                             "Last name: "))
  (setq grade-name2-string (if grade-first-name-first
                               "Last name: "
                             "First name: "))
  (setq grade-tex-columns-per-page
        (/
         (- 100 (+ grade-fname-column-width 3)
            (+ grade-lname-column-width 3)
            (if grade-use-studentid
                (+ grade-studentid-column-width 3)
              0))
         (+ grade-score-column-width 3)))
  (if (= (point-max) 1)
      (grade-new-class))
  (grade-set-rows-columns)
  (grade-find-boundaries)
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible nil)
  (setq buffer-read-only t)
  (grade-goto-table)
  (run-hooks 'grade-mode-hook))

;;; Now the function

(defun grade (arg)
  (interactive "P")
  (find-file (expand-file-name
              (read-file-name "Grade file: "
                              grade-default-directory)))
  (grade-mode)
  (grade-hide-marked-students-and-columns)
  (if arg
      (grade-show-reports)))

;;; Some stuff for the report buffer

(defun grade-reports-insert-student (sname sid info buf)
  "Insert the student information into the report buffer"
  (let ((cname (grade-class-name))
        (sc)
        (i)
        (grcw grade-report-column-width)
        (gscw grade-score-column-width)
        (gar grade-put-avg-in-reports)
        (grac grade-report-avg-column)
        (sem (grade-semester-name))
        (sec (grade-section-name))
        (instr (grade-instructor-name)))
    (save-excursion
      (set-buffer buf)
      (insert "\n\n")
      (insert cname "\n")
      (if sec (insert "Section: " sec "\n"))
      (if sem (insert "Semester: " sem "\n"))
      (if instr (insert "Instructor: " instr "\n"))
      (insert "\n\n")
      (insert "Report for " sname "\n")
      (if sid (insert "        ID:" sid "\n"))
      (insert "\n\n")
      (while info
        (setq sc (car info))
        (insert (nth 0 sc))
        (when (string= (nth 1 sc) (make-string gscw ?=))
          (insert "*"))
        (insert ":")
        (setq i (+ (length (nth 0 sc)) 1))
        (if (string= (nth 1 sc) (make-string gscw ?=))
            (setq i (1+ i)))
        (while (< i grcw)
          (insert " ")
          (setq i (+ i 1)))
        (insert (nth 2 sc))
        (if (> (string-to-number (nth 1 sc)) 0)
            (insert "/" (nth 1 sc)))
        (when (and gar  (not (string= (nth 3 sc) "")))
          (setq i (- (point) (line-beginning-position)))
          (while (< i grac)
            (insert " ")
            (setq i (1+ i)))
          (insert "  (Class average: " (nth 3 sc) ")"))
        (insert "\n")
        (setq info (cdr info)))
      (insert "\n"))))

(defun grade-show-reports ()
  (interactive)
  (let* ((class (grade-class-name))
         (sec (grade-section-name))
         (sem (grade-semester-name))
         (instr (grade-instructor-name))
         (i 3)
         (forms (grade-formulas))
         (bufname (concat "*Reports for " class
                          (if sec (concat ", Section " sec) "")
                          "*"))
         (reportbuf (get-buffer-create bufname)))
    (save-excursion
      (set-buffer reportbuf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "\nReports for " class "\n")
      (if sec (insert "Section: " sec "\n"))
      (if sem (insert "Semester: " sem "\n"))
      (if instr (insert "Instructor: " instr "\n"))
      (insert "\n\n")
      (insert "\"n\" for next record\n")
      (insert "\"p\" for previous record\n")
      (insert "\"s\" to search forward for expression\n")
      (insert "\"r\" to search backward for expression\n")
      (insert "\"q\" to quit reading reports\n"))
    (save-excursion
      (grade-goto-row 3)
      (while (< i (- grade-total-rows grade-invisible-students))
        (save-excursion
          (set-buffer reportbuf)
          (insert ""))
        (grade-reports-insert-student
         (grade-student-name)
         (grade-studentid)
         (grade-student-info)
         reportbuf)
        (if (and forms (not (string= forms "")))
            (save-excursion
              (set-buffer reportbuf)
              (insert "\n* Formulas used: \n")
              (insert forms "\n")))
        (setq i (+ i 1))
        (grade-forward-row)))
    (switch-to-buffer reportbuf)
    (goto-char (point-min))
    (grade-report-mode)))

(defun grade-report-show-current-entry ()
  "Narrow to the current entry"
  (let ((beg)
        (end))
    (save-excursion
      (if (search-forward "" (point-max) t)
          (setq end (- (point) 1))
        (setq end (point-max))))
    (save-excursion
      (if (search-backward "" (point-min) t)
          (setq beg (+ (point) 2))
        (setq beg (point-min))))
    (narrow-to-region beg end)
    (goto-char (point-min))))

(defun grade-report-next-entry ()
  "Go to the next entry"
  (interactive)
  (widen)
  (if (search-forward "" (point-max) t)
      (forward-char 1))
  (grade-report-show-current-entry))

(defun grade-report-previous-entry ()
  "Go to the previous entry"
  (interactive)
  (widen)
  (if (search-backward "" (point-min) t)
      (forward-char -1))
  (grade-report-show-current-entry))

(defvar grade-report-last-search nil)

(defun grade-report-search (regexp)
  "Search for a regular expression throughout the entries."
  (interactive
   (list (read-string (concat "Grade report search for"
				  (if grade-report-last-search
				   (concat " ("
					   grade-report-last-search
					   ")"))
				  ": "))))
  (let ((repeat))
    (if (equal "" regexp)
        (setq regexp grade-report-last-search
              repeat t)
      (setq repeat nil))
    (let ((start (point-min))
          (end (point-max))
          (here (point)))
      (widen)
      (if repeat
          (forward-char 1))
      (if (null (re-search-forward regexp (point-max) t))
          (progn
            (narrow-to-region start end)
            (goto-char here)
            (message (concat "\"" regexp "\" not found.")))
        (save-excursion
          (grade-report-show-current-entry)))
      (setq grade-report-last-search regexp))))

(defun grade-report-backward-search (regexp)
  "Reverse search for a regular expression throughout the entries."
  (interactive
   (list (read-string (concat "Grade report search for"
				  (if grade-report-last-search
				   (concat " ("
					   grade-report-last-search
					   ")"))
				  ": "))))
  (let ((repeat))
    (if (equal "" regexp)
        (setq regexp grade-report-last-search
              repeat t)
      (setq repeat nil))
    (let ((start (point-min))
          (end (point-max))
          (here (point)))
      (widen)
      (if repeat
          (forward-char -1))
      (if (null (re-search-backward regexp (point-min) t))
          (progn
            (narrow-to-region start end)
            (goto-char here)
            (message (concat "\"" regexp "\" not found.")))
        (save-excursion
          (grade-report-show-current-entry)))
      (setq grade-report-last-search regexp))))

(defun grade-report-kill ()
  (interactive)
  (kill-buffer (current-buffer)))

(defvar grade-report-mode-map nil
  "The keymap used for viewing individual student reports")

(if grade-report-mode-map
    ()
  (setq grade-report-mode-map (make-sparse-keymap))
  (define-key grade-report-mode-map "\C-n" 'grade-report-next-entry)
  (define-key grade-report-mode-map "\C-p" 'grade-report-previous-entry)
  (define-key grade-report-mode-map "\C-s" 'grade-report-search)
  (define-key grade-report-mode-map "\C-r" 'grade-report-backward-search)
  (define-key grade-report-mode-map "n" 'grade-report-next-entry)
  (define-key grade-report-mode-map "p" 'grade-report-previous-entry)
  (define-key grade-report-mode-map "s" 'grade-report-search)
  (define-key grade-report-mode-map "r" 'grade-report-backward-search)
  (define-key grade-report-mode-map (kbd "SPC")  'scroll-up)
  (define-key grade-report-mode-map (kbd "<delete>")  'scroll-down)
  (define-key grade-report-mode-map "q" 'grade-report-kill))

(easy-menu-define grade-report-mode-menu grade-report-mode-map
  "Grade report mode menu"
  '("Grade Reports"
    ["Next student" grade-report-next-entry t]
    ["Previous student" grade-report-previous-entry t]
    ["Search" grade-report-search t]
    ["Backwards search" grade-report-backward-search t]))

(defun grade-report-mode ()
  "A mode for looking at individual student grades"
  (interactive)
  (kill-all-local-variables)
  (widen)
  (make-variable-buffer-local 'grade-report-last-search)
  (setq major-mode 'grade-report-mode)
  (setq mode-name "Grade report")
  (use-local-map grade-report-mode-map)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (grade-report-show-current-entry))

;;; grade.el ends here

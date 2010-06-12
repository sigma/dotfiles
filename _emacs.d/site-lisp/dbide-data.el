;;;; dbide-data.el --- DocBook IDE element and attribute data
;; $Id: dbide-data.el,v 1.2 2000/03/29 19:12:16 nwalsh Exp $

;; Copyright (C) 2000 Norman Walsh
;; Based extensively on (one might go so far as to say "totally hacked
;; from") Tony Graham's xslide.

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Created: 29 March 2000
;; Version: $Revision: 1.2 $
;; Keywords: languages, xml, docbook

;;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;;; Commentary:

;; Data about elements and attributes in DOCBOOK documents collected
;; in one place

;; Send bugs to docbookide-bug@menteith.com
;; Use `docbook-submit-bug-report' for bug reports


;;;; Variables

;; These are broken into separate lists to avoid problems with nesting
;; depth

(defvar docbook-element-symbol-alist
  (list
    '("abbrev"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xabbrev")
    '("abstract"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xabstract")
    '("accel"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xaccel")
    '("ackno"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xackno")
    '("acronym"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xacronym")
    '("action"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xaction")
    '("address"
      "block"
      ("arch" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xaddress")
    '("affiliation"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xaffiliation")
    '("alt"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xalt")
    '("anchor"
      "empty"
      ("arch" "conformance" "id" "lang" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xanchor")
    '("answer"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xanswer")
    '("appendix"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xappendix")
    '("appendixinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xappendixinfo")
    '("application"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xapplication")
    '("area"
      "empty"
      ("arch" "conformance" "coords" "id" "label" "lang" "linkends" "os" "otherunits" "remap" "revision" "revisionflag" "role" "units" "userlevel" "vendor" "xreflabel" )
      "xarea")
    '("areaset"
      "block"
      ("arch" "conformance" "coords" "id" "label" "lang" "os" "otherunits" "remap" "revision" "revisionflag" "role" "units" "userlevel" "vendor" "xreflabel" )
      "xareaset")
    '("areaspec"
      "block"
      ("arch" "conformance" "id" "lang" "os" "otherunits" "remap" "revision" "revisionflag" "role" "units" "userlevel" "vendor" "xreflabel" )
      "xareaspec")
    '("arg"
      "inline"
      ("arch" "choice" "conformance" "id" "lang" "os" "remap" "rep" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xarg")
    '("artheader"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xartheader")
    '("article"
      "block"
      ("arch" "class" "conformance" "id" "lang" "os" "parentbook" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xarticle")
    '("articleinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xarticleinfo")
    '("artpagenums"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xartpagenums")
    '("attribution"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xattribution")
    '("audiodata"
      "block"
      ("arch" "conformance" "entityref" "fileref" "format" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "srccredit" "userlevel" "vendor" "xreflabel" )
      "xaudiodata")
    '("audioobject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xaudioobject")
    '("author"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xauthor")
    '("authorblurb"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xauthorblurb")
    '("authorgroup"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xauthorgroup")
    '("authorinitials"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xauthorinitials")))

(defvar docbook-element-symbol-alist-1
  (list
    '("beginpage"
      "empty"
      ("arch" "conformance" "id" "lang" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbeginpage")
    '("bibliodiv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xbibliodiv")
    '("biblioentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbiblioentry")
    '("bibliography"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xbibliography")
    '("bibliographyinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbibliographyinfo")
    '("bibliomisc"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbibliomisc")
    '("bibliomixed"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbibliomixed")
    '("bibliomset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "relation" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbibliomset")
    '("biblioset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "relation" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbiblioset")
    '("blockquote"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xblockquote")
    '("book"
      "block"
      ("arch" "conformance" "fpi" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xbook")
    '("bookbiblio"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbookbiblio")
    '("bookinfo"
      "block"
      ("arch" "conformance" "contents" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbookinfo")
    '("bridgehead"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "renderas" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xbridgehead")
    '("callout"
      "block"
      ("arch" "arearefs" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcallout")
    '("calloutlist"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcalloutlist")
    '("caption"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcaption")
    '("caution"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcaution")
    '("chapter"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xchapter")
    '("chapterinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xchapterinfo")
    '("citation"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcitation")
    '("citerefentry"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xciterefentry")
    '("citetitle"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "pubwork" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcitetitle")
    '("city"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcity")
    '("classname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xclassname")
    '("classsynopsis"
      "block"
      ("arch" "class" "conformance" "id" "lang" "language" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xclasssynopsis")
    '("classsynopsisinfo"
      "block"
      ("arch" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xclasssynopsisinfo")
    '("cmdsynopsis"
      "block"
      ("arch" "cmdlength" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "sepchar" "userlevel" "vendor" "xreflabel" )
      "xcmdsynopsis")
    '("co"
      "empty"
      ("arch" "conformance" "id" "label" "lang" "linkends" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xco")
    '("collab"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcollab")
    '("collabname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcollabname")
    '("colophon"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xcolophon")
    '("colspec"
      "block"
      ("align" "arch" "char" "charoff" "colname" "colnum" "colsep" "colwidth" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "rowsep" "userlevel" "vendor" "xreflabel" )
      "xcolspec")
    '("command"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcommand")
    '("comment"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcomment")
    '("computeroutput"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcomputeroutput")
    '("confdates"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconfdates")
    '("confgroup"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconfgroup")
    '("confnum"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconfnum")
    '("confsponsor"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconfsponsor")
    '("conftitle"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconftitle")
    '("constant"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconstant")
    '("constructorsynopsis"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xconstructorsynopsis")
    '("contractnum"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcontractnum")
    '("contractsponsor"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcontractsponsor")
    '("contrib"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcontrib")
    '("copyright"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcopyright")
    '("corpauthor"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcorpauthor")
    '("corpname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcorpname")
    '("country"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xcountry")))

(defvar docbook-element-symbol-alist-2
  (list
    '("database"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xdatabase")
    '("date"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xdate")
    '("dedication"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xdedication")
    '("destructorsynopsis"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xdestructorsynopsis")
    '("docinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xdocinfo")
    '("edition"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xedition")
    '("editor"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xeditor")
    '("email"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xemail")
    '("emphasis"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xemphasis")
    '("entry"
      "block"
      ("align" "arch" "char" "charoff" "colname" "colsep" "conformance" "id" "lang" "morerows" "nameend" "namest" "os" "remap" "revision" "revisionflag" "role" "rotate" "rowsep" "spanname" "userlevel" "valign" "vendor" "xreflabel" )
      "xentry")
    '("entrytbl"
      "block"
      ("align" "arch" "char" "charoff" "colname" "cols" "colsep" "conformance" "id" "lang" "nameend" "namest" "os" "remap" "revision" "revisionflag" "role" "rowsep" "spanname" "tgroupstyle" "userlevel" "vendor" "xreflabel" )
      "xentrytbl")
    '("envar"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xenvar")
    '("epigraph"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xepigraph")
    '("equation"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xequation")
    '("errorcode"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xerrorcode")
    '("errorname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xerrorname")
    '("errortype"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xerrortype")
    '("example"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "width" "xreflabel" )
      "xexample")
    '("exceptionname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xexceptionname")
    '("fax"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfax")
    '("fieldsynopsis"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfieldsynopsis")
    '("figure"
      "block"
      ("arch" "conformance" "float" "id" "label" "lang" "os" "pgwide" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfigure")
    '("filename"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "path" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfilename")
    '("firstname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfirstname")
    '("firstterm"
      "inline"
      ("arch" "conformance" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfirstterm")
    '("footnote"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfootnote")
    '("footnoteref"
      "empty"
      ("arch" "conformance" "id" "label" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfootnoteref")
    '("foreignphrase"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xforeignphrase")
    '("formalpara"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xformalpara")
    '("funcdef"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfuncdef")
    '("funcparams"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfuncparams")
    '("funcprototype"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfuncprototype")
    '("funcsynopsis"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfuncsynopsis")
    '("funcsynopsisinfo"
      "block"
      ("arch" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfuncsynopsisinfo")
    '("function"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xfunction")))

(defvar docbook-element-symbol-alist-3
  (list
    '("glossary"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xglossary")
    '("glossaryinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xglossaryinfo")
    '("glossdef"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "subject" "userlevel" "vendor" "xreflabel" )
      "xglossdef")
    '("glossdiv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xglossdiv")
    '("glossentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "sortas" "userlevel" "vendor" "xreflabel" )
      "xglossentry")
    '("glosslist"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xglosslist")
    '("glosssee"
      "block"
      ("arch" "conformance" "id" "lang" "os" "otherterm" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xglosssee")
    '("glossseealso"
      "block"
      ("arch" "conformance" "id" "lang" "os" "otherterm" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xglossseealso")
    '("glossterm"
      "inline"
      ("arch" "baseform" "conformance" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xglossterm")
    '("graphic"
      "block"
      ("align" "arch" "conformance" "depth" "entityref" "fileref" "format" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "scale" "scalefit" "srccredit" "userlevel" "vendor" "width" "xreflabel" )
      "xgraphic")
    '("graphicco"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xgraphicco")
    '("group"
      "inline"
      ("arch" "choice" "conformance" "id" "lang" "optmult" "os" "remap" "rep" "reqmult" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xgroup")
    '("guibutton"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguibutton")
    '("guiicon"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguiicon")
    '("guilabel"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguilabel")
    '("guimenu"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguimenu")
    '("guimenuitem"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguimenuitem")
    '("guisubmenu"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xguisubmenu")
    '("hardware"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xhardware")
    '("highlights"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xhighlights")
    '("holder"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xholder")
    '("honorific"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xhonorific")
    '("imagedata"
      "block"
      ("align" "arch" "conformance" "depth" "entityref" "fileref" "format" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "scale" "scalefit" "srccredit" "userlevel" "vendor" "width" "xreflabel" )
      "ximagedata")
    '("imageobject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "ximageobject")
    '("imageobjectco"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "ximageobjectco")
    '("important"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "ximportant")
    '("index"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xindex")
    '("indexdiv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xindexdiv")
    '("indexentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xindexentry")
    '("indexinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xindexinfo")
    '("indexterm"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "pagenum" "remap" "revision" "revisionflag" "role" "scope" "significance" "startref" "userlevel" "vendor" "xreflabel" "zone" )
      "xindexterm")
    '("informalequation"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinformalequation")
    '("informalexample"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "width" "xreflabel" )
      "xinformalexample")
    '("informalfigure"
      "block"
      ("arch" "conformance" "float" "id" "label" "lang" "os" "pgwide" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinformalfigure")
    '("informaltable"
      "block"
      ("arch" "colsep" "conformance" "frame" "id" "label" "lang" "orient" "os" "pgwide" "remap" "revision" "revisionflag" "role" "rowsep" "shortentry" "tabstyle" "tocentry" "userlevel" "vendor" "xreflabel" )
      "xinformaltable")
    '("initializer"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinitializer")
    '("inlineequation"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinlineequation")
    '("inlinegraphic"
      "inline"
      ("align" "arch" "conformance" "depth" "entityref" "fileref" "format" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "scale" "scalefit" "srccredit" "userlevel" "vendor" "width" "xreflabel" )
      "xinlinegraphic")
    '("inlinemediaobject"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinlinemediaobject")
    '("interface"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinterface")
    '("interfacedefinition"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinterfacedefinition")
    '("interfacename"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinterfacename")
    '("invpartnumber"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xinvpartnumber")
    '("isbn"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xisbn")
    '("issn"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xissn")
    '("issuenum"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xissuenum")
    '("itemizedlist"
      "block"
      ("arch" "conformance" "id" "lang" "mark" "os" "remap" "revision" "revisionflag" "role" "spacing" "userlevel" "vendor" "xreflabel" )
      "xitemizedlist")
    '("itermset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xitermset")))

(defvar docbook-element-symbol-alist-4
  (list
    '("jobtitle"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xjobtitle")
    '("keycap"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeycap")
    '("keycode"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeycode")
    '("keycombo"
      "inline"
      ("action" "arch" "conformance" "id" "lang" "moreinfo" "os" "otheraction" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeycombo")
    '("keysym"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeysym")
    '("keyword"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeyword")
    '("keywordset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xkeywordset")
    '("label"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlabel")
    '("legalnotice"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlegalnotice")
    '("lineage"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlineage")
    '("lineannotation"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlineannotation")
    '("link"
      "inline"
      ("arch" "conformance" "endterm" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "type" "userlevel" "vendor" "xreflabel" )
      "xlink")
    '("listitem"
      "block"
      ("arch" "conformance" "id" "lang" "os" "override" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlistitem")
    '("literal"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xliteral")
    '("literallayout"
      "block"
      ("arch" "class" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "width" "xreflabel" )
      "xliterallayout")
    '("lot"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xlot")
    '("lotentry"
      "block"
      ("arch" "conformance" "id" "lang" "linkend" "os" "pagenum" "remap" "revision" "revisionflag" "role" "srccredit" "userlevel" "vendor" "xreflabel" )
      "xlotentry")
    '("manvolnum"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmanvolnum")
    '("markup"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmarkup")
    '("medialabel"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmedialabel")
    '("mediaobject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmediaobject")
    '("mediaobjectco"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmediaobjectco")
    '("member"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmember")
    '("menuchoice"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmenuchoice")
    '("methodname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmethodname")
    '("methodparam"
      "inline"
      ("arch" "choice" "conformance" "id" "lang" "os" "remap" "rep" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmethodparam")
    '("methodsynopsis"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmethodsynopsis")
    '("modespec"
      "block"
      ("application" "arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmodespec")
    '("modifier"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmodifier")
    '("mousebutton"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmousebutton")
    '("msg"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsg")
    '("msgaud"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgaud")
    '("msgentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgentry")
    '("msgexplan"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgexplan")
    '("msginfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsginfo")
    '("msglevel"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsglevel")
    '("msgmain"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgmain")
    '("msgorig"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgorig")
    '("msgrel"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgrel")
    '("msgset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgset")
    '("msgsub"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgsub")
    '("msgtext"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xmsgtext")))

(defvar docbook-element-symbol-alist-5
  (list
    '("note"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xnote")
    '("objectinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xobjectinfo")
    '("olink"
      "inline"
      ("arch" "conformance" "id" "lang" "linkmode" "localinfo" "os" "remap" "revision" "revisionflag" "role" "targetdocent" "type" "userlevel" "vendor" "xreflabel" )
      "xolink")
    '("ooclass"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xooclass")
    '("ooexception"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xooexception")
    '("oointerface"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xoointerface")
    '("option"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xoption")
    '("optional"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xoptional")
    '("orderedlist"
      "block"
      ("arch" "conformance" "continuation" "id" "inheritnum" "lang" "numeration" "os" "remap" "revision" "revisionflag" "role" "spacing" "userlevel" "vendor" "xreflabel" )
      "xorderedlist")
    '("orgdiv"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xorgdiv")
    '("orgname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xorgname")
    '("otheraddr"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xotheraddr")
    '("othercredit"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xothercredit")
    '("othername"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xothername")
    '("pagenums"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpagenums")
    '("para"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpara")
    '("paramdef"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xparamdef")
    '("parameter"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xparameter")
    '("part"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xpart")
    '("partinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpartinfo")
    '("partintro"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpartintro")
    '("phone"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xphone")
    '("phrase"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xphrase")
    '("pob"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpob")
    '("postcode"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpostcode")
    '("preface"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xpreface")
    '("prefaceinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprefaceinfo")
    '("primary"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "sortas" "userlevel" "vendor" "xreflabel" )
      "xprimary")
    '("primaryie"
      "block"
      ("arch" "conformance" "id" "lang" "linkends" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprimaryie")
    '("printhistory"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprinthistory")
    '("procedure"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprocedure")
    '("productname"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xproductname")
    '("productnumber"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xproductnumber")
    '("programlisting"
      "block"
      ("arch" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "width" "xreflabel" )
      "xprogramlisting")
    '("programlistingco"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprogramlistingco")
    '("prompt"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xprompt")
    '("property"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xproperty")
    '("pubdate"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpubdate")
    '("publisher"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpublisher")
    '("publishername"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpublishername")
    '("pubsnumber"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xpubsnumber")))

(defvar docbook-element-symbol-alist-6
  (list
    '("qandadiv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xqandadiv")
    '("qandaentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xqandaentry")
    '("qandaset"
      "block"
      ("arch" "conformance" "defaultlabel" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xqandaset")
    '("question"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xquestion")
    '("quote"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xquote")
    '("refclass"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefclass")
    '("refdescriptor"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefdescriptor")
    '("refentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xrefentry")
    '("refentryinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefentryinfo")
    '("refentrytitle"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefentrytitle")
    '("reference"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xreference")
    '("referenceinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xreferenceinfo")
    '("refmeta"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefmeta")
    '("refmiscinfo"
      "block"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefmiscinfo")
    '("refname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefname")
    '("refnamediv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefnamediv")
    '("refpurpose"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefpurpose")
    '("refsect1"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect1")
    '("refsect1info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect1info")
    '("refsect2"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect2")
    '("refsect2info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect2info")
    '("refsect3"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect3")
    '("refsect3info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsect3info")
    '("refsynopsisdiv"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsynopsisdiv")
    '("refsynopsisdivinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrefsynopsisdivinfo")
    '("releaseinfo"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xreleaseinfo")
    '("remark"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xremark")
    '("replaceable"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xreplaceable")
    '("returnvalue"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xreturnvalue")
    '("revdescription"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrevdescription")
    '("revhistory"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrevhistory")
    '("revision"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrevision")
    '("revnumber"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrevnumber")
    '("revremark"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xrevremark")
    '("row"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "rowsep" "userlevel" "valign" "vendor" "xreflabel" )
      "xrow")))

(defvar docbook-element-symbol-alist-7
  (list
    '("sbr"
      "empty"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsbr")
    '("screen"
      "block"
      ("arch" "conformance" "format" "id" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "width" "xreflabel" )
      "xscreen")
    '("screenco"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xscreenco")
    '("screeninfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xscreeninfo")
    '("screenshot"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xscreenshot")
    '("secondary"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "sortas" "userlevel" "vendor" "xreflabel" )
      "xsecondary")
    '("secondaryie"
      "block"
      ("arch" "conformance" "id" "lang" "linkends" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsecondaryie")
    '("sect1"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect1")
    '("sect1info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect1info")
    '("sect2"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect2")
    '("sect2info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect2info")
    '("sect3"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect3")
    '("sect3info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect3info")
    '("sect4"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect4")
    '("sect4info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect4info")
    '("sect5"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect5")
    '("sect5info"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsect5info")
    '("section"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xsection")
    '("sectioninfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsectioninfo")
    '("see"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsee")
    '("seealso"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseealso")
    '("seealsoie"
      "block"
      ("arch" "conformance" "id" "lang" "linkends" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseealsoie")
    '("seeie"
      "block"
      ("arch" "conformance" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseeie")
    '("seg"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseg")
    '("seglistitem"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseglistitem")
    '("segmentedlist"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsegmentedlist")
    '("segtitle"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsegtitle")
    '("seriesinfo"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseriesinfo")
    '("seriesvolnums"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xseriesvolnums")
    '("set"
      "block"
      ("arch" "conformance" "fpi" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "status" "userlevel" "vendor" "xreflabel" )
      "xset")
    '("setindex"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsetindex")
    '("setindexinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsetindexinfo")
    '("setinfo"
      "block"
      ("arch" "conformance" "contents" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsetinfo")
    '("sgmltag"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsgmltag")
    '("shortaffil"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xshortaffil")
    '("shortcut"
      "inline"
      ("action" "arch" "conformance" "id" "lang" "moreinfo" "os" "otheraction" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xshortcut")
    '("sidebar"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsidebar")
    '("sidebarinfo"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsidebarinfo")
    '("simpara"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsimpara")
    '("simplelist"
      "block"
      ("arch" "columns" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "type" "userlevel" "vendor" "xreflabel" )
      "xsimplelist")
    '("simplemsgentry"
      "block"
      ("arch" "audience" "conformance" "id" "lang" "level" "origin" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsimplemsgentry")
    '("simplesect"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsimplesect")
    '("spanspec"
      "block"
      ("align" "arch" "char" "charoff" "colsep" "conformance" "id" "lang" "nameend" "namest" "os" "remap" "revision" "revisionflag" "role" "rowsep" "spanname" "userlevel" "vendor" "xreflabel" )
      "xspanspec")
    '("state"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xstate")
    '("step"
      "block"
      ("arch" "conformance" "id" "lang" "os" "performance" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xstep")
    '("street"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xstreet")
    '("structfield"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xstructfield")
    '("structname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xstructname")
    '("subject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "weight" "xreflabel" )
      "xsubject")
    '("subjectset"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "scheme" "userlevel" "vendor" "xreflabel" )
      "xsubjectset")
    '("subjectterm"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsubjectterm")
    '("subscript"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsubscript")
    '("substeps"
      "block"
      ("arch" "conformance" "id" "lang" "os" "performance" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsubsteps")
    '("subtitle"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsubtitle")
    '("superscript"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsuperscript")
    '("surname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsurname")
    '("symbol"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsymbol")
    '("synopfragment"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsynopfragment")
    '("synopfragmentref"
      "block"
      ("arch" "conformance" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsynopfragmentref")
    '("synopsis"
      "block"
      ("arch" "conformance" "format" "id" "label" "lang" "linenumbering" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsynopsis")
    '("systemitem"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xsystemitem")))

(defvar docbook-element-symbol-alist-8
  (list
    '("table"
      "block"
      ("arch" "colsep" "conformance" "frame" "id" "label" "lang" "orient" "os" "pgwide" "remap" "revision" "revisionflag" "role" "rowsep" "shortentry" "tabstyle" "tocentry" "userlevel" "vendor" "xreflabel" )
      "xtable")
    '("tbody"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "valign" "vendor" "xreflabel" )
      "xtbody")
    '("term"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xterm")
    '("tertiary"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "sortas" "userlevel" "vendor" "xreflabel" )
      "xtertiary")
    '("tertiaryie"
      "block"
      ("arch" "conformance" "id" "lang" "linkends" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtertiaryie")
    '("textobject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtextobject")
    '("tfoot"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "valign" "vendor" "xreflabel" )
      "xtfoot")
    '("tgroup"
      "block"
      ("align" "arch" "char" "charoff" "cols" "colsep" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "rowsep" "tgroupstyle" "userlevel" "vendor" "xreflabel" )
      "xtgroup")
    '("thead"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "valign" "vendor" "xreflabel" )
      "xthead")
    '("tip"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtip")
    '("title"
      "block"
      ("arch" "conformance" "id" "lang" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtitle")
    '("titleabbrev"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtitleabbrev")
    '("toc"
      "block"
      ("arch" "conformance" "id" "lang" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoc")
    '("tocback"
      "block"
      ("arch" "conformance" "id" "label" "lang" "linkend" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtocback")
    '("tocchap"
      "block"
      ("arch" "conformance" "id" "label" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtocchap")
    '("tocentry"
      "block"
      ("arch" "conformance" "id" "lang" "linkend" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtocentry")
    '("tocfront"
      "block"
      ("arch" "conformance" "id" "label" "lang" "linkend" "os" "pagenum" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtocfront")
    '("toclevel1"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoclevel1")
    '("toclevel2"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoclevel2")
    '("toclevel3"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoclevel3")
    '("toclevel4"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoclevel4")
    '("toclevel5"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoclevel5")
    '("tocpart"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtocpart")
    '("token"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtoken")
    '("trademark"
      "inline"
      ("arch" "class" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtrademark")
    '("type"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xtype")
    '("ulink"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "type" "url" "userlevel" "vendor" "xreflabel" )
      "xulink")
    '("userinput"
      "inline"
      ("arch" "conformance" "id" "lang" "moreinfo" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xuserinput")
    '("varargs"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvarargs")
    '("variablelist"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "termlength" "userlevel" "vendor" "xreflabel" )
      "xvariablelist")
    '("varlistentry"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvarlistentry")
    '("varname"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvarname")
    '("videodata"
      "block"
      ("align" "arch" "conformance" "depth" "entityref" "fileref" "format" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "scale" "scalefit" "srccredit" "userlevel" "vendor" "width" "xreflabel" )
      "xvideodata")
    '("videoobject"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvideoobject")
    '("void"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvoid")
    '("volumenum"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xvolumenum")
    '("warning"
      "block"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xwarning")
    '("wordasword"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xwordasword")
    '("xref"
      "empty"
      ("arch" "conformance" "endterm" "id" "lang" "linkend" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xxref")
    '("year"
      "inline"
      ("arch" "conformance" "id" "lang" "os" "remap" "revision" "revisionflag" "role" "userlevel" "vendor" "xreflabel" )
      "xyear")))

;; It would be nice to make these start-tag dependent

(defvar docbook-attributes-alist
  (list
   '("action" "ac" ())
   '("align" "al" ())
   '("application" "ap" ())
   '("arch" "arch" ())
   '("arearefs" "ar" ())
   '("audience" "au" ())
   '("baseform" "bf" ())
   '("char" "char" ())
   '("charoff" "charo" ())
   '("choice" "ch" ())
   '("class" "cl" ())
   '("cmdlength" "cmdl" ())
   '("colname" "cna" ())
   '("colnum" "cnu" ())
   '("cols" "cols" ())
   '("colsep" "colsep" ())
   '("columns" "columns" ())
   '("colwidth" "colw" ())
   '("conformance" "conf" ())
   '("contents" "contents" ())
   '("continuation" "cont" ())
   '("coords" "co" ())
   '("defaultlabel" "dl" ())
   '("depth" "de" ())
   '("endterm" "et" ())
   '("entityref" "er" ())
   '("fileref" "fr" ())
   '("float" "fl" ())
   '("format" "fo" ())
   '("fpi" "fpi" ())
   '("frame" "fr" ())
   '("id" "id" ())
   '("inheritnum" "in" ())
   '("label" "la" ())
   '("lang" "lang" ())
   '("language" "language" ())
   '("level" "level" ())
   '("linenumbering" "ln" ())
   '("linkend" "le" ())
   '("linkends" "les" ())
   '("linkmode" "lm" ())
   '("localinfo" "li" ())
   '("mark" "ma" ())
   '("moreinfo" "morei" ())
   '("morerows" "morer" ())
   '("nameend" "ne" ())
   '("namest" "ns" ())
   '("numeration" "nu" ())
   '("optmult" "om" ())
   '("orient" "or" ())
   '("origin" "orig" ())
   '("os" "os" ())
   '("otheraction" "otha" ())
   '("otherterm" "otht" ())
   '("otherunits" "othu" ())
   '("override" "over" ())
   '("pagenum" "pn" ())
   '("parentbook" "pb" ())
   '("path" "path" ())
   '("performance" "perf" ())
   '("pgwide" "pgw" ())
   '("pubwork" "pw" ())
   '("relation" "rel" ())
   '("remap" "rem" ())
   '("renderas" "ras" ())
   '("rep" "rep" ())
   '("reqmult" "reqm" ())
   '("revision" "rev" ())
   '("revisionflag" "revf" ())
   '("role" "role" ())
   '("rotate" "rot" ())
   '("rowsep" "row" ())
   '("scale" "sc" ())
   '("scalefit" "scf" ())
   '("scheme" "sch" ())
   '("scope" "sc" ())
   '("sepchar" "sepc" ())
   '("shortentry" "sh" ())
   '("significance" "sig" ())
   '("sortas" "sas" ())
   '("spacing" "spac" ())
   '("spanname" "sn" ())
   '("srccredit" "srcc" ())
   '("startref" "str" ())
   '("status" "stat" ())
   '("subject" "subj" ())
   '("tabstyle" "tabs" ())
   '("targetdocent" "tdoc" ())
   '("termlength" "tlen" ())
   '("tgroupstyle" "tgs" ())
   '("tocentry" "toce" ())
   '("type" "type" ())
   '("units" "un" ())
   '("url" "url" ())
   '("userlevel" "ul" ())
   '("valign" "val" ())
   '("vendor" "ven" ())
   '("weight" "we" ())
   '("width" "wi" ())
   '("xreflabel" "xr" ())
   '("zone" "zo" ())))

;; If docbook-insert-tag inserts one of the following elements,
;; the corresponding content is also inserted automatically

(defvar docbook-autoinsert-alist
  (list
   (list "chapter" "<title>^</title>")
   (list "section" "<title>^</title>")
   (list "sect1" "<title>^</title>")
   (list "sect2" "<title>^</title>")
   (list "sect3" "<title>^</title>")
   (list "sect4" "<title>^</title>")
   (list "sect5" "<title>^</title>")
   (list "figure" "<title>^</title>")
   (list "example" "<title>^</title>")
   (list "equation" "<title>^</title>")
   (list "table" "<title>^</title>")
   (list "book"
	 (concat
	  "\n"
	  "  <bookinfo>\n"
	  "    <title>^</title>\n"
	  "    <author><firstname></firstname><surname></surname></author>\n"
	  "    <date>"
	  (substring (current-time-string) 8 10)
	  " "
	  (substring (current-time-string) 4 7)
	  " "
	  (substring (current-time-string) 20 24)
	  "</date>\n"
	  "  </bookinfo>\n"))
   (list "article" 
	 (concat
	  "\n"
	  "  <artheader>\n"
	  "    <title>^</title>\n"
	  "    <author><firstname></firstname><surname></surname></author>\n"
	  "    <date>"
	  (substring (current-time-string) 8 10)
	  " "
	  (substring (current-time-string) 4 7)
	  " "
	  (substring (current-time-string) 20 24)
	  "</date>\n"
	  "  </artheader>\n"))
   (list "author" "<firstname>^</firstname><surname></surname>")
   (list "editor" "<firstname>^</firstname><surname></surname>")
   (list "simplelist" "<member>^</member>")
   (list "varlistentry" "<term>^</term>\n  <listitem><para></para></listitem>")
   ))

(setq docbook-all-attribute-alist
      (sort
       docbook-attributes-alist
       (lambda (a b) (string< (car a) (car b)))))

(setq docbook-all-elements-alist
      (sort
       (append
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-1)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-2)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-3)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-4)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-5)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-6)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-7)
	(mapcar (lambda (x)
		  (cons (car x)
			(cdr x)))
		docbook-element-symbol-alist-8))
       (lambda (a b) (string< (car a) (car b)))))

(setq docbook-all-autoinsert-alist
      (sort
       docbook-autoinsert-alist
       (lambda (a b) (string< (car a) (car b)))))

(provide 'dbide-data)

;; end of dbide-data.el

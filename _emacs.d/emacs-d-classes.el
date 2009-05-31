;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;; Default emacs classes

;; Automaticly insert these include files when found in c++ file
(defvar project-include-classes '((("QLabel") "<qlabel.h>")
				  (("QSplitter") "<qsplitter.h>")
				  (("QLayout" "QHBoxLayout" "QVBoxLayout" "QBoxLayout" "QGridLayout") "<qlayout.h>")
				  (("QFrame") "<qframe.h>")
				  (("QDate" "QDateTime" "QTime") "<qdatetime.h>")
				  (("QPixmap") "<qpixmap.h>")
				  (("QString") "<qstring.h>")
				  (("QStringList") "<qstringlist.h>")
				  (("QPainter") "<qpainter.h>")
				  (("QGroupBox") "<qgroupbox.h>")
				  (("QApplication") "<qapplication.h>")
				  (("qDebug" "qWarning" "qFatal" "qInstallMsgHandler" "qSysInfo" "ASSERT" "CHECK_PTR" "qAddPostRoutine") "<qglobal.h>" )
				  (("QBrush") "<qbrush.h>")
				  (("QButton") "<qbutton.h>")
				  (("QCheckBox") "<qcheckbox.h>")
				  (("QColor") "<qcolor.h>")
				  (("QPalette" "QColorGroup") "<qpalette.h>")
				  (("QComboBox") "<qcombobox.h>")
				  (("QDialog") "<qdialog.h>")
				  (("QDir") "<qdir.h>")
				  (("QEvent" "QMouseEvent" "QMoveEvent" "QFocusEvent" "QKeyEvent" "QPaintEvent" "QResizeEvent" "QTimerEvent") "<qevent.h>")
				  (("QFile") "<qfile.h>")
				  (("QFileInfo") "<qfileinfo.h>")
				  (("QFont") "<qfont.h>")
				  (("QFontInfo") "<qfontinfo.h>")
				  (("QFontMetrics") "<qfontmetrics.h>")
				  (("QHeader" "header()") "<qheader.h>")
				  (("QImage") "<qimage.h>")
				  (("QLineEdit") "<qlineedit.h>")
				  (("QList" "QListIterator") "<qlist.h>")
				  (("QListBox" "QListBoxItem" "QListBoxPixmap" "QListBoxTexT") "<qlistbox.h>")
				  (("QListView" "QListViewItem") "<qlistview.h>")
				  (("QMessageBox") "<qmessagebox.h>")
				  (("QMainWindow") "<qmainwindow.h>")
				  (("menuBar\\(\\)" "QMenuBar") "<qmenubar.h>")
				  (("QMultiLineEdit") "<qmultilinedit.h>")
				  (("QObject" "Q_OBJECT") "<qobject.h>")
				  (("QPaintDevice") "<qpaintdevice.h>")
				  (("QPen") "<qpen.h>")
				  (("QPicture") "<qpicture.h>")
				  (("QPoint") "<qpoint.h>")
				  (("QPopupMenu") "<qpopupmenu.h>")
				  (("QPrinter") "<qprinter.h>")
				  (("QPushButton") "<qpushbutton.h>")
				  (("QRadioButton") "<qradiobutton.h>")
				  (("QRect") "<qrect.h>")
				  (("QRegExp") "<qregexp.h>")
				  (("QRegion") "<qregion.h>")
				  (("QScrollBar") "<qscrollbar.h>")
				  (("QScrollView") "<qscrollview.h>")
				  (("QSignal") "<qsignal.h>")
				  (("QSize") "<qsize.h>")
				  (("QSlider") "<qslider.h>")
				  (("QSpinBox") "<qspinbox.h>")
				  (("QStack") "<qstack.h>")
				  (("statusBar()" "QStatusBar") "<qstatusbar.h>")
				  (("QTableView") "<qtableview.h>")
				  (("QTextStream") "<qtextstream.h>")
				  (("QTimer") "<qtimer.h>")
				  (("QToolBar") "<qtoolbar.h>")
				  (("QToolButton") "<qtoolbutton.h>")
				  (("toolTipGroup()" "QToolTip" "QToolTipGroup") "<qtooltip.h>")
				  (("QWhatsThis") "<qwhatsthis.h>")
				  (("QWidget") "<qwidget.h>")
				  (("QWidgetStack") "<qwidgetstack.h>")
				  (("QWindow") "<qwindow.h>")
				  (("QWMatrix") "<qwmatrix.h>")
				  (("QArray") "<qarray.h>")
				  (("QVector") "<qvector.h>")
				  (("qHeapSort" "qBubbleSort" "qSwap" "qCopy") "<qtl.h>")
				  (("Qt::") "<qnamespace.h>")
				  (("QDict" "QDictIterator") "<qdict.h>")
				  (("QAsciiDict") "<qasciidict.h>")
				  (("QIntDict") "<qintdict.h>")
				  (("QPtrDict") "<qptrdict.h>")
				  (("QCollection") "<qcollection.h>")
				  (("QAsciiCache" "QAsciiCacheIterator") "<qasciicache.h>")
				  (("QMap" "QMapIterator" "QMapConstIterator") "<qmap.h>")
				  (("QMotifStyle") "<qmotifstyle.h>")
				  (("QCDEStyle") "<qcdestyle.h>")
				  (("QWindowsStyle") "<qwindowsstyle.h>")
				  (("QPlatinumStyle") "<qplatinumstyle.h>")
				  (("QValidator") "<qvalidator.h>")
				  (("QValueList") "<qvaluelist.h>")

;; New classes for Qt 2.2.0 or higher
;; Canvas
				  (("QCanvas" "QCanvasView" "QCanvasItem"
				    "QCanvasText" "QCanvasSprite" "QCanvasPolygonalItem"
				    "QCanvasEllipse" "QCanvasLine" "QCanvasPolygon" "QCanvasRectangle") "<qcanvas.h>")
;; Iconview
				  (("QIconView" "QIconViewItem") "<qiconview.h>")
;; Network
				  (("QSocket") "<qsocket.h>")
				  (("QServerSocket") "<qserversocket.h>")
				  (("QDns") "<qdns.h>")
				  (("QNetworkProtocol" "QNetworkOperation") "<qnetworkprotocol.h>")
				  (("QFtp") "<qftp.h>")
				  (("QLocalFs") "<qlocalfs.h>")
				  (("QUrlOperator") "<qurloperator.h>")
				  (("QUrl") "<qurl.h>")
;; OpenGL
				  (("QGL" "QGLWidget" "QGLContext" "QGLFormat") "<qgl.h>")
;; Table
				  (("QTable" "QTableItem") "<qtable.h>")
;; Workspace
				  (("QWorkspace") "<qworkspace.h>")
;; XML
				  (("QXmlReader" "QXmlSimpleReader" "QXmlInputSource" "QXmlAttributes" "QXmlLocator" "QXmlNamespaceSupport"
				    "QXmlDefaultHandler" "QXmlContentHandler" "QXmlDTDHandler" "QXmlErrorHandler" "QXmlEntityResolver" "QXmlDeclHandler" "QXmlLexicalHandler")
				   "<qxml.h>")
;; XML DOM
				  (("QDomNode" "QDomAttr" "QDomCharacterDate" "QDomDocument" "QDomComment" "QDomText" "QDomDocumentFragment" "QDomDocumentType" "QDomElement" "QDomEntity" "QDomEntityReference" "QDomNotation" "QDomProcessingInstruction" "QDomCDATASection" "QDomNodeList" "QDomNamedNodeMap" "QDomImplementation") "<qdom.h>")
;; Thread
				  (("QThread" "QSemaphore" "QMutex" "QWaitCondition") "<qthread.h>")
;; Div
				  (("QAction" "QActionGroup") "<qaction.h>")
				  (("QGuardedPtr") "<qguardedptr.h>")
				  )
  "A list of classes connected to an include file known to project")


(defvar project-include-params    (list
				   (list (concat "^" (regexp-opt '("QWidget"
								   "QFrame"
								   "QButton"
								   "QCheckBox"
								   "QPushButton"
								   "QRadioButton"
								   "QToolButton"
								   "QGroupBox"
								   "QHBox"
								   "QLCDNumber"
								   "QLabel"
								   "QMenuBar"
								   "QPopupMenu"
								   "QProgressBar"
								   "QScrollView"
								   "QIconView"
								   "QListBox"
								   "QListView"
								   "QTextView"
								   "QTextBrowser"
								   "QSpinBox"
								   "QSplitter"
								   "QTableView"
								   "QMultiLineEdit"
								   "QWidgetStack"
								   "QHeader"
								   "QLineEdit"
								   "QScrollBar"
								   "QSizeGrip"
								   "QSlider"
								   "QStatusBar"
								   "QTabBar"
								   "QTabWidget"
								   "QComboBox"
								   "QWorkSpace")) "$")
					 "QWidget *parent = 0, const char *name = 0"
					 "QWidget *parent, const char *name"
					 "parent, name")
				   (list (concat "^" (regexp-opt '("QDialog"
								   "QSemiModal"
								   "QProgressDialog")) "$")
					 "QWidget *parent = 0, const char *name = 0, bool modal = FALSE, WFlags f = 0"
					 "QWidget *parent, const char *name, bool modal, WFlags f"
					 "parent, name, modal, f")
				   (list (concat "^" (regexp-opt '("QGrid")) "$")
					 "int n, Direction dir, QWidget *parent = 0, const char *name = 0, WFlags f = 0"
					 "int n, Direction dir, QWidget *parent, const char *name, WFlags f"
					 "n, dir, parent, name, f")
				   (list (concat "^" (regexp-opt '("QGLWidget")) "$")
					 "QWidget *parent = 0, const char *name = 0, const QGLWidget *shareWidget = 0, WFlags f = 0"
					 "QWidget *parent, const char *name, const QGLWidget *shareWidget, WFlags f"
					 "parent, name, shareWidget, f")
				   (list (concat "^" (regexp-opt '("QMainWindow")) "$")
					 "QWidget *parent = 0, const char *name = 0, WFlags f = WType_TopLevel"
					 "QWidget *parent, const char *name, WFlags f"
					 "parent, name, f")
				   (list (concat "^" (regexp-opt '("QToolBar")) "$")
					 "const QString &label, QMainWindow *, QMainWindow::ToolBarDock = QMainWindow::Top, bool newLine = false, const char *name = 0"
					 "const QString &label, QMainWindow *main, QMainWindow::ToolBarDock dock , bool newLine, const char *name"
					 "label, main, dock, newLine, name")
				   (list (concat "^" (regexp-opt '("QNPWidget")) "$")
					 ""
					 ""
					 "")
				   ))

;; This example shows how to add classes in a .emacs-classes file.

(defvar project-stl-classes    '( 
				 (("cout" "cin") "<iostream>")
				 (("ifstream" "ofstream" "fstream") "<fstream>")
				 (("auto_ptr") "memory")
				 (("string") "<string>")
				 ))

(project-add-include-list project-stl-classes)


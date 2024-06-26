(in-package :cs-cc)

(defconstant +space-mask+ #.(ash 1 0))
(defconstant +id1st-mask+ #.(ash 1 1))
(defconstant +idrest-mask+ #.(ash 1 2))
(defconstant +decimal-mask+ #.(ash 1 3))
(defconstant +octal-mask+ #.(ash 1 4))
(defconstant +hexa-mask+ #.(ash 1 5))
(defconstant +control-mask+ #.(ash 1 6))
(defconstant +dec1st-mask+ #.(ash 1 7))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type (simple-array (unsigned-byte 32) (128)) *char-class*))
  (defvar *char-class* (make-array 128 :element-type '(unsigned-byte 32))))

(loop :for i :from 0 :to 127
      :for c := (code-char i)
      :when (or (char= c #\Space) (char= c #\Tab) (char= c #\Vt)
		(char= c #\Page) #+nil(char= c #\Newline) (char= c #\Return))
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +space-mask+))
      :when (or (char= c #\_) (char-not-greaterp #\A c #\Z))
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +id1st-mask+))
      :when (or (char= c #\_) (char-not-greaterp #\A c #\Z)
		(char<= #\0 c #\9))
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +idrest-mask+))
      :when (char<= #\0 c #\9)
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +decimal-mask+))
      :when (char<= #\0 c #\7)
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +octal-mask+))
      :when (or (char-not-greaterp #\A c #\F) (char<= #\0 c #\9))
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +hexa-mask+))
      :when (< i #x20)
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +control-mask+))
      :when (char<= #\1 c #\9)
      :do (setf (aref *char-class* i)
		(logior (aref *char-class* i) +dec1st-mask+)))

(macrolet ((mkcc-test (name mask)
	     `(defun ,name (c)
		(declare (optimize (speed 3) (safety 0) (debug 0)))
		(declare (type character c))
		(let ((code (char-code c)))
		  (when (< code 128)
		    (/= 0 (logand (aref *char-class* code) ,mask)))))))
  (mkcc-test space? +space-mask+)
  (mkcc-test id1st? +id1st-mask+)
  (mkcc-test idrest? +idrest-mask+)
  (mkcc-test dec? +decimal-mask+)
  (mkcc-test oct? +octal-mask+)
  (mkcc-test hex? +hexa-mask+)
  (mkcc-test ctl? +control-mask+)
  (mkcc-test dec1st? +dec1st-mask+))

(in-package :alien-helper)

(defun strtod (string)
  (declare (type simple-string string))
  (with-alien ((endptr (* char)))
    (let ((ans (alien-funcall
		(extern-alien "strtod"
			      (function double-float c-string (* (* char))))
		string (addr endptr))))
      (values ans (deref endptr)))))

(defun strtof (string)
  (declare (type simple-string string))
  (with-alien ((endptr (* char)))
    (let ((ans (alien-funcall
		(extern-alien "strtof"
			      (function single-float c-string (* (* char))))
		string (addr endptr))))
      (values ans (deref endptr)))))


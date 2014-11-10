;; actual directory sequence not complete, eg no "05"
(setq dd "/data/ref/global/cru/CRUTEM.4.2.0.0.station_files")

(setq ds (directory-files dd  t "^[[:digit:]]+$"))

(setq flst 
      (apply #'append 
	     (mapcar (lambda (x) (directory-files x t "^[[:digit:]]+$")) ds)))

(defun compile-cru-metadata (flst ofile)
  "consolidate CRU met station metadata into CSV format"
  (find-file-noselect ofile)
  (with-current-buffer ofile 
    (insert "wmoid,name,co,lat,lon,z,y0,y1,fgy,sid\n"))
  (dolist (ifile flst)
    (find-file-other-window ifile)
    (dotimes (j 10)
      (search-forward "= ")
      (setq datum (buffer-substring (point) (point-at-eol)))
      (setq datum (apply 'concat (split-string datum ",")))  ;; excise commas and
      (setq datum (replace-regexp-in-string "-+$" "" datum)) ;; trailing hyphens
      (with-current-buffer ofile 
	(if (= j 0)
	    (if (not (equal ifile (car fs))) (insert "\n"))
	  (setq datum (concat "," datum)))
	(insert datum)))
    (kill-buffer (buffer-name)))
  (save-buffer (find-file-other-window ofile)))

(compile-cru-metadata flst "crutem-4.2-stations.csv")

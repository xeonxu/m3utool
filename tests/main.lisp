(defpackage :m3utool/tests/main
  (:use :cl :rove))

(in-package :m3utool/tests/main)

;;; Tests for m3u-data:transform-uri
(deftest test-transform-uri-no-transformation
  (testing "transform-uri without any transformations should return original URI"
    (ok (string= "http://example.com/stream.m3u"
                 (m3u-data:transform-uri "http://example.com/stream.m3u")))))

(deftest test-transform-uri-strip-proxy-udp
  (testing "transform-uri with strip-proxy should extract IP:Port from udp URL"
    (ok (string= "233.18.1.1:5000"
                 (m3u-data:transform-uri "http://proxy.example.com:8080/udp/233.18.1.1:5000"
                                         :strip-proxy t)))))

(deftest test-transform-uri-strip-proxy-rtp
  (testing "transform-uri with strip-proxy should extract IP:Port from rtp URL"
    (ok (string= "192.168.1.100:8000"
                 (m3u-data:transform-uri "http://proxy.example.com:8080/rtp/192.168.1.100:8000"
                                         :strip-proxy t)))))

(deftest test-transform-uri-server-url-only
  (testing "transform-uri with server-url should prepend server URL"
    (ok (string= "http://newserver.com/udp/233.18.1.1:5000"
                 (m3u-data:transform-uri "233.18.1.1:5000"
                                         :server-url "http://newserver.com/udp/")))))

(deftest test-transform-uri-strip-and-server
  (testing "transform-uri with both strip-proxy and server-url"
    (ok (string= "http://newserver.com/udp/233.18.1.1:5000"
                 (m3u-data:transform-uri "http://oldproxy.com:8080/udp/233.18.1.1:5000"
                                         :strip-proxy t
                                         :server-url "http://newserver.com/udp/")))))

(deftest test-transform-uri-no-match-strip
  (testing "transform-uri with strip-proxy on non-matching URL should return original"
    (ok (string= "http://example.com/stream.m3u"
                 (m3u-data:transform-uri "http://example.com/stream.m3u"
                                         :strip-proxy t)))))

(deftest test-transform-uri-empty-server-url
  (testing "transform-uri with empty server-url should not modify URI"
    (ok (string= "233.18.1.1:5000"
                 (m3u-data:transform-uri "233.18.1.1:5000"
                                         :server-url "")))))

;;; Tests for m3u-xlsx:save-xlsx and load-xlsx
(deftest test-save-and-load-xlsx
  (testing "save-xlsx and load-xlsx should preserve playlist items"
    (let* ((test-file (format nil "~a/test-playlist.xlsx" (uiop:temporary-directory)))
           (item1 (make-instance 'm3u-data::playlist-item
                                 :duration 120
                                 :title "Test Channel 1"
                                 :uri "http://example.com/stream1"))
           (item2 (make-instance 'm3u-data::playlist-item
                                 :duration 180
                                 :title "Test Channel 2"
                                 :uri "http://example.com/stream2"))
           (items (list item1 item2)))
      
      ;; Add some attributes to test attribute preservation
      (setf (gethash "tvg-id" (m3u-data::item-attributes item1)) "channel1")
      (setf (gethash "group-title" (m3u-data::item-attributes item1)) "Sports")
      (setf (gethash "tvg-id" (m3u-data::item-attributes item2)) "channel2")
      
      ;; Save to XLSX
      (m3u-xlsx:save-xlsx items test-file)
      
      ;; Load from XLSX
      (let ((loaded-items (m3u-xlsx:load-xlsx test-file)))
        ;; Verify we loaded 2 items
        (ok (= 2 (length loaded-items)))
        
        ;; Verify first item
        (let ((loaded-item1 (first loaded-items)))
          (ok (= 120 (m3u-data::item-duration loaded-item1)))
          (ok (string= "Test Channel 1" (m3u-data::item-title loaded-item1)))
          (ok (string= "http://example.com/stream1" (m3u-data::item-uri loaded-item1))))
        
        ;; Verify second item
        (let ((loaded-item2 (second loaded-items)))
          (ok (= 180 (m3u-data::item-duration loaded-item2)))
          (ok (string= "Test Channel 2" (m3u-data::item-title loaded-item2)))
          (ok (string= "http://example.com/stream2" (m3u-data::item-uri loaded-item2)))))
      
      ;; Clean up
      (when (probe-file test-file)
        (delete-file test-file)))))

(deftest test-save-xlsx-with-uri-transformation
  (testing "save-xlsx with strip-proxy should transform URIs in output"
    (let* ((test-file (format nil "~a/test-transform.xlsx" (uiop:temporary-directory)))
           (item (make-instance 'm3u-data::playlist-item
                                :duration 60
                                :title "Test Channel"
                                :uri "http://proxy.com:8080/udp/233.18.1.1:5000"))
           (items (list item)))
      
      ;; Save with strip-proxy and server-url
      (m3u-xlsx:save-xlsx items test-file
                          :strip-proxy t
                          :server-url "http://newserver.com/udp/")
      
      ;; Load and verify transformation
      (let* ((loaded-items (m3u-xlsx:load-xlsx test-file))
             (loaded-item (first loaded-items)))
        (ok (string= "http://newserver.com/udp/233.18.1.1:5000"
                     (m3u-data::item-uri loaded-item))))
      
      ;; Clean up
      (when (probe-file test-file)
        (delete-file test-file)))))

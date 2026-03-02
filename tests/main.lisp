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
    (ok (string= "udp://233.18.1.1:5000"
                 (m3u-data:transform-uri "http://proxy.example.com:8080/udp/233.18.1.1:5000"
                                         :strip-proxy t)))))

(deftest test-transform-uri-strip-proxy-rtp
  (testing "transform-uri with strip-proxy should extract IP:Port from rtp URL"
    (ok (string= "rtp://192.168.1.100:8000"
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

;; --- Append this to your existing tests/main.lisp ---

;; Note: You might need to add m3u-check to your defpackage :use list at the top of the file.
;; Example: (:use :cl :rove :m3u-data :m3u-check)

(deftest test-m3u-check
  (testing "Validating HTTP URL liveliness"
    ;; We use httpbin.org for reliable HTTP response testing.
    ;; Note: This requires an active internet connection to pass.
    (let ((alive-url "https://httpbin.org/status/200")
          (dead-url "https://httpbin.org/status/404")
          (timeout 5))
      (ok (eq t (m3u-check:check-uri-alive-p alive-url timeout)) "Should return T for 200 OK")
      (ok (eq nil (m3u-check:check-uri-alive-p dead-url timeout)) "Should return NIL for 404 Not Found")))

  (testing "Validating non-HTTP URL bypassing"
    (let ((rtp-url "rtp://239.1.1.1:5000")
          (timeout 3))
      ;; Non-HTTP streams should be bypassed and assumed alive by default
      (ok (eq t (m3u-check:check-uri-alive-p rtp-url timeout)) "Should return T for non-HTTP protocols")))

  (testing "Concurrent filtering logic"
    (let* ((item1 (make-instance 'm3u-data::playlist-item :uri "https://httpbin.org/status/200"))
           (item2 (make-instance 'm3u-data::playlist-item :uri "https://httpbin.org/status/404"))
           (item3 (make-instance 'm3u-data::playlist-item :uri "rtp://239.1.1.2:5000"))
           (items (list item1 item2 item3))
           ;; Spin up a 2-worker pool to test concurrency
           (alive-items (m3u-check:filter-alive-items items :threads 2 :timeout 5)))

      (ok (= (length alive-items) 2) "Should filter out exactly one dead link")
      (ok (string= (m3u-data::item-uri (first alive-items)) "https://httpbin.org/status/200") "First alive should be the 200 URL")
      (ok (string= (m3u-data::item-uri (second alive-items)) "rtp://239.1.1.2:5000") "Second alive should be the bypassed RTP URL"))))

;; ==========================================
;; Test Suite for m3u-data:merge-items
;; ==========================================
(deftest test-merge-items
  (testing "Incremental merging: appending new channels, alternative URIs, and ignoring duplicates"
    (let* (;; Construct existing database (contains only CCTV-1)
           (old-items (list (make-instance 'm3u-data::playlist-item
                                           :title "CCTV-1"
                                           :uri "http://old.com/1.ts"
                                           :attributes (make-hash-table :test 'equal))))
           ;; Construct newly scanned data
           (new-items (list
                       ;; 1. Completely identical record (same title and URI), should be ignored
                       (make-instance 'm3u-data::playlist-item
                                      :title "CCTV-1"
                                      :uri "http://old.com/1.ts"
                                      :attributes (make-hash-table :test 'equal))
                       ;; 2. Same channel name, but new source. Should be appended as uri-2
                       (make-instance 'm3u-data::playlist-item
                                      :title "CCTV-1"
                                      :uri "http://new.com/1.ts"
                                      :attributes (make-hash-table :test 'equal))
                       ;; 3. Completely new channel, should be appended to the end of the list
                       (make-instance 'm3u-data::playlist-item
                                      :title "CCTV-2"
                                      :uri "http://new.com/2.ts"
                                      :attributes (make-hash-table :test 'equal))))
           ;; Execute merge
           (merged (m3u-data:merge-items old-items new-items)))

      ;; Assertion 1: Total items should be 2 (CCTV-1 and CCTV-2)
      (ok (= (length merged) 2) "Merge should result in exactly 2 distinct channels")

      (let ((cctv1 (first merged))
            (cctv2 (second merged)))

        ;; Assertion 2: The primary URI of CCTV-1 must remain unchanged
        (ok (string= (m3u-data:item-uri cctv1) "http://old.com/1.ts") "Primary URI of existing channel should not be altered")

        ;; Assertion 3: CCTV-1 should contain uri-2, matching the new scanned source
        (ok (string= (gethash "uri-2" (m3u-data:item-attributes cctv1)) "http://new.com/1.ts") "New URI should be appended as uri-2")

        ;; Assertion 4: The completely identical source should be ignored, so uri-3 should be empty
        (ok (null (gethash "uri-3" (m3u-data:item-attributes cctv1))) "Identical sources should be ignored, uri-3 must be empty")

        ;; Assertion 5: The new channel CCTV-2 must be appended correctly
        (ok (string= (m3u-data:item-title cctv2) "CCTV-2") "New channel should be appended at the end")
        (ok (string= (m3u-data:item-uri cctv2) "http://new.com/2.ts") "New channel URI should be correct")))))

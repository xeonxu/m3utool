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

;; --- Append this to your existing tests/main.lisp ---

;; Note: You might need to add m3u-check to your defpackage :use list at the top of the file.
;; Example: (:use :cl :rove :m3u-data :m3u-check)

(deftest test-m3u-check
  (testing "Validating HTTP URL liveliness with stream check"
    ;; httpbin.org provides reliable HTTP testing endpoints.
    ;; Since the raw-socket check ignores HTTP status codes and judges
    ;; liveness by body data, 200=alive(T), 404 with Content-Length:0=dead(NIL).
    (let ((alive-url "http://httpbin.org/status/200")
          (dead-url "http://127.0.0.1:19999/stream")
          (timeout 5))
      (ok (eq t (m3u-check:check-uri-alive-p alive-url timeout
                  :stream-check-p t))
          "Should return T for 200 OK (has body data)")
      ;; Connection refused -> no data can be read -> DEAD
      (ok (eq nil (m3u-check:check-uri-alive-p dead-url timeout
                   :stream-check-p t))
          "Should return NIL for connection refused (no data)")))

  (testing "Validating non-HTTP URL bypassing"
    (let ((rtp-url "rtp://239.1.1.1:5000")
          (timeout 3))
      ;; Non-HTTP streams should be bypassed and assumed alive by default
      (ok (eq t (m3u-check:check-uri-alive-p rtp-url timeout)) "Should return T for non-HTTP protocols")))

  (testing "Concurrent filtering logic"
    (let* ((item1 (make-instance 'm3u-data::playlist-item :uri "http://httpbin.org/status/200"))
           (item2 (make-instance 'm3u-data::playlist-item :uri "http://127.0.0.1:19999/stream"))
           (item3 (make-instance 'm3u-data::playlist-item :uri "rtp://239.1.1.2:5000"))
           (items (list item1 item2 item3))
           ;; Spin up a 2-worker pool to test concurrency
           (alive-items (m3u-check:filter-alive-items items :threads 2 :timeout 5)))

      (ok (= (length alive-items) 2) "Should filter out exactly one dead link")
      (ok (string= (m3u-data::item-uri (first alive-items)) "http://httpbin.org/status/200") "First alive should be the 200 URL")
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

;;; Tests for pure I/O in m3u-xlsx (Decoupled from transform-uri)
(deftest test-write-and-read-xlsx-items
  (testing "write-xlsx-items and read-xlsx-items should preserve playlist items and all dynamic attributes perfectly"
    (let* ((test-file (format nil "~a/test-pure-io.xlsx" (uiop:temporary-directory)))
           (item1 (make-instance 'm3u-data::playlist-item
                                 :duration 120
                                 :title "Test Channel 1"
                                 :uri "http://example.com/stream1"))
           (item2 (make-instance 'm3u-data::playlist-item
                                 :duration 180
                                 :title "Test Channel 2"
                                 :uri "http://example.com/stream2"))
           (items (list item1 item2)))

      ;; Add diverse attributes (including the newly designed uri-2)
      (setf (gethash "tvg-id" (m3u-data::item-attributes item1)) "channel1")
      (setf (gethash "uri-2" (m3u-data::item-attributes item1)) "http://backup.com/stream1")
      (setf (gethash "group-title" (m3u-data::item-attributes item2)) "Sports")

      ;; Save to XLSX (Pure I/O)
      (m3u-xlsx:write-xlsx-items items test-file)

      ;; Load from XLSX
      (let ((loaded-items (m3u-xlsx:read-xlsx-items test-file)))
        (ok (= 2 (length loaded-items)) "Should successfully load exactly 2 items")

        ;; Verify item 1
        (let ((loaded-item1 (first loaded-items)))
          (ok (= 120 (m3u-data::item-duration loaded-item1)))
          (ok (string= "Test Channel 1" (m3u-data::item-title loaded-item1)))
          (ok (string= "http://example.com/stream1" (m3u-data::item-uri loaded-item1)))
          (ok (string= "channel1" (gethash "tvg-id" (m3u-data::item-attributes loaded-item1))))
          (ok (string= "http://backup.com/stream1" (gethash "uri-2" (m3u-data::item-attributes loaded-item1)))))

        ;; Verify item 2
        (let ((loaded-item2 (second loaded-items)))
          (ok (= 180 (m3u-data::item-duration loaded-item2)))
          (ok (string= "Test Channel 2" (m3u-data::item-title loaded-item2)))
          (ok (string= "http://example.com/stream2" (m3u-data::item-uri loaded-item2)))
          (ok (string= "Sports" (gethash "group-title" (m3u-data::item-attributes loaded-item2))))))

      Clean up
      (when (probe-file test-file)
        (delete-file test-file)))))


;;; ==========================================
;;; Tests for P0: Multi-Sheet XLSX Support
;;; ==========================================

(deftest test-multi-sheet-write-and-read
  (testing "write-xlsx-items with named sheet and read-xlsx-items from that sheet"
    (let* ((test-file (format nil "~a/test-multi-sheet.xlsx" (uiop:temporary-directory)))
           (sheet-name "TestSheet")
           (item1 (make-instance 'm3u-data::playlist-item
                                 :duration 60
                                 :title "Sheet Test Channel"
                                 :uri "http://example.com/sheet-test"))
           (items (list item1)))

      (setf (gethash "tvg-id" (m3u-data::item-attributes item1)) "sheet-chan-1")
      (setf (gethash "group-title" (m3u-data::item-attributes item1)) "Testing")

      (m3u-xlsx:write-xlsx-items items test-file :sheet-name sheet-name)

      (let ((loaded (m3u-xlsx:read-xlsx-items test-file :sheet-name sheet-name)))
        (ok (= 1 (length loaded)) "Should load exactly 1 item from named sheet")
        (let ((li (first loaded)))
          (ok (string= "Sheet Test Channel" (m3u-data::item-title li))
              "Title should be preserved")
          (ok (string= "http://example.com/sheet-test" (m3u-data::item-uri li))
              "URI should be preserved")
          (ok (string= "sheet-chan-1" (gethash "tvg-id" (m3u-data::item-attributes li)))
              "tvg-id attribute should be preserved")
          (ok (string= "Testing" (gethash "group-title" (m3u-data::item-attributes li)))
              "group-title attribute should be preserved")))

      (when (probe-file test-file)
        (delete-file test-file))))

(deftest test-multi-sheet-backward-compatibility
  (testing "read-xlsx-items and write-xlsx-items without sheet-name should use defaults"
    (let* ((test-file (format nil "~a/test-default-sheet.xlsx" (uiop:temporary-directory)))
           (item1 (make-instance 'm3u-data::playlist-item
                                 :duration 0
                                 :title "Default Sheet Item"
                                 :uri "http://default.com/stream"))
           (items (list item1)))

      (m3u-xlsx:write-xlsx-items items test-file)

      (let ((loaded (m3u-xlsx:read-xlsx-items test-file)))
        (ok (= 1 (length loaded)) "Should load exactly 1 item from default sheet")
        (ok (string= "Default Sheet Item" (m3u-data::item-title (first loaded)))
            "Default sheet read should work")))

      (when (probe-file test-file)
        (delete-file test-file))))

(deftest test-list-sheet-names
  (testing "list-sheet-names should return all sheet names in the XLSX"
    (let* ((test-file (format nil "~a/test-list-sheets.xlsx" (uiop:temporary-directory)))
           (items (list (make-instance 'm3u-data::playlist-item
                                       :title "Sheet1Item"
                                       :uri "http://s1.com"))))

      (m3u-xlsx:write-xlsx-items items test-file :sheet-name "Beijing")
      (m3u-xlsx:write-xlsx-items items test-file :sheet-name "Shanghai")

      (let ((sheet-names (m3u-xlsx:list-sheet-names test-file)))
        (ok (>= (length sheet-names) 1) "Should have at least 1 sheet")
        (ok (member "Beijing" sheet-names :test #'string=)
            "Should contain Beijing sheet")
        (ok (member "Shanghai" sheet-names :test #'string=)
            "Should contain Shanghai sheet")))

      (when (probe-file test-file)
        (delete-file test-file))))

;;; ==========================================
;;; Tests for P1: IP Range Parsing and Scan
;;; ==========================================

(deftest test-ip-string-to-integer
  (testing "ip-string-to-integer should correctly convert IP addresses"
    (ok (= #xEF010101 (m3u-scan:ip-string-to-integer "239.1.1.1"))
        "239.1.1.1 should convert to correct integer")
    (ok (= #xC0A80101 (m3u-scan:ip-string-to-integer "192.168.1.1"))
        "192.168.1.1 should convert to correct integer")
    (ok (= 0 (m3u-scan:ip-string-to-integer "0.0.0.0"))
        "0.0.0.0 should be 0")
    (ok (= #xFFFFFFFF (m3u-scan:ip-string-to-integer "255.255.255.255"))
        "255.255.255.255 should be max 32-bit integer")))

(deftest test-ip-range-parsing-cidr
  (testing "parse-ip-range with CIDR notation"
    (multiple-value-bind (start end count) (m3u-scan:parse-ip-range "239.1.1.0/24")
      (ok (= start (m3u-scan:ip-string-to-integer "239.1.1.0")) "Network start should be correct")
      (ok (= end (m3u-scan:ip-string-to-integer "239.1.1.255")) "Broadcast end should be correct")
      (ok (= count 256) "/24 should contain 256 addresses"))))

(deftest test-ip-range-parsing-range
  (testing "parse-ip-range with explicit range notation"
    (multiple-value-bind (start end count) (m3u-scan:parse-ip-range "239.1.1.1-239.1.1.10")
      (ok (= start (m3u-scan:ip-string-to-integer "239.1.1.1")) "Range start should be correct")
      (ok (= end (m3u-scan:ip-string-to-integer "239.1.1.10")) "Range end should be correct")
      (ok (= count 10) "Range should contain 10 addresses"))))

(deftest test-ip-range-parsing-single
  (testing "parse-ip-range with single IP"
    (multiple-value-bind (start end count) (m3u-scan:parse-ip-range "239.1.1.100")
      (ok (= start end) "Single IP start and end should be equal")
      (ok (= count 1) "Single IP should have count 1"))))

(deftest test-generate-ip-list
  (testing "generate-ip-list should produce correct list of IP strings"
    (let ((ips (m3u-scan:generate-ip-list "239.1.1.1-239.1.1.3")))
      (ok (= 3 (length ips)) "Should generate exactly 3 IPs")
      (ok (string= "239.1.1.1" (first ips)) "First IP should match")
      (ok (string= "239.1.1.2" (second ips)) "Second IP should match")
      (ok (string= "239.1.1.3" (third ips)) "Third IP should match"))))

(deftest test-port-range-parsing
  (testing "parse-port-range with various formats"
    (let ((ports (m3u-scan:parse-port-range "5000")))
      (ok (= 1 (length ports)) "Single port should return list of 1")
      (ok (= 5000 (first ports)) "Port should be 5000"))
    (let ((ports (m3u-scan:parse-port-range "5000-5003")))
      (ok (= 4 (length ports)) "Range 5000-5003 should have 4 ports")
      (ok (equal '(5000 5001 5002 5003) ports) "Ports should be sequential"))
    (let ((ports (m3u-scan:parse-port-range "5000,5002,5008")))
      (ok (= 3 (length ports)) "Comma-separated should have 3 ports")
      (ok (member 5000 ports) "Should contain 5000")
      (ok (member 5002 ports) "Should contain 5002")
      (ok (member 5008 ports) "Should contain 5008"))
    (let ((ports (m3u-scan:parse-port-range "5000-5001,8000")))
      (ok (= 3 (length ports)) "Mixed format should produce 3 ports")
      (ok (member 5000 ports) "Should contain 5000")
      (ok (member 5001 ports) "Should contain 5001")
      (ok (member 8000 ports) "Should contain 8000"))))

(deftest test-generate-scan-uris
  (testing "generate-scan-uris should produce correct URI combinations"
    (let ((uris (m3u-scan:generate-scan-uris "239.1.1.1-239.1.1.2" "5000" :proto "rtp")))
      (ok (= 2 (length uris)) "Should generate 2 URIs (2 IPs x 1 port)")
      (ok (string= "rtp://239.1.1.1:5000" (first uris)) "First URI should match")
      (ok (string= "rtp://239.1.1.2:5000" (second uris)) "Second URI should match"))
    (let ((uris (m3u-scan:generate-scan-uris "239.1.1.1" "5000-5001" :proto "udp")))
      (ok (= 2 (length uris)) "Should generate 2 URIs (1 IP x 2 ports)")
      (ok (string= "udp://239.1.1.1:5000" (first uris)) "First port should match")
      (ok (string= "udp://239.1.1.1:5001" (second uris)) "Second port should match"))
    (let ((uris (m3u-scan:generate-scan-uris "239.1.1.1" "5000"
                                              :template "http://192.168.1.1:8080/udp/~a:~a")))
      (ok (= 1 (length uris)) "Custom template should produce 1 URI")
      (ok (string= "http://192.168.1.1:8080/udp/239.1.1.1:5000" (first uris))
          "Custom template should generate correct URI")))))

(deftest test-generate-scan-uris-with-proxy
  (testing "generate-scan-uris with proxy-url should append IP:port to proxy base URL"
    (let ((uris (m3u-scan:generate-scan-uris "239.1.1.1-239.1.1.2" "5000"
                                              :proxy-url "http://192.168.0.5:7088/udp/")))
      (ok (= 2 (length uris)) "Should generate 2 URIs (2 IPs x 1 port)")
      (ok (string= "http://192.168.0.5:7088/udp/239.1.1.1:5000" (first uris))
          "First URI should use proxy URL")
      (ok (string= "http://192.168.0.5:7088/udp/239.1.1.2:5000" (second uris))
          "Second URI should use proxy URL"))
    ;; Proxy with trailing slash already included
    (let ((uris (m3u-scan:generate-scan-uris "239.1.1.1" "5000"
                                              :proxy-url "http://10.0.0.1:8080/rtp/")))
      (ok (= 1 (length uris)) "Single IP with proxy should generate 1 URI")
      (ok (string= "http://10.0.0.1:8080/rtp/239.1.1.1:5000" (first uris))
          "Proxy URL should be prepended correctly"))))



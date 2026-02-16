(defpackage m3utool/tests/main
  (:use :cl :rove))

(in-package :m3utool/tests/main)

;;; Test M3U Parsing and Item Creation
(deftest test-playlist-item-creation
  (testing "Create a playlist item with basic properties"
    (let ((item (make-instance 'm3u-data::playlist-item
                               :duration -1
                               :title "Test Channel"
                               :uri "http://example.com/stream")))
      (ok (= -1 (m3u-data::item-duration item)))
      (ok (string= "Test Channel" (m3u-data::item-title item)))
      (ok (string= "http://example.com/stream" (m3u-data::item-uri item))))))

(deftest test-parse-extinf-line
  (testing "Parse EXTINF line with attributes"
    (let ((item (make-instance 'm3u-data::playlist-item))
          (line "#EXTINF:-1 tvg-id=\"100\" tvg-name=\"Test\" group-title=\"Test Group\",Test Channel"))
      (m3u-data::parse-extinf-line line item)
      (ok (= -1 (m3u-data::item-duration item)))
      (ok (string= "Test Channel" (m3u-data::item-title item)))
      (ok (string= "100" (gethash "tvg-id" (m3u-data::item-attributes item))))
      (ok (string= "Test" (gethash "tvg-name" (m3u-data::item-attributes item))))
      (ok (string= "Test Group" (gethash "group-title" (m3u-data::item-attributes item)))))))

(deftest test-parse-extinf-unquoted-attributes
  (testing "Parse EXTINF line with unquoted attributes"
    (let ((item (make-instance 'm3u-data::playlist-item))
          (line "#EXTINF:-1 tvg-id=200 group-title=\"Mixed\" resolution=1920x1080,Channel Name"))
      (m3u-data::parse-extinf-line line item)
      (ok (string= "200" (gethash "tvg-id" (m3u-data::item-attributes item))))
      (ok (string= "Mixed" (gethash "group-title" (m3u-data::item-attributes item))))
      (ok (string= "1920x1080" (gethash "resolution" (m3u-data::item-attributes item)))))))

;;; Test URI Transformation
(deftest test-transform-uri-strip-proxy
  (testing "Strip proxy from URI"
    (let ((original "http://proxy.example.com:8080/udp/233.18.1.1:5000")
          (expected "233.18.1.1:5000"))
      (ok (string= expected 
                   (m3u-data:transform-uri original :strip-proxy t))))))

(deftest test-transform-uri-add-server
  (testing "Add server URL to URI"
    (let ((original "233.18.1.1:5000")
          (server "http://newserver.com/udp/")
          (expected "http://newserver.com/udp/233.18.1.1:5000"))
      (ok (string= expected 
                   (m3u-data:transform-uri original :server-url server))))))

(deftest test-transform-uri-strip-and-add
  (testing "Strip proxy and add new server URL"
    (let ((original "http://oldproxy.com:8080/udp/233.18.1.1:5000")
          (server "http://newserver.com/rtp/")
          (expected "http://newserver.com/rtp/233.18.1.1:5000"))
      (ok (string= expected 
                   (m3u-data:transform-uri original 
                                           :strip-proxy t 
                                           :server-url server))))))

(deftest test-transform-uri-no-change
  (testing "URI transformation with no options should return original"
    (let ((original "http://example.com/stream"))
      (ok (string= original 
                   (m3u-data:transform-uri original))))))

(deftest test-transform-uri-rtp-protocol
  (testing "Strip proxy with RTP protocol"
    (let ((original "http://proxy.example.com:8080/rtp/192.168.1.100:6000")
          (expected "192.168.1.100:6000"))
      (ok (string= expected 
                   (m3u-data:transform-uri original :strip-proxy t))))))

;;; Test Collection Functions
(deftest test-collect-all-attribute-keys
  (testing "Collect unique attribute keys from multiple items"
    (let ((item1 (make-instance 'm3u-data::playlist-item))
          (item2 (make-instance 'm3u-data::playlist-item)))
      (setf (gethash "tvg-id" (m3u-data::item-attributes item1)) "1")
      (setf (gethash "tvg-name" (m3u-data::item-attributes item1)) "Test1")
      (setf (gethash "tvg-id" (m3u-data::item-attributes item2)) "2")
      (setf (gethash "group-title" (m3u-data::item-attributes item2)) "Group1")
      (let ((keys (m3u-data::collect-all-attribute-keys (list item1 item2))))
        (ok (= 3 (length keys)))
        (ok (member "tvg-id" keys :test #'string=))
        (ok (member "tvg-name" keys :test #'string=))
        (ok (member "group-title" keys :test #'string=))))))

;;; Test CSV to Item Conversion
(deftest test-flexible-csv-row-to-item
  (testing "Convert CSV row to playlist item"
    (let* ((headers '("Duration" "Title" "URI" "tvg-id" "group-title"))
           (row '("-1" "Test Channel" "http://example.com/stream" "100" "TestGroup"))
           (item (m3u-data::flexible-csv-row-to-item row headers)))
      (ok (= -1 (m3u-data::item-duration item)))
      (ok (string= "Test Channel" (m3u-data::item-title item)))
      (ok (string= "http://example.com/stream" (m3u-data::item-uri item)))
      (ok (string= "100" (gethash "tvg-id" (m3u-data::item-attributes item))))
      (ok (string= "TestGroup" (gethash "group-title" (m3u-data::item-attributes item)))))))

(deftest test-flexible-csv-row-to-item-case-insensitive
  (testing "CSV headers should be case insensitive"
    (let* ((headers '("DURATION" "TITLE" "URI"))
           (row '("-1" "Channel" "http://test.com"))
           (item (m3u-data::flexible-csv-row-to-item row headers)))
      (ok (= -1 (m3u-data::item-duration item)))
      (ok (string= "Channel" (m3u-data::item-title item)))
      (ok (string= "http://test.com" (m3u-data::item-uri item))))))

;;; Test M3U File I/O
(deftest test-parse-and-save-m3u
  (testing "Parse M3U file and save it back"
    (let* ((test-m3u-content "#EXTM3U
#EXTINF:-1 tvg-id=\"1\" group-title=\"Test\",Test Channel
http://example.com/stream1
#EXTINF:-1 tvg-id=\"2\" group-title=\"Test\",Another Channel
http://example.com/stream2
")
           (input-file "/tmp/test-input.m3u")
           (output-file "/tmp/test-output.m3u"))
      ;; Create test input file
      (with-open-file (stream input-file 
                             :direction :output 
                             :if-exists :supersede
                             :external-format :utf-8)
        (write-string test-m3u-content stream))
      
      ;; Parse and save
      (let ((items (m3u-data::parse-m3u-file input-file)))
        (ok (= 2 (length items)))
        (m3u-data::save-items-to-m3u items output-file)
        
        ;; Verify output file was created
        (ok (probe-file output-file))
        
        ;; Parse the output to verify it matches
        (let ((output-items (m3u-data::parse-m3u-file output-file)))
          (ok (= 2 (length output-items)))
          (ok (string= "Test Channel" (m3u-data::item-title (first output-items))))
          (ok (string= "http://example.com/stream1" (m3u-data::item-uri (first output-items))))))
      
      ;; Cleanup
      (when (probe-file input-file) (delete-file input-file))
      (when (probe-file output-file) (delete-file output-file)))))

;;; Test Edge Cases
(deftest test-empty-attributes
  (testing "Handle items with no attributes"
    (let ((item (make-instance 'm3u-data::playlist-item
                               :duration 0
                               :title "Simple"
                               :uri "http://simple.com")))
      (ok (= 0 (hash-table-count (m3u-data::item-attributes item)))))))

(deftest test-special-characters-in-title
  (testing "Handle special characters in title"
    (let ((item (make-instance 'm3u-data::playlist-item))
          (line "#EXTINF:-1,中文频道「HD」"))
      (m3u-data::parse-extinf-line line item)
      (ok (string= "中文频道「HD」" (m3u-data::item-title item))))))

(deftest test-attribute-key-normalization
  (testing "Attribute keys should be normalized to lowercase"
    (let ((item (make-instance 'm3u-data::playlist-item))
          (line "#EXTINF:-1 TVG-ID=\"100\" Group-Title=\"Test\",Channel"))
      (m3u-data::parse-extinf-line line item)
      (ok (string= "100" (gethash "tvg-id" (m3u-data::item-attributes item))))
      (ok (string= "Test" (gethash "group-title" (m3u-data::item-attributes item)))))))

(defpackage :m3u-server
  (:use :cl)
  (:export :start-server))

(in-package :m3u-server)

(defun items-to-m3u-string (items)
  "Convert a list of playlist-item objects to an M3U format string in memory."
  (with-output-to-string (stream)
    (format stream "#EXTM3U~%")
    (dolist (item items)
      (let ((attr-str ""))
        ;; Reconstruct the attributes string
        (maphash (lambda (k v)
                   (when (and k v)
                     (setf attr-str (concatenate 'string attr-str (format nil " ~a=\"~a\"" k v)))))
                 (m3u-data:item-attributes item))
        ;; Write the EXTINF line and the URI
        (format stream "#EXTINF:~d~a,~a~%" 
                (m3u-data:item-duration item) 
                attr-str 
                (m3u-data:item-title item))
        (format stream "~a~%" (m3u-data:item-uri item))))))

(defun generate-web-player-html (items)
  "Generate a modern HTML UI with an embedded video player and playlist."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>M3U Web Player</title>
    <script src='https://cdn.jsdelivr.net/npm/hls.js@latest'></script>
    <script src='https://cdn.jsdelivr.net/npm/mpegts.js@latest/dist/mpegts.min.js'></script>
    <style>
        body { margin: 0; padding: 0; background: #121212; color: #fff; font-family: sans-serif; display: flex; height: 100vh; overflow: hidden; }
        #player-container { flex: 3; display: flex; flex-direction: column; justify-content: center; align-items: center; background: #000; padding: 20px; }
        video { width: 100%; max-height: 80vh; background: #000; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.5); }
        #now-playing { margin-top: 15px; font-size: 1.2rem; color: #00ff88; font-weight: bold; }
        #playlist-container { flex: 1; background: #1e1e1e; border-left: 1px solid #333; display: flex; flex-direction: column; }
        .header { padding: 15px; background: #252525; font-size: 1.1rem; font-weight: bold; border-bottom: 1px solid #333; }
        #channel-list { flex: 1; overflow-y: auto; padding: 0; margin: 0; list-style: none; }
        .channel-item { padding: 12px 15px; border-bottom: 1px solid #2a2a2a; cursor: pointer; transition: background 0.2s; display: flex; flex-direction: column; }
        .channel-item:hover { background: #2a2d35; }
        .channel-title { font-size: 1rem; margin-bottom: 4px; }
        .channel-group { font-size: 0.8rem; color: #888; }
    </style>
</head>
<body>
    <div id='player-container'>
        <video id='video-element' controls autoplay></video>
        <div id='now-playing'>Select a channel to play...</div>
    </div>
    <div id='playlist-container'>
        <div class='header'>Channel List (~a)</div>
        <ul id='channel-list'>~%" (length items))
    
    ;; Render playlist items
    (dolist (item items)
      (let* ((title (m3u-data:item-title item))
             (uri (m3u-data:item-uri item))
             (group (gethash "group-title" (m3u-data:item-attributes item) "Uncategorized"))
             ;; Safely escape quotes in the URI to prevent JavaScript injection errors
             (safe-uri (str:replace-all "'" "\\'" uri)))
        (format s "<li class='channel-item' onclick=\"playStream('~a', '~a')\">
                      <span class='channel-title'>~a</span>
                      <span class='channel-group'>~a</span>
                   </li>~%" safe-uri title title group)))

    (format s "        </ul>
    </div>
    <script>
        var videoElement = document.getElementById('video-element');
        var mpegtsPlayer = null;

        function playStream(url, title) {
            document.getElementById('now-playing').innerText = 'Playing: ' + title;
            
            // Alert on native UDP/RTP browser limitations
            if(url.startsWith('udp://') || url.startsWith('rtp://')) {
                alert('Web Browsers cannot play raw UDP/RTP multicast streams natively. Please use a proxy server or VLC.');
                return;
            }

            // Destroy the old mpegts player instance if it exists
            if (mpegtsPlayer != null) {
                mpegtsPlayer.destroy();
                mpegtsPlayer = null;
            }

            // 1. Try HLS (.m3u8) first
            if (url.includes('.m3u8')) {
                if (Hls.isSupported()) {
                    var hls = new Hls();
                    hls.loadSource(url);
                    hls.attachMedia(videoElement);
                    hls.on(Hls.Events.MANIFEST_PARSED, function() { videoElement.play(); });
                } else if (videoElement.canPlayType('application/vnd.apple.mpegurl')) {
                    videoElement.src = url;
                    videoElement.play();
                }
            } 
            // 2. Try MPEG-TS (e.g., proxied HTTP-UDP stream or raw .ts file)
            else if (mpegts.getFeatureList().mseLivePlayback) {
                mpegtsPlayer = mpegts.createPlayer({
                    type: 'mse',  // mse / mpegts
                    isLive: true,
                    url: url
                });
                mpegtsPlayer.attachMediaElement(videoElement);
                mpegtsPlayer.load();
                mpegtsPlayer.play();
            } 
            // 3. Native fallback for other formats (mp4, webm, etc.)
            else {
                videoElement.src = url;
                videoElement.play();
            }
        }
    </script>
</body>
</html>")))

(defun start-server (input-xlsx bind-address port server-url strip-proxy)
  "Start the Hunchentoot HTTP server and serve the dynamic playlist and Web UI."
  (format t "Starting M3U server on ~a:~d...~%" bind-address port)
  (format t "Serving database: ~a~%" input-xlsx)
  
  ;; --- Route 1: Raw M3U file download endpoint ---
  (hunchentoot:define-easy-handler (playlist :uri "/playlist.m3u") ()
    (setf (hunchentoot:content-type*) "audio/x-mpegurl; charset=utf-8")
    (handler-case
        (let ((items (m3u-xlsx:read-xlsx-items input-xlsx)))
          (dolist (item items)
            (setf (m3u-data:item-uri item)
                  (m3u-data:transform-uri (m3u-data:item-uri item)
                                          :strip-proxy strip-proxy
                                          :server-url server-url)))
          (items-to-m3u-string items))
      (error (e)
        (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
        (format nil "Error generating playlist: ~a" e))))

  ;; --- Route 2: New Web UI player endpoint ---
  (hunchentoot:define-easy-handler (web-ui :uri "/") ()
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
    (handler-case
        (let ((items (m3u-xlsx:read-xlsx-items input-xlsx)))
          ;; Apply URI transformations dynamically so the browser gets the playable HTTP proxy links
          (dolist (item items)
            (setf (m3u-data:item-uri item)
                  (m3u-data:transform-uri (m3u-data:item-uri item)
                                          :strip-proxy strip-proxy
                                          :server-url server-url)))
          (generate-web-player-html items))
      (error (e)
        (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
        (format nil "<h1>Error reading database: ~a</h1>" e))))
  
  ;; Initialize and start the server
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor 
                                 :address bind-address 
                                 :port port)))
    (hunchentoot:start acceptor)
    (format t "~%[SUCCESS] Server is running!~%")
    (format t "--> Web Player: http://~a:~d/~%" (if (string= bind-address "0.0.0.0") "127.0.0.1" bind-address) port)
    (format t "--> Raw Playlist: http://~a:~d/playlist.m3u~%" (if (string= bind-address "0.0.0.0") "127.0.0.1" bind-address) port)
    (format t "Press Ctrl+C to stop the server.~%")
    
    (handler-case
        (loop (sleep 60))
      #+sbcl (sb-sys:interactive-interrupt () 
               (format t "~%Shutting down server...~%")
               (hunchentoot:stop acceptor)
               (uiop:quit 0)))))


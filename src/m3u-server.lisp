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
  "Generate a modern HTML UI with an embedded video player, channel logos, and external player activation links."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>M3U Web Player & Manager</title>
    <script src='https://cdn.jsdelivr.net/npm/hls.js@latest'></script>
    <script src='https://cdn.jsdelivr.net/npm/mpegts.js@latest/dist/mpegts.min.js'></script>
    <style>
        body { margin: 0; padding: 0; background: #121212; color: #fff; font-family: sans-serif; display: flex; height: 100vh; overflow: hidden; }
        #player-container { flex: 3; display: flex; flex-direction: column; justify-content: center; align-items: center; background: #000; padding: 20px; }
        video { width: 100%; max-height: 65vh; background: #111; border-radius: 8px; box-shadow: 0 4px 15px rgba(0,0,0,0.5); }
        .controls-area { margin-top: 20px; width: 100%; text-align: center; background: #1e1e1e; padding: 15px; border-radius: 8px; box-sizing: border-box; }
        #now-playing { font-size: 1.2rem; color: #00ff88; font-weight: bold; margin-bottom: 10px; }
        #raw-url { font-family: monospace; color: #aaa; background: #000; padding: 8px; border-radius: 4px; word-break: break-all; margin-bottom: 15px; font-size: 0.9rem; }
        .ext-btn { display: inline-block; color: white; text-decoration: none; padding: 10px 20px; border-radius: 5px; font-weight: bold; font-size: 1rem; transition: background 0.2s; margin: 0 5px; cursor: pointer; border: none; }
        .vlc-btn { background: #ff8800; } .vlc-btn:hover { background: #cc6600; }
        .pot-btn { background: #dddd00; color: #000; } .pot-btn:hover { background: #aaaa00; }
        .iina-btn { background: #007bff; } .iina-btn:hover { background: #0056b3; }
        #playlist-container { flex: 1; background: #1e1e1e; border-left: 1px solid #333; display: flex; flex-direction: column; min-width: 320px; }
        .header { padding: 15px; background: #252525; font-size: 1.1rem; font-weight: bold; border-bottom: 1px solid #333; }
        #channel-list { flex: 1; overflow-y: auto; padding: 0; margin: 0; list-style: none; }
        .channel-item { display: flex; flex-direction: row; align-items: center; padding: 10px 15px; border-bottom: 1px solid #2a2a2a; cursor: pointer; transition: background 0.2s; }
        .channel-item:hover { background: #2a2d35; }
        .channel-logo { width: 60px; height: 35px; object-fit: contain; margin-right: 15px; background: #000; border-radius: 4px; }
        .channel-logo-placeholder { width: 60px; height: 35px; margin-right: 15px; background: #333; border-radius: 4px; display: flex; justify-content: center; align-items: center; font-size: 0.7rem; color: #888; text-align: center; line-height: 1.1; }
        .channel-info { display: flex; flex-direction: column; flex: 1; overflow: hidden; }
        .channel-title { font-size: 1rem; margin-bottom: 4px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
        .channel-group { font-size: 0.8rem; color: #888; }
    </style>
</head>
<body>
    <div id='player-container'>
        <video id='video-element' controls autoplay></video>
        <div class='controls-area'>
            <div id='now-playing'>Select a channel...</div>
            <div id='raw-url'>Stream URL will appear here</div>
            <a id='vlc-link' href='#' class='ext-btn vlc-btn'>Play in VLC</a>
            <a id='pot-link' href='#' class='ext-btn pot-btn'>PotPlayer</a>
            <a id='iina-link' href='#' class='ext-btn iina-btn'>IINA (Mac)</a>
        </div>
    </div>
    <div id='playlist-container'>
        <div class='header'>Channel List (~a)</div>
        <ul id='channel-list'>~%" (length items))

    ;; Render playlist items
    (dolist (item items)
      (let* ((title (m3u-data:item-title item))
             (uri (m3u-data:item-uri item))
             (group (gethash "group-title" (m3u-data:item-attributes item) "Uncategorized"))
             (logo (gethash "tvg-logo" (m3u-data:item-attributes item) ""))
             (safe-uri (str:replace-all "'" "\\'" uri))
             ;; Safely handle logo HTML generation
             (logo-html (if (and (> (length logo) 0) (not (string= logo "#<MISSING>")))
                            (format nil "<img class='channel-logo' src='~a' loading='lazy' onerror=\"this.style.display='none'\">" (str:replace-all "'" "\\'" logo))
                            "<div class='channel-logo-placeholder'>No<br>Logo</div>")))
        (format s "<li class='channel-item' onclick=\"playStream('~a', '~a')\">
                      ~a
                      <div class='channel-info'>
                          <span class='channel-title'>~a</span>
                          <span class='channel-group'>~a</span>
                      </div>
                   </li>~%" safe-uri title logo-html title group)))

    (format s "        </ul>
    </div>
    <script>
        var videoElement = document.getElementById('video-element');
        var mpegtsPlayer = null;

        function playStream(url, title) {
            document.getElementById('now-playing').innerText = 'Playing: ' + title;
            document.getElementById('raw-url').innerText = url;

            // Dynamically update external player activation links
            document.getElementById('vlc-link').href = 'vlc://' + url;
            document.getElementById('pot-link').href = 'potplayer://' + url;
            // IINA requires standard URL encoding for its custom protocol parameter
            document.getElementById('iina-link').href = 'iina://weblink?url=' + encodeURIComponent(url);

            // Alert on native UDP/RTP browser limitations
            if(url.startsWith('udp://') || url.startsWith('rtp://')) {
                document.getElementById('now-playing').innerText = 'Cannot play raw multicast in browser. Please click an external player button below.';
                document.getElementById('now-playing').style.color = '#ff4444';
                return;
            } else {
                document.getElementById('now-playing').style.color = '#00ff88';
            }

            // Destroy the old mpegts player instance if it exists
            if (mpegtsPlayer != null) {
                mpegtsPlayer.destroy();
                mpegtsPlayer = null;
            }

            try {
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
                    mpegtsPlayer = mpegts.createPlayer({ type: 'mse', isLive: true, url: url });
                    mpegtsPlayer.attachMediaElement(videoElement);
                    mpegtsPlayer.load();
                    mpegtsPlayer.play();
                }
                // 3. Native fallback for other formats
                else {
                    videoElement.src = url;
                    videoElement.play();
                }
            } catch (error) {
                console.error('Playback failed:', error);
            }
        }
    </script>
</body>
</html>")))

;; --- Helper function to print iOS Safari SSL guide ---
(defun print-ssl-guide (bind-address)
  (let ((display-ip (if (string= bind-address "0.0.0.0") "192.168.x.x" bind-address)))
    (format t "~%==================================================~%")
    (format t "🚀 Server Startup & iOS Safari Guide~%")
    (format t "--------------------------------------------------~%")
    (format t "To enable HTTPS (required for modern iOS Safari), run this command~%")
    (format t "in the current directory (~A) to generate a valid cert:~%~%" (uiop:getcwd))
    (format t "  openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem \\~%")
    (format t "  -sha256 -days 365 -nodes -subj '/CN=~A' \\~%" display-ip)
    (format t "  -addext 'subjectAltName=IP:~A'~%~%" display-ip)
    (format t "Then AirDrop 'cert.pem' to your iPhone and fully trust it in settings.~%")
    (format t "The server will auto-detect the certs on the next startup.~%")
    (format t "==================================================~%~%")))

(defun start-server (input-xlsx bind-address port server-url strip-proxy)
  "Start the Hunchentoot HTTP/HTTPS server and serve the dynamic playlist and Web UI."

  (print-ssl-guide bind-address)

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

  ;; --- Initialize and start the server (Auto SSL Logic) ---
  (let* ((current-dir (uiop:getcwd))
         (cert-path (merge-pathnames "cert.pem" current-dir))
         (key-path (merge-pathnames "key.pem" current-dir))
         (is-ssl (and (probe-file cert-path) (probe-file key-path)))
         (protocol (if is-ssl "https" "http"))
         (display-ip (if (string= bind-address "0.0.0.0") "127.0.0.1" bind-address)))

    (let ((acceptor (if is-ssl
                        (make-instance 'hunchentoot:easy-ssl-acceptor
                                       :address bind-address
                                       :port port
                                       :ssl-certificate-file cert-path
                                       :ssl-privatekey-file key-path)
                        (make-instance 'hunchentoot:easy-acceptor
                                       :address bind-address
                                       :port port))))

      (if is-ssl
          (format t "[INFO] Found cert.pem and key.pem. Starting in HTTPS mode...~%")
          (format t "[WARN] Certificates not found. Falling back to HTTP mode...~%"))

      (hunchentoot:start acceptor)
      (format t "~%[SUCCESS] Server is running!~%")
      (format t "--> Web Player: ~a://~a:~d/~%" protocol display-ip port)
      (format t "--> Raw Playlist: ~a://~a:~d/playlist.m3u~%" protocol display-ip port)
      (format t "Press Ctrl+C to stop the server.~%")

      (handler-case
          (loop (sleep 60))
        #+sbcl (sb-sys:interactive-interrupt ()
                 (format t "~%Shutting down server...~%")
                 (hunchentoot:stop acceptor)
                 (uiop:quit 0))))))

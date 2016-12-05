#lang racket ; To get highlighting to go away

(
 ;; Actual DSL for video editing
 ("AVI Synth" "2000" "2015" "http://avisynth.nl/index.php/Main_Page")

 ;; Video editing Library
 ("MLT" "2008" "2016" "https://www.mltframework.org/docs/")
 ("Openshot" "2014" "2016" "http://openshot.org/files/libopenshot/")
 ("GStreamer" "1999" "2016" "https://gstreamer.freedesktop.org/documentation/")

 ;; Graphical viditors with an API
 ("Final Cut Pro" "2005" "2016" "https://developer.apple.com/library/content/documentation/AppleApplications/Conceptual/FXPlug_overview/FXPlugSDKOverview/FXPlugSDKOverview.html")
 ("Blender" "2003" "2016" "https://www.blender.org/api/")
 ("Windows Movie Maker" "???" "2016" "https://msdn.microsoft.com/en-us/library/windows/desktop/bb288384(v=vs.85).aspx")
 ("Adobe Premier" "???" "2016" "http://www.adobe.com/devnet/premiere/sdk/cc.html")

 ;; Website based APIs
 ("Movie Masher (moviemasher.rb)" "???" "2016" "http://www.moviemasher.com/docs/")
 ("Magistro" "???" "???" "http://developers.magisto.com/Getting_Started.html")
)

;; Underpowered (not capaper of non-linear video editing
(
 ;; DSLs
 ("Virtual Dub" "???" "2013" "http://virtualdub.org/virtualdub_docs.html")

 ;; APIs
 ("Xuggle" "2008" "2010" "http://www.xuggle.com/public/documentation/java/api/")

 ;; Website based APIs
 ("Magistro" "???" "???" "http://developers.magisto.com/Getting_Started.html")
)

;; Realtime focused toolss (that can do a bit of video editing
(
 ;; APIs
 ("Java Media Framework" "???" "???" "http://www.oracle.com/technetwork/java/javase/download-142937.html")
)

;; Common Videos WITHOUT any API support
(
 ("Youtube")
 ("iMovie")
)

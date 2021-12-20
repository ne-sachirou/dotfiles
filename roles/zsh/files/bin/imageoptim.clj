#!/usr/bin/env bb
;; Optimize JPEG images by mozjpeg & PNG images by zopflipng.
;;
;; Usage :
;; ag -0 -g '\.((jp(e?)g)|png)$' | xargs -0 -t -n2 -P$(nproc) imageoptim.clj

(ns util
  (:require
    [clojure.core.async :as async]
    [clojure.java.shell :as shell]
    [clojure.string :as str]))


(defn task-parallel
  "Do tasks in parallel by processers number.

  (task-parallel
    (fn [chan] (doseq [item stream] (async/>!! chan item)))
    (fn [item] (do-something item)))"
  [producer consumer]
  (let [nproc (.availableProcessors (Runtime/getRuntime))
        chan (async/chan nproc)
        timeout-ms 60000
        threads (mapv (fn [_]
                        (let [on-exit (async/chan)
                              ;; Using alts!! instead of alts! because of Babashka compatibility.
                              thread (async/go-loop [[data _] (async/alts!! [chan (async/timeout timeout-ms)])]
                                       (if data
                                         ;; Can only recur from tail position
                                         (do (consumer data)
                                             (recur (async/alts!! [chan (async/timeout timeout-ms)])))
                                         ;; (try
                                         ;;   (consumer data)
                                         ;;   (recur (async/alts!! [chan (async/timeout timeout-ms)]))
                                         ;;   (catch Exception ex
                                         ;;     (println ex)
                                         ;;     (recur (async/alts!! [chan (async/timeout timeout-ms)]))))
                                         (async/close! on-exit)))]
                          {:on-exit on-exit :thread thread}))
                      (range nproc))]
    (try
      (producer chan)
      (finally (async/close! chan)))
    (doseq [{:keys [on-exit thread]} threads]
      (async/<!! on-exit)
      (async/close! thread))))


(defn sh
  "Rakefile style sh. Returns a stdout string or throw an exception."
  [& args]
  (println "+" (str/join " " args))
  (let [{:keys [exit out err]} (apply shell/sh args)]
    (if (not= 0 exit) (throw (Exception. err)) :ok)
    (println out)
    out))


(ns image
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [util]))


(defn as-image
  ""
  [filename]
  (let [lower-case-filename (str/lower-case filename)
        [type extension] (cond
                           (str/ends-with? lower-case-filename ".gif") ["GIF" ".gif"]
                           (or (str/ends-with? lower-case-filename ".jpg")
                               (str/ends-with? lower-case-filename ".jpeg")) ["JPEG" ".jpg"]
                           (str/ends-with? lower-case-filename ".png") ["PNG" ".png"]
                           :else (throw (Exception. (str "Can not detect file type: " filename))))]
    {:extension extension
     :filename filename
     :type type}))


(defn- compress-by-command-fn
  [image command-fn]
  (let [input-filename (:filename image)
        output-filename (str (java.util.UUID/randomUUID) (:extension image))]
    (try (apply util/sh (apply command-fn [input-filename output-filename]))
         (let [input-file (io/file input-filename)
               input-file-length (.length input-file)
               output-file (io/file output-filename)
               output-file-length (.length output-file)]
           (println (format "%s : %.3f KB -> %.3f KB (%.2f %%)"
                            input-filename
                            (/ input-file-length 1024.0)
                            (/ output-file-length 1024.0)
                            (* (/ output-file-length input-file-length) 100.0)))
           (.renameTo output-file input-file))
         (catch Exception ex
           (println ex)
           (if (-> output-filename io/file .exists)
             (io/delete-file output-filename)
             true)))))


(defn- compress-gif
  [_image]
  (println "Do not support GIF yet."))


(defn- compress-jpeg
  [image]
  (compress-by-command-fn image
                          (fn [input-filename output-filename]
                            ["jpegtran.exe"
                             "-copy" "none"
                             "-optimize"
                             "-outfile" output-filename
                             input-filename])))


(defn- compress-png
  [image]
  (compress-by-command-fn image
                          (fn [input-filename output-filename]
                            ["zopflipng"
                             "-m"
                             "--lossy_transparent"
                             input-filename
                             output-filename])))


(defn compress
  ""
  [image]
  (case (:type image)
    "GIF" (compress-gif image)
    "JPEG" (compress-jpeg image)
    "PNG" (compress-png image)
    (throw (Exception. (str "Unknown image type: " (:type image))))))


(ns user
  (:require
    [clojure.core.async :as async]
    [image]
    [util]))


;; (doseq [filename *command-line-args*]
;;   (-> filename as-image compress))
(util/task-parallel
  (fn [chan]
    (doseq [filename *command-line-args*]
      (async/>!! chan (image/as-image filename))))
  (fn [image]
    (try
      (image/compress image)
      (catch Exception ex (println image ex)))))

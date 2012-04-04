(ns newark.core
  (:gen-class)
  (:use [newark.builder :only [init! start-watcher!]]
        [newark.packages :only [add-path!]]
        [clojure.tools.cli :only [cli]]))


(defn parse-args [args]
  (cli args
    ["-h" "--help"  "show this message" :flag true :default false]
    ["-w" "--watch" "watch targets for changes" :flag true :default false]
    ["-p" "--path"  "directories to add to loadpath"]))

(defn -main [& args]
  (let [[{:keys [help watch path]} targets banner] (parse-args args)]
    (if help
      (println banner)
      (do
        (when path (add-path! path))
        (init! targets)
        (when watch (start-watcher!))))))
;; Claude Code statusline helper. Reads the JSON from stdin and
;; outputs string to be displayed under the commandline.
;; Styled after the powerlevel10k prompt in config/p10k.zsh.

(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(defn fg [n] (str "\u001b[38;5;" n "m"))
(defn bg [n] (str "\u001b[48;5;" n "m"))
(def bold "\u001b[1m")
(def no-bold "\u001b[22m")
(def reset "\u001b[0m")

(def segment-bg 238)
(def bar-width 10)

(defn powerline [segments]
  (str (fg segment-bg) "" (bg segment-bg) " "
       (str/join (str " " (fg 246) "╱ ") segments)
       " " reset (fg segment-bg) "" reset))

(defn unique-prefix [name siblings]
  (or (first (for [n (range 1 (count name))
                   :let [prefix (subs name 0 n)]
                   :when (= 1 (count (filter #(str/starts-with? % prefix) siblings)))]
               prefix))
      name))

(defn shorten [name parent]
  (let [siblings (try (->> (fs/list-dir parent)
                           (filter fs/directory?)
                           (map (comp str fs/file-name)))
                      (catch Exception _ nil))]
    (if (some #{name} siblings)
      (unique-prefix name siblings)
      name)))

(defn dir-segment [dir]
  (let [home (System/getProperty "user.home")
        in-home? (or (= dir home) (str/starts-with? dir (str home "/")))
        [label base] (if in-home? ["~" home] ["" "/"])
        names (remove str/blank? (str/split (subs dir (if in-home? (count home) 0)) #"/"))
        parents (reductions fs/path (fs/path base) names)
        shortened (map shorten (butlast names) parents)
        anchor (fn [s] (str bold (fg 39) s no-bold))]
    (if (empty? names)
      (anchor (if in-home? "~" "/"))
      (str/join (str (fg 31) "/")
                (concat (map #(str (fg 103) %) (cons label shortened))
                        [(anchor (last names))])))))

(defn parse-porcelain [out]
  (reduce
   (fn [acc line]
     (let [[kind a b c] (str/split line #" ")]
       (case kind
         "#" (case a
               "branch.head" (assoc acc :branch b)
               "branch.oid" (assoc acc :commit (subs b 0 (min 8 (count b))))
               "branch.ab" (assoc acc
                                  :ahead (parse-long (subs b 1))
                                  :behind (parse-long (subs c 1)))
               "stash" (assoc acc :stashes (parse-long b))
               acc)
         ("1" "2") (cond-> acc
                     (not= \. (first a)) (update :staged inc)
                     (not= \. (second a)) (update :unstaged inc))
         "u" (update acc :conflicted inc)
         "?" (update acc :untracked inc)
         acc)))
   {:ahead 0 :behind 0 :stashes 0 :staged 0 :unstaged 0 :conflicted 0 :untracked 0}
   (str/split-lines out)))

(defn git-status [dir]
  (try
    (let [{:keys [exit out]} (p/shell {:dir dir :out :string :err :string :continue true}
                                      "git" "--no-optional-locks" "status"
                                      "--porcelain=v2" "--branch" "--show-stash")]
      (when (zero? exit)
        (parse-porcelain out)))
    (catch Exception _ nil)))

(defn truncate-middle [s]
  (if (> (count s) 32)
    (str (subs s 0 12) "…" (subs s (- (count s) 12)))
    s))

(defn vcs-segment [{:keys [branch commit ahead behind stashes conflicted staged unstaged untracked]}]
  (let [meta (fg 248) clean (fg 76) modified (fg 178) untracked-c (fg 39) conflicted-c (fg 196)]
    (str (if (= branch "(detached)")
           (str meta "@" clean commit)
           (str clean (truncate-middle branch)))
         (when (pos? behind) (str " " clean "⇣" behind))
         (when (pos? ahead) (str (when (zero? behind) " ") clean "⇡" ahead))
         (when (pos? stashes) (str " " clean "*" stashes))
         (when (pos? conflicted) (str " " conflicted-c "~" conflicted))
         (when (pos? staged) (str " " modified "+" staged))
         (when (pos? unstaged) (str " " modified "!" unstaged))
         (when (pos? untracked) (str " " untracked-c "?" untracked)))))

(defn usage-color [pct]
  (cond (>= pct 90) 196
        (>= pct 70) 178
        :else 76))

(defn context-bar [pct]
  (let [filled (-> pct (* bar-width) (quot 100) (min bar-width))]
    (str (fg (usage-color pct)) (str/join (repeat filled "█"))
         (fg 242) (str/join (repeat (- bar-width filled) "█"))
         " " (fg (usage-color pct)) pct "%")))

(defn duration [ms]
  (let [s (quot ms 1000)
        units [[(quot s 86400) "d"] [(rem (quot s 3600) 24) "h"] [(rem (quot s 60) 60) "m"] [(rem s 60) "s"]]]
    (->> units
         (drop-while (fn [[n unit]] (and (zero? n) (not= unit "s"))))
         (map (fn [[n unit]] (str n unit)))
         (str/join " "))))

;; Claude Code indents the statusline, so filling all of $COLUMNS would wrap.
(def ui-margin 4)

(defn visible-width [s]
  (let [text (str/replace s #"\u001b\[[0-9;]*m" "")]
    (.codePointCount text 0 (count text))))

(defn fill-line [left right columns]
  (let [gap (max 1 (- columns ui-margin (visible-width left) (visible-width right) 2))]
    (str left " " (fg 242) (str/join (repeat gap "─")) reset " " right)))

(defn statusline [status columns]
  (let [dir (get-in status ["workspace" "current_dir"])
        git (git-status dir)
        pct (long (or (get-in status ["context_window" "used_percentage"]) 0))
        cost (or (get-in status ["cost" "total_cost_usd"]) 0)
        ms (or (get-in status ["cost" "total_duration_ms"]) 0)]
    (fill-line (powerline (cond-> [(dir-segment dir)]
                            git (conj (vcs-segment git))))
               (powerline [(str (fg 255) (get-in status ["model" "display_name"]))
                           (context-bar pct)
                           (str (fg 178) (format "$%.2f" (double cost)))
                           (str (fg 248) (duration ms))])
               columns)))

(when (= *file* (System/getProperty "babashka.file"))
  (println (statusline (-> *in* slurp json/parse-string)
                       (or (some-> (System/getenv "COLUMNS") parse-long) 80))))

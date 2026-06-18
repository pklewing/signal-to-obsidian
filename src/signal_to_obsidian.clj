#!/usr/bin/env bb
;; signal_to_obsidian.clj
;;
;; Convert a Signal Desktop "local backup" JSONL export into Obsidian Markdown,
;; one file per conversation. Media are embedded inline as Obsidian wikilinks,
;; reactions are rendered inline in chronological order.
;;
;; Usage (via the bb task, recommended):
;;   bb run <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
;;
;; Or directly as a script:
;;   bb src/signal_to_obsidian.clj <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]
;;
;;   <export.jsonl>   The newline-delimited JSON export.
;;   <output-dir>     Directory where the per-conversation .md files are written.
;;   --files-root     Path to the backup's `files/` parent (the directory that
;;                    CONTAINS `files/`). Defaults to the export file's directory.
;;   --copy-media     Copy referenced attachments into <output-dir>/files/xx/ so
;;                    the output directory is a self-contained Obsidian vault.
;;
;; The script reads the whole file once, building lookup tables from the
;; `recipient` and `chat` records, then renders every `chatItem`.

(ns signal-to-obsidian
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [babashka.fs :as fs])
  (:import [java.security MessageDigest]
           [java.util Base64]
           [java.time Instant ZoneId]
           [java.time.format DateTimeFormatter]))

;; ---------------------------------------------------------------------------
;; Byte / hash helpers  (mirror Signal-Desktop mediaId.preload.ts)
;; ---------------------------------------------------------------------------

(defn b64->bytes ^bytes [^String s]
  (.decode (Base64/getDecoder) s))

(defn hex->bytes ^bytes [^String s]
  (let [n (/ (count s) 2)
        out (byte-array n)]
    (dotimes [i n]
      (let [hi (Character/digit (.charAt s (* 2 i)) 16)
            lo (Character/digit (.charAt s (inc (* 2 i))) 16)]
        (aset out i (unchecked-byte (+ (* hi 16) lo)))))
    out))

(defn bytes->hex ^String [^bytes bs]
  (let [sb (StringBuilder. (* 2 (alength bs)))]
    (doseq [b bs]
      (.append sb (format "%02x" (bit-and b 0xff))))
    (.toString sb)))

(defn sha256 ^bytes [^bytes bs]
  (.digest (MessageDigest/getInstance "SHA-256") bs))

(defn concat-bytes ^bytes [^bytes a ^bytes b]
  (let [out (byte-array (+ (alength a) (alength b)))]
    (System/arraycopy a 0 out 0 (alength a))
    (System/arraycopy b 0 out (alength a) (alength b))
    out))

;; filename = hex( sha256( base64(plaintextHash) ++ base64(localKey) ) )
;; NOTE: In the JSONL export, BOTH plaintextHash and localKey are base64-encoded.
;; (The Signal-Desktop *source* calls fromHex(plaintextHash), but that operates on
;; an already-hex string used internally; the exported JSON value is base64.
;; Verified empirically: base64-decoding both reproduces the real on-disk names.)
(defn local-backup-filename
  "Returns the 64-char hex on-disk name for an attachment, or nil if the
   required locator fields are absent."
  [locator]
  (let [ph (get locator "plaintextHash")
        lk (get locator "localKey")]
    (when (and ph lk)
      (bytes->hex (sha256 (concat-bytes (b64->bytes ph) (b64->bytes lk)))))))

(defn ext-for
  "Pick a file extension from contentType, falling back to the fileName suffix."
  [content-type file-name]
  (let [from-name (when (and file-name (str/includes? file-name "."))
                    (subs file-name (inc (str/last-index-of file-name "."))))]
    (cond
      (and from-name (<= (count from-name) 5)) (str/lower-case from-name)
      (and content-type (str/includes? content-type "/"))
      (let [sub (subs content-type (inc (str/index-of content-type "/")))]
        ;; normalise a few common ones; otherwise use the subtype verbatim
        (get {"jpeg" "jpg" "svg+xml" "svg" "quicktime" "mov"} sub sub))
      :else "bin")))

(defn attachment-path
  "Build the files/xx/<hash>.<ext> path for one attachment map, or nil."
  [att]
  (let [ptr (get att "pointer")
        locator (get ptr "locatorInfo")
        name (local-backup-filename locator)]
    (when name
      (let [ext (ext-for (get ptr "contentType") (get ptr "fileName"))]
        (str "files/" (subs name 0 2) "/" name "." ext)))))

(defn attachment-hash
  "Just the 64-char hex stem (no extension), or nil."
  [att]
  (local-backup-filename (get-in att ["pointer" "locatorInfo"])))

(defn build-file-index
  "Walk <files-root>/files once and return a map of hash-stem -> absolute Path.
   Turns thousands of per-attachment directory listings into a single walk."
  [files-root]
  (let [root (fs/path files-root "files")]
    (if-not (fs/exists? root)
      {}
      (into {}
            (comp (filter fs/regular-file?)
                  (map (fn [p]
                         (let [fname (fs/file-name p)
                               dot (str/last-index-of fname ".")
                               stem (if dot (subs fname 0 dot) fname)]
                           [stem p]))))
            (fs/glob root "**/*")))))

(defn resolve-on-disk
  "Look up an attachment's real Path via the prebuilt file index. nil if absent."
  [file-index att]
  (when-let [h (attachment-hash att)]
    (get file-index h)))

;; ---------------------------------------------------------------------------
;; Time formatting
;; ---------------------------------------------------------------------------

(def ^DateTimeFormatter ts-fmt
  (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
             (ZoneId/systemDefault)))

(def ^DateTimeFormatter time-only-fmt
  (.withZone (DateTimeFormatter/ofPattern "HH:mm")
             (ZoneId/systemDefault)))

(defn parse-millis [s]
  (when s (Long/parseLong (str s))))

(defn fmt-ts [millis]
  (when millis (.format ts-fmt (Instant/ofEpochMilli millis))))

(defn fmt-time [millis]
  (when millis (.format time-only-fmt (Instant/ofEpochMilli millis))))

;; ---------------------------------------------------------------------------
;; Name resolution
;; ---------------------------------------------------------------------------

(defn contact-name
  "Best display name for an individual contact recipient."
  [contact]
  (let [sys (->> [(get contact "systemGivenName") (get contact "systemFamilyName")]
                 (remove str/blank?) (str/join " ") str/trim)
        prof (->> [(get contact "profileGivenName") (get contact "profileFamilyName")]
                  (remove str/blank?) (str/join " ") str/trim)]
    (or (not-empty sys)
        (not-empty prof)
        (get contact "e164")
        "Unknown")))

(defn group-title [group]
  (or (get-in group ["snapshot" "title" "title"])
      "Unknown Group"))

(defn recipient-display-name
  "Display name for any recipient record (contact, group, or self)."
  [recip self-name]
  (cond
    (get recip "self") self-name
    (get recip "contact") (contact-name (get recip "contact"))
    (get recip "group") (group-title (get recip "group"))
    ;; distributionList / callLink etc. -> fall back to id
    :else (str "Recipient " (get recip "id"))))

;; ---------------------------------------------------------------------------
;; Filesystem-safe filenames
;; ---------------------------------------------------------------------------

(defn sanitize-filename [s]
  (-> s
      (str/replace #"[\\/:*?\"<>|]" "")   ; characters illegal on common FS
      (str/replace #"\s+" " ")
      str/trim
      (#(if (str/blank? %) "conversation" %))))

;; ---------------------------------------------------------------------------
;; First pass: build lookup tables
;; ---------------------------------------------------------------------------

(defn build-indexes [records]
  (let [recipients (atom {})       ; id -> recipient map
        chats (atom {})            ; chatId -> recipientId
        self-id (atom nil)
        self-name (atom "Me")]
    (doseq [rec records]
      (cond
        (contains? rec "recipient")
        (let [r (get rec "recipient")]
          (swap! recipients assoc (get r "id") r)
          (when (get r "self")
            (reset! self-id (get r "id"))))

        (contains? rec "chat")
        (let [c (get rec "chat")]
          (swap! chats assoc (get c "id") (get c "recipientId")))

        (contains? rec "account")
        (let [a (get rec "account")
              full (not-empty (str/trim (str (get a "givenName") " "
                                             (get a "familyName"))))]
          (when full
            (reset! self-name full)))))
    ;; If the account holder also appears as a recipient with :self that carries
    ;; a contact name, prefer that (it is usually the fuller profile/system name).
    (when-let [sid @self-id]
      (when-let [self-recip (get @recipients sid)]
        (when (get self-recip "contact")
          (let [nm (contact-name (get self-recip "contact"))]
            (when (and nm (not= nm "Unknown"))
              (reset! self-name nm))))))
    {:recipients @recipients
     :chats @chats
     :self-id @self-id
     :self-name @self-name}))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(defn resolve-author-name [idx author-id]
  (let [{:keys [recipients self-id self-name]} idx]
    (if (and self-id (= author-id self-id))
      self-name
      (if-let [r (get recipients author-id)]
        (recipient-display-name r self-name)
        (str "User " author-id)))))

(defn render-reactions [idx reactions]
  (when (seq reactions)
    (->> reactions
         (sort-by #(parse-millis (get % "sentTimestamp")))
         (map (fn [r]
                (str (get r "emoji")
                     " " (resolve-author-name idx (get r "authorId")))))
         (str/join "  ")
         (#(str "    – reactions: " %)))))

(defn render-attachments [file-index attachments]
  (->> attachments
       (keep (fn [att]
               (let [h (attachment-hash att)
                     src (when h (resolve-on-disk file-index att))
                     ctype (get-in att ["pointer" "contentType"])]
                 (cond
                   ;; resolved on disk: embed the real filename (correct extension)
                   src (str "![[files/" (subs h 0 2) "/" (fs/file-name src) "]]")
                   ;; derivable but not present locally: placeholder, no broken link
                   h   (str "*[attachment unavailable" (when ctype (str ": " ctype)) "]*")
                   ;; no locator at all: nothing renderable
                   :else nil))))
       (str/join "\n")))

(defn outgoing? [ci] (contains? ci "outgoing"))
(defn incoming? [ci] (contains? ci "incoming"))

(defn render-chat-item
  "Render one chatItem to a Markdown block, or nil if nothing to show."
  [idx file-index ci]
  (let [author (resolve-author-name idx (get ci "authorId"))
        millis (parse-millis (get ci "dateSent"))
        time (fmt-time millis)
        sm (get ci "standardMessage")]
    (when sm
      (let [body (get-in sm ["text" "body"])
            attachments (get sm "attachments")
            reactions (get sm "reactions")
            lines (cond-> []
                    true (conj (str "**" author "** · " time))
                    (not (str/blank? body)) (conj body)
                    (seq attachments) (conj (render-attachments file-index attachments))
                    (seq reactions) (conj (render-reactions idx reactions)))]
        (str/join "\n" lines)))))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn parse-args [args]
  (loop [a args, opts {:files-root nil}, pos []]
    (if (empty? a)
      (assoc opts :positional pos)
      (let [x (first a)]
        (cond
          (= x "--files-root") (recur (drop 2 a) (assoc opts :files-root (second a)) pos)
          (= x "--copy-media")  (recur (rest a) (assoc opts :copy-media true) pos)
          :else                 (recur (rest a) opts (conj pos x)))))))

(defn convert!
  "Core conversion. Reads in-path, writes one Markdown file per conversation to
   out-dir. Returns a summary map {:conversations n :missing n :indexed n}.
   Does NOT print usage or call System/exit — that is the CLI's job.
   Throws if in-path does not exist."
  [{:keys [in-path out-dir files-root copy-media verbose]
    :or {verbose true}}]
  (when-not (fs/exists? in-path)
    (throw (ex-info (str "Input file not found: " in-path) {:in-path in-path})))
  (fs/create-dirs out-dir)
  (let [files-root (or files-root (str (fs/parent (fs/absolutize in-path))))
        file-index (build-file-index files-root)
        _ (when verbose
            (println (format "Indexed %d media file(s) under %s/files/"
                             (count file-index) files-root)))
        records (with-open [r (io/reader in-path)]
                  (->> (line-seq r)
                       (remove str/blank?)
                       (mapv #(json/parse-string % false))))
        idx (build-indexes records)
        items-by-chat (reduce (fn [m rec]
                                (if (contains? rec "chatItem")
                                  (let [ci (get rec "chatItem")]
                                    (update m (get ci "chatId") (fnil conj []) ci))
                                  m))
                              {}
                              records)
        missing (atom 0)]
    (doseq [[chat-id items] items-by-chat]
      (let [recip-id (get-in idx [:chats chat-id])
            recip (get-in idx [:recipients recip-id])
            conv-name (if recip
                        (recipient-display-name recip (:self-name idx))
                        (str "chat-" chat-id))
            fname (str (sanitize-filename conv-name) ".md")
            out-file (str (fs/path out-dir fname))
            header (str "# " conv-name "\n\n"
                        "_" (count items) " messages_\n")
            blocks (keep #(render-chat-item idx file-index %) (reverse items))
            _ (doseq [ci items
                      att (get-in ci ["standardMessage" "attachments"])]
                (let [src (resolve-on-disk file-index att)]
                  (if (nil? src)
                    (swap! missing inc)
                    (when copy-media
                      (let [rel (str "files/" (subs (attachment-hash att) 0 2)
                                     "/" (fs/file-name src))
                            dst (fs/path out-dir rel)]
                        (fs/create-dirs (fs/parent dst))
                        (when-not (fs/exists? dst)
                          (fs/copy src dst)))))))
            content (str header "\n" (str/join "\n\n---\n\n" blocks) "\n")]
        (spit out-file content)
        (when verbose
          (println (format "Wrote %s  (%d messages)" fname (count items))))))
    (when verbose
      (println (format "\nDone. %d conversation file(s) in %s"
                       (count items-by-chat) out-dir))
      (when (pos? @missing)
        (println (format "Note: %d attachment file(s) referenced were not found under %s/files/ — links are still written."
                         @missing files-root))))
    {:conversations (count items-by-chat)
     :missing @missing
     :indexed (count file-index)}))

(defn -main [args]
  (let [{:keys [positional files-root copy-media]} (parse-args args)
        [in-path out-dir] positional]
    (when (or (nil? in-path) (nil? out-dir))
      (println "Usage: bb run <export.jsonl> <output-dir> [--files-root <dir>] [--copy-media]")
      (System/exit 1))
    (when-not (fs/exists? in-path)
      (println "Input file not found:" in-path)
      (System/exit 1))
    (convert! {:in-path in-path
               :out-dir out-dir
               :files-root files-root
               :copy-media copy-media})))

;; Run when invoked directly as a script (bb src/signal_to_obsidian.clj ...)
;; or via the `bb run` task (which calls -main). Guarded so that loading the
;; namespace from tests does NOT trigger a conversion.
(when (= *file* (System/getProperty "babashka.file"))
  (-main *command-line-args*))

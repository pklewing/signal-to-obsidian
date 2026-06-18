;; test/signal_to_obsidian_test.clj
;;
;; Test suite for signal-to-obsidian. With src/ and test/ on the classpath
;; (see bb.edn), run with:
;;
;;   bb test
;;
;; or directly:
;;
;;   bb --classpath src:test -e "(require 'signal-to-obsidian-test)(signal-to-obsidian-test/run)"

(ns signal-to-obsidian-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            ;; namespace under test; loaded by classpath, does not auto-run
            [signal-to-obsidian :as sig]))

;; Bring the functions under test into scope with short names.
(def local-backup-filename   sig/local-backup-filename)
(def attachment-hash         sig/attachment-hash)
(def attachment-path         sig/attachment-path)
(def ext-for                 sig/ext-for)
(def contact-name            sig/contact-name)
(def group-title             sig/group-title)
(def recipient-display-name  sig/recipient-display-name)
(def resolve-author-name     sig/resolve-author-name)
(def sanitize-filename       sig/sanitize-filename)
(def parse-millis            sig/parse-millis)
(def build-indexes           sig/build-indexes)
(def render-reactions        sig/render-reactions)
(def render-chat-item        sig/render-chat-item)
(def build-file-index        sig/build-file-index)
(def resolve-on-disk         sig/resolve-on-disk)
(def parse-args              sig/parse-args)
(def convert!                sig/convert!)

;; ---------------------------------------------------------------------------
;; 1. Attachment path derivation  — the heart of the tool.
;;    These vectors were verified against real on-disk files; they double as
;;    regression tests against the hex-vs-base64 bug we hit during development.
;; ---------------------------------------------------------------------------

(deftest local-backup-filename-known-vectors
  (testing "plaintextHash + localKey (both base64) -> verified on-disk hex name"
    ;; Confirmed present on disk during development as files/5e/5ec3...pdf
    (is (= "5ec395356d6e3cab7a8618a5ecf42388a674df19c16818141af65a64fbbde474"
           (local-backup-filename
            {"plaintextHash" "7M+2gah3LOVsCOGJJ/x9wy31FGU8fbWp4mNnuMdbFSw="
             "localKey" "nAdZxo2Hgz+hjD9fiffG+SrPZiQbq8tMbP1SlKHRFoG5fiL8v7+v2owFuG9gk8UalI5OQHCtLorkOIyC0/LgrA=="})))
    ;; Confirmed present on disk as files/dd/ddaf...jpg
    (is (= "ddaf67507faf47ba526e1079a03c845341cbf2deb57e93c71de1c2f425c7451f"
           (local-backup-filename
            {"plaintextHash" "tOdFThsY7/ELT8TagOBtRLfZY7nVRqMv8br1IfiKZnQ="
             "localKey" "SveXa4aGRrd91d7XgVrMVZHDJLjZSvG1v53QzFXGgXbs0yVwvONkZCtMFlFm4vA2WT8RbDbHhWFyFgRjj8VKtw=="})))
    ;; Confirmed present on disk as files/42/42a9...jpg
    (is (= "42a90b883bbb3e53536687ebf9869765a5b3d332bf7ad64e630faab2a671142d"
           (local-backup-filename
            {"plaintextHash" "bptmOyPGh6/rFaYNo2kqLfGZ83hdsLBlNMqG48DhF4s="
             "localKey" "732vBCDL+iwOhEvjK/YQpfiVzfTh39RtBh3q3VGj8wa46zqAej5sBTVrQixD/SrPdYQcPp/+R4aTCq7/cn/g8Q=="})))))

(deftest local-backup-filename-shape
  (testing "output is always 64 lowercase hex chars"
    (let [n (local-backup-filename
             {"plaintextHash" "tOdFThsY7/ELT8TagOBtRLfZY7nVRqMv8br1IfiKZnQ="
              "localKey" "SveXa4aGRrd91d7XgVrMVZHDJLjZSvG1v53QzFXGgXbs0yVwvONkZCtMFlFm4vA2WT8RbDbHhWFyFgRjj8VKtw=="})]
      (is (= 64 (count n)))
      (is (re-matches #"[0-9a-f]{64}" n))))
  (testing "missing locator fields -> nil (not an exception)"
    (is (nil? (local-backup-filename {"plaintextHash" "abc"})))
    (is (nil? (local-backup-filename {"localKey" "abc"})))
    (is (nil? (local-backup-filename {})))))

(deftest attachment-hash-and-path
  (let [att {"pointer" {"contentType" "application/pdf"
                        "fileName" "report.pdf"
                        "locatorInfo"
                        {"plaintextHash" "7M+2gah3LOVsCOGJJ/x9wy31FGU8fbWp4mNnuMdbFSw="
                         "localKey" "nAdZxo2Hgz+hjD9fiffG+SrPZiQbq8tMbP1SlKHRFoG5fiL8v7+v2owFuG9gk8UalI5OQHCtLorkOIyC0/LgrA=="}}}]
    (testing "attachment-hash returns the bare 64-char stem"
      (is (= "5ec395356d6e3cab7a8618a5ecf42388a674df19c16818141af65a64fbbde474"
             (attachment-hash att))))
    (testing "attachment-path shards by first two chars and appends extension"
      (is (= "files/5e/5ec395356d6e3cab7a8618a5ecf42388a674df19c16818141af65a64fbbde474.pdf"
             (attachment-path att))))
    (testing "no locator -> nil"
      (is (nil? (attachment-hash {"pointer" {"contentType" "image/png"}})))
      (is (nil? (attachment-path {"pointer" {"contentType" "image/png"}}))))))

;; ---------------------------------------------------------------------------
;; 2. Extension picking
;; ---------------------------------------------------------------------------

(deftest ext-for-cases
  (testing "fileName suffix wins when present and short"
    (is (= "pdf" (ext-for "application/octet-stream" "doc.pdf")))
    (is (= "jpg" (ext-for nil "photo.JPG"))))            ; lowercased
  (testing "falls back to contentType subtype"
    (is (= "png" (ext-for "image/png" nil)))
    (is (= "jpg" (ext-for "image/jpeg" nil)))            ; normalised jpeg->jpg
    (is (= "mov" (ext-for "video/quicktime" nil)))       ; normalised
    (is (= "svg" (ext-for "image/svg+xml" nil))))        ; normalised
  (testing "unknown -> bin"
    (is (= "bin" (ext-for nil nil)))
    (is (= "bin" (ext-for "garbage-no-slash" nil))))
  (testing "absurdly long 'extension' is ignored in favour of contentType"
    (is (= "png" (ext-for "image/png" "file.thisisnotanextension")))))

;; ---------------------------------------------------------------------------
;; 3. Name resolution and its fallback chain
;; ---------------------------------------------------------------------------

(deftest contact-name-fallbacks
  (testing "system name preferred over profile name"
    (is (= "Sys Name"
           (contact-name {"systemGivenName" "Sys" "systemFamilyName" "Name"
                          "profileGivenName" "Prof" "profileFamilyName" "Ile"}))))
  (testing "profile name used when no system name"
    (is (= "Prof Ile"
           (contact-name {"profileGivenName" "Prof" "profileFamilyName" "Ile"}))))
  (testing "partial names join cleanly without stray spaces"
    (is (= "Jane" (contact-name {"systemFamilyName" "Jane"})))
    (is (= "Solo" (contact-name {"profileGivenName" "Solo"}))))
  (testing "e164 used when no names at all"
    (is (= "491635981282" (contact-name {"e164" "491635981282"}))))
  (testing "Unknown when nothing is present"
    (is (= "Unknown" (contact-name {})))))

(deftest group-title-cases
  (is (= "Jane, Jim and Sally"
         (group-title {"snapshot" {"title" {"title" "Jane, Jim and Sally"}}})))
  (is (= "Unknown Group" (group-title {"snapshot" {}})))
  (is (= "Unknown Group" (group-title {}))))

(deftest recipient-display-name-dispatch
  (testing "self -> provided self-name"
    (is (= "Me" (recipient-display-name {"self" {} "id" "166"} "Me"))))
  (testing "contact -> contact-name"
    (is (= "Alice"
           (recipient-display-name {"contact" {"profileGivenName" "Alice"} "id" "3"} "Me"))))
  (testing "group -> group title"
    (is (= "Trip"
           (recipient-display-name
            {"group" {"snapshot" {"title" {"title" "Trip"}}} "id" "9"} "Me"))))
  (testing "unknown recipient kind -> Recipient <id>"
    (is (= "Recipient 7"
           (recipient-display-name {"id" "7" "distributionList" {}} "Me")))))

(deftest resolve-author-name-cases
  (let [idx {:recipients {"166" {"self" {} "id" "166"}
                          "22" {"contact" {"profileGivenName" "Jane"} "id" "22"}}
             :self-id "166"
             :self-name "Me"}]
    (is (= "Me" (resolve-author-name idx "166")))
    (is (= "Jane" (resolve-author-name idx "22")))
    (testing "unknown author id -> User <id>, never throws"
      (is (= "User 999" (resolve-author-name idx "999"))))))

;; ---------------------------------------------------------------------------
;; 4. Filename sanitization
;; ---------------------------------------------------------------------------

(deftest sanitize-filename-cases
  (is (= "Jane, Jim and Sally" (sanitize-filename "Jane, Jim and Sally")))
  (testing "illegal filesystem characters stripped (segments fuse, no space inserted)"
    (is (= "abcd" (sanitize-filename "a/b:c*d")))
    (is (= "namepart" (sanitize-filename "name<>:\"/\\|?*part"))))
  (testing "whitespace collapsed and trimmed"
    (is (= "a b" (sanitize-filename "  a    b  "))))
  (testing "empty / all-illegal falls back to a default"
    (is (= "conversation" (sanitize-filename "")))
    (is (= "conversation" (sanitize-filename "/////")))
    (is (= "conversation" (sanitize-filename "   ")))))

;; ---------------------------------------------------------------------------
;; 5. Timestamp parsing
;; ---------------------------------------------------------------------------

(deftest parse-millis-cases
  (is (= 1781607348551 (parse-millis "1781607348551")))
  (is (= 1781607348551 (parse-millis 1781607348551)))   ; tolerates numeric too
  (is (nil? (parse-millis nil))))

;; ---------------------------------------------------------------------------
;; 6. Index building from records (the recipient/chat lookup tables)
;; ---------------------------------------------------------------------------

(deftest build-indexes-cases
  (let [records [{"version" 1}
                 {"account" {"givenName" "Pete"}}
                 ;; self recipient carries a fuller name than account.givenName
                 {"recipient" {"id" "166" "self" {}
                               "contact" {"profileGivenName" "Pete" "profileFamilyName" "Stone"}}}
                 {"recipient" {"id" "22" "contact" {"profileGivenName" "Jane"}}}
                 {"recipient" {"id" "175" "group" {"snapshot" {"title" {"title" "Grp"}}}}}
                 {"chat" {"id" "175" "recipientId" "175"}}
                 {"chat" {"id" "7" "recipientId" "22"}}
                 {"chatItem" {"chatId" "175"}}]
        idx (build-indexes records)]
    (is (= "166" (:self-id idx)))
    (testing "self recipient's full contact name is preferred over account givenName"
      (is (= "Pete Stone" (:self-name idx))))
    (is (= "175" (get-in idx [:chats "175"])))
    (is (= "22" (get-in idx [:chats "7"])))
    (is (= "Jane" (contact-name (get-in idx [:recipients "22" "contact"])))))
  (testing "falls back to account name when self recipient has no contact name"
    (let [idx (build-indexes [{"account" {"givenName" "Solo" "familyName" "Person"}}
                              {"recipient" {"id" "1" "self" {}}}])]
      (is (= "Solo Person" (:self-name idx)))))
  (testing "defaults to Me when nothing provides a name"
    (let [idx (build-indexes [{"recipient" {"id" "1" "self" {}}}])]
      (is (= "Me" (:self-name idx))))))

;; ---------------------------------------------------------------------------
;; 7. Reaction rendering — chronological order regardless of input order
;; ---------------------------------------------------------------------------

(deftest render-reactions-chronological
  (let [idx {:recipients {"1" {"contact" {"profileGivenName" "Early"} "id" "1"}
                          "2" {"contact" {"profileGivenName" "Late"} "id" "2"}}
             :self-id nil :self-name "Me"}
        ;; deliberately out of order in the input
        reactions [{"emoji" "🎉" "authorId" "2" "sentTimestamp" "2000"}
                   {"emoji" "👍" "authorId" "1" "sentTimestamp" "1000"}]
        out (render-reactions idx reactions)]
    (testing "earlier reaction appears before later one"
      (is (< (.indexOf out "👍") (.indexOf out "🎉")))
      (is (str/includes? out "👍 Early"))
      (is (str/includes? out "🎉 Late")))
    (testing "empty reactions -> nil"
      (is (nil? (render-reactions idx []))))))

;; ---------------------------------------------------------------------------
;; 8. End-to-end rendering of a chat item
;; ---------------------------------------------------------------------------

(deftest render-chat-item-text
  (let [idx {:recipients {"166" {"self" {} "id" "166"}}
             :self-id "166" :self-name "Me"}
        ci {"chatId" "175" "authorId" "166" "dateSent" "1781607348551"
            "outgoing" {} "standardMessage" {"text" {"body" "Hello world"}}}
        out (render-chat-item idx {} ci)]
    (is (str/includes? out "**Me**"))
    (is (str/includes? out "Hello world"))))

(deftest render-chat-item-with-reaction
  (let [idx {:recipients {"166" {"self" {} "id" "166"}
                          "76" {"contact" {"profileGivenName" "Jim"} "id" "76"}}
             :self-id "166" :self-name "Me"}
        ci {"chatId" "175" "authorId" "76" "dateSent" "1781607795234"
            "incoming" {}
            "standardMessage" {"text" {"body" "Have fun"}
                               "reactions" [{"emoji" "👍" "authorId" "166"
                                             "sentTimestamp" "1781609673539"}]}}
        out (render-chat-item idx {} ci)]
    (is (str/includes? out "**Jim"))
    (is (str/includes? out "Have fun"))
    (is (str/includes? out "👍 Me"))))

(deftest render-chat-item-unavailable-attachment
  (testing "attachment with no matching file -> placeholder, not a broken embed"
    (let [idx {:recipients {} :self-id nil :self-name "Me"}
          ci {"chatId" "1" "authorId" "5" "dateSent" "1000"
              "incoming" {}
              "standardMessage"
              {"attachments"
               [{"pointer" {"contentType" "image/jpeg"
                            "locatorInfo"
                            {"plaintextHash" "tOdFThsY7/ELT8TagOBtRLfZY7nVRqMv8br1IfiKZnQ="
                             "localKey" "SveXa4aGRrd91d7XgVrMVZHDJLjZSvG1v53QzFXGgXbs0yVwvONkZCtMFlFm4vA2WT8RbDbHhWFyFgRjj8VKtw=="}}}]}}
          out (render-chat-item idx {} ci)]   ; empty index -> nothing resolves
      (is (str/includes? out "attachment unavailable"))
      (is (str/includes? out "image/jpeg"))
      (is (not (str/includes? out "![["))))))

(deftest render-chat-item-non-standard-skipped
  (testing "a chatItem without standardMessage renders nothing"
    (is (nil? (render-chat-item {:recipients {} :self-name "Me"} {}
                                {"chatId" "1" "authorId" "1"
                                 "updateMessage" {"some" "group-event"}})))))

;; ---------------------------------------------------------------------------
;; 9. Filesystem round-trips: build-file-index and resolve-on-disk
;; ---------------------------------------------------------------------------

(deftest build-file-index-and-resolve
  (let [tmp (fs/create-temp-dir {:prefix "sig-test-"})]
    (try
      ;; Lay out a fake backup tree: files/<xx>/<hash>.<ext> plus a .DS_Store
      (let [hash "5ec395356d6e3cab7a8618a5ecf42388a674df19c16818141af65a64fbbde474"
            sub (fs/path tmp "files" (subs hash 0 2))]
        (fs/create-dirs sub)
        (spit (str (fs/path sub (str hash ".pdf"))) "fake pdf bytes")
        ;; a hidden file that find(1) would count but glob should skip
        (spit (str (fs/path tmp "files" ".DS_Store")) "junk")
        (let [index (build-file-index (str tmp))]
          (testing "index keys are extension-stripped hash stems"
            (is (contains? index hash))
            (is (= 1 (count index))))            ; .DS_Store excluded
          (testing "resolve-on-disk finds the real file regardless of extension"
            (let [att {"pointer" {"contentType" "application/pdf"
                                  "locatorInfo"
                                  {"plaintextHash" "7M+2gah3LOVsCOGJJ/x9wy31FGU8fbWp4mNnuMdbFSw="
                                   "localKey" "nAdZxo2Hgz+hjD9fiffG+SrPZiQbq8tMbP1SlKHRFoG5fiL8v7+v2owFuG9gk8UalI5OQHCtLorkOIyC0/LgrA=="}}}
                  hit (resolve-on-disk index att)]
              (is (some? hit))
              (is (str/ends-with? (fs/file-name hit) ".pdf"))))
          (testing "unresolvable attachment -> nil"
            (is (nil? (resolve-on-disk index
                                       {"pointer" {"locatorInfo"
                                                   {"plaintextHash" "bptmOyPGh6/rFaYNo2kqLfGZ83hdsLBlNMqG48DhF4s="
                                                    "localKey" "732vBCDL+iwOhEvjK/YQpfiVzfTh39RtBh3q3VGj8wa46zqAej5sBTVrQixD/SrPdYQcPp/+R4aTCq7/cn/g8Q=="}}}))))))
      (testing "missing files/ dir -> empty index, no throw"
        (is (= {} (build-file-index (str (fs/path tmp "does-not-exist"))))))
      (finally
        (fs/delete-tree tmp)))))

;; ---------------------------------------------------------------------------
;; 10. Argument parsing
;; ---------------------------------------------------------------------------

(deftest parse-args-cases
  (let [a (parse-args ["in.jsonl" "out" "--files-root" "/b" "--copy-media"])]
    (is (= ["in.jsonl" "out"] (:positional a)))
    (is (= "/b" (:files-root a)))
    (is (true? (:copy-media a))))
  (testing "defaults"
    (let [a (parse-args ["in.jsonl" "out"])]
      (is (nil? (:files-root a)))
      (is (nil? (:copy-media a))))))

;; ---------------------------------------------------------------------------
;; 11. End-to-end integration: run -main against the bundled fixture.
;; ---------------------------------------------------------------------------

(deftest integration-fixture
  (let [fixture (some-> (io/resource "fixtures/sample.jsonl") fs/file str)
        out (fs/create-temp-dir {:prefix "sig-int-"})]
    (when-not fixture
      (throw (ex-info "fixture not found on classpath as fixtures/sample.jsonl — is test/ on :paths?" {})))
    (try
      ;; files-root points at a dir with no files/, so attachments are
      ;; "unavailable" — that's fine; we're testing text/structure/naming here.
      (let [summary (convert! {:in-path fixture :out-dir (str out)
                               :files-root (str out) :verbose false})]
        (testing "convert! returns a summary"
          (is (= 2 (:conversations summary)))
          (is (= 1 (:missing summary)))))     ; the one PDF attachment, no files/ present
      (testing "expected per-conversation files are written"
        (is (fs/exists? (fs/path out "Alex Doe.md")))
        (is (fs/exists? (fs/path out "Weekend Trip.md"))))
      (testing "DM content and reaction rendered, newest-first"
        (let [md (slurp (str (fs/path out "Alex Doe.md")))]
          (is (str/includes? md "# Alex Doe"))
          (is (str/includes? md "_2 messages_"))
          (is (str/includes? md "Looking forward to it."))
          (is (str/includes? md "\ud83d\udc4d Sam Example"))     ; reaction by self
          ;; newest message ("Looking forward") should appear before the older one
          (is (< (.indexOf md "Looking forward to it.")
                 (.indexOf md "are we still on for Saturday")))))
      (testing "group title, system-name author, and unavailable attachment"
        (let [md (slurp (str (fs/path out "Weekend Trip.md")))]
          (is (str/includes? md "# Weekend Trip"))
          (is (str/includes? md "**Riley**"))                    ; systemFamilyName
          (is (str/includes? md "attachment unavailable"))
          (is (str/includes? md "application/pdf"))))
      (finally
        (fs/delete-tree out)))))

(defn run [& _]
  (let [{:keys [fail error]} (run-tests 'signal-to-obsidian-test)]
    (when (pos? (+ (or fail 0) (or error 0)))
      (System/exit 1))))

{ pkgs, ... }:
{
  home.packages = with pkgs; [
    # Plugins needed for video
    gst_all_1.gst-libav
    gst_all_1.gst-plugins-bad
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-ugly
    nyxt
  ];

  xdg.configFile."nyxt/config.lisp".text = /*lisp*/''
    ;; Define browser configuration
    (define-configuration browser
      ((theme
        (make-instance 'theme:theme
                       :background-color "#1a1a1a"       ;; bivrost0 - Base dark background
                       :text-color "#b2b2b2"             ;; bivrost5 - Base foreground, warm white
                       :contrast-text-color "#303035"    ;; bivrost2 - Alternative dark color
                       :primary-color "#719cd6"          ;; bivrost9 - Blue color
                       :secondary-color "#303035"        ;; bivrost2 - Alternative dark color
                       :action-color "#303035"           ;; bivrost2  - Alternative dark background
                       :highlight-color "#B48EAD"        ;; bivrost16 - Purple color
                       :success-color "#A3BE8C"          ;; bivrost15 - Green color
                       :warning-color "#D08770"          ;; bivrost13 - Light orange color
                       :codeblock-color "#303035"        ;; bivrost2  - Alternative dark color
                       :error-color "#BF616A")           ;; bivrost12 - Red color
        :doc "Custom Bivrost Dark Theme based on Emacs colors")))

    (defvar *my-search-engines*
      (list
       ;; NixOS Packages search
       (list "NixOS Packages" "nix" "https://search.nixos.org/packages?query=~a"
             "https://search.nixos.org")

       ;; Reddit search
       (list "Reddit" "reddit" "https://www.reddit.com/search/?q=~a"
             "https://www.reddit.com/search")

       ;; YouTube search
       (list "YouTube" "yt" "https://www.youtube.com/results?search_query=~a"
             "https://www.youtube.com/"
             (make-search-completion-function
              :base-url "https://suggestqueries.google.com/complete/search?client=youtube&ds=yt&q=~a"
              :processing-function (lambda (response)
                                   (mapcar #'first (second (njson:decode response))))))

       ;; GitHub search
       (list "GitHub" "gh" "https://github.com/search?q=~a"
             "https://github.com")

       ;; Google search
       (list "Google" "google" "https://www.google.com/search?q=~a"
             "https://www.google.com/"
             (make-search-completion-function
              :base-url "https://suggestqueries.google.com/complete/search?client=firefox&q=~a"
              :processing-function (lambda (response)
                                   (mapcar #'first (second (njson:decode response)))))))
      "List of search engines: name, shortcut, search URL, fallback URL, and optional completion function.")

    (define-configuration context-buffer
      "Configure search engines from the list above."
      ((search-engines
        (append
         (mapcar (lambda (engine)
                   (apply #'make-instance 'search-engine
                          :name (first engine)
                          :shortcut (second engine)
                          :search-url (third engine)
                          :fallback-url (fourth engine)
                          (when (fifth engine)
                            (list :completion-function (fifth engine)))))
                 *my-search-engines*)
         %slot-value%))))

    ;; Reroute bookmarks to a static file
    (defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
      "Reroute the bookmarks to the config directory."
      #p"~/.config/nyxt/bookmarks.lisp")

    ;; Buffer configuration
    (define-configuration buffer
      ((smooth-scrolling t)
       (default-search-engine "duck")  ;; Set DuckDuckGo as default
       (default-modes (append '(nyxt/mode/vi:vi-normal-mode) %slot-value%))
       (external-editor-program '("emacsclient"))
       (override-map (let ((map (make-keymap "override-map")))
                       (define-key map "M-f" 'switch-buffer)
                       (define-key map "M-o" 'set-url-from-bookmark)
                       (define-key map "M-x" 'delete-buffer)
                       (define-key map "M-q" 'delete-all-buffers)
                       (define-key map "\"" 'search-buffers)
                       map))))

    ;; Prompt buffer configuration
    (define-configuration prompt-buffer
      ((default-modes (append '(nyxt/mode/vi:vi-insert-mode) %slot-default%))))
  '';

  xdg.configFile."nyxt/bookmarks.lisp".text = /*lisp*/''
    (
    ;; Development bookmarks
    (:url "https://github.com" :title "Github Dashboard" :date "2025-03-31T18:05:04Z" :tags ("dev" "code"))
    (:url "https://github.com/search?type=code&auto_enroll=true" :title "Github Search" :date "2025-03-31T18:05:04Z" :tags ("dev" "code"))
    (:url "https://github.com/copilot" :title "Github Copilot" :date "2025-03-31T18:05:04Z" :tags ("dev" "ai"))
    (:url "https://portal.azure.com/#home" :title "Azure Portal" :date "2025-03-31T18:05:04Z" :tags ("dev" "cloud"))
    (:url "https://search.nixos.org/packages" :title "Nix Packages" :date "2025-03-31T18:05:04Z" :tags ("dev" "nix"))
    (:url "https://docs.scala-lang.org/scala3/book/scala-features.html" :title "Scala3 Book" :date "2025-03-31T18:05:04Z" :tags ("dev" "scala"))
    (:url "https://typelevel.org/cats-effect/docs/getting-started" :title "Cats Effect" :date "2025-03-31T18:05:04Z" :tags ("dev" "scala"))
    (:url "https://http4s.org/v1/docs/quickstart.html" :title "Http4s" :date "2025-03-31T18:05:04Z" :tags ("dev" "scala"))
    (:url "https://tapir.softwaremill.com/en/latest/" :title "Tapir" :date "2025-03-31T18:05:04Z" :tags ("dev" "scala"))
    (:url "https://tapir.softwaremill.com/en/latest/" :title "Tapir" :date "2025-03-31T18:05:04Z" :tags ("dev" "scala"))
    (:url "https://home-manager-options.extranix.com/" :title "Home-Manager Options" :date "2025-03-31T18:05:04Z" :tags ("dev" "nix"))

    ;; News bookmarks
    (:url "https://www.reddit.com/" :title "Reddit" :date "2025-03-31T18:05:04Z" :tags ("news" "social"))
    )
  '';
}

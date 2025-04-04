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

  xdg.configFile."nyxt/config.lisp".text = /*common-lisp*/''
      (in-package #:nyxt-user)

      (define-configuration browser
        ((theme
          (make-instance 'theme:theme
            :font-family "Iosevka Slab"
            :monospace-font-family "Iosevka Fixed"
            :dark-p t

            ;; Your existing colors
            :background-color "#262a34"        ;; Dark background
            :background-color+ "#2a2e38"       ;; Slightly lighter background
            :background-color- "#22262e"       ;; Slightly darker background
            :on-background-color "#dfdfe0"     ;; Light text on background

            :accent-color "#BF616A"            ;; Nord red accent
            :on-accent-color "#262a34"         ;; Dark text on accent

            ;; Action color (derived from accent)
            :action-color "#BF616A"            ;; Same as accent
            :action-color+ "#d5727b"           ;; Lighter variation
            :action-color- "#a94e56"           ;; Darker variation

            ;; Primary color variations
            :primary-color "#3B4252"           ;; Nord Polar Night
            :on-primary-color "#dfdfe0"        ;; Light text on primary
            :primary-color+ "#4C566A"          ;; Lighter Nord Polar Night
            :primary-color- "#2E3440"          ;; Darker Nord Polar Night

            ;; Secondary color variations
            :secondary-color "#303035"         ;; Dark secondary color
            :on-secondary-color "#dfdfe0"      ;; Light text on secondary
            :secondary-color+ "#3e3e45"        ;; Lighter secondary
            :secondary-color- "#252528"        ;; Darker secondary

            ;; Text color variations
            :text-color "#dfdfe0"              ;; Base text color (same as on-background)
            :text-color+ "#ffffff"             ;; Brighter text
            :text-color- "#b8b8bd"             ;; Dimmer text
            :contrast-text-color "#262a34"     ;; For contrast against light colors

            ;; Code blocks
            :codeblock-color "#252528"         ;; Similar to secondary-color-
            :codeblock-color+ "#303035"        ;; Same as secondary-color
            :codeblock-color- "#1e1e20"        ;; Darker codeblock
            :on-codeblock-color "#dfdfe0"      ;; Text on codeblock

            ;; Status indicators
            :success-color "#A3BE8C"           ;; Nord green
            :success-color+ "#b9d0a6"          ;; Lighter green
            :success-color- "#8ca675"          ;; Darker green
            :on-success-color "#262a34"        ;; Dark text on success

            :warning-color "#BF616A"           ;; Red (same as accent)
            :warning-color+ "#d5727b"          ;; Lighter warning
            :warning-color- "#a94e56"          ;; Darker warning
            :on-warning-color "#ffffff"        ;; White on warning

            ;; Highlight colors
            :highlight-color "#EBCB8B"         ;; Nord yellow
            :highlight-color+ "#f2d9a6"        ;; Lighter highlight
            :highlight-color- "#d2b576"        ;; Darker highlight
            :on-highlight-color "#262a34"))))  ;; Dark text on highlight

      ;; The prompt buffer is the interface for user interactions.
      (define-configuration prompt-buffer
          ((style
            (str:concat
             %slot-value%
             (theme:themed-css
              (theme *browser*)
              '("#prompt"
                :background-color "#303643" :color "#dfdfe0")
              '("#prompt-extra"
                :background-color "#303643" :color "#dfdfe0")
              '("#selection"
                :background-color "#719cd6" :color "#dfdfe0")
              '("#input"
                :background-color "#3B4252" :color "#dfdfe0" :border-color "#719cd6")
              '("#input:focus" :border-color "#3B4252"))))))

      ;; Window to which the status buffer is attached.
      (define-configuration status-buffer
          ((style (str:concat %slot-value%
                              (theme:themed-css (theme *browser*)
                                                '("#controls" :background-color "#3B4252" :color "#dfdfe0")
                                                '("#modes" :background-color "#50fa7b" :color "#2E3440")
                                                '("#url" :background-color "#88C0D0" :color "#2E3440")
                                                '("#tabs" :background-color "#3B4252" :color "#dfdfe0")
                                                '(".tab" :background-color "#262a34" :color "#dfdfe0")
                                                )))))

      ;; Panel buffer (also known as sidebar): small view on the side of the screen.
      (define-configuration (panel-buffer)
          ((style
            (str:concat
             %slot-default%
             (theme:themed-css (theme *browser*)
    			   '(body :background-color "#dfdfe0")
    			   '(hr :color "#dfdfe0")
    			   '(a :color "#88C0D0")
    			   '(.button :color "#88C0D0" :background-color "#2E3440"))))))


      (define-configuration window
        ((message-buffer-style
          (str:concat
           %slot-default%
           (theme:themed-css (theme *browser*)
                             '(body :background-color "#262a34" :color "#dfdfe0"))))))

      ;; Set StatusLines Mode Icons
      (define-configuration status-buffer ((glyph-mode-presentation-p t)))
      (define-configuration nyxt/mode/force-https:force-https-mode ((glyph "")))
      (define-configuration nyxt/mode/blocker:blocker-mode ((glyph "")))
      (define-configuration nyxt/mode/proxy:proxy-mode ((glyph "")))
      (define-configuration nyxt/mode/reduce-tracking:reduce-tracking-mode  ((glyph "")))
      (define-configuration nyxt/mode/certificate-exception:certificate-exception-mode ((glyph "")))
      (define-configuration nyxt/mode/style:style-mode ((glyph "")))
      (define-configuration nyxt/mode/help:help-mode ((glyph "")))
      (define-configuration document-mode ((glyph "ω")))

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
                         (define-key map "M-s" 'search-buffers)
                         (define-key map "M-p" 'copy-password)
                         (define-key map "M-u" 'copy-username)
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

    ;; Streaming
    (:url "https://twitch.tv" :title "Twitch" :date "2025-03-31T18:05:04Z" :tags ("streaming" "social"))
    (:url "https://www.youtube.com" :title "YouTube" :date "2025-03-31T18:05:04Z" :tags ("streaming" "social"))
    )
  '';
}

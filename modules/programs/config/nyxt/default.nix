{ pkgs, ... }:
{
  home.packages = with pkgs; [
    nyxt
  ];

  xdg.configFile."nyxt/config.lisp".text = /*lisp*/''
    ;; This is a sample configuration file for Nyxt.
    ;; It demonstrates how to configure Nyxt using the Common Lisp language.
    (define-configuration browser
      ((theme
        (make-instance 'theme:theme
                       :background-color "#1a1a1a"       ;; bivrost0 - Base dark background
                       :text-color "#4C566A"             ;; bivrost4 - Base foreground, warm white
                       :contrast-text-color "#b2b2b2"    ;; bivrost5 - Gray contrast
                       :primary-color "#719cd6"          ;; bivrost9 - Blue color
                       :secondary-color "#303035"        ;; bivrost2 - Dark text color
                       :action-color "#8FBCBB"           ;; bivrost8 - Cyan color
                       :highlight-color "#B48EAD"        ;; bivrost16 - Purple color
                       :success-color "#A3BE8C"          ;; bivrost15 - Green color
                       :warning-color "#D08770"          ;; bivrost13 - Light orange color
                       :codeblock-color "#81A1C1"        ;; bivrost10 - Light blue color
                       :error-color "#BF616A")           ;; bivrost12 - Red color
        :doc "Custom Bivrost Dark Theme based on Emacs colors")))

    ;; Reroute bookmarks to a static file
    (defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
      "Reroute the bookmarks to the config directory."
      #p"~/.config/nyxt/bookmarks.lisp")

    (define-configuration buffer
      ((default-modes
         (pushnew 'nyxt/mode/vi:vi-normal-mode %slot-value%))))

    (defvar *duckduckgo-keywords*
      '(:theme :dark
        :help-improve-duckduckgo nil
        :homepage-privacy-tips nil
        :privacy-newsletter nil
        :newsletter-reminders nil
        :install-reminders nil
        :install-duckduckgo nil
        :units-of-measure :metric
        :keyboard-shortcuts t
        :advertisements nil
        :open-in-new-tab nil
        :infinite-scroll t
        :safe-search :off
        :font-size :medium
        :header-behavior :on-fixed
        :font :helvetica
        :background-color "242424"
        :center-alignment t))

    (defvar *my-search-engines*
      (list
       '("google" "https://google.com/search?q=~a" "https://google.com")
       '("wikipedia" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org")
       '("reddit" "https://www.reddit.com/search/?q=~a" "https://www.reddit.com")
       '("nixpackages" "https://search.nixos.org/packages?query=~a" "https://search.nixos.org/packages")
       '("duckduckgo" "https://duckduckgo.com/?q=~a" "https://duckduckgo.com")))

    (define-configuration context-buffer
      "Configure the search engines."
      ((search-engines
        (append
         (mapcar (lambda (engine) (apply 'make-search-engine engine))
                *my-search-engines*)
         %slot-default%))))
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

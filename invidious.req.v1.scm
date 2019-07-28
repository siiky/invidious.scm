(module
  invidious.req.v1
  (
   sxml-read
   )

  (import
    scheme
    (only chicken.eval
          module-environment)
    (only chicken.keyword
          string->keyword)
    (only chicken.module
          export
          reexport)
    chicken.syntax
    chicken.type)

  (import
    (only http-client
          with-input-from-request)
    (only json
          json-read)
    (only ssax
          ssax:xml->sxml)
    openssl)

  (import
    (only invidious.uri.v1
          *fields*
          *pretty?*))

  (reexport
    (only invidious.uri.v1
          *fields*
          *host*
          *pretty?*
          *scheme*))

  ;; @brief Convert a symbol to a keyword
  (: symbol->keyword (symbol -> keyword))
  (define (symbol->keyword s)
    (string->keyword (symbol->string s)))

  ;; @brief Get the procedure associated to symbol @a proc in module @a module
  ;; @param proc A symbol naming a function in the module @a module
  ;; @param module A symbol naming a module
  ;; @returns The procedure with name @a proc from module @a module
  (: get-proc-with-name (symbol symbol -> (procedure (#!rest (or boolean fixnum string symbol)) 'uri)))
  (define (get-proc-with-name proc module)
    (eval proc (module-environment module)))

  ;; @brief Read an XML structure from @a port and convert it to SXML
  ;; @param port A port to read XML from
  ;; @returns SXML read from @a port
  (: sxml-read (#!optional input-port -> list))
  (define (sxml-read #!optional (port (current-input-port)))
    (ssax:xml->sxml port '()))

  ;; @brief Define an Invidious API call
  ;; @param default-reader The default reader procedure for with-input-from-request
  ;; @param name The name of the functions to define
  ;; @param keys The key parameters of the API call
  ;; @param positional Optional (zero or one) positional argument
  ;;
  ;; Defines a function named @a name, that takes @a positional positional
  ;; argument and @a keys key arguments, and makes an HTTP call with
  ;; with-input-from-request
  ;;
  ;; Example:
  ;; #;> (define-iv read-string (example positional) key1 key2)
  ;;     (example positional #!key (reader read-string) (fields (*fields*)) (pretty? (*pretty?*)) (key1 #f) (key2 #f))
  (define-syntax define-iv
    (syntax-rules ()
      ((define-iv default-reader (name pos1 pos2 too-many ...) key ...)
       (syntax-error "There must be at most one positional argument"))

      ((define-iv default-reader (name positional ...) key ...)
       (begin
         (export name)
         (: name (#!rest (or boolean string symbol) -> list))
         (define name
           ; TODO: Is there a better way to do this?
           (let ((iv:name (get-proc-with-name 'name 'invidious.uri.v1)))
             (lambda (positional ... #!key (reader default-reader) (fields (*fields*)) (pretty? (*pretty?*)) (key #f) ...)
               (let
                 ((uri
                    (apply
                      iv:name
                      `(,positional
                         ...
                         #:fields ,fields
                         #:pretty? ,pretty?
                         ,@(append `(,(symbol->keyword 'key) ,key) ...)))))
                 (with-input-from-request uri #f reader)))))))))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1stats
  (define-iv json-read (stats))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1videosid
  (define-iv json-read (videos id) region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1annotationsid
  (define-iv sxml-read (annotations id) source)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1commentsid
  (define-iv json-read (comments id) sort_by source continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1insightsid
  (define-iv json-read (insights))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1captionsid
  (define-iv json-read (captions id) label lang tlang region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1trending
  (define-iv json-read (trending) type region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1top
  (define-iv json-read (top))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1popular
  (define-iv json-read (popular))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucid
  (define-iv json-read (channels ucid) sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidvideos-apiv1channelsvideosucid
  (define-iv json-read (channels/videos ucid) page sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsucidlatest-apiv1channelslatestucid
  (define-iv json-read (channels/latest))

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelsplaylistsucid-apiv1channelsucidplaylists
  (define-iv json-read (channels/playlists ucid) continuation sort_by)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelscommentsucid-apiv1channelsucidcomments
  (define-iv json-read (channels/comments ucid) continuation)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1channelssearchucid
  (define-iv json-read (channels/search ucid) q page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1searchsuggestions
  (define-iv json-read (suggestions) q)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1search
  (define-iv json-read (search) q page sort_by date duration type features region)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1playlistsplid
  (define-iv json-read (playlists plid) page)

  ;; @see https://github.com/omarroth/invidious/wiki/API#get-apiv1mixesrdid
  (define-iv json-read (mixes rdid))
  )
